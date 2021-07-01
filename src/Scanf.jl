module Scanf

using Base.Ryu

export @scanf, @sscanf

# whitespace characters in format strings
const WHITESPACE = b" \n\t\r\f\v"
# format escape character
const ESC = UInt8('%')
# format specifier to skip whitespace
const SKIP = UInt8('_')
# format indicator no-assign
const NOASSIGN = UInt8('*')
# format specifiers for character sets
const CSOPEN = UInt8('[')
const CSCLOSE = UInt8(']')
const CSNEG = UInt8('^')
const CSMINUS = UInt8('-')

# format specifier categories
const Ints = Union{Val{'d'}, Val{'i'}, Val{'u'}, Val{'x'}, Val{'X'}, Val{'o'}}
const Floats = Union{Val{'e'}, Val{'E'}, Val{'f'}, Val{'F'}, Val{'g'}, Val{'G'}, Val{'a'}, Val{'A'}}
const Chars = Union{Val{'c'}, Val{'C'}}
const Strings = Union{Val{'s'}, Val{'S'}}
const Pointer = Val{'p'}
const HexBases = Union{Val{'x'}, Val{'X'}, Val{'a'}, Val{'A'}}
const DecBases = Union{Val{'d'}, Val{'u'}}
const OctBases = Val{'o'}
const IntBases = Val{'i'}
const PositionCounter = Val{'n'}
const Whitespace = Val{Char(SKIP)}
const CharSets = Union{Val{Char(CSOPEN)}, Val{Char(CSNEG)}}
const EscapeChar = Val{Char(ESC)}

abstract type AbstractSpec end

"""
Typed representation of a format specifier.

`T` is a `Val{'_'}`, where `_` is a valid format specifier character.

Fields are the various modifiers allowed for various format specifiers.
"""
struct Spec{T} <: AbstractSpec # T => %type => Val{'type'}
    noassign::Bool
    width::Int
end

struct CharsetSpec{T,S} <: AbstractSpec
    noassign::Bool
    width::Int
    set::S
end

# recreate the format specifier string from a typed Spec
Base.string(f::Spec{Val{T}}; modifier::String="") where {T} =
    string("%", f.noassign ? "*" : "", f.width > 0 ? f.width : "", modifier, T)
Base.string(f::CharsetSpec{Val{Char(CSOPEN)}}; modifier::String="") =
    string("%", f.noassign ? "*" : "", f.width > 0 ? f.width : "", modifier, Char(CSOPEN), f.set..., Char(CSCLOSE))
Base.string(f::CharsetSpec{Val{Char(CSNEG)}}; modifier::String="") =
    string("%", f.noassign ? "*" : "", f.width > 0 ? f.width : "", modifier, Char(CSOPEN), Char(CSNEG), f.set..., Char(CSCLOSE))
Base.show(io::IO, f::AbstractSpec) = print(io, string(f))

function make_spec(T, noassign, width, cs)
    if cs === nothing || isempty(cs)
        Spec{T}(noassign, width)
    else
        charset = make_charset(cs)
        CharsetSpec{T,typeof(charset)}(noassign, width, charset)
    end
end

make_charset(cs::AbstractVector{UInt8}) = make_charset(String(cs))

function make_charset(cs::String)
    M = Char(CSMINUS)
    ch = String[]
    ra = Any[]
    start = 1
    stop = lastindex(cs)
    mind = findall(isequal(M), cs)
    a = start
    for i in mind
        if i > start && i < stop
            j = prevind(cs, i)
            k = nextind(cs, i)
            cj, ck = cs[j], cs[k]
            if cj < ck
                push!(ra, UInt32(cj):UInt32(ck))
                b = prevind(cs, j)
            elseif cj == ck
                b = j
            end
            push!(ch, SubString(cs,a:b))
            a = nextind(cs, k)
        end
    end
    push!(ch, SubString(cs, a:stop))
    str = String(unique(collect(string(ch...))))
    isempty(ra) ? str : isempty(str) ? (length(ra) == 1 ? ra[1] : (ra...,)) : (str, ra...) 
end     

"""
    Scanf.Format(format_str)

Create a C scanf-compatible format object that can be used for parse formated texts.

The input `format_str` can include any valid format specifier character and modifiers.

A `Format` object can be passed to `Scanf.format(str::String, f::Format, args...)` to parse a
formatted string, or `Printf.format(io::IO, f::Format, args...)` to parse the
formatted string directly from `io`.

For convenience, the `Scanf.format"..."` string macro form can be used for building
a `Scanf.Format` object at macro-expansion-time.

!!! compat "Julia 1.6"
    `Scanf.Format` requires Julia 1.6 or later.
"""
struct Format{S, T}
    str::S # original full format string as CodeUnits
    # keep track of non-format specifier strings to print
    # length(substringranges) == length(formats) + 1
    # so when scanning, we start with scanning
      # str[substringranges[1]], then formats[1] + args[1]
      # then str[substringranges[2]], then formats[2] + args[2]
      # and so on, then at the end, str[substringranges[end]]
    substringranges::Vector{UnitRange{Int}}
    formats::T # Tuple of Specs
end

# parse format string
function Format(f::AbstractString)
    isempty(f) && throw(ArgumentError("empty format string"))
    bytes = codeunits(f)
    len = length(bytes)
    pos = 1
    b = 0x00
    while pos <= len
        b = bytes[pos]
        pos += 1
        if b == ESC
            pos > len && throw(ArgumentError("invalid format string: '$f'"))
            break
        elseif b in WHITESPACE
            b = SKIP
            break
        end
    end
    strs = [1:pos - 1 - (b == ESC || b == SKIP)]
    fmts = []
    while pos <= len || b == SKIP
        noassign = false
        width = 0
        charset = nothing
        if b == ESC
            b = bytes[pos]
            pos += 1
            # positioned at start of first format str %
            # parse flags
            if b == NOASSIGN
                noassign = true
                pos > len && throw(ArgumentError("incomplete format string: '$f'"))
                b = bytes[pos]
                pos += 1
            end
            # parse width
            while 0x00 <= b - UInt8('0') < 0x0a
                width = 10 * width + (b - UInt8('0'))
                b = bytes[pos]
                pos += 1
                pos > len && break
            end
            # parse length modifier (ignored)
            if b == UInt8('h') || b == UInt8('l')
                prev = b
                b = bytes[pos]
                pos += 1
                if b == prev
                    pos > len && throw(ArgumentError("invalid format string: '$f'"))
                    b = bytes[pos]
                    pos += 1
                end
            elseif b in b"Ljqtz"
                b = bytes[pos]
                pos += 1
            end
            # parse type
            if b in b"diouxXDOUeEfFgGaAcCsSpn"
                type = Val{Char(b)}
            elseif b == CSOPEN
                txt = "["
                start = pos
                xpos = findnext(isequal(CSCLOSE), bytes, start)
                if start <= len && bytes[start] == CSNEG
                    b = CSNEG
                    txt = "[^"
                    start += 1
                end
                xpos === nothing && throw(ArgumentError("invalid format string '$f', after '$txt' a Char(CSCLOSE) is missing"))
                if bytes[start] == CSCLOSE # first character after "[" or "[^"
                    xpos = findnext(isequal(CSCLOSE), bytes, start+1)
                end
                (xpos === nothing || start >= xpos ) &&  throw(ArgumentError("invalid format string '$f', after '$txt]' a Char(CSCLOSE) is missing"))
                charset = view(bytes, start:xpos-1)   
                pos = xpos + 1
            elseif b == ESC
                noassign = true
            else
                 throw(ArgumentError("invalid format string: '$f', invalid type specifier: '$(Char(b))'"))
            end
            type = Val{Char(b)}
        else # format contains a WS character
            type = Whitespace
            noassign=true
        end
        push!(fmts, make_spec(type, noassign, width, charset))
        start = pos
        b = 0x00
        while pos <= len
            b = bytes[pos]
            pos += 1
            if b == ESC
                pos > len && throw(ArgumentError("invalid format string: '$f'"))
                break
            elseif b in WHITESPACE
                b = SKIP
                break
            end
        end
        push!(strs, start:pos - 1 - (b == ESC || b == SKIP))
    end
    return Format(bytes, strs, Tuple(fmts))
end

Format(f::Format) = f

macro format_str(str)
    str = unescape_string(str)
    esc(:(Scanf.Format($str)))
end

import Base: ==
function ==(f::T, g::T) where T<:Format
    f.substringranges == g.substringranges && f.formats == g.formats
end

# length of utf8 encoding of character starting with this byte
@inline _ncodeunits(a::UInt8) = a < 0xc0 ? 1 : a < 0xe0 ? 2 : a < 0xf0 ? 3 : 4

# character starting in this buffer position
# assume buffer size is sufficient
function _next_char(buf, pos)
    b = buf[pos]
    pos += 1
    l = 8(4-leading_ones(b))
    c = UInt32(b) << 24
    if l < 24
        s = 16
        while s ≥ l
            b = buf[pos]
            b & 0xc0 == 0x80 || break
            pos += 1
            c |= UInt32(b) << s
            s -= 8
        end
    end
    return reinterpret(Char, c)
end

# match escape character
@inline function fmt(buf, pos, arg, j, spec::Spec{EscapeChar})
    len = length(buf)
    return pos + (pos <= len && buf[pos] == ESC), j
end

# skip whitespace
@inline function fmt(buf, pos, arg, j, spec::Spec{Whitespace})
    skip_ws(buf, pos)[1], j
end

@inline function skip_ws(buf, pos)
    len = length(buf)
    while pos <= len 
        b = buf[pos]
        n = _ncodeunits(b)
        if n == 1
            if b in WHITESPACE
                pos += 1
            else
                break
            end
        elseif pos + n - 1 <= len
            c = _next_char(buf, pos)
            if isspace(c)
                pos += n
            else
                break
            end
        else
            break
        end
    end
    pos, len
end

# single character
@inline function fmt(buf, pos, arg, j, spec::Spec{T}) where {T <: Chars}
    noassign, width = spec.noassign, spec.width
    width = ifelse(width == 0, 1, width)
    len = length(buf)
    pos > len && throw(ArgumentError("no complete input character"))
    i = 0
    while pos <= len && i < width
        b = buf[pos]
        n = _ncodeunits(b)
        n > 1 && pos + n - 1 > len && throw(ArgumentError("no complete input character"))
        r = _next_char(buf, pos)
        if !noassign
            i += 1
            assignto!(arg[j], r, i)
        end
        pos += n
    end
    !noassign && arg isa AbstractVector && resize!(arg, i)
    return pos, j + !noassign
end

assignto!(arg::Ref, r, i=1) = arg[] = if i == 1; arg[] = r end
function assignto!(arg::AbstractVector, r, i=1)
    if i > length(arg)
        resize!(arg, i)
    end
    arg[i] = r
end

# strings
@inline function fmt(buf, pos, arg, j, spec::Spec{T}) where {T <: Strings}
    pos, len = skip_ws(buf, pos)
    noassign, width = spec.noassign, spec.width
    start = pos
    m = 0
    l = 0
    while (width == 0 || l < width) && pos <= len
        b = buf[pos]
        b in WHITESPACE && break
        n = _ncodeunits(b)
        if n > 1
            pos + n - 1 > len && break
            c = _next_char(buf, pos)
            isspace(c) && break
        end
        m += n
        pos += n
        l += 1
    end
    if !noassign
        r = String(view(buf, start:pos-1))
        assignto!(arg[j], r)
    end
    return pos, j + !noassign
end

const DECIMAL = b"0123456789"
const OCTAL = b"01234567"
const HEXADECIMAL = b"0123456789ABCDEFabcdef"

basespec(::Type{<:HexBases}) = 16, HEXADECIMAL
basespec(::Type{<:OctBases}) = 8, OCTAL
basespec(::Type{<:DecBases}) = 10, DECIMAL
basespec(::Type) = nothing, DECIMAL

inttype(::Type{<:Float64}) = Int64
inttype(::Type{<:Float32}) = Int32
inttype(::Type{<:Float16}) = Int16
inttype(::Type{<:BigFloat}) = BigInt
inttype(::Type{T}) where T<:Integer = T
inttype(::Type{<:Ptr}) = UInt
inttype(::Type) = Int

# integer types
@inline function fmt(buf, pos, arg, j, spec::Spec{T}) where T <: Ints
    pos, len = skip_ws(buf, pos)
    noassign, width = spec.noassign, spec.width
    if width > 0 && pos + width - 1 < len
        len = pos + width - 1
    end
    start = pos
    sig = false
    pos > len && return pos, j
    b = buf[pos]
    negate = false
    if b in b"+-"
        pos += 1
        negate = b == UInt('-')
        sig = true
    end
    digits = DECIMAL
    base, digits = basespec(T)
    if base === nothing || base == 16
        b = buf[pos]
        if b == UInt('0')
            pos += 1
            if pos <= len && buf[pos] in b"xX"
                digits = HEXADECIMAL
                base = 16
                pos += 1
                start = pos
            elseif base == nothing
                digits = OCTAL
                base = 8
            end
        end
    end
    while pos <= len
        b = buf[pos]
        if b in digits
            pos += 1
        else
            break
        end
    end
    if !noassign
        S = inttype(eltype(arg[j]))
        start += sig && S <: Unsigned
        r = String(view(buf, start:pos-1))
        x = parse(S, r, base=base)
        if S <: Unsigned && negate
            x = -x
        end
        assignto!(arg[j], x)
    end
    pos, j + !noassign
end

# pointers
@inline function fmt(buf, pos, arg, j, spec::Spec{T}) where T <: Pointer
    pos, len = skip_ws(buf, pos)
    noassign, width = spec.noassign, spec.width
    if width > 0 && pos + width - 1 < len
        len = pos + width - 1
    end
    start = pos
    (pos + 1 <= len && buf[pos] == UInt('0') && buf[pos+1] in b"xX") || return pos, j
    pos += 2
    base, digits = nothing, HEXADECIMAL
    while pos <= len
        b = buf[pos]
        if b in digits
            pos += 1
        else
            break
        end
    end
    if !noassign
        r = String(view(buf, start:pos-1))
        S = inttype(eltype(arg[j]))
        assignto!(arg[j], parse(S, r, base=base))
    end
    pos, j + !noassign
end

# floating point types
@inline function fmt(buf, pos, arg, j, spec::Spec{T}) where T<:Floats
    pos, len = skip_ws(buf, pos)
    noassign, width = spec.noassign, spec.width
    if width > 0 && pos + width - 1 < len
        len = pos + width - 1
    end
    start = pos
    digits = DECIMAL
    expch = b"eEfF"
    x_sign = 0x01
    x_sep = 0x02
    x_exp = 0x04
    x_base = 0x08
    status = x_sign  | x_sep | x_base
    while pos <= len
        b = buf[pos]
        if b == UInt('0') && status & x_base != 0
            if pos < len && buf[pos+1] in b"xX"
                digits = HEXADECIMAL
                expch = b"pP"
                pos += 1
            end
            status = (status | x_exp) & ~(x_base | x_sign)
        elseif b in digits
            status = (status | x_exp ) & ~(x_base | x_sign) 
        elseif b in b"+-" && (status & x_sign) != 0
            status = status & ~x_sign
        elseif b == UInt8('.') && (status & x_sep) != 0
            status = status & ~(x_base | x_sign | x_sep)
        elseif b in expch && (status & x_exp) != 0
            status = (status & ~(x_base | x_exp | x_sep)) | x_sign
            digits = DECIMAL
        else
            break
        end
        pos += 1
    end
    if !noassign
        r = String(view(buf, start:pos-1))
        A = eltype(arg[j])
        assignto!(arg[j], parse(float(A), r))
    end
    pos, j + !noassign
end

# position counters
function fmt(buf, pos, arg, j, spec::Spec{PositionCounter})
    noassign = spec.noassign
    if !noassign
        assignto!(arg[j], pos - 1)
    end
    pos, j + !noassign
end

@inline check_set(c::Char, spec::CharsetSpec{Val{Char(CSOPEN)}}) = check_in(c, spec.set)
@inline check_set(c::Char, spec::CharsetSpec{Val{Char(CSNEG)}}) = check_!in(c, spec.set)
@inline check_in(c, set::AbstractString) = occursin(c, set)
@inline check_!in(c, set::AbstractString) = !occursin(c, set)
@inline check_in(c, set::UnitRange) = in(UInt32(c), set)
@inline check_!in(c, set::UnitRange) = !in(UInt32(c), set)
@inline check_in(c, set::Tuple) = any(x->check_in(c, x), set)
@inline check_!in(c, set::Tuple) = all(x->check_!in(c, x), set)

# charset types
@inline function fmt(buf, pos, arg, j, spec::CharsetSpec)
    pos, len = skip_ws(buf, pos)
    noassign, width = spec.noassign, spec.width
    if width > 0 && pos + width - 1 < len
        len = pos + width - 1
    end
    start = pos
    while pos <= len
        b = buf[pos]
        n = _ncodeunits(b)
        pos + n - 1 > len && break
        c = _next_char(buf, pos)
        check_set(c, spec) || break
        pos += n
    end
    if !noassign
        assignto!(arg[j], String(view(buf, start:pos-1)))
    end
    pos, j + !noassign
end

const UNROLL_UPTO = 8
# if you have your own buffer + pos, write formatted args directly to it
@inline function format(buf::AbstractVector{UInt8}, pos::Integer, f::Format, args...)
    # skip first substring
    len = length(buf)
    n = length(args)
    m = countspecs(f)
    n == m || argmismatch(m, n)

    fstr = f.str
    sr = f.substringranges[1]
    newpos = pos + length(sr) - 1
    (newpos > len || buf[sr] != view(buf, pos:newpos)) && return pos, 0
    pos = newpos + 1

    # for each format, scan arg and next substring
    # unroll up to 16 formats
    N = length(f.formats)
    j = 1
    Base.@nexprs 8 i -> begin
        if N >= i
            pos, j = fmt(buf, pos, args, j, f.formats[i])
            sr = f.substringranges[i + 1]
            newpos = pos + length(sr) - 1
            (newpos > len || fstr[sr] != view(buf, pos:newpos)) && return pos, j - 1
            pos = newpos + 1
        end
    end
    if N > UNROLL_UPTO
        for i = UNROLL_UPTO+1:N
            pos, j = fmt(buf, pos, args, j, f.formats[i])
            sr = f.substringranges[i + 1]
            newpos = pos + length(sr) - 1
            (newpos > len || buf[sr] != view(buf, pos:newpos)) && return pos, j - 1
            pos = newpos + 1
        end
    end
    return pos, j - 1
end

@noinline argmismatch(a, b) =
    throw(ArgumentError("mismatch between # of format specifiers and provided args: $a != $b"))

countspecs(f::Format) = count(x-> !x.noassign, f.formats )

"""
    Scanf.format(b::String, f::Scanf.Format, args::Ref...) => Int
    Scanf.format(io::IO, f::Scanf.Format, args::Ref...)

Apply a Scanf format object `f` to provided string, store results in `args`.
(1st method), or scan directly from an `io` object (2nd method). See [`@scanf`](@ref)
for more details on C `scanf` support.
Return the number of assigned arguments.
"""
function format end

function format(str::String, f::Scanf.Format, args::Union{Ref,AbstractVector}...)
    b = codeunits(str)
    format(b, 1, f, args...)[2]
end

function format(io::IO, f::Format, args::Union{Ref,AbstractVector}...)
    str = read(io, String)
    # TODO support reading line by line
    format(str, f, args...)[2]
end

"""
    @scanf([io:IO, ], "%Fmt", args...)

Print `args` using C `scanf` style format specification string.
Optionally, an `IO` may be passed as the first argument to redirect output.

# Examples
```jldoctest
julia> @scanf "Hello %s" "world"
Hello world

julia> @scanf "Scientific notation %e" 1.234
Scientific notation 1.234000e+00

julia> @scanf "Scientific notation three digits %.3e" 1.23456
Scientific notation three digits 1.235e+00

julia> @scanf "Decimal two digits %.2f" 1.23456
Decimal two digits 1.23

julia> @scanf "Padded to length 5 %5i" 123
Padded to length 5   123

julia> @scanf "Padded with zeros to length 6 %06i" 123
Padded with zeros to length 6 000123

julia> @scanf "Use shorter of decimal or scientific %g %g" 1.23 12300000.0
Use shorter of decimal or scientific 1.23 1.23e+07
```

For a systematic specification of the format, see [here](https://www.cplusplus.com/reference/cstdio/scanf/).
See also [`@scanf`](@ref).

# Caveats
`Inf` and `NaN` are printed consistently as `Inf` and `NaN` for flags `%a`, `%A`,
`%e`, `%E`, `%f`, `%F`, `%g`, and `%G`. Furthermore, if a floating point number is
equally close to the numeric values of two possible output strings, the output
string further away from zero is chosen.

# Examples
```jldoctest
julia> @scanf("%f %F %f %F", Inf, Inf, NaN, NaN)
Inf Inf NaN NaN

julia> @scanf "%.0f %.1f %f" 0.5 0.025 -0.0078125
0 0.0 -0.007812
```
"""
macro scanf(io_or_fmt, args...)
    if io_or_fmt isa String
        f = Format(io_or_fmt)
        return esc(:($Scanf.format(stdin, $f, $(args...))))
    else
        f = Format(args[1])
        return esc(:($Scanf.format($io_or_fmt, $f, $(Base.tail(args)...))))
    end
end

"""
    @sscanf("%Fmt", args...)

Return [`@scanf`](@ref) formatted output as string.

# Examples
```jldoctest
julia> @sscanf "this is a %s %15.1f" "test" 34.567
"this is a test            34.6"
```
"""
macro sscanf(str, fmt, args...)
    fmt = fmt isa String ? Format(fmt) : fmt
    return esc(:($Scanf.format($str, $fmt, $(args...))))
end

end # module
