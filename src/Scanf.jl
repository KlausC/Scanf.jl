module Scanf

using Base.Ryu

export @scanf, @sscanf, scanf

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

`T` is a `Val{'X'}`, where `X` is a valid format specifier character.

Fields are the various modifiers allowed for various format specifiers.
"""
struct Spec{T} <: AbstractSpec # T => %type => Val{'type'}
    assign::Bool
    width::Int
end

struct CharsetSpec{T,S} <: AbstractSpec
    assign::Bool
    width::Int
    set::S
end

struct LiteralSpec{T} <: AbstractSpec
    string::T
end

function LiteralSpec(str::T) where T<:AbstractString
    if contains(str, "%%")
        str = replace(str, "%%" => "%")
        S = typeof(str)
    else
        S = T
    end
    LiteralSpec{S}(str)
end

# accessor function
assign(::LiteralSpec) = false
assign(spec::AbstractSpec) = spec.assign != false

# recreate the format specifier string from a typed Spec
Base.string(f::Spec{Val{T}}; modifier::String="") where {T} =
    string(Char(ESC), !assign(f) ? "*" : "", f.width > 0 ? f.width : "", modifier, T)
Base.string(f::CharsetSpec{Val{Char(CSOPEN)}}; modifier::String="") =
    string(Char(ESC), !assign(f) ? "*" : "", f.width > 0 ? f.width : "", modifier, Char(CSOPEN), showset(f.set), Char(CSCLOSE))
Base.string(f::CharsetSpec{Val{Char(CSNEG)}}; modifier::String="") =
    string(Char(ESC), !assign(f) ? "*" : "", f.width > 0 ? f.width : "", modifier, Char(CSOPEN), Char(CSNEG), showset(f.set), Char(CSCLOSE))
Base.string(f::LiteralSpec) = string('"', f.string, '"')

Base.show(io::IO, f::AbstractSpec) = print(io, string(f))

# reconstruct internals of %[...]
showset(s::AbstractString) = isempty(s) ? "1-0" : s
showset(s::UnitRange) = "$(Char(first(s)))-$(Char(last(s)))"
showset(t::Tuple{}) = ""
function showset(t::Tuple{String,Vararg})
    s = t[1]
    m, s = special(Char(CSMINUS), s)
    b, s = special(Char(CSCLOSE), s)
    string(b, s, showset.(t[2:end])..., m)
end
showset(t::Tuple) = string(showset.(t)...)
function special(x::Char, s::AbstractString)
    if occursin(x, s)
        string(x), filter(!isequal(x), s)
    else
        "", s
    end
end

# internal representation of % and whitespace format specifiers
function make_spec(T, assign, width, cs)
    if cs === nothing || isempty(cs)
        Spec{T}(assign, width)
    else
        charset = make_charset(cs)
        CharsetSpec{T,typeof(charset)}(assign, width, charset)
    end
end

# details of internal representation of %[...] format specifiers
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
            else
                b = prevind(cs, j)
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

A `Format` object can be passed to `scanf(str::String, f::Format, args...)` to parse a
formatted string, or `scanf(io::IO, f::Format, args...)` to parse the
formatted string directly from `io`.

For convenience, the `Scanf.format"..."` string macro form can be used for building
a `Scanf.Format` object at macro-expansion-time.
"""
struct Format{S, T}
    str::S # original full format string as CodeUnits
    formats::T # Tuple of Specs (including whitespace specs and literal specs)
end

# construct Format object by parsing the format string
function Format(f::AbstractString)
    isempty(f) && throw(ArgumentError("empty format string"))
    bytes = codeunits(f)
    len = length(bytes)
    fmts = AbstractSpec[]
    pos, b = pushliteral!(fmts, f, bytes, 1)

    while pos <= len || b == SKIP
        assign = true
        width = 0
        charset = nothing
        if b == ESC
            b = bytes[pos]
            pos += 1
            # positioned at start of first format str %
            # parse flags
            if b == NOASSIGN
                assign = false
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
            # parse length modifiers (ignored)
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
                assign = false
            else
                 throw(ArgumentError("invalid format string: '$f', invalid type specifier: '$(Char(b))'"))
            end
            type = Val{Char(b)}
        else # format contains a WS character
            type = Whitespace
            assign = false
        end
        push!(fmts, make_spec(type, assign, width, charset))
        pos, b = pushliteral!(fmts, f, bytes, pos)
    end
    return Format(bytes, Tuple(fmts))
end

@inline function pushliteral!(fmts, f, bytes, pos)
    len = length(bytes)
    start = pos
    b = 0x00
    while pos <= len
        b = bytes[pos]
        pos += 1
        if b == ESC
            pos > len && throw(ArgumentError("incomplete format string: '$f'"))
            if bytes[pos] == ESC
                pos += 1
            else
                break
            end
        elseif b in WHITESPACE
            b = SKIP
            break
        end
    end
    j = pos - 1 - ( b == ESC || b == SKIP)
    start <= j && push!(fmts, LiteralSpec(view(f, start:j)))
    return pos, b
end


Format(f::Format) = f

"""
    Scanf.format"..."

Convenience string macro to call `Scanf.Format("...")
"""
macro format_str(str)
    str = unescape_string(str)
    esc(:(Scanf.Format($str)))
end

import Base: ==
function ==(f::T, g::T) where T<:Format
    f.formats == g.formats
end

# length of utf8 encoding of character starting with this byte
@inline _ncodeunits(a::UInt8) = a < 0xc0 ? 1 : a < 0xe0 ? 2 : a < 0xf0 ? 3 : 4

# character starting in this buffer position
# assume buffer size is sufficient
function _next_char(buf::AbstractVector{UInt8}, pos)
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

_next_char(io::IO) = peek(io, Char) 

# match literal strings
@inline function fmt(io, pos, arg, j, spec::LiteralSpec)
    str = spec.string
    len = sizeof(str)
    ix = 1
    while ix <= len && !eof(io)
        c = str[ix]
        peek(io, Char) != str[ix] && break
        n = ncodeunits(c)
        skip(io, ncodeunits(c))
        pos += n
        ix = nextind(str, ix)
    end
    pos, j
end

# skip whitespace
@inline function fmt(io, pos, arg, j, spec::Spec{Whitespace})
    skip_ws(io, pos), j
end

@inline function skip_ws(io, pos)
    while !eof(io) 
        b = peek(io)
        n = _ncodeunits(b)
        if n == 1
            if b in WHITESPACE
                skip(io, 1)
                pos += 1
            else
                break
            end
        else
            c = peek(io, Char)
            if isspace(c)
                skip(io, n)
                pos += n
            else
                break
            end
        end
    end
    pos
end

# single character
@inline function fmt(io, pos, arg, j, spec::Spec{T}) where {T <: Chars}
    assign, width = spec.assign, spec.width
    width = ifelse(width == 0, 1, width)

    eof(io) && read(io, Char)
    i = 0
    while !eof(io) && i < width
        r = peek(io, Char)
        isvalid(Char, r) || break
        if assign
            i += 1
            assignto!(arg[j], r, i)
        end
        n = ncodeunits(r)
        skip(io, n)
        pos += n
    end
    assign && arg isa AbstractVector && resize!(arg, i)
    return pos, j + assign
end

assignto!(arg::Ref, r, i=1) = arg[] = if i == 1; arg[] = r end
function assignto!(arg::AbstractVector, r, i=1)
    if i > length(arg)
        resize!(arg, i)
    end
    arg[i] = r
end

# strings
@inline function fmt(io, pos, arg, j, spec::Spec{T}) where {T <: Strings}
    pos = skip_ws(io, pos)
    assign, width = spec.assign, spec.width
    l = 0
    out = IOBuffer()
    while (width == 0 || l < width) && !eof(io)
        c = peek(io, Char)
        (isspace(c) || !isvalid(Char, c)) && break
        assign && write(out, c)
        n = ncodeunits(c)
        skip(io, n)
        pos += n
        l += 1
    end
    if assign
        r = String(take!(out))
        assignto!(arg[j], r)
    end
    return pos, j + assign
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
@inline function fmt(io, pos, arg, j, spec::Spec{T}) where T <: Ints
    pos = skip_ws(io, pos)
    assign, width = spec.assign, spec.width
    width = ifelse(width == 0, typemax(Int), width)
    l = 0
    sig = false
    eof(io) && return pos, j
    out = IOBuffer()
    b = peek(io)
    negate = false
    if b in b"+-"
        skip(io, 1)
        write(out, b)
        l += 1
        negate = b == UInt('-')
        sig = true
    end
    digits = DECIMAL
    base, digits = basespec(T)
    if base === nothing || base == 16
        b = peek(io)
        if b == UInt('0')
            skip(io, 1)
            write(out, b)
            l += 1
            if !eof(io) && peek(io) in b"xX"
                digits = HEXADECIMAL
                base = 16
                skip(io, 1)
                l += 1
            elseif base == nothing
                digits = OCTAL
                base = 8
            end
        end
    end
    while l < width && !eof(io)
        b = peek(io)
        if b in digits
            skip(io, 1)
            write(out, b)
            l += 1
        else
            break
        end
    end
    if assign
        S = inttype(eltype(arg[j]))
        b = take!(out)
        if sig && S <: Unsigned
            deleteat!(b, 1)
        end
        r = String(b)
        x = parse(S, r, base=base)
        if S <: Unsigned && negate
            x = -x
        end
        assignto!(arg[j], x)
    end
    pos + l, j + assign
end

@inline function peek2(io)
    try
        u = peek(io, UInt16)
        v = ntoh(u)
        UInt8(v >> 8), UInt8(v & 0x00ff)
    catch
        peek(io), 0xff
    end
end

# pointers
@inline function fmt(io, pos, arg, j, spec::Spec{T}) where T <: Pointer
    pos = skip_ws(io, pos)
    assign, width = spec.assign, spec.width
    width = ifelse(width == 0, typemax(Int), width)
    rv = (pos, j)
    eof(io) && return rv
    b1, b2 = peek2(io)
    b1 == UInt8('0') && b2 in b"xX" || return rv
    skip(io, 2)
    l = 2
    digits = HEXADECIMAL
    out = IOBuffer()
    while l < width && !eof(io)
        b = peek(io)
        if b in digits
            skip(io, 1)
            write(out, b)
            l += 1
        else
            break
        end
    end
    if assign
        r = String(take!(out))
        S = inttype(eltype(arg[j]))
        assignto!(arg[j], parse(S, r, base=16))
    end
    pos + l, j + assign
end

# floating point types
@inline function fmt(io, pos, arg, j, spec::Spec{T}) where T<:Floats
    pos = skip_ws(io, pos)
    assign, width = spec.assign, spec.width
    width = ifelse(width == 0, typemax(Int), width)
    digits = DECIMAL
    expch = b"eEfF"
    x_sign = 0x01
    x_sep = 0x02
    x_exp = 0x04
    x_base = 0x08
    l = 0
    out = IOBuffer()
    status = x_sign  | x_sep | x_base
    while l < width && !eof(io)
        b1, b2 = peek2(io)
        b = c = b1
        if status & x_base != 0 && b == UInt8('0') && b2 in b"xX"
            digits = HEXADECIMAL
            expch = b"pP"
            l += 1
            skip(io, 1)
            write(out, c)
            c = b2
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
        write(out, c)
        skip(io, 1)
        l += 1
    end
    if assign
        r = String(take!(out))
        A = eltype(arg[j])
        assignto!(arg[j], parse(float(A), r))
    end
    pos + l, j + assign
end

# position counters
function fmt(io, pos, arg, j, spec::Spec{PositionCounter})
    assign = spec.assign
    if assign
        assignto!(arg[j], pos - 1)
    end
    pos, j + assign
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
@inline function fmt(io, pos, arg, j, spec::CharsetSpec)
    pos = skip_ws(io, pos)
    assign, width = spec.assign, spec.width
    width = ifelse(width == 0, typemax(Int), width)
    l = 0
    out = IOBuffer()
    while l < width && !eof(io)
        c = peek(io, Char)
        n = ncodeunits(c)
        check_set(c, spec) || break
        skip(io, n)
        write(out, c)
        l += n
    end
    if assign
        assignto!(arg[j], String(take!(out)))
    end
    pos + l, j + assign
end

const UNROLL_UPTO = 8

# call the format specifiers aligned with the IO stream
@inline function formats(buf::IO, pos::Integer, f::Format, args...)
    n = length(args)
    m = countspecs(f)
    n == m || argmismatch(m, n)

    # for each format, scan arg and next substring
    # unroll up to 16 formats
    N = length(f.formats)
    j = 1
    Base.@nexprs 8 i -> begin
        if N >= i
            pos, j = fmt(buf, pos, args, j, f.formats[i])
        end
    end
    if N > UNROLL_UPTO
        for i = UNROLL_UPTO+1:N
            pos, j = fmt(buf, pos, args, j, f.formats[i])
        end
    end
    return pos, j - 1
end

@noinline argmismatch(a, b) =
    throw(ArgumentError("mismatch between # of format specifiers and provided args: $a != $b"))

# count number of assigning format specifiers in format
countspecs(f::Format) = count(assign, f.formats )

"""
    Scanf.format(b::String, f::Scanf.Format, args::Ref...)
    Scanf.format([io::IO,] f::Scanf.Format, args::Ref...)

Apply a Scanf format object `f` to provided string, store results in `args`.
(1st method), or scan directly from an `io` object (2nd method). See [`@scanf`](@ref)
for more details on C `scanf` support. `io` defaults to `stdin`.

Return the number of assigned arguments.
"""
function scanf end

scanf(io::IO, f::Format, args::Union{Ref,AbstractVector}...) = formats(io, 1, f, args...)[2]
scanf(f::Format, args...) = scanf(stdin, f, args...)
scanf(str::String, f::Format, args...) = scanf(IOBuffer(str), f, args...)

"""
    @scanf([io:IO, ], "%Fmt", args::Ref...)

Scan input stream using C `scanf` style format specification string and assign values 
to arguments.

The format string (not a variable) is analyzed at macro expansion time.
Equivalent to `scanf(io, Scanf.format"%Fmt", args...)`.

# Examples
```jldoctest
refs = Ref{Float64}.(1:4)
@scanf(%f%f%f%f, refs...)
```
"""
macro scanf(io_or_fmt, args...)
    if io_or_fmt isa String
        f = Format(io_or_fmt)
        return esc(:($Scanf.scanf(stdin, $f, $(args...))))
    else
        f = Format(args[1])
        return esc(:($Scanf.scanf($io_or_fmt, $f, $(Base.tail(args)...))))
    end
end

"""
    @sscanf(str, "%Fmt", args...)

Like `@scanf`, taking input from a string. The format string must be literal, not a variable.

# Examples
```jldoctest
julia> ra = Ref{Float64}()
Base.RefValue{Float64}(6.8990893377741e-310)

julia> rb = Ref{String}()
Base.RefValue{String}(#undef)

julia> @sscanf "23.4 text" "%f %2s" ra rb
2

julia> ra[]
23.4

julia> rb[]
"te"
```
"""
macro sscanf(str, fmt, args...)
    fmt = fmt isa String ? Format(fmt) : fmt
    return esc(:($Scanf.scanf($str, $fmt, $(args...))))
end

end # module
