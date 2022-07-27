module Scanf

using BufferedStreams

export @scanf, @sscanf, scanf

const EOF = -1
const ARGNUM_TYPE = UInt16
const WIDTH_TYPE = UInt32
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
const Ints = Union{Val{'d'},Val{'i'},Val{'u'},Val{'x'},Val{'X'},Val{'o'}}
const Floats = Val{'f'}
const Chars = Val{'c'}
const Strings = Val{'s'}
const Pointer = Val{'p'}
const HexBases = Union{Val{'x'},Val{'X'},Val{'a'},Val{'A'}}
const DecBases = Union{Val{'d'},Val{'u'}}
const OctBases = Val{'o'}
const IntBases = Val{'i'}
const PositionCounter = Val{'n'}
const Whitespace = Val{Char(SKIP)}
const CharSets = Union{Val{Char(CSOPEN)},Val{Char(CSNEG)}}
const EscapeChar = Val{Char(ESC)}

const DECIMAL = b"0123456789"
const OCTAL = b"01234567"
const HEXADECIMAL = b"0123456789ABCDEFabcdef"
abstract type AbstractSpec{T} end

"""
    Spec{T}

Typed representation of a format specifier.

`T` is a `Val{'X'}`, where `X` is a valid format specifier character.

Special case `X == '_'` represents whitespace (not assigning)

See also `CharsetSpec` and `LiteralSpec`.
"""
struct Spec{T<:Val} <: AbstractSpec{T} # T => %type => Val{'type'}
    assign::ARGNUM_TYPE
    width::WIDTH_TYPE
end

"""
    CharsetSpec{T,S}

Format specifier representing a character set (inclusive or exclusive)
"""
struct CharsetSpec{T<:Val,S} <: AbstractSpec{T}
    assign::ARGNUM_TYPE
    width::WIDTH_TYPE
    set::S
end

"""
    LiteralSpec{S<:AbstractString}

Non-assigning Format specifier representing a literal matching string.
"""
struct LiteralSpec{S} <: AbstractSpec{0}
    string::S
end

# replace double %% by single %
function LiteralSpec(str::T) where {T<:AbstractString}
    if contains(str, "%%")
        str = replace(str, "%%" => "%")
        S = typeof(str)
    else
        S = T
    end
    LiteralSpec{S}(str)
end

"""
    Scanf.Format(format_str)

Create a C scanf-compatible format object that can be used for parse formatted texts.

The input `format_str` can include any valid format specifier character and modifiers.

A `Format` object can be passed to `scanf(str::String, f::Format, args...)` to parse a
formatted string, or `scanf(io::IO, f::Format, args...)` to parse the
formatted string directly from `io`.

For convenience, the `Scanf.format"..."` string macro form can be used for building
a `Scanf.Format` object at macro-expansion-time.

The `@scanf` macro converts a literal string into a `Scanf.Format` implicitly.
"""
struct Format{S,T}
    str::S # original full format string as CodeUnits
    formats::T # Tuple of Specs (including whitespace specs and literal specs)
end

"""
    Scanf.scanf(b::String, f::Scanf.Format, args...)
    Scanf.scanf([io::IO,] f::Scanf.Format, args...)

Apply a Scanf format object `f` to provided String
(1st method), or scan directly from an `io` object (2nd method). See [`@scanf`](@ref)
for more details on C `scanf` support.
`io` defaults to `stdin`.
The args determine the type and default value for the output data.

Return the number of assigned arguments, followed by the assigned output data.
"""
function scanf end

scanf(io::IO, f::Format, args...) = scanner(io, f, args...)
scanf(io::Base.TTY, f::Format, args...) = scanf(BufferedInputStream(io), f, args...)
scanf(f::Format, args...) = scanf(stdin, f, args...)
scanf(str::AbstractString, f::Format, args...) = scanf(IOBuffer(str), f, args...)

"""
    @scanf([io:Union{IO,String}, ], "%Fmt", args::...)

Scan input stream or string of using C `scanf` style format specification string and assign values
to arguments.

The format string (not a variable) is analyzed at macro expansion time.
Equivalent to `scanf(io, Scanf.format"%Fmt", args...)`.

# Examples
```jldoctest
r, a... = @scanf("%f%f%f%f", zeros(4)...)

julia> r, a, b = @scanf "23.4 text" "%f %2s" Float64 String
(2, 23.4, "te")
```
"""
macro scanf(arg, args...)
    n = length(args)
    if arg isa String && !(n > 0 && args[1] isa String)
        fmt = arg
        io = :stdin
    elseif n >= 1 && args[1] isa String
        fmt = args[1]
        io = arg
        args = Base.tail(args)
    else
        throwa("invalid macro arguments: @scanf $arg $(join(args, ' '))")
    end
    f = Format(unescape_string(fmt))
    return esc(:($Scanf.scanf($io, $f, $(args...))))
end

"""
    Scanf.format"..."

Convenience string macro to call `Scanf.Format("...")
"""
macro format_str(str)
    str = unescape_string(str)
    esc(:(Scanf.Format($str)))
end

# Implementation details

# construct Format object by parsing the format string
function Format(f::AbstractString)
    isempty(f) && throwa("empty format string")
    bytes = codeunits(f)
    len = length(bytes)
    fmts = AbstractSpec[]
    pos, b = pushliteral!(fmts, f, bytes, 1)
    ac = 0 # count number of assigning specifiers

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
                pos > len && throwa("incomplete format string: '$f'")
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
            if !(0 <= width <= typemax(WIDTH_TYPE))
                throwi("\"$f\", width not in 1..$(typemax(Int32))")
            end
            # type modifier characters (ignored)
            if b == UInt8('h') || b == UInt8('l')
                prev = b
                pos > len && throwa("incomplete format string: \"$f\"")
                b = bytes[pos]
                pos += 1
                if b == prev # cases "ll" and "hh"
                    pos > len && throwa("incomplete format string: \"$f\"")
                    b = bytes[pos]
                    pos += 1
                end
            elseif b in b"Ljqtz" # more type modifiers (ignored)
                b = bytes[pos]
                pos += 1
            end
            # parse conversion specifier
            if b in b"diouxXcspn" # uper case variants are invalid
                type = Val{lowercase(Char(b))}
            elseif b in b"eEfFgGaA"
                type = Val{Char('f')}
            elseif b == CSOPEN
                txt = "["
                start = pos
                xpos = findnext(isequal(CSCLOSE), bytes, start)
                if start <= len && bytes[start] == CSNEG
                    b = CSNEG
                    txt = "[^"
                    start += 1
                end
                if xpos === nothing
                    throwi("\"$f\", after '$txt' a Char(CSCLOSE) is missing")
                end
                if bytes[start] == CSCLOSE # first character after "[" or "[^"
                    xpos = findnext(isequal(CSCLOSE), bytes, start + 1)
                end
                if (xpos === nothing || start >= xpos)
                    throwi("\"$f\", after '$txt]' a Char(CSCLOSE) is missing")
                end
                charset = view(bytes, start:(xpos-1))
                pos = xpos + 1
                type = Val{Char(b)}
            else
                throwi("\"$f\", invalid conversion specifier: '$(Char(b))'")
            end
        else # format contains a WS character
            type = Whitespace
            assign = false
        end
        ac += assign
        ass = ifelse(assign, ac, 0)
        if ass > typemax(ARGNUM_TYPE)
            throwi("\"$f\", too many assignable conversion specifiers")
        end
        push!(fmts, make_spec(type, ass, width, charset))
        pos, b = pushliteral!(fmts, f, bytes, pos)
    end
    return Format(bytes, Tuple(fmts))
end

# consume characters in format string up to next % or whitespace and insert LiteralSpec
@inline function pushliteral!(fmts, f, bytes, pos)
    len = length(bytes)
    start = pos
    b = 0x00
    j = -1
    while pos <= len
        b = bytes[pos]
        pos += 1
        if b == ESC
            pos > len && throwa("incomplete format string: \"$f\"")
            if bytes[pos] == ESC # "%%" will be removed in LiteralSpec
                pos += 1
            else
                j = pos - 2
                break
            end
        elseif b in WHITESPACE
            j = pos - 2
            while pos <= len && bytes[pos] in WHITESPACE
                pos += 1
            end
            b = SKIP
            break
        end
    end
    j = j < 0 ? pos - 1 : j
    start <= j && push!(fmts, LiteralSpec(SubString(f, start:j)))
    return pos, b
end

Format(f::Format) = f

# extract text items from input stream

# match literal strings
@inline function fmt(io, pos, spec::LiteralSpec)
    str = spec.string
    len = sizeof(str)
    ix = 1
    while ix <= len && !eof(io)
        c = str[ix]
        peek(io, Char) != c && break
        skip(io, ncodeunits(c))
        ix = nextind(str, ix)
    end
    l = ix - 1
    IOBuffer(), l == len, pos + l
end

# skip whitespace spec
@inline function fmt(io, pos, ::Spec{Whitespace})
    pos = skip_ws(io, pos)
    IOBuffer(), true, pos
end

# string spec
@inline function fmt(io, pos, spec::Spec{T}) where {T<:Strings}
    pos = skip_ws(io, pos)
    width = spec.width
    l = 0
    out = IOBuffer()
    while (width == 0 || l < width) && !eof(io)
        c = peek(io, Char)
        (isspace(c) || !isvalid(Char, c)) && break
        write(out, c)
        n = ncodeunits(c)
        skip(io, n)
        l += 1
    end
    out, l > 0, pos + l
end

# charset and chars specs
@inline function fmt(io, pos, spec::S) where {S<:Union{CharsetSpec,Spec{<:Chars}}}
    width = spec.width
    width = ifelse(width == 0, S <: Spec{<:Chars} ? 1 : typemax(Int), width)
    l = 0
    out = IOBuffer()
    while l < width && !eof(io)
        c = peek(io, Char)
        n = ncodeunits(c)
        check_set(c, spec) || break
        skip(io, n)
        write(out, c)
        l += 1
        pos += 1
    end
    out, l > 0, pos
end

# integer specs
@inline function fmt(io, pos, spec::Spec{T}) where {T<:Ints}
    pos = skip_ws(io, pos)
    out = IOBuffer()
    eof(io) && return out, false, pos
    width = spec.width
    width = ifelse(width == 0, typemax(Int), width)
    l = 0
    sig = false
    b = peek(io)
    negate = false
    if b in b"+-"
        l = writeskip(io, out, b, l)
        negate = b == UInt('-')
        sig = true
    end
    digits = DECIMAL
    base, digits = basespec(T)
    ndig = 0
    if base === nothing || base == 16
        b = peek(io)
        if b == UInt('0')
            ndig += 1
            l = writeskip(io, out, b, l)
            if !eof(io) && (b = peek(io)) in b"xX"
                digits = HEXADECIMAL
                l = writeskip(io, out, b, l)
                ndig = 0
            elseif base === nothing
                digits = OCTAL
            end
        end
    end
    while l < width && !eof(io)
        b = peek(io)
        if b in digits
            ndig += 1
            l = writeskip(io, out, b, l)
        else
            break
        end
    end
    succ = ndig > 0
    out, succ, pos + l
end

# pointer spec
@inline function fmt(io, pos, spec::Spec{T}) where {T<:Pointer}
    pos = skip_ws(io, pos)
    out = IOBuffer()
    eof(io) && return out, false, pos
    width = spec.width
    width = ifelse(width == 0, typemax(Int), width)
    digits = HEXADECIMAL
    l = 0
    while l < width && !eof(io)
        b = peek(io)
        if l == 0 && b == UInt8('0')
            l = writeskip(io, out, b, l)
            if !eof(io) && (b = peek(io)) in b"xX"
                l = writeskip(io, out, b, l)
            end
        elseif b in digits
            l = writeskip(io, out, b, l)
        else
            break
        end
    end
    out, l > 2, pos + l
end

# floating point spec
@inline function fmt(io, pos, spec::Spec{T}) where {T<:Floats}
    pos = skip_ws(io, pos)
    width = spec.width
    width = ifelse(width == 0, typemax(Int), width)
    digits = DECIMAL
    expch = b"eEfF"
    x_sign = 0x001
    x_sep = 0x002
    x_exp = 0x004
    x_base = 0x008
    x_inexp = 0x010
    x_nexp = 0x020
    x_mdigits = 0x040
    x_edigits = 0x080
    x_infnan = 0x100
    l = 0
    out = IOBuffer()
    status = x_sign | x_sep | x_base | x_edigits | x_infnan
    while l < width && !eof(io)
        b = peek(io)
        if status & x_base != 0 && b == UInt8('0')
            l = writeskip(io, out, b, l)
            if !eof(io) && (b = peek(io)) in b"xX"
                digits = HEXADECIMAL
                expch = b"pP"
                l = writeskip(io, out, b, l)
            else
                status |= x_mdigits
            end
            status = (status | x_exp | x_edigits) & ~(x_base | x_sign | x_infnan)
            continue
        elseif b in digits
            status = (status | x_exp) & ~(x_base | x_sign | x_infnan)
            status |= (status & x_inexp != 0) ? x_edigits : x_mdigits
        elseif b == UInt8('-') && (status & x_sign) != 0
            status |= ifelse(status & x_base == 0, x_nexp, 0)
            status = status & ~x_sign
        elseif b == UInt8('+') && (status & x_sign) != 0
            status = status & ~x_sign
        elseif b == UInt8('.') && (status & x_sep) != 0
            status = status & ~(x_base | x_sign | x_sep | x_infnan)
        elseif b in expch && (status & x_exp) != 0
            status = (status & ~(x_base | x_exp | x_edigits | x_sep)) | x_sign | x_inexp
            digits = DECIMAL
        elseif b in b"iI" && (status & x_infnan) != 0
            n = expect(io, out, b"INFINITY", 3)
            l = (n == 3 || n == 8) ? l + n : 0
            status |= x_mdigits | x_edigits
            break
        elseif b in b"nN" && (status & x_infnan) != 0
            n = expect(io, out, b"NAN", 3)
            l = n == 3 ? l + n : 0
            status |= x_mdigits | x_edigits
            break
        else
            break
        end
        l = writeskip(io, out, b, l)
    end
    succ = l > 0 && (status & x_mdigits != 0) && (status & x_edigits != 0)
    out, succ, pos + l
end

# helpers for matching nan and inf
@inline function expect(io::IO, out::IO, bytes::AbstractVector{UInt8}, n)
    mark(io)
    m = length(bytes)
    rbytes = read(io, n)
    if !isequiv(rbytes, bytes, n)
        reset(io)
        return 0
    end
    write(out, rbytes)
    if n < m
        mark(io)
        xbytes = read(io, m - n)
        append!(rbytes, xbytes)
        if isequiv(rbytes, bytes, m)
            unmark(io)
            return m
        else
            reset(io)
            return n
        end
    end
    return n
end

@inline function isequiv(rbytes, bytes, n)
    length(rbytes) >= n || return false
    for i = 1:n
        if !(rbytes[i] == bytes[i] || rbytes[i] == bytes[i] + (UInt8('a') - UInt8('A')))
            return false
        end
    end
    return true
end

# position counter spec
@inline function fmt(_, pos, ::Spec{PositionCounter})
    IOBuffer(string(pos - 1)), true, pos
end

# convert to output type specializations
function toout(::Type{<:Val}, out, arg::AbstractString)
    r = String(take!(out))
    oftype(arg, r)
end

function toout(::Type{<:Val}, out, arg::AbstractChar)
    r = String(take!(out))
    length(r) >= 1 ? oftype(arg, r[1]) : arg
end

function toout(::Type{<:Val}, out, arg::AbstractVector{<:AbstractChar})
    r = String(take!(out))
    resize!(arg, length(r))
    arg .= collect(r)
    arg
end

const IntsAndPointers = Union{Ints,Val{'p'},Val{'n'}}
function toout(::Type{T}, out, arg::R) where {T<:IntsAndPointers,R<:Union{Real,Ptr}}
    S = inttype(typeof(arg))
    r = String(take!(out))
    x = xparse(S, r; base = basespec(T)[1])
    x !== nothing ? convert(R, x) : nothing
end

function toout(::Type{T}, out, arg::R) where {T<:Floats,R<:Real}
    r = String(take!(out))
    A = floattype(typeof(arg))
    s = xparse(A, r)
    s !== nothing ? convert(R, s) : nothing
end

function toout(::Type{<:Strings}, out, arg::R) where R<:Real
    r = String(take!(out))
    xparse(R, r; base = R <: Integer ? 10 : nothing)
end

# fmt helpers

# try parse float - assume input string is syntaxtically correct
@inline function xparse(::Type{A}, r::AbstractString; base = nothing) where A<:AbstractFloat
    if 'f' in r || 'F' in r
        r = replace(r, r"[fF]" => 'e')
    end
    s = tryparse(A, r)
    if s === nothing
        expos = findfirst(x -> x in "eEfF", r)
        negx = expos !== nothing && expos < length(r) && r[nextind(r, expos)] == '-'
        s = ifelse(negx, zero(A), typemax(A))
        r[1] == '-' && (s = -s)
    end
    s
end

# try parse integers - assume input string is syntaxtically correct
@inline function xparse(::Type{S}, r::AbstractString; base = nothing) where {S<:Integer}
    sig = r[1] in "+-"
    negate = r[1] == '-'
    n = length(r)
    if n >= 1 && r[1+sig] == '0'
        base = n >= 2 + sig && r[2+sig] in "xX" ? nothing : base === nothing ? 8 : base
    end
    if sig && S <: Unsigned
        r = SubString(r, 2:n)
    end
    x = tryparse(S, r; base = base)
    if x !== nothing && negate && S <: Unsigned
        x = -x
    end
    x
end

# skip WS characters in io stream
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
                pos += 1
            else
                break
            end
        end
    end
    pos
end

# consume 1 byte
@inline function writeskip(io, out, b, l)
    write(out, b)
    skip(io, 1)
    l + 1
end

itemtype(::AbstractSpec{T}) where {T<:Union{Strings,CharSets}} = Val{'s'}
itemtype(::AbstractSpec{T}) where {T} = T

# call the format specifiers aligned with the IO stream
@inline function scanner(io::IO, f::Format, args...)
    n = length(args)
    m = countspecs(f)
    n == m || argmismatch(m, n)
    res = Any[valuefor(x) for x in args]
    eof(io) && return EOF, res...
    pos = 1

    # for each format, scan arg and next substring
    # unroll up to 8 formats
    formats = f.formats
    N = length(formats)
    j = 0
    UNROLL_UPTO = 8
    Base.@nexprs 8 i -> begin
        if N >= i
            fi = formats[i]
            out, succ, pos = fmt(io, pos, fi)
            succ || @goto BREAK
            assign, argno = assignnr(fi)
            if assign
                r = toout(itemtype(fi), out, res[argno])
                r === nothing && @goto BREAK
                res[argno] = r
                j += 1
            end
        end
    end
    if N > UNROLL_UPTO
        for i = (UNROLL_UPTO+1):N
            fi = formats[i]
            out, succ, pos = fmt(io, pos, fi)
            succ || break
            assign, argno = assignnr(fi)
            if assign
                r = toout(itemtype(fi), out, res[argno])
                r === nothing && break
                res[argno] = r
                j += 1
            end
        end
    end
    @label BREAK
    return tuple(j, res...)
end

# utility functions

# default value for types
valuefor(v::AbstractVector{T}) where {T<:AbstractChar} = resize!(v, 0)
valuefor(::Type{Ptr}) = Ptr{Nothing}(0)
valuefor(::Type{T}) where {T<:Union{Real,Ptr,Char}} = T(0)
valuefor(::Type{T}) where {T<:AbstractString} = T("")
valuefor(a::T) where {T<:Union{Real,AbstractChar,AbstractString,Ptr}} = a

# accessor functions
assignnr(::LiteralSpec) = false, 0
assignnr(spec::AbstractSpec) = !iszero(spec.assign), Int(spec.assign)
assign(spec::AbstractSpec) = assignnr(spec)[1]

# showing formats

# recreate the format specifier string from a typed Spec
Base.string(f::Spec{Val{T}}) where {T} = string(string_header(f), T)
function Base.string(f::CharsetSpec{T}) where T<:CharSets
    string(
        string_header(f),
        Char(CSOPEN),
        T <: Val{Char(CSNEG)} ? Char(CSNEG) : "",
        showset(f.set),
        Char(CSCLOSE),
    )
end
Base.string(f::LiteralSpec) = string('"', f.string, '"')

function string_header(f::AbstractSpec)
    string(Char(ESC), !assign(f) ? "*" : "", f.width > 0 ? f.width : "")
end

Base.show(io::IO, f::AbstractSpec) = print(io, string(f))
Base.show(io::IO, f::Format) = print(io, string("Scanf.format\"", String(f.str), '"'))

# reconstruct internals of %[...]
showset(s::UnitRange) = "$(Char(first(s)))-$(Char(last(s)))"

function showset(t::Tuple{String,Vararg})
    s = t[1]
    m, s = special(Char(CSMINUS), s)
    b, s = special(Char(CSCLOSE), s)
    string(b, s, showset.(t[2:end])..., m)
end

showset(t::Tuple) = string(showset.(t)...)

showset(s::AbstractString) = isempty(s) ? "1-0" : s

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
            push!(ch, SubString(cs, a:b))
            a = nextind(cs, k)
        end
    end
    push!(ch, SubString(cs, a:stop))
    str = String(unique(collect(string(ch...))))
    isempty(ra) ? str : isempty(str) ? (length(ra) == 1 ? ra[1] : (ra...,)) : (str, ra...)
end

# bases for integer specs
basespec(::Type{<:HexBases}) = 16, HEXADECIMAL
basespec(::Type{<:OctBases}) = 8, OCTAL
basespec(::Type{<:DecBases}) = 10, DECIMAL
basespec(::Type) = nothing, DECIMAL

# type conversion hints for integer specs
inttype(::Type{<:Float64}) = Int64
inttype(::Type{<:Float32}) = Int32
inttype(::Type{<:Float16}) = Int16
inttype(::Type{<:BigFloat}) = BigInt
inttype(::Type{<:Ptr}) = UInt
inttype(::Type{T}) where {T} = T
floattype(::Type{T}) where {T<:Integer} = float(T)
floattype(::Type{T}) where {T} = T

# check if character is in or not in charset
@inline check_set(c::Char, spec::Spec{<:Chars}) = true
@inline check_set(c::Char, spec::CharsetSpec{Val{Char(CSOPEN)}}) = check_in(c, spec.set)
@inline check_set(c::Char, spec::CharsetSpec{Val{Char(CSNEG)}}) = check_!in(c, spec.set)
@inline check_in(c, set::AbstractString) = occursin(c, set)
@inline check_!in(c, set::AbstractString) = !occursin(c, set)
@inline check_in(c, set::UnitRange) = in(UInt32(c), set)
@inline check_!in(c, set::UnitRange) = !in(UInt32(c), set)
@inline check_in(c, set::Tuple) = any(x -> check_in(c, x), set)
@inline check_!in(c, set::Tuple) = all(x -> check_!in(c, x), set)

# count number of assigning format specifiers in format
countspecs(f::Format) = count(assign, f.formats)

import Base: ==
function ==(f::T, g::T) where {T<:Format}
    f.formats == g.formats
end

# length of utf8 encoding of character starting with this byte
@inline _ncodeunits(a::UInt8) = a < 0xc0 ? 1 : a < 0xe0 ? 2 : a < 0xf0 ? 3 : 4

# error
@noinline function argmismatch(a, b)
    throwa("mismatch between # of format specifiers and provided args: $a != $b")
end

@noinline throwa(msg) = throw(ArgumentError(msg))
@noinline throwi(msg) = throwa("invalid format string: " * msg)

end # module
