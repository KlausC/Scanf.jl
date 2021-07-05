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

const DECIMAL = b"0123456789"
const OCTAL = b"01234567"
const HEXADECIMAL = b"0123456789ABCDEFabcdef"
abstract type AbstractSpec end

"""
Typed representation of a format specifier.

`T` is a `Val{'X'}`, where `X` is a valid format specifier character.

Fields are the various modifiers allowed for various format specifiers.
"""
struct Spec{T} <: AbstractSpec # T => %type => Val{'type'}
    assign::UInt8
    width::UInt8
end

struct CharsetSpec{T,S} <: AbstractSpec
    assign::UInt8
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

"""
    Scanf.format(b::String, f::Scanf.Format, args::Ref...)
    Scanf.format([io::IO,] f::Scanf.Format, args::Ref...)

Apply a Scanf format object `f` to provided string, store results in `args`.
(1st method), or scan directly from an `io` object (2nd method). See [`@scanf`](@ref)
for more details on C `scanf` support. `io` defaults to `stdin`.

Return the number of assigned arguments.
"""
function scanf end

scanf(io::IO, f::Format, args...) = scanner(io, f, args...)
scanf(f::Format, args...) = scanf(stdin, f, args...)
scanf(str::AbstractString, f::Format, args...) = scanf(IOBuffer(str), f, args...)

"""
    @scanf([io:Union{IO,String}, ], "%Fmt", args::Ref...)

Scan input stream or string of using C `scanf` style format specification string and assign values 
to arguments.

The format string (not a variable) is analyzed at macro expansion time.
Equivalent to `scanf(io, Scanf.format"%Fmt", args...)`.

# Examples
```jldoctest
refs = Ref{Float64}.(1:4)
@scanf(%f%f%f%f, refs...)

julia> ra = Ref{Float64}()
Base.RefValue{Float64}(6.8990893377741e-310)

julia> rb = Ref{String}()
Base.RefValue{String}(#undef)

julia> @scanf "23.4 text" "%f %2s" ra rb
2

julia> ra[]
23.4

julia> rb[]
"te"
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
        throw(ArgumentError("invalid macro arguments: @scanf $arg $(join(args, ' '))"))
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
    isempty(f) && throw(ArgumentError("empty format string"))
    bytes = codeunits(f)
    len = length(bytes)
    fmts = AbstractSpec[]
    pos, b = pushliteral!(fmts, f, bytes, 1)
    ac = 0

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
            else
                 throw(ArgumentError("invalid format string: '$f', invalid type specifier: '$(Char(b))'"))
            end
            type = Val{Char(b)}
        else # format contains a WS character
            type = Whitespace
            assign = false
        end
        ac += assign
        ass = ifelse(assign, ac, 0)
        push!(fmts, make_spec(type, ass, width, charset))
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
    start <= j && push!(fmts, LiteralSpec(SubString(f, start:j)))
    return pos, b
end

Format(f::Format) = f

# match literal strings
@inline function fmt(io, pos, arg, res, spec::LiteralSpec)
    str = spec.string
    len = sizeof(str)
    ix = 1
    while ix <= len && !eof(io)
        c = str[ix]
        peek(io, Char) != str[ix] && break
        n = ncodeunits(c)
        skip(io, ncodeunits(c))
        ix = nextind(str, ix)
    end
    l = ix - 1
    pos + l, l == len
end

# skip whitespace spec
@inline function fmt(io, pos, arg, res, spec::Spec{Whitespace})
    skip_ws(io, pos), true
end

# match character(s)
@inline function fmt(io, pos, arg, res, spec::Spec{T}) where {T <: Chars}
    assign, j = assignnr(spec); width = spec.width
    width = ifelse(width == 0, 1, width)

    eof(io) && read(io, Char)
    i = 0
    start = pos
    while !eof(io) && i < width
        r = peek(io, Char)
        isvalid(Char, r) || break
        if assign
            i += 1
            assignto!(arg[j], res, j, r, i)
        end
        n = ncodeunits(r)
        skip(io, n)
        pos += n
    end
    assign && arg isa AbstractVector && resize!(arg, i)
    return pos, pos > start
end

# string spec
@inline function fmt(io, pos, arg, res, spec::Spec{T}) where {T <: Strings}
    pos = skip_ws(io, pos)
    assign, j = assignnr(spec); width = spec.width
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
    if assign && l > 0
        r = String(take!(out))
        assignto!(arg[j], res, j, r)
    end
    return pos, l > 0
end

# charset specs
@inline function fmt(io, pos, arg, res, spec::CharsetSpec)
    assign, j = assignnr(spec); width = spec.width
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
    if assign && l > 0
        assignto!(arg[j], res, j, String(take!(out)))
    end
    pos + l, l > 0
end

# integer specs
@inline function fmt(io, pos, arg, res, spec::Spec{T}) where T <: Ints
    pos = skip_ws(io, pos)
    assign, j = assignnr(spec); width = spec.width
    width = ifelse(width == 0, typemax(Int), width)
    l = 0
    sig = false
    eof(io) && return pos, false
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
    if assign && l > 0
        S = inttype(eltype(arg[j]))
        b = take!(out)
        if sig && S <: Unsigned
            deleteat!(b, 1)
        end
        r = String(b)
        x = tryparse(S, r, base=base)
        if x === nothing
            x = typemax(S)
            negate && S <: Signed && ( x = typemin(S))
        elseif S <: Unsigned && negate
            x = -x
        end
        assignto!(arg[j], res, j, x)
    end
    pos + l, l > 0
end

# pointer spec
@inline function fmt(io, pos, arg, res, spec::Spec{T}) where T <: Pointer
    pos = skip_ws(io, pos)
    assign, j = assignnr(spec); width = spec.width
    width = ifelse(width == 0, typemax(Int), width)
    rv = (pos, false)
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
    if assign && l > 0
        r = String(take!(out))
        S = inttype(arg[j])
        s = tryparse(S, r, base=16)
        if s === nothing
            s = typemax(S)
        end
        assignto!(arg[j], res, j, s)
    end
    pos + l, l > 0
end

# floating point spec
@inline function fmt(io, pos, arg, res, spec::Spec{T}) where T<:Floats
    pos = skip_ws(io, pos)
    assign, j = assignnr(spec); width = spec.width
    width = ifelse(width == 0, typemax(Int), width)
    digits = DECIMAL
    expch = b"eEfF"
    x_sign = 0x01
    x_sep = 0x02
    x_exp = 0x04
    x_base = 0x08
    x_nsign = 0x10
    x_nexp = 0x20
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
        elseif b == UInt8('-') && (status & x_sign) != 0
            status |= ifelse(status & x_base == 0, x_nexp , x_nsign)
            status = status & ~x_sign
        elseif b == UInt8('+') && (status & x_sign) != 0
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
    if assign && l > 0
        r = String(take!(out))
        A = float(eltype(arg[j]))
        s = tryparse(A, r)
        if s === nothing
            s = ifelse(status & x_nexp != 0, zero(A), A(Inf))
            (status & x_nsign != 0) && (s = -s)
        end
        assignto!(arg[j], res, j, s)
    end
    pos + l, l > 0
end

# position counter spec
function fmt(io, pos, arg, res, spec::Spec{PositionCounter})
    assign, j = assignnr(spec)
    if assign
        assignto!(arg[j], res, j, pos - 1)
    end
    pos, true
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
                pos += n
            else
                break
            end
        end
    end
    pos
end

# call the format specifiers aligned with the IO stream
@inline function scanner(io::IO, f::Format, args::Union{Ref,AbstractVector,Type,Integer,AbstractChar,AbstractString,AbstractFloat}...)
    n = length(args)
    m = countspecs(f)
    n == m || argmismatch(m, n)
    res = Any[valuefor(x) for x in args]
    EOF = -1
    eof(io) && return EOF, Tuple(res)
    pos = 1

    # for each format, scan arg and next substring
    # unroll up to 8 formats
    formats = f.formats
    N = length(formats)
    j = 0
    UNROLL_UPTO = 8
    Base.@nexprs 8 i -> begin if N >= i
            fi = formats[i]
            pos, succ = fmt(io, pos, args, res, fi)
            succ || @goto BREAK
            j += assign(fi)
        end
    end
    if N > UNROLL_UPTO
        for i = UNROLL_UPTO+1:N
            fi = formats[i]
            pos, succ = fmt(io, pos, args, res, fi)
            succ || break
            j += assign(fi)
        end
    end
    @label BREAK; return tuple(j, res...)
end

# utility functions

# default value for types
valuefor(r::Ref{T}) where T = valuefor(T)
valuefor(v::AbstractVector{T}) where T<:AbstractChar = similar(v, 0)
valuefor(::Type{T}) where T<:Union{Real,Ptr,Char} = T(0)
valuefor(::Type{T}) where T<:AbstractString = T("")
valuefor(a::T) where T<:Union{Integer,AbstractChar,AbstractFloat,AbstractString} = a

# assign value to reference or vector element
function assignto!(arg::Base.RefValue, res, j, r, i=1)
    if i == 1
        arg[] = r
    end
    res[j] = r
end
function assignto!(arg::AbstractVector, res, j, r, i=1)
    if i > length(arg)
        resize!(arg, i)
    end
    arg[i] = r
    res[j] = arg
end
function assignto!(::Type{T}, res, j, r, i=1) where T
    res[j] = T(r)
end
function assignto!(a::Any, res, j, r, i=1)
    res[j] = oftype(a, r)
end

# accessor functions
assignnr(::LiteralSpec) = false, 0
assignnr(spec::AbstractSpec) = !iszero(spec.assign), Int(spec.assign)
assign(spec::AbstractSpec) = assignnr(spec)[1]

# showing formats

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
            push!(ch, SubString(cs,a:b))
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
inttype(::Type{T}) where T<:Integer = T
inttype(::Union{Ptr,Type{<:Ptr},Ref{<:Ptr}}) = UInt
inttype(::Type) = Int

# peek 2 bytes
@inline function peek2(io)
    try
        u = peek(io, UInt16)
        v = ntoh(u)
        UInt8(v >> 8), UInt8(v & 0x00ff)
    catch
        peek(io), 0xff
    end
end

# check if character is in or not in charset
@inline check_set(c::Char, spec::CharsetSpec{Val{Char(CSOPEN)}}) = check_in(c, spec.set)
@inline check_set(c::Char, spec::CharsetSpec{Val{Char(CSNEG)}}) = check_!in(c, spec.set)
@inline check_in(c, set::AbstractString) = occursin(c, set)
@inline check_!in(c, set::AbstractString) = !occursin(c, set)
@inline check_in(c, set::UnitRange) = in(UInt32(c), set)
@inline check_!in(c, set::UnitRange) = !in(UInt32(c), set)
@inline check_in(c, set::Tuple) = any(x->check_in(c, x), set)
@inline check_!in(c, set::Tuple) = all(x->check_!in(c, x), set)

# count number of assigning format specifiers in format
countspecs(f::Format) = count(assign, f.formats)

import Base: ==
function ==(f::T, g::T) where T<:Format
    f.formats == g.formats
end

# length of utf8 encoding of character starting with this byte
@inline _ncodeunits(a::UInt8) = a < 0xc0 ? 1 : a < 0xe0 ? 2 : a < 0xf0 ? 3 : 4

# error
@noinline function argmismatch(a, b)
    throw(ArgumentError("mismatch between # of format specifiers and provided args: $a != $b"))
end

end # module
