# Scanf

[![Build Status](https://github.com/KlausC/Scanf.jl/workflows/CI/badge.svg)](https://github.com/KlausC/Scanf.jl/actions)
[![Coverage](https://codecov.io/gh/KlausC/Scanf.jl/branch/master/graph/badge.svg)](https://codecov.io/gh/KlausC/Scanf.jl)

Scanf scans UTF8-encoded input streams or strings and creates output data according to a format string.
It mimics the behaviour of the C-function with the same name.

## Features

Scanf provides the macro `r, a,... = @scanf([io, ] "%format", args...)`
and the function `r, a,... = scanf(io, f::Scanf.Format, args...)`.

The format string must be a string literal, which is evaluated once at macro expansion time.

Alternatively `f = Scanf.format"%format_string"` creates a format object, which can be used in the function call.

The arguments are default values of types `Real`, `AbstractChar`, `AbstractString`, `Ptr`.

They may also have types `T`, `Ref{T}`, or `Vector{T}` where `T` is a concrete type.

All output data are returned as a tuple including the number of assigned values at first. If a value cannot be parsed,
the default value is assigned. In the case of no value is in the corresponding element of `arg`, the default value is derived form `T`.

If the default argument is a `Ref` or `Vector` object, the output value is additionally stored in it.

The format strings follow the definition of GNU-scanf [manual page scanf](https://www.man7.org/linux/man-pages/man3/scanf.3.html)
with some adaptations:

+ All unicode characters are supported in format strings and input data.

+ strings are not NUL-terminated, `\0` is a valid character in format string and input data.

+ in format strings, whitespace specifiers are only the ASCII space characters " \n\r\t\f\v".

+ in input data, all characters `x` with `isspace(x) == true` are skipped by any whitespace specifier in the format string.

+ The `%n$...` form of format specifications is not supported (like in C99).

+ The optional modifiers `'` and `m` are not supported / not applicable.

+ The optional type modifiers like `h`, `l`, `L` etc. are ignored;
 the target type is derived form the type of the corresponding default argument, instead.

+ for type specifier `%c`, without `width` specified, the corresponding argument must have type `Char`.

+ for type specifier `%Nc`, with a width field `N`, the argument must have type `Vector{Char}`.
  This vector is re-used and resized to the number of characters actually read.

+ the type specifier `%n`, returns an integer value, which is the byte offset in the input data, consumed so far.

+ the type specifier `%p` requires a `Ptr` default argument.

+ The return value of both calls is the amount of output arguments, followed by all output data, the trailing ones maybe default values.
 In contrast to `C`, also the arguments for `%n` are counted. 

 ## Usage

 ```julia
julia> using Scanf

julia> ra = Ref{Int}();

julia> r, a, b = @scanf("  13 This is a prime number", "%d%[ a-zA-Z]", ra, String)
(2, 13, " This is a prime number")

julia> ra
Base.RefValue{Int64}(13)

julia> b
" This is a prime number"

```
