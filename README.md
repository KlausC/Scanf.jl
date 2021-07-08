# Scanf

[![Build Status](https://github.com/KlausC/Scanf.jl/workflows/CI/badge.svg)](https://github.com/KlausC/Scanf.jl/actions)
[![Coverage](https://codecov.io/gh/KlausC/Scanf.jl/branch/master/graph/badge.svg)](https://codecov.io/gh/KlausC/Scanf.jl)

Scanf scans UTF8-encoded input streams or strings and creates output data according to a format string.
It mimics the behaviour of the C-function with the same name.
 ## Usage

 ```julia
julia> using Scanf

julia> r, a, b = @scanf("  13 This is a prime number", "%d%[ a-zA-Z]", Int, String)
(2, 13, " This is a prime number")

# collect data in a happy tuple
julia> r, t... = @scanf "1 2 -inf 4 \u4119" "%d%u%e%x%s" 0 UInt 0.0 Int ""
(5, 1, 0x0000000000000002, -Inf, 4, "䄙")

# scan date-time string - note the S and ms parts use the default values
julia> f = Scanf.format"%d.%2d.%2d%*1[ T]%2d:%2d:%2d.%3d";

julia> r, y, m, d, H, M, S, ms = scanf("2021.07.04T15:53", f, Int, zeros(Int8, 5)..., Int16)
(5, 2021, 7, 4, 15, 53, 0, 0)

```
## Features

Scanf provides the macro `r, a,... = @scanf([io, ] "%format", args...)`
and the function `r, a,... = scanf(io, f::Scanf.Format, args...)`.

The format string must be a string literal, which is evaluated once at macro expansion time.

Alternatively `f = Scanf.format"%format_string"` creates a format object, which can be used in the function call.

The arguments are default values of types `Real`, `AbstractChar`, `AbstractString`, `Ptr`, `AbstractVector{Char}`.

They may also be concrete subtypes of `Real`, `AbstractChar`, `AbstractString`, `Ptr`.

All output data are returned as a tuple including the number of assigned values at first. If a value cannot be parsed,
the default value is assigned. In the case of no value is in the corresponding element of `arg`, the default value is derived form `T`.

If the default argument is a `Vector` object, the output values are additionally stored in it.

The format strings follow the definition of C++-scanf [C++ reference](https://en.cppreference.com/w/c/io/fscanf)
with some adaptations:

+ All unicode characters are supported in format strings and input data.

+ in format strings, whitespace specifiers are only the ASCII space characters " \n\r\t\f\v".

+ in input data, all characters `x` with `isspace(x) == true` are skipped by any whitespace specifier in the format string.

+ The `%n$...` form of format specifications is not supported.

+ Optional type modifiers like `h`, `l`, `L` etc. are ignored; the target type is derived from the type of the corresponding default argument, instead.

+ for type specifier `%c`, without `width` specified, the corresponding argument must have type `Char` or `String`.

+ for type specifier `%Nc`, with a width field `N`, the argument must have type `String` or `Vector{Char}`.
  This vector is re-used and resized to the number of characters actually read.

+ the type specifier `%n`, returns an integer value, which is the byte offset in the input data, consumed by this scanf so far.

+ the type specifier `%p` requires a `Ptr` default argument.

+ The return value of both calls is the amount of output arguments, followed by all output data, the trailing ones maybe default values.
 In contrast to C and C++, also the arguments for `%n` are counted. 

