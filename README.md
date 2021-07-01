# Scanf

[![Build Status](https://github.com/KlausC/Scanf.jl/workflows/CI/badge.svg)](https://github.com/KlausC/Scanf.jl/actions)
[![Coverage](https://codecov.io/gh/KlausC/Scanf.jl/branch/master/graph/badge.svg)](https://codecov.io/gh/KlausC/Scanf.jl)


## Features

Scanf provides the macros `@scanf([io, ] "%format", args...)` and `@sscanf(string, "%format", args...)`.

The format string must be a string literal, which is evaluated once at macro expansion time.

Alternatively `f = Scanf.Format(format_string)` creates a format object, which can be used like `Scanf.format([io|string,] f, args...)`.

The arguments are of type `Ref{T}` where `T` is a concrete type. 
All output data are stored in new objects of that type.

The format strings follow the definition of GNU-scanf [manual page scanf](https://www.man7.org/linux/man-pages/man3/scanf.3.html)
with some adaptations:

+ All unicode characters are supported in format strings and input data.

+ strings are not NUL-terminated, `\0` is a valid character in format string and input data.

+ in format strings, whitespace specifiers are only ASCII space characters " \n\r\t\f\v".

+ in input data, all characters `x` with `isspace(x) == true` are skipped by all whitespace specifiers in the format string.

+ The `%n$...` form of format specifications is not supported.

+ The optional modifiers `'` and `m` are not supported / not applicable.

+ The optional type modifiers like `l`, `h`, etc. are not supported; the taget type is derived form the reference type of the corresponding
argument, though.

+ for type specifier `%c`, without `width` specified, the corresponding argument must have type `Ref{Char}`.

+ for type specifier `%Nc`, with a width field `N`, the argument must have type `Vector{Char}`.
  This vector is re-used and resized to the number of characters actually read.

+ the type specifier `n`, returns an integer value, which is the byte offset in the input data, consumed so far.

+ the type specifier `p` requires a `Ref{Ptr{Nothing}}` output argument.

+ all floating point type specifiers `efgaEFGA` are used interchangeably. They support all input data, as produced by the corresponding
  @printf type specifiers, especially the hexadecimal floating point format.

+ The return value of both functions is the amount of output arguments populated by the functions. Also the arguments for `%n` are counted. 

 ## Usage

 ```julia

     using Scanf
    
     ra = Ref{Int}()
     rb = Ref{String}()

     @sscanf("  13 This is a prime number", "%d%[a-zA-Z]"), ra, rb)
```
