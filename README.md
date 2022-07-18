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

`Scanf` provides the macro `r, a,... = @scanf([io, ] "format_string", args...)`
and the function `r, a,... = scanf(io, f::Scanf.Format, args...)`.

The format string must be a string literal, which is evaluated once at macro expansion time.

Alternatively `f = Scanf.format"format_string"` or `f = Scanf.Format(::AbstractString)` create a format object, which can be
used in the function call.

The arguments are default values of types `Real`, `AbstractChar`, `AbstractString`, `Ptr`, `AbstractVector{Char}`.

They may also be concrete subtypes of `Real`, `AbstractChar`, `AbstractString`, `Ptr`.

Numeric format specifiers are compatible with numeric arguments and `String`. Conversion errors may happen (float => int).

If the numeric arg type is not wide enough for the value, boundary values with the correct sign are stored (e.g. `Inf`, `-0.0`).

Format specifiers `c`, `s`, `[` are all compatible with arguments `Char`, `Char[]`, and `String`.
In case of `Char` the first character of a string is taken.

All output data are returned as a tuple including the number of assigned values as the first element.
If a value cannot be parsed,
the default value is assigned. In the case of no value is in the corresponding element of `arg`, the default value is derived form `T`.

If the default argument is a `Vector` object, the output values are additionally stored in it.

The format strings follow the definition of C++-scanf [C++ reference](https://en.cppreference.com/w/c/io/fscanf)
with some adaptations:

+ All unicode characters are supported in format strings and input data.

+ in format strings, whitespace specifiers are only the ASCII space characters in `" \n\r\t\f\v"`.

+ in input data, all characters `x` with `isspace(x) == true` are skipped by any whitespace specifier in the format string.

+ The `%n$...` form of format specifications is not supported.

+ Optional type modifiers like `h`, `l`, `L` etc. are ignored; the target type is derived from the corresponding default argument, instead.

+ for type specifier `%c`, without `width` specified, the corresponding argument must have type `Char` or `String`.

+ for type specifier `%Nc`, with a width field `N`, the argument must have type `String` or `Vector{Char}`.
  This vector is re-used and resized to the number of characters actually read.

+ the type specifier `%n`, returns an integer value, which is the character offset in the input data, consumed by this scanf so far.

+ the type specifier `%p` requires a `Ptr` default argument.

+ The return value of both calls is the amount of output arguments, followed by all output data, the trailing ones maybe default values.
 In contrast to C and C++, also the arguments for `%n` are counted.

### Implementation

If the input stream is exhausted before a character is read, the EOF-indicator `-1` is returned in place of the number of assigned values.

For format specifiers "Whitespace", any number of characters (also zero) `x` with `isspace(x) == true` are consumed from the input stream.

For a format specifier literal character, the next character is read and compared with the literal character. If it is equal, it is consumed,
otherwise the process fails.

If format specifier "Character" `%c` is processed, at least one character is read and assigned to the output argument. If no character is available, the process fails.

If format specifier `%n` is processed, no data are consumed (and no EOF returned), but the current read position is returned.

## Description

As derived from [C++ reference](https://en.cppreference.com/w/c/io/fscanf)

The `scanf` function reads input from the stream pointed to by `io`, under control of the string `format` that specifies the admissible
input sequences and how they are to be interpreted, using subsequent arguments 
to define the type of the converted input. The number of arguments must match the number of format specifiers required by the format.

The format is composed of zero or more directives: one or more (ASCII) white-space characters, an ordinary (UTF8) character (neither `'%'` nor a
(ASCII) white-space character), or a conversion specification. Each conversion specification is introduced by the character `'%'`. After the
`'%'`, the following appear in sequence:

+ An optional assignment-suppressing character `'*'`.
+ An optional decimal integer greater than zero that specifies the maximum field width (in characters).
+ An optional length modifier that is not used by this implementation.
+ A conversion specifier character that specifies the type of conversion to be applied.

The `scanf` function executes each directive of the format in turn. When all directives have been executed, or if a directive fails (as
detailed  below), the function returns. Failures are described as input failures (due to the occurrence of an encoding error or the unavailability of input characters), or matching failures (due to inappropriate input).

A directive composed of white-space character(s) is executed by reading input up to the first non-white-space character (which
remains unread), or until no more characters can be read. The directive never fails.

A directive that is an ordinary character is executed by reading the next character of the stream. If that character differs from the  directive, the directive fails and the differing and subsequent characters remain unread. Similarly, if end-of-file, an encoding error, or a read error prevents a character from being read, the directive fails.

A directive that is a conversion specification defines a set of matching input sequences, as described below for each specifier. A conversion
specification is executed in the following steps:

Input white-space characters (as specified by the `isspace` function) are skipped, unless the specification includes a `'['`, `'c'`,
or `'n'` specifier. These white-space characters are not counted against a specified field width.

An input item is read from the stream, unless the specification includes an `'n'` specifier.  An input item is defined as the longest sequence
of input characters which does not exceed any specified field width and which is, or is a prefix of, a matching input sequence.

The first character, if any, after the input item remains unread. If the length of the input item is zero, the execution of the directive
fails; this condition is a matching failure unless end-of-file, an encoding error, or a read error prevented input from the stream, in which
case it is an input failure.

Except in the case of a `'%'` specifier, the input item (or, in the case of a `'%n'` directive, the count of input characters) is converted
to a type appropriate to the conversion specifier and corresponding argument. If the input item is not a matching sequence, the execution of the
directive fails: this condition is a matching failure. Unless assignment suppression was indicated by a `*`, the result of the conversion
is  pushed to the ouput tuple.

If the result of the conversion cannot be represented in the output type, a conversion error is thrown.

Optional length modifiers `l`, `ll`, `h`, `hh`, `L`, `j`, `z`, `t` are accepted before all type specifier characters, but otherwise ignored.

The conversion specifiers and their meanings are:

+ `d` Matches an optionally signed decimal integer, whose format is the same as expected for the subject sequence of the `parse(T, _, base=10)`
    function, where `T` is the integer type of the argument.
+ `i` Matches an optionally signed integer, whose format is the same as expected for the subject sequence of the `parse(T, _, base=nothing)`
    function.
+ `o` Matches an optionally signed octal integer, whose format is the same as expected for the subject sequence of the `parse(T, _, base=8)`
    function.
+ `u` Matches an optionally signed decimal integer, whose format is the same as expected for the subject sequence of the `parse(T, _, base=10)`
    function.
+ `x` Matches an optionally signed hexadecimal integer, whose format is the same as expected for the subject sequence of thew `parse(T, _, base=16)` function.
+ `a`,`e`,`f`,`g` Matches an optionally signed floating-point number, `infinity`, or `NaN`, whose format is the same as expected for the  
    subject sequence of the `parse(T, _)` function, where `T` is a floating point type.
+ `c` Matches a sequence of characters of exactly the number specified by the field width (`1` if no field width is present in the directive).
    The argument type must be `String`, `Char`, or `Vector{Char}`, If the field width is greater than `1` and a `Char` type is given, only the
    first character is stored.
+ `s` Matches a (non-empty) sequence of non-white-space characters. The argument types are as for `c`.

+ `[` Matches a nonempty sequence of characters from a set of expected characters (the scanset). The argument types are as for `c`.
    The conversion specifier includes all subsequent characters in the format string, up to and including the matching right bracket (`]`).
    The characters between the brackets (the scanlist) compose the scanset, unless the character after the left bracket is a circumflex (`^`),
    in which case the scanset contains all characters that do not appear in the scanlist between the circumflex and the right bracket.
    If the conversion specifier begins with `[]` or `[^]`, the right bracket character is in the scanlist and the next following right bracket
    character is the matching right bracket that ends the specification; otherwise the first following right bracket character is the one that ends the specification. If a `-` character is in the scanlist and is not the first, nor the second where the first character is a `^`,
    nor the last character, it defines the range between the character left of `-` and the character right of `-`. The order defining
    the range is the integer range of unicode codepoints of the characters. Empty ranges (like `b-a`) are ignored.

+ `p` Matches an set of sequences, which are the same as the set of sequences that may be produced by
     the `%p` conversion of the `printf`-function. The corresponding argument must be of type `Ptr{T}` where T may be any type.
     The  input  item  is  converted  to  a  pointer  value  in  an implementation-defined manner.  If the input item is a value converted earlier during  the  same  program  execution,  the  pointer  that  results  shall  compare equal to that value; otherwise the behavior of the `%p` conversion is undefined.

+ `n` No input is consumed. The corresponding argument must be an integer type, into which is converted the number of characters read from the input
   stream so far by this call to the `scanf` function. Execution of a `%n` directive does as well increment the assignment count returned at
   the completion of execution of the `scanf` function. If the conversion specification includes an assignment-suppressing character no argument
   is consumed. An optional width field is ignored.

+ `%` Matches a single `'%'` character; no conversion or assignment occurs. The complete conversion specification is `%%`.
   (with other words, `%%` is treated like a single ordinary character `%`).

If a conversion specification is invalid, the behavior is undefined.

The conversion specifiers `A`,`E`,`F`,`G`, and `X` are also valid and behave the same as, respectively, `a`,`e`,`f`,`g`, and `x`.

Trailing white space (including new-line characters) is left unread unless matched by a directive. The success of literal matches and
suppressed assignments is not directly determinable other than via the `%n` directive.
