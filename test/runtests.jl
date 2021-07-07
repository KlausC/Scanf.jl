using Test, Scanf

@testset "Scanf" begin

    # pointer
    @testset "%p" begin
        rp = Ref{Ptr{Nothing}}()
        @scanf("0x42      ", "%4p", rp)
        @test rp[] == Ptr{Nothing}(UInt(0x42))
    end

    # floating point numbers
    @testset "float to $T" for T in (Float64, Float32, Float16, BigFloat)
        f1 = Scanf.Format("%g")
        f2 = Scanf.Format("%4e")
        ra = Ref{T}()
        @testset "$res" for res in (
            "-13",
            "99.76 ",
            "-12.749123479791234797123498781234981432e-4",
            "0x1.4711p2",
        )
            @test scanf(res, f1, ra)[1] == 1 && ra[] == parse(T, res)
            @test scanf(res, f2, ra)[1] == 1 && ra[] == parse(T, res[1:min(4, length(res))])
        end
    end

    # floating point nan and infinity
    @testset "float to $T" for T in (Float64, Float32, Float16, BigFloat)
        f = Scanf.format"%f%n"
        @testset "$inp" for (inp, res, rr, nn) in (
            ("InfX", T(Inf), 2, 3),
            ("-InFX", T(-Inf), 2, 4),
            ("infiNITYX", T(Inf), 2, 8),
            ("NANX", T(NaN), 2, 3),
            ("-nanX", T(-NaN), 2, 4),
            ("infiX", T(0.0), 0, 0),
            ("naX", T(0.0), 0, 0),
        )
            io = IOBuffer(inp)
            r, x, n = scanf(io, f, T(0.0), 0)
            @test r == rr
            @test n == nn
            @test x === res || T <: BigFloat && (x == res || isnan(x) && isnan(res))
            @test peek(io, Char) == 'X'
        end
    end

    # integers
    @testset "integer %i to $T" for T in (UInt64, UInt32, UInt16)
        f1 = Scanf.Format("%i")
        ra = Ref{T}()
        @testset "$res" for res in ("17", "4711", "0x7ffe")
            @test scanf(res, f1, ra)[1] == 1 && ra[] == parse(T, res)
        end
    end
    @testset "integer %i to $T" for T in (
        Int64,
        Int32,
        Int16,
        BigInt,
        Float64,
        Float32,
        Float16,
        BigFloat,
    )
        f1 = Scanf.Format("%i")
        ra = Ref{T}()
        @testset "$res" for res in ("+17", "-4711", "0x7ffe")
            @test scanf(res, f1, ra)[1] == 1 && ra[] == parse(T, res)
        end
    end

    @testset "integer %i octal input" for T in (Int64,)
        ra = Ref{T}()
        @test scanf("0377", Scanf.Format("%i"), ra)[1] == 1 && ra[] == 255
    end
    @testset "integer %i negated input" for T in (UInt64, UInt16)
        ra = Ref{T}()
        @test scanf("-99", Scanf.Format("%i"), ra)[1] == 1 && ra[] == -T(99)
    end

    @testset "integer %o to $T" for T in (Int64, UInt64, Int32, UInt32, Int16, UInt16)
        f1 = Scanf.Format("%o")
        ra = Ref{T}()
        @testset "$res" for res in ("17", "4711", "0377")
            @test scanf(res, f1, ra)[1] == 1 && ra[] == parse(T, res, base = 8)
        end
    end

    @testset "integer %x to $T" for T in (Int64, UInt64, Int32, UInt32, Int16, UInt16)
        f1 = Scanf.Format("%x")
        ra = Ref{T}()
        @testset "$res" for res in ("0x4711",)
            @test scanf(res, f1, ra)[1] == 1 && ra[] == parse(T, res)
        end
    end

    @testset "convert integer to other type" begin
        f = Scanf.format"%i"
        @test scanf("  14", f, String) == (1, "14")
    end

    @testset "%i follow up" for (inp, rr, a, b) in [
        ("Z", 0, 0, ""),
        ("0", 1, 0, ""),
        ("0x", 1, 0, ""),
        ("0xZ", 2, 0, "Z"),
    ]
        f = Scanf.format"%i%s"
        ri = Ref{Int}(a)
        r, x, y = scanf(inp, f, ri, String)
        @test r == rr
        @test r < 1 || ri[] == a == x
        @test r < 2 || b == y
    end

    # character sets
    @testset "character set [" begin
        f1 = Scanf.Format("%10[a-c ]")
        f2 = Scanf.Format("%10[abc ]")
        f3 = Scanf.Format("%10[abcx-y ]")
        res = " abbcbbcbadabbdbbabbbann"
        ra = Ref{String}()
        @test scanf(res, f1, ra)[1] == 1 && ra[] == res[1:10]
        @test scanf(res, f2, ra)[1] == 1 && ra[] == res[1:10]
        @test scanf(res, f3, ra)[1] == 1 && ra[] == res[1:10]
    end

    @testset "character set ^" begin
        f1 = Scanf.Format("%10[^A-A]")
        f2 = Scanf.Format("%10[^A-B]")
        f3 = Scanf.Format("%10[^A-BX]")
        res = " abbcb Abadabbdbbabbbann"
        ra = Ref{String}()
        @test ((r, a) = scanf(res, f1, ra); r == 1) && a == res[1:7]
        @test scanf(res, f2, ra)[1] == 1 && ra[] == res[1:7]
        @test scanf(res, f3, ra)[1] == 1 && ra[] == res[1:7]
    end

    @testset "many arguments" begin
        f = Scanf.Format("%d%d%d%d%d%d%d%d%d%d ")
        @test scanf("1 2 3 4 5 6 7 8 9 10", f, fill(Int, 10)...)[1] == 10
    end

    @testset "single characters" begin
        rc = Ref{Char}()
        r, c = @scanf("a%bX", "a%%b%c", rc)
        @test c == 'X' == rc[]
    end

    @testset "multiple characters" begin
        v = Char[]
        r, cc = @scanf("abXYZ", "ab%3c", v)
        @test ['X', 'Y', 'Z'] == cc == v
        r, cc = @scanf("abXYZ", "ab%3c", Char[])
        @test ['X', 'Y', 'Z'] == cc
        r, cc = @scanf("abXYZ", "ab%3c", Char)
        @test 'X' == cc
        r, cc = @scanf("abXYZ", "ab%3c", String)
        @test "XYZ" == cc
    end


    # string
    @testset "strings" begin
        r, a, b = @scanf("Hällo\u1680heimør", "%s%s", String, String)
        @test r == 2
        @test "Hällo" == a
        @test "heimør" == b
    end

    # position
    @testset "%n" begin
        rn = Ref{Int}()
        @test @scanf(" 15 16  \n", " %*hhd %*Ld %n", rn) == (1, 9)
        @test rn[] == 9
    end

    # basics
    @testset "basics" begin
        ra = Ref{Int}()
        @test_throws ArgumentError try
            @eval @scanf(1, 2, 3)
        catch ex
            rethrow(ex.error)
        end
        @test @scanf("%15", "%%%d", ra) == (1, 15)
        @test ra[] == 15
        @test_throws ArgumentError Scanf.Format("")
        @test_throws ArgumentError Scanf.Format("%")
        @test_throws ArgumentError Scanf.Format("%l")
        @test_throws ArgumentError Scanf.Format("%hh")
        @test_throws ArgumentError Scanf.Format("%hhU")
        @test_throws ArgumentError Scanf.Format("%+")
        @test_throws ArgumentError Scanf.Format("%.")
        @test_throws ArgumentError Scanf.Format("%.0")
        @test Scanf.Format("%%").formats[1].string == "%"
        @test_throws ArgumentError Scanf.Format("%y%d")
        @test_throws ArgumentError Scanf.Format("%\u00d0%d")
        @test_throws ArgumentError Scanf.Format("%\u0f00%d")
        @test_throws ArgumentError Scanf.Format("%\U0001ffff%d")
        @test_throws ArgumentError Scanf.Format("%[")
        @test_throws ArgumentError Scanf.Format("%[^")
        @test_throws ArgumentError Scanf.Format("%[]")
        @test_throws ArgumentError Scanf.Format("%[^]")
        @test Scanf.Format("%[A-A]").formats[1].set == "A"
        f = Scanf.Format("%d ")
        @test Scanf.Format(f) === f
        @test Scanf.format"%d " == f
        @test_throws ArgumentError scanf("", f)

        toobig_argnum = (Int64(typemax(Scanf.ARGNUM_TYPE)) + 1)
        @test Scanf.Format("%d"^(toobig_argnum - 1)) isa Scanf.Format
        @test_throws ArgumentError Scanf.Format("%d"^toobig_argnum)

        toobig_width = (Int64(typemax(Scanf.WIDTH_TYPE)) + 1)
        @test Scanf.Format("%$(toobig_width - 1)d") isa Scanf.Format
        @test_throws ArgumentError Scanf.Format("%$(toobig_width)d")
    end

    @testset "default arguments" begin
        def = (1, 2.0, "x", 'y')
        r, i, f, s, c = @scanf("42X", "%i%f%s%c", def...)
        @test r == 1
        @test (i, f, s, c) == (42, def[2:end]...)
    end

    # failing literal match consumes all matching characters
    @testset "literal string" begin
        io = IOBuffer("AÖÜdef")
        @test scanf(io, Scanf.format"AÖÜDEF")[1] == 0
        @test read(io, String) == "def"
    end

    @testset "%n" begin
        x = Ref{Int}()
        rs = Ref{String}()
        ri = Ref{Int}()
        @test (@scanf("1234", "%3d4%n", ri, x); x[] == 4)
        @test (@scanf("😉", "%s%n", rs, x);
        x[] == 4)
        @test (@scanf("1234 ", "%s%n", rs, x); x[] == 4)
    end

    @testset "show specifiers" begin
        f = Scanf.format"%dABC%[a-cA-C]%[^]a-cx] %[]B-Aa-zC-]%[B-A]"
        @test sprint(show, f.formats) ==
              "(%d, \"ABC\", %[a-cA-C], %[^]xa-c], %*_, %[]Ca-z-], %[1-0])"
    end

    @testset "scanf from stdin" begin
        ri = Ref{Int}()
        f1() = scanf(Scanf.format"abc%i", ri)
        f2() = @scanf "abc%i" ri

        file = tempname(cleanup = true)
        write(file, "abc42")

        io = open(file)
        @test redirect_stdin(f1, io)[1] == 1
        close(io)

        io = open(file)
        @test redirect_stdin(f2, io)[1] == 1
        close(io)
    end
    @testset "overflow $T" for T in (Float64, Float32, Float16, BigFloat)
        @testset "$T $res" for (input, res) in [
            ("+1.55e9999999999999999999", Inf),
            ("-1.55e+9999999999999999999", -Inf),
            ("1.5e-9999999999999999999", 0.0),
            ("-1.5e-9999999999999999999", -0.0),
        ]
            @test ((r, a) = @scanf(input, "%f", oneunit(T)); r == 1) && isequal(a, T(res))
        end
        # pointer 0x9....
        # int and uint with overflow value
        # float with overflow exponent
    end
    @testset "overflow $T" for T in (Int128, Int64, Int32, Int16, Int8)
        @testset "$T $input" for (input, res) in [
            ("270141183460469231731687303715884105727", typemax(T)),
            ("-270141183460469231731687303715884105727", typemin(T)),
        ]
            @test ((r, a) = @scanf(input, "%d", T); r == 1) && isequal(a, T(res))
        end
        # pointer 0x9....
    end
    @testset "overflow $T" for T in (UInt128, UInt64, UInt32, UInt16, UInt8)
        @testset "$T $input" for (input, res) in [
            ("470141183460469231731687303715884105727", typemax(T)),
            ("-470141183460469231731687303715884105727", typemax(T)),
        ]
            @test ((r, a) = @scanf(input, "%d", T); r == 1) && isequal(a, T(res))
        end
    end
    @testset "overflow Ptr" begin
        T = Ptr{String}
        input = "0x12341234abcdabcd0"
        res = T(typemax(UInt))
        @test ((r, a) = @scanf(input, "%p", T); r == 1) && isequal(a, T(res))
    end

    @testset "convert to different type" begin
        res = "-1.5e+3"
        f = Scanf.format"%f"
        @test ((r, a) = scanf(res, f, ""); r == 1) && a == res
        @test ((r, a) = scanf(res, f, Int); r == 1) && a == -1500
    end

end # @testset "Scanf"
