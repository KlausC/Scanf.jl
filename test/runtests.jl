using Test, Scanf

@testset "Scanf" begin

# pointer
@testset "%p" begin
    rp = Ref{Ptr{Nothing}}()
    @sscanf("0x42      ", "%4p", rp)
    @test rp[] == Ptr{Nothing}(UInt(0x42))
end

# floating point numbers
@testset "float to $T" for T in (Float64, Float32, Float16, BigFloat)
    f1 = Scanf.Format("%g")
    f2 = Scanf.Format("%4e")
    ra = Ref{T}()
    @testset "$res" for res in ("-13", "99.76 ", "-12.749123479791234797123498781234981432e-4", "0x1.4711p2")
        @test scanf(res, f1, ra) == 1 &&  ra[] == parse(T, res)
        @test scanf(res, f2, ra) == 1 &&  ra[] == parse(T, res[1:min(4,length(res))])
    end
end

# integers
@testset "integer %i to $T" for T in (UInt64, UInt32, UInt16)
    f1 = Scanf.Format("%i")
    ra = Ref{T}()
    @testset "$res" for res in ("17", "4711", "0x7ffe")
        @test scanf(res, f1, ra) == 1 &&  ra[] == parse(T, res)
    end
end
@testset "integer %i to $T" for T in (Int64, Int32, Int16, BigInt, Float64, Float32, Float16, BigFloat)
    f1 = Scanf.Format("%i")
    ra = Ref{T}()
    @testset "$res" for res in ("+17", "-4711", "0x7ffe")
        @test scanf(res, f1, ra) == 1 &&  ra[] == parse(T, res)
    end
end

@testset "integer %i octal input" for T in (Int64,)
    ra = Ref{T}()
    @test scanf("0377", Scanf.Format("%i"), ra) == 1 && ra[] == 255
end
@testset "integer %i negated input" for T in (UInt64, UInt16)
    ra = Ref{T}()
    @test scanf("-99", Scanf.Format("%i"), ra) == 1 && ra[] == -T(99)
end

@testset "integer %o to $T" for T in (Int64, UInt64, Int32, UInt32, Int16, UInt16)
    f1 = Scanf.Format("%o")
    ra = Ref{T}()
    @testset "$res" for res in ("17", "4711", "0377")
        @test scanf(res, f1, ra) == 1 &&  ra[] == parse(T, res, base=8)
    end
end

@testset "integer %x to $T" for T in (Int64, UInt64, Int32, UInt32, Int16, UInt16)
    f1 = Scanf.Format("%x")
    ra = Ref{T}()
    @testset "$res" for res in ("0x4711",)
        @test scanf(res, f1, ra) == 1 &&  ra[] == parse(T, res)
    end
end

@testset "convert integer to other type" begin
    f = Scanf.format"%i"
    rx = Ref{String}()
    @test_throws MethodError scanf("14", f, rx)
end

@testset "%i follow up" for (in, a, b) in [("0", 0, "")]
    f = Scanf.format"%i%s"
    ri = Ref{Int}()
    rs = Ref{String}()
    r = scanf(in, f, ri, rs)
    @test r == 2
    @test ri[] == a
    @test rs[] == b
end

# character sets
@testset "character set [" begin
    f1 = Scanf.Format("%10[a-c]")
    f2 = Scanf.Format("%10[abc]")
    f3 = Scanf.Format("%10[abcx-y]")
    res = "abbbcbbcbadabbdbbabbbann"
    ra = Ref{String}()
    @test scanf(res, f1, ra) == 1 && ra[] == res[1:10]
    @test scanf(res, f2, ra) == 1 && ra[] == res[1:10]
    @test scanf(res, f3, ra) == 1 && ra[] == res[1:10]
end

@testset "character set ^" begin
    f1 = Scanf.Format("%10[^A-A]")
    f2 = Scanf.Format("%10[^A-B]")
    f3 = Scanf.Format("%10[^A-BX]")
    res = "abbbcb Abadabbdbbabbbann"
    ra = Ref{String}()
    @test scanf(res, f1, ra) == 1 && ra[] == res[1:7]
    @test scanf(res, f2, ra) == 1 && ra[] == res[1:7]
    @test scanf(res, f3, ra) == 1 && ra[] == res[1:7]
end

@testset "many arguments" begin
    r = Ref{Int}.(zeros(10))
    f = Scanf.Format("%d%d%d%d%d%d%d%d%d%d ")
    @test scanf("1 2 3 4 5 6 7 8 9 10", f, r...) == 10
end

@testset "single characters" begin
    rc = Ref{Char}()
    r = @sscanf("a%bX", "a%%b%c", rc)
    @test rc[] == 'X'
end

@testset "multiple characters" begin
    rc = Vector{Char}(undef, 0)
    r = @sscanf("abXYZ", "ab%3c", rc)
    @test rc == ['X', 'Y', 'Z']
end


# string
@testset "strings" begin
    ra = Ref{String}()
    rb = Ref{String}()

    r = @sscanf("Hällo\u1680heimør", "%s%s", ra, rb)
    @test r == 2
    @test ra[] == "Hällo"
    @test rb[] == "heimør"
end

# position
@testset "%n" begin
    rn = Ref{Int}()
    @test @sscanf(" 15 16  \n", " %*hhd %*Ld %n", rn) == 1
    @test rn[] == 9
end

# basics
@testset "basics" begin
    ra = Ref{Int}()
    @test @sscanf("%15", "%%%d", ra) == 1
    @test ra[] == 15
    @test_throws ArgumentError Scanf.Format("")
    @test_throws ArgumentError Scanf.Format("%+")
    @test_throws ArgumentError Scanf.Format("%.")
    @test_throws ArgumentError Scanf.Format("%.0")
    @test length(Scanf.Format("%%").formats) == 1
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
end

@testset "%n" begin
    x = Ref{Int}()
    rs = Ref{String}()
    ri = Ref{Int}()
    @test (@sscanf("1234", "%3d4%n", ri, x); x[] == 4)
    @test (@sscanf("😉", "%s%n", rs, x); x[] == 4)
    @test (@sscanf("1234 ", "%s%n", rs, x); x[] == 4)
end

@testset "show specifiers" begin
    f = Scanf.format"%dABC%[abc]%[^abc] %[B-AC]"
    @test sprint(show, f.formats) == "(%d, \"ABC\", %[abc], %[^abc], %*_, %[C])"
end

end # @testset "Scanf"
