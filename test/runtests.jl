using Test, Scanf

@testset "Scanf" begin

#=
@testset "%p" begin

    # pointers
    if Sys.WORD_SIZE == 64
        @test (Scanf.@sprintf "%20p" 0) == "  0x0000000000000000"
        @test (Scanf.@sprintf "%-20p" 0) == "0x0000000000000000  "
        @test (Scanf.@sprintf "%20p" C_NULL) == "  0x0000000000000000"
        @test (@sprintf "%-20p" C_NULL) == "0x0000000000000000  "
    elseif Sys.WORD_SIZE == 32
        @test (Scanf.@sprintf "%20p" 0) == "          0x00000000"
        @test (Scanf.@sprintf "%-20p" 0) == "0x00000000          "
        @test (@sprintf "%20p" C_NULL) == "          0x00000000"
        @test (@sprintf "%-20p" C_NULL) == "0x00000000          "
    end

    #40318
    @test @sprintf("%p", 0xfffffffffffe0000) == "0xfffffffffffe0000"

end

@testset "%a" begin

    # hex float
    @test (Scanf.@sprintf "%a" 0.0) == "0x0p+0"
    @test (Scanf.@sprintf "%a" -0.0) == "-0x0p+0"
    @test (Scanf.@sprintf "%.3a" 0.0) == "0x0.000p+0"
    @test (Scanf.@sprintf "%a" 1.5) == "0x1.8p+0"
    @test (Scanf.@sprintf "%a" 1.5f0) == "0x1.8p+0"
    @test (Scanf.@sprintf "%a" big"1.5") == "0x1.8p+0"
    @test (Scanf.@sprintf "%#.0a" 1.5) == "0x2.p+0"
    @test (Scanf.@sprintf "%+30a" 1/3) == "         +0x1.5555555555555p-2"

    @test Scanf.@sprintf("%a", 1.5) == "0x1.8p+0"
    @test Scanf.@sprintf("%a", 3.14) == "0x1.91eb851eb851fp+1"
    @test Scanf.@sprintf("%.0a", 3.14) == "0x2p+1"
    @test Scanf.@sprintf("%.1a", 3.14) == "0x1.9p+1"
    @test Scanf.@sprintf("%.2a", 3.14) == "0x1.92p+1"
    @test Scanf.@sprintf("%#a", 3.14) == "0x1.91eb851eb851fp+1"
    @test Scanf.@sprintf("%#.0a", 3.14) == "0x2.p+1"
    @test Scanf.@sprintf("%#.1a", 3.14) == "0x1.9p+1"
    @test Scanf.@sprintf("%#.2a", 3.14) == "0x1.92p+1"
    @test Scanf.@sprintf("%.6a", 1.5) == "0x1.800000p+0"

end
=#
@testset "float to $T" for T in (Float64, Float32, Float16, BigFloat)
    f1 = Scanf.Format("%g")
    f2 = Scanf.Format("%4e")
    ra = Ref{T}()
    @testset "$res" for res in ("-13", "99.76", "-12.749123479791234797123498781234981432e-4", "0x1.4711p2")
        b = codeunits(res)
        @test Scanf.format(b, 1, f1, ra) == length(res) + 1 &&  ra[] == parse(T, res)
        @test Scanf.format(b, 1, f2, ra) > 0 &&  ra[] == parse(T, res[1:min(4,length(res))])
    end
end
@testset "integer %i to $T" for T in (Int64, UInt64, Int32, UInt32, Int16, UInt16, Float64, Float32, Float16, BigFloat)
    f1 = Scanf.Format("%i")
    ra = Ref{T}()
    @testset "$res" for res in ("17", "4711", "0x7ffe")
        b = codeunits(res)
        @test Scanf.format(b, 1, f1, ra) == length(res) + 1 &&  ra[] == parse(T, res)
    end
end

@testset "character set [" begin
    f = Scanf.Format("%10[abc]")
    res = "abbbcbbcbadabbdbbabbbann"
    b = codeunits(res)
    ra = Ref{String}()
    @test Scanf.format(b, 1, f, ra) == 11 && ra[] == res[1:10]
end
@testset "character set ^" begin
    f = Scanf.Format("%10[^A]")
    res = "abbbcb Abadabbdbbabbbann"
    b = codeunits(res)
    ra = Ref{String}()
    @test Scanf.format(b, 1, f, ra) == 8 && ra[] == res[1:7]
end

#=
@testset "strings" begin

    @test Scanf.@sprintf("Hallo heimur") == "Hallo heimur"
    @test Scanf.@sprintf("+%s+", "hello") == "+hello+"
    @test Scanf.@sprintf("%.1s", "foo") == "f"
    @test Scanf.@sprintf("%s", "%%%%") == "%%%%"
    @test Scanf.@sprintf("%s", "Hallo heimur") == "Hallo heimur"
    @test Scanf.@sprintf("%+s", "Hallo heimur") == "Hallo heimur"
    @test Scanf.@sprintf("% s", "Hallo heimur") == "Hallo heimur"
    @test Scanf.@sprintf("%+ s", "Hallo heimur") == "Hallo heimur"
    @test Scanf.@sprintf("%1s", "Hallo heimur") == "Hallo heimur"
    @test Scanf.@sprintf("%20s", "Hallo") == "               Hallo"
    @test Scanf.@sprintf("%-20s", "Hallo") == "Hallo               "
    @test Scanf.@sprintf("%0-20s", "Hallo") == "Hallo               "
    @test Scanf.@sprintf("%.20s", "Hallo heimur") == "Hallo heimur"
    @test Scanf.@sprintf("%20.5s", "Hallo heimur") == "               Hallo"
    @test Scanf.@sprintf("%.0s", "Hallo heimur") == ""
    @test Scanf.@sprintf("%20.0s", "Hallo heimur") == "                    "
    @test Scanf.@sprintf("%.s", "Hallo heimur") == ""
    @test Scanf.@sprintf("%20.s", "Hallo heimur") == "                    "
    @test (Scanf.@sprintf "%s" "test") == "test"
    @test (Scanf.@sprintf "%s" "tést") == "tést"
    @test Scanf.@sprintf("ø%sø", "hey") == "øheyø"
    @test Scanf.@sprintf("%4sø", "ø") == "   øø"
    @test Scanf.@sprintf("%-4sø", "ø") == "ø   ø"

    @test (Scanf.@sprintf "%8s" "test") == "    test"
    @test (Scanf.@sprintf "%-8s" "test") == "test    "

    @test (Scanf.@sprintf "%s" :test) == "test"
    @test (Scanf.@sprintf "%#s" :test) == ":test"
    @test (Scanf.@sprintf "%#8s" :test) == "   :test"
    @test (Scanf.@sprintf "%#-8s" :test) == ":test   "

    @test (Scanf.@sprintf "%8.3s" "test") == "     tes"
    @test (Scanf.@sprintf "%#8.3s" "test") == "     \"te"
    @test (Scanf.@sprintf "%-8.3s" "test") == "tes     "
    @test (Scanf.@sprintf "%#-8.3s" "test") == "\"te     "
    @test (Scanf.@sprintf "%.3s" "test") == "tes"
    @test (Scanf.@sprintf "%#.3s" "test") == "\"te"
    @test (Scanf.@sprintf "%-.3s" "test") == "tes"
    @test (Scanf.@sprintf "%#-.3s" "test") == "\"te"

    # issue #41068
    @test Scanf.@sprintf("%.2s", "föó") == "fö"
    @test Scanf.@sprintf("%5s", "föó") == "  föó"
    @test Scanf.@sprintf("%6s", "😍🍕") == "  😍🍕"
    @test Scanf.@sprintf("%2c", '🍕') == "🍕"
    @test Scanf.@sprintf("%3c", '🍕') == " 🍕"
end

@testset "chars" begin

    @test Scanf.@sprintf("%c", 'a') == "a"
    @test Scanf.@sprintf("%c",  32) == " "
    @test Scanf.@sprintf("%c",  36) == "\$"
    @test Scanf.@sprintf("%3c", 'a') == "  a"
    @test Scanf.@sprintf( "%c", 'x') == "x"
    @test Scanf.@sprintf("%+c", 'x') == "x"
    @test Scanf.@sprintf("% c", 'x') == "x"
    @test Scanf.@sprintf("%+ c", 'x') == "x"
    @test Scanf.@sprintf("%1c", 'x') == "x"
    @test Scanf.@sprintf("%20c"  , 'x') == "                   x"
    @test Scanf.@sprintf("%-20c" , 'x') == "x                   "
    @test Scanf.@sprintf("%-020c", 'x') == "x                   "
    @test Scanf.@sprintf("%c", 65) == "A"
    @test Scanf.@sprintf("%c", 'A') == "A"
    @test Scanf.@sprintf("%3c", 'A') == "  A"
    @test Scanf.@sprintf("%-3c", 'A') == "A  "
    @test Scanf.@sprintf("%c", 248) == "ø"
    @test Scanf.@sprintf("%c", 'ø') == "ø"
    @test Scanf.@sprintf("%c", "ø") == "ø"
    @test Scanf.@sprintf("%c", '𐀀') == "𐀀"

end

function _test_flags(val, vflag::AbstractString, fmt::AbstractString, res::AbstractString, prefix::AbstractString)
    vflag = string("%", vflag)
    space_fmt = string(length(res) + length(prefix) + 3, fmt)
    fsign = string((val < 0 ? "-" : "+"), prefix)
    nsign = string((val < 0 ? "-" : " "), prefix)
    osign = val < 0 ? string("-", prefix) : string(prefix, "0")
    esign = string(val < 0 ? "-" : "", prefix)
    esignend = val < 0 ? "" : " "

    for (flag::AbstractString, ans::AbstractString) in (
            ("", string("  ", nsign, res)),
            ("+", string("  ", fsign, res)),
            (" ", string("  ", nsign, res)),
            ("0", string(osign, "00", res)),
            ("-", string(esign, res, "  ", esignend)),
            ("0+", string(fsign, "00", res)),
            ("0 ", string(nsign, "00", res)),
            ("-+", string(fsign, res, "  ")),
            ("- ", string(nsign, res, "  ")),
        )
        fmt_string = string(vflag, flag, space_fmt)
        fmtd = Scanf.format(Scanf.Format(fmt_string), val)
        @test fmtd == ans
    end
end

@testset "basics" begin

    @test Scanf.@sprintf("%%") == "%"
    @test Scanf.@sprintf("1%%") == "1%"
    @test Scanf.@sprintf("%%1") == "%1"
    @test Scanf.@sprintf("1%%2") == "1%2"
    @test Scanf.@sprintf("1%%%d", 2) == "1%2"
    @test Scanf.@sprintf("1%%2%%3") == "1%2%3"
    @test Scanf.@sprintf("GAP[%%]") == "GAP[%]"
    @test Scanf.@sprintf("hey there") == "hey there"
    @test_throws ArgumentError Scanf.Format("")
    @test_throws ArgumentError Scanf.Format("%+")
    @test_throws ArgumentError Scanf.Format("%.")
    @test_throws ArgumentError Scanf.Format("%.0")
    @test isempty(Scanf.Format("%%").formats)
    @test Scanf.@sprintf("%d%d", 1, 2) == "12"
    @test (Scanf.@sprintf "%d%d" [1 2]...) == "12"
    @test (Scanf.@sprintf("X%d", 2)) == "X2"
    @test (Scanf.@sprintf("\u00d0%d", 2)) == "\u00d02"
    @test (Scanf.@sprintf("\u0f00%d", 2)) == "\u0f002"
    @test (Scanf.@sprintf("\U0001ffff%d", 2)) == "\U0001ffff2"
    @test (Scanf.@sprintf("%dX%d", 1, 2)) == "1X2"
    @test (Scanf.@sprintf("%d\u00d0%d", 1, 2)) == "1\u00d02"
    @test (Scanf.@sprintf("%d\u0f00%d", 1, 2)) == "1\u0f002"
    @test (Scanf.@sprintf("%d\U0001ffff%d", 1, 2)) == "1\U0001ffff2"
    @test (Scanf.@sprintf("%d\u2203%d\u0203", 1, 2)) == "1\u22032\u0203"
    @test_throws ArgumentError Scanf.Format("%y%d")
    @test_throws ArgumentError Scanf.Format("%\u00d0%d")
    @test_throws ArgumentError Scanf.Format("%\u0f00%d")
    @test_throws ArgumentError Scanf.Format("%\U0001ffff%d")
    @test Scanf.@sprintf("%10.5d", 4) == "     00004"
    @test (Scanf.@sprintf "%d" typemax(Int64)) == "9223372036854775807"

    for (fmt, val) in (("%7.2f", "   1.23"),
                   ("%-7.2f", "1.23   "),
                   ("%07.2f", "0001.23"),
                   ("%.0f", "1"),
                   ("%#.0f", "1."),
                   ("%.4e", "1.2345e+00"),
                   ("%.4E", "1.2345E+00"),
                   ("%.2a", "0x1.3cp+0"),
                   ("%.2A", "0X1.3CP+0")),
        num in (1.2345, big"1.2345")
        @test Scanf.format(Scanf.Format(fmt), num) == val
    end

    for (fmt, val) in (("%i", "42"),
                   ("%u", "42"),
                   ("Test: %i", "Test: 42"),
                   ("%#x", "0x2a"),
                   ("%x", "2a"),
                   ("%X", "2A"),
                   ("% i", " 42"),
                   ("%+i", "+42"),
                   ("%4i", "  42"),
                   ("%-4i", "42  "),
                   ("%f", "42.000000"),
                   ("%g", "42"),
                   ("%e", "4.200000e+01")),
        num in (UInt16(42), UInt32(42), UInt64(42), UInt128(42),
                Int16(42), Int32(42), Int64(42), Int128(42), big"42")
        @test Scanf.format(Scanf.Format(fmt), num) == val
    end

    for i in (
            (42, "", "i", "42", ""),
            (42, "", "d", "42", ""),

            (42, "", "u", "42", ""),
            (42, "", "x", "2a", ""),
            (42, "", "X", "2A", ""),
            (42, "", "o", "52", ""),

            (42, "#", "x", "2a", "0x"),
            (42, "#", "X", "2A", "0X"),
            (42, "#", "o", "052", ""),

            (1.2345, "", ".2f", "1.23", ""),
            (1.2345, "", ".2e", "1.23e+00", ""),
            (1.2345, "", ".2E", "1.23E+00", ""),

            (1.2345, "#", ".0f", "1.", ""),
            (1.2345, "#", ".0e", "1.e+00", ""),
            (1.2345, "#", ".0E", "1.E+00", ""),

            (1.2345, "", ".2a", "1.3cp+0", "0x"),
            (1.2345, "", ".2A", "1.3CP+0", "0X"),
        )
        _test_flags(i...)
        _test_flags(-i[1], i[2:5]...)
    end

    # reasonably complex
    @test (Scanf.@sprintf "Test: %s%c%C%c%#-.0f." "t" 65 66 67 -42) == "Test: tABC-42.."

    # combo
    @test (Scanf.@sprintf "%f %d %d %f" 1.0 [3 4]... 5) == "1.000000 3 4 5.000000"

    # multi
    @test (Scanf.@sprintf "%s %f %9.5f %d %d %d %d%d%d%d" [1:6;]... [7,8,9,10]...) == "1 2.000000   3.00000 4 5 6 78910"

    # comprehension
    @test (Scanf.@sprintf "%s %s %s %d %d %d %f %f %f" Any[10^x+y for x=1:3,y=1:3 ]...) == "11 101 1001 12 102 1002 13.000000 103.000000 1003.000000"

    # more than 16 formats/args
    @test (Scanf.@sprintf "%s %s %s %d %d %d %f %f %f %s %s %s %d %d %d %f %f %f" Any[10*x+(x+1) for x=1:18 ]...) ==
        "12 23 34 45 56 67 78.000000 89.000000 100.000000 111 122 133 144 155 166 177.000000 188.000000 199.000000"

    # Check bug with trailing nul printing BigFloat
    @test (Scanf.@sprintf("%.330f", BigFloat(1)))[end] != '\0'

    # Check bugs with truncated output printing BigFloat
    @test (Scanf.@sprintf("%f", parse(BigFloat, "1e400"))) ==
           "10000000000000000000000000000000000000000000000000000000000000000000000000000025262527574416492004687051900140830217136998040684679611623086405387447100385714565637522507383770691831689647535911648520404034824470543643098638520633064715221151920028135130764414460468236314621044034960475540018328999334468948008954289495190631358190153259681118693204411689043999084305348398480210026863210192871358464.000000"

    # Check that does not attempt to output incredibly large amounts of digits
    @test_throws ErrorException Scanf.@sprintf("%f", parse(BigFloat, "1e99999"))

    # Check bug with precision > length of string
    @test Scanf.@sprintf("%4.2s", "a") == "   a"

    # issue #29662
    @test (Scanf.@sprintf "%12.3e" pi*1e100) == "  3.142e+100"

    @test string(Scanf.Format("%a").formats[1]) == "%a"
    @test string(Scanf.Format("%a").formats[1]; modifier="R") == "%Ra"

    @test Scanf.@sprintf("%d", 3.14) == "3"
    @test Scanf.@sprintf("%2d", 3.14) == " 3"
    @test Scanf.@sprintf("%2d", big(3.14)) == " 3"
    @test Scanf.@sprintf("%s", 1) == "1"
    @test Scanf.@sprintf("%f", 1) == "1.000000"
    @test Scanf.@sprintf("%e", 1) == "1.000000e+00"
    @test Scanf.@sprintf("%g", 1) == "1"

    # issue #39748
    @test Scanf.@sprintf("%.16g", 194.4778127560983) == "194.4778127560983"
    @test Scanf.@sprintf("%.17g", 194.4778127560983) == "194.4778127560983"
    @test Scanf.@sprintf("%.18g", 194.4778127560983) == "194.477812756098302"
    @test Scanf.@sprintf("%.1g", 1.7976931348623157e308) == "2e+308"
    @test Scanf.@sprintf("%.2g", 1.7976931348623157e308) == "1.8e+308"
    @test Scanf.@sprintf("%.3g", 1.7976931348623157e308) == "1.8e+308"

    # escaped '%'
    @test_throws ArgumentError @sprintf("%s%%%s", "a")
    @test @sprintf("%s%%%s", "a", "b") == "a%b"

    # print float as %d uses round(x)
    @test @sprintf("%d", 25.5) == "26"

    # 37539
    @test @sprintf(" %.1e\n", 0.999) == " 1.0e+00\n"
    @test @sprintf("   %.1f", 9.999) == "   10.0"

    # 37552
    @test @sprintf("%d", 1.0e100) == "10000000000000000159028911097599180468360808563945281389781327557747838772170381060813469985856815104"
    @test @sprintf("%d", 3//1) == "3"
    @test @sprintf("%d", Inf) == "Inf"
    @test @sprintf(" %d", NaN) == " NaN"
end

@testset "integers" begin

    @test Scanf.@sprintf( "% d",  42) == " 42"
    @test Scanf.@sprintf( "% d", -42) == "-42"
    @test Scanf.@sprintf( "% 5d",  42) == "   42"
    @test Scanf.@sprintf( "% 5d", -42) == "  -42"
    @test Scanf.@sprintf( "% 15d",  42) == "             42"
    @test Scanf.@sprintf( "% 15d", -42) == "            -42"
    @test Scanf.@sprintf("%+d",  42) == "+42"
    @test Scanf.@sprintf("%+d", -42) == "-42"
    @test Scanf.@sprintf("%+5d",  42) == "  +42"
    @test Scanf.@sprintf("%+5d", -42) == "  -42"
    @test Scanf.@sprintf("%+15d",  42) == "            +42"
    @test Scanf.@sprintf("%+15d", -42) == "            -42"
    @test Scanf.@sprintf( "%0d",  42) == "42"
    @test Scanf.@sprintf( "%0d", -42) == "-42"
    @test Scanf.@sprintf( "%05d",  42) == "00042"
    @test Scanf.@sprintf( "%05d", -42) == "-0042"
    @test Scanf.@sprintf( "%015d",  42) == "000000000000042"
    @test Scanf.@sprintf( "%015d", -42) == "-00000000000042"
    @test Scanf.@sprintf("%-d",  42) == "42"
    @test Scanf.@sprintf("%-d", -42) == "-42"
    @test Scanf.@sprintf("%-5d",  42) == "42   "
    @test Scanf.@sprintf("%-5d", -42) == "-42  "
    @test Scanf.@sprintf("%-15d",  42) == "42             "
    @test Scanf.@sprintf("%-15d", -42) == "-42            "
    @test Scanf.@sprintf("%-0d",  42) == "42"
    @test Scanf.@sprintf("%-0d", -42) == "-42"
    @test Scanf.@sprintf("%-05d",  42) == "42   "
    @test Scanf.@sprintf("%-05d", -42) == "-42  "
    @test Scanf.@sprintf("%-015d",  42) == "42             "
    @test Scanf.@sprintf("%-015d", -42) == "-42            "
    @test Scanf.@sprintf( "%0-d",  42) == "42"
    @test Scanf.@sprintf( "%0-d", -42) == "-42"
    @test Scanf.@sprintf( "%0-5d",  42) == "42   "
    @test Scanf.@sprintf( "%0-5d", -42) == "-42  "
    @test Scanf.@sprintf( "%0-15d",  42) == "42             "
    @test Scanf.@sprintf( "%0-15d", -42) == "-42            "
    @test_throws ArgumentError Scanf.Format("%d %")

    @test Scanf.@sprintf("%lld", 18446744065119617025) == "18446744065119617025"
    @test Scanf.@sprintf("%+8lld", 100) == "    +100"
    @test Scanf.@sprintf("%+.8lld", 100) == "+00000100"
    @test Scanf.@sprintf("%+10.8lld", 100) == " +00000100"
    @test_throws ArgumentError Scanf.Format("%_1lld")
    @test Scanf.@sprintf("%-1.5lld", -100) == "-00100"
    @test Scanf.@sprintf("%5lld", 100) == "  100"
    @test Scanf.@sprintf("%5lld", -100) == " -100"
    @test Scanf.@sprintf("%-5lld", 100) == "100  "
    @test Scanf.@sprintf("%-5lld", -100) == "-100 "
    @test Scanf.@sprintf("%-.5lld", 100) == "00100"
    @test Scanf.@sprintf("%-.5lld", -100) == "-00100"
    @test Scanf.@sprintf("%-8.5lld", 100) == "00100   "
    @test Scanf.@sprintf("%-8.5lld", -100) == "-00100  "
    @test Scanf.@sprintf("%05lld", 100) == "00100"
    @test Scanf.@sprintf("%05lld", -100) == "-0100"
    @test Scanf.@sprintf("% lld", 100) == " 100"
    @test Scanf.@sprintf("% lld", -100) == "-100"
    @test Scanf.@sprintf("% 5lld", 100) == "  100"
    @test Scanf.@sprintf("% 5lld", -100) == " -100"
    @test Scanf.@sprintf("% .5lld", 100) == " 00100"
    @test Scanf.@sprintf("% .5lld", -100) == "-00100"
    @test Scanf.@sprintf("% 8.5lld", 100) == "   00100"
    @test Scanf.@sprintf("% 8.5lld", -100) == "  -00100"
    @test Scanf.@sprintf("%.0lld", 0) == "0"
    @test Scanf.@sprintf("%#+21.18llx", -100) == "-0x000000000000000064"
    @test Scanf.@sprintf("%#.25llo", -100) == "-00000000000000000000000144"
    @test Scanf.@sprintf("%#+24.20llo", -100) == "  -000000000000000000144"
    @test Scanf.@sprintf("%#+18.21llX", -100) == "-0X000000000000000000064"
    @test Scanf.@sprintf("%#+20.24llo", -100) == "-0000000000000000000000144"
    @test Scanf.@sprintf("%#+25.22llu", -1) == "  -0000000000000000000001"
    @test Scanf.@sprintf("%#+25.22llu", -1) == "  -0000000000000000000001"
    @test Scanf.@sprintf("%#+30.25llu", -1) == "    -0000000000000000000000001"
    @test Scanf.@sprintf("%+#25.22lld", -1) == "  -0000000000000000000001"
    @test Scanf.@sprintf("%#-8.5llo", 100) == "000144  "
    @test Scanf.@sprintf("%#-+ 08.5lld", 100) == "+00100  "
    @test Scanf.@sprintf("%#-+ 08.5lld", 100) == "+00100  "
    @test Scanf.@sprintf("%.40lld",  1) == "0000000000000000000000000000000000000001"
    @test Scanf.@sprintf("% .40lld",  1) == " 0000000000000000000000000000000000000001"
    @test Scanf.@sprintf("% .40d",  1) == " 0000000000000000000000000000000000000001"
    @test Scanf.@sprintf("%lld",  18446744065119617025) == "18446744065119617025"

    @test Scanf.@sprintf("+%d+",  10) == "+10+"
    @test Scanf.@sprintf("%#012x",  1) == "0x0000000001"
    @test Scanf.@sprintf("%#04.8x",  1) == "0x00000001"

    @test Scanf.@sprintf("%#-08.2x",  1) == "0x01    "
    @test Scanf.@sprintf("%#08o",  1) == "00000001"
    @test Scanf.@sprintf("%d",  1024) == "1024"
    @test Scanf.@sprintf("%d", -1024) == "-1024"
    @test Scanf.@sprintf("%i",  1024) == "1024"
    @test Scanf.@sprintf("%i", -1024) == "-1024"
    @test Scanf.@sprintf("%u",  1024) == "1024"
    @test Scanf.@sprintf("%u",  UInt(4294966272)) == "4294966272"
    @test Scanf.@sprintf("%o",  511) == "777"
    @test Scanf.@sprintf("%o",  UInt(4294966785)) == "37777777001"
    @test Scanf.@sprintf("%x",  305441741) == "1234abcd"
    @test Scanf.@sprintf("%x",  UInt(3989525555)) == "edcb5433"
    @test Scanf.@sprintf("%X",  305441741) == "1234ABCD"
    @test Scanf.@sprintf("%X",  UInt(3989525555)) == "EDCB5433"
    @test Scanf.@sprintf("%+d",  1024) == "+1024"
    @test Scanf.@sprintf("%+d", -1024) == "-1024"
    @test Scanf.@sprintf("%+i",  1024) == "+1024"
    @test Scanf.@sprintf("%+i", -1024) == "-1024"
    @test Scanf.@sprintf("%+u",  1024) == "+1024"
    @test Scanf.@sprintf("%+u",  UInt(4294966272)) == "+4294966272"
    @test Scanf.@sprintf("%+o",  511) == "+777"
    @test Scanf.@sprintf("%+o",  UInt(4294966785)) == "+37777777001"
    @test Scanf.@sprintf("%+x",  305441741) == "+1234abcd"
    @test Scanf.@sprintf("%+x",  UInt(3989525555)) == "+edcb5433"
    @test Scanf.@sprintf("%+X",  305441741) == "+1234ABCD"
    @test Scanf.@sprintf("%+X",  UInt(3989525555)) == "+EDCB5433"
    @test Scanf.@sprintf("% d",  1024) == " 1024"
    @test Scanf.@sprintf("% d", -1024) == "-1024"
    @test Scanf.@sprintf("% i",  1024) == " 1024"
    @test Scanf.@sprintf("% i", -1024) == "-1024"
    @test Scanf.@sprintf("% u",  1024) == " 1024"
    @test Scanf.@sprintf("% u",  UInt(4294966272)) == " 4294966272"
    @test Scanf.@sprintf("% o",  511) == " 777"
    @test Scanf.@sprintf("% o",  UInt(4294966785)) == " 37777777001"
    @test Scanf.@sprintf("% x",  305441741) == " 1234abcd"
    @test Scanf.@sprintf("% x",  UInt(3989525555)) == " edcb5433"
    @test Scanf.@sprintf("% X",  305441741) == " 1234ABCD"
    @test Scanf.@sprintf("% X",  UInt(3989525555)) == " EDCB5433"
    @test Scanf.@sprintf("%+ d",  1024) == "+1024"
    @test Scanf.@sprintf("%+ d", -1024) == "-1024"
    @test Scanf.@sprintf("%+ i",  1024) == "+1024"
    @test Scanf.@sprintf("%+ i", -1024) == "-1024"
    @test Scanf.@sprintf("%+ u",  1024) == "+1024"
    @test Scanf.@sprintf("%+ u",  UInt(4294966272)) == "+4294966272"
    @test Scanf.@sprintf("%+ o",  511) == "+777"
    @test Scanf.@sprintf("%+ o",  UInt(4294966785)) == "+37777777001"
    @test Scanf.@sprintf("%+ x",  305441741) == "+1234abcd"
    @test Scanf.@sprintf("%+ x",  UInt(3989525555)) == "+edcb5433"
    @test Scanf.@sprintf("%+ X",  305441741) == "+1234ABCD"
    @test Scanf.@sprintf("%+ X",  UInt(3989525555)) == "+EDCB5433"
    @test Scanf.@sprintf("%#o",  511) == "0777"
    @test Scanf.@sprintf("%#o",  UInt(4294966785)) == "037777777001"
    @test Scanf.@sprintf("%#x",  305441741) == "0x1234abcd"
    @test Scanf.@sprintf("%#x",  UInt(3989525555)) == "0xedcb5433"
    @test Scanf.@sprintf("%#X",  305441741) == "0X1234ABCD"
    @test Scanf.@sprintf("%#X",  UInt(3989525555)) == "0XEDCB5433"
    @test Scanf.@sprintf("%#o",  UInt(0)) == "00"
    @test Scanf.@sprintf("%#x",  UInt(0)) == "0x0"
    @test Scanf.@sprintf("%#X",  UInt(0)) == "0X0"
    @test Scanf.@sprintf("%1d",  1024) == "1024"
    @test Scanf.@sprintf("%1d", -1024) == "-1024"
    @test Scanf.@sprintf("%1i",  1024) == "1024"
    @test Scanf.@sprintf("%1i", -1024) == "-1024"
    @test Scanf.@sprintf("%1u",  1024) == "1024"
    @test Scanf.@sprintf("%1u",  UInt(4294966272)) == "4294966272"
    @test Scanf.@sprintf("%1o",  511) == "777"
    @test Scanf.@sprintf("%1o",  UInt(4294966785)) == "37777777001"
    @test Scanf.@sprintf("%1x",  305441741) == "1234abcd"
    @test Scanf.@sprintf("%1x",  UInt(3989525555)) == "edcb5433"
    @test Scanf.@sprintf("%1X",  305441741) == "1234ABCD"
    @test Scanf.@sprintf("%1X",  UInt(3989525555)) == "EDCB5433"
    @test Scanf.@sprintf("%20d",  1024) == "                1024"
    @test Scanf.@sprintf("%20d", -1024) == "               -1024"
    @test Scanf.@sprintf("%20i",  1024) == "                1024"
    @test Scanf.@sprintf("%20i", -1024) == "               -1024"
    @test Scanf.@sprintf("%20u",  1024) == "                1024"
    @test Scanf.@sprintf("%20u",  UInt(4294966272)) == "          4294966272"
    @test Scanf.@sprintf("%20o",  511) == "                 777"
    @test Scanf.@sprintf("%20o",  UInt(4294966785)) == "         37777777001"
    @test Scanf.@sprintf("%20x",  305441741) == "            1234abcd"
    @test Scanf.@sprintf("%20x",  UInt(3989525555)) == "            edcb5433"
    @test Scanf.@sprintf("%20X",  305441741) == "            1234ABCD"
    @test Scanf.@sprintf("%20X",  UInt(3989525555)) == "            EDCB5433"
    @test Scanf.@sprintf("%-20d",  1024) == "1024                "
    @test Scanf.@sprintf("%-20d", -1024) == "-1024               "
    @test Scanf.@sprintf("%-20i",  1024) == "1024                "
    @test Scanf.@sprintf("%-20i", -1024) == "-1024               "
    @test Scanf.@sprintf("%-20u",  1024) == "1024                "
    @test Scanf.@sprintf("%-20u",  UInt(4294966272)) == "4294966272          "
    @test Scanf.@sprintf("%-20o",  511) == "777                 "
    @test Scanf.@sprintf("%-20o",  UInt(4294966785)) == "37777777001         "
    @test Scanf.@sprintf("%-20x",  305441741) == "1234abcd            "
    @test Scanf.@sprintf("%-20x",  UInt(3989525555)) == "edcb5433            "
    @test Scanf.@sprintf("%-20X",  305441741) == "1234ABCD            "
    @test Scanf.@sprintf("%-20X",  UInt(3989525555)) == "EDCB5433            "
    @test Scanf.@sprintf("%020d",  1024) == "00000000000000001024"
    @test Scanf.@sprintf("%020d", -1024) == "-0000000000000001024"
    @test Scanf.@sprintf("%020i",  1024) == "00000000000000001024"
    @test Scanf.@sprintf("%020i", -1024) == "-0000000000000001024"
    @test Scanf.@sprintf("%020u",  1024) == "00000000000000001024"
    @test Scanf.@sprintf("%020u",  UInt(4294966272)) == "00000000004294966272"
    @test Scanf.@sprintf("%020o",  511) == "00000000000000000777"
    @test Scanf.@sprintf("%020o",  UInt(4294966785)) == "00000000037777777001"
    @test Scanf.@sprintf("%020x",  305441741) == "0000000000001234abcd"
    @test Scanf.@sprintf("%020x",  UInt(3989525555)) == "000000000000edcb5433"
    @test Scanf.@sprintf("%020X",  305441741) == "0000000000001234ABCD"
    @test Scanf.@sprintf("%020X",  UInt(3989525555)) == "000000000000EDCB5433"
    @test Scanf.@sprintf("%#20o",  511) == "                0777"
    @test Scanf.@sprintf("%#20o",  UInt(4294966785)) == "        037777777001"
    @test Scanf.@sprintf("%#20x",  305441741) == "          0x1234abcd"
    @test Scanf.@sprintf("%#20x",  UInt(3989525555)) == "          0xedcb5433"
    @test Scanf.@sprintf("%#20X",  305441741) == "          0X1234ABCD"
    @test Scanf.@sprintf("%#20X",  UInt(3989525555)) == "          0XEDCB5433"
    @test Scanf.@sprintf("%#020o",  511) == "00000000000000000777"
    @test Scanf.@sprintf("%#020o",  UInt(4294966785)) == "00000000037777777001"
    @test Scanf.@sprintf("%#020x",  305441741) == "0x00000000001234abcd"
    @test Scanf.@sprintf("%#020x",  UInt(3989525555)) == "0x0000000000edcb5433"
    @test Scanf.@sprintf("%#020X",  305441741) == "0X00000000001234ABCD"
    @test Scanf.@sprintf("%#020X",  UInt(3989525555)) == "0X0000000000EDCB5433"
    @test Scanf.@sprintf("%0-20d",  1024) == "1024                "
    @test Scanf.@sprintf("%0-20d", -1024) == "-1024               "
    @test Scanf.@sprintf("%0-20i",  1024) == "1024                "
    @test Scanf.@sprintf("%0-20i", -1024) == "-1024               "
    @test Scanf.@sprintf("%0-20u",  1024) == "1024                "
    @test Scanf.@sprintf("%0-20u",  UInt(4294966272)) == "4294966272          "
    @test Scanf.@sprintf("%-020o",  511) == "777                 "
    @test Scanf.@sprintf("%-020o",  UInt(4294966785)) == "37777777001         "
    @test Scanf.@sprintf("%-020x",  305441741) == "1234abcd            "
    @test Scanf.@sprintf("%-020x",  UInt(3989525555)) == "edcb5433            "
    @test Scanf.@sprintf("%-020X",  305441741) == "1234ABCD            "
    @test Scanf.@sprintf("%-020X",  UInt(3989525555)) == "EDCB5433            "
    @test Scanf.@sprintf("%.20d",  1024) == "00000000000000001024"
    @test Scanf.@sprintf("%.20d", -1024) == "-00000000000000001024"
    @test Scanf.@sprintf("%.20i",  1024) == "00000000000000001024"
    @test Scanf.@sprintf("%.20i", -1024) == "-00000000000000001024"
    @test Scanf.@sprintf("%.20u",  1024) == "00000000000000001024"
    @test Scanf.@sprintf("%.20u",  UInt(4294966272)) == "00000000004294966272"
    @test Scanf.@sprintf("%.20o",  511) == "00000000000000000777"
    @test Scanf.@sprintf("%.20o",  UInt(4294966785)) == "00000000037777777001"
    @test Scanf.@sprintf("%.20x",  305441741) == "0000000000001234abcd"
    @test Scanf.@sprintf("%.20x",  UInt(3989525555)) == "000000000000edcb5433"
    @test Scanf.@sprintf("%.20X",  305441741) == "0000000000001234ABCD"
    @test Scanf.@sprintf("%.20X",  UInt(3989525555)) == "000000000000EDCB5433"
    @test Scanf.@sprintf("%20.5d",  1024) == "               01024"
    @test Scanf.@sprintf("%20.5d", -1024) == "              -01024"
    @test Scanf.@sprintf("%20.5i",  1024) == "               01024"
    @test Scanf.@sprintf("%20.5i", -1024) == "              -01024"
    @test Scanf.@sprintf("%20.5u",  1024) == "               01024"
    @test Scanf.@sprintf("%20.5u",  UInt(4294966272)) == "          4294966272"
    @test Scanf.@sprintf("%20.5o",  511) == "               00777"
    @test Scanf.@sprintf("%20.5o",  UInt(4294966785)) == "         37777777001"
    @test Scanf.@sprintf("%20.5x",  305441741) == "            1234abcd"
    @test Scanf.@sprintf("%20.10x",  UInt(3989525555)) == "          00edcb5433"
    @test Scanf.@sprintf("%20.5X",  305441741) == "            1234ABCD"
    @test Scanf.@sprintf("%20.10X",  UInt(3989525555)) == "          00EDCB5433"
    @test Scanf.@sprintf("%020.5d",  1024) == "               01024"
    @test Scanf.@sprintf("%020.5d", -1024) == "              -01024"
    @test Scanf.@sprintf("%020.5i",  1024) == "               01024"
    @test Scanf.@sprintf("%020.5i", -1024) == "              -01024"
    @test Scanf.@sprintf("%020.5u",  1024) == "               01024"
    @test Scanf.@sprintf("%020.5u",  UInt(4294966272)) == "          4294966272"
    @test Scanf.@sprintf("%020.5o",  511) == "               00777"
    @test Scanf.@sprintf("%020.5o",  UInt(4294966785)) == "         37777777001"
    @test Scanf.@sprintf("%020.5x",  305441741) == "            1234abcd"
    @test Scanf.@sprintf("%020.10x",  UInt(3989525555)) == "          00edcb5433"
    @test Scanf.@sprintf("%020.5X",  305441741) == "            1234ABCD"
    @test Scanf.@sprintf("%020.10X",  UInt(3989525555)) == "          00EDCB5433"
    @test Scanf.@sprintf("%20.0d",  1024) == "                1024"
    @test Scanf.@sprintf("%20.d", -1024) == "               -1024"
    @test Scanf.@sprintf("%20.d",  0) == "                   0"
    @test Scanf.@sprintf("%20.0i",  1024) == "                1024"
    @test Scanf.@sprintf("%20.i", -1024) == "               -1024"
    @test Scanf.@sprintf("%20.i",  0) == "                   0"
    @test Scanf.@sprintf("%20.u",  1024) == "                1024"
    @test Scanf.@sprintf("%20.0u",  UInt(4294966272)) == "          4294966272"
    @test Scanf.@sprintf("%20.u",  UInt(0)) == "                   0"
    @test Scanf.@sprintf("%20.o",  511) == "                 777"
    @test Scanf.@sprintf("%20.0o",  UInt(4294966785)) == "         37777777001"
    @test Scanf.@sprintf("%20.o",  UInt(0)) == "                   0"
    @test Scanf.@sprintf("%20.x",  305441741) == "            1234abcd"
    @test Scanf.@sprintf("%20.0x",  UInt(3989525555)) == "            edcb5433"
    @test Scanf.@sprintf("%20.x",  UInt(0)) == "                   0"
    @test Scanf.@sprintf("%20.X",  305441741) == "            1234ABCD"
    @test Scanf.@sprintf("%20.0X",  UInt(3989525555)) == "            EDCB5433"
    @test Scanf.@sprintf("%20.X",  UInt(0)) == "                   0"

end

@testset "%n" begin
    x = Ref{Int}()
    @test (Scanf.@sprintf("%d4%n", 123, x); x[] == 4)
    @test (Scanf.@sprintf("%s%n", "😉", x); x[] == 4)
    @test (Scanf.@sprintf("%s%n", "1234", x); x[] == 4)
end
=#

end # @testset "Scanf"
