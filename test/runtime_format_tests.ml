open Lg_runtime.Runtime_format

let check expected fmt args =
  let actual = format fmt args in
  if actual <> expected then failwith (Printf.sprintf "%S: expected %S, got %S" fmt expected actual)

let () =
  check "hello" "hello" [];
  check "%\n" "%%%n" [];
  check "null 3.0 true" "%s %s %s" [Nil; Decimal 3.; Boolean true];
  check "1.0E7 1.0E-4 1.2345678901234 -0.0" "%s %s %s %s"
    [Decimal 1.0e7; Decimal 0.0001; Decimal 1.2345678901234; Decimal (-0.)];
  check "false false true" "%b %b %b" [Nil; Boolean false; Text ""];
  check "   ab|xy   " "%5.2s|%-5s" [Text "abcd"; Text "xy"];
  check "   中" "%4.1s" [Text "中文"];
  check "  😀" "%4.2s" [Text "😀x"];
  check "12,345 -0000042 0x2a" "%,d %08d %#x" [Integer 12345; Integer (-42); Integer 42];
  check "1.24 1.23e+03 12.00" "%.2f %.2e %.4g" [Decimal 1.235; Decimal 1234.; Decimal 12.];
  check "1.01 2.68 1.3" "%.2f %.2f %.1f" [Decimal 1.005; Decimal 2.675; Decimal 1.25];
  check "1.01e+00 1.01" "%.2e %.3g" [Decimal 1.005; Decimal 1.005];
  check "1. 1.e+00" "%#.0f %#.0e" [Decimal 1.; Decimal 1.];
  check "1e+04" "%,.1g" [Decimal 12345.];
  List.iter (fun (value, fixed, scientific, general) ->
    check fixed "%.2f" [Decimal value];
    check scientific "%.2e" [Decimal value];
    check general "%.3g" [Decimal value])
    [9.995, "10.00", "1.00e+01", "10.0";
     99.95, "99.95", "1.00e+02", "100";
     0.00009995, "0.00", "1.00e-04", "0.000100";
     1.0e23, "100000000000000000000000.00", "1.00e+23", "1.00e+23";
     4.9e-324, "0.00", "4.90e-324", "4.90e-324";
     -0.0, "-0.00", "-0.00e+00", "-0.00"];
  check "two/two/one" "%2$s/%<s/%s" [Text "one"; Text "two"];
  check "FF 077 (42)" "%X %#o %(d" [Integer 255; Integer 63; Integer (-42)];
  check "A Z" "%c %C" [Integer 65; Character 'z'];
  check "ignored" "ignored" [Integer 1];
  List.iter (fun (fmt, args) ->
    match format fmt args with
    | _ -> failwith ("accepted invalid format " ^ fmt)
    | exception Invalid_argument _ -> ())
    ["%", []; "%q", []; "%d", []; "%d", [Text "12"];
     "%0$s", [Text "x"]; "%<s", [Text "x"]; "%--s", [Text "x"];
     "%+s", [Text "x"]; "%.2d", [Integer 1]; "%5n", []; "%-s", [Text "x"]]
