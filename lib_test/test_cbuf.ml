open OUnit2

module Box = Sodium.Box

let rec hex_of_str = function
| "" -> ""
| s -> (Printf.sprintf "%02x" (int_of_char s.[0]))
    ^(hex_of_str (String.sub s 1 ((String.length s) - 1)))

let setup () =
  (Box.random_keypair (), Box.random_keypair (),
   "The rooster crows at midnight.", Box.random_nonce ())
let test_precompute ctxt =
  let ((sk,pk),(sk',pk'),message,nonce) = setup () in
  OUnit2.assert_equal (Box.precompute sk pk) (Box.precompute_ sk pk)

let suite = "Cbuf" >::: [
  "precompute" >:: test_precompute;
]
