open OUnit2

let suite = "test">::: [
  "checker">::: Checker_test.suite;
  "typing">::: Typing_test.suite;
]

let () = run_test_tt_main suite
