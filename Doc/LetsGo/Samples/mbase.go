/*
 * Test sending mobile code around
 */

base .. {
  include "sys:go/io.gof".
  include "sys:go/stdlib.gof".
  include "mobile.gh".

  base() ->
      ( A<~mbAgent[] << _ -> spawn { A.exec(stdout)}
      );base().
}.
