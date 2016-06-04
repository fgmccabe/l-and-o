/*
 * Test sending mobile code around
 */

base .. {
  include "sys:go/io.gof".
  include "sys:go/stdlib.gof".
  include "m.gh".

  base() ->
      ( exec(P) << _ -> spawn { P(stdout)}
      );base().
}.
