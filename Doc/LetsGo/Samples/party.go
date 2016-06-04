/*
 * A sample use of the directory package
 * A set of threads are spawned off, each registers a description of itself.
 * Another set of threads attempts to get different kinds of listings of the directory
 */
party{
  import directory.
  import go.io.
  import publish.
  import lister.

  main(_) ->
      spawn { publish(10) };
      spawn { lister('when') };
      spawn { lister('skills') };
      spawn { listAll()};
      delay(10).
}