# scip-exercises
Solutions to the exercises from the book "Structure and Interpretation of Computer Programs" https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book.html,
written for MIT/GNU Scheme.

## Layout

- `scheme/chN/N.M.scm` — the solution to exercise N.M (a few files cover
  several closely related exercises, e.g. `2.83.84.85.scm`).
- `scheme/chN/lib/` — the book's programs that several exercises build on:
  the generic arithmetic system (ch2), queues, circuits, constraints,
  serializers and streams (ch3), the metacircular, analyzing, lazy and amb
  evaluators and the query system (ch4), the register-machine simulator,
  the explicit-control evaluator and the compiler (ch5).
- `scheme/ch5/5.51/`, `scheme/ch5/5.52/` — the Scheme interpreter in Rust
  and the compiler to C of exercises 5.51 and 5.52.
- `scheme/lib/check.scm` — a small test library:
  `(check expr => expected)`, `(check expr (=> same?) expected)`,
  `(check-error expr)`.

## Running the tests

Every solution from exercise 2.91 on carries its own checks. Run them from
the `scheme` directory, which all `load` paths are relative to:

```sh
cd scheme
./run-tests.sh                            # all solutions
./run-tests.sh ch3/3.17.scm ch4/4.6.scm   # selected solutions
```

A single file can also be run directly:

```sh
cd scheme
mit-scheme --quiet --load ch3/3.17.scm --eval '(exit)'
```

A failing check stops with an error describing the expression, the expected
and the actual value. Exercise 5.51 additionally needs `cargo`, and
exercise 5.52 `gcc` and `make`.
