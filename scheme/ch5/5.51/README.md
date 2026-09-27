# Exercise 5.51: a Scheme interpreter in Rust

The explicit-control evaluator of [section 5.4](https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-34.html)
translated into Rust, with the list-structured memory and stop-and-copy
garbage collector of [section 5.3](https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-33.html)
as its storage allocation.

## Building

It needs a Rust toolchain with `cargo` and has no dependencies.

```sh
cargo build --release
```

This builds the interpreter as `target/release/scheme`.

## Running

The interpreter reads a program from the file given as its argument, or from
standard input without one. It prints the value of each expression it reads,
like the driver loop of section 5.4.4:

```sh
$ echo '(define (square x) (* x x)) (square 12) (/ 1 2)' | target/release/scheme
ok
144
.5
```

To run a program from a file:

```sh
$ cat factorial.scm
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(display "20! = ")
(display (factorial 20))
(newline)
$ target/release/scheme factorial.scm
ok
20! = 2432902008176640000
```

Run it without arguments in a terminal to type expressions one at a time.
There is no prompt, and each value is printed as soon as its expression is
complete. Press Ctrl-D to quit.

`cargo run --release` builds and starts the interpreter in one step; a file
to run goes after `--`, as in `cargo run --release -- factorial.scm`.

### Errors

An error is reported on standard error after a semicolon, and the interpreter
goes on with the next expression. If any expression failed, it exits with
status 1 at the end:

```sh
$ echo "(car '()) 'next" | target/release/scheme
;The object passed to car is not a pair: ()
next
$ echo $?
1
```

## The language

- **Special forms**: `quote` (and `'`), `define`, `set!`, `if`, `lambda`
  (including a rest parameter, as in `(lambda (a . rest) ...)`), `begin`,
  `cond` with `else`, and `let`.
- **Data**: integers, decimals, strings, symbols, booleans (`#t`, `#f`, and
  the variables `true` and `false`), pairs and lists.
- **Primitives**:
  - pairs and lists: `car`, `cdr`, `cons`, `set-car!`, `set-cdr!`, `list`,
    `length`, and `caar` to `cadddr`;
  - numbers: `+`, `-`, `*`, `/`, `quotient`, `remainder`, `abs`, `=`, `<`,
    `>`, `<=`, `>=`;
  - predicates: `null?`, `pair?`, `number?`, `symbol?`, `string?`,
    `procedure?`, `eq?`, `equal?`, `not`;
  - output and errors: `display`, `newline`, `error`.

Integers stay exact until a result overflows or is a fraction, and then
become decimals: `(/ 6 3)` is `2` and `(/ 1 2)` is `.5`.

The machine has room for 2^18 pairs and 100,000 values on its stack. Tail
calls run in constant space, so an iterative process can loop for as long as
it likes. A deeper recursion aborts with `;Aborting!: maximum recursion
depth exceeded`, and more live data than the memory holds aborts with
`;Aborting!: out of memory`. Either way the interpreter goes on with the next
expression.

## Testing

```sh
cargo test
```

This runs the unit tests at the end of each module in `src/` and the
integration tests in `tests/`, which run the interpreter as a program. From
the root of the repository, `scheme/run-tests.sh ch5/5.51.scm` builds the
interpreter and runs these tests together with the checks of
`scheme/ch5/5.51.scm`.

## Source

| File | Contents |
|---|---|
| `src/main.rs` | The driver loop |
| `src/eval.rs` | The controller of the explicit-control evaluator |
| `src/machine.rs` | Values, registers, stack, memory and the garbage collector |
| `src/environment.rs` | Environments |
| `src/primitives.rs` | The primitive procedures |
| `src/reader.rs` | The reader |
| `src/printer.rs` | The printer |
