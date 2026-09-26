# Conventions for the SICP exercise solutions

## Layout

- One file per exercise: `scheme/chN/N.M.scm` (e.g. `scheme/ch3/3.17.scm`).
  Exercises that are parts of one program may share a file named like the
  existing `scheme/ch2/2.83.84.85.scm`, but prefer one file per exercise.
- Book code shared by several exercises goes into `scheme/chN/lib/<name>.scm`.
  Library files contain no checks. Write them yourself, keeping the book's
  procedure names where exercises refer to them.
- All paths in `load` are relative to the `scheme/` directory; every file is
  run from there:

  ```sh
  cd scheme && ./run-tests.sh ch3/3.17.scm ch3/3.18.scm   # selected files
  cd scheme && ./run-tests.sh                             # everything
  ```

## Shared libraries (read-only — do not edit; redefine in your own file instead)

- `scheme/lib/check.scm` — tests: `(check expr => expected)` (equal?),
  `(check expr (=> same?) expected)`, `(check-error expr)`, `(approx= tol)`.
- `scheme/ch4/lib/mceval.scm` — the 4.1 metacircular evaluator. The book's
  `eval`/`apply` are named **`mc-eval`/`mc-apply`** so MIT Scheme's own `eval`
  and `apply` stay intact. `(interpret exp ...)` evaluates expressions in a
  fresh environment and returns the last value. `primitive-procedures`,
  `setup-environment`, `the-global-environment` are as in the book.
- `scheme/ch5/lib/regsim.scm` — the 5.2 simulator with the monitored stack of
  5.2.4. Extra: `(stack-statistics machine)` returns
  `((total-pushes . n) (maximum-depth . d))`. `make-execution-procedure`
  dispatches with `case`.
- `scheme/ch5/lib/eceval.scm` — explicit-control evaluator built from code
  sections: `eceval-dispatch-table` (list of `(predicate label)`, applications
  are always tried last), `simple-expressions-code`, `application-code`,
  `apply-dispatch-code`, `sequence-code`, `conditional-code`,
  `assignment-code`, `error-code`, `eceval-operations`.
  `(make-eceval dispatch-table extra-code extra-operations)` builds a variant;
  `(eceval-run machine exp ...)` evaluates in a fresh global environment and
  returns the last value. `eceval` is the default machine.
- `scheme/ch5/lib/compiler.scm` — the 5.5 compiler.
  `(compile exp target linkage)`, `(statements seq)`,
  `(make-compiled-machine exp ...)`, `(compile-and-run exp ...)`,
  `compiled-code-operations`.

Read these files before you start; they are short.

## Every solution file

```scheme
(load "lib/check.scm")
(load "ch3/lib/streams.scm")          ; only when needed

(define (count-pairs x)
  ...)

(check (count-pairs '(a b c)) => 3)
(check-error (count-pairs 'a))
```

- Starts with `(load "lib/check.scm")`, then the libraries it needs.
- Has at least one `check`; tests are small, deterministic and cover the
  interesting cases, including edge cases. A failing check aborts the file with
  a non-zero exit, which `run-tests.sh` reports. Each file must finish within a
  few seconds.
- Exercises that ask a question, or ask for a diagram or explanation: give a
  concise answer in comments, and back it with checks wherever the claim can be
  demonstrated — count calls, compare results, measure `stack-statistics`,
  capture output with `(with-output-to-string (lambda () ...))`, and so on.
- Anything random must be tested deterministically: fixed sequences,
  tolerances, or properties that always hold.

## Style

- Idiomatic MIT Scheme in the book's style: standard Lisp indentation, no
  dangling close parens, kebab-case names, `?` for predicates, `!` for
  mutators; internal `define`s, `let`/`let*`/named `let`, `cond`/`case`,
  `assoc`/`assq`, `error` with a message and irritants. Lines under 80 columns.
- Small, well-named procedures; no dead code, no debug `display`s.
- Implement what the exercise asks for yourself, instead of calling an MIT
  built-in that does the job. Built-ins are fine for everything else.
- Comments only where they genuinely clarify: answers to the exercise's
  questions, a non-obvious invariant or trick. Don't restate the exercise text
  and don't narrate the code.
- Don't redefine built-ins that `lib/check.scm` relies on (`equal?`, `error`,
  `apply`, `with-exception-handler`, `call-with-current-continuation`, `not`,
  `abs`, `<`, `-`). When an exercise wants a procedure named like a built-in,
  and redefining it would be risky, give it a distinct name.
- MIT Scheme 12.1 has `cons-stream`, `stream-car`, `stream-cdr`,
  `the-empty-stream`, `stream-pair?`, `stream-null?` built in.

## Book code — important

Don't reproduce long passages of the book's code verbatim: the output gets
blocked ("Output blocked by content filtering policy"). Write the book's
programs in your own words, keeping the interface and the names the exercises
rely on. Write large files in several smaller pieces (for example `Write` a
first part, then append with further edits) rather than in one huge output.

## Process

- Touch only the files your step owns (see its description).
- Do not run git commands that change history or the index (commit, stash,
  reset, checkout); Zenflow commits the work automatically.
- Before you finish, run `./run-tests.sh` on all of your files; every one must
  pass. Then mark your plan step Completed.
