# speclj-structure-check

A static analysis tool that catches structural errors in [Speclj](https://github.com/slagyr/speclj) spec files before you run them.

Speclj silently ignores certain nesting mistakes — like an `(it)` inside another `(it)`, or a `(describe)` inside a `(context)`. This tool parses your spec files and reports those errors with line numbers.

## Detected Errors

- `(it)` nested inside `(it)`
- `(describe)` nested inside `(describe)` or `(context)`
- `(before)`, `(with-stubs)`, `(around)`, `(with)`, `(context)` inside `(it)`
- Unclosed forms at end of file

## Usage

Requires [Clojure CLI](https://clojure.org/guides/install_clojure).

### Check a file

```bash
clj -M:check path/to/my_spec.clj
```

### Check a directory

```bash
clj -M:check spec/
```

### Show structure tree

```bash
clj -M:check --tree spec/
```

## Output

**Clean file:**
```
spec/my_spec.clj: OK
```

**Error:**
```
spec/my_spec.clj: ERROR line 5: (it) inside (it) at line 3
```

**With `--tree`:**
```
spec/my_spec.clj: OK
(describe :line 1
  (before :line 2)
  (it :line 3)
  (it :line 5))
```

Exit code is `1` if any errors are found, `0` otherwise.

## Running the Tests

```bash
clj -M:spec
```
