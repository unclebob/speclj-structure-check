---
name: speclj-structure-check
description: Use after every spec file edit to catch Speclj nesting errors before running tests
---

# Speclj Structure Check

## When to use

**Run after every spec file edit** — before running tests.

## How to run

```bash
clj -M:spec-structure-check <file-or-dir>
```

Optional `--tree` flag shows indented Speclj form tree with line numbers:
```bash
clj -M:spec-structure-check --tree <file-or-dir>
```

## What it checks

Speclj nesting rules:
- `(describe)` is top-level only — never nested inside another `describe` or `context`
- `(context)` can nest inside `describe` or other `context` blocks
- `(it)` is a leaf — no structural Speclj forms inside it (`describe`, `context`, `it`, `before`, `before-all`, `after`, `with-stubs`, `with`, `around`)

## Output

- `OK` — file has valid structure
- `ERROR line N: (form) inside (parent) at line M` — nesting violation

## Workflow

1. Edit a spec file
2. Run `clj -M:spec-structure-check <file>`
3. If errors: fix parens before running tests
4. If OK: run tests

## Key insight

If a test disappears from Speclj `-f d` output, it's almost certainly a paren error — an `(it ...)` accidentally nested inside another `(it ...)`. Don't debug assertions; fix parens. Run this checker to find the problem instantly.

## Setup

Add to your project's `deps.edn`:
```clojure
:spec-structure-check {:extra-deps {io.github.unclebob/speclj-structure-check
                                    {:git/url "https://github.com/unclebob/speclj-structure-check"
                                     :git/sha "..."}}
                       :main-opts ["-m" "speclj-structure-check.core"]}
```
