# LeveretLisp Language Specification

For tag-stream evaluation inside `%t leveretlisp` (and aliases).

---

## 1. Lexical Structure
- **Tokens**
  - `(` `)` as list delimiters
  - **STRING**: quoted with `"` or `'`, supports escapes `\"` `\'` `\\` `\n` `\t` `\r`
  - **IDENT**: `[A-Za-z_+\-/*?<>=!:][A-Za-z0-9_+\-/*?<>=!:]*` (colons are allowed in identifiers)
  - **QUOTE** `'`, **QUASIQUOTE** ``` ` ```, **UNQUOTE** `,` reader macros
  - **EOF**
- Whitespace is ignored outside strings. Line comments start with `;` and run to end of line.
- Multiple top-level forms are only allowed when the lexer marks `_allowMultiple` (e.g., multi-line input); otherwise extra tokens error.

## 2. AST Nodes
- **StringLiteral**: `{ type: "StringLiteral", value }`
- **Symbol**: `{ type: "Symbol", name }`
- **List**: `{ type: "List", head: ASTNode, args: ASTNode[] }`
- **Nil**: `{ type: "Nil" }`
- **Program**: `{ type: "Program", body: ASTNode[] }` (only when multiple top forms are present)

## 3. Evaluation Model
- **Strings** evaluate to their contents.
- **Symbols**: if a builtin exists, call it; else call a tag with zero args (`execTag(name, "")`).
- **Lists**: head must be a symbol; special forms handled first; otherwise evaluate args and call builtin or tag.
- **Nil** evaluates to `null`.
- Truthiness: falsey when `null`, `undefined`, `""`, `false`, `0`, or `"false"`; everything else is truthy.

### 3.1 Special Forms
- `(if cond then [else])`
- `(quote x)` — returns literal AST
- `(quasiquote x)` / `(unquote x)` — supports nesting; `unquote` inside quasiquote evaluates
- `(let ((name val) ...) body...)` — sequential bindings; `(let* ...)` is an alias
- `(set! name value)` — mutates an existing binding in the nearest scope where it is found
- `(defmacro name (params...) body...)` — defines a macro in the current env
- `(cond (test body...) ... )` — `t`/`otherwise` as default; supports arrow clauses: `(cond (test => fn))` calls `fn` with test value
- `(case key ( (k1 k2 ...) body...) ... (t default))` — matches by stringified equality; default via `t`/`otherwise`
- `(loop ...)` — Common Lisp–style subset:
  - `(loop for i from A to|below B [by STEP] collect EXPR)`; step defaults to 1 (or -1 if descending); returns list of collected results.
  - `(loop while TEST collect EXPR)` — repeats while TEST is truthy, collecting EXPR each time.

### 3.2 Macros
- Macros receive raw AST args and expand to AST.
- `gensym` builtin produces hygienic symbols.
- Expansion happens before evaluation (`macroExpand`), skipping `quote`, `quasiquote`, and `defmacro` bodies.

## 4. Builtins (selected)
- String/number:
  - `concat`, `+` (string join), `-`, `*`, `number`, `len`, `upper`, `lower`, `trim`, `substr`, `replace`, `join`, `split`, `starts_with`, `ends_with`, `includes`
  - Comparisons: `=` (stringified equality), `<`, `>`, `<=`, `>=`
- Boolean helpers: `and`, `or`, `not`, `coalesce`
- JSON: `json_parse`, `json_stringify`
- Data access: `get obj key`
- List utilities: `list`, `cons`, `car`, `cdr`, `mapcar`, `assoc`
- Mutation: `set!` special form updates an existing binding (used in loops or stateful code)
- Env access: `msg`, `util`, `tag`, `overclocking`, `http`
- HTTP wrappers: `http_request` (string URL or JSON string/object), `http_get`
- Discord helper: `unembed_desc`
- `gensym` produces a unique symbol node.
- `eq` compares symbols (by name) or stringified values.

### 4.1 Builtin Reference (quick table)
| Builtin | Purpose |
| --- | --- |
| `concat`, `+` | Join args as strings |
| `-`, `*`, `number` | Arithmetic/coercion |
| `=`, `<`, `>`, `<=`, `>=`, `eq` | Equality/ordering (`eq` is symbol/name-aware) |
| `len`, `upper`, `lower`, `trim`, `substr`, `replace`, `join`, `split`, `starts_with`, `ends_with`, `includes` | String utilities |
| `and`, `or`, `not`, `coalesce` | Boolean/flow helpers |
| `json_parse`, `json_stringify` | JSON helpers |
| `get` | Property lookup by key |
| `list`, `cons`, `car`, `cdr`, `mapcar`, `assoc` | List/functional utilities |
| `msg`, `util`, `tag`, `overclocking`, `http` | Host context accessors |
| `http_request`, `http_get` | HTTP wrappers |
| `unembed_desc` | Extract `embed.description` |
| `gensym` | Generate unique symbol |

## 5. Tag Invocation
- Calls go through `execTag(name, argsString)` (host `util.executeTagSafe`).
- Arg string building:
  - Zero args: `""`
  - One arg: `toStringValue(arg)` (strings pass through, objects JSON.stringify)
  - Multiple args: `toStringValue` for each, joined with space
- Nested tags are fully evaluated before outer calls.

## 6. JIT and Performance
- A small JIT path is enabled by default. It memoizes simple application forms (non-special forms) and constant-folds pure builtins. You can disable it by passing `{ enableJit: false }` to `evaluate` or `runLeveretLisp`.
- Special forms like `loop`, `let`, `if`, `cond`, `case`, `defmacro`, `quote` remain interpreted to preserve semantics.
- Benchmarks: `npm run bench` runs a small harness comparing JIT on/off across several scenarios.

## 6. Return Payload
- Strings => `{ content: string }`
- Objects are passed through verbatim (no wrapping). If you need `{ content, embed }`, return that shape.
- Other primitives => `{ content: JSON.stringify(value) }`

## 7. Quoting
- Reader macros: `'x` -> `(quote x)`, ``` `x ``` -> `(quasiquote x)`, `,x` -> `(unquote x)`
- `quote` returns literal AST; symbols inside quoted forms are not evaluated.
- `quasiquote` allows `unquote` to splice evaluated values.

## 8. Aliases & Code Fences
- Entry supports alias substitution: `%t alias NAME leveretlisp` sets an alias; `$` inside args is replaced with the alias string. If the alias looks like a form (`(` ``` ` ``` `'` `"`), it is injected raw; otherwise it is JSON-stringified.
- Code fences like ```...``` (optional language identifier) are stripped before lexing.

## 9. Errors
- Syntax errors: unterminated string, unmatched paren, unexpected token.
- Runtime errors: head not symbol, invalid macro expansion (must return AST), let binding shape invalid, `unquote` outside `quasiquote`, arity errors in macros, number coercion failures, unknown tag head type.
- Errors include a simple trace of evaluation frames (e.g., `Trace: (case) > >`) to show where evaluation was when the error was thrown.

## 10. Examples
- Concatenation: `(concat "a" "b" "c")` -> `"abc"`
- Zero-arg tag: `latest` -> tag `latest` with `""`
- Let binding: `(let ((a "1") (b "2")) (+ a b))` -> `"12"`
- Macro: `(defmacro twice (x) (+ x x)) (twice "ha")` -> `"haha"`
- Quasiquote: ``` `(a ,(b)) ``` splices result of `(b)` into list
- Random split with number compare: `(let ((r (randnum))) (if (< r "0.5") "low" "high"))`
