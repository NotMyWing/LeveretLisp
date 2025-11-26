# LeveretLisp

Tiny Lisp-like tag evaluator for Leveret tags. Parse/quote/quasiquote, macros, builtins (string/number/list/http helpers), and tag invocation via `util.executeTagSafe`.

Repo: https://github.com/NotMyWing/LeveretLisp

## Usage
- Wrap expressions in `%t leveretlisp (expr ...)` or via aliases.
- Supports reader macros `'` ``` ` ``` `,`, `cond`/`case`, `let`/`let*`, macros (`defmacro` + `gensym`), and truthiness like CL.
- Builtins include `concat/+/-/*`, comparisons `= < > <= >= eq`, string helpers (`upper/lower/trim/...`), list ops (`list/cons/car/cdr/mapcar/assoc`), env access (`msg/util/tag/http/overclocking`), HTTP (`http_request/http_get`), and `unembed_desc`.
- Tag args: 0 args => `""`; 1 arg => stringified; many => space-joined; objects are passed through unchanged on return.
- JIT is enabled by default; you can disable it by passing `{ enableJit: false }` to `evaluate` or `runLeveretLisp`.

## Development
- Install deps: `npm install`
- Run tests: `npm test`
- Benchmark (adjust `BENCH_ITERS`): `npm run bench`
- Spec: see [the spec](./SPEC.md) for full language details.
