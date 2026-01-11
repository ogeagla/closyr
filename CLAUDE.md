# CLAUDE.md - Notes for Claude Code

## Project Overview
Closyr is a symbolic regression tool that uses genetic algorithms to find mathematical formulas that fit data points.

## Key Architecture

### Symja Math Library
- Uses `org.matheclipse.core` (Symja) for symbolic math
- **Critical**: Parser creates different symbol objects than `F/x` or `F/Dummy "x"`
- The system uses `ops-common/sym-x` which is `(F/Dummy "x")` as the variable
- When parsing formula strings, the parser creates its own `x` symbol objects

### Phenotype Structure
A phenotype is a map with:
- `:sym` - The ISymbol used in the expression (should be `ops-common/sym-x`)
- `:expr` - The IExpr mathematical expression
- `:util` - ExprEvaluator instance
- `:id` - UUID
- `:score` - Double score from evaluation

### Evaluation Flow
1. `expr->fn` wraps expression: `Function({sym}, expr)`
2. `eval-phenotype-on-expr-args` evaluates the function at input values
3. **Critical**: The `:sym` in the phenotype MUST match the symbol used inside `:expr`

### Thread Safety
- **Critical**: Symja's `ExprEvaluator` is NOT thread-safe
- Has internal mutable state (ArrayDeque stacks) that corrupts under concurrent access
- When using `pmap` for parallel scoring, ALWAYS create a fresh `ExprEvaluator` per evaluation
- Never share `ExprEvaluator` instances across threads

## Keep Going Feature (Implemented)

### Overview
Allows continuing evolution from a completed/stopped job's results by seeding the new population with previous formulas.

### Implementation
1. **Formula Parsing** (`src/closyr/ops/initialize.clj`):
   - `parse-formula->phenotype` - Parses formula string to phenotype
   - `seeded-phenotypes` - Creates population seeded from formulas (default 80% seeded, 20% fresh)
   - Uses recursive tree-walk to replace parser's `x` symbol with `ops-common/sym-x`

2. **API Endpoint** (`src/closyr/web/handlers/api.clj`):
   - `POST /api/jobs/:id/continue` - Continue from a completed/stopped job
   - Accepts `xs`, `ys`, and `config` (including `freshPercent`)

3. **Frontend** (`resources/public/js/solver-history.js`):
   - "Keep Going" button in history items
   - `keepGoingFromHistory(index)` function

### Key Technical Solution
The Symja parser creates its own Symbol instances for "x" that are NOT equal to `F/x` or `(F/Dummy "x")`.
Solution: Recursive tree-walk that replaces any symbol named "x" with `ops-common/sym-x`:

```clojure
(defn- make-tree-replacer [^ISymbol target-sym]
  (ops-common/as-function
    (fn tree-replace [^IExpr ie]
      (cond
        (and (.isSymbol ie) (= "x" (str ie))) target-sym
        (instance? IAST ie) (.map ^IAST ie (make-tree-replacer target-sym))
        :else ie))))
```

### Tests
- `test/closyr/seeded_evolution_test.clj` - Comprehensive tests for formula parsing and seeded evolution

### Error Handling
Robust error handling at multiple levels prevents crashes during evolution:
1. **Formula validation** (`api.clj`): `valid-seed-formula?` filters corrupted formulas before seeding
2. **Scoring** (`ga.clj`): `with-score` catches exceptions, returns min-score
3. **Competition** (`ga.clj`): `compete` catches exceptions, keeps parents unchanged
4. **Evaluation** (`eval.clj`): Returns infinity vectors on failure instead of throwing

## Build/Test Commands
- `lein test` - Run all tests
- `lein test :only ns/test-name` - Run specific test
- `lein run --web 3000` - Start the web server
- `lein run` - Start the java GUI
