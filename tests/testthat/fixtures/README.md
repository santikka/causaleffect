`causal-effect-baseline.rds` contains results from causaleffect commit
`ff6059f`, using R 4.6.1 and igraph 2.3.3. It covers every combination of
`expr`, `simp`, `prune`, and `primes` for the ten graphs in
`helper-caugi.R`, with `steps = TRUE` and `stop_on_nonid = FALSE`.

Step graphs are stored as vertex names and sorted, unique typed edges to avoid
igraph object identities and irrelevant edge ordering in comparisons. The
remaining step data and probability expressions are stored without normalization.

To regenerate, install that baseline commit into a separate library and run
from the repository root:

```r
library(causaleffect, lib.loc = "/path/to/baseline/library")
source("tests/testthat/helper-caugi.R")
saveRDS(
  causal.fixture.results(causal.effect),
  "tests/testthat/fixtures/causal-effect-baseline.rds",
  version = 2,
  compress = "xz"
)
```

`legacy-results.rds` records the transport and recovery examples with selection
variables from the same baseline. Regenerate it in that R session with:

```r
cases <- legacy.fixtures()
results <- lapply(names(cases), function(name) {
  do.call(getExportedValue("causaleffect", name), cases[[name]])
})
saveRDS(setNames(results, names(cases)), "tests/testthat/fixtures/legacy-results.rds")
```
