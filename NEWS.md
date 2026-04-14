# rblimp 1.1.0

* Added an `as.array()` method for `blimp_obj` that returns an
  `[iterations, chains, parameters]` 3-D array compatible with **bayesplot**.
  Supports a `warmup = c("exclude", "include", "only")` argument for
  including burn-in draws; when `warmup = "include"` the returned array
  carries an `"n_warmup"` attribute so it pipes directly into
  `bayesplot::mcmc_trace(arr, n_warmup = attr(arr, "n_warmup"))`.
* Added S3 methods registering `blimp_obj` with the **posterior** package's
  `as_draws()`, `as_draws_array()`, and `as_draws_df()` generics, unlocking
  the posterior / bayesplot summary and plotting ecosystems. **posterior**
  is a soft dependency (in `Suggests`).
* Trace plots now display rank-normalized split-Rhat (Vehtari et al., 2021)
  instead of the basic PSRF, and fixed facet labels when plotting a subset
  of parameters.

# rblimp 1.0.0

* Initial CRAN submission.
