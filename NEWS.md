# rblimp 1.2.0

* New `install_blimp()` and `uninstall_blimp()` functions let rblimp
  download and manage the Blimp engine directly, without going through
  the system installer. Installs to a user-writable directory
  (`~/.blimp/` on macOS and Linux, `%LOCALAPPDATA%/Blimp/` on Windows),
  overridable via the `R_BLIMP_HOME` environment variable.
* When `rblimp()` / `rblimp_source()` is run in an interactive session
  and Blimp is not installed, the user is now offered the option to
  install it via `install_blimp()` on the spot.
* `detect_blimp()` gained a `prompt =` argument; defaults to `TRUE` for
  the new interactive install offer.
* `update_blimp()` now routes based on install type: for managed installs
  it checks the manifest version first and re-downloads only when a newer
  version is available; system installs still launch the Blimp Updater
  application.
* Update version checks now use a JSON manifest endpoint
  (`https://updates.blimpstats.com/v1/blimp_base.json`) instead of
  parsing the plain-text changelog. Manifest URL is overridable via
  the `R_BLIMP_MANIFEST_URL` environment variable.
* New `R_BLIMP_NO_UPDATE_CHECK` environment variable disables automatic
  update checks (alternative to `options(check_blimp_update = FALSE)`).
* New `R_BLIMP_PREVENT_INSTALL` environment variable blocks
  `install_blimp()` for locked-down environments.
* `standardized()` output now prints rounded to 3 decimals in an
  aligned table (matching the other estimate tables) via a new
  `print()` method. The returned object is still a numeric matrix and
  retains full precision for computation; pass `digits =` to `print()`
  to change the displayed precision.

# rblimp 1.1.1

* Fixed a bug in `trace_plot()` when called with specific parameters.

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
