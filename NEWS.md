# EPLSIM 1.0.1

## Bug fixes
* Corrected inaccurate descriptions in the DESCRIPTION and vignette files from the previous version.


# EPLSIM 1.0.0

This is a major release: the partial linear single index model family gains
three new fitting functions with automatic smoothness selection, and several
correctness bugs in the original fitting/plotting functions are fixed.
`plsi.lr.auto()` is now the recommended starting point for continuous
outcomes; `plsi.lr.v2()` remains available for manual control over the link
function's degrees of freedom (e.g. for simulation studies or when the
right basis complexity is already known).

## New features

* New `plsi.lr.auto()`: fits the partial linear single index model for a
  continuous outcome with automatic smoothness selection via `mgcv::gam()`
  (REML), removing the need to hand-tune `spline.num`/degrees of freedom.
  Only an upper-bound basis dimension `k` is required.
* New `plsi.logistic.auto()`: binary-outcome (logistic) analog of
  `plsi.lr.auto()`, using a `binomial()` family GAM with the same automatic
  smoothness selection.
* New `plsi.log.auto()`: count-outcome analog of `plsi.lr.auto()`, supporting
  Poisson and negative binomial (`family = "nb"`, the default, recommended
  for real overdispersed count data) families with a log link.
* All three `*.auto()` fitting functions report Wald-based standard errors,
  confidence intervals, and effect-size summaries (odds ratios for
  `plsi.logistic.auto()`, rate ratios for `plsi.log.auto()`) for both the
  single-index coefficients and the confounder coefficients.
* New/updated plotting functions, all supporting a `type` argument
  (`"linear"`, `"logistic"`, `"log"`) matching the fitting function used:
  `si.fun.plot()`, `si.coef.plot()`, `e.main.plot()`, `e.interaction.plot()`,
  `interquartile.quartile.plot()`, `mixture.overall.plot()`.

## Deprecated / removed

* `plsi.lr.v1()` is superseded by `plsi.lr.v2()`, which fixes all bugs listed
  below and adds a `seed` argument for reproducibility. Existing code using
  `plsi.lr.v1()` should switch to `plsi.lr.v2()` or, preferably,
  `plsi.lr.auto()`.

## Bug fixes

* Fixed a transposition bug (`t(beta_est)` inside `cbind()`) that produced
  incorrect matrix dimensions and crashed the function whenever more than
  one exposure variable was used. This was present in `plsi.lr.v1()` and is
  fixed in its replacement, `plsi.lr.v2()`.
* Fixed `initial.random.num = 0` producing an invalid index sequence in
  `plsi.lr.v2()` (the `*.auto()` functions instead validate this input
  directly and raise an informative error).
* Fixed missing `as.numeric()` coercion on `logLik()` output in
  `plsi.lr.v2()`.
* Fixed two typos in `plsi.lr.v2()`'s returned list element names
  (`intial.table` -> `initial.table`, `all.intial.results` ->
  `all.initial.results`).
* Fixed `e.interaction.plot()` conditioning on the wrong exposure's quantiles
  in its second panel (a copy-paste bug meant both panels were secretly
  conditioning on the same exposure's quantiles instead of each on the
  other's).
* Fixed an undefined-variable bug in `e.main.plot()` (`out_value` was
  referenced but never computed).
* Replaced all `ciTools::add_ci()` calls across the plotting functions with
  direct `predict.gam(se.fit = TRUE)` calls, since `ciTools` does not support
  `mgcv::gam` model objects (this previously caused errors such as
  `object 'AGE.c' not found` when predicting from a single-index value alone).
* Fixed `plsi.lr.v2()`'s `si.fun` output using `ciTools`'s default `pred`
  column name instead of `fit`, which caused `si.fun.plot(type = "linear")`
  to fail on `plsi.lr.v2()` output despite being documented to support it.
  `si.fun` now consistently uses `fit`/`lwr`/`upr` across `plsi.lr.v2()` and
  all three `*.auto()` functions.
* Fixed standard errors for the single-index coefficients in
  `plsi.logistic.auto()`/`plsi.log.auto()` collapsing to near-zero
  (~1e-16) due to residualizing against the full model's own fitted values
  rather than the actual observed outcome; the confounder-free prediction
  model is now fit against real observed data via a fixed offset, preserving
  genuine sampling variability.
* Standard errors for the single-index coefficients no longer silently
  return `NaN` when the numerically-differentiated information matrix is not
  positive definite at the selected optimum; negative/near-zero eigenvalues
  are now floored before inversion, yielding conservative (rather than
  missing) standard errors, with a warning noting when this occurs.

## Documentation

* Corrected `@importFrom` declarations (`mgcv` functions were incorrectly
  listed under `@importFrom stats`).
* Fixed Rd generation corruption caused by mixing raw Rd markup
  (`\describe{}`/`\item{}{}`) with underscore-containing identifiers inside
  roxygen2's markdown-mode processing; `@return` documentation now uses
  markdown-native syntax.

## Internal

* Added `stats`, `graphics`, and `utils` to `Imports` in `DESCRIPTION`.
  These base packages are used extensively via `::` throughout the fitting
  and plotting functions but were previously undeclared, which would have
  surfaced as an `R CMD check` NOTE/WARNING on the next CRAN submission.
* Expanded the test suite with dedicated test files for `plsi.lr.v2()`,
  `plsi.lr.auto()`, `plsi.logistic.auto()`, `plsi.log.auto()`, and all six
  plotting functions.
  
