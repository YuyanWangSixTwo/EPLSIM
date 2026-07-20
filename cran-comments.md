# EPLSIM 1.0.1

## Bug fixes
* Corrected inaccurate descriptions in the DESCRIPTION and vignette files from the previous version.


# EPLSIM 1.0.0

## New features

* New `plsi.lr.auto()`: fits the partial linear single index model for a
  continuous outcome with automatic smoothness selection via `mgcv::gam()`
  (REML), removing the need to hand-tune `spline.num`/degrees of freedom.
  Only an upper-bound basis dimension `k` is required.
* New `plsi.logistic.auto()`: binary-outcome (logistic) analog of
  `plsi.lr.auto()`, using a `binomial()` family GAM with the same automatic
  smoothness selection.
* New `plsi.log.auto()`: non-negative count-outcome analog, supporting both
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

## Bug fixes
* Fixed a transposition bug in `plsi.lr.v1()`/`plsi.lr.v2()` (`t(beta_est)`
  inside `cbind()`) that produced incorrect matrix dimensions and crashed the
  function whenever more than one exposure variable was used.
* Fixed `initial.random.num = 0` producing an invalid index sequence.
* Fixed missing `as.numeric()` coercion on `logLik()` output.
* Fixed two typos in returned list element names (`intial.table` ->
  `initial.table`, `all.intial.results` -> `all.initial.results`).
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

## R CMD check results
0 errors | 0 warnings | 1 note
