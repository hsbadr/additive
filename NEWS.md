# additive 1.0.1

- Built vignettes to fix CRAN warnings
- Replaced `citEntry()` with `bibentry()`
- Updated citation and invalid URLs

# additive 1.0.0

- Fixed handling of `extended.family` class
- Fixed multi-class probability (`type = "prob"`) predictions
- Fixed multi-class confidence-intervals (`type = "conf_int"`) predictions
- Fixed multi-class prediction-intervals (`type = "pred_int"`) predictions

# additive 0.0.5

- Depends on `parsnip >= 1.0.0`
- Using base R pipe and depends on R >= 4.1.0
- Supported the use of case weights
- Supported threshold (inverse probability weights) for multi-class predictions
- Fixed inconsistent probability (`type = "prob"`) predictions for two-class models
- Updated package dependencies

# additive 0.0.4

- Updated package website
- Updated package `CITATION`
- Updated package `DESCRIPTION` and `README`
- Updated package dependencies and `WORDLIST`
- Fixed threshold probability check for class predictions
- Fixed possibly invalid URLs
- Using `extract_fit_engine()` in `GetStarted` vignette
- Set dependency by model's mode with `parsnip > 0.1.7`

# additive 0.0.3

- Fixed `GetStarted.Rmd` vignette build
- Fixed class predictions for binary classification
- Fixed confidence intervals and standard errors
- Added threshold probability option for class predictions
- Replaced deprecated `pull_workflow_fit()`

# additive 0.0.2

- Added functionality to change to declare an engine in the model specification function.
- Updated `conf_int` predictions to respect the confidence level
- Updated package dependencies and `NAMESPACE`
- Updated `README`, citation, and vignettes

# additive 0.0.1

- Initial release
