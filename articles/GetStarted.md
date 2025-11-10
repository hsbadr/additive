# Get started with \`additive\`

``` r
library(additive)
```

``` r
library(recipes)
library(workflows)
```

Let’s simulate a data using `mgcv` package, which is automatically
loaded by `additive`.

``` r
set.seed(2020)
dat <- gamSim(1, n = 400, dist = "normal", scale = 2)
```

    ## Gu & Wahba 4 term additive model

In a first step, we use the `recipes` package to prepare (a recipe for)
the data.

``` r
test_recipe <- dat |>
  recipe() |>
  update_role(y, new_role = "outcome") |>
  update_role(x0, x1, x2, x3, new_role = "predictor") |>
  step_normalize(all_numeric_predictors())
```

``` r
print(test_recipe)
```

    ## 

    ## ── Recipe ──────────────────────────────────────────────────────────────────────

    ## 

    ## ── Inputs

    ## Number of variables by role

    ## outcome:         1
    ## predictor:       4
    ## undeclared role: 5

    ## 

    ## ── Operations

    ## • Centering and scaling for: all_numeric_predictors()

Above, we not only define the roles of the relevant variables but also
normalized all numeric predictors to facilitate model fitting later on.
In the next step, we use `additive` to set up a basic model structure.

``` r
test_model <- additive(
    family = gaussian(),
    method = "REML"
  ) |>
  set_engine("mgcv") |>
  set_mode("regression")
```

``` r
print(test_model)
```

    ## Generalized Additive Model (GAM) Specification (regression)
    ## 
    ## Main Arguments:
    ##   family = gaussian()
    ##   method = REML
    ## 
    ## Computational engine: mgcv

The `additive` function is the main function of the package to
initialize a Generalized Additive Model (GAM). We can set up a lot of
the information directly within the function or update the information
later on, via the `update` method. For example, if we didn’t specify the
family initially or set it to something else that we now wanted to
change, we could use the `update` method as follows

``` r
test_model <- test_model |>
  update(family = gaussian())
```

Next, we define a workflow via the `workflows` package, by combining the
above defined data processing recipe and the model plus the actual model
formula to be passed to the `mgcv` engine.

``` r
test_workflow <- workflow() |>
  add_recipe(test_recipe) |>
  add_model(
    spec = test_model,
    formula = y ~ s(x0) + s(x1) + s(x2) + s(x3)
  )
```

``` r
print(test_workflow)
```

    ## ══ Workflow ════════════════════════════════════════════════════════════════════
    ## Preprocessor: Recipe
    ## Model: additive()
    ## 
    ## ── Preprocessor ────────────────────────────────────────────────────────────────
    ## 1 Recipe Step
    ## 
    ## • step_normalize()
    ## 
    ## ── Model ───────────────────────────────────────────────────────────────────────
    ## Generalized Additive Model (GAM) Specification (regression)
    ## 
    ## Main Arguments:
    ##   family = gaussian()
    ##   method = REML
    ## 
    ## Computational engine: mgcv

We are now ready to fit the model by calling the `fit` method with the
data set we want to train the model on.

``` r
test_workflow_fit <- test_workflow |>
  fit(data = dat)
```

``` r
print(test_workflow_fit)
```

    ## ══ Workflow [trained] ══════════════════════════════════════════════════════════
    ## Preprocessor: Recipe
    ## Model: additive()
    ## 
    ## ── Preprocessor ────────────────────────────────────────────────────────────────
    ## 1 Recipe Step
    ## 
    ## • step_normalize()
    ## 
    ## ── Model ───────────────────────────────────────────────────────────────────────
    ## 
    ## Family: gaussian 
    ## Link function: identity 
    ## 
    ## Formula:
    ## y ~ s(x0) + s(x1) + s(x2) + s(x3)
    ## 
    ## Estimated degrees of freedom:
    ## 4.24 3.25 8.26 2.22  total = 18.98 
    ## 
    ## REML score: 859.5808

To extract the parsnip model fit from the workflow

``` r
test_fit <- test_workflow_fit |>
  extract_fit_parsnip()
```

The `gamObject` object can be extracted as follows

``` r
gam_fit <- test_workflow_fit |>
  extract_fit_engine()
```

``` r
class(gam_fit)
```

    ## [1] "gam" "glm" "lm"

We can use the trained workflow, which includes the fitted model, to
conveniently `predict` using new data without having to worry about all
the data reprocessing, which is automatically applied using the workflow
preprocessor (recipe).

``` r
newdata <- dat[1:5, ]
```

``` r
test_workflow_fit |>
  predict(
    new_data = newdata,
    type = "conf_int",
    level = 0.95
  )
```

    ## # A tibble: 5 × 2
    ##   .pred_lower .pred_upper
    ##     <dbl[1d]>   <dbl[1d]>
    ## 1        2.60        4.45
    ## 2        4.90        6.48
    ## 3        8.74       10.5 
    ## 4        4.89        6.40
    ## 5        2.97        4.57

To add the standard errors on the scale of the linear predictors

``` r
test_workflow_fit |>
  predict(
    new_data = newdata,
    type = "conf_int",
    level = 0.95,
    std_error = TRUE
  )
```

    ## # A tibble: 5 × 3
    ##   .pred_lower .pred_upper .std_error
    ##     <dbl[1d]>   <dbl[1d]>  <dbl[1d]>
    ## 1        2.60        4.45      0.470
    ## 2        4.90        6.48      0.401
    ## 3        8.74       10.5       0.457
    ## 4        4.89        6.40      0.383
    ## 5        2.97        4.57      0.408
