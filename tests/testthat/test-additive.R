test_that("additive execution", {
  skip_on_cran()

  # Simulate example data for GAM
  set.seed(2020)
  dat <- gamSim(1, n = 400, dist = "normal", scale = 2)

  # Fit GAM directly
  gam_fit <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3),
    family = gaussian(), method = "REML", data = dat
  )

  # Generate predictions for the data head from the GAM fit
  gam_pred <- predict(gam_fit, head(dat))

  # Create a recipe for data processing
  test_recipe <- dat |>
    recipe() |>
    update_role(y, new_role = "outcome") |>
    update_role(x0, x1, x2, x3, new_role = "predictor") |>
    step_normalize(all_predictors())

  # Create an additive model specification
  test_model <- additive(
    family = gaussian(),
    method = "REML"
  ) |>
    set_engine("mgcv") |>
    set_mode("regression")

  # Create a workflow
  test_workflow <- workflow() |>
    add_recipe(test_recipe) |>
    add_model(
      spec = test_model,
      formula = y ~ s(x0) + s(x1) + s(x2) + s(x3)
    )

  # Train the workflow
  test_workflow_fit <- test_workflow |>
    fit(data = dat)

  # Extract the fit object from the trained workflow
  test_fit <- test_workflow_fit |>
    extract_fit_parsnip()

  # Generate predictions for the data head from the trained workflow
  test_pred <- predict(test_workflow_fit, head(dat))

  # Check the classes
  expect_s3_class(gam_fit, "gam")
  expect_s3_class(test_fit$fit, "gam")
  expect_s3_class(test_fit, "model_fit")
  expect_s3_class(test_recipe, "recipe")
  expect_s3_class(test_model, "model_spec")
  expect_s3_class(test_workflow, "workflow")
  expect_s3_class(test_workflow_fit, "workflow")

  # Check the structure of the fit object
  expect_equal(test_fit$fit$family$family, "gaussian")
  expect_equal(test_fit$fit$method, "REML")
  expect_equal(test_fit$preproc$y_var, "y")

  # Check the model coefficients
  expect_equal(coef(test_fit$fit), coef(gam_fit))

  # Check the predictions
  expect_equal(test_pred$.pred, gam_pred, ignore_attr = TRUE)

  # Check the default engine
  expect_equal(additive()$engine, "mgcv")
})
