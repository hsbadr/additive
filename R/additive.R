#' General Interface for Generalized Additive Models (GAM)
#'
#' `additive()` is a way to generate a _specification_ of a model
#'  before fitting and allows the model to be created using
#'  \pkg{mgcv} package in \pkg{R}.
#'
#'  The arguments are converted to their specific names at the
#'  time that the model is fit. Other options and argument can be
#'  set using `set_engine()`. If left to their defaults
#'  here (`NULL`), the values are taken from the underlying model
#'  functions. If parameters need to be modified, `update()` can be
#'  used in lieu of recreating the object from scratch.
#'
#' @param mode A single character string for the type of model.
#'  Possible values for this model are "unknown", "regression", or
#'  "classification".
#'
#' @param fitfunc A named character vector that describes how to call
#'  a function for fitting a generalized additive model. This defaults
#'  to \code{c(pkg = "mgcv", fun = "gam")} (\code{\link[mgcv]{gam}}).
#'  \code{fitfunc} should have elements \code{pkg} and \code{fun}.
#'  The former is optional but is recommended and the latter is
#'  required. For example, \code{c(pkg = "mgcv", fun = "bam")} would
#'  be used to invoke \code{\link[mgcv]{bam}} for big data.
#'  A user-specified function is also accepted provided that it is
#'  fully compatible with \code{\link[mgcv]{gam}}.
#'
#' @param formula.override Overrides the formula; for details see
#'  \code{\link[mgcv]{formula.gam}}.
#'
#' @inheritParams mgcv::gam
#' @inheritParams mgcv::bam
#'
#' @inheritParams mgcv::gam.fit
#'
#' @details
#' The data given to the function are not saved and are only used
#'  to determine the _mode_ of the model. For `additive()`, the
#'  possible modes are "regression" and "classification".
#'
#' The model can be created by the `fit()` function using the
#'  following _engines_:
#' \itemize{
#' \item \pkg{mgcv}:  `"mgcv"`
#' }
#'
#' @includeRmd man/rmd/additive-engine.Rmd details
#'
#' @examples
#'
#' additive()
#'
#' show_model_info("additive")
#'
#' additive(mode = "classification")
#' additive(mode = "regression")
#' \dontrun{
#' set.seed(2020)
#' dat <- gamSim(1, n = 400, dist = "normal", scale = 2)
#' additive_mod <-
#'   additive() %>%
#'   set_engine("mgcv") %>%
#'   fit(
#'     y ~ s(x0) + s(x1) + s(x2) + s(x3),
#'     data = dat
#'   )
#'
#' summary(additive_mod$fit)
#' }
#'
#' # -------------------------------------------------------------------------
#' @export
additive <-
  function(mode = "regression",
           fitfunc = NULL,
           formula.override = NULL,
           family = NULL,
           method = NULL,
           optimizer = NULL,
           control = NULL,
           scale = NULL,
           gamma = NULL,
           knots = NULL,
           sp = NULL,
           min.sp = NULL,
           paraPen = NULL,
           chunk.size = NULL,
           rho = NULL,
           AR.start = NULL,
           H = NULL,
           G = NULL,
           offset = NULL,
           weights = NULL,
           subset = NULL,
           start = NULL,
           etastart = NULL,
           mustart = NULL,
           drop.intercept = NULL,
           drop.unused.levels = NULL,
           cluster = NULL,
           nthreads = NULL,
           gc.level = NULL,
           use.chol = NULL,
           samfrac = NULL,
           coef = NULL,
           discrete = NULL,
           select = NULL,
           fit = NULL) {
    args <- list(
      fitfunc = rlang::enquo(fitfunc),
      formula.override = rlang::enquo(formula.override),
      family = rlang::enquo(family),
      method = rlang::enquo(method),
      optimizer = rlang::enquo(optimizer),
      control = rlang::enquo(control),
      scale = rlang::enquo(scale),
      gamma = rlang::enquo(gamma),
      knots = rlang::enquo(knots),
      sp = rlang::enquo(sp),
      min.sp = rlang::enquo(min.sp),
      paraPen = rlang::enquo(paraPen),
      chunk.size = rlang::enquo(chunk.size),
      rho = rlang::enquo(rho),
      AR.start = rlang::enquo(AR.start),
      H = rlang::enquo(H),
      G = rlang::enquo(G),
      offset = rlang::enquo(offset),
      weights = rlang::enquo(weights),
      subset = rlang::enquo(subset),
      start = rlang::enquo(start),
      etastart = rlang::enquo(etastart),
      mustart = rlang::enquo(mustart),
      drop.intercept = rlang::enquo(drop.intercept),
      drop.unused.levels = rlang::enquo(drop.unused.levels),
      cluster = rlang::enquo(cluster),
      nthreads = rlang::enquo(nthreads),
      gc.level = rlang::enquo(gc.level),
      use.chol = rlang::enquo(use.chol),
      samfrac = rlang::enquo(samfrac),
      coef = rlang::enquo(coef),
      discrete = rlang::enquo(discrete),
      select = rlang::enquo(select),
      fit = rlang::enquo(fit)
    )

    parsnip::new_model_spec(
      "additive",
      args = args,
      eng_args = NULL,
      mode = mode,
      method = NULL,
      engine = NULL
    )
  }

#' @export
print.additive <- function(x, ...) {
  cat("Generalized Additive Model (GAM) Specification (", x$mode, ")\n\n", sep = "")
  parsnip::model_printer(x, ...)

  if (!is.null(x$method$fit$args)) {
    cat("Model fit template:\n")
    print(parsnip::show_call(x))
  }

  invisible(x)
}


#' @export
translate.additive <- function(x, engine = x$engine, ...) {
  if (is.null(engine)) {
    message("Used `engine = 'gam'` for translation.")
    engine <- "mgcv"
  }

  x <- parsnip::translate.default(x, engine, ...)

  x
}

# -------------------------------------------------------------------------

#' @param object A Generalized Additive Model (GAM) specification.
#' @param parameters A 1-row tibble or named list with _main_
#'  parameters to update. If the individual arguments are used,
#'  these will supersede the values in `parameters`. Also, using
#'  engine arguments in this object will result in an error.
#' @param ... Other arguments passed to internal functions.
#' @param fresh A logical for whether the arguments should be
#'  modified in-place of or replaced wholesale.
#'
#' @return An updated model specification.
#'
#' @examples
#' model <- additive(select = FALSE)
#' model
#' update(model, select = TRUE)
#' update(model, select = TRUE, fresh = TRUE)
#' @method update additive
#' @rdname additive
#' @export
update.additive <-
  function(object,
           parameters = NULL,
           fitfunc = NULL,
           formula.override = NULL,
           family = NULL,
           method = NULL,
           optimizer = NULL,
           control = NULL,
           scale = NULL,
           gamma = NULL,
           knots = NULL,
           sp = NULL,
           min.sp = NULL,
           paraPen = NULL,
           chunk.size = NULL,
           rho = NULL,
           AR.start = NULL,
           H = NULL,
           G = NULL,
           offset = NULL,
           weights = NULL,
           subset = NULL,
           start = NULL,
           etastart = NULL,
           mustart = NULL,
           drop.intercept = NULL,
           drop.unused.levels = NULL,
           cluster = NULL,
           nthreads = NULL,
           gc.level = NULL,
           use.chol = NULL,
           samfrac = NULL,
           coef = NULL,
           discrete = NULL,
           select = NULL,
           fit = NULL,
           fresh = FALSE,
           ...) {
    parsnip::update_dot_check(...)

    if (!is.null(parameters)) {
      parameters <- parsnip::check_final_param(parameters)
    }

    args <- list(
      fitfunc = rlang::enquo(fitfunc),
      formula.override = rlang::enquo(formula.override),
      family = rlang::enquo(family),
      method = rlang::enquo(method),
      optimizer = rlang::enquo(optimizer),
      control = rlang::enquo(control),
      scale = rlang::enquo(scale),
      gamma = rlang::enquo(gamma),
      knots = rlang::enquo(knots),
      sp = rlang::enquo(sp),
      min.sp = rlang::enquo(min.sp),
      paraPen = rlang::enquo(paraPen),
      chunk.size = rlang::enquo(chunk.size),
      rho = rlang::enquo(rho),
      AR.start = rlang::enquo(AR.start),
      H = rlang::enquo(H),
      G = rlang::enquo(G),
      offset = rlang::enquo(offset),
      weights = rlang::enquo(weights),
      subset = rlang::enquo(subset),
      start = rlang::enquo(start),
      etastart = rlang::enquo(etastart),
      mustart = rlang::enquo(mustart),
      drop.intercept = rlang::enquo(drop.intercept),
      drop.unused.levels = rlang::enquo(drop.unused.levels),
      cluster = rlang::enquo(cluster),
      nthreads = rlang::enquo(nthreads),
      gc.level = rlang::enquo(gc.level),
      use.chol = rlang::enquo(use.chol),
      samfrac = rlang::enquo(samfrac),
      coef = rlang::enquo(coef),
      discrete = rlang::enquo(discrete),
      select = rlang::enquo(select),
      fit = rlang::enquo(fit)
    )

    args <- parsnip::update_main_parameters(args, parameters)

    eng_args <- parsnip::update_engine_parameters(object$eng_args, ...)

    if (fresh) {
      object$args <- args
      object$eng_args <- eng_args
    } else {
      null_args <- purrr::map_lgl(args, parsnip::null_value)
      if (any(null_args)) {
        args <- args[!null_args]
      }
      if (length(args) > 0) {
        object$args[names(args)] <- args
      }
      if (length(eng_args) > 0) {
        object$eng_args[names(eng_args)] <- eng_args
      }
    }

    parsnip::new_model_spec(
      "additive",
      args = object$args,
      eng_args = object$eng_args,
      mode = object$mode,
      method = NULL,
      engine = object$engine
    )
  }

# -------------------------------------------------------------------------

check_args.additive <- function(object) {
  args <- lapply(object$args, rlang::eval_tidy)

  if (!is.null(args$fitfunc)) {
    check_func_val <- utils::getFromNamespace("check_func_val", "parsnip")
    check_func_val(args$fitfunc)
  }

  if (!is.null(args$select) & !is.logical(args$select)) {
    rlang::abort("`select` should be logical.")
  }

  invisible(object)
}

# -------------------------------------------------------------------------

#' Fit Generalized Additive Models (GAM)
#'
#' @inheritParams mgcv::gam
#' @inheritParams mgcv::bam
#'
#' @inheritParams mgcv::gam.fit
#'
#  @param ... Other arguments passed to \code{\link[mgcv]{gam.fit}}.
#'
#  @return An fitted GAM object of class \code{gamObject}.
#'
#' @seealso \code{\link[mgcv]{mgcv-package}},
#'   \code{\link[mgcv]{gam}},
#'   \code{\link[mgcv]{bam}},
#'   \code{\link[mgcv]{gamObject}},
#'   \code{\link[mgcv]{gam.models}},
#'   \code{\link[mgcv]{smooth.terms}},
#'   \code{\link[mgcv]{predict.gam}},
#'   \code{\link[mgcv]{plot.gam}},
#'   \code{\link[mgcv]{summary.gam}},
#'   \code{\link[mgcv]{gam.side}},
#'   \code{\link[mgcv]{gam.selection}},
#'   \code{\link[mgcv]{gam.control}},
#'   \code{\link[mgcv]{gam.check}},
#'   \code{\link[mgcv]{vis.gam}},
#'   \code{\link[mgcv]{family.mgcv}},
#'   \code{\link[mgcv]{formula.gam}},
#'   \code{\link[stats]{family}},
#'   \code{\link[stats]{formula}},
#'   \code{\link[stats]{update.formula}}.
#'
#' @rdname additive
#' @export
additive_fit <- function(formula, data, ...) {
  dots <- list(formula = formula, data = rlang::enquo(data), ...)

  # Override the formula, if needed
  if (!is.null(dots$formula.override)) {
    if (inherits(
      dots$formula.override,
      c("formula", "list")
    )) {
      dots$formula <- dots$formula.override
    } else {
      rlang::abort("Unsupported or invalid formula.override!")
    }
  }
  dots$formula.override <- NULL

  # Simplify and expose the family call
  if (inherits(dots$family, c("family", "brmsfamily"))) {
    family_chr <- purrr::map_lgl(dots$family, is.character)
    family_func <- ifelse(
      inherits(dots$family, "brmsfamily"),
      "brmsfamily",
      dots$family$family
    )
    family_fmls <- rlang::fn_fmls_names(
      get(
        family_func,
        mode = "function"
      )
    )
    family_chrs <- intersect(names(family_chr[family_chr]), family_fmls)
    family_args <- dots$family[family_chrs]
    dots$family <- rlang::call2(family_func, !!!family_args)
  }

  # Check the fit function
  fitfunc <- as.list(dots$fitfunc)
  dots$fitfunc <- NULL

  # Create the fit call
  if (!is.null(fitfunc$fun)) {
    fitcall <- rlang::call2(fitfunc$fun, !!!dots, .ns = fitfunc$pkg)
  } else {
    fitcall <- rlang::call2("gam", !!!dots, .ns = "mgcv")
  }

  # Evaluate the fit call
  rlang::eval_tidy(fitcall)
}
