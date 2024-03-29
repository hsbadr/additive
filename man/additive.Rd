% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/additive.R
\name{additive}
\alias{additive}
\alias{update.additive}
\alias{additive_fit}
\title{General Interface for Additive TidyModels}
\usage{
additive(
  mode = "regression",
  engine = "mgcv",
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
  fit = NULL
)

\method{update}{additive}(
  object,
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
  ...
)

additive_fit(formula, data, ...)
}
\arguments{
\item{mode}{A single character string for the prediction outcome mode.
Possible values for this model are "unknown", "regression", or
"classification".}

\item{engine}{A single character string specifying what computational
engine to use for fitting. Possible engines are listed below.
The default for this model is \code{"mgcv"}.}

\item{fitfunc}{A named character vector that describes how to call
a function for fitting a generalized additive model. This defaults
to \code{c(pkg = "mgcv", fun = "gam")} (\code{\link[mgcv]{gam}}).
\code{fitfunc} should have elements \code{pkg} and \code{fun}.
The former is optional but is recommended and the latter is
required. For example, \code{c(pkg = "mgcv", fun = "bam")} would
be used to invoke \code{\link[mgcv]{bam}} for big data.
A user-specified function is also accepted provided that it is
fully compatible with \code{\link[mgcv]{gam}}.}

\item{formula.override}{Overrides the formula; for details see
\code{\link[mgcv]{formula.gam}}.}

\item{family}{
This is a family object specifying the distribution and link to use in
fitting etc (see \code{\link{glm}} and \code{\link{family}}). See 
\code{\link[mgcv]{family.mgcv}} for a full list of what is available, which goes well beyond exponential family.
Note that \code{quasi} families actually result in the use of extended quasi-likelihood 
if \code{method} is set to a RE/ML method (McCullagh and Nelder, 1989, 9.6).
}

\item{method}{The smoothing parameter estimation method. \code{"GCV.Cp"} to use GCV for unknown scale parameter and
Mallows' Cp/UBRE/AIC for known scale. \code{"GACV.Cp"} is equivalent, but using GACV in place of GCV. \code{"NCV"}
for neighbourhood cross-validation using the neighbourhood structure speficied by \code{nei} (\code{"QNCV"} for numerically more ribust version).  \code{"REML"} 
for REML estimation, including of unknown scale, \code{"P-REML"} for REML estimation, but using a Pearson estimate 
of the scale. \code{"ML"} and \code{"P-ML"} are similar, but using maximum likelihood in place of REML. Beyond the 
exponential family \code{"REML"} is the default, and the only other options are \code{"ML"}, \code{"NCV"} or \code{"QNCV"}.}

\item{optimizer}{An array specifying the numerical optimization method to use to optimize the smoothing 
parameter estimation criterion (given by \code{method}). \code{"outer"} 
for the direct nested optimization approach. \code{"outer"} can use several alternative optimizers, specified in the 
second element of \code{optimizer}: \code{"newton"} (default), \code{"bfgs"}, \code{"optim"} or \code{"nlm"}. \code{"efs"}
for the extended Fellner Schall method of Wood and Fasiolo (2017).}

\item{control}{A list of fit control parameters to replace defaults returned by 
\code{\link[mgcv]{gam.control}}. Values not set assume default values. }

\item{scale}{ If this is positive then it is taken as the known scale parameter. Negative signals that the 
scale parameter is unknown. 0 signals that the scale parameter is 1  for Poisson and binomial and unknown otherwise. 
Note that (RE)ML methods can only work with scale parameter 1 for the Poisson and binomial cases.    
}

\item{gamma}{Increase this beyond 1 to produce smoother models. \code{gamma} multiplies the effective degrees of freedom in the GCV or UBRE/AIC. \code{n/gamma} can be viewed as an effective sample size in the GCV score, and this also enables it to be used with REML/ML. Ignored with P-RE/ML or the \code{efs} optimizer. }

\item{knots}{this is an optional list containing user specified knot values to be used for basis construction. 
For most bases the user simply supplies the knots to be used, which must match up with the \code{k} value
supplied (note that the number of knots is not always just \code{k}). 
See \code{\link[mgcv]{tprs}} for what happens in the \code{"tp"/"ts"} case. 
Different terms can use different numbers of knots, unless they share a covariate.
}

\item{sp}{A vector of smoothing parameters can be provided here.
 Smoothing parameters must be supplied in the order that the smooth terms appear in the model 
formula. Negative elements indicate that the parameter should be estimated, and hence a mixture 
of fixed and estimated parameters is possible. If smooths share smoothing parameters then \code{length(sp)} 
must correspond to the number of underlying smoothing parameters.}

\item{min.sp}{Lower bounds can be supplied for the smoothing parameters. Note
that if this option is used then the smoothing parameters \code{full.sp}, in the 
returned object, will need to be added to what is supplied here to get the 
 smoothing parameters actually multiplying the penalties. \code{length(min.sp)} should 
always be the same as the total number of penalties (so it may be longer than \code{sp},
if smooths share smoothing parameters).}

\item{paraPen}{optional list specifying any penalties to be applied to parametric model terms. 
\code{\link[mgcv]{gam.models}} explains more.}

\item{chunk.size}{The model matrix is created in chunks of this size, rather than ever being formed whole. 
Reset to \code{4*p} if \code{chunk.size < 4*p} where \code{p} is the number of coefficients.}

\item{rho}{An AR1 error model can be used for the residuals (based on dataframe order), of Gaussian-identity 
           link models. This is the AR1 correlation parameter. Standardized residuals (approximately 
           uncorrelated under correct model) returned in 
           \code{std.rsd} if non zero. Also usable with other models when \code{discrete=TRUE}, in which case the AR model
           is applied to the working residuals and corresponds to a GEE approximation.}

\item{AR.start}{logical variable of same length as data, \code{TRUE} at first observation of an independent
section of AR1 correlation. Very first observation in data frame does not need this. If \code{NULL} then 
there are no breaks in AR1 correlaion.}

\item{H}{A user supplied fixed quadratic penalty on the parameters of the 
GAM can be supplied, with this as its coefficient matrix. A common use of this term is 
to add a ridge penalty to the parameters of the GAM in circumstances in which the model
is close to un-identifiable on the scale of the linear predictor, but perfectly well
defined on the response scale.}

\item{G}{Usually \code{NULL}, but may contain the object returned by a previous call to \code{gam} with 
\code{fit=FALSE}, in which case all other arguments are ignored except for
\code{sp}, \code{gamma}, \code{in.out}, \code{scale}, \code{control}, \code{method} \code{optimizer} and \code{fit}.}

\item{offset}{Can be used to supply a model offset for use in fitting. Note
that this offset will always be completely ignored when predicting, unlike an offset 
included in \code{formula} (this used to conform to the behaviour of
\code{lm} and \code{glm}).}

\item{subset}{ an optional vector specifying a subset of observations to be
          used in the fitting process.}

\item{start}{Initial values for the model coefficients.}

\item{etastart}{Initial values for the linear predictor.}

\item{mustart}{Initial values for the expected response.}

\item{drop.intercept}{Set to \code{TRUE} to force the model to really not have a constant in the parametric model part,
even with factor variables present. Can be vector when \code{formula} is a list.}

\item{drop.unused.levels}{by default unused levels are dropped from factors before fitting. For some smooths 
involving factor variables you might want to turn this off. Only do so if you know what you are doing.}

\item{cluster}{\code{bam} can compute the computationally dominant QR decomposition in parallel using \link[parallel:clusterApply]{parLapply}
from the \code{parallel} package, if it is supplied with a cluster on which to do this (a cluster here can be some cores of a 
single machine). See details and example code. 
}

\item{nthreads}{Number of threads to use for non-cluster computation (e.g. combining results from cluster nodes).
If \code{NA} set to \code{max(1,length(cluster))}. See details.}

\item{gc.level}{to keep the memory footprint down, it can help to call the garbage collector often, but this takes 
a substatial amount of time. Setting this to zero means that garbage collection only happens when R decides it should. Setting to 2 gives frequent garbage collection. 1 is in between. Not as much of a problem as it used to be, but can really matter for very large datasets.
}

\item{use.chol}{By default \code{bam} uses a very stable QR update approach to obtaining the QR decomposition
of the model matrix. For well conditioned models an alternative accumulates the crossproduct of the model matrix
and then finds its Choleski decomposition, at the end. This is somewhat more efficient, computationally.}

\item{samfrac}{For very large sample size Generalized additive models the number of iterations needed for the model fit can 
be reduced by first fitting a model to a random sample of the data, and using the results to supply starting values. This initial fit is run with sloppy convergence tolerances, so is typically very low cost. \code{samfrac} is the sampling fraction to use. 0.1 is often reasonable. }

\item{coef}{initial values for model coefficients}

\item{discrete}{experimental option for setting up models for use with discrete methods employed in \code{\link[mgcv]{bam}}. Do not modify.}

\item{select}{ If this is \code{TRUE} then \code{gam} can add an extra penalty to each term so 
that it can be penalized to zero.  This means that the smoothing parameter estimation that is 
part of fitting can completely remove terms from the model. If the corresponding 
smoothing parameter is estimated as zero then the extra penalty has no effect. Use \code{gamma} to increase level of penalization.
}

\item{fit}{If this argument is \code{TRUE} then \code{gam} sets up the model and fits it, but if it is
\code{FALSE} then the model is set up and an object \code{G} containing what
would be required to fit is returned is returned. See argument \code{G}.}

\item{object}{A Generalized Additive Model (GAM) specification.}

\item{parameters}{A 1-row tibble or named list with \emph{main}
parameters to update. If the individual arguments are used,
these will supersede the values in \code{parameters}. Also, using
engine arguments in this object will result in an error.}

\item{fresh}{A logical for whether the arguments should be
modified in-place of or replaced wholesale.}

\item{...}{Other arguments passed to internal functions.}

\item{formula}{ A GAM formula, or a list of formulae (see \code{\link[mgcv]{formula.gam}} and also \code{\link[mgcv]{gam.models}}). 
These are exactly like the formula for a GLM except that smooth terms, \code{\link[mgcv]{s}}, \code{\link[mgcv]{te}}, \code{\link[mgcv]{ti}} 
and \code{\link[mgcv]{t2}}, can be added to the right hand side to specify that the linear predictor depends on smooth functions of predictors (or linear functionals of these).
}

\item{data}{ A data frame or list containing the model response variable and 
covariates required by the formula. By default the variables are taken 
from \code{environment(formula)}: typically the environment from 
which \code{gam} is called.}
}
\value{
An updated model specification.
}
\description{
\code{additive()} is a way to generate a \emph{specification} of a model
before fitting and allows the model to be created using
\pkg{mgcv} package in \pkg{R}.
}
\details{
The arguments are converted to their specific names at the
time that the model is fit. Other options and argument can be
set using \code{set_engine()}. If left to their defaults
here (\code{NULL}), the values are taken from the underlying model
functions. If parameters need to be modified, \code{update()} can be
used in lieu of recreating the object from scratch.

The data given to the function are not saved and are only used
to determine the \emph{mode} of the model. For \code{additive()}, the
possible modes are "regression" and "classification".

The model can be created by the \code{fit()} function using the
following \emph{engines}:
\itemize{
\item \pkg{mgcv}:  \code{"mgcv"}
}
}
\section{Engine Details}{
Engines may have pre-set default arguments when executing the model fit
call. For this type of model, the template of the fit calls are:

\if{html}{\out{<div class="sourceCode r">}}\preformatted{additive() |>
  set_engine("mgcv") |>
  translate()
}\if{html}{\out{</div>}}

\if{html}{\out{<div class="sourceCode">}}\preformatted{## Generalized Additive Model (GAM) Specification (regression)
## 
## Computational engine: mgcv 
## 
## Model fit template:
## additive::additive_fit(formula = missing_arg(), data = missing_arg(), 
##     weights = missing_arg())
}\if{html}{\out{</div>}}
}

\examples{

additive()

show_model_info("additive")

additive(mode = "classification")
additive(mode = "regression")

set.seed(2020)
dat <- gamSim(1, n = 400, dist = "normal", scale = 2)

additive_mod <-
  additive() |>
  set_engine("mgcv") |>
  fit(
    y ~ s(x0) + s(x1) + s(x2) + s(x3),
    data = dat
  )

summary(additive_mod$fit)

model <- additive(select = FALSE)
model
update(model, select = TRUE)
update(model, select = TRUE, fresh = TRUE)
}
\seealso{
\code{\link[mgcv]{mgcv-package}},
\code{\link[mgcv]{gam}},
\code{\link[mgcv]{bam}},
\code{\link[mgcv]{gamObject}},
\code{\link[mgcv]{gam.models}},
\code{\link[mgcv]{smooth.terms}},
\code{\link[mgcv]{predict.gam}},
\code{\link[mgcv]{plot.gam}},
\code{\link[mgcv]{summary.gam}},
\code{\link[mgcv]{gam.side}},
\code{\link[mgcv]{gam.selection}},
\code{\link[mgcv]{gam.control}},
\code{\link[mgcv]{gam.check}},
\code{\link[mgcv]{vis.gam}},
\code{\link[mgcv]{family.mgcv}},
\code{\link[mgcv]{formula.gam}},
\code{\link[stats]{family}},
\code{\link[stats]{formula}},
\code{\link[stats]{update.formula}}.
}
