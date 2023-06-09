% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generic-s3.R
\name{print.morf.marginal}
\alias{print.morf.marginal}
\title{Print Method for morf.marginal Objects}
\usage{
\method{print}{morf.marginal}(x, latex = FALSE, ...)
}
\arguments{
\item{x}{An \code{morf.marginal} object.}

\item{latex}{If \code{TRUE}, prints LATEX code.}

\item{...}{Further arguments passed to or from other methods.}
}
\value{
Prints an \code{morf.marginal} object.
}
\description{
Prints an \code{morf.marginal} object.
}
\details{
Compilation of the LATEX code requires the following packages: \code{booktabs}, \code{float}, \code{adjustbox}. If
standard errors have been estimated, they are printed in parenthesis below each point estimate.
}
\examples{
## Load data from orf package.
set.seed(1986)

library(orf)
data(odata)
odata <- odata[1:200, ] # Subset to reduce elapsed time.

y <- as.numeric(odata[, 1])
X <- as.matrix(odata[, -1])

## Fit morf. Use large number of trees.
forests <- morf(y, X, n.trees = 4000)

## Marginal effects at the mean.
me <- marginal_effects(forests, eval = "atmean")
print(me)
print(me, latex = TRUE)

\donttest{
## Add standard errors.
honest_forests <- morf(y, X, n.trees = 4000, honesty = TRUE)
honest_me <- marginal_effects(honest_forests, eval = "atmean", inference = TRUE)
print(honest_me, latex = TRUE)}

}
\seealso{
\code{\link{morf}}, \code{\link{marginal_effects}}.
}
\author{
Riccardo Di Francesco
}
