% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prop_ci.R
\name{ci_prop_wald}
\alias{ci_prop_wald}
\title{Wald CI}
\usage{
ci_prop_wald(x, conf.level = 0.95, correct = FALSE, data = NULL)
}
\arguments{
\item{x}{(\code{binary}/\code{numeric}/\code{logical})\cr
vector of a binary values, i.e. a logical vector, or numeric with values \code{c(0, 1)}}

\item{conf.level}{(\verb{scalar numeric}) \cr a scalar in (0,1) indicating the confidence level. Default is 0.95}

\item{correct}{(\code{logical})\cr apply continuity correction.}

\item{data}{(\code{data.frame}) \cr Optional data frame containing the variables specified in \code{x} and \code{by}.}
}
\value{
An object containing the following components:

\item{n}{Number of responses}
\item{N}{Total number}
\item{estimate}{The point estimate of the proportion}
\item{conf.low}{Lower bound of the confidence interval}
\item{conf.high}{Upper bound of the confidence interval}
\item{conf.level}{The confidence level used}
\item{method}{Type of method used}
}
\description{
Calculates the Wald interval by following the usual textbook definition
for a single proportion confidence interval using the normal approximation.
}
\details{
\deqn{\hat{p} \pm z_{\alpha/2} \sqrt{\frac{\hat{p}(1 - \hat{p})}{n}}}
}
\examples{
# example code
x <- c(
TRUE, TRUE, TRUE, TRUE, TRUE,
FALSE, FALSE, FALSE, FALSE, FALSE
)

ci_prop_wald(x, conf.level = 0.9)

}
