% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prop_ci.R
\name{ci_prop_wilson}
\alias{ci_prop_wilson}
\title{Wilson CI}
\usage{
ci_prop_wilson(x, conf.level = 0.95, correct = FALSE, data = NULL)
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
Calculates the Wilson interval by calling \code{\link[stats:prop.test]{stats::prop.test()}}.
Also referred to as Wilson score interval.
}
\details{
\deqn{\frac{\hat{p} +
\frac{z^2_{\alpha/2}}{2n} \pm z_{\alpha/2} \sqrt{\frac{\hat{p}(1 - \hat{p})}{n} +
\frac{z^2_{\alpha/2}}{4n^2}}}{1 + \frac{z^2_{\alpha/2}}{n}}}
}
