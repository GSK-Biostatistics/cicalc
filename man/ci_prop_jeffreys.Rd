% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prop_ci.R
\name{ci_prop_jeffreys}
\alias{ci_prop_jeffreys}
\title{Jeffreys CI}
\usage{
ci_prop_jeffreys(x, conf.level = 0.95, data = NULL)
}
\arguments{
\item{x}{(\code{binary}/\code{numeric}/\code{logical})\cr
vector of a binary values, i.e. a logical vector, or numeric with values \code{c(0, 1)}}

\item{conf.level}{(\verb{scalar numeric}) \cr a scalar in (0,1) indicating the confidence level. Default is 0.95}

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
Calculates the Jeffreys interval, an equal-tailed interval based on the
non-informative Jeffreys prior for a binomial proportion.
}
\details{
\deqn{\left( \text{Beta}\left(\frac{k}{2} + \frac{1}{2}, \frac{n - k}{2} + \frac{1}{2}\right)_\alpha,
\text{Beta}\left(\frac{k}{2} + \frac{1}{2}, \frac{n - k}{2} + \frac{1}{2}\right)_{1-\alpha} \right)}
}
