% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/my_t.test.R
\name{my_t.test}
\alias{my_t.test}
\title{T-test}
\usage{
my_t.test(x, alternative, mu)
}
\arguments{
\item{x}{Numeric vector.}

\item{alternative}{Character string specifying alternative hypothesis, must be
"two.sided", "less", or "greater".}

\item{mu}{A number indicating the null hypothesis value of the mean.}
}
\value{
List containing the test statistic, degrees of freedom, alternative hypothesis, and p-value.
}
\description{
Function that conducts a one sample t-test.
}
\examples{
my_t.test(iris$Sepal.Length, "two.sided", 5)
my_t.test(iris$Sepal.Length, "less", 5)
my_t.test(iris$Sepal.Length, "greater", 5)

}
