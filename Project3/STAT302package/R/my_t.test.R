#' T-test
#'
#' Function that conducts a one sample t-test.
#'
#' @param x Numeric vector.
#' @param alternative Character string specifying alternative hypothesis, must be
#'   "two.sided", "less", or "greater".
#' @param mu A number indicating the null hypothesis value of the mean.
#'
#' @return List containing the test statistic, degrees of freedom, alternative hypothesis, and p-value.
#'
#' @examples
#' my_t.test(iris$Sepal.Length, "two.sided", 5)
#' my_t.test(iris$Sepal.Length, "less", 5)
#' my_t.test(iris$Sepal.Length, "greater", 5)
#'
#' @export
my_t.test <- function(x, alternative, mu) {
  # Check if character string for alternative is valid
  if (!(alternative %in% c("two.sided", "less", "greater"))) {
    stop("alternative must be either 'two.sided', 'less', or 'greater'")
  }
  # Compute sample mean of x for estimate of mu
  mu_hat <- mean(x)
  # Compute standard error of estimate
  se <- sd(x) / sqrt(length(x))
  # Compute test statistic
  test_stat <- (mu_hat - mu) / se
  # Compute degrees of freedom for t-distribution
  df <- length(x) - 1
  # Compute p-value
  if (alternative == "two.sided") {
    p_val <- pt(-test_stat, df=df) + pt(test_stat, df=df,
                                        lower.tail=FALSE)
  } else if (alternative == "less") {
    p_val <- pt(test_stat, df=df)
  } else {
    p_val <- pt(test_stat, df=df, lower.tail=FALSE)
  }
  # Create list to return as output
  out <- list(test_stat, df, alternative, p_val)
  names(out) <- c("test_stat", "df", "alternative", "p_val")
  return (out)
}
