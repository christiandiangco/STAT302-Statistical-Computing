#' Linear Model
#' 
#' Function that fits a linear model.
#'
#' @param formula Formula object (model to be fitted).
#' @param data Input data frame.
#'
#' @return Table object containing the the estimate, standard error, t-value, and p-value
#'   for each coefficient of the model.
#'   
#' @examples 
#' my_lm(Petal.Width ~ Sepal.Length + Sepal.Width, iris)
#' my_lm(Petal.Length ~ Sepal.Length + Sepal.Width, iris)
#' 
#' @import kableExtra
#' 
#' @export
my_lm <- function(formula, data) {
  # Get sample size from data
  n <- nrow(data)
  # Get X and Y matrices from inputs
  X <- model.matrix(formula, data)
  Y <- model.response(model.frame(formula, data))
  # Solve for coefficients
  beta_hat <- solve(t(X) %*% X) %*% t(X) %*% Y
  # Get degrees of freedom
  df <- n - ncol(X)
  # Estimate sigma squared
  sigma_squared_hat <- sum((Y - X %*% beta_hat)**2 / df)
  
  # For every Coefficient
  # ~~~~~~~~~~~~~~~~~~~~~
  # Estimate standard error
  se <- sqrt(diag(sigma_squared_hat * solve(t(X) %*% X)))
  # Get test statistic
  test_stat <- beta_hat / se
  # Compute p-value (multiply pt by two because two-sided test)
  p_val <- pt(abs(test_stat), df=df, lower.tail=FALSE) * 2
  
  # Return table
  summary_table <- data.frame(estimate = beta_hat, se = se,
                              t_val = test_stat, p_val = p_val)
  colnames(summary_table) <- c("Estimate", "Std. Error", "t value", "p value")
  kable_styling(kbl(summary_table))
}