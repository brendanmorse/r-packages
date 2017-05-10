#' Logistic Regression pseudo-R-square function
#'
#' This function calculates psuedo r-square values for logistic regression models.
#' @param lreg.model Specifies the fitted logistic regression model. Required.
#' @export
#' @examples
#' x <- data.frame(condition = as.factor(c(rep("control", 50), rep("experimental", 50))), pass = round(runif(100)))
#' x.lreg <- glm(pass ~ condition, data = x, family = binomial)
#' summary(x.lreg)
#' pr2(x.lreg)

pr2 <- function(lreg.model){
	dev <- lreg.model$deviance
	null.dev <- lreg.model$null.deviance
	model.n <- length(lreg.model$fitted.values)
	r.l <- 1 - dev / null.dev
	r.cs <- 1 - exp(-(null.dev - dev) / model.n)
	r.n <- r.cs / (1 - (exp(-(null.dev / model.n))))
	cat("Pseudo R^2 for logistic regression model\n")
	cat("Hosmer and Lemeshow R^2 ", round(r.l, 3), "\n")
	cat("Cox and Snell R^2 ", round(r.cs, 3), "\n")
	cat("Nagelkerke R^2 ", round(r.n, 3), "\n")
}