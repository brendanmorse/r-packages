#' Standard Error Summary function
#'
#' This function calculates standard errors for groups and summarizes the data for clean plotting with ggplot2.
#' @param data Specifies the dataframe/matrix. Required.
#' @param measurevar Specifies the measured variable to be summarized. Required.
#' @param groupvars Specifies the grouping variable(s). Required.
#' @param na.rm What to do with missing values. Default = FALSE.
#' @param conf.interval Specifies the desired confidence interval. Default = 95%
#' @export
#' @examples
#' x <- data.frame(condition = as.factor(c(rep("control", 50), rep("experimental", 50))), IQ = runif(100,90,110))
#' summarySE(data = x, measurevar = "IQ", groupvars = "condition")

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE, conf.interval=.95) {
    require(plyr)
    length2 <- function (x, na.rm=FALSE) {
        if (na.rm) sum(!is.na(x))
        else       length(x)
    }
    datac <- ddply(data, groupvars, .drop=TRUE,
      .fun = function(xx, col) {
        c(N    = length2(xx[[col]], na.rm=na.rm),
          mean = mean   (xx[[col]], na.rm=na.rm),
          sd   = sd     (xx[[col]], na.rm=na.rm)
        )
      },
      measurevar
    )
    datac    <- rename(datac, c("mean" = measurevar))
    datac$se <- datac$sd / sqrt(datac$N)
    ciMult   <- qt(conf.interval/2 + .5, datac$N-1)
    datac$ci <- datac$se * ciMult
    return(datac)
}