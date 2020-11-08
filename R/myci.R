#' my confidence interval
#'
#' @param x vector of data
#' @param conf.int the confidence interval, defaulted to 95%
#'
#' @return a confidence interval for the mean of the data given in x
#' @export
#'
#' @examples
#' \dontrun{set.seed(30); x=rnorm(30,mean=10,sd=12); myci(x)}
myci=function(x,conf.int = 0.95) {
  mean(x)+c(-1,1)*qt(1-(1-conf.int)/2,length(x)-1)*sd(x)/sqrt(length(x))
}
