#' normal curve maker
#'
#' @param mu mean
#' @param sigma standard deviation
#' @param a x-value that will be shaded up to from -inf
#'
#' @return curve with shaded region from -inf to a and the area under the curve from -inf to a
#' @export
#'
#' @examples
#' \dontrun{myncurve(10,4,1)}
myncurve = function(mu, sigma, a){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))
  xcurve = seq(mu-3*sigma,a,length=1000)
  ycurve = dnorm(xcurve,mu,sigma)
  polygon(c(mu-3*sigma,xcurve,a),c(0,ycurve,0),col="black")
  pnorm(a, mu, sigma)
}
