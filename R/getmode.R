#' Mode function
#'
#' More detailed description
#'
#' @param x Vecor, data frame, or array
#'
#' @return the mode of that data
#'
#' @export
#'
#' @examples
#' \dontrun{getmode(c(12, 18, 18, 24))}
#'
getmode <- function(x) {
  uniqv <- unique(x)
  uniqv[which.max(tabulate(match(x, uniqv)))]
}
