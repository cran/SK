#' Boxcox transform for a response variable
#'
#' @usage bxcxt(y, x)
#'
#' @param y A vector of response variable
#' @param x A vector of explanatory variable
#'
#' @importFrom MASS boxcox
#' @importFrom FitAR bxcx
#' @importFrom stats qqnorm
#'
#' @examples
#' vehicles_obs <- vehicles[vehicles@data$obs1pred0 == 1,]
#' vehicles_obs$transheavy <- bxcxt(vehicles_obs$heavy, vehicles_obs$width)[[1]]
#' qqnorm(vehicles_obs$heavy)
#' qqnorm(vehicles_obs$transheavy)
#' lambda <- bxcxt(vehicles_obs$heavy, vehicles_obs$width)[[2]]
#' @export

bxcxt <- function(y, x){
  bc <- boxcox(y ~ x, plotit = FALSE)
  trans <- bc$x[which.max(bc$y)]
  z <- bxcx(y, trans)
  result <- list("transy" = z, "lambda" = trans)
  return(result)
}
