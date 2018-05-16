#' Function for Segment-based Kriging models
#'
#' @description Segment-based Kriging models, including Segment-based Ordinary Kriging (SOK) and
#' Segment-based Regression Kriging (SRK), for spatial prediction of
#' line segment spatial data (polyline). The methods are described in
#' Yongze Song (2018) <doi:10.1109/TITS.2018.2805817>.
#'
#' @usage skriging(formula, polyline = polyline, method = "srk",
#'           lwd = "width", obspred = "obs1pred0", boxcox = TRUE)
#' \method{print}{skriging}(x, ...)
#' \method{plot}{skriging}(x, studyarea = NULL, ...)
#'
#' @aliases skriging print.skriging plot.skriging
#'
#' @param formula A skriging formula.
#' @param polyline A shapefile of spatial polyline.
#' @param method A characteor of segment-based Kriging model. The default is "srk",
#' segment-based regression Kriging Another method is "sok", segment-based ordinary
#' Kriging.
#' @param lwd A fixed number or a variable name of polyline of the line width.
#' @param obspred A variable name of polyline to define the observation and prediction lines.
#' Observation is 1 and prediction is 0.
#' @param boxcox A logical parameter to set whether the dependent variable should be transformed
#' with boxcox function. The default is TRUE.
#' @param x A list of \code{skriging} result.
#' @param studyarea A shapefile of spatial polygon of study area.
#' @param ... new print and plot
#'
#' @importFrom graphics par
#' @importFrom sp SpatialPolygonsDataFrame spplot layout.scale.bar layout.north.arrow
#' @importFrom RColorBrewer brewer.pal
#' @importFrom GD disc
#' @importFrom stats as.formula lm predict median
#' @importFrom rgeos gBuffer
#' @importFrom rtop createRtopObject rtopFitVariogram rtopKrige
#' @importFrom FitAR bxcx
#' @importFrom utils capture.output
#'
#' @examples
#' ## SRK: segment-based regression Kriging
#' ## dataset 'vtest' is a sample of dataset 'vehicles'
#' srk1 <- skriging(heavy ~ wpai + width, polyline = vtest, method = "srk",
#'                  lwd = "width", obspred = "obs1pred0", boxcox = TRUE)
#' srk1
#' plot(srk1)
#'
#' @export

skriging <- function(formula, polyline = polyline, method = "srk",
                     lwd = "width", obspred = "obs1pred0", boxcox = TRUE){

  formula <- as.formula(formula)

  namepolyline <- names(polyline)

  y <- polyline@data[,namepolyline == formula[[2]]]
  polyline$y <- y

  if (class(lwd) == "character"){
    width1 <- polyline@data[,namepolyline == lwd]
  } else if (class(lwd) == "numeric"){
    width1 <- rep(lwd, nrow(polyline@data))
  } else {
    warnings("lwd should be a number or a variable of polyline.")
  }

  op1 <- polyline@data[,namepolyline == obspred]
  polyline$obspred <- op1

  if (boxcox == TRUE){
    k <- which(op1 == 1)
    polyline$transh <- NA
    bxcxt1 <- bxcxt(y[k], width1[k])
    polyline$transh[k] <- bxcxt1[[1]]
    trans1 <- bxcxt1[[2]]
  } else {
    polyline$transh <- y
  }

  ## generate polygon
  linebf <- gBuffer(polyline, width=width1, byid=TRUE )
  ## add data to shapefile
  linebf <- SpatialPolygonsDataFrame(linebf, data=linebf@data )
  ## generate obs and pred
  linebf_obs <- linebf[which(op1 == 1),]
  linebf_pred <- linebf[which(op1 == 0),]

  ## mean (SOK) or linear regression (skriging)
  xx <- as.character(formula[[3]])
  formula2 <- NA
  if (method == "sok" && xx == 1){
    formula2 <- as.formula(paste("transh", paste(xx, collapse=" + "), sep=" ~ "))
  } else if (method == "srk" && xx != 1){
    xx <- xx[-which(xx == "+")]
    formula2 <- as.formula(paste("transh", paste(xx, collapse=" + "), sep=" ~ "))
  } else if (method == "sok" && xx != 1){
    cat("In SOK model, the form of formula should be 'y ~ 1'.\n")
  } else {
    cat("In SRK model, one or several exxplanatory variables are required.\n")
  }

  lmh <- lm(formula2, data = linebf_obs)
  pred.lmh <- predict(lmh, linebf_pred, se.fit = FALSE)
  ## residuals are observations in Kriging
  linebf_obs$transh_res <- lmh$residuals

  ## rtop Kriging
  invisible(capture.output(rtopkriging <- createRtopObject(linebf_obs, linebf_pred,
                                  formulaString = transh_res ~ 1,
                                  params = list(gDist = TRUE, rresol = 25))))
  invisible(capture.output(rtopkriging <- rtopFitVariogram(rtopkriging)))
  invisible(capture.output(rtopkriging <- rtopKrige(rtopkriging)))

  pred.rtoph <- rtopkriging$predictions$var1.pred
  pred1 <- pred.lmh + pred.rtoph
  stdev1 <- sqrt(rtopkriging$predictions$var1.var )

  if (boxcox == TRUE){
    inverstranspred1 <- bxcx(pred1, trans1, InverseQ = TRUE)
    inverstransstdev1 <- (pred1 * trans1 + 1)^((1 - trans1)/trans1) * stdev1
    uncertainty1 <- inverstransstdev1/inverstranspred1
  } else {
    inverstranspred1 <- pred1
    inverstransstdev1 <- stdev1
    uncertainty1 <- inverstransstdev1/inverstranspred1
  }
  polyline$y[op1 == 0] <- inverstranspred1
  polyline$stdev <- NA
  polyline$stdev[op1 == 0] <- inverstransstdev1
  polyline$uncertainty <- NA
  polyline$uncertainty[op1 == 0] <- uncertainty1

  result <- list("polyline" = polyline)
  ## define class
  class(result) <- "skriging"
  result
}


print.skriging <- function(x, ...){

  obs <- x$polyline@data$y[x$polyline@data$obspred == 1]
  pred <- x$polyline@data$y[x$polyline@data$obspred == 0]
  kriging_stdev <- x$polyline@data$stdev[x$polyline@data$obspred == 0]
  kriging_uncertainty <- x$polyline@data$uncertainty[x$polyline@data$obspred == 0]
  summary_obspred <- function(obspred_num){
    result <- as.data.frame(cbind("N" = length(obspred_num), "Min." = min(obspred_num, digits = 2),
                         "Median" = median(obspred_num), "Mean" = mean(obspred_num),
                         "Max." = max(obspred_num)))
    return(result)
  }
  s_obs <- summary_obspred(obs)
  s_pred <- summary_obspred(pred)
  s_kstdev <- summary_obspred(kriging_stdev)
  s_kuncertainty <- summary_obspred(kriging_uncertainty)
  s_result <- rbind(s_obs, s_pred, s_kstdev, s_kuncertainty)
  rownames(s_result) <- c("observation", "prediction",
                          "kriging stdev", "kriging uncertainty")
  cat("skriging result includes a polyline shapefile ... \n")
  cat("polyline shapefile can be derived by 'yourskrigingresult$polyline' ... \n")
  cat("summary of observations and predictions: \n")
  print(s_result, digits = 3)
  invisible(x)
}


plot.skriging <- function(x, studyarea = NULL, ...){

  if (is.null(studyarea)){
    obs <- x$polyline[x$polyline$obspred == 1,]
    obs.layer <- list("sp.lines", obs, col = "gray70", lwd = 2)
    ## plot y and uncertainty
    my.palette <- rev(brewer.pal(n = 7, name = "RdYlGn"))
    colbreaks <- disc(x$polyline$y, n = 6)$itv
    p1 <- spplot(x$polyline, "y",
                 col.regions = my.palette, at = colbreaks, lwd=2)
    p2 <- spplot(x$polyline, "uncertainty",
                 sp.layout = list(obs.layer),
                 col="transparent", lwd=2)
  } else {
    ## basic elements of maps
    scale1 <- signif((studyarea@bbox[1,2] - studyarea@bbox[1,1])/10^10, digits = 1) * 10^9
    scale <- list("SpatialPolygonsRescale", layout.scale.bar(height = 0.2),
                  scale = scale1, fill = c("transparent", "black"),
                  offset = c(studyarea@bbox[1,1] + scale1*3/5, studyarea@bbox[2,1] + scale1*6/5))

    # The scale argument sets length of bar in map units
    text1 <- list("sp.text", c(studyarea@bbox[1,1] + scale1*3/5,
                               studyarea@bbox[2,1] + scale1*4/5), "0")
    text2 <- list("sp.text", c(studyarea@bbox[1,1] + scale1*8/5,
                               studyarea@bbox[2,1] + scale1*4/5), as.character(scale1))

    arrow <- list("SpatialPolygonsRescale", layout.north.arrow(),
                  offset = c(studyarea@bbox[1,2] - scale1, studyarea@bbox[2,2] - scale1),
                  scale = scale1)

    ## layer lists of studyarea and obs
    studyarea.layer <- list("sp.polygons", studyarea, col = "black")
    obs <- x$polyline[x$polyline$obspred == 1,]
    obs.layer <- list("sp.lines", obs, col = "gray70", lwd = 2)

    ## plot y and uncertainty
    my.palette <- rev(brewer.pal(n = 7, name = "RdYlGn"))
    colbreaks <- disc(x$polyline$y, n = 6)$itv

    p1 <- spplot(x$polyline, "y",
                 sp.layout = list(studyarea.layer, scale, text1, text2, arrow),
                 col.regions = my.palette, at = colbreaks, lwd=2,
                 xlim=c(studyarea@bbox[1,1], studyarea@bbox[1,2]),
                 ylim=c(studyarea@bbox[2,1], studyarea@bbox[2,2]))
    p2 <- spplot(x$polyline, "uncertainty",
                 sp.layout = list(studyarea.layer, obs.layer, scale, text1, text2, arrow),
                 col="transparent", lwd=2,
                 xlim=c(studyarea@bbox[1,1], studyarea@bbox[1,2]),
                 ylim=c(studyarea@bbox[2,1], studyarea@bbox[2,2]))
  }
  par(mfrow=c(1,2))
  print(p1, position = c(0,0,0.5,1), more = TRUE)
  print(p2, position = c(0.5,0,1,1), more = FALSE)
  par(mfrow=c(1,1))
  plots <- list("obspred" = p1, "uncertainty" = p2)
}

