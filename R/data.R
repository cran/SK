#' @title Spatial dataset of traffic volumes
#'
#' @description The "vehicles" dataset is the spatial data of traffic volumes
#' in Wheatbelt region, Western Australia (WA), Australia, in 2015.
#' The format is polyline. The attributes include road properties (width and length)
#' and traffic volumes of heavy, light and total vehicles.
#' The variable "obs1pred0" defines the the road segments that have observations or
#' to be predicted.
#' More details and data sources can be referred in the Citation Info of the package.
#'
#' @name vehicles
#' @format \code{vehicles}: A Spatial Lines Data Frame with 280 rows and 10 variables.
#' \describe{
#'   \item{segmentID}{Number of road segment}
#'   \item{obs1pred0}{Observation is 1 and prediction is 0}
#'   \item{length}{Length of road segment}
#'   \item{width}{Width of road segment}
#'   \item{heavy}{Traffic volumes of heavy vehicles}
#'   \item{light}{Traffic volumes of light vehicles}
#'   \item{total}{Total trffic volumes}
#'   \item{wpai}{Weighted population accessibility index}
#'   \item{longitude}{Longitude of the center of road segment}
#'   \item{latitude}{Longitude of the center of road segment}
#' }
#' @docType data
#' @author Yongze Song \email{yongze.song@postgrad.curtin.edu.au}
#' @keywords dataset vehicles
"vehicles"

#' @rdname vehicles
"vtest"

#' @title Spatial dataset of study area
#'
#' @description The "wheatbelt" dataset is spatial polylon of the study area
#' Wheatbelt region, Western Australia (WA), Australia.
#' More details and data sources can be referred in the Citation Info of the package.
#'
#' @name wheatbelt
#' @format \code{wheatbelt}: A Spatial Polygon Data Frame.
#' @docType data
#' @author Yongze Song \email{yongze.song@postgrad.curtin.edu.au}
#' @keywords dataset wheatbelt
"wheatbelt"

