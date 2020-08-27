rad2deg <- function(rad) {(rad * 180) / (pi)}
deg2rad <- function(deg) {(deg * pi) / (180)}


# The parameter in Tobler's hiking function uses maximum speed
# Yet, flat terrain is mostly used
# Therefore, reverse method by using given flat terrain speed input to calculate max speed parameter

#' Tobler's hiking function
#'
#' Calculate walking speed as specific slope
#'
#' @param slope_angle 
#' @param flat_terrain_speed 
#'
#' @return
#' @export
#'
#' @examples
toblers_hiking_function <- function(slope_angle = 0, flat_terrain_speed = 5.036742) {
  
  
  # Get max walking speed from flat terrain walking speed
  max_speed = flat_terrain_speed / exp(-3.5 * 0.05)
  
  max_speed * exp(-3.5 * abs(tan(deg2rad(slope_angle)) + 0.05))
}