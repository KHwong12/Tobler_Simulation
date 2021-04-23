rad2deg <- function(rad) {(rad * 180) / (pi)}
deg2rad <- function(deg) {(deg * pi) / (180)}

#' @title Tobler's hiking function
#'
#' @description Calculate walking speed at specific slope. Parameters used in Tobler's hiking function in maximum speed.
#'However, people usually refer walking speed as walking speed in flat terrain. This function reverses the calculation by 
#'first accepting the flat terrain speed to reversely compute the maximum walking speed at 2.86 degree downhill.
#'
#' @param slope_angle Slope of the path, in degree.
#' @param flat_terrain_speed Walking speed on flat terrain, in km/hr. Default to be 5.03.
#'
#' @return Walking speed at designated terrain, in km/hr (double) 
#' @export
#'
#' @examples
toblers_hiking_function <- function(slope_angle, flat_terrain_speed = 5.036742) {
  
  
  # Get max walking speed from flat terrain walking speed
  max_speed = flat_terrain_speed / exp(-3.5 * 0.05)
  
  max_speed * exp(-3.5 * abs(tan(deg2rad(slope_angle)) + 0.05))
}



#' @title Calculate Walking time
#'
#' @description Calculate walking time with given path length and walking speed. 
#'
#' @param speed Walking speed.
#' @param path_length Length of the path.
#'
#' @return A walking time value (double) 
#' @export
#'
#' @examples
walking_time <- function(speed, path_length) {
  path_length / speed
}