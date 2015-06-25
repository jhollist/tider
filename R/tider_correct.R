#' Calculate corrected spring, neap, etc.
#'
#' Need better description here
#'
#' @param mhw_acc A numeric of the Mean High Water of the nearest NOAA Control Station
#' @param s2_ctrl A numeric of the Prinicipal solor semidiurnal constituent from the nearest NOAA
#'                control station.
#' @param mn_acc A numeric of the Mean range from nearest NOAA control station
#' @param type the corrected value to return
#' @return returns a numeric of the corrected spring or neap
#' @export
tider_correct <- function(mhw_acc,s2_ctrl,mn_acc,type=c("spring","neap")){
  type <- match.args(type)
  mwhws_ctrl <- mw_acc + s2ctrl

  ### OTHER STUFF HERE
  ### Need to pull in monthly mean high and monthly mean low from the sampling
  ### stations and from the nearest control station
  output <- NULL

  return(output)
}
