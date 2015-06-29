#' Calculate mean high water spring, neap, etc.
#'
#' During spring tides higher high tides are produced. This
#' package calculates mean high water spring at a desired location.
#' Since often only short term data is available, this package uses the
#' simltaneous comparison method to compute the datum. Data from the desired location and a
#' control station for the same time period is required.
#'
#' @param df_sub data frame for subordinate/short term station
#' @param df_ctrl data fram for control station
#' @param level character of column containg gauge data
#' @param daytime character of column containg daytime values
#' @param mhw_ctrl A numeric of the Mean High Water of the nearest NOAA Control Station
#' @param s2_ctrl A numeric of the Prinicipal solor semidiurnal constituent from the nearest NOAA
#'                control station.
#' @param mn_ctrl A numeric of the Mean range of tide from nearest NOAA control station
#' @param type the corrected value to return
#' @return returns a numeric of the mean high water spring or neap
#' @export
#'
#'sub$DateTime <- lubridate::parse_date_time(paste(sub$Date,sub$Time),
#'                                           "%m/%d/%y %H:%M:%S")
#'ctrl$DateTime <- lubridate::parse_date_time(paste(ctrl$Date,ctrl$Time),
#'                                           "%m/%d/%y %H:%M:%S")

tider_correct <- function(df_sub, df_ctrl,level, daytime, mhw_ctrl,s2_ctrl,mn_ctrl, type=c("spring","neap")){
  type <- match.args(type)

  mwhs_ctrl <- mhw_ctrl + s2_ctrl

  ### fill in gaps if less than four hours other wise
  ### stop and tell user need to fill in gaps

  daily_sub<-tider_byday(df_sub,"level","daytime")
  daily_ctrl<-tider_byday(ctrl,"level","daytime")

  ###get rid of any -Inf or Inf in data

  df_sub$higher_high <- as.numeric(as.character(df_sub$higher_high))
  df_sub$lower_low <- as.numeric(as.character(df_sub$lower_low))
  df_sub$high <- as.numeric(as.character(df_sub$high))
  df_sub$low <- as.numeric(as.character(df_sub$low))

  df_ctrl$higher_high <- as.numeric(as.character(df_ctrl$higher_high))
  df_ctrl$lower_low <- as.numeric(as.character(df_ctrl$lower_low))
  df_ctrl$high <- as.numeric(as.character(df_ctrl$high))
  df_ctrl$low <- as.numeric(as.character(df_ctrl$low))


  mm_sub<-df_sub %>%
    group_by(Year=year(ymd), Month=month(ymd)) %>%
    summarise(mmhw_sub=mean(high),
              mmlw_sub=mean(low),
              mmn_sub = mean(high) - mean(low))
  mm_ctrl<-df_ctrl %>%
    group_by(Year=year(ymd), Month=month(ymd)) %>%
    summarise(mmhw_ctrl=mean(high),
              mmlw_ctrl=mean(low),
              mmn_ctrl = mean(high) - mean(low))


  ratio_mmn <- c(mmn_sub/mmn_ctrl)
  sums <- sum(ratio_mmn)
  total_months <- NROW(mm_ctrl)
  means <- sums/total_months
  mn_sub <- mn_ctrl + means


  mhws  <- mn_ctrl/mn_sub * mhws_ctrl

  ### figure out error of mhws value
  ### need to return error

  return(mhws)
}
