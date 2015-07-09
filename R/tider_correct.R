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
#'df_sub$DateTime <- lubridate::parse_date_time(paste(df_sub$Date,df_sub$Time),
#'                                           "%m/%d/%y %H:%M:%S")
#'df_ctrl$DateTime <- lubridate::parse_date_time(paste(df_ctrl$Date,df_ctrl$Time),
#'                                        "%m/%d/%y %H:%M:%S")

spring  <- tider_correct(sub, ctrl, "level",
                         "DateTime",
                         "level",
                         "DateTime",
                         1.6, .35, 4.07, "spring")

tider_correct <- function(df_sub, df_ctrl,sub_level, sub_daytime, ctrl_level, ctrl_daytime,
                          mhw_ctrl,s2_ctrl,mn_ctrl, type=c("spring","neap")){
  #type <- match.args(type)

  require(dplyr)
  require(lubridate)
  require(tider)


  df_sub  <- data.frame(sub_level = df_sub$sub_level, sub_daytime = df_sub$sub_daytime )
  df_ctrl  <- data.frame(ctrl_level = df_ctrl$ctrl_level, ctrl_daytime = df_ctrl$ctrl_daytime )



  ### check for gaps
  ### 4 hrs or less fill using least sqaures curve
  ### over 4hrs stop program and tell user to fill

  mwhs_ctrl <- mhw_ctrl + s2_ctrl

  #find daily high and low tides
  daily_sub<-tider_byday(df_sub,"sub_level","sub_daytime")
  daily_ctrl<-tider_byday(df_ctrl,"ctrl_level","ctrl_daytime")

  daily_sub$higher_high <- as.numeric(as.character(daily_sub$higher_high))
  daily_sub$lower_low <- as.numeric(as.character(daily_sub$lower_low))
  daily_sub$high <- as.numeric(as.character(daily_sub$high))
  daily_sub$low <- as.numeric(as.character(daily_sub$low))
  daily_ctrl$higher_high <- as.numeric(as.character(daily_ctrl$higher_high))
  daily_ctrl$lower_low <- as.numeric(as.character(daily_ctrl$lower_low))
  daily_ctrl$high <- as.numeric(as.character(daily_ctrl$high))
  daily_ctrl$low <- as.numeric(as.character(daily_ctrl$low))

  daily_sub[ is.infinite(daily_sub$low), "low"  ]  <- daily_sub[ is.infinite(daily_sub$low), "lower_low" ]
  daily_sub[ is.infinite(daily_sub$high), "high"  ]  <- daily_sub[ is.infinite(daily_sub$high), "higher_high" ]
  daily_ctrl[ is.infinite(daily_ctrl$low), "low"  ]  <- daily_ctrl[ is.infinite(daily_ctrl$low), "lower_low" ]
  daily_ctrl[ is.infinite(daily_ctrl$high), "high"  ]  <- daily_ctrl[ is.infinite(daily_ctrl$high), "higher_high" ]
  daily_sub[ is.na(daily_sub$low), "low"  ]  <- daily_sub[ is.na(daily_sub$low), "lower_low" ]
  daily_sub[ is.na(daily_sub$high), "high"  ]  <- daily_sub[ is.na(daily_sub$high), "higher_high" ]
  daily_ctrl[ is.na(daily_ctrl$low), "low"  ]  <- daily_ctrl[ is.na(daily_ctrl$low), "lower_low" ]
  daily_ctrl[ is.na(daily_ctrl$high), "high"  ]  <- daily_ctrl[ is.na(daily_ctrl$high), "higher_high" ]

  #calculate monthly values
  mm_sub<-daily_sub %>%
    group_by(Year=year(ymd), Month=month(ymd)) %>%
    summarise(sum_hh = sum(higher_high),
              sum_h= sum(high),
              sum_l= sum(low),
              sum_ll = sum(lower_low),
              num_obs = NROW(high))

  mm_sub<-daily_sub %>%
    group_by(Year=year(ymd), Month=month(ymd)) %>%
    summarise(sum_hh = sum(higher_high),
              sum_h= sum(high),
              sum_l= sum(low),
              sum_ll = sum(lower_low),
              num_obs = NROW(high))

  m_mhw <- cbind(mm_sub$sum_hh + mm_sub$sum_h)/(mm_sub$num_obs*2)
  mm_sub$mmhw <- m_mhw
  m_mlw  <-  cbind(mm_sub$sum_ll + mm_sub$sum_l)/(mm_sub$num_obs *2)
  mm_sub$mmlw <- m_mlw
  m_mn  <-  cbind(mm_sub$mmhw - mm_sub$mmlw)
  mm_sub$mmn   <- m_m

  mm_ctrl<-daily_ctrl %>%
    group_by(Year=year(ymd), Month=month(ymd)) %>%
    summarise(sum_hh = sum(higher_high),
              sum_h= sum(high),
              sum_l= sum(low),
              sum_ll = sum(lower_low),
              num_days = NROW(high))
  m_mhw <- cbind(mm_ctrl$sum_hh + mm_ctrl$sum_h)/(mm_ctrl$num_days*2)
  mm_ctrl$mmhw <- m_mhw
  m_mlw  <-  cbind(mm_ctrl$sum_ll + mm_ctrl$sum_l)/(mm_ctrl$num_days *2)
  mm_ctrl$mmlw <- m_mlw
  m_mn  <-  cbind(mm_ctrl$mmhw - mm_ctrl$mmlw)
  mm_ctrl$mmn   <- m_mn

  #simultaneous comparsion method
  ratio_mmn <- c(mm_sub$mmn/mm_ctrl$mmn)
  sums <- sum(ratio_mmn)
  total_months <- NROW(mm_ctrl)
  means <- sums/total_months
  mn_sub <- mn_ctrl + means

  mhws  <- mn_sub/mn_ctrl * mhws_ctrl

  return(mhws)
}
