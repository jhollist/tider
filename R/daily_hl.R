#' Daily Tide Values
#'
#' Pulls out higher high, high, low, lower low tides from daily gauge values.
#' Expects a single day as input or grouped output (i.e. from dplyr::group_by)
#'
#' @param data frame containg tide data and day time.
#' @param level character of column containing gauge data.
#' @param daytime character of column containing daytime values from lubridate.
#'
#'
#' @importFrom lubridate hour
#'
#' @export
#' @examples
#' file<-system.file("extdata","EDC_WaterLevel_Test.csv",package="tider")
#' tides<-read.csv(file,stringsAsFactor=F)
#' tides$DateTime <- lubridate::parse_date_time(paste(tides$Date,tides$Time),
#'                                  "%m/%d/%y %H:%M:%S")
#' daily_hl(tides, "Depth_NAVD88m", "DateTime")

daily_hl<-function(df,level,daytime){
  level <- df[[level]]
  daytime <- df[[daytime]]
  hh<-max(level)
  ll<-min(level)
  dt_hh<-min(daytime[level==hh])
  dt_ll<-min(daytime[level==ll])
  if(hour(dt_hh)>12){
    idx1<-hour(daytime)>=hour(dt_hh)-14
    idx2<-hour(daytime)<=hour(dt_hh)-11
    h_idx<-(idx1+idx2)==2
    h<-max(level[h_idx])
    dt_h<-min(daytime[level==h])
  } else {
    idx1<-hour(daytime)<=hour(dt_hh)+14
    idx2<-hour(daytime)>=hour(dt_hh)+11
    h_idx<-(idx1+idx2)==2
    h<-max(level[h_idx])
    dt_h<-min(daytime[level==h])
  }
  if(hour(dt_ll)>12){
    idx1<-hour(daytime)>=hour(dt_ll)-14
    idx2<-hour(daytime)<=hour(dt_ll)-11
    l_idx<-(idx1+idx2)==2
    l<-min(level[l_idx])
    dt_l<-min(daytime[level==l])
  } else {
    idx1<-hour(daytime)<=hour(dt_ll)+14
    idx2<-hour(daytime)>=hour(dt_ll)+11
    l_idx<-(idx1+idx2)==2
    l<-min(level[l_idx])
    dt_l<-min(daytime[level==l])
  }
  return(list(higher_high=hh,
              dt_higher_high=as.character(dt_hh),
              high=h,
              dt_high=as.character(dt_h),
              low=l,
              dt_low=as.character(dt_l),
              lower_low=ll,
              dt_lower_low=as.character(dt_ll)))
}
