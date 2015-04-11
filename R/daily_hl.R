#' Daily Tide Values
#'
#' Pulls out higher high, high, low, lower low tides from daily gauge values
#'
#' @param level a vector gauge data. length and order same as daytime
#' @param daytime a vector of daytime values from lubridate.
#'                length and order same as daytime
#'
#' @import lubridate
#'
#' @export
#' @examples
#' file<-system.file("extdata","EDC_WaterLevel_Test.csv",package="tider")
#' tides<-read.csv(file,stringsAsFactor=F)
#' tides$DateTime <- lubridate::parse_date_time(paste(tides$Date,tides$Time),
#'                                  "%m/%d/%y %H:%M:%S")
#' daily_hl(tides$Depth_NAVD88m,tides$DateTime)

daily_hl<-function(level,daytime){
  hh<-max(level)
  ll<-min(level)
  dt_hh<-daytime[level==hh]
  dt_ll<-daytime[level==ll]
  if(hour(dt_hh)>12){
    idx1<-hour(daytime)>=hour(dt_hh)-14
    idx2<-hour(daytime)<=hour(dt_hh)-11
    h_idx<-(idx1+idx2)==2
    h<-max(level[h_idx])
  } else {
    idx1<-hour(daytime)<=hour(dt_hh)+14
    idx2<-hour(daytime)>=hour(dt_hh)+11
    h_idx<-(idx1+idx2)==2
    h<-max(level[h_idx])
  }
  if(hour(dt_ll)>12){
    idx1<-hour(daytime)>=hour(dt_ll)-14
    idx2<-hour(daytime)<=hour(dt_hh)-11
    l_idx<-(idx1+idx2)==2
    l<-min(level[l_idx])
  } else {
    idx1<-hour(daytime)<=hour(dt_ll)+14
    idx2<-hour(daytime)>=hour(dt_ll)+11
    l_idx<-(idx1+idx2)==2
    l<-min(level[l_idx])
  }
  idx<-daytime
  return(list(higher_high=hh,high=h,low=l,lower_low=ll))
}
