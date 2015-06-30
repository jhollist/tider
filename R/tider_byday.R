#' Return a data frame of tide metrics per day
#'
#' @param data frame containg tide data and day time.
#' @param level character of column containing gauge data.
#' @param daytime character of column containing daytime values from lubridate.
#' @import lubridate
#' @export
#' @examples
#' file<-system.file("extdata","TideFile2days.csv",package="tider")
#' tides<-read.csv(file,stringsAsFactor=F)
#' tides$DateTime <- lubridate::parse_date_time(paste(tides$Date,tides$Time),
#'                                  "%m/%d/%y %H:%M:%S")
#' tider_byday(tides, "Depth_NAVD88m", "DateTime")
tider_byday<-function(df,level,daytime){
 df <- data.frame(level = df[[level]],daytime = df[[daytime]],
                  ymd = factor(as.character(strptime(df[[daytime]],"%Y-%m-%d"))))
 #Do for each then combine.
 out_hh<-tider_process_byday(df,"hh")
 out_h<-tider_process_byday(df,"h")
 out_l<-tider_process_byday(df,"l")
 out_ll<-tider_process_byday(df,"ll")
 out<-rbind(out_hh,out_h,out_l,out_ll)[,1:3]
 out$year <- year(out$date_time)
 out$month <- month(out$date_time)
 out$day <- day(out$date_time)
 return(out)
}

#' Daily Tide Values
#'
#' Pulls out higher high, high, low, lower low tides from daily gauge values.
#' Expects a single day as input or grouped output (i.e. from dplyr::group_by)
#'
#' @param level numeric vector containing gauge data.
#' @param daytime POSIXct vector of daytime for a single day
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

tider_hl<-function(level,daytime,metric=c("hh","h","l","ll")){
  metric<-match.arg(metric)
  hh<-max(level)
  dt_hh<-min(daytime[level==hh])
  ll<-min(level)
  dt_ll<-min(daytime[level==ll])
  #if(!exists("dt_hh")){browser()}
  if(metric=="hh"){
    return(data.frame(date_time=dt_hh,type="hh",level=hh))
  }
  if(metric=="ll"){
    return(data.frame(date_time=dt_ll,type="ll",level=ll))
  }
  if(metric=="h"){
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
    return(data.frame(date_time=dt_h,type="h",level=h))
  }
  if(metric=="l"){
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
    return(data.frame(date_time=dt_l,type="l",level=l))
  }
}

#'Process byday output
#'
#'@keywords internal
tider_process_byday<-function(df,metric){
  out<-by(df,df$ymd,function(x) tider_hl(x$level, x$daytime,metric))
  out<-data.frame(do.call(rbind,out))
  out<-data.frame(apply(out,2,unlist))
  out$ymd <- row.names(out)
  row.names(out)<-1:nrow(out)
  return(out)
}
