#' Return a data frame of tide metrics per day
#'
#' @param data frame containg tide data and day time.
#' @param level character of column containing gauge data.
#' @param daytime character of column containing daytime values from lubridate.
#'
#' @export
tider_byday<-function(df,level,daytime){

 df <- data.frame(level = df[[level]],daytime = df[[daytime]],
                  ymd = factor(as.character(strptime(df[[daytime]],"%Y-%m-%d"))))
 out<-by(df,df[,3],function(x) daily_hl(x, "level", "daytime"))
 out<-data.frame(do.call(rbind,out))
 out<-data.frame(apply(out,2,unlist))
 out$ymd <- row.names(out)
 row.names(out)<-1:nrow(out)
 return(out)
}
