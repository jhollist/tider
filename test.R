#Load up example data
file<-system.file("extdata","EDC_WaterLevel_Test.csv",package="tider")
two_day<-system.file("extdata","TideFile2days.csv",package="tider")
tides<-read.csv(file,stringsAsFactor=F)
two_day_tides<-read.csv(two_day,stringsAsFactor=F)
#Create a Date/Time vector
tides$DateTime <- lubridate::parse_date_time(paste(tides$Date,tides$Time),
                                             "%m/%d/%y %H:%M:%S")
#Create a Date/Time vector
two_day_tides$DateTime <- lubridate::parse_date_time(paste(two_day_tides$Date,two_day_tides$Time),
                                             "%m/%d/%y %H:%M:%S")
#Get all values
tide_meas<-daily_hl(tides,"Depth_NAVD88m","DateTime")
tide_meas
tider_byday(two_day_tides,"Depth_NAVD88m","DateTime")
two_day_tides_meas

#plot data
plot(tides$DateTime,tides$Depth_NAVD88m)
plot(two_day_tides$DateTime,two_day_tides$Depth_NAVD88m)
