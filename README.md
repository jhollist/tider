tider
=====

`tider` is an R package for parsing common water level data logger formats to return a variety of commonly used tidal measurements. 

##Installation
`tider` is not currently on CRAN, but can be installed easily with the `devtools` package.


```r
#install devtools if not already done
install.packages("devtools")
library("devtools")

#install tider
install_github("jhollist/tider")
```

##Basic Use
To return higher high, high, low, and lower low from a vector of water levels from a single day:


```r
#Load up example data
file<-system.file("extdata","TideFile2days.csv",package="tider")
tides<-read.csv(file,stringsAsFactor=F)
#Create a Date/Time vector
tides$DateTime <- lubridate::parse_date_time(paste(tides$Date,tides$Time),
                                  "%m/%d/%y %H:%M:%S")
#Get all values
tide_meas<-tider_byday(tides,"Depth_NAVD88m","DateTime")
tide_meas
```

```
##   higher_high      dt_higher_high   high             dt_high   low
## 1      0.5512 2014-12-05 23:06:00 0.5448 2014-12-05 11:12:00 0.357
## 2      0.6753 2014-12-06 12:12:00 0.6734 2014-12-06 23:42:00 0.482
##                dt_low lower_low        dt_lower_low        ymd
## 1 2014-12-05 17:54:00     0.295 2014-12-05 05:18:00 2014-12-05
## 2 2014-12-06 18:18:00    0.3745 2014-12-06 05:18:00 2014-12-06
```

##EPA Disclaimer
The United States Environmental Protection Agency (EPA) GitHub project code is provided on an "as is" basis and the user assumes responsibility for its use. EPA has relinquished control of the information and no longer has responsibility to protect the integrity , confidentiality, or availability of the information. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recomendation or favoring by EPA. The EPA seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity by EPA or the United States Government.

By submitting a pull request, you make an agreement with EPA that you will not submit a claim of compensation for services rendered to EPA or any other federal agency. Further, you agree not to charge the time you spend developing software code related to this project to any federal grant or cooperative agreement.
