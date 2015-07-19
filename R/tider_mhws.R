#' Calculate Mean high water spring for a particular location
#'
#' This function will just use the desired location's water level data
#' to find the greatest tidal range that occur
#' during new and full mmoons. Then averages the heights of the two successive high waters
#' that occur in 24 hours when the tidal range is greatest. Then averages
#' all of the spring high waters to get Mean High water spring.
#'
#' @param df data frame for the desired location
#' @param level thecolumn containg gauge data
#' @param daytime  the column containg daytime values
#' @param phase_lag a numeric that tells phase lag, default is one
#' @return returns a numeric of the mean high water spring
#'df$DateTime <- lubridate::parse_date_time(paste(df$Date,df$Time),
#'                                           "%m/%d/%y %H:%M:%S")
#' @examples
#' i<-system.file("extdata","i.csv",package="tider")
#' i$DateTime <- lubridate::parse_date_time(paste(i$Date,i$Time),
#'                                           "%m/%d/%y %H:%M:%S")
#'
#'  k  <- tide_mhws(i, "Level", "DateTime")
#'

tider_mhws  <- function(df, level, daytime, phase_lag=1){

  library(tider)
  library(chron)

  hltide  <- tider_byday(df, "Level", "DateTime")
  hltide$date_time = strftime(hltide$date_time, format = '%Y/%m/%d %H:%M:%S', tz = "GMT")
  hltide <- hltide[order(hltide$date_time),]
  hltide$level <- as.numeric(as.character(hltide$level))

  x  <- 1
  while (x <NROW(hltide))
  {
    TF  <- (hltide[x , "type"] == hltide[x+1, "type"])
    if (TF == TRUE)
    {
      if (abs(hltide[x, "level"]) >= abs(hltide[x+1, "level"]))
      {
        y = x + 1
        hltide  <- hltide[-y, ]
      }else
      {
        hltide  <- hltide[-x, ]
      }
    }
    x = x + 1
  }

  hltide$tidalrange  <- c(NA, abs(diff(hltide$level)))
  hltide  <- hltide[!is.na(hltide$tidalrange),]
  hltide  <- hltide[!duplicated(hltide), ]

  #gets phase day of first day of data
  phaseday <- function(year, month, day)
  {
    lasttwo = year %% 100
    r = lasttwo %% 19
    if(r > 9)
    {
      r = r - 19
    }
    r = (r * 11) %% 30
    if(month == 1 || month == 2)
    {
      month = month + 2
    }
    r = r + month + day
    if ( year <= 2000)
    {
      r = r -4
    } else
    {
      r = r - 8.3
    }
    r = r %% 30
    r = round(r)
    return(r)
  }
  hltide$julianday <- julian(hltide$month, hltide$day, hltide$year)
  #get start date values
  smth  <- hltide[1, "month"]
  sda  <- hltide[1, "day"]
  syr  <- hltide[1, "year"]
  jd = julian(smth, sda, syr)
  #end date values
  emth= hltide[NROW(hltide), "month"]
  eda = hltide[NROW(hltide), "day"]
  eyr = hltide[NROW(hltide), "year"]
  ejd = julian(emth, eda, eyr)

  sgt <- data.frame(NULL)
  rea = floor((ejd - jd)/14.77)
  for (i in 1:rea)
  {
    mdy = month.day.year(jd, origin = c(month=1, day=1, year=1970))
    phase_day  <- phaseday(mdy$year, mdy$month, mdy$day) #ymd
    if(phase_day <= 15)
    {
      d = 15 - phase_day
      startdate= jd + d
      enddate= startdate + phase_lag
    } else
    {
      d = 30 - phase_day
      startdate = jd + d
      enddate= startdate + phase_lag
    }
    df <- subset(hltide, julianday >= startdate & julianday <= enddate)
    mr  <- (which.max( df$tidalrange))
    rn  <- row.names(df[mr, ])
    lrn  <- (row.names(hltide[NROW(hltide), ]))
    lrnm1  <- as.character(as.numeric(lrn)-1)
    df2  <- data.frame(NULL)
    if (NROW(df)== 0)
    {

    }else
    {
      if ((df[mr, "type"] == "l" ||df[mr, "type"] == "ll" ) & mr != 1 & mr != NROW(df))
      {
        sum = df[mr - 1, "level"] + df[mr + 1, "level"]
        df2  <- cbind(df[mr, ], sum)
        sgt <- rbind(sgt, df2[1, ])
      }else
      {
        if ((df[mr, "type" ]== "h"  || df[mr, "type" ]== "hh") & mr < (NROW(df)-1))
        {
          sum = df[mr , "level"] + df[mr + 2 , "level"]
          df2  <- cbind(df[mr, ], sum)
          sgt <- rbind(sgt, df2[1, ])
        }else
        {
          if (mr == 1)
          {
            dfx  <- subset(hltide, julianday >=(startdate -1) & julianday <= enddate)
            sum = dfx[mr , "level"] + dfx[mr + 2  , "level"]
            df2  <- cbind(df[mr, ], sum)
            sgt <- rbind(sgt, df2[1, ])
          }else
          {
            if ("rn" == "lrn" || "rn" == "lrnm1" )
            {

            }else
            {
              dfx  <- subset(hltide, julianday >=(startdate) & julianday <= enddate +1)
              if(df[mr, "type" ]== "h"  || df[mr, "type" ]== "hh")
              {
                sum = dfx[mr , "level"] + dfx[mr + 2 , "level"]
                df2  <- cbind(df[mr, ], sum)
                sgt <- rbind(sgt, df2[1, ])
              }else
              {
                sum = dfx[mr - 1, "level"] + dfx[mr + 1, "level"]
                df2  <- cbind(df[mr, ], sum)
                sgt <- rbind(sgt, df2[1, ])
              }
            }
          }
        }
      }
    }
    jd = jd + 15
  }
  mhws = sum(sgt$sum)/(NROW(sgt)*2)
  return(mhws)
}
