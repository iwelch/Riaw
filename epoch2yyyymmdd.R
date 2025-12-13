
iaw$epoch2yyyymmdd <- function(epochin) 
    as.integer(strftime( as.POSIXct(epochin, origin="1970-01-01", tz="EST5EDT"), format="%Y%m%d" ))

iaw$epoch2hhmmss <- function(epochin)
    as.integer(strftime( as.POSIXct(epochin, origin="1970-01-01", tz="EST5EDT"), format="%H%M%S" ))
