
iaw$epoch2nyc <- function( epoch ) paste( as.POSIXlt( epoch, origin = "1970-01-01", tz="EST5EDT" ), "NYC" )
