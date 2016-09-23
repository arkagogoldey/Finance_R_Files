#INformation from https://github.com/sboysel/fredr
# install.packages("devtools")
devtools::install_github("sboysel/fredr")
#Main Commands
# Website to search Fred variables
# https://research.stlouisfed.org/

library(fredr)
library(dplyr)
library(ggfortify)

#My API Key for Lathropmike0@GMAIL.com
fredr_key("58009e7b98595c8405fcb15ec6851d64")
FRED API key successfully set. Run 'Sys.getenv('FRED_API_KEY')' to see key

fredr_search(search_text = "housing")

fredr_series(series_id = "HOUST",
             observation_start = "1990-01-01",
             observation_end = "1995-01-01")
HOUS <- data.frame(housing)
autoplot.zoo(HOUS)
write.csv(HOUS, "HOUS.CSV")

fredr_series(series_id = "BAMLH0A0HYM2", observation_start = "2008-01-01", frequency = "m", units = "lin")
autoplot.zoo()

## Converting Tables from Quarterly and Monthly into a Single Series
#The data for the series does not contain the dates in the table, it will need to be added in Col 1








######################################################################################################
######################################################################################################
#               Notes
######################################################################################################
######################################################################################################

library(fredr)
#My API Key for Lathropmike0@GMAIL.com
fredr_key("58009e7b98595c8405fcb15ec6851d64")
FRED API key successfully set. Run 'Sys.getenv('FRED_API_KEY')' to see key

#Searching for a FRED Series
fredr_search(search_text = "housing")
#> Source: local data frame [1,000 x 15]
#> 
#>             id realtime_start realtime_end
#>          (chr)          (chr)        (chr)
#> 1        HOUST     2016-02-05   2016-02-05
#> 2     HOUSTNSA     2016-02-05   2016-02-05
#> 3      USSTHPI     2016-02-05   2016-02-05
#> 4  HPIPONM226S     2016-02-05   2016-02-05
#> 5  HPIPONM226N     2016-02-05   2016-02-05
#> 6  PONHPIM226S     2016-02-05   2016-02-05
#> 7  PONHPIM226N     2016-02-05   2016-02-05
#> 8      CASTHPI     2016-02-05   2016-02-05
#> 9      FLSTHPI     2016-02-05   2016-02-05
#> 10     NYSTHPI     2016-02-05   2016-02-05
#> ..         ...            ...          ...
#> Variables not shown: title (chr), observation_start (chr), observation_end
#>   (chr), frequency (chr), frequency_short (chr), units (chr), units_short
#>   (chr), seasonal_adjustment (chr), seasonal_adjustment_short (chr),
#>   last_updated (chr), popularity (int), notes (chr).


#Get a FRED Series. Returns  ts Object

fredr_series(series_id = "HOUST",
             observation_start = "1990-01-01",
             observation_end = "1995-01-01")
#>       Jan  Feb  Mar  Apr  May  Jun  Jul  Aug  Sep  Oct  Nov  Dec
#> 1990 1551 1437 1289 1248 1212 1177 1171 1115 1110 1014 1145  969
#> 1991  798  965  921 1001  996 1036 1063 1049 1015 1079 1103 1079
#> 1992 1176 1250 1297 1099 1214 1145 1139 1226 1186 1244 1214 1227
#> 1993 1210 1210 1083 1258 1260 1280 1254 1300 1343 1392 1376 1533
#> 1994 1272 1337 1564 1465 1526 1409 1439 1450 1474 1450 1511 1455
#> 1995 1407

fredr_series(series_id = "UNRATE",
             observation_start = "1990-01-01",
             frequency = "q",
             units = "chg")
#>      Qtr1 Qtr2 Qtr3 Qtr4
#> 1990 -0.1  0.0  0.4  0.4
#> 1991  0.5  0.2  0.1  0.2
#> 1992  0.3  0.2  0.0 -0.2
#> 1993 -0.3  0.0 -0.3 -0.2
#> 1994  0.0 -0.4 -0.2 -0.4
#> 1995 -0.1  0.2  0.0 -0.1
#> 1996 -0.1  0.0 -0.2  0.0
#> 1997 -0.1 -0.2 -0.1 -0.2
#> 1998 -0.1 -0.2  0.1 -0.1
#> 1999 -0.1  0.0 -0.1 -0.1
#> 2000 -0.1 -0.1  0.1 -0.1
#> 2001  0.3  0.2  0.4  0.7
#> 2002  0.2  0.1 -0.1  0.2
#> 2003  0.0  0.2  0.0 -0.3
#> 2004 -0.1 -0.1 -0.2  0.0
#> 2005 -0.1 -0.2 -0.1  0.0
#> 2006 -0.3 -0.1  0.0 -0.2
#> 2007  0.1  0.0  0.2  0.1
#> 2008  0.2  0.3  0.7  0.9
#> 2009  1.4  1.0  0.3  0.3
#> 2010 -0.1 -0.2 -0.1  0.0
#> 2011 -0.5  0.1 -0.1 -0.4
#> 2012 -0.3 -0.1 -0.2 -0.2
#> 2013 -0.1 -0.2 -0.2 -0.4
#> 2014 -0.2 -0.5 -0.1 -0.4
#> 2015 -0.1 -0.2 -0.2 -0.2
#> 2016   NA

Combine with other packages for a slick workflow

library(dplyr)
library(ggfortify)
fredr_series(series_id = "GNPCA",
             units = "log") %>%
  autoplot.zoo()

fredr_series(series_id = "NMFCI", observation_start = "2008-01-01", frequency = "m", units = "lin")
fredr_series(series_id = "BAMLH0A0HYM2", observation_start = "2008-01-01", frequency = "m", units = "lin")