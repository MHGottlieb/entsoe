
# Auhtor: https://github.com/MHGottlieb 2022-05-30

# https://transparency.entsoe.eu/content/static_content/Static%20content/web%20api/Guide.html#_basics
# Users are advised to place parameters that identify the data item first,
# followed by any additional criteria and end with date range. This is a
# recommended convention to facilitate troubleshooting. Parameter names are case
# sensitive.
# We're focusing on: 4.4.7. Actual Generation Output per Generation Unit [16.1.A]
# - One day range limit applies
# - Minimum time interval in query response is one MTU period
# - Mandatory parameters
# --- DocumentType ('A73' = Actual Generation)
# --- ProcessType ('A16' = Realised)
# --- In_Domain (can only be queried for Control Area EIC Code)
# ---------- '10Y1001A1001A65H' = Denmark
# ---------- '10YDK-1--------W' = DK1 BZ / MBA
# ---------- '10YDK-2--------M' = DK2 BZ / MBA
# ---------- '10Y1001A1001A796' = Denmark, Energinet CA
# --- TimeInterval or combination of PeriodStart and PeriodEnd
# - Optional parameters
# --- PsrType (When used, only queried production type is returned) (e.g. 'B18' = Wind offshore)
# --- RegisteredResource (EIC of GesecurityToken <- readLines("token.txt") # Explanation
# depends

library(xml2)
library(httr)
library(lubridate)

# basics for API interaction
url <- "https://transparency.entsoe.eu/api?"
securityToken <- readLines("token.txt")

# what do we want to look into?
documentType <- 'A73' # = Actual Generation
processType <- 'A16' # = Realised
psrType <- 'B18' # = Offshore wind (optional)
In_Domain <- '10Y1001A1001A796' # = DK1, Energinet

# define timespan we're requesting from Entso-E's API
from <- as.POSIXct("2015-01-01 00:00", tz="UTC") # Time is always in UTC.
span <- 2705 # days
runtimestart <- Sys.time()

# loop trough days and fething data
for(y in 1:span){
  # create GET request and fetching data
  from_new <- from+days(y-1)
  PeriodStart <- format(from_new, format="%Y%m%d%H%M")
  PeriodEnd <- format(from_new+days(1), format="%Y%m%d%H%M")
  request <- paste0(url,
                    "securityToken=", securityToken,
                    "&documentType=", documentType,
                    "&processType=", processType,
                    "&psrType=", psrType,
                    "&In_Domain=", In_Domain,
                    "&PeriodStart=", PeriodStart,
                    "&PeriodEnd=", PeriodEnd)
  dat <- GET(request)
  # extracting payload from response and stripping name space
  dat <- read_xml(rawToChar(dat$content))
  dat <- xml_ns_strip(dat)
  # Error handling
  len <- length(xml_find_all(dat, ".//TimeSeries"))
  if(len==0){
    cat("ERROR: ", xml_text(xml_find_all(dat, ".//Reason/text")))
    break
  }
  # Finding all nodes (e.g. each node = one wind farm w. 24 hourly data points)
  TimeSeries <- xml_find_all(dat, ".//TimeSeries")
  parker <- gsub(" ", "_", xml_text(xml_find_all(TimeSeries, ".//name"))) #names
  # For each node, extracting production data and compiling to data frame
  for(i in 1:length(TimeSeries)){
    start <- xml_find_first(TimeSeries[i], ".//timeInterval/start")
    start <- as.POSIXct(xml_text(start), format="%Y-%m-%dT%H:%MZ", tz="UTC")
    end <- xml_find_first(TimeSeries[i], ".//timeInterval/end")
    end <- as.POSIXct(xml_text(end), format="%Y-%m-%dT%H:%MZ", tz="UTC")
    by  <- xml_find_first(TimeSeries[i], ".//resolution")
    by <- duration(xml_text(by))
    time <- seq.POSIXt(start, end-by, by)
    prod <- xml_integer(xml_find_all(TimeSeries[i], ".//quantity"))
    if(!exists(parker[i])){
      assign(parker[i], data.frame(time, prod))
    } else {
      assign(parker[i], rbind(get(parker[i]), data.frame(time, prod)))
    }
  }
  # Printing status message
  runtime <- round(difftime(Sys.time(), runtimestart, units="mins"), 0)
  cat("Getting data from", format(from_new, format="%Y-%m-%d"), "- Assets:",
      length(TimeSeries), "hours:", length(time), "- runtime:", runtime, "min \n")
}

# cleaning up
remove(dat, TimeSeries, by, documentType, end, from, from_new, i, y, In_Domain,
       parker, PeriodEnd, PeriodStart, processType, psrType, request, runtime,
       runtimestart, span, start, prod, time, url, securityToken, len)
