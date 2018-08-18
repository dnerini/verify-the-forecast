
update.obs.ws      <- function(ws, dbprefix="obs.ws", path=NULL){
  #' Parse hourly observations from a MeteoSwiss station, aggregate to daily and
  #' append them to a SQLite database.
  #'  
  #' Arguments
  #' ---------
  #' ws : string
  #'  the 3-letter abbreviation identifying the station of interest
  #'  example: 'OTL'
  #' dbprefix : str
  #'  the name of the (existing) SQLite database. 
  #' path : str
  #'  path to the (existing) SQLite database. Default to working directory.

  # get daily observations
  df = extract.daily.obs(ws)
  
  # connect to database
  if(is.null(path)){path = getwd()}
  dbname = paste(dbprefix, "sqlite", sep=".")
  db.obs = dbConnect(SQLite(), dbname=file.path(path, dbname))
  
  # write data
  dbWriteTable(conn=db.obs, name=ws, value=df, append=TRUE)
  
  # close connection
  dbDisconnect(db.obs)
  
  cat(sprintf("New obs data for %s wrote to %s\n",ws,file.path(path, dbname)))
}

update.fct.plz     <- function(plz, provider, dbprefix="fct.plz", path=NULL){
  #' Parse daily forecast for a plz and from a given provider and append them to 
  #' a SQLite database.
  #'  
  #' Arguments
  #' ---------
  #' plz : int
  #'  the 4 digits PLZ identifier of a Swiss municipality (e.g. 6900)
  #' provider: str
  #'  the name of the forecast provider (e.g. "mch")
  #' dbprefix : str
  #'  the prefix to the db name of the (existing) SQLite database. 
  #' path : str
  #'  path to the (existing) SQLite database. Default to working directory.
  
  # get daily observations
  df = extract.daily.fct(plz, provider)
  
  # connect to database
  if(is.null(path)){path = getwd()}
  dbname = paste(dbprefix, provider, "sqlite", sep=".")
  db.fct = dbConnect(SQLite(), dbname=file.path(path, dbname))
  
  # write data
  dbWriteTable(conn=db.fct, name=as.character(plz), value=df, append=TRUE)
  
  # close connection
  dbDisconnect(db.fct)
  
  cat(sprintf("New fct data for %s wrote to %s\n",as.character(plz),file.path(path, dbname)))
}

update.sun.plz     <- function(plz, dbprefix="sun.plz", path=NULL){
  
  # get sunrise and sunset
  df = extract.daily.sun(plz)
  
  # connect to database
  if(is.null(path)){path = getwd()}
  dbname = paste(dbprefix, "sqlite", sep=".")
  db.sun = dbConnect(SQLite(), dbname=file.path(path, dbname))
  
  # write data
  dbWriteTable(conn=db.sun, name=as.character(plz), value=df, append=TRUE)
  
  # close connection
  dbDisconnect(db.sun)
  
  cat(sprintf("New sun data for %s wrote to %s\n",as.character(plz), file.path(path, dbname)))
  
}

extract.daily.obs  <- function(ws, lag=1, na.rm=TRUE) {
  #' Parse hourly observations from a MeteoSwiss station and aggregate to daily.
  #'  
  #' Arguments
  #' ---------
  #' ws : string
  #'  the 3-letter abbreviation identifying the station of interest
  #'  example: 'OTL'
  #' lag : int
  #'  the day lag w.r.t. the current day. 0: today, 1: yesterday and so on. 
  #' na.rm : bool  
  #'  wheter or not to ignore missing values when aggreating hourly to daily.
  #' Returns
  #' -------
  #' obs.data : data frame
  #'  a data frame containing the daily surface parameters for station ws.
    
  # get all available hourly observations for station ws
  obs.data.hourly = extract.hourly.obs(ws)
    
  # compute day limits for the given lag day
  date.lims = c(as.POSIXct(paste(format(as.POSIXct(Sys.time()) - lag*24*3600,       "%Y-%m-%d"), " 00:00:00")),
                as.POSIXct(paste(format(as.POSIXct(Sys.time()) - (lag - 1)*24*3600, "%Y-%m-%d"), " 00:00:00")))
  
  # compute indices
  timestamps = obs.data.hourly$obs.timestamps
  idx = timestamps > date.lims[1] & timestamps <= date.lims[2] 
  if(sum(idx) != 24){
    warning(sprintf("%s observations for %s are incomplete (%i hours only)", ws, format(as.POSIXct(Sys.time()) - lag*24*3600, "%Y-%m-%d"), sum(idx)))
  }
  
  # aggregate to daily
  tavg.daily       = round(mean(obs.data.hourly$tavg.degrees[idx],   na.rm=na.rm), 1)
  tmin.daily       =        min(obs.data.hourly$tmin.degrees[idx],   na.rm=na.rm)
  tmax.daily       =        max(obs.data.hourly$tmax.degrees[idx],   na.rm=na.rm)
  precip.daily     =        sum(obs.data.hourly$precip.mm[idx],      na.rm=na.rm)
  sunshine.daily   =        sum(obs.data.hourly$sunshine.min[idx],   na.rm=na.rm)
  wind.speed.daily = round(mean(obs.data.hourly$wind.speed.kmh[idx], na.rm=na.rm), 1)
  wind.gust.daily  =        max(obs.data.hourly$wind.gust.kmh[idx],  na.rm=na.rm)
  
  # organize relevant data as dataframe
  obs.data.daily = data.frame(
                   nominal.time   = as.character(Sys.time()),
                   obs.time       = format(as.POSIXct(Sys.time()) - lag*24*3600, "%Y-%m-%d"),
                   tavg.degrees   = tavg.daily,
                   tmin.degrees   = tmin.daily,
                   tmax.degrees   = tmax.daily,
                   precip.mm      = precip.daily,
                   sunshine.min   = sunshine.daily,
                   wind.speed.kmh = wind.speed.daily,
                   wind.gust.kmh  = wind.gust.daily)
  
  # insert NaNs
  obs.data.daily = do.call(data.frame, lapply(obs.data.daily, 
                              function(x) replace(x, !is.finite(x), NA)))
  
  return(obs.data.daily)
}

extract.daily.fct  <- function(plz, provider){
  #' Parse daily forecasts for a given municipality from a given provider.
  #'  
  #' Arguments
  #' ---------
  #' plz : int
  #'  the 4 digits PLZ identifier of a Swiss municipality (e.g. 6900)
  #' provider: str
  #'  the name of the forecast provider (e.g. "mch")
  #' Returns
  #' -------
  #' fct.data : data frame
  #'  a data frame containing the daily forecasts for surface parameters for municipality
  #'  plz.

  retrieve.time = Sys.time()
  
  if(provider == "mch"){
    
    fct.plz.url = paste(mch.url, plz, "00", sep="") 
    fct.data    = fromJSON(fct.plz.url)
    
    fct.data = data.frame(nominal.time    = as.character(retrieve.time),
                          forecast.time   = fct.data$regionForecast$dayDate,
                          leadtime.days   = c(0:(length(fct.data$regionForecast$dayDate) - 1)),
                          tmin.degrees    = fct.data$regionForecast$temperatureMin, 
                          tmax.degrees    = fct.data$regionForecast$temperatureMax,
                          precip.mm       = fct.data$regionForecast$precipitation,
                          icon.id         = fct.data$regionForecast$iconDay)
    
  }
  
  else if(provider == "srf"){
    
    fct.plz.url = paste(srf.url, plz, sep="")
    fct.data    = fromJSON(fct.plz.url)
    
    # organize relevant data as dataframe
    fct.data = data.frame(nominal.time       = as.character(retrieve.time),
                          forecast.time      = fct.data$days$date,
                          leadtime.days      = c(0:(length(fct.data$days$date) - 1)),
                          tmin.degrees       = fct.data$days$TN_C, 
                          tmax.degrees       = fct.data$days$TX_C,
                          precip.mm          = fct.data$days$RRR_MM,
                          precip.prob        = fct.data$days$PROBPCP_PERCENT,
                          sunshine.min       = as.integer(fct.data$days$SUN_H*60),
                          wind.kmh           = fct.data$days$FF_KMH,
                          windgusts.kmh      = fct.data$days$FX_KMH,
                          winddir.deg        = fct.data$days$DD_DEG)
    
    # change datetime format
    fct.datetime = as.POSIXlt(fct.data$forecast.time, tz="", "%d.%m.%Y")
    fct.data$forecast.time = strftime(fct.datetime, "%Y-%m-%d")  
    
  }
  
  else if(provider == "meteoblue"){
    
    idx_plz = list.plz == plz
    name = tolower(list.names.en[idx_plz])
    name = gsub('_','%2F',name)
    locid = list.locid[idx_plz]
    
    fct.plz.url = paste(meteoblue.url, name, "_switzerland_", locid, sep="") 
    fct.html    = getURL(fct.plz.url)
    fct.html.parsed = htmlTreeParse(fct.html, error=function(...){}, useInternalNodes = TRUE)
    
    # Tmin
    tmin.degrees = xpathSApply(fct.html.parsed, "//*/div[@class='tab_temp_min']", xmlValue)
    tmin.degrees = unlist(strsplit(tmin.degrees, "\n"))
    tmin.degrees = gsub('[^0-9-]','',tmin.degrees)[seq(2, length(tmin.degrees), 2)]
    tmin.degrees = as.numeric(tmin.degrees)
    tmin.degrees[tmin.degrees==""] = NA
    
    # Tmax
    tmax.degrees = xpathSApply(fct.html.parsed, "//*/div[@class='tab_temp_max']", xmlValue)
    tmax.degrees = unlist(strsplit(tmax.degrees, "\n"))
    tmax.degrees = gsub('[^0-9-]','',tmax.degrees)[seq(2, length(tmax.degrees), 2)]
    tmax.degrees = as.numeric(tmax.degrees)
    tmax.degrees[tmax.degrees==""] = NA
    
    # Precip
    precip.mm = xpathSApply(fct.html.parsed, '//*[@class="tab_precip"] | //*[@class="tab_precip dry"] | //*[@class="tab_precip highlight"]', xmlValue)
    precip.mm = gsub(' |cm||m|\n','',precip.mm) # cm of snow =  mm equiv
    for(k in 1:length(precip.mm)){
      this.precip = precip.mm[k]
      if(this.precip == '' | this.precip == '-'){
        precip.mm[k] = 0.0
      }else if(nchar(this.precip) > 1 & grepl("-", this.precip)){ # sample random value within the given interval
        minmax = as.numeric(unlist(strsplit(this.precip, "-")))
        precip.mm[k] = round(runif(1, minmax[1], minmax[2]),1)
      }else if(grepl(">", this.precip)){
        gt = as.numeric(gsub(">", "", this.precip))
        precip.mm[k] = gt + 0.1
      }else{
        precip.mm[k] = NA
      } # if
    } # for k
    precip.mm = as.numeric(precip.mm)
    precip.mm[precip.mm==""] = NA
    
    # Sun
    sunshine.min = xpathSApply(fct.html.parsed, "//*/div[@class='tab_sun'] | //*/div[@class='tab_sun tab_no_sun'] | //*/div[@class='tab_sun tab_no_data']", xmlValue)
    sunshine.min = unlist(strsplit(sunshine.min, "\n"))
    sunshine.min = gsub('\\D+','',sunshine.min)[seq(2, length(sunshine.min), 2)]
    sunshine.min = as.numeric(sunshine.min)*60
    sunshine.min[sunshine.min==""] = NA
    
    forecast.datetime = retrieve.time + c(0:(length(tmin.degrees) - 1))*24*60*60
    forecast.time = strftime(forecast.datetime,"%Y-%m-%d")  
    
    # organize relevant data as dataframe
    fct.data = data.frame(nominal.time     = as.character(retrieve.time),
                          forecast.time    = forecast.time,
                          leadtime.days    = c(0:(length(forecast.time) - 1)),
                          tmin.degrees     = tmin.degrees, 
                          tmax.degrees     = tmax.degrees,
                          precip.mm        = precip.mm,
                          sunshine.min     = as.integer(sunshine.min))
    
    
  }
  
  else if(provider == "meteonews"){
    
    idx_plz = list.plz == plz
    name = tolower(list.names.en[idx_plz])
    locid = list.locid[idx_plz]
    
    fct.plz.url = paste(meteonews.url, locid, "/", name, sep="") 
    fct.html    = getURL(fct.plz.url)
    fct.html.parsed = htmlTreeParse(fct.html, error=function(...){}, useInternalNodes = TRUE)
    
    # Tmax
    tmax.degrees = xpathSApply(fct.html.parsed, "//*/span[@class='temp_hoch']", xmlValue)
    tmax.degrees = unlist(strsplit(tmax.degrees, "\n"))
    tmax.degrees = gsub("[^0-9-]", "", tmax.degrees)
    tmax.degrees = as.numeric(tmax.degrees[!(tmax.degrees %in% c("", "|"))])[1:6]
    
    # Tmin
    tmin.degrees = xpathSApply(fct.html.parsed, "//*/td", xmlValue)
    tmin.degrees = unlist(strsplit(tmin.degrees, "\n"))
    tmin.degrees = gsub("[^0-9-]", "", tmin.degrees)
    tmin.degrees = as.numeric(tmin.degrees[!(tmin.degrees %in% c("", "|"))])
    idxMax       = which(rollapply(tmin.degrees, length(tmax.degrees), identical, tmax.degrees))
    tmin.degrees = tmin.degrees[(idxMax + length(tmax.degrees)):(idxMax + 2*length(tmax.degrees) - 1)]
    
    # Sun
    sunshine.min = xpathSApply(fct.html.parsed, "//*/td", xmlValue)
    sunshine.min = unlist(strsplit(sunshine.min, "\n"))
    sunshine.min = gsub("[^0-9\\.\\-]", "", sunshine.min)
    sunshine.min = as.numeric(sunshine.min[!(sunshine.min %in% c("", "|"))])
    idxMax       = which(rollapply(sunshine.min, length(tmax.degrees), identical, tmax.degrees))
    sunshine.min = sunshine.min[(idxMax + 2*length(tmax.degrees)):(idxMax + 3*length(tmax.degrees) - 1)]*60
    
    # Precip
    precip.mm = xpathSApply(fct.html.parsed, "//*/td", xmlValue)
    precip.mm = unlist(strsplit(precip.mm, "\n"))
    precip.mm = gsub("[^0-9\\.\\-]", "", precip.mm)
    precip.mm = precip.mm[!(precip.mm %in% c("", "|"))]
    idxMax = which(rollapply(as.numeric(precip.mm), length(tmax.degrees), identical, tmax.degrees))
    precip.mm = precip.mm[(idxMax + 4*length(tmax.degrees)):(idxMax + 5*length(tmax.degrees) - 1)]
    for(k in 1:length(precip.mm)){
      this.precip = precip.mm[k]
      if(this.precip == "" | this.precip == "0" | this.precip == "-"){
        precip.mm[k] = 0.0
      }else if(nchar(this.precip) > 1 & grepl("-", this.precip)){ # sample random value within the given interval
        minmax = as.numeric(unlist(strsplit(this.precip, "-")))
        precip.mm[k] = round(runif(1, minmax[1], minmax[2]),1)
      }else if(grepl(">", this.precip)){
        gt = as.numeric(gsub(">", "", this.precip))
        precip.mm[k] = gt + 0.1
      }else{
        precip.mm[k] = NA
      } # if
    } # for k
    precip.mm = as.numeric(precip.mm)
    precip.mm[precip.mm==""] = NA
    
    # Prob of precip
    precip.prob = xpathSApply(fct.html.parsed, "//*/td", xmlValue)
    precip.prob = unlist(strsplit(precip.prob, "\n"))
    precip.prob = gsub("[^0-9\\.\\-]", "", precip.prob)
    precip.prob = as.numeric(precip.prob[!(precip.prob %in% c("", "|"))])
    idxMax      = which(rollapply(precip.prob, length(tmax.degrees), identical, tmax.degrees))
    precip.prob = precip.prob[(idxMax + 5*length(tmax.degrees)):(idxMax + 6*length(tmax.degrees) - 1)]
    
    forecast.datetime = retrieve.time + c(0:(length(tmin.degrees) - 1))*24*60*60
    forecast.time     = strftime(forecast.datetime,"%Y-%m-%d")  
    
    # organize relevant data as dataframe
    fct.data = data.frame(nominal.time   = as.character(retrieve.time),
                         forecast.time   = forecast.time,
                         leadtime.days   = c(0:(length(forecast.time) - 1)),
                         tmin.degrees    = tmin.degrees, 
                         tmax.degrees    = tmax.degrees,
                         precip.mm       = precip.mm,
                         precip.prob     = precip.prob,
                         sunshine.min    = sunshine.min)

  }
  
  else{
    stop(sprintf("Unknown provider '%s'", provider))
  }
  
  return(fct.data)
  
}

extract.daily.sun  <- function(plz){
  
  retrieve.time = Sys.time()
  
  sun.plz.url = paste(srf.url, plz, sep="")
  sun.data    = fromJSON(sun.plz.url)
  
  sun.data = data.frame(doy             = as.POSIXlt(sun.data$days$date[1], format = "%d.%m.%Y")$yday,
                        sunrise         = sun.data$days$SUNRISE[1],
                        sunset          = sun.data$days$SUNSET[1],
                        daylength.min  = as.integer(as.numeric(difftime(as.POSIXlt(sun.data$days$SUNSET[1],  format="%H:%M"), 
                                                              as.POSIXlt(sun.data$days$SUNRISE[1], format="%H:%M"), units="mins"))))
  
  return(sun.data)
  
}

extract.hourly.obs <- function(ws, not.a.value=32767) {
  #' Parse hourly observations from a MCH station.
  #'  
  #' Arguments
  #' ---------
  #' ws : string
  #'  the 3-letter abbreviation identifying the station of interest
  #'  example: 'OTL'
  #' not.a.value : int
  #'  the value that identifies missing obsevations
  #'   
  #' Returns
  #' -------
  #' obs.data : data frame
  #'  a data frame containing the time-series of hourly surface parameters for
  #'  the last 3 days at station ws.
  
  obs.ws.url = paste(obs.url, ws, "_verlauf.json", sep="")
  obs.data   = fromJSON(obs.ws.url)
  
  timeStart  = as.POSIXct(obs.data$temperatureAvg$start/1000, origin="1970-01-01")
  timestamps = timeStart + 
    c(0:(length(obs.data$temperatureAvg$data) - 1))*obs.data$temperatureAvg$timeStep/1000
  obs.data = data.frame(obs.timestamps   = timestamps,
                        tavg.degrees     = obs.data$temperatureAvg$data,
                        tmin.degrees     = obs.data$temperatureMin$data,
                        tmax.degrees     = obs.data$temperatureMax$data,
                        precip.mm        = obs.data$precipitation$data,
                        sunshine.min     = obs.data$sun$data,
                        wind.speed.kmh   = obs.data$windSpeed$data,
                        wind.gust.kmh    = obs.data$windGust$data)
  
  obs.data[obs.data==not.a.value] = NA
  
  return(obs.data)
}