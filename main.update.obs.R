#removes all saved variables
rm(list=ls(all=TRUE)) 
#close all figures
graphics.off()

source("~/proj/verify-the-forecast/config/config.R")
setwd(base.dir)

source(file.path("source", "update.archives.R"))


for (i in 1:length(list.plz)){
  
  plz = list.plz[i]
  
  # Get reference weather station
  ## TODO: use general function to find nearest station to PLZ
  ws = list.ws[i]
    
  # Update observations
  update.obs.ws(ws, path=file.path(base.dir, "data"))
  update.sun.plz(plz, path=file.path(base.dir, "data"))
}
