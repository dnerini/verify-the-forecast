#removes all saved variables
rm(list=ls(all=TRUE)) 
#close all figures
graphics.off()

source("~/proj/verify-the-forecast/config/config.R")
setwd(base.dir)

source(file.path("source", "update.archives.R"))


for (i in 1:length(list.plz)){
  
  plz = list.plz[i]
  
  # Update forecasts
  for (j in 1:length(list.providers)){
    update.fct.plz(plz, list.providers[j], path=file.path(base.dir, "data"))
  }
}
