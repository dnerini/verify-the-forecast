
# -- General configuration -----------------------------------------------------

# Paths 

base.dir   = "~/proj/verify-the-forecast"

# -- Libraries -----------------------------------------------------------------

library(jsonlite)
library(RCurl)
library(sqldf)
library(XML)

# -- List of parameters --------------------------------------------------------

# Weather stations 

list.ws = c("OTL","LUG","SMA","GVE","BER","SIO","BAS","TAE","LUZ","COY","STG",
            "CHU","BUS","NEU","SBO","BIA","ROB","SAM","SCU","PAY","PIO")

# Variables

list.var                = c("tmax.degrees", "tmin.degrees", "precip.mm", 
                            "sunshine.min")
list.var.obs            = c("tavg.degrees", "tmin.degrees", "tmax.degrees", 
                            "precip.mm", "sunshine.min")
list.var.long           = c("Daily maximum temperature","Daily minimum temperature",
                            "Daily total precipitation", "Daily total sunshine")
list.units.short        = c("°C", "°C", "mm", "min")

list.skill.scores.short = c("MAE","RMSE","ME","Corr","Scatter",
                            "CSI","GSS","HK","HSS","SEDI")
list.skill.scores.long  = c("Mean absolute error","Root mean squared error","Mean error","Correlation","Scatter",
                            "Critical success index","Gilbert skill score","Hanssen-Kuipers discriminant",
                            "Heidke skill score ","Symmetric extremal dependency index")
list.skill.scores.cont.short = c("MAE","RMSE","ME","Corr","Scatter")
list.skill.scores.cat.short  = c("CSI","GSS","HK","HSS","SEDI")


# Providers

list.providers        = c("mch", "srf", "meteoblue", "meteonews")
list.providers.labels = c("MeteoSwiss","SRF Meteo", "meteoblue", "MeteoNews") 

# Municipalities

list.plz          = c(6600,6900,8001,1201,3001,1950,4001,8400,6010,2501,9000,7000,
                      5000,2000,6850,6710,7742,7503,7550,1530,6776)
list.names.en     = c("Locarno","Lugano","Zurich","Geneva","Bern","Sion","Basel",
                      "Winterthur","Lucerne","Biel_Bienne","St.-Gallen","Chur","Aarau",
                      "Neuchâtel","Mendrisio","Biasca","Poschiavo","Samedan","Scuol",
                      "Payerne","Piotta") # meteonews,meteoblue
list.names.en.2   = c("Locarno","Lugano","Zurich","Geneva","Bern","Sion","Basel",
                      "Winterthur","Lucerne","Biel","St-Gallen","Chur","Aarau",
                      "Neuchatel","Mendrisio","Biasca","Poschiavo","Samedan",
                      "Scuol","Payerne","Piotta") # accuweather
list.names.it     = c("Locarno","Lugano","Zurigo","Ginevra","Berna","Sion","Basel",
                      "Winterthur","Lucerna","Biel","St.+Gallen","Chur","Aarau",
                      "Neuch%E2tel","Mendrisio","Biasca","Poschiavo","Samedan",
                      "Scuol","Payerne","Piotta") # ilmeteo.it
list.locid        = c(2659869,2659836,2657896,2660646,2661552,2658576,2661604,
                      2657970,2659811,2661513,2658822,2661169,2661881,2659496,
                      2659689,2661524,2659165,2658846,2658660,2659243,2659198) # meteonews, meteoblue
list.locid.2      = c(315268,1671,316622,313082,312122,315550,312066,316623,314051,
                      312548,314638,313122,311629,991,315272,315269,313128,"99128_poi",
                      313315,316009,315404) # accuweather
list.municip      = setNames(list.plz,list.names.en.2)
list.region.menu  = list("Regions" = c("All","North","West","South"),
                        "Municipalities" = sort(list.names.en.2))
list.region       = list("south" = c(6600,6900,6850,6710,7742,7503,6776),
                         "west" =  c(1201,1950,2501,2000,1530),
                         "north" = c(3001,4001,5000,6010,7000,8001,8400,9000,7550))

# Databases
db.name.obs      = c("obs.ws.sqlite")
db.name.sun      = c("sun.plz.sqlite")
db.name.clm      = c("clm.ws.sqlite")
db.name.fct      = paste("fct.plz", list.providers, "sqlite", sep=".")
db.name.all      = c(db.name.obs, db.name.sun, db.name.clm, db.name.fct)

# -- URLs ----------------------------------------------------------------------

source(file.path(base.dir, "config", "urls.R"))

# -- Visualization parameters --------------------------------------------------

# Color palette

tableau10 = c('#17becf','#bcbd22','#7f7f7f','#e377c2','#8c564b','#9467bd','#d62728',
              '#2ca02c','#ff7f0e','#1f77b4')
