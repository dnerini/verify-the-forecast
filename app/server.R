
library(shiny)
library(reshape2)
library(ggplot2)

source("~/proj/verify-the-forecast/config/config.R")
setwd(base.dir)

source(file.path("app", "utils.R"))

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$distPlot <- renderPlot(width = 120*6, height = 90*6, {
    
# -- Read the data -------------------------------------------------------------
    
    # get plz to evaluate
    aplz = get.plz(input$region)
    nplz = length(aplz)
    
    # is it a categorical score
    if(input$score %in% list.skill.scores.cat.short & !is.null(input$thr)){
      output$thr <- renderUI({
        textInput("thr", "Threshold", value = input$thr, width="27.5%")
      })
      thrin = input$thr
    }else if(input$score %in% list.skill.scores.cat.short & is.null(input$thr)){
      output$thr <- renderUI({
        textInput("thr", "Threshold", value = ">20", width="27.5%")
      })
      thrin = ">20"
    }else{
      thrin = NA
    }
    
    for (k in 1:nplz){
      
      plz = aplz[k]
      ws  = list.ws[list.plz==plz]
      
      # define time interval
      tz = 'GMT'
      if(input$range == "last week"){
        timebounds     = as.POSIXct(paste(c(Sys.Date() - 7, Sys.Date()), '00:00'), format='%Y-%m-%d %H:%M', tz=tz)
      }else if (input$range == "last month"){
        timebounds     = as.POSIXct(paste(c(Sys.Date() - 31, Sys.Date()), '00:00'), format='%Y-%m-%d %H:%M', tz=tz)
      }else if (input$range == "last year"){
        timebounds     = as.POSIXct(paste(c(Sys.Date() - 365, Sys.Date()), '00:00'), format='%Y-%m-%d %H:%M', tz=tz)
      }else if (input$range == "select"){
        timebounds     = as.POSIXct(paste(input$daterange1, '00:00'), format='%Y-%m-%d %H:%M', tz=tz)
      }

      # extract observations
      db.fn  = file.path(base.dir, "data", db.name.obs)
      db.con = dbConnect(SQLite(), dbname=db.fn)
      obs.df = dbReadTable(db.con, ws)
      dbDisconnect(db.con)
      obs.df = data.frame(obs.df["obs.time"], obs.df[[input$var]], fix.empty.names=FALSE)
      colnames(obs.df) = c("obs.time", paste0(input$var,'.obs'))
      
      # filter dates
      timestamps.fct = as.POSIXct(paste(obs.df$obs.time, '00:00'), format='%Y-%m-%d %H:%M', tz=tz)
      idxKeep        = timestamps.fct >= timebounds[1] & timestamps.fct <= timebounds[2]
      obs.df         = obs.df[idxKeep,]
      
      # extract all forecasts
      nfor = length(input$forecasts)
      for (j in 1:nfor){
        
        # extract this forecast
        db.fn  = file.path(base.dir, "data", db.name.fct[which(list.providers.labels==input$forecasts[j])])
        db.con = dbConnect(SQLite(), dbname=db.fn)
        fct.df = dbReadTable(db.con, as.character(plz))
        dbDisconnect(db.con)
        
        if(length(fct.df[[input$var]])>0){
          fct.df          = data.frame(fct.df["forecast.time"], fct.df["leadtime.days"], fct.df[[input$var]], fix.empty.names=FALSE)
          fct.df_         = fct.df[, 3]
          fct.df_[fct.df_ == -2147483648.0] = NA
          fct.df[, 3]     = fct.df_
        }else{
          fct.df          = data.frame(fct.df["forecast.time"], fct.df["leadtime.days"], array(NA, nrow(fct.df)), fix.empty.names=FALSE)
        }
        colnames(fct.df)  = c("forecast.time", "leadtime.days", input$var)
        
        # keep only relevant lead times
        fct.df = fct.df[fct.df$leadtime.days <= input$leadtime,]

        # co-locate obsevations to forecasts
        all.df_ = merge(obs.df, fct.df, by.y = "forecast.time", by.x = "obs.time")
        
        if(nrow(all.df_)>0){
          all.df_$provider = input$forecasts[j]
          if(!exists("all.df")){
            all.df = all.df_
          }else{
            all.df = rbind(all.df, all.df_, fill=TRUE)
          }
        }
        rm(all.df_)
      } # for j nfor
    } # for k nplz
    
# -- Do the verification -------------------------------------------------------
    
    the.var    = which(list.var == input$var)
    the.score  = list.skill.scores.short == input$score
    all.df = all.df[is.finite(all.df[[input$var]]),]
    
    for (j in 1:nfor){
      
      this.fct   = all.df$provider == input$forecasts[j]
      its.number = which(list.providers.labels == input$forecasts[j])
      its.colour = tableau10[its.number]
      
      if (sum(this.fct)>0){
        
        all.df_ = all.df[this.fct,]
        lt  = sort(unique(all.df$leadtime.days))
        nlt = length(lt)
        y   = array(NA, nlt)
        
        r = parse.thr(thrin)
        thr = r$thr
        ineq = r$ineq
        
        for(t in 1:nlt){
          this.lt = all.df_$leadtime.days == lt[t]
          obs     = as.numeric(all.df_[[input$var]])[this.lt]
          fct     = as.numeric(all.df_[[paste0(input$var,".obs")]])[this.lt]
          if(length(obs)>0){
            y[t]    = error.function(obs, fct, list.skill.scores.short[the.score], thr, ineq)
          }
        } # for t nlt
        
        err.df_ = data.frame(row.names=lt, y) 
        colnames(err.df_) = c(input$forecasts[j])
        
        if(!exists("err.df")){
          err.df= err.df_
          color.arr = its.colour
        }else{
          err.df = cbind(err.df, err.df_)
          color.arr = c(color.arr, its.colour)}
      } # if any forecasted value
      
    } # for j nfor
    
    # TODO: add climatology
    
    # TODO: add persistence
    
# -- Plot results --------------------------------------------------------------
    
    # define axis bounds and intervals
    xmin = 0
    xmax = min(input$leadtime, max(all.df$leadtime.days))

    # now plot all curves
    err.df = melt(as.matrix(err.df), value.name="error")
    err.df = err.df[is.finite(err.df$error),]
    colnames(err.df) = c("leadtime.days", "provider", "error")
    
    title = paste0("Forecast ", tolower(list.skill.scores.long[the.score]), " (", 
                   list.skill.scores.short[the.score],") in\n",
                   tolower(list.var.long[the.var]), " [", list.units.short[the.var], "]\n",
                   strftime(timebounds[1], format="%Y-%m-%d"), " - ", strftime(timebounds[2], format="%Y-%m-%d"))

    # ggplot2
    
    g = ggplot(err.df, aes(x=leadtime.days, y=error, color=provider))
    g = g + ggtitle(title)
    g = g + geom_line(size=1.2) + geom_point()
    g = g + scale_colour_manual(values = color.arr)
    if(input$score %in% list.skill.scores.cont.short){
      ytitle = list.skill.scores.short[the.score]
     }else{
      ytitle = paste0(list.skill.scores.short[the.score], " (", ineq, thr, list.units.short[the.var], ")")
     }
    g = g + labs(x='Forecast range in days', y=ytitle, color=NULL)
    g = g + scale_x_continuous(breaks=seq(xmin, xmax, 1), limits=c(xmin, xmax))
    
    # theme
    # https://ggplot2.tidyverse.org/reference/theme.html
    g = g + theme(plot.title=element_text(size=22))
    g = g + theme(axis.text=element_text(size=14),
                  axis.title=element_text(size=16))
    g = g + theme(legend.text=element_text(size=14),
                  legend.position = "bottom")
    #g = g + theme(legend.text=element_text(size=14),
    #              legend.position=c(.20, .95),
    #              legend.justification=c("right", "top"),
    #              legend.box.just="right",
    #              legend.margin=margin(6, 6, 6, 6))
    print(g)
  })
  
})
