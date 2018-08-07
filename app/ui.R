library(shiny)

source("~/proj/verify-the-forecast/config/config.R")
setwd(base.dir)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Verify the forecast"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("forecasts", "Forecasts to evaluate:", choices=list.providers.labels,
                         inline=FALSE, width=NULL, choiceNames=NULL, selected=list.providers.labels),
      dateRangeInput("daterange1", "Validation period",
                     start="2018-08-01", min="2018-08-01",
                     end=Sys.Date() - 1, max=Sys.Date() - 1),
      selectInput("region", "Region", list.region.menu, selected=NULL, multiple=FALSE,
                  selectize=TRUE, width=NULL, size=NULL),
      selectInput("var", "Variable", list.var, selected = NULL, multiple = FALSE,
                  selectize=TRUE, width=NULL, size=NULL),
      selectInput("score", "Skill score", list.skill.scores.short, selected=NULL, multiple=FALSE,
                  selectize=TRUE, width=NULL, size=NULL),
      sliderInput("leadtime", "Forecast range", 0, 7, 5, step=1, round=TRUE,
                  ticks=TRUE, animate=FALSE)
      #checkboxInput("persistence", "Include persistence", value=FALSE, width=NULL),
      #checkboxInput("climatology", "Include climatology", value=TRUE, width=NULL)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)
