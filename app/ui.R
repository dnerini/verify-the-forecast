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
      
      selectInput("range", "Time range", list.time.ranges, selected=NULL, multiple=FALSE,
                  selectize=TRUE, size=NULL, width="36%"),
      conditionalPanel(
        condition = "input.range == 'select'",
        dateRangeInput("daterange1", "Select time range",
                     start="2018-08-01", min="2018-08-01",
                     end=Sys.Date() - 1, max=Sys.Date() - 1)),
      
      div(style="display: inline-block;vertical-align:top; width: 200px;",
        selectInput("region", "Region", list.region.menu, selected=NULL, multiple=FALSE,
                  selectize=TRUE, size=NULL, width="100%")),
      
      div(style="display: inline-block;vertical-align:top; width: 20px;",HTML("<br>")),
      div(style="display: inline-block;vertical-align:top; width: 200px;",
        selectInput("var", "Variable", list.var, selected = NULL, multiple=FALSE,
                  selectize=TRUE, size=NULL, width="100%")),
      
      sliderInput("leadtime", "Forecast range", 0, 7, 5, step=1, round=TRUE,
                  ticks=TRUE, animate=FALSE),
      
      #checkboxInput("persistence", "Include persistence", value=FALSE, width=NULL),
      #checkboxInput("climatology", "Include climatology", value=TRUE, width=NULL)
      
      #selectInput("score", "Skill score", list.skill.scores.short2, width="27%"),

      selectInput("type", "Score type", c('Continuous', 'Categorical'), width="150px"),
        conditionalPanel(
          condition = "input.type == 'Continuous'",
          div(style="display: inline-block;vertical-align:top; width: 150px;",
            selectInput("contscore", "Skill score", list.skill.scores.cont.short, width="100%"))),
      
      conditionalPanel(
        condition = "input.type == 'Categorical'",
        div(style="display: inline-block;vertical-align:top; width: 150px;",
          selectInput("catscore", "Skill score", list.skill.scores.cat.short, width="100%")),
        div(style="display: inline-block;vertical-align:top; width: 150px;",
          textInput("thr", "Threshold", value = ">20", width="100%"))
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)
