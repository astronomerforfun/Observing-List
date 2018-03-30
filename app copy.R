library(shiny)
library(shinydashboard)
library(ggvis)
library(shinyjs)
library(shinythemes)
library(dplyr)
library(DT)


source("script.R", local = TRUE)
ui <- navbarPage(
  theme = shinytheme("cerulean"),"Night Sky",
  tabPanel("MESSIER", value = "messiertab",
           
           sidebarLayout(
             sidebarPanel(
               selectInput("light", "Light Zone", Zone_Table$Zone, NULL),
               checkboxGroupInput("lists", "Report", choices = c(Messier = "Mssr",'Herschel 400' = "Herschel 400", 'IC Objects'= "IC Objects")),
               conditionalPanel(
                 condition = "input.lists == c('Mssr', 'Herschel 400')",
                  selectInput("ssn", "Season", choices = c(Winter = "wnt", Spring = "spg", Summer = "smmr", Fall = "fall")),
                 actionButton('selectallB','Select All'),
               conditionalPanel(
                 condition = "input.ssn == 'wnt'",
                 checkboxGroupInput("winterconst", "Winter Constellations", choices = Winter)),
               conditionalPanel(
                 condition = "input.ssn == 'spg'",
                checkboxGroupInput("springconst", "Spring Constellations", choices = Spring)),
                conditionalPanel(
                  condition = "input.ssn == 'smmr'",
                  checkboxGroupInput("summerconst", "Summer Constellations", choices = Summer)),
               conditionalPanel(
                 condition = "input.ssn == 'fall'",
                 checkboxGroupInput("fallconst", "Fall Constellations", choices = Fall)
                 
              )
              
                )),
             
             mainPanel(
               fluidRow(csvDownloadUI("dwnld", "DOWNLOAD"), style = "padding:10px"),
               dataTableOutput("messier"),
               dataTableOutput("herschel")
        )
           )))
               









# Define server logic required to draw a histogram
server <- function(input, output, session){


messierinspring <- reactive({
  if(input$ssn == 'spg' & input$lists == 'Mssr'){
  y <- MessierObjects$Constellation %in% input$springconst
  z <-MessierObjects[y,]
  return(z)
  }else if(input$ssn == 'wnt'){
  t <- MessierObjects$Constellation %in% input$winterconst
  q <- MessierObjects[t,]
  return(q)
  }else if(input$ssn == 'fall'){
    l <- MessierObjects$Constellation %in% input$fallconst
    m <- MessierObjects[l,]
    return(m)
  }else if(input$ssn == 'smmr'){
    o <- MessierObjects$Constellation %in% input$summerconst
    p <- MessierObjects[o,]
    return(p)
  } else {
    return(NULL)
  }
  })

herschel <- reactive({
  if(input$lists == "Herschel" & input$ssn == 'spg'){
      iii <- Herschel$Constellation %in% input$springconst
      kkk <-Herschel[iii,]
      return(kkk)
    }else if(input$ssn == 'wnt'){
      gg <- Herschel$Constellation %in% input$winterconst
      hhh <- Herschel[gg,]
      return(hhh)
    }else if(input$ssn == 'fall'){
      llll <- Herschel$Constellation %in% input$fallconst
      m <- Herschel[llll,]
      return(m)
    }else if(input$ssn == 'smmr'){
      oooo <- Herschel$Constellation %in% input$summerconst
      p <- Herschel[oooo,]
      return(p)
    } else {
      return(NULL)
    }
  })

lightpollutionherschel <- reactive({
  yyy <- Zone_Table[Zone_Table$Zone == input$light,]
  zzz <- herschel()[herschel()$Magnitude <= yyy$MaxMag,]
})


lightpollution <- reactive({
  h <- Zone_Table[Zone_Table$Zone == input$light,]
  j <- messierinspring()[messierinspring()$Magnitude <= h$MaxMag,]
  return(j)
})

output$herschel <- renderDataTable({
  datatable(lightpollutionherschel())
})

output$messier <- renderDataTable({
  datatable(lightpollution())
})


dwnld <- reactive({
  tmp2 <- lightpollution()
})

observe({
  callModule(csvDownload,"dwnld", dwnld)
  # print(marginfordwnld())
})


}
# Run the application 
shinyApp(ui = ui, server = server)
