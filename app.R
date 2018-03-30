library(shiny)
library(shinydashboard)
library(ggvis)
library(shinyjs)
library(shinythemes)
library(dplyr)
library(DT)
library(test)

createLink <- function(val) {
  sprintf('<a href="https://www.google.com/#q=%s" target="_blank" class="btn btn-primary">Info</a>',val)
}

# wiki <- function(number){
#   x <- paste0('<a href="https://en.wikipidia.org/wiki/Messier_',number, sep = "")
#   return(x)
# }

source("script.R", local = TRUE)
ui <- navbarPage(
  theme = shinytheme("cerulean"),"Night Sky",
  tabPanel("OBSERVING LIST", value = "messiertab",
           
           sidebarLayout(
             sidebarPanel(
               checkboxGroupInput("lists", "Report", choices = c("Messier" = "Messier Objects", 'Herschel 400' = "Herschel 400")),
               h5("Assumptions: Magnifications of Diffuse Objects were reduced by 1."),
               selectInput("season", "Season", c("Winter", "Spring", "Summer", "Fall")),
               h5("Seasons based on Northern Hemisphere"),
               selectInput("light", "Light Zone", Zone_Table$Zone, NULL),
               h5("Choose Light Zone (Red = Mag 7 and below , Yellow = Mag 9 and below, White = Mag 8 and below, Green = Mag 10 and below")
               
               
             ),
             
             mainPanel(
               # fluidRow(csvDownloadUI("dwnld", "DOWNLOAD"), style = "padding:10px"),
               dataTableOutput("messier")
        )
           )))
               









# Define server logic required to draw a histogram
server <- function(input, output, session){
 
  
df <- reactive({
  if(is.null(input$lists)) return(NULL)
  x <- input$lists
  yy <- finaldf%>%
    filter(List == x[1] | List == x[2])
  return(yy)
})

season <- reactive({
  if(is.null(df())) return(NULL)
  curr_ssn <- switch(input$season,
                     Summer = Summer,
                     Fall = Fall,
                     Spring = Spring,
                     Winter = Winter)
  
  xxg <- df()$Constellation %in% curr_ssn
  yyyy <- df()[xxg,]
  h <- Zone_Table[Zone_Table$Zone == input$light,]
  j <- yyyy[yyyy$Magnitude <= h$MaxMag,]
  j$Link <- createLink(j$Name)
  return(j)
  
})


output$messier <- renderDataTable({
  # season()$Link <- createLink(season()$Name)

  DT::datatable(season(),
                rownames = FALSE, 
                extensions="Buttons",options = list(dom = 'Bfrtip',
                                                    buttons = c('copy', 'csv', 'excel', 'pdf', 'print')),  escape = FALSE)

               
               
})
  

# dwnld <- reactive({
#   tmp2 <- season()
# })
# 
# observe({
#   callModule(csvDownload,"dwnld", dwnld)
#   # print(marginfordwnld())
# })
# 
# 
# }
# Run the application 
}
shinyApp(ui = ui, server = server)
