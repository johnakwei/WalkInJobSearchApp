library(shiny)
library(shinydashboard)
library(leaflet)

# WalkInJobSearch App
SNCAddress <- readRDS("SNCAddress.rds")

ui <- dashboardPage(
  dashboardHeader(title="WalkInJobSearch App"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Job Search", tabName="dashboard", icon=icon("th")),
      menuItem("Overall Map", tabName="dashboard2", icon=icon("th")),
      menuItem("Documentation", tabName="documentation",
               icon=icon("th"))
      )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName="dashboard",
              fluidRow(
                box(
                  selectInput("State", label=h5("Select State"),
                              choices=as.character(SNCAddress$State),
                              selected=as.character(1)),
                  uiOutput("CountySelector"),
                  uiOutput("clinicSelector"),
                  width=6),
                box(
                  h4(textOutput('text1')),
                  leafletOutput("mymap"), width="100%")
                )
              ),
      tabItem(tabName="dashboard2",
              fluidRow(box(leafletOutput("oregonmap"), width="100%"))
              ),
      tabItem(tabName="documentation",
              fluidRow(box(
                strong("Introduction"),
                br(),
                
                p("The objective of the WalkInJobSearch App is to enable Job Seekers to find opportunities to apply for employment via Walk-In application, and to enable Businesses to find employees via in person application by Job Seekers."),
                br(),
                
                strong(em("Job Search")),
                p("Generates a zoomable Road Map of the location set."),
                br(),
                
                strong(em("Overall Map")),
                p("Generates a zoomable Road Map of all Safety Net Clinics."),
                br(),
                
                strong(em("Zoom")), 
                p("The zoom controls are on the top left corners of the Maps."),
                br(),
                
                strong(em("Markers")), 
                p("Clicking within the Markers displays the location name.")
              )))
          )
      )
    )

server <- function(input, output) {
  # County Selector
  output$CountySelector <- renderUI({
    CountyList <- subset(SNCAddress$County, SNCAddress$State==input$State)
    selectInput("County", label=h5("Select County"),
                choices=as.character(CountyList))})
  
  # Clinic Selector
  output$clinicSelector <- renderUI({
    clinicList <- subset(SNCAddress$Clinic,
                         SNCAddress$County==input$County)
    selectInput("Clinic", label=h5("Select Clinic"),
                choices=as.character(clinicList))})
  
  # Print Clinic Name, and Address above Map:
  output$text1 <- renderPrint({
    clinicAddr <- subset(SNCAddress, SNCAddress$Clinic==input$Clinic)
    clinicAddr <- clinicAddr[1:5]
    rownames(clinicAddr) <- " "
    colnames(clinicAddr) <- c(" ", " ", " ", " ", " ")
    clinicAddr})
  
  # Create Map
  output$mymap <- renderLeaflet({
      clinicAddr <- subset(SNCAddress, SNCAddress$Clinic==input$Clinic)
      leaflet(clinicAddr) %>% addTiles() %>% addMarkers(
        lng=clinicAddr$lon, lat=clinicAddr$lat, popup=clinicAddr$Clinic)
  })
  
  # Create Overall Map
  output$oregonmap <- renderLeaflet({
    SNCAddress <- readRDS("SNCAddress.rds")
    leaflet(SNCAddress) %>% addTiles() %>%
      addMarkers(lng=SNCAddress$lon, lat=SNCAddress$lat,
                 popup=SNCAddress$Clinic)})
}

shinyApp(ui, server)