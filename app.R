
install.packages("leaflet")
library(shiny)
library(shinydashboard)
library(leaflet)
library(rgdal)


#### Load Data ####
# From http://www.cbc.ca/toronto/features/crimemap/
Crime<-read.csv("CrimeScores.csv")
# From http://www1.toronto.ca/wps/portal/contentonly?vgnextoid=75d6e03bb8d1e310VgnVCM10000071d60f89RCRD
Demos<- read.csv("DemoPercents.csv")

Neighbourhoods <-merge(Demos, Crime, by=c("AREA_CODE"))

# From http://www1.toronto.ca/wps/portal/contentonly?vgnextoid=75d6e03bb8d1e310VgnVCM10000071d60f89RCRD
shapeData <- readOGR("NEIGHBOURHOODS_UTM6.shp")
shapeData <- spTransform(shapeData, CRS("+proj=longlat +datum=WGS84 +no_defs"))
shapeData$AREA_CODE <- as.integer(shapeData$AREA_CODE)
shapeData <- shapeData[order(shapeData$AREA_CODE),] 
Neighbourhoods <- merge(Neighbourhoods,shapeData, by=c("AREA_CODE"))



#### Create Switch to load right data to map ####


DataType <- function(x) {
  switch(x,
         "High Crime Score" = Neighbourhoods$CrimeScore,
         "Murder" = Neighbourhoods$Murder,
         "Assault" = Neighbourhoods$Assault,
         "Break and Enter" = Neighbourhoods$Break.and.Enter,
         "Sexual Assault" = Neighbourhoods$Sex.Assault,
         "Car Theft"= Neighbourhoods$Cars,
         "Theft"= Neighbourhoods$Theft,
         "High House Prices" = Neighbourhoods$Home.Prices,
         "Debt Risk" = Neighbourhoods$Debt.Risk.Score,
         "Linguistic Diversity" = Neighbourhoods$Linguistic.Diversity,
         "Speak Chinese" = Neighbourhoods$Language...Chinese,
         "Speak Italian"= Neighbourhoods$Language...Italian,
         "Speak Korean" = Neighbourhoods$Language...Korean,
         "Speak Persian Farsi" = Neighbourhoods$Language...Persian..Farsi.,
         "Speak Portuguese" = Neighbourhoods$Language...Portuguese,
         "Speak Russian" = Neighbourhoods$Language...Russian,
         "Speak Spanish" =Neighbourhoods$Language...Spanish,
         "Speak Tagalog" = Neighbourhoods$Language...Tagalog,
         "Speak Tamil" = Neighbourhoods$Language...Tamil,
         "Speak Urdu" = Neighbourhoods$Language...Urdu,
         "0-4 year olds" =Neighbourhoods$Pop.0...4.years,
         "5-9 year olds" =Neighbourhoods$Pop.5...9.years,
         "10-14 year olds" =Neighbourhoods$Pop.10...14.years,
         "15-19 year olds" =Neighbourhoods$Pop.15..19.years,
         "20-24 year olds" = Neighbourhoods$Pop.20...24.years,
         "25-29 year olds" = Neighbourhoods$Pop..25...29.years,
         "30-34 year olds" = Neighbourhoods$Pop.30...34.years,
         "35-39 year olds" = Neighbourhoods$Pop.35...39.years,
         "40-44 year olds" = Neighbourhoods$Pop.40...44.years,
         "45-49 year olds" = Neighbourhoods$Pop.45...49.years,
         "50-54 year olds"=Neighbourhoods$Pop.50...54.years,
         "55-59 year olds"=Neighbourhoods$Pop.55...59.years,
         "60-64 year olds"=Neighbourhoods$Pop.60...64.years,
         "65-69 year olds"=Neighbourhoods$Pop.65...69.years,
         "70-74 year olds"=Neighbourhoods$Pop.70...74.years,
         "75-79 year olds"=Neighbourhoods$Pop.75...79.years,
         "80-84 year olds"=Neighbourhoods$Pop.80...84.years,
         "85+ year olds"=Neighbourhoods$Pop.85.years.and.over,
         "Population Density" = Neighbourhoods$Population.Density,
         "Population" = Neighbourhoods$Total.Population
                  )
}

#### Running Application ####

ui <- dashboardPage(
  dashboardHeader(title = "Mapping Toronto"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Choose Metrics to Map", icon = icon("bar-chart-o"),
      radioButtons("pType", "",
                 list("High Crime Score", "Murder", "Assault", "Break and Enter", "Sexual Assault", "Car Theft", "Theft","High House Prices", "Debt Risk",
                 "Linguistic Diversity", "Speak Chinese","Speak Italian","Speak Korean","Speak Persian Farsi",
                      "Speak Portuguese", "Speak Russian", "Speak Spanish", "Speak Tagalog","Speak Tamil","Speak Urdu", "Population Density", "Population",
                     "0-4 year olds","5-9 year olds",  "10-14 year olds",  "15-19 year olds",  "20-24 year olds",  "25-29 year olds", "30-34 year olds",  "35-39 year olds",
                                  "40-44 year olds",  "45-49 year olds",  "50-54 year olds",  "55-59 year olds",  "60-64 year olds",    "65-69 year olds",  "70-74 year olds",  "75-79 year olds",
                                  "80-84 year olds", "85+ year olds"))
  ))),
  dashboardBody(
    tabBox(
      width = 12, title = tagList(shiny::icon("map"), "Created By Kenneth Preston"),
    tabPanel("Neighbourhood Viewer",
    tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
    leafletOutput("map"))
    )
  )
)



server <- function(input, output) {
  
  

  output$map <- renderLeaflet({
        leaflet() %>%
        addTiles() %>%
        setView(lng = -79.35325, lat = 43.701130, zoom = 12) %>%
        addPolygons(data=shapeData,weight=4,col = "Blue", popup=paste(Neighbourhoods$Name, sep=" <br>") , 
                    fillOpacity = DataType(input$pType)/max(DataType(input$pType)))%>%
      addLegend("topright", 
                colors =c("#1414ff", "#3b3bff", "#8989ff", "#c4c4ff", "#ffffff"),
                labels= c("Stronger", "","","", "Weaker"),
                title= "Metric in Neighbourhood",
                opacity = 1)
        })
  

 
}
shinyApp(ui, server)
