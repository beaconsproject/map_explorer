library(sf)
library(leaflet)
library(tidyverse)
library(shinydashboard)
library(shinycssloaders)
library(shinyjs)

options(shiny.maxRequestSize=100*1024^2) 

ui = dashboardPage(skin="blue",
    dashboardHeader(title = "BEACONs Disturbance Explorer", titleWidth=320),
    dashboardSidebar(
        sidebarMenu(id="tabs",
            menuItem("Mapview", tabName = "mapview", icon = icon("th")),
            fileInput(inputId = "upload_poly", label = "Upload geopackage:", multiple = FALSE, accept = ".gpkg")
      )
    ),
  dashboardBody(
    useShinyjs(),
    tags$head(tags$style(".skin-blue .sidebar a { color: #8a8a8a; }")),
    tabItems(
      tabItem(tabName="mapview",
            fluidRow(
                tabBox(id = "one", width="12",
                    tabPanel("Mapview", leafletOutput("map1", height=750) %>% withSpinner())
                )
            )
        )
    )
  )
)

server = function(input, output, session) {

  lyr_names <- eventReactive(input$upload_poly, {
    file <- input$upload_poly$datapath
    ext <- tools::file_ext(file)
    if(ext == "gpkg"){
      st_layers(file)$name
    }
  })

  output$map1 <- renderLeaflet({
    if (!is.null(input$upload_poly)) {
    m <- leaflet() %>%
      addProviderTiles("Esri.WorldImagery", group="Esri.WorldImagery") %>%
      addProviderTiles("Esri.WorldTopoMap", group="Esri.WorldTopoMap") %>%
      addMeasure(position="bottomleft", primaryLengthUnit="meters", primaryAreaUnit="sqmeters", activeColor="#3D535D", completedColor = "#7D4479")
      grps <- NULL
      for(i in lyr_names()) {
        file <- input$upload_poly$datapath
        ext <- tools::file_ext(file)
        x <- st_read(file, i, quiet=T) %>% st_transform(4326)
        if (i=='KDTT') {m <- m %>% addPolygons(data=x, color='black', fill=F, weight=2, group=i)}
        else if (i=='Linear disturbances') {
          pop = ~paste("Industry type:", TYPE_INDUSTRY, "<br>Disturbance type:", TYPE_DISTURBANCE)
          m <- m %>% addPolylines(data=x, color='orange', weight=2, group=i, popup=pop)
        }
        else if (i=='Areal disturbances') {
          pop = ~paste("Industry type:", TYPE_INDUSTRY, "<br>Disturbance type:", TYPE_DISTURBANCE)
          m <- m %>% addPolygons(data=x, fill=T, stroke=F, fillColor='darkorange', fillOpacity=0.5, group=i, popup=pop)
        }
        else if (i=='Burned areas') {
          pop = ~paste("Year of fire:", Year, "<br>Area of fire (ha):", round(Area_ha,1), "<br>Area in KDTT (ha):", round(Area_in_kdtt,1))
          m <- m %>% addPolygons(data=x, fill=T, stroke=F, fillColor='red', fillOpacity=0.5, group=i, label=~Year, popup=pop)
        }
        else if (i=='CPCAD 2021') {
          pop = ~paste("Name:", NAME_E, "<br>Aichi target:", AICHI_T11, "<br>IUCN category:", IUCN_CAT, "<br>OECM:", OECM, "<br>Protected date:", PROTDATE)
          m <- m %>% addPolygons(data=x, fill=T, stroke=F, fillColor='darkgreen', fillOpacity=0.5, group=i, popup=pop)
        }
        else {m <- m %>% addPolygons(data=x, color='red', fill=T, weight=1, group=i)}
        grps <- c(grps,i)
      }
      m <- m %>% 
      addLayersControl(position = "topright",
        baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery"),
        overlayGroups = c("Graticule", grps),
        options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup(c('fda', grps[-1]))
    m
    }
  })

}
shinyApp(ui, server)
