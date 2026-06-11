library(flexdashboard)
library(readxl)
library(leaflet)
library(leaflet)
library(DT)
library(htmltools)

LoadCoordinates <- function(coordinateXLSX) {
  mapData <- readxl::read_xlsx(coordinateXLSX)
  mapData$Latitude <- as.numeric(mapData$Latitude)
  mapData$Longitude <- as.numeric(mapData$Longitude)
  return(mapData)
}

GenerateMap <- function(mapData) {
  colorByResultat <- colorFactor(palette = c("darkgreen", "red", "orange"), domain = c("positiv", "negativ", "okänd"), ordered = TRUE)

  counts_html <- paste0(
    '<div style="background:white; padding:10px;">',
    'Antal positiva prover: <span style="float:right; font-weight:bold"> ', sum(mapData$Resultat == "positiv", na.rm = TRUE), "</span><br>",
    'Antal negativa prover: <span style="float:right; font-weight:bold"> ', sum(mapData$Resultat == "negativ", na.rm = TRUE), "</span>"
  )
  
  leaflet::leaflet(mapData, elementId = "myMap") %>%
    leaflet::addTiles() %>%
    # leaflet::addMarkers(lng = ~ Longitude, lat = ~ Lattitude, label = ~ Prov) %>%
    leaflet::addCircleMarkers(lng = ~ Longitude, lat = ~ Latitude, popup = ~ paste0("<b>", Prov, "</b>: ", Resultat, " ", Replikat), color = ~ colorByResultat(Resultat)) %>%
    leaflet::addLegend(values = ~ Resultat, pal = colorByResultat, title = "Resultat") %>%
    leaflet::addControl(html = counts_html, position = "bottomright")%>%
    htmlwidgets::onRender("
      function(el, x) {
        window.myNativeMap = this; 
      }
    ") # expose the map globally
}