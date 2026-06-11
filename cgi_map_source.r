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