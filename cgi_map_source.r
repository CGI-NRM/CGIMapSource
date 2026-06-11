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

GenerateMapQPCR <- function(mapData) {
  colorByResultat <- colorFactor(palette = c("darkgreen", "red", "orange"), domain = c("positiv", "negativ", "okänd"), ordered = TRUE)

  counts_html <- paste0(
    '<div style="background:white; padding:10px;">',
    'Antal positiva prover: <span style="float:right; font-weight:bold"> ', sum(mapData$Resultat == "positiv", na.rm = TRUE), "</span><br>",
    'Antal negativa prover: <span style="float:right; font-weight:bold"> ', sum(mapData$Resultat == "negativ", na.rm = TRUE), "</span>"
  )
  
  leaflet::leaflet(mapData, elementId = "myMap") %>%
    leaflet::addTiles() %>%
    leaflet::addCircleMarkers(lng = ~ Longitude, lat = ~ Latitude, popup = ~ paste0("<b>", Prov, "</b>: ", Resultat, " ", Replikat), color = ~ colorByResultat(Resultat)) %>%
    leaflet::addLegend(values = ~ Resultat, pal = colorByResultat, title = "Resultat") %>%
    leaflet::addControl(html = counts_html, position = "bottomright")%>%
    htmlwidgets::onRender("
      function(el, x) {
        window.myNativeMap = this; 
      }
    ") # expose the map globally
}

GenerateTable <- function(mapData, columnNames = c("Resultat", "Replikat")) {
  DT::datatable(
    data = mapData, 
    rownames = FALSE, 
    options = list(
      pageLength = 100, 
      dom = "t",
      autoWidth = TRUE,
      # columnDefs = list(list(visible = FALSE, targets = c(1, 2))),
      columnDefs = list(list(visible = FALSE, targets = match(c("Latitude", "Longitude"), colnames(mapData)))),
      rowCallback = JS("
      function(row, data, index) {
        // data[0]=Prov, data[1]=Latitude, data[2]=Longitude, data[3]=Resultat, data[4]=Replikat
        $(row).attr('data-lat', data[1]);
        $(row).attr('data-lng', data[2]);
        $(row).attr('data-title', data[0]);
        $(row).attr('data-replicates', data[4]);
        $(row).css('cursor', 'pointer'); 
      }
    ") # store the data in the html
    ),
    callback = JS("
    // Listen for direct clicks on the table body rows
    table.on('click', 'tbody tr', function() {
      var row = $(this);
      var lat   = parseFloat(row.attr('data-lat'));
      var lng   = parseFloat(row.attr('data-lng'));
      var title = row.attr('data-title');
      var date  = row.attr('data-replicates');

      // Check if our global map instance has loaded and coordinates are valid numbers
      if (window.myNativeMap && !isNaN(lat) && !isNaN(lng)) {
        
        window.myNativeMap.closePopup();

        // Target the native Leaflet map engine directly to execute the panning camera 
        window.myNativeMap.flyTo([lat, lng], 11);

        // Render an interactive info popup window bubble
        L.popup()
          .setLatLng([lat, lng])
          .setContent('<strong>' + title + '</strong><br>' + date)
          .openOn(window.myNativeMap);
      }
    });
  ")
  )
}