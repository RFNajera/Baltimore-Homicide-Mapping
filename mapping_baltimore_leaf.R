# Mapping Baltimore Homicides and Shootings with Leaflet
# Rene F. Najera, DrPH

# Set working directory
setwd("~/Dropbox/RProjects/Baltimore-Homicide-Mapping") #Set the working directory

# A program to create maps of Baltimore that are interactive (e.g. zoom, choose layers) and show homicides, shootings, and homicide rates by neighborhood.
# Using Leaflet and hints/tips from here: https://andrewbtran.github.io/NICAR/2017/maps/leaflet-r.html

############### Create a map of Baltimore 2018 homicides and shootings ############### 

# Read the data
violence_2018 <- read.csv("data/2018_homicides_shootings_jan_july.csv", stringsAsFactors=FALSE)

# Different colors for homicides and shootings.
color_factor <- colorFactor(c("red", "blue"), domain=c("HOMICIDE", "SHOOTING")) # Note the alphabetical order of the values.

# Create the map
baltimore_map1 <- leaflet() %>%
  addTiles() %>% # Note that it adds Open Street Map tiles. These are customizable.
  setView(-76.6425073,39.2928739, zoom = 12) %>% #Centers on Baltimore
  addCircles(violence_2018$Longitude, # x coordinate
             violence_2018$Latitude, # y coordinate
             popup=violence_2018$Description, # What pops up when clicking on a circle? This can be from the data or a line of text.
             weight = 3, 
             radius=60, 
             color=color_factor(violence_2018$Description), # Colored according to homicide or shooting, based on the "color_factor" created above.
             stroke = TRUE, 
             fillOpacity = 0.8) %>%
  addLegend("bottomright", colors= c("blue", "red"), labels=c("Non-Fatal Shooting", "Homicide"), title="Violence") # Where to put the legend and what to put in it.

# Look at the map. Simple, right?

baltimore_map1

# Save the map
library(htmlwidgets)
saveWidget(widget = baltimore_map1, file = "points_shootings_homicides.html")

############ Create a choropleth map of Baltimore 2018 homicides and shootings ###########

# Using dplyr to summarize the data count by neighborhood
library(dplyr)
by_nbhood <- violence_2018 %>%
  group_by(Neighborhood) %>%
  summarize(total=n()) # Total number of events

# Bring in the neighborhood shapefile
library(rgdal)
nbhoods <- readOGR("data/baltimore_neighborhoods","nhood_2010") # Read the shapefile in
nbhoods <- spTransform(nbhoods, CRS("+init=epsg:4326")) # Transform the coordinate system
proj4string(nbhoods) # Look at the coordinate system

# Add population and other demographics to "by_nbhood" to add it to the layer below
nbhood_demo <- read.csv("data/neighborhood_demo_2010.csv", stringsAsFactors = FALSE)
by_nbhood <- merge(by_nbhood, nbhood_demo, by.x = "Neighborhood", by.y = "Name", type = "left")

# Calculate the event rate per 100,000 residents.
# If the population is <200 (in industrial areas), return the average population of a neighborhood (Look into this)
by_nbhood$Population <- ifelse(by_nbhood$Population < 200, mean(by_nbhood$Population), by_nbhood$Population)

# Now calculate the event rate per 100,000 residents
by_nbhood$rate <- ifelse(by_nbhood$Population == 0, 0, (by_nbhood$total / (by_nbhood$Population / 100000)))

# Now we use the Tigris function geo_join to bring together 
# the neighborhoods shapefile and the by_nbhood dataframe -- LABEL & Neighborhood 
# are the two columns they'll be joined by
library(tigris)
neighborhood_merged <- geo_join(nbhoods, by_nbhood, "LABEL", "Neighborhood", how = "left")

# Creating a color palette based on the number range in the total column
color_palette <- colorNumeric("Reds", domain=neighborhood_merged$rate, na.color = "#88bc89")

# Setting up the pop up text
popup_text <- paste0("Event Rate per 100k Residents: ", as.character(neighborhood_merged$rate))

# Mapping it
baltimore_map <- leaflet() %>%
  addProviderTiles("Wikimedia") %>% # Full list of tiles you can use: http://leaflet-extras.github.io/leaflet-providers/preview/ 
  setView(-76.6425073,39.2928739, zoom = 12) %>% 
  addPolygons(data = neighborhood_merged , 
              fillColor = ~color_palette(neighborhood_merged$rate), 
              fillOpacity = 0.85, 
              weight = 0.2, 
              smoothFactor = 0.2, 
              popup = ~popup_text,
              label = neighborhood_merged$NBRDESC,
              group = "Event Rate") %>%
  addCircles(violence_2018$Longitude, violence_2018$Latitude, 
             label = violence_2018$Description, 
             weight = 3, 
             radius=60, 
             color=cof(violence_2018$Description), 
             stroke = TRUE, 
             fillOpacity = 0.8, 
             group = "Locations") %>%
  addLegend(pal = color_palette, 
            values = neighborhood_merged$rate, 
            position = "bottomright", 
            title = "Violent Events
            <br> (Non-Fatal Shootings
            <br> and Homicides)
            <br> per 100,000 Residents",
            opacity = 0.5,
            na.label = "N/A") %>%
  addLegend("bottomright", colors= c("blue", "red"), labels=c("Non-Fatal Shooting", "Homicide"), title="Location of Violent<br> Events") %>%
  hideGroup("Locations") %>%
  addLayersControl(overlayGroups = c("Event Rate","Locations"),
                 options = layersControlOptions(collapsed = FALSE))

# Look at the map.
baltimore_map

# Save the map
saveWidget(widget = baltimore_map, file = "choropleth_shootings_homicides.html")
