# C:\Users\henri\data_fran_G

# Importing the libraries
library(dplyr)
library(ggplot2)
library(tidyverse)
library(sf)
library(mapview)

# Importing the dataset

fil <- "C:/Users/henri/data_fran_G/"

smaort <- "So2015_Swe99TM.shp"
smaort <- st_read(paste0(fil, smaort), crs = 3006)
smaort_layer <- smaort |> 
  filter(LANSNAMN == "Dalarna")
mapview(smaort_layer)

tatort <- "Tatorter_1980_2020.gpkg"
st_layers(paste0(fil, tatort))
tatort_path <- paste0(fil, tatort) # Detta bör vara den fullständiga sökvägen till din .gpkg-fil
tatort_layer <- st_read(tatort_path, layer = "To2020_SR99TM", crs = 3006)
tatort_layer <- tatort_layer |> 
  dplyr::filter(LAN == "20")

# library(ggplot2)
# 
# ggplot(tatort_layer) +
#   geom_sf() +
#   theme_minimal() +
#   ggtitle("Tätorter i Dalarna 2020")

fritid_omr <- "Fo2015_Swe99TM.shp"
fritid_layer <- st_read(paste0(fil, fritid_omr), crs = 3006)
fritid_layer <- fritid_layer |> 
  dplyr::filter(LANSNAMN == "Dalarna")
mapview(fritid_layer)

anlag_omr <- "anlaggningsomrade_ln20.gpkg"
anlag_omr_path <- paste0(fil, anlag_omr)
st_layers(anlag_omr_path)
anlag_layer <- st_read(anlag_omr_path, layer = "anlaggningsomrade", crs = 3006)
mapview(anlag_layer)

# a mapview of all the data so far, diffrent colors for diffrent layers
mapview(smaort_layer, col.regions = "red") + 
  mapview(tatort_layer, col.regions = "blue") + 
  mapview(fritid_layer, col.regions = "green") + 
  mapview(anlag_layer, col.regions = "yellow")

library(sf)
library(dplyr)
library(tidyr)

buffer = 200

# Buffra varje lager
tatort_buffer <- st_buffer(tatort_layer, dist = buffer)
smaort_buffer <- st_buffer(smaort_layer, dist = buffer)
fritid_buffer <- st_buffer(fritid_layer, dist = buffer)
anlag_buffer <- st_buffer(anlag_layer, dist = buffer)

# mapview(tatort_buffer, col.regions = "blue") + 
#   mapview(smaort_buffer, col.regions = "red") + 
#   mapview(fritid_buffer, col.regions = "green") + 
#   mapview(anlag_buffer, col.regions = "yellow")

# Lägg till prioritet
tatort_buffer$priority <- 1
smaort_buffer$priority <- 2
fritid_buffer$priority <- 3
anlag_buffer$priority <- 4

# fixar kolumner så att alla lager ser lika ut i uppsättningen  

funk_tatort <- tatort_buffer |> 
  dplyr::select(TATORTSKOD, TATORT, KOMMUNNAMN, KOMMUN, BEF, geom, priority) |> 
  rename(namn = TATORT, 
         kod = TATORTSKOD, 
         kommun = KOMMUN, 
         kommunnamn = KOMMUNNAMN, 
         befolkning = BEF) |> 
  mutate(omr = "Tätort")

funk_smaort <- smaort_buffer |> 
  dplyr::select(SMAORT, KOMMUNNAMN, KOMMUNKOD, BEF, geometry, priority) |> 
  rename(kod = SMAORT, 
         kommun = KOMMUNKOD, 
         kommunnamn = KOMMUNNAMN, 
         befolkning = BEF,
         geom = geometry) |> 
  mutate(namn = NA, omr = "Småort")

funk_fritid <- fritid_buffer |> 
  dplyr::select(FRITIDSHUS, KOMMUNNAMN, KOMMUNKOD, geometry, priority) |> 
  rename(kod = FRITIDSHUS, 
         kommun = KOMMUNKOD, 
         kommunnamn = KOMMUNNAMN,
         geom = geometry) |> 
  mutate(befolkning = NA, namn = NA, omr = "Fritidshusområde")

funk_anlag <- anlag_buffer |> 
  dplyr::select(objekttyp, andamal, geom, priority) |> 
  mutate(namn = paste(objekttyp, andamal, sep = ", ")) |>
  select(namn, geom, priority) |>
  mutate(befolkning = NA, kommun = NA, kommunnamn = NA, kod = NA, omr = "Anläggningsområde")

standard_order <- c("omr", "kod", "namn", "kommunnamn", "kommun", "befolkning", "priority", "geom")

funk_anlag <- funk_anlag[standard_order]
funk_smaort <- funk_smaort[standard_order]
funk_tatort <- funk_tatort[standard_order]
funk_fritid <- funk_fritid[standard_order]

mapview(funk_anlag, col.regions = "blue") + 
  mapview(funk_smaort, col.regions = "red") + 
  mapview(funk_tatort, col.regions = "green") + 
  mapview(funk_fritid, col.regions = "yellow")

funk_ort <- rbind(funk_tatort, funk_smaort, funk_fritid, funk_anlag) |> 
  mutate(funk_ort= "funk_ort")


mapview(funk_ort, col.regions = "blue")

funk_ort_diss <- funk_ort %>%
  st_make_valid() %>%
  group_by(funk_ort) %>%
  summarize(geom = st_union(geom))

library(sf)

# Antag att funk_ort_diss är ditt multipolygon-objekt
# Dela upp multipolygonen till enskilda polygoner
single_polygons <- st_cast(funk_ort_diss, "POLYGON")
mapview(single_polygons)

# Skapa centroider för funk_ort
funk_ort_centroids <- st_centroid(funk_ort)
mapview(funk_ort_centroids, zcol = "priority")+
  mapview(single_polygons)

# # Antag att `centroids` är ditt centroid-dataset med attributen inklusive 'namn', 'kod', 'prioritet', och 'befolkning'
# centroids <- funk_ort_centroids %>%
#   arrange(befolkning, priority) %>%
#   mutate(rank = row_number())
# view(centroids)
# glimpse(centroids)

# funk_ort_diss_with_attrs <- st_join(single_polygons, centroids, join = st_nearest_feature)
# glimpse(funk_ort_diss_with_attrs)
# mapview(funk_ort_diss_with_attrs, zcol = "priority")
# 
# 
# funk_ort_diss_final <- funk_ort_diss_with_attrs %>%
#   group_by(geom) %>%
#   filter(rank == min(rank)) %>%
#   mutate(namn = ifelse(is.na(namn) | namn == "", kod, namn)) %>%
#   ungroup()
# glimpse(funk_ort_diss_final)
# 
# funk_ort_diss_final <- funk_ort_diss_final %>%
#   distinct(geom, .keep_all = TRUE)
# glimpse(funk_ort_diss_final)
# 
# 
# mapview(funk_ort_diss_final, zcol = "rank")

library(sf)
library(dplyr)

# Step 1: Spatial Join
joined_data <- st_join(single_polygons, funk_ort_centroids)

# Step 2 & 3: Sorting and Filtering
# Assuming 'priority' and 'population' are the column names in 'centroids'
processed_data <- joined_data %>%
  arrange(priority, desc(befolkning)) %>%
  group_by(geom) %>%
  slice(1) %>%
  ungroup()

# Step 4: Cleanup
# Removing duplicates and keeping necessary columns
final_data <- processed_data %>%
  distinct(geom, .keep_all = TRUE)

mapview(final_data)+
  mapview(smaort_layer, col.regions = "red") + 
  mapview(tatort_layer, col.regions = "blue") + 
  mapview(fritid_layer, col.regions = "green") + 
  mapview(anlag_layer, col.regions = "yellow")

funktionella_orter <- final_data

















