
# Clean console and environment
cat('\f')
rm(list = ls())

# load packages
packages = c('tidyverse','raster','rgdal','rgeos','ggsn')
sapply(packages,require,character.only=T)

# Load shapefile de bogota
bogota = readRDS('data/originales/shape bogota.rds') 
bogota@data$prop_NBI_per <-as.numeric(bogota@data$prop_NBI_per)

# Load data stations
stations = data.frame(id = 1:15 , 
                      longitud = runif(n = 15, min = bogota@bbox[1,1] , max = bogota@bbox[1,2]) , 
                      latitud = runif(n = 15, min = bogota@bbox[2,1] , max = bogota@bbox[2,2]))
#DATOS_EXCEL <- DATOS_EXCEL[DATOS_EXCEL$variable=="PM10",]
coordinates(stations)=~longitud+latitud
proj4string(stations)
stations@proj4string <-CRS("+proj=longlat +datum=WGS84 +no_defs")

# Reproject shapes
bogota = spTransform(x = bogota , CRSobj = CRS('+proj=utm +zone=19 +datum=WGS84 +units=m +no_defs'))
stations <- spTransform(x = stations , CRSobj = CRS('+proj=utm +zone=19 +datum=WGS84 +units=m +no_defs'))

# Generate buffer
buffer_2km <- rgeos::gBuffer(spgeom = stations , byid = T , width = 2000)
plot(stations)
plot(buffer_2km,border='red',add=T)

# fortify to shapes
f_bogota = fortify(bogota , region = 'MANZ_CCNCT') %>% merge(x = .,y = bogota@data , by.x = 'id' , by.y = 'MANZ_CCNCT' , all.x = T)
f_bogota = f_bogota[oorder(f_bogota$order),]
f_stations = stations %>% data.frame(name = .@data$id , long = stations@coords[,1] , lat = stations@coords[,2] )
f_buffer = fortify(buffer_2km)

# labels
label_map <- geom_text(data=f_stations, aes(x = long, y = lat, label = name), size=2.5, fontface="bold") 

# Paint map
plot_full <- ggplot() + geom_polygon(data = f_bogota, aes(x = long, y = lat, group=id , fill = prop_NBI_per), colour = NA , size = 0.1 , alpha = 0.9) +
             scale_fill_gradient2(low = "#B9D3DC", high = "#141B4D", mid="#5F8DDA", midpoint=20) + 
             geom_polygon(data = f_buffer, aes(x = long, y = lat, group=id , subgroup=group , color = 'A'), fill = NA, size = 0.5 , alpha = 0.2, show.legend = TRUE) +
             scale_color_manual(values = c("A"="red"), labels = c("2km alrededor \nde la estación") , name = NULL) +
             labs(fill = "% personas NBI") + theme_bw() + ggsn::north(f_bogota) + ggsn::scalebar(f_bogota, dist = 5, dist_unit = "km",transform = F, model = "WGS84",location = "bottomleft") +
             coord_equal() + label_map
ggsave(plot = plot_full, filename = 'results/mapa.jpeg',width = 7 , height = 8)









