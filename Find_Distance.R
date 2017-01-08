library(ggmap)

library(ggplot2)

library(ggthemes)

library(sp)

library(leaflet)

options(digits=15)

setwd('C:/Users/someUser/Desktop/Folder')

dir(pattern='*.csv')

warehouses<-read.csv('warehouse_stats_w_latlon.csv')

warehouses$sq_ft<-as.numeric(as.character(warehouses$sq_ft))

my.loc<-'boston, ma'

loc.coords<-geocode(my.loc)

loc.coords

complete.latlon<-subset(warehouses, complete.cases(warehouses$lat)==T)

warehouses.coords <- SpatialPoints(coords = data.frame(lon=complete.latlon$lon,                                          

lat=complete.latlon$lat), proj4string=CRS("+proj=longlat +datum=WGS84"))

my.coords <- SpatialPoints(coords = data.frame(loc.coords),

                 proj4string=CRS("+proj=longlat +datum=WGS84"))

complete.latlon$distance <- spDists(warehouses.coords, my.coords, longlat=T)

closest.warehouse<-complete.latlon[which.min(complete.latlon$distance),]

closest.warehouse

warehouse.address<-as.character(closest.warehouse$location)

route.df<-route(my.loc, warehouse.address, structure = 'route')

qmap(my.loc, zoom = 13) +  

  geom_path(data = route.df, aes(x = lon, y = lat),  colour = "red", size = 1.5, lineend = "round")

ind.warehouse <- get_googlemap(center = c(lon = closest.warehouse$lon, lat = closest.warehouse$lat),

scale = 2, maptype = 'satellite', zoom=17,

markers = data.frame(lon = closest.warehouse$lon,

lat = closest.warehouse$lat))

ggmap(ind.warehouse, extent = 'device')

dynamic.map<-leaflet(complete.latlon)

dynamic.map<- addTiles(dynamic.map)

pal<-colorFactor(c('orange','darkblue','red','blue'), domain=complete.latlon$retailer)

dynamic.map<- addCircleMarkers(dynamic.map,

                               popup =

paste('Retailer:', complete.latlon$retailer,'<br>',

      'Type:', complete.latlon$type, '<br>',

      'Square Feet:', complete.latlon$sq_ft, '<br>',

      'Opened:', complete.latlon$yr_open,'<br>',

      'Address:',complete.latlon$location,'<br>',

      'Distance to me:',complete.latlon$distance),

color = ~pal(retailer))