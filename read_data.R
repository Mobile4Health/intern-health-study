#
# Mood sample data reading
#
# First version Feb 24, 2017


data_dir <- "/Users/zhenkewu/Documents/professional/data/Sen_S/IHS_MoodLocation_sampleData.xlsx"

dat_mood <- xlsx::read.xlsx(data_dir,1)
dat_geo <- xlsx::read.xlsx(data_dir,2)
dat_geo0 <- dat_geo
# explore mood data:
dat_mood_wide <- reshape2::dcast(dat_mood,UserID~date, variable=mood_score)

pdf("mood_eda.pdf",width=12,height=8)
plot(1:(ncol(dat_mood_wide)-1),dat_mood_wide[1,-1],type="l",xlab="",
     ylab="mood score",ylim=c(0,10),xaxt="n")
points(1:(ncol(dat_mood_wide)-1),dat_mood_wide[2,-1],type="l",col="red")
points(1:(ncol(dat_mood_wide)-1),dat_mood_wide[3,-1],type="l",col="blue")

unique_dates <- as.Date(colnames(dat_mood_wide)[-1])
axis(1,at = 1:length(unique_dates),labels=unique_dates,las=2,cex.axis=0.5)
dev.off()

# explore geolocation data:

geo_coord_metric <- t(sapply(as.character(dat_geo$metric_measure_value),
                             function(v) as.numeric(unlist(strsplit(v,split=" ")))))
colnames(geo_coord_metric) <- c("lat","lon")
rownames(geo_coord_metric) <- NULL
dat_geo <- cbind(dat_geo,geo_coord_metric)



#
# learn GPS plotting:
#

library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)

usa <- map_data("usa")
w2hr <- map_data("world2Hires")

ggplot() + geom_polygon(data = usa, aes(x=lon, y = lat, group = group)) + 
  coord_fixed(1.3)

ggplot() + 
  geom_polygon(data = usa, aes(x=lon, y = lat, group = group), fill = NA, color = "red") + 
  coord_fixed(1.3)

gg1 <- ggplot() + 
  geom_polygon(data = usa, aes(x=lon, y = lat, group = group), fill = "violet", color = "blue") + 
  coord_fixed(1.3)
gg1


labs <- data.frame(
  lon = c(-122.064873, -122.306417),
  lat = c(36.951968, 47.644855),
  names = c("SWFSC-FED", "NWFSC"),
  stringsAsFactors = FALSE
)  

gg1 + 
  geom_point(data = dat_geo, aes(x = lon, y = lat), color = "black", size = 5) +
  geom_point(data = dat_geo, aes(x = lon, y = lat), color = "yellow", size = 4)


#
# Michigan
#

states <- map_data("state")
michigan <- subset(states, region %in% c("michigan"))

ggplot(data = michigan) + 
  geom_polygon(aes(x = lon, y = lat, group = group), fill = "palegreen", color = "black") + 
  coord_fixed(1.3)

counties <- map_data("county")
mi_county <- subset(counties, region == "michigan")

mi_base <- ggplot(data = michigan, mapping = aes(x = lon, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "gray")
mi_base + theme_nothing()

mi_base + theme_nothing() + 
  geom_polygon(data = mi_county, fill = NA, color = "white") +
  geom_polygon(color = "black", fill = NA)


sbbox <- make_bbox(lon = dat_geo$lon, lat = dat_geo$lat, f = .1)
sbbox

# First get the map. By default it gets it from Google.  I want it to be a satellite map
sq_map <- get_map(location = sbbox, maptype = "roadmap", source = "google")
ggmap(sq_map) + geom_point(data = dat_geo, mapping = aes(x = lon, y = lat), color = "red")

ll_means <- sapply(dat_geo[,c("lon","lat")], mean)
sq_map2  <- get_map(location = ll_means,  maptype = "roadmap", source = "google", zoom = 12)
ggmap(sq_map2) + 
  geom_point(data = dat_geo, mapping = aes(x = lon, y = lat), color = "red", size = 4) +
geom_text(data = dat_geo, aes(label = paste("  ", as.character(start_date), sep="")), 
          angle = 60, hjust = 0, color = "yellow")

dat_mood$date > dat_geo[1,c("start_date","end_date")]

dat_geo0 <- dat_geo
curr_ID <- which(dat_geo0$UserID=="222222")
dat_geo <- dat_geo0[curr_ID,]

#>       left     bottom      right        top 
#> -119.76198   34.75111 -119.74201   34.75507
mybox <- c(-83.705,42.24,-83.75,42.31)

mybox <- make_bbox(-83.7,42.28,f=0.01)
sq_map2  <- get_map(mybox, 
                    maptype = "roadmap", source = "google",zoom=13)
ggmap(sq_map2) + 
  geom_point(data = dat_geo,aes(lon,lat,color=UserID),color="red")+
  geom_path(data = dat_geo, aes(color = UserID), size = 1, lineend = "round") + 
  scale_color_gradientn(colours = rainbow(7), breaks = seq(25, 200, by = 25))+
  geom_text(data = data.frame(lon=dat_geo$lon+0.003*c(1,2,3,1,2,1,2,4,5,3),
                              lat=dat_geo$lat,
                              start_date = 1:length(curr_ID)
                              ), aes(label = paste("  ", as.character(start_date), sep="")), 
            angle = 0, hjust = 0, color = "blue")






