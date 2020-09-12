library(RgoogleMaps)
#
maplocation <- read.xlsx("map-location.xlsx", sheet = 1)
#
my.lat <- maplocation$'纬度'
my.lon <- maplocation$'经度'
#找邊界
bb = qbbox(my.lat, my.lon)
print(bb)
MyMap <- GetMap.bbox(bb$lonR, bb$latR, maptype = "roadmap")
My.markers <- cbind.data.frame(lat = my.lat, lon = my.lon)
tmp <- PlotOnStaticMap(MyMap, lat = My.markers[,"lat"], lon = My.markers[,"lon"], 
                       cex=3, pch=16, col=10, add=F)

    

