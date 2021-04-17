##GBIF上下载数据
#install.packages(c('raster', 'rgdal', 'dismo', 'rJava'))
# loads the dismo (distribution modeling) library
#using an example file that is installed with the dismo package
setwd("d:/gbif")
library(dismo)
#Xishu= gbif("niviventer", "niviventer")
Xishu= gbif("desmodus", "rotundus")
#Use '*' to download the entire genus. 
#Append '*' to the species name to get all naming variants 
#Xishu= gbif("Camptotheca", "acuminata*",extent(85,125,10,55))
#Xishu= gbif("Camptotheca", "",extent(85,125,10,55))
# how many rows and colums?
dim(Xishu)
#select the records that have longitude and latitude data
colnames(Xishu)
Xishu$lon
Xishu$lat
XSgeo <- subset(Xishu, !is.na(lon) & !is.na(lat))
dim(XSgeo)
#remove duplicated record
dups <- duplicated(XSgeo[, c('lon', 'lat')])
XSfin=XSgeo[!dups,]
dim(XSfin)
# show some values
#opne world simple map
library(maptools)
data(wrld_simpl)
plot(wrld_simpl, xlim=c(-180,180), ylim=c(-60,60), axes=TRUE,col="light blue")
# restore the box around the map
box()
# plot points
points(XSfin$lon, XSfin$lat, col='red', pch=5, cex=0.1)

Xishu

write.csv(Xishu,"a.csv")

