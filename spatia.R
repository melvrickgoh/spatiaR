library(sp)  # vector data
library(raster)  # raster data
library(rgdal)  # input/output, projections
library(rgeos)  # geometry ops
library(spdep)  # spatial dependence
library(maptools) # read n handle spatial objects
library(spatstat) # point pattern analysis
library(leafletR)
library(KernSmooth)
library(RColorBrewer)

dfToSpatialPoints <- function (dbframe) {
	#print(dbframe)
	#print(typeof(dbframe))
	coords = cbind(dbframe$lng, dbframe$lat)
	#print(typeof(coords))
	#create spatial points from scratch
	sp = SpatialPoints(coords)
	#print("spatial points")
	#print(typeof(sp))
	#assign data attributes
	spDataAttributes = cbind(dbframe$id, dbframe$userid, dbframe$uuid, dbframe$weight, dbframe$status)
	spdf = SpatialPointsDataFrame(coords,dbframe)
	spdf = SpatialPointsDataFrame(sp,dbframe)
	#print("spatial points data frame")
	#print(spdf)
	#return info as a list
	return(list( "dataframe"=spdf, "minLat"=min(dbframe$lat), "maxLat"=max(dbframe$lat), "minLng"=min(dbframe$lng), "maxLng"=max(dbframe$lng) ))
}

lfunctionKDE <- function(spdfSet,userid,uuid){
	spdfFrame = spdfSet$dataframe
	minLat = spdfSet$minLat
	maxLat = spdfSet$maxLat
	minLng = spdfSet$minLng
	maxLng = spdfSet$maxLng

	kdePattern <- ppp(spdfFrame$lng, spdfFrame$lat, window=owin(c(minLng,maxLng), c(minLat,maxLat)))

	print('kde envelope')
	p = 0.05
	n = 100

	K.inh = envelope(kdePattern, Kest, nsim=n, rank=(p * (n + 1)))

	#contourPlot <- plot(K.inh, xlab="d (km)", ylab="K(d)", main=paste("p= ",p))
	return(plot(K.inh, xlab="d (km)", ylab="K(d)", main=paste("p= ",p)))
}

contourKDE <- function(spdfSet,userid,uuid){
	spdfFrame = spdfSet$dataframe

	#new binning to geojson
	# Apply 
	print('binning 50')
	contourKDESub(spdfFrame,userid,uuid,0.00050,"50")

	print('binning 100')
	contourKDESub(spdfFrame,userid,uuid,0.00100,"100")

	print('binning 200')
	contourKDESub(spdfFrame,userid,uuid,0.00200,"200")

	print('binning 300')
	contourKDESub(spdfFrame,userid,uuid,0.00300,"300")

	print('binning 400')
	contourKDESub(spdfFrame,userid,uuid,0.00400,"400")

	# Create a style for the Leaflet map 0.00225
	#sty = styleCat(prop="Value",val=values,style.val=brewer.pal(length(values),"PuRd"),leg = "Tree Cover")

	# Create the map object. This will automatically create a .html file on your machine
	#map = leaflet(dd_json,base.map = "osm",popup="Value",style=sty)
}

contourKDESub <- function(spdfFrame,userid,uuid,bwSize,bwName){
	d2d = bkde2D(cbind(spdfFrame$lng,spdfFrame$lat),bandwidth=c(bwSize,bwSize))
	# Visualise
	contour(d2d$x1,d2d$x2,d2d$fhat)

	# Create linestrings
	lines = contourLines(x=d2d$x1,y=d2d$x2,z = d2d$fhat,nlevels = 8)

	# Create independent polygons within a list
	dd1 = sapply(1:length(lines),function(i) Polygon(as.matrix(cbind(lines[[i]]$x,lines[[i]]$y))))

	# Merge all independent polygons into a Polygons object (this contains multiple polygons)
	dd2 = sapply(1:length(lines),function(i) Polygons(list(dd1[[i]]),i))

	# Don't forget to remember the contour value for each polygon - we store it into a dataframe for use in the next step
	poly_data = data.frame(Value = sapply(1:length(lines),function(i) lines[[i]]$level))

	# Merge the Polygons object dd2 with the dataframe containing the contour level data, poly_data.
	sps <- SpatialPolygons(dd2)
	proj4string(sps) = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

	dd3 = SpatialPolygonsDataFrame(sps,data = poly_data)

	# Convert our dd3 SpatialPolygonDataFrame object to JSON
	dd_json = toGeoJSON(dd3,name=paste("Contour.",userid,".",bwName,".",uuid),dest="C:/Users/Melvrick/Documents/GitHub/is415/geospatialnode/public/r")

	# Store the unique levels of the contours, this will come in handy for colouring
	values = unique(sapply(1:length(lines),function(i) lines[[i]]$level))
}

customKDE <- function(spdfSet) {
	spdfFrame = spdfSet$dataframe
	minLat = spdfSet$minLat
	maxLat = spdfSet$maxLat
	minLng = spdfSet$minLng
	maxLng = spdfSet$maxLng

	kdePattern <- ppp(spdfFrame$lng, spdfFrame$lat, window=owin(c(minLng,maxLng), c(minLat,maxLat)))

	print('kdePlot')
	kdePlot <- plot(kdePattern)
	print(kdePlot)

	print('kdeSummary')
	kdeSummary <- summary(kdePattern)
	print(kdeSummary)
	#k-function estimate

	print('kdeKEstimate normal')
	kdeKEstimate <- Kest(kdePattern)
	plot(kdeKEstimate)
	print(kdeKEstimate)

	print('kde envelope')
	p = 0.05
	n = 500

	K.inh = envelope(kdePattern, Kest, nsim=n, rank=(p * (n + 1)))

	print('kdeKEstimate ripley')
	kdeKEstimate <- Kest(kdePattern,correction="Ripley")
	plot(kdeKEstimate, xlab="d (km)", ylab="K(d)")
	plot(K.inh, xlab="d (km)", ylab="K(d)", main=paste("p= ",p))
	print(kdeKEstimate)

	print('kdePCF')
	kdePCF <- pcf(kdePattern)
	plot(kdePCF)
	print(kdePCF)

	print('kde view 1000')
	kdeMap <- density(kdePattern)
	plot(kdeMap)
	contour(kdeMap)

	print('kde view 0.005')
	plot(density(kdePattern,sigma = 0.005))
	contour(density(kdePattern,sigma = 0.005))

	print('kde view 0.01')
	plot(density(kdePattern,sigma = 0.01))
	contour(density(kdePattern,sigma = 0.01))

	print('kde view 0.02')
	plot(density(kdePattern,sigma = 0.02))
	contour(density(kdePattern,sigma = 0.02))

	writeGDAL(contour(density(kdePattern,sigma = 0.02)), "C:/Users/Melvrick/Documents/GitHub/is415/geospatialnode/public/r/contour1.asc")

	print('kde view 0.03')
	plot(density(kdePattern,sigma = 0.03))
	contour(density(kdePattern,sigma = 0.03))

	print('kde view 0.05')
	plot(density(kdePattern,sigma = 0.05))
	contour(density(kdePattern,sigma = 0.05))
}

adaptiveKDE <- function (spdf) {
	# read shapefile into r as spatial data frame
	#nm <- readShapeSpatial("G://Geospatial_101_Data/SHPFiles/PriSch.shp")

	# convert from spatial data frame to generic 
	nmp <- as(spdf, "SpatialPoints")

	#Convert the generic sp format into spatstat's pp format
	nm_ppp <- as(nmp, "ppp")

	#Computer KDE
	kde_nm_1000 <- density(nm_ppp,1000)
	kde_plot <- plot(kde_nm_1000)
	print(typeof(kde_plot))

	#Alternative method
	plot(density(nm_ppp, 1000))

	#convert the kde out to grid object
	gridded_kde_payload <- as.SpatialGridDataFrame.im(kde_nm_1000)

	spplot(gridded_kde_payload)

	#write the kde raster into ESRI grid format
	writeGDAL(gridded_kde_payload, "C:/Users/Melvrick/Documents/GitHub/is415/spatiaR/raster/1.asc")

	#boundary
	w = owin(c(503500, 562000), c(155800, 201000))
	nmw_ppp <- nm_ppp[w]

	#kde with boundary
	kde_nmw_1000 <- density(nmw_ppp, 1000)

	#adaptive kernel density
	akde_nmw <- adaptive.density(nmw_ppp, 0.1, nrep=5) 

	return(kde_plot)
}

normalKDE <- function (spdf) {
	# load maptools library

	# convert from spatial data frame to generic 
	nmp <- as(spdf, "SpatialPoints")

	#Convert the generic sp format into spatstat's pp format
	nm_ppp <- as(nmp, "ppp")

	#Computer KDE
	kde_nm_1000 <- density(nm_ppp,1000)
	plot(kde_nm_1000)

	#Alternative method
	plot(density(nm_ppp, 1000))

	#convert the kde out to grid object
	kde_payload <- as.SpatialGridDataFrame.im(kde_nm_1000)

	spplot(kde_payload)

	#write the kde raster into ESRI grid format
	writeGDAL(kde_payload, "C:/Users/Melvrick/Documents/GitHub/is415/spatiaR/raster/normalkde.asc")
}