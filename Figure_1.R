# Figure 1

# Prepare projection and shapefiles 

# Shapefile reference for state outlines. This downloads the files.
us<-getData("GADM", country='USA', level=1,download=TRUE)
states_all_sites <- us[us$NAME_1 %in% c('California','New Mexico','Arizona','Utah',
                                        'Arizona','Colorado','Washington','Wyoming',
                                        'Idaho','Oregon','Idaho','Montana','Texas',
                                        'North Dakota','South Dakota','Nebraska',
                                        'Oklahoma','Kansas'),]

# Update projection
aea.proj <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
states_all_sites <- spTransform(states_all_sites, CRS(aea.proj))

# 1A: Ecoregion map 

# Set extent from west-wdie raster as a reference for the spatial extent
mean_production<-aggregate(npp.x~ x + y,mean,data=rangeland_npp_covariates)
mean_production_raster<-rasterFromXYZ(mean_production)

# Mojave and Sonoran desert shapefile

# Step 1:
mojave.sonoran.shape<-readOGR(dsn="Shapefiles/MojaveSonoran",layer="MojaveSonoran")
mojave.sonoran.shape@bbox <- as.matrix(extent(mean_production_raster))

# Step 2:
mojave.sonoran.shape.2 <- sp::spTransform(mojave.sonoran.shape, CRS(aea.proj))
mojave.sonoran.shape.3 <- spTransform(mojave.sonoran.shape.2, CRS(aea.proj))

# Chihuahan Desert

# Step 1:
ChihuahuanDesert.shape<-readOGR(dsn="Shapefiles/ChihuahuanDesert",layer="ChihuahuanDesert")
ChihuahuanDesert.shape@bbox <- as.matrix(extent(mean_production_raster))

# Step 2:
ChihuahuanDesert.shape.2 <- sp::spTransform(ChihuahuanDesert.shape, CRS(aea.proj))

# Grama Galleta Steppe

# Step 1:
Grama.Galleta.Steppe.shape<-readOGR(dsn="Shapefiles/GramaGalletaSteppe",layer="GramaGalletaSteppe")
Grama.Galleta.Steppe.shape@bbox <- as.matrix(extent(mean_production_raster))

# Step 2:
Grama.Galleta.Steppe.shape.2 <- sp::spTransform(Grama.Galleta.Steppe.shape, CRS(aea.proj))

# California Annuals

# Step 1:
CaliforniaAnnual.shape<-readOGR(dsn="Shapefiles/CaliforniaAnnual",layer="CaliforniaAnnual")
CaliforniaAnnual.shape@bbox <- as.matrix(extent(mean_production_raster))

# Step 2:
CaliforniaAnnual.shape.2 <- sp::spTransform(CaliforniaAnnual.shape, CRS(aea.proj))

# Cold Deserts

# Step 1:
ColdDeserts.shape<-readOGR(dsn="Shapefiles/ColdDeserts",layer="ColdDeserts")
ColdDeserts.shape@bbox <- as.matrix(extent(mean_production_raster))

# Step 2:
ColdDeserts.shape.2 <- sp::spTransform(ColdDeserts.shape, CRS(aea.proj))

# Shortgrass Steppe

# Step 1:
SGS.shape<-readOGR(dsn="Shapefiles/SGS",layer="SGS")
SGS.shape@bbox <- as.matrix(extent(mean_production_raster))

# Step 2:
SGS.shape.2 <- sp::spTransform(SGS.shape, CRS(aea.proj))

# Northern Mixed Prairies

# Step 1:
NorthernMixedSubset.shape<-readOGR(dsn="Shapefiles/NorthernMixedSubset",layer="NorthernMixedSubset")
NorthernMixedSubset.shape@bbox <- as.matrix(extent(mean_production_raster))

# Step 2:
NorthernMixedSubset.shape.2 <- sp::spTransform(NorthernMixedSubset.shape, CRS(aea.proj))

# Panel setup

png(file='Tables_Figures/Scope.png',
    width=1500,height=1200,res=150)

layout(matrix(1:4, ncol=2))
par(oma=c(6, 5, 6, 5), mar=c(0.2, 0.0, 1.6, 0.2),pty='s')

# Panel label setup
line = 0.5
cex = 1.5
side = 3
adj= 0.05

# Set up color scale
npp= c('wheat3','wheat', "orange", "yellow",'green','darkgreen')
bks_npp<- quantile(mean_production$npp.x, probs=seq(0, 1, by=0.05), na.rm = TRUE)
bkcols.npp <- colorRampPalette(npp)(length(bks_npp)-1)
proj4string(mean_production_raster)<-CRS(aea.proj)
r.range.npp <- round(c(minValue(mean_production_raster), maxValue(mean_production_raster)))

# Update projection
proj4string(mean_production_raster) <- CRS("+proj=longlat")
mean_production_raster_2<-projectRaster(mean_production_raster, crs=aea.proj)

# Make a 'blank' raster to populate with ecoregions
mean_production_raster_2$new<-0

#ecoregion plot
plot(mean_production_raster_2$new,breaks = bks_npp,box=F,axes=F,col = bkcols.npp,
     legend=F,main='Ecoregion Distribution',cex.main=0.75)
plot(states_all_sites, lwd = 1,add=TRUE)
plot(NorthernMixedSubset.shape.2,col='steelblue2', lwd = .1,add=TRUE)
plot(SGS.shape.2, lwd = 0.1,col='green4', lwd = .1,add=TRUE)
plot(CaliforniaAnnual.shape.2,col='grey', lwd = .1,add=TRUE)
plot(ChihuahuanDesert.shape.2, col='firebrick3', lwd = .1,add=TRUE)
plot(mojave.sonoran.shape.2, col='firebrick3', lwd = .1,add=TRUE)
plot(Grama.Galleta.Steppe.shape.2, col='firebrick3', lwd = .1,add=TRUE)
plot(ColdDeserts.shape.2, col='gold', lwd = .1,add=TRUE)
mtext("A", side=side, line=line, cex=cex, adj=adj)

#legend
legend('bottomleft', legend=c("Hot deserts", "Cold deserts","California annuals",
                              "Northern mixed prairies","Shortgrass steppe"),
       col=c("firebrick3", "gold","grey","steelblue2","green4"), lty=1,
       bty='n',lwd=1.5,cex=0.5,box.lty=0)

# 1B: mean production map 

plot(mean_production_raster_2$npp.x,breaks = bks_npp,box=F,axes=F,col = bkcols.npp,
     legend.width=0.80,legend.shrink=1,main='Mean Net Primary Production',cex.main=0.75,
     axis.args=list(at=seq(r.range.npp[1], r.range.npp[2], 100),
                    labels=seq(r.range.npp[1], r.range.npp[2], 100),
                    cex.axis=0.75),
     legend.args=list(text=expression(paste("g"/"m"^"2")),side=4, font=2,adj=0.5, line=2.5, cex=0.9))
plot(states_all_sites, lwd = 1,add=TRUE)
mtext("B", side=side, line=line, cex=cex, adj=adj)

# 1C: Mean annual precipitation map 

# Create raster
mean_mm<-aggregate(mm.x~ x + y,mean,data=rangeland_npp_covariates)
mean_mm_raster<-rasterFromXYZ(mean_mm)

# Color scale
precip= c("red", "orange", "yellow",'green','cyan3','purple')
bks_map<- quantile(mean_mm$mm.x, probs=seq(0, 1, by=0.05), na.rm = TRUE)
bkcols.precip <- colorRampPalette(precip)(length(bks_map)-1)
proj4string(mean_mm_raster)<-CRS(aea.proj)
r.range <- round(c(minValue(mean_mm_raster), maxValue(mean_mm_raster)))

# Update projection
proj4string(mean_mm_raster) <- CRS("+proj=longlat")
mean_mm_raster_2<-projectRaster(mean_mm_raster, crs=aea.proj)

# Plot it
plot(mean_mm_raster_2,breaks = bks_map,axes=F,box=F,col = bkcols.precip,
     legend.width=0.80,legend.shrink=1,main='Mean Annual Precipitation',cex.main=0.75,
     axis.args=list(at=seq(r.range[1], r.range[2], 100),
                    labels=seq(r.range[1], r.range[2], 100),
                    cex.axis=0.75),
     legend.args=list(expression(paste("mm")), side=4, font=2,adj=0.5, line=2.5, cex=0.9))
plot(states_all_sites, lwd = 1,add=TRUE)
mtext("C", side=side, line=line, cex=cex, adj=adj)

# 1D: %Herbaceous NPP map 

# Turn into raster
mean_herb<-aggregate(perc_herb_mean~ x + y,mean,data=rangeland_npp_covariates)
mean_herb_raster<-rasterFromXYZ(mean_herb)

# Set up color scale
herb= c('wheat3','wheat', "orange", "yellow",'green','darkgreen')
bks_herb<- quantile(mean_herb$perc_herb_mean, probs=seq(0.0, 1, by=0.05), na.rm = TRUE)
bkcols.herb <- colorRampPalette(herb)(length(bks_herb)-1)
proj4string(mean_herb_raster)<-CRS(aea.proj)
r.range.herb <- round(c(minValue(mean_herb_raster), maxValue(mean_herb_raster)))

# Update projection
proj4string(mean_herb_raster) <- CRS("+proj=longlat")
mean_herb_raster_2<-projectRaster(mean_herb_raster, crs=aea.proj)

# Plot it
plot(mean_herb_raster_2,breaks = bks_herb,box=F,axes=F,col = bkcols.herb,
     legend.width=0.80,legend.shrink=1,main='Hebaceous Vegetation',cex.main=0.75,
     axis.args=list(at=seq(r.range.herb[1], r.range.herb[2], 10),
                    labels=seq(r.range.herb[1], r.range.herb[2], 10),
                    cex.axis=0.75),
     legend.args=list(expression(paste("% Cover")), side=4, font=2,adj=0.5, line=2.5, cex=0.9))
plot(states_all_sites, lwd = 1,add=TRUE)
mtext("D", side=side, line=line, cex=cex, adj=adj)

dev.off()
