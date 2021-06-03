
# Observed versus predicted temporal sensitivity maps

# It is assumed some Figure_1.R has been run. If not, run it.

# empirical map of temporal sensitivity #

sensitivity_conus <- rangeland_npp_covariates %>% group_by(x, y) %>%
  dplyr::do(model = lm(npp.x~mm.dev, data = .)) %>%
  dplyr::mutate(coef=coef(model)[2])

# Isolate coefficient so only slope is graphed
sensitivity_conus_coef_only<- data.frame(sensitivity_conus[ -c(3) ]) 
#head(sensitivity_conus_coef_only)

# Make raster
sensitivity_raster <- rasterFromXYZ(sensitivity_conus_coef_only)

# Make colors
bks<- quantile(sensitivity_conus_coef_only$coef, probs=seq(0, 1, by=0.05), na.rm = TRUE)
sensitivity=c("purple",'cyan3','green','yellow','orange','red')
bkcols.sensitivity <- colorRampPalette(sensitivity)(length(bks)-1)
r.range.sens <- round(c(minValue(sensitivity_raster), maxValue(sensitivity_raster)),digits=2)

# Update projection
proj4string(sensitivity_raster) <- CRS("+proj=longlat")
sensitivity_raster_2<-projectRaster(sensitivity_raster, crs=aea.proj)

# Prep for producing modeled maps of sensitivity #

# Look at temporal sensitivities in map, veg, and ecoregion S-T interaction models

# Herb null model
herb_coef_predicted<-aggregate(perc_herb_mean~x+y,mean,data=rangeland_npp_covariates)
herb_coef_predicted$coef<-mean(map_herb_coefficients$coefficient.temporal) + 
  mean(map_herb_coefficients$coefficient.temporal_herb_mean)*herb_coef_predicted$perc_herb_mean
#head(herb_coef_predicted)

# MAP null model
map_coef_predicted<-aggregate(mm.y~x+y,mean,data=rangeland_npp_covariates)
map_coef_predicted$coef<-mean(map_coefficients$coefficient.temporal) + 
  mean(map_coefficients$coefficient.spatial_temporal)*map_coef_predicted$mm.y
#head(map_coef_predicted)

# Ecoregion null model
unique(rangeland_npp_covariates$region)
region.list<-c('shortgrass_steppe','northern_mixed_prairies','hot_deserts','cold_deserts','california_annuals')

# Ecoregion
ecoregion_coefficients<-list_to_df_initial(list.coefficients.ecoregion.null)
ecoregion_coefficient_2<-ecoregion_coefficients[c(1,2,3)]

# Do this part manually

# Temporal slopes

# California annuals
ecoregion_coefficients$california_annuals <- ecoregion_coefficients$coefficient.temporal

# Cold deserts
ecoregion_coefficients$cold_deserts <- ecoregion_coefficients$coefficient.temporal + ecoregion_coefficients$coefficient.temporal_regioncold_deserts 

# Hot_deserts
ecoregion_coefficients$hot_deserts <-  ecoregion_coefficients$coefficient.temporal + ecoregion_coefficients$coefficient.temporal_regionhot_deserts 

# Northern mixed prairies
ecoregion_coefficients$northern_mixed_prairies <- ecoregion_coefficients$coefficient.temporal + ecoregion_coefficients$coefficient.temporal_regionnorthern_mixed_prairies

# Shortgrass steppe
ecoregion_coefficients$shortgrass_steppe <- ecoregion_coefficients$coefficient.temporal  + ecoregion_coefficients$coefficient.temporal_regionshortgrass_steppe

temporal_slopes<-select_columns(ecoregion_coefficients)

# Change to long format
data_long_temporal <- gather(temporal_slopes, region, coefficient,-run.id, factor_key=TRUE)
data_long_temporal$model<-'temporal'
coefficients_wide<- spread(data_long_temporal, model, coefficient)

# Merge the spatial and temporal coefficient dataframes
rbind_spatial_temporal<-merge(coefficients_wide,ecoregion_coefficient_2,by=c('run.id'))

ecoregion_null_model<-rbind_spatial_temporal %>%
  group_by(region) %>%
  summarise(temporal=mean(temporal))
ecoregion_null_model<-data.frame(ecoregion_null_model)

mean_for_merge<-aggregate(npp.x~x+y+region,mean,data=rangeland_npp_covariates)
#head(mean_for_merge)
mean_for_merge<-merge(mean_for_merge[c(1,2,3)],ecoregion_null_model,by=c('region'))
mean_for_merge$coef<-round(mean_for_merge$temporal,2)
mean_for_merge_test<-aggregate(coef~region,mean,data=mean_for_merge)
mean_for_merge<-mean_for_merge[c(2,3,5)]
#head(mean_for_merge)


# plot them out and save #

# Per-pixel slopes #


#plot PNG
png(file='Tables_Figures/sens_maps_multipanel.png',
    width=1500,height=1200,res=150)

# Set up multi-panel plot
layout(matrix(1:4, ncol=2))
par(oma=c(6, 5, 6, 5), mar=c(0.2, 0.0, 1.6, 0.2),pty='s')

# Panel label setup
line = 0.5
cex = 1.5
side = 3
adj= 0.1

# Observed patterns
plot(sensitivity_raster_2,breaks = bks,axes=F,box=F,col = bkcols.sensitivity,legend=TRUE,
     legend.width=1, legend.shrink=.75,main='Observed',cex.main=1,
     axis.args=list(at=seq(r.range.sens[1], r.range.sens[2], 0.1),
                    labels=seq(r.range.sens[1], r.range.sens[2], 0.1),
                    cex.axis=1.0),
     legend.args=list(text='', side=4, font=10, line=2.5, cex=0.7))
plot(states_all_sites,add=TRUE,lwd = 1)
mtext("A", side=side, line=line, cex=cex, adj=adj)

# Climate model #

plot_sensitivity(rasterFromXYZ(map_coef_predicted[c(1,2,4)]),title='Climate Model')
mtext("B", side=side, line=line, cex=cex, adj=adj)

# Herb model #

plot_sensitivity(rasterFromXYZ(herb_coef_predicted[c(1,2,4)]),title='Herb Model')
mtext("C", side=side, line=line, cex=cex, adj=adj)

# Ecoregion model #

plot_sensitivity(rasterFromXYZ(mean_for_merge),title='Ecoregion Model')
mtext("D", side=side, line=line, cex=cex, adj=adj)
mtext('Temporal sensitivity', side=2,cex=1.5,line=1.5,outer=TRUE)
mtext(expression(paste("(g"/"m"^"2"/"mm)")),side=2,line= -0.5,cex=1.5,outer=TRUE)

dev.off()

#