#produce different data frames and summary stats from the loop


#### Get coefficients into a usable dataframe -----------


# climate interaction, no ecoregion #
map_coefficients<-list_to_df(list.coefficients.noveg,region=F)

# veg interaction, no ecoregion #
map_herb_coefficients<-list_to_df(list.coefficients.herb.map,region=F) 

# # ecoregion interaction only #
# *Have to do this one manually in the Sensitivity_Maps.R script

# climate interaction with annual precipitation, varies by ecoregion #
map_region_coefficients <-list_to_df_with_ecoregion(list.coefficients.region,veg=F,full=F)

# get the predicted temporal slope at MAP for each ecoregion #
map_region_coefficients$temporal_sensitivity_map <-
  map_region_coefficients$Temporal + map_region_coefficients$Spatiotemporal*map_region_coefficients$map

# herbaceous vegetation interaction with annual precipitation, varies by ecoregion #
herb_region_coefficients <-list_to_df_with_ecoregion(list.coefficients.herb.region,veg = T,full=F)

# get mean herb cover for each ecoregion and merge with coefficients data frame #
region_mean_herb<-aggregate(perc_herb_mean~region,mean,data=rangeland_npp_covariates) 
region_mean_herb$herb<-round(region_mean_herb$perc_herb,1)
region_mean_herb<-region_mean_herb[-c(2)]
herb_region_coefficients<-merge(herb_region_coefficients,region_mean_herb,by=c('region'))

#-------------------------------------------------------------------------------

# Summary statistics of model coefficients --------------

# MAP, ecoregion-specific model #

# Mean
map_region_coefficient_means <-map_region_coefficients %>%
  group_by(region) %>%
  summarise_all(mean)
map_region_coefficient_means<-data.frame(map_region_coefficient_means)
map_region_coefficient_means$stat <- 'mean'

# 95% confidence interval
map_region_coefficient_95ci <- map_region_coefficients %>%
  group_by(region) %>%
  summarise_all(error.95)
map_region_coefficient_95ci <-data.frame(map_region_coefficient_95ci)
map_region_coefficient_95ci$stat <- '95_ci'

# Merge
map_region_coef_summary_merged<-rbind(map_region_coefficient_95ci,map_region_coefficient_means)

# Save 
write.csv(map_region_coef_summary_merged,
          file = 'Tables_Figures/Supporting/map_region_coef.csv')

# Herb, ecoregion model #

# Mean
herb_region_coefficient_means <- herb_region_coefficients %>%
  group_by(region) %>%
  summarise_all(mean)
herb_region_coefficient_means<-data.frame(herb_region_coefficient_means)
herb_region_coefficient_means$stat <- 'mean'

# 95% confidence interval
herb_region_coefficient_95ci <- herb_region_coefficients %>%
  group_by(region) %>%
  summarise_all(error.95)
herb_region_coefficient_95ci <-data.frame(herb_region_coefficient_95ci)
herb_region_coefficient_95ci$stat <- '95_ci'

# Merge
herb_region_coef_summary_merged<-rbind(herb_region_coefficient_95ci,herb_region_coefficient_means)

# Save
write.csv(herb_region_coef_summary_merged,
          file = 'Tables_Figures/Supporting/herb_region_coef.csv')

#
#


#-------------------------------------------------------------------------------
# Summary statistics of mean annual precipitation ---------

map_means_by_ecoregion<-aggregate(mm.y~x+y+region,mean,data=rangeland_npp_covariates)
map_ci_by_ecoregion<-aggregate(mm.y~region,error.95,data=map_means_by_ecoregion)
map_means_ci_by_ecoregion<-merge(map_means_by_ecoregion,map_ci_by_ecoregion,by='region')
colnames(map_means_ci_by_ecoregion) <-c('region','mean annual precipitation','95% ci')

# Save
write.csv(map_means_ci_by_ecoregion,
          file = 'Tables_Figures/Supporting/ecoregion_MAP.csv')
#
#

#-------------------------------------------------------------------------------
# AIC data frame production -------------

# Climate interaction with annual precipitation deviation, varies by ecoregion #
aic.region.binded<-bind.aic(list.aic.region)
aic.region.binded$model<-'ecoregion.map'

# Herb interaction, does not vary by ecoregion #
aic.herb.map.binded<-bind.aic(list.aic.herb.map)
aic.herb.map.binded$model<-'only.herb'

# Herb interaction with annual precipitation deviation, varies by ecoregion #
aic.herb.region.binded<-bind.aic(list.aic.herb.region)
aic.herb.region.binded$model<-'ecoregion.herb'

# Climate interaction, no ecoregion #
aic.noveg.binded<-bind.aic(list.aic.noveg)
aic.noveg.binded$model<-'only.map'

# Ecoregion only interaction #
list.aic.ecoregion.null.binded<-bind.aic(list.aic.ecoregion.null)
list.aic.ecoregion.null.binded$model<-'only.ecoregion'

# Bind them all
aic.binded<-do.call("rbind", list(aic.herb.map.binded, aic.region.binded, aic.herb.region.binded,
                                  aic.noveg.binded,list.aic.ecoregion.null.binded))

# Mean
aic_means<-aggregate(aic~model,mean,data=aic.binded)

# Save
write.csv(aic_means,
          file = 'Tables_Figures/Supporting/AIC.csv')

#
#

#-------------------------------------------------------------------------------

# Total R-squared data frame production -------------

# Climate interaction with annual precipitation deviation, varies by ecoregion
r.squared.region.binded<-bind.r.squared(list.r.squared.region)
r.squared.region.binded$model <- 'ecoregion.map'

# Herb interaction with annual precipitation deviation, no ecoregion
r.squared.herb.map.binded<-bind.r.squared(list.r.squared.herb.map)
r.squared.herb.map.binded$model <- 'only.herb'

r.squared.model.1.2<-rbind(r.squared.region.binded,r.squared.herb.map.binded)

# Herb interaction with annual precipitation deviation, no ecoregion
r.squared.region.binded<-bind.r.squared(list.r.squared.herb.region)
r.squared.region.binded$model<-'ecoregion.herb'

# Climate interaction, no ecoregion
r.squared.noveg.binded<-bind.r.squared(list.r.squared.noveg)
r.squared.noveg.binded$model<-'only.map'

r.squared.model.3.4<-rbind(r.squared.region.binded,r.squared.noveg.binded)

r.squared.model.1234<-rbind(r.squared.model.1.2,r.squared.model.3.4)

r.squared.model.1234.mean<-aggregate(r.squared~model,mean,data=r.squared.model.1234)
r.squared.model.1234.mean$r.squared <-round(r.squared.model.1234.mean$r.squared,2)


# Save
write.csv(r.squared.model.1234.mean,
          file = 'Tables_Figures/Supporting/model_r-squared.csv')

#
#


#done---------------

# done


