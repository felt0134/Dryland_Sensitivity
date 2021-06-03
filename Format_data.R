
# Import and set up data frame

# Load file NPP and PPT file
rangeland_npp_covariates<-readRDS("Data/Dryland_NPP.rds")

#get mean annual precip for each pixel
mean_mm_site<-aggregate(mm~x+y+region,mean,data=rangeland_npp_covariates)
#head(mean_mm_site)
#summary(mean_mm_site)

#hot deserts
hot_deserts_1 <-subset(mean_mm_site,region=="hot_deserts")
hot_deserts_above_below <- above_below_map(hot_deserts_1)
rm(hot_deserts_1)

#cold_deserts
cold_deserts_1 <-subset(mean_mm_site,region=="cold_deserts")
cold_deserts_above_below <- above_below_map(cold_deserts_1)
rm(cold_deserts_1)

#california_annuals
california_annuals_1 <-subset(mean_mm_site,region=="california_annuals")
california_annuals_above_below <- above_below_map(california_annuals_1)
rm(california_annuals_1)

#shortgrass steppe
semiarid_steppe_1 <-subset(mean_mm_site,region=="shortgrass_steppe")
semiarid_steppe_above_below <- above_below_map(semiarid_steppe_1)
rm(semiarid_steppe_1)

#northern mixed prairies
northern_mixed_prairies_1 <-subset(mean_mm_site,region=="northern_mixed_prairies")
northern_mixed_prairies_above_below <- above_below_map(northern_mixed_prairies_1)
rm(northern_mixed_prairies_1)

#making colomnn for per-pixel mean npp
rangeland_mean_npp<-aggregate(npp ~ x + y + region,mean,data=rangeland_npp_covariates)

#merge with mean precip
mm_production_mean<-merge(rangeland_mean_npp,mean_mm_site,by=c('x','y','region'))

rm(rangeland_mean_npp,mean_mm_site)

#merge initial dataframe with means
rangeland_npp_covariates<-merge(rangeland_npp_covariates,mm_production_mean,by=c('x','y','region'))

rm(mm_production_mean)

#add mm and npp deviations from the mean
rangeland_npp_covariates$npp.dev<-rangeland_npp_covariates$npp.x - rangeland_npp_covariates$npp.y
rangeland_npp_covariates$mm.dev<-rangeland_npp_covariates$mm.x - rangeland_npp_covariates$mm.y

# Import fractional cover data frame

fractional_cover<-readRDS(('Data/Fractional_cover_Western_US.rds'))
#head(fractional_cover)
fractional_cover <-aggregate(herb_cover~x+y,mean,data=fractional_cover)
fractional_cover$perc_herb_mean <- fractional_cover$herb_cover
fractional_cover$perc_herb_mean <- round(fractional_cover$perc_herb_mean,1)
fractional_cover <- fractional_cover[-c(3)]

#look at summed cover per site

#merge
rangeland_npp_covariates<-merge(rangeland_npp_covariates,fractional_cover,
                                by=c('x','y'))

rm(fractional_cover)

#done