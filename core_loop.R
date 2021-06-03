
# spatial block bootrapping code


# Set up lists as the initial objects to store model outputs in -------

# model coefficients #
list.coefficients.region<-list()
list.coefficients.herb.map<-list()
list.coefficients.herb.region<-list()
list.coefficients.noveg<-list()
list.coefficients.ecoregion.null<-list()

# AIC #
list.aic.region<-list()
list.aic.herb.map<-list()
list.aic.herb.region<-list()
list.aic.noveg<-list()
list.aic.ecoregion.null<-list()

# r-squared: total #
list.r.squared.region<-list()
list.r.squared.herb.map<-list()
list.r.squared.herb.region<-list()
list.r.squared.noveg<-list()
list.r.squared.ecoregion.null<-list()

# r-squared comparisons #
compare.r.squared.region.list<-list()
compare.r.squared.herb.list<-list()
compare.r.squared.map.list<-list()

# rmse comparisons #
compare.rmse.region.list<-list()
compare.rmse.herb.list<-list()
compare.rmse.map.list<-list()


# Stratified spatial block bootstrapping loop ----------

for(i in 1:1000)
{
  
  # Stratify by region and the MAP of the region #
  test.strat.northern_mixed<-stratified(northern_mixed_prairies_above_below, c("map"), 0.01)
  test.strat.cold_deserts<-stratified(cold_deserts_above_below, c("map"), 0.01)
  test.strat.california_annuals<-stratified(california_annuals_above_below, c("map"), 0.05)
  test.strat.semiarid_steppe<-stratified(semiarid_steppe_above_below, c("map"), 0.02)
  test.strat.hot_deserts<-stratified(hot_deserts_above_below, c("map"), 0.02)
  test.strat<-rbind(test.strat.northern_mixed, test.strat.cold_deserts, test.strat.california_annuals, 
                    test.strat.semiarid_steppe, test.strat.hot_deserts)
  
  # Combine into one data set to run different models #
  stratified_final<-merge(test.strat, rangeland_npp_covariates,by=c('x','y','region'))
  
  # Climate interaction, no ecoregion #
  stratified_final_lm_noveg<-lm(npp.x~mm.y + mm.dev + mm.dev:mm.y
                                ,stratified_final)
  
  # Herb interaction, no ecoregion #
  stratified_final_lm_herb_map<-lm(npp.x~mm.y + mm.dev  +  mm.dev:perc_herb_mean
                                   ,stratified_final)
  
  # Ecoregion interaction #
  stratified_final_ecoregion.null<-lm(npp.x ~ mm.y + mm.dev  +  mm.dev:region,stratified_final)
  
  # Climate interaction with annual precipitation deviation, varies by ecoregion #
  stratified_final_lm_region<-lm(npp.x~mm.dev + region + mm.y + mm.dev:region + mm.dev:mm.y
                                 + region:mm.y + mm.dev:region:mm.y,stratified_final)
  # 
  #  Herb vegetation interaction with annual precipitation deviation, varies by ecoregion #
  stratified_final_lm_herb_region<-lm(npp.x~mm.dev + region + mm.y + perc_herb_mean + mm.dev:region + mm.dev:perc_herb_mean +
                                        + region:mm.y + region:perc_herb_mean + mm.dev:region:perc_herb_mean,stratified_final)

  # 
  
  # Now get and store information from the models in the lists 
  
  # Coefficients #
  list.coefficients.region[[i]] <- df.coef.create(stratified_final_lm_region)
  list.coefficients.herb.map[[i]] <- df.coef.create(stratified_final_lm_herb_map)
  list.coefficients.herb.region[[i]] <- df.coef.create(stratified_final_lm_herb_region)
  list.coefficients.noveg[[i]] <- df.coef.create(stratified_final_lm_noveg)
  list.coefficients.ecoregion.null[[i]] <- df.coef.create(stratified_final_ecoregion.null)
  
  # AIC #
  list.aic.region[[i]] <- df.aic.create(stratified_final_lm_region)
  list.aic.herb.map[[i]] <- df.aic.create(stratified_final_lm_herb_map)
  list.aic.herb.region[[i]] <- df.aic.create(stratified_final_lm_herb_region)
  list.aic.noveg[[i]] <- df.aic.create(stratified_final_lm_noveg)
  list.aic.ecoregion.null[[i]]<-df.aic.create(stratified_final_ecoregion.null)
  
  # r-squared: total #
  list.r.squared.region[[i]] <- df.r.squared.create(stratified_final_lm_region)
  list.r.squared.herb.map[[i]] <- df.r.squared.create(stratified_final_lm_herb_map)
  list.r.squared.herb.region[[i]] <- df.r.squared.create(stratified_final_lm_herb_region)
  list.r.squared.noveg[[i]] <- df.r.squared.create(stratified_final_lm_noveg)
  list.r.squared.ecoregion.null[[i]]<-df.r.squared.create(stratified_final_ecoregion.null)
  
  # r-squared comparisons: decomposing sources of variation in NPP #
  compare.r.squared.region.list[[i]]<-compare_fit_2(model=stratified_final_ecoregion.null,stratified_final,metric = 'R2',main='same')
  compare.r.squared.herb.list[[i]]<-compare_fit_2(model=stratified_final_lm_herb_map,stratified_final,metric = 'R2',main='same')
  compare.r.squared.map.list[[i]]<-compare_fit_2(model=stratified_final_lm_noveg,stratified_final,metric = 'R2',main='same')
  
  # RMSE comparisons: decomposing sources variation in NPP #
  compare.rmse.region.list[[i]]<-compare_fit_2(model=stratified_final_ecoregion.null,stratified_final,metric = 'RMSE',main='same')
  compare.rmse.herb.list[[i]]<-compare_fit_2(model=stratified_final_lm_herb_map,stratified_final,metric = 'RMSE',main='same')
  compare.rmse.map.list[[i]]<-compare_fit_2(model=stratified_final_lm_noveg,stratified_final,metric = 'RMSE',main='same')
  # *check/replace 'ompare'

  
}

#remove excess stuff so doesn't clog memory
rm(stratified_final,stratified_final_ecoregion.null,stratified_final_lm_herb_map,
   stratified_final_lm_herb_region, stratified_final_lm_noveg,stratified_final_lm_region,
   test.strat,test.strat.california_annuals,test.strat.cold_deserts,test.strat.hot_deserts,
   test.strat.northern_mixed,test.strat.semiarid_steppe,california_annuals_above_below,
   cold_deserts_above_below,semiarid_steppe_above_below,northern_mixed_prairies_above_below,
   hot_deserts_above_below)


#done