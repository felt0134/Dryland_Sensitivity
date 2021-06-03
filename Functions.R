# Functions

# Function to separate pixels by above or below the regional mean annual precip----

above_below_map <- function(x){
  
  mean.mm <- round(mean(x$mm),digits=1)
  below <- x %>% dplyr::filter(mm < mean.mm)
  below$map <- 'below'
  above <- x %>% dplyr::filter(mm > mean.mm)
  above$map <- 'above'
  above_below <-rbind(above,below)
  
  return(above_below)
  
}

# Produce data frame from model coefficients ----

df.coef.create<-function(x){
  df<-data.frame(x$coefficients)
  df$id = i
  
  return(df)
}

#
#

# Data frame from model AIC values ----

# Within-loop
df.aic.create<-function(x){
  df<-data.frame(AIC(x))
  df$id = i
  
  return(df)
}

# Post-loop
bind.aic<-function(x){
  
  x.binded <- do.call("rbind", x)
  colnames(x.binded)  <- c("aic","run.id")
  
  return(x.binded)
}

#
#

# Produce data frame from model BIC values -----

# Within-loop
df.bic.create<-function(x){
  df<-data.frame(BIC(x))
  df$id = i
  
  return(df)
}

# Post-loop
bind.bic<-function(x){
  
  x.binded <- do.call("rbind", x)
  colnames(x.binded)  <- c("bic","run.id")
  
  return(x.binded)
}

#
#

# Produce data frame from model r-squared values ----

# Within-loop
df.r.squared.create<-function(x){
  df<-data.frame(summary(x)$r.squared)
  df$id = i
  
  return(df)
}

# Post-loop
bind.r.squared<-function(x){
  
  x.binded <- do.call("rbind", x)
  colnames(x.binded)  <- c("r.squared","run.id")
  
  return(x.binded)
}

#
#

# Compare R2 values for full model to versions that Hold temporal and spatiotemporal components constant ----

#modified (in progress) version of original for different main effects...
compare_fit_2 <-function(model,data,metric,main){
  
  
  # # metric = "R2" or "RMSE"
  
  # Get predictions from full model (with space*time interaction)
  data$pred_intX <- predict(model)
  
  if(main=='diff'){
    
    
    #just spatial
    data$pred_s <- coef(model)[1] + coef(model)[2]*data$mm.y + coef(model)[4]*data$perc_herb_mean
    
    #spatial and temporal
    data$pred_st <- coef(model)[1] + coef(model)[2]*data$mm.y + coef(model)[3]*data$mm.dev +
      coef(model)[4]*data$perc_herb_mean
    
  }
  
  else{
    
    #just spatial
    data$pred_s <- coef(model)[1] + coef(model)[2]*data$mm.y
    
    #spatial and temporal   
    data$pred_st <- coef(model)[1] + coef(model)[2]*data$mm.y + coef(model)[3]*data$mm.dev
  }
  
  attach(data,warn.conflicts = FALSE)
  if(metric=="RMSE"){
    out <- c(sqrt(mean((npp.x-pred_intX)^2)),
             sqrt(mean((npp.x-pred_st)^2)),
             sqrt(mean((npp.x-pred_s)^2)))
  }else{
    out <- c(cor(npp.x,pred_intX,method="pearson")^2,
             cor(npp.x,pred_st,method="pearson")^2,
             cor(npp.x,pred_s,method="pearson")^2)
  }
  detach(data)
  
  names(out) <- c("intX","S&T","S")
  
  return(out)
  
}

# 95 confidence interval from a normal distribution
error.95 <-function(x) {
  n = length(x)
  se = sd(x)/sqrt(n)
  error <- qnorm(0.975)*se
  
  return(error)
}

#
#

# Remove repetitive code by selecting the new columns for each coefficient ----
select_columns <-function(x) { 
  
  regions<-subset(x,select=c('hot_deserts','cold_deserts','california_annuals','northern_mixed_prairies',
                             'shortgrass_steppe','run.id'))
  return(regions)
  
}

# Some initial cleanup of the lists
list_to_df_initial <- function(x) {
  
  df.coefficients <- do.call("rbind", x)
  df.coefficients.2 <- cbind(rownames(df.coefficients), data.frame(df.coefficients, row.names=NULL))
  
  #rename columns and get rid of row name junk
  colnames(df.coefficients.2)  <- c("predictor","coefficient","run.id")
  df.coefficients.2$predictor<-gsub('[[:digit:]]+', '', df.coefficients.2$predictor)
  df.coefficients.2$predictor<-gsub(':', '_', df.coefficients.2$predictor)
  df.coefficients.2$predictor<-gsub('mm.y', 'spatial', df.coefficients.2$predictor)
  df.coefficients.2$predictor<-gsub('perc_herb', 'herb', df.coefficients.2$predictor)
  df.coefficients.2$predictor<-gsub('mm.dev', 'temporal', df.coefficients.2$predictor)
  df2<-reshape(df.coefficients.2, idvar = "run.id", timevar = "predictor", direction = "wide")
  colnames(df2)[colnames(df2)=="coefficient.(Intercept)"] <- "intercept"
  rm(df.coefficients.2)
  return(df2)
  
  
}

# When ecoregion is not used as a covariate, use this one

list_to_df <- function(x,region=T) {
  
  df2<-list_to_df_initial(x)
  
  if(region==T){
    
    df2 <- df2 %>%
      select(run.id, coefficient.temporal_herb)
    return(df2)
    
  }else{
    
    return(df2)
  }
  
}

# When ecoregion is used as a covariate, use this function to get you
# straight to the final, long-form data frame

list_to_df_with_ecoregion <- function(x,veg=T,full=F) {
  
  #slightly ammended version to deal with mm.y and per_herb difference in model 1 and 3
  
  df2<-list_to_df_initial(x) #x
  
  #spatial slopes
  
  #california
  df2$california_annuals <- df2$coefficient.spatial
  #cold deserts
  df2$cold_deserts <- df2$coefficient.spatial + df2$coefficient.regioncold_deserts_spatial
  #hot_deserts
  df2$hot_deserts<-  df2$coefficient.spatial + df2$coefficient.regionhot_deserts_spatial
  #northern mixed
  df2$northern_mixed_prairies <- df2$coefficient.spatial  + df2$coefficient.regionnorthern_mixed_prairies_spatial
  #sgs
  df2$shortgrass_steppe <- df2$coefficient.spatial + df2$coefficient.regionshortgrass_steppe_spatial
  
  spatial_slopes<-select_columns(df2)
  
  #change to long format
  data_long_spatial <- gather(spatial_slopes, region, coefficient,-run.id, factor_key=TRUE)
  data_long_spatial$model<-'Spatial'
  
  #temporal slopes
  
  #california annuals
  df2$california_annuals <- df2$coefficient.temporal
  #cold deserts
  df2$cold_deserts <- df2$coefficient.temporal + df2$coefficient.temporal_regioncold_deserts 
  #hot_deserts
  df2$hot_deserts <-  df2$coefficient.temporal + df2$coefficient.temporal_regionhot_deserts 
  #northern mixed
  df2$northern_mixed_prairies <- df2$coefficient.temporal + df2$coefficient.temporal_regionnorthern_mixed_prairies
  #sgs
  df2$shortgrass_steppe <- df2$coefficient.temporal  + df2$coefficient.temporal_regionshortgrass_steppe
  
  temporal_slopes<-select_columns(df2)
  
  #change to long format
  
  data_long_temporal <- gather(temporal_slopes, region, coefficient,-run.id, factor_key=TRUE)
  data_long_temporal$model<-'Temporal'
  
  #merge the spatial and temporal coefficient dataframes
  rbind_spatial_temporal<-rbind(data_long_spatial,data_long_temporal)
  
  if(veg==T){
    # spatiotemporal interaction with %herb
    
    #california annuals
    df2$california_annuals<- df2$coefficient.temporal_herb
    #cold deserts
    df2$cold_deserts <- df2$coefficient.temporal_herb + df2$coefficient.temporal_regioncold_deserts_herb
    #hot_deserts
    df2$hot_deserts<-  df2$coefficient.temporal_herb + df2$coefficient.temporal_regionhot_deserts_herb
    #northern mixed
    df2$northern_mixed_prairies<- df2$coefficient.temporal_herb + df2$coefficient.temporal_regionnorthern_mixed_prairies_herb
    #sgs
    df2$shortgrass_steppe <- df2$coefficient.temporal_herb  + df2$coefficient.temporal_regionshortgrass_steppe_herb
    
    temporal_spatial_slopes<-select_columns(df2)
    
    #change to long format
    
    data_long_temporal_spatial <- gather(temporal_spatial_slopes, region, coefficient,-run.id, factor_key=TRUE)
    data_long_temporal_spatial$model<-'Spatiotemporal'
    
    #get %herb main effect
    
    df2$california_annuals<- df2$coefficient.herb
    #cold deserts
    df2$cold_deserts <- df2$coefficient.herb + df2$coefficient.regioncold_deserts_herb
    #hot_deserts
    df2$hot_deserts<-  df2$coefficient.herb + df2$coefficient.regionhot_deserts_herb
    #northern mixed
    df2$northern_mixed_prairies<- df2$coefficient.herb + df2$coefficient.regionnorthern_mixed_prairies_herb
    #sgs
    df2$shortgrass_steppe <- df2$coefficient.herb  + df2$coefficient.regionshortgrass_steppe_herb
    
    herb_spatial_slopes<-select_columns(df2)
    
    #change to long format
    
    data_long_herb <- gather(herb_spatial_slopes, region, coefficient,-run.id, factor_key=TRUE)
    data_long_herb$model<-'%Herb'
    
    data_long_temporal_spatial<-rbind(data_long_herb,data_long_temporal_spatial)
    
  }else{if(full==F){
    
    # spatiotemporal interaction with MAP
    
    #california annuals
    df2$california_annuals<- df2$coefficient.temporal_spatial
    #cold deserts
    df2$cold_deserts <- df2$coefficient.temporal_spatial + df2$coefficient.temporal_regioncold_deserts_spatial
    #hot_deserts
    df2$hot_deserts<-  df2$coefficient.temporal_spatial + df2$coefficient.temporal_regionhot_deserts_spatial
    #northern mixed
    df2$northern_mixed_prairies<- df2$coefficient.temporal_spatial + df2$coefficient.temporal_regionnorthern_mixed_prairies_spatial
    #sgs
    df2$shortgrass_steppe <- df2$coefficient.temporal_spatial  + df2$coefficient.temporal_regionshortgrass_steppe_spatial
    
    temporal_spatial_slopes<-select_columns(df2)
    
    #change to long format
    data_long_temporal_spatial <- gather(temporal_spatial_slopes, region, coefficient,-run.id, factor_key=TRUE)
    data_long_temporal_spatial$model<-'Spatiotemporal'
    
  }else{
    df2$coefficient.temporal_region
    #california annuals
    df2$california_annuals<- df2$coefficient.temporal_regioncalifornia_annuals_spatial
    #cold deserts
    df2$cold_deserts <- df2$coefficient.temporal_regioncold_deserts_spatial
    #hot_deserts
    df2$hot_deserts<-   df2$coefficient.temporal_regionhot_deserts_spatial
    #northern mixed
    df2$northern_mixed_prairies<- df2$coefficient.temporal_regionnorthern_mixed_prairies_spatial
    #sgs
    df2$shortgrass_steppe <-  df2$coefficient.temporal_regionshortgrass_steppe_spatial
    
    temporal_spatial_slopes<-select_columns(df2)
    
    #change to long format
    data_long_temporal_spatial <- gather(temporal_spatial_slopes, region, coefficient,-run.id, factor_key=TRUE)
    data_long_temporal_spatial$model<-'Spatiotemporal'
    
  }}
  
  #merge the spatial and temporal coefficient dataframes
  rbind_spatial_temporal_spatiotemporal<-rbind(data_long_temporal_spatial,rbind_spatial_temporal)
  
  # intercepts 
  
  #california annuals
  df2$california_annuals <- df2$intercept
  #cold deserts
  df2$cold_deserts <- df2$intercept + df2$coefficient.regioncold_deserts
  #hot_deserts
  df2$hot_deserts <-  df2$intercept  + df2$coefficient.regionhot_deserts
  #northern mixed
  df2$northern_mixed_prairies <- df2$intercept  + df2$coefficient.regionnorthern_mixed_prairies
  #sgs
  df2$shortgrass_steppe<- df2$intercept + df2$coefficient.regionshortgrass_steppe
  
  vegetation_intercepts<-select_columns(df2)
  
  #change to long format
  data_long_vegetation_intercepts <- gather(vegetation_intercepts, region, coefficient,-run.id, factor_key=TRUE)
  data_long_vegetation_intercepts$model<-'Intercept'
  
  #all coefficient for each veg type
  coefficients_full<-rbind(data_long_vegetation_intercepts,rbind_spatial_temporal_spatiotemporal)
  
  #for wide format
  coefficients_wide<- spread(coefficients_full, model, coefficient)
  
  #region_map<-aggregate(mm.x~region,mean,data=rangeland_npp_covariates)
  region_map<-aggregate(mm.y~region,mean,data=rangeland_npp_covariates) #either way averaging works out same
  region_map$map<-round(region_map$mm.y,1)
  region_map<-region_map[-c(2)]
  
  coefficients_wide_map<-merge(coefficients_wide,region_map,by=c('region'))
  
  
  return(coefficients_wide_map)
  
}



# Plot the sensitivity maps, relative to empirical, per-pixel slope values----
plot_sensitivity<-function(x,title){
  
  #use color gradient/range from empirical data for all maps so all are ons ame scale
  bks<- quantile(sensitivity_conus_coef_only$coef, probs=seq(0, 1, by=0.05), na.rm = TRUE)
  sensitivity=c("purple",'cyan3','green','yellow','orange','red')
  bkcols.sensitivity <- colorRampPalette(sensitivity)(length(bks)-1)
  r.range.sens <- round(c(minValue(sensitivity_raster), maxValue(sensitivity_raster)),digits=2)
  
  #update projection
  proj4string(x) <- CRS("+proj=longlat")
  sensitivity_raster_2<-projectRaster(x, crs=aea.proj)
  
  plot(sensitivity_raster_2,breaks = bks,axes=F,box=F,col = bkcols.sensitivity,legend=TRUE,
       legend.width=1, legend.shrink=.75,main=title,cex.main=1,
       axis.args=list(at=seq(r.range.sens[1], r.range.sens[2], 0.1),
                      labels=seq(r.range.sens[1], r.range.sens[2], 0.1),
                      cex.axis=1.0),
       legend.args=list(text='', side=4, font=10, line=2.5, cex=0.7))
  plot(states_all_sites,add=TRUE,lwd = 1)
  
}

addTrans <- function(color,trans)
{
  # This function adds transparancy to a color.
  # Define transparancy with an integer between 0 and 255
  # 0 being fully transparant and 255 being fully visable
  # Works with either color and trans a vector of equal length,
  # or one of the two of length 1.
  
  if (length(color)!=length(trans)&!any(c(length(color),length(trans))==1)) stop("Vector lengths not correct")
  if (length(color)==1 & length(trans)>1) color <- rep(color,length(trans))
  if (length(trans)==1 & length(color)>1) trans <- rep(trans,length(color))
  
  num2hex <- function(x)
  {
    hex <- unlist(strsplit("0123456789ABCDEF",split=""))
    return(paste(hex[(x-x%%16)/16+1],hex[x%%16+1],sep=""))
  }
  rgb <- rbind(col2rgb(color),trans)
  res <- paste("#",apply(apply(rgb,2,num2hex),2,paste,collapse=""),sep="")
  return(res)
}