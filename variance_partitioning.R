# variance partitioning figures code

# Predicted versus fitted temporal sensitivity #

# FIRST GET PER-PIXEL SLOPES #

sensitivity_conus <- rangeland_npp_covariates %>% group_by(x, y) %>%
  dplyr::do(model = lm(npp.x~mm.dev, data = .)) %>%
  dplyr::mutate(coef=coef(model)[2])

sensitivity_conus_coef_only<- data.frame(sensitivity_conus[-c(3)])

# NOW GET ECOREGION MODEL SLOPES #

# Temporal slopes

df2<-list_to_df_initial(list.coefficients.ecoregion.null) 

# California annuals
df2$california_annuals <- df2$coefficient.temporal

# Cold deserts
df2$cold_deserts <- df2$coefficient.temporal + df2$coefficient.temporal_regioncold_deserts 

# Hot deserts
df2$hot_deserts <-  df2$coefficient.temporal + df2$coefficient.temporal_regionhot_deserts 

# Northern mixed prairies
df2$northern_mixed_prairies <- df2$coefficient.temporal + df2$coefficient.temporal_regionnorthern_mixed_prairies

# Shortgrass steppe
df2$shortgrass_steppe <- df2$coefficient.temporal  + df2$coefficient.temporal_regionshortgrass_steppe

temporal_slopes<-select_columns(df2)

# Change to long format

ecoregion_model <- gather(temporal_slopes, region, coefficient,-run.id, factor_key=TRUE)
#head(ecoregion_model)

# Model with ecoregion-specific interaction terms...
region.list<-c('shortgrass_steppe','northern_mixed_prairies','hot_deserts','cold_deserts','california_annuals')
coef.list.map<-list()

for(iRegion in 1:length(region.list)){
  
  ecoregion <- region.list[iRegion]
  
  # Climate model
  empirical.1<-subset(rangeland_npp_covariates,region==ecoregion)
  model.map<-subset(ecoregion_model,region==ecoregion)
  empirical.1$coef<-mean(model.map$coefficient) 
  slopes.map.st.model.region<-aggregate(coef~x+y,mean,data=empirical.1)
  coef.list.map[[iRegion]] <-slopes.map.st.model.region
  
}

ecoregion_model_2<-do.call("rbind",coef.list.map)
#head(ecoregion_model_2)

# Merge with empirical dataset #
ecoregion_empirical<-merge(sensitivity_conus_coef_only,ecoregion_model_2,by=c('x','y'))
head(ecoregion_empirical)

# Look at pearson's correlation coefficient #
cor(ecoregion_empirical$coef.x,ecoregion_empirical$coef.y,method = "pearson")

# Merge with ecorergion ID

# Just get site means so we have one value per site to merge with
rangeland_npp_covariates_mean<-aggregate(mm.y~x+y+region,mean,data=rangeland_npp_covariates)
#head(rangeland_npp_covariates_mean)
ecoregion_empirical <- merge(ecoregion_empirical,rangeland_npp_covariates_mean[c(1,2,3)],by=c('x','y'))
#head(ecoregion_empirical)


##


# MODEL WITH JUST MAP INTERACTION TERM #

map_coefficients <-list_to_df(list.coefficients.noveg,region=F)
map.model<-aggregate(mm.y~x+y,mean,data=rangeland_npp_covariates)

# Temporal slope at mean annual precipitation for each ecoregion
map.model$temporal_sensitivity <-
  mean(map_coefficients$coefficient.temporal) + 
  mean(map_coefficients$coefficient.spatial_temporal)*map.model$mm.y

# Merge with empirical data set
map_empirical<-merge(sensitivity_conus_coef_only,map.model,by=c('x','y'))
map_empirical <- merge(map_empirical,rangeland_npp_covariates_mean[c(1,2,3)],by=c('x','y'))
head(map_empirical)


# MODEL WITH JUST %HERB INTERACTION TERM #

herb_coefficients <-list_to_df(list.coefficients.herb.map,region=F)
herb.model<-aggregate(perc_herb_mean~x+y,mean,data=rangeland_npp_covariates)

# Temporal slope at mean annual precipitation for each ecoregion
herb.model$temporal_sensitivity <-
  mean(herb_coefficients$coefficient.temporal) + 
  mean(herb_coefficients$coefficient.temporal_herb_mean)*herb.model$perc_herb_mean

# Merge with empirical dataset
herb_empirical<-merge(sensitivity_conus_coef_only,herb.model,by=c('x','y'))
herb_empirical <- merge(herb_empirical,rangeland_npp_covariates_mean[c(1,2,3)],by=c('x','y'))

# Look at correlations #

# Ecoregion
cor(ecoregion_empirical$coef.x,ecoregion_empirical$coef.y,method = 'pearson')

# Climate
cor(map_empirical$coef,map_empirical$temporal_sensitivity,method = 'pearson')

# Herb
cor(herb_empirical$coef,herb_empirical$temporal_sensitivity,method = 'pearson')


# Plot all this out and save #

# Specify the color scheme for each ecoregion, corresponding with those in the maps #

sgs.cols <- sample(c("green4"),100,TRUE)
nmp.cols <- sample(c("steelblue2"),100,TRUE)
hd.cols <- sample(c("firebrick3"),100,TRUE)
cd.cols <- sample(c("gold"),100,TRUE)
ca.cols <- sample(c("grey"),100,TRUE)

# Multi-panel plot

#choose file format for saving

png('Tables_Figures/obs.predicted.model_highres.png',
    width=1400,height=1000,res=150)

# Panel setup
layout(matrix(1:3, ncol=3))
par(oma=c(6, 5, 6, 5), mar=c(0, 0, 0, 0),pty='s')

# Panel label setup
line = 0.5
cex = 1.5
side = 3
adj=-0.05


# Plot 1: map model predicted versus observed

sgs.map<-subset(map_empirical,region=='shortgrass_steppe')
nmp.map<-subset(map_empirical,region=='northern_mixed_prairies')
hd.map<-subset(map_empirical,region=='hot_deserts')
cd.map<-subset(map_empirical,region=='cold_deserts')
ca.map<-subset(map_empirical,region=='california_annuals')

plot(temporal_sensitivity~coef,data=nmp.map,cex=1.2,
     ylab='',xlab='',axes=F,
     main='',ylim=c(-.13,0.55),xlim=c(-.13,0.55),col=addTrans(nmp.cols,15),cex.lab=1.25)
axis(2)
axis(1)
box()
mtext('Predicted temporal sensitivity',side=2,line=2.5,cex=1.1)
mtext('Climate Model',side=3,line=0.75,cex=1)
mtext("A", side=side, line=line, cex=cex, adj=adj)

# Cold deserts
points(temporal_sensitivity~coef,data=cd.map,cex=1.2,col=addTrans(cd.cols,15))

# Shortgrass steppe
points(temporal_sensitivity~coef,data=sgs.map,cex=1.2,col=addTrans(sgs.cols,15))

# Hod deserts
points(temporal_sensitivity~coef,data=hd.map,cex=1.2,col=addTrans(hd.cols,5))

# California annuals
points(temporal_sensitivity~coef,data=ca.map,cex=1.2,col=addTrans(ca.cols,60))

# Make 1:1 line
abline(a=0,b=1,col='black',lwd=4)
text(0.02,-0.1,'1:1 line')

# Make legend
legend(-0.15, 0.55, legend=c("Hot deserts", "Cold deserts","California annuals",
                             "Northern mixed prairies","Shortgrass steppe"),
       col=c("firebrick3", "gold","grey","steelblue2","green4"), lty=1,lwd=2,cex=0.75,box.lty=0)
# dev.off()

# Herb model predicted versus observed

sgs.herb<-subset(herb_empirical,region=='shortgrass_steppe')
nmp.herb<-subset(herb_empirical,region=='northern_mixed_prairies')
hd.herb<-subset(herb_empirical,region=='hot_deserts')
cd.herb<-subset(herb_empirical,region=='cold_deserts')
ca.herb<-subset(herb_empirical,region=='california_annuals')

plot(temporal_sensitivity~coef,data=nmp.herb,cex=1.2,axes=F,
     ylab='',xlab='',
     main='',ylim=c(-.13,0.55),xlim=c(-.13,0.55),col=addTrans(nmp.cols,15),cex.lab=1.25)

axis(1)
box()
mtext('Observed temporal sensitivity',side=1,line=3,cex=1.1)
mtext('Herb Model',side=3,line=0.75,cex=1)
mtext("B", side=side, line=line, cex=cex, adj=adj)

# Cold deserts
points(temporal_sensitivity~coef,data=cd.herb,cex=1.2,col=addTrans(cd.cols,15))

# Shortgrass steppe
points(temporal_sensitivity~coef,data=sgs.herb,cex=1.2,col=addTrans(sgs.cols,15))

# Hod deserts
points(temporal_sensitivity~coef,data=hd.herb,cex=1.2,col=addTrans(hd.cols,5))

# California annuals
points(temporal_sensitivity~coef,data=ca.herb,cex=1.2,col=addTrans(ca.cols,60))
abline(a=0,b=1,col='black',lwd=4) 

# Predicted versus observed for ecoregion
sgs.ecoregion<-subset(ecoregion_empirical,region=='shortgrass_steppe')
nmp.ecoregion<-subset(ecoregion_empirical,region=='northern_mixed_prairies')
hd.ecoregion<-subset(ecoregion_empirical,region=='hot_deserts')
cd.ecoregion<-subset(ecoregion_empirical,region=='cold_deserts')
ca.ecoregion<-subset(ecoregion_empirical,region=='california_annuals')

plot(coef.y~coef.x,data=nmp.ecoregion,cex=1.2,axes=F,
     ylab='',xlab='',
     main='',ylim=c(-.13,0.55),xlim=c(-.13,0.55),col=addTrans(nmp.cols,15),cex.lab=1.25)
axis(1)
box()
mtext('Ecoregion Model',side=3,line=0.75,cex=1)
mtext("C", side=side, line=line, cex=cex, adj=adj)

# Cold deserts
points(coef.y~coef.x,data=cd.ecoregion,cex=1.4,col=addTrans(cd.cols,15))

# Shortgrass steppe
points(coef.y~coef.x,data=sgs.ecoregion,cex=1.2,col=addTrans(sgs.cols,15))

# Hod deserts
points(coef.y~coef.x,data=hd.ecoregion,cex=1.2,col=addTrans(hd.cols,5))

# California annuals
points(coef.y~coef.x,data=ca.ecoregion,cex=1,col=addTrans(ca.cols,60))
abline(a=0,b=1,col='black',lwd=4) 

dev.off()

# Sum of Squares and R-squared #

# Approach to NPP variance partition using sums of squares (SS)

new_order <- c("hot_deserts","cold_deserts","california_annuals","northern_mixed_prairies","shortgrass_steppe","total")
new_order_2 <- c("temporal",'spatial')
ss_region_list<-c("shortgrass_steppe","northern_mixed_prairies","hot_deserts","cold_deserts",           
                  "california_annuals")

# Entire NPP dataset
SStot <- var(rangeland_npp_covariates$npp.x)*(nrow(rangeland_npp_covariates)-1)
SSnonspatial <- sum((rangeland_npp_covariates$npp.dev)^2)
SSspatial <- SStot - SSnonspatial

# Look at what % of total SS the spatial SS is
SSspatial/SStot 
SSnonspatial/SStot

# Temporal SS
temporal_ss<-data.frame(SSnonspatial)
temporal_ss$region<-'total'
temporal_ss$source<-'temporal'
temporal_ss$perc_tempoal<- (SSnonspatial/SStot)*100
colnames(temporal_ss)<-c('ss','region','source','perc')

# Spatial SS
spatial_ss<-data.frame(SSspatial)
spatial_ss$region<-'total'
spatial_ss$source<-'spatial'
spatial_ss$perc_spatial<- (SSspatial/SStot)*100
colnames(spatial_ss)<-c('ss','region','source','perc')

# Bind them
spatial_temporal_ss_total<-rbind(spatial_ss,temporal_ss)

# Now do this by ecoregion 

ss_list<-list()

for(iregion in 1:length(ss_region_list)){
  
  do_region <- ss_region_list[iregion]
  
  keep <- which(rangeland_npp_covariates$region==do_region)
  SStot <- var(rangeland_npp_covariates$npp.x[keep])*(length(keep)-1)
  SSnonspatial <-sum((rangeland_npp_covariates$npp.dev[keep])^2)
  SSspatial <- SStot - SSnonspatial
  
  #temporal
  keep_temporal_ss<-data.frame(SSnonspatial)
  keep_temporal_ss$region<-do_region
  keep_temporal_ss$source<-'temporal'
  keep_temporal_ss$perc_temporal<- (SSnonspatial/SStot)*100
  colnames(keep_temporal_ss)<-c('ss','region','source','perc')
  
  #spatial
  keep_spatial_ss<-data.frame(SSspatial)
  keep_spatial_ss$region<-do_region
  keep_spatial_ss$source<-'spatial'
  keep_spatial_ss$perc_spatial<- (SSspatial/SStot)*100
  colnames(keep_spatial_ss)<-c('ss','region','source','perc')
  
  #bind them
  spatial_temporal_ss_keep<-rbind(keep_spatial_ss,keep_temporal_ss)
  
  
  ss_list[[iregion]]<-spatial_temporal_ss_keep
  
}

# Make the list into a data frame
ecoregion_ss<-do.call("rbind", ss_list)

# Merge with total SS data frame
ecoregion_ss_total <-rbind(ecoregion_ss,spatial_temporal_ss_total)

# Look at % difference between spatial and temporal SS
((ecoregion_ss_total[11,]$ss)-(ecoregion_ss_total[12,]$ss))/ecoregion_ss_total[12,]$ss

# Order it: ecoregions
ecoregion_ss_total_ordered <- arrange(mutate(ecoregion_ss_total,
                                             region=factor(region,levels=new_order)),region)

# Order it: source of variation
ecoregion_ss_total_ordered <- arrange(mutate(ecoregion_ss_total_ordered,
                                             source=factor(source,levels=new_order_2)),source)

# Plot it
ss.plot<-ggplot(ecoregion_ss_total_ordered,aes(x=region,y=perc,fill=source)) +
  scale_y_continuous(expand = c(0,0)) + #for bar plot
  scale_fill_manual(values=c('spatial'='grey','temporal'='blue'),
                    labels=c('temporal'='Temporal & Spatiotemporal','spatial'='Spatial')) +
  stat_summary(geom='bar',position="stack",fun='mean',color='black',size=0.25) +
  scale_x_discrete(breaks=c("california_annuals","cold_deserts","hot_deserts","northern_mixed_prairies",
                            "shortgrass_steppe","total"),
                   labels=c('California annuals', "Cold deserts", "Hot deserts",
                            "Northern mixed prairies","Shortgrass steppe","All ecoregions")) +
  ylab('Relativized sum of squares') +
  xlab('') +
  theme(
    axis.text.x = element_text(color='black',size=9, angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=9),
    axis.title = element_text(color='black',size=15),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size=8),
    legend.position = "top",
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=7),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

##


# R squared partitioning
  
# LOOK AT R-SQUARED #
  
# Ecoregion model
ecoregion.rsquare.binded <- do.call("rbind", compare.r.squared.region.list)
ecoregion.rsquare.binded<-data.frame(ecoregion.rsquare.binded)
ecoregion.rsquare.binded$model<-'ecoregion'

# Herb model
herb.rsquare.binded <- do.call("rbind", compare.r.squared.herb.list)
herb.rsquare.binded<-data.frame(herb.rsquare.binded)
herb.rsquare.binded$model<-'herb'

# Climate model
map.rsquare.binded <- do.call("rbind", compare.r.squared.map.list)
map.rsquare.binded<-data.frame(map.rsquare.binded)
map.rsquare.binded$model<-'map'

#combine all three
rsquare.comparisons<-rbind(ecoregion.rsquare.binded,herb.rsquare.binded,map.rsquare.binded)

#partition
rsquare.comparisons$temporal<-rsquare.comparisons$S.T - rsquare.comparisons$S
rsquare.comparisons$spatiotemporal<-rsquare.comparisons$intX - rsquare.comparisons$S.T
rsquare.comparisons$spatial<- rsquare.comparisons$S
rsquare.comparisons_2<-rsquare.comparisons[c(4:7)] #isolate columns before condensing

#change to long format
rsquare.comparisons_long <- gather(rsquare.comparisons_2, term, value, temporal:spatial, factor_key=TRUE)
head(rsquare.comparisons_long)
rsquare.comparisons_long$variance<-rsquare.comparisons_long$value*100
rsquare.comparisons_mean<-aggregate(variance~model + term,mean,data=rsquare.comparisons_long)

#re-order factors on x axis for plotting
rsquare.comparisons_long$model <- factor(rsquare.comparisons_long$model,
                                         levels = c("map", "herb","ecoregion"))

#get summary stats
rsquare.comparisons.mean<-aggregate(variance~model + term,mean,data=rsquare.comparisons_long)

#plot it
r.square.plot<-ggplot(rsquare.comparisons_long,aes(x=model,y=variance,fill=term)) +
  scale_y_continuous(expand = c(0,0),limits = c(0,55)) + #for bar plot
  scale_fill_manual(values=c('spatial'='grey','temporal'='blue',spatiotemporal='red'),
                    labels=c('spatial'='Spatial','temporal'='Temporal',
                             'spatiotemporal'='Spatiotemporal')) +
  stat_summary(geom='bar',position="stack",fun='mean',color='black',size=0.2)+
  scale_x_discrete(breaks=c("ecoregion","herb","map"),
                   labels=c('Ecoregion', "Herb", "Climate")) +
  ylab('Variation in NPP explained (%)') +
  xlab('Model') +
  theme(
    axis.text.x = element_text(color='black',size=9), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=9),
    axis.title = element_text(color='black',size=15),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size=8),
    legend.position = "top",
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=7),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

# For the plot_grid

png('Tables_Figures/npp_variation_ss_multipanel.png',
    width=1200,height=700,res=150)

print(plot_grid(r.square.plot, ss.plot, labels = c('A', 'B'),
          rel_widths = c(1, 1), label_size = 12))
dev.off()


#done


