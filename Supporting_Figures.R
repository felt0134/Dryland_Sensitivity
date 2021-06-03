
# Supporting Figures of AIC and vegetation cover by ecoregion

veg_names <- c(
  `hot_deserts` = "Hot deserts",
  `cold_deserts` = "Cold deserts",
  'northern_mixed_prairies' = "Northern mixed",
  `shortgrass_steppe` = "Shortgrass steppe",
  `california_annuals` = "California annuals"
)

# AIC comparisons #

# Plot and save
png(file='Tables_Figures/Supporting/aic_distributions.png',
    width=1200,height=600,res=150)
aic.plot<-ggplot(aic.binded,aes(x=aic ,color=model)) +
  scale_y_continuous(expand = c(0,0),limits = c(0,1.02)) +
  scale_x_continuous(expand = c(0,0),limits = c(161900,192950)) +
  geom_density(alpha=1,size=1,aes(y=..scaled..)) +
  scale_colour_manual(name = 'Spatiotemporal interaction',
                      values=c('ecoregion.map' = 'black',
                               'only.herb'='red',
                               'ecoregion.herb'='blue',
                               'only.map' = 'darkgrey',
                               'only.ecoregion' = 'gold'
                      ),
                      labels=c('ecoregion.map'='MAP x Dev x Ecoregion',
                               'only.herb'='%Herb x Dev',
                               'ecoregion.herb'='%Herb x Dev x Ecoregion',
                               'only.map' = 'MAP x Dev',
                               'only.ecoregion' = 'Ecoregion x Dev')) +
  xlab('AIC') +
  ylab('Probability density') +
  theme(
    axis.text.x = element_text(color='black',size=18), 
    axis.text.y = element_text(color='black',size=18),
    axis.title = element_text(color='black',size=25),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_text(size=14),
    legend.text = element_text(size=12),
    legend.position = c(0.55,0.6),
    #legend.position = 'top'5
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

print(aic.plot)

dev.off()

# Vegetation cover distributions

# % woody and herb cover distributions 

# Prep #if not already run
mean_ppt<-aggregate(mm.y~x+y+region,mean,data=rangeland_npp_covariates) 
fractional_cover_herb_woody<-readRDS(('Data/Fractional_cover_Western_US.rds'))

# Get means by functional groups...
fractional_cover_herb_woody<-fractional_cover_herb_woody[-c(3)] %>% #-3 remove year factor column...
  group_by(x,y) %>%
  summarise_all(mean)

#merge and round
fractional_cover_herb_woody<-merge(data.frame(fractional_cover_herb_woody),mean_ppt,by=c('x','y'))
fractional_cover_herb_woody$tree <-round(fractional_cover_herb_woody$tree ,2)
fractional_cover_herb_woody$shrub <-round(fractional_cover_herb_woody$shrub,2)
fractional_cover_herb_woody$annual_grass_forb <-round(fractional_cover_herb_woody$annual_grass_forb,2)
fractional_cover_herb_woody$perennial_grass_forb <-round(fractional_cover_herb_woody$perennial_grass_forb,2)
fractional_cover_herb_woody$herb_cover <-round(fractional_cover_herb_woody$herb_cover,2)
fractional_cover_herb_woody$woody_cover <-round(fractional_cover_herb_woody$woody_cover,2)

# Make and save plots

# & Herabceous cover #

png(file='Tables_Figures/Supporting/herb_cover.png',
    width=1200,height=600,res=150)

herb_plot<-ggplot(fractional_cover_herb_woody,aes(x=herb_cover,fill=region)) +
  facet_wrap(~region,scales = "free",labeller = as_labeller(veg_names)) +
  scale_y_continuous(expand = c(0,0),limits = c(0,1.02)) +
  geom_density(color='black',alpha=0.75,aes(y=..scaled..)) +
  scale_fill_manual(values=c('shortgrass_steppe'='green4','northern_mixed_prairies'='lightblue',
                             california_annuals='grey',cold_deserts='gold',hot_deserts='firebrick3'),
                    labels=c('shortgrass_steppe'='Shortgrass steppe','northern_mixed_prairies'='Northern mixed prairies',
                             california_annuals='California annuals',cold_deserts='Cold deserts',hot_deserts='Hot deserts')) +
  ylab('Probability density') +
  xlab('% Herbaceous cover') +
  theme(
    axis.text.x = element_text(color='black',size=12), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=12),
    axis.title = element_text(color='black',size=25),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size=17),
    legend.position = "none",
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

print(herb_plot)

dev.off()


# % woody cover

png(file='Tables_Figures/Supporting/woody_cover.png',
    width=1200,height=600,res=150)

woody_plot<-ggplot(fractional_cover_herb_woody,aes(x=woody_cover,fill=region)) +
  facet_wrap(~region,scales = "free",labeller = as_labeller(veg_names)) +
  scale_y_continuous(expand = c(0,0),limits = c(0,1.02)) +
  geom_density(color='black',alpha=0.75,aes(y=..scaled..)) +
  scale_fill_manual(values=c('shortgrass_steppe'='green4','northern_mixed_prairies'='lightblue',
                             california_annuals='grey',cold_deserts='gold',hot_deserts='firebrick3'),
                    labels=c('shortgrass_steppe'='Shortgrass steppe','northern_mixed_prairies'='Northern mixed prairies',
                             california_annuals='California annuals',cold_deserts='Cold deserts',hot_deserts='Hot deserts')) +
  ylab('Probability density') +
  xlab('% Woody cover') +
  theme(
    axis.text.x = element_text(color='black',size=12), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=12),
    axis.title = element_text(color='black',size=25),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size=17),
    legend.position = "none",
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

print(woody_plot)

dev.off()

fractional_cover_herb_woody<-fractional_cover_herb_woody[-c(1,2,10)]

#finally, make some tables of this

# Mean
fractional_cover_summary_mean <-fractional_cover_herb_woody %>%
  dplyr:: group_by(region) %>%
  dplyr:: summarise_if(is.numeric,error.95)
fractional_cover_summary_mean<-data.frame(fractional_cover_summary_mean)
fractional_cover_summary_mean$stat<-'mean'

# 95% confidence interval
fractional_cover_summary_ci <-fractional_cover_herb_woody %>%
  dplyr:: group_by(region) %>%
  dplyr:: summarise_if(is.numeric,error.95)
fractional_cover_summary_ci<-data.frame(fractional_cover_summary_ci)
fractional_cover_summary_ci$stat<-'95ci'

# Bind them
fractional_cover_summary_final <- rbind(fractional_cover_summary_ci,fractional_cover_summary_mean,
                                        by=c('stat'))

# Cleanup
rm(fractional_cover_summary_ci,fractional_cover_summary_mean)

write.csv(fractional_cover_summary_final,
          file = 'Tables_Figures/Supporting/vegetation_cover_summary.csv')



