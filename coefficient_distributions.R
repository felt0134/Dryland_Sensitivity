
# coefficient distributions for temporal sensitivity and
# changes in temporal sensitivity per mm of mean annual precipitation and 
# % herbaceous vegetation cover

# Temporal sensitivity among ecoregions #

ts<-ggplot(map_region_coefficients,aes(x=temporal_sensitivity_map,fill=region)) +
  scale_y_continuous(expand = c(0,0),limits = c(0,1.02)) +
  geom_density(color='black',alpha=0.75,aes(y=..scaled..)) +
  scale_fill_manual(values=c('shortgrass_steppe'='green4','northern_mixed_prairies'='lightblue',
                             california_annuals='grey',cold_deserts='gold',hot_deserts='firebrick3'),
                    labels=c('shortgrass_steppe'='Shortgrass steppe','northern_mixed_prairies'='Northern mixed prairies',
                             california_annuals='California annuals',cold_deserts='Cold deserts',hot_deserts='Hot deserts')) +
  xlab(bquote('Temporal sensitivity ('*g/m^2/mm*')')) +
  ylab('Probability density') +
  theme(
    axis.text.x = element_text(color='black',size=13), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=13),
    axis.title.x = element_text(color='black',size=16),
    axis.title.y = element_text(color='black',size=19),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size=8.25),
    legend.position = c(0.48,0.8),
    legend.margin =margin(r=5,l=5,t=5,b=5),
    #legend.position = 'none',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))


# change in sensitivity per mm map #

ts_map<-ggplot(map_region_coefficients,aes(x=Spatiotemporal,fill=region)) +
  scale_y_continuous(expand = c(0,0),limits = c(0,1.02)) +
  scale_x_continuous(limits = c(-0.00081,0.00061)) +
  geom_vline(xintercept=0,color='black',size=1,linetype='dashed') +
  geom_density(color='black',alpha=0.75,aes(y=..scaled..)) +
  scale_fill_manual(values=c('shortgrass_steppe'='green4','northern_mixed_prairies'='lightblue',
                             california_annuals='grey',cold_deserts='gold',hot_deserts='firebrick3'),
                    labels=c('shortgrass_steppe'='Shortgrass steppe','northern_mixed_prairies'='Northern mixed prairies',
                             california_annuals='California annuals',cold_deserts='Cold deserts',hot_deserts='Hot deserts')) +
  xlab('Change in sensitivity per mm MAP') +
  ylab('') +
  theme(
    axis.text.x = element_text(color='black',size=13), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=13),
    axis.title.x = element_text(color='black',size=16),
    axis.title.y = element_text(color='black',size=12),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size=8),
    #legend.position = c(0.15,0.7),
    legend.position = 'none',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

# change in sensitivity % herbaceous cover #

st_herb<-ggplot(herb_region_coefficients,aes(x=Spatiotemporal,fill=region)) +
  scale_y_continuous(expand = c(0,0),limits = c(0,1.02)) +
  scale_x_continuous(expand = c(0,0),limits = c(-.0033,0.0082)) +
  geom_vline(xintercept=0,color='black',size=1,linetype='dashed') +
  geom_density(color='black',alpha=0.6,aes(y=..scaled..)) +
  scale_fill_manual(values=c('shortgrass_steppe'='green4','northern_mixed_prairies'='lightblue',
                             california_annuals='grey',cold_deserts='gold',hot_deserts='firebrick3'),
                    labels=c('shortgrass_steppe'='Shortgrass steppe','northern_mixed_prairies'='Northern mixed prairies',
                             california_annuals='California annuals',cold_deserts='Cold deserts',hot_deserts='Hot deserts')) +
  xlab('Change in sensitivity per % herbaceous cover') +
  ylab('') +
  theme(
    axis.text.x = element_text(color='black',size=13), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=13),
    axis.title = element_text(color='black',size=16),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size=10),
    #legend.position = c(0.15,0.7),
    legend.position = 'none',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))


# make multi-panel figure

png('Tables_Figures/coefficient_distributions_figure.png',
    width=2550,height=700,res=150)

# pdf('Tables_Figures/coefficient_distributions_figure.pdf',
#     width=17.5,height=4)

print(plot_grid(ts, ts_map,st_herb,labels = c('A', 'B','C'),ncol = 3, nrow=1,
          rel_widths = c(1.5, 1.5,1.5), rel_heights = c(0.5, 0.5,0.5),label_size = 18))

dev.off()

#done


