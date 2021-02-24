############
# Script to visualize fire and forest impacts
# (correspond to figures 1 & 2 in main text)

###########
# libraries
library(ggplot2)
library(scales)
library(dplyr)

##########
# Figure 1 [Total burned area & distribution of fire sizes]

# Figure 1a
fires = read.csv("data/Fire_metrics_2012_2020_long.csv", header =TRUE)
ggplot(fires, aes(y= area/1000000, x = season, fill = size)) + 
  geom_bar(stat ='identity', position = 'stack') +
  theme_classic(base_size = 14)+
  scale_fill_grey(start = 0.25, end = 0.75, name ='fire size',
                  labels = c('large (>10,000 ha)', 'moderate (>100 ha)', 'small (<100 ha)')) +
  labs(x = 'fire season', y = 'burned area (Mha)') +
  theme(axis.text.x = element_text(angle = 50, hjust = 1))+
  scale_y_continuous(limits=c(0,9), breaks = seq(0,9, by =1), expand = c(0, 0))+
  expand_limits(y = 0)

# Figure 1b
yr_cols = c("#00A08A", "#F2AD00")
all = read.csv("output/fire_size_distribution.csv", header =TRUE)

hec100_top = all[all$value == 116.16 & all$year =='2012-2013', 'prop']
hec100_bottom = all[all$value == 116.16 & all$year =='2019-2020', 'prop']
hec10000_top = all[all$value == 10125.28 & all$year =='2012-2013', 'prop']
hec10000_bottom = all[all$value == 10086.56 & all$year =='2019-2020', 'prop']

max_2019 = max(all[all$year =='2019-2020', 'value'])
max_2012 = max(all[all$year =='2012-2013', 'value'])

sub_19 = subset(all, all$value > 10086.56 & all$year =='2019-2020')
sub_19 = sub_19[,c('value','prop')] 
shade_19 = rbind(sub_19,
                  cbind('value' = rev(sub_19$value), 'prop' = rep(1.0, nrow(sub_19))),
                  cbind('value' = rep(10086.56, nrow(sub_19)), 'prop' = rev(sub_19$prop)))
shade_19$year = '2019-2020'

sub_12 = subset(all, all$value > 10086.56 & all$year =='2012-2013')
sub_12 = sub_12[,c('value','prop')] 
shade_12 = rbind(sub_12,
                  cbind('value' = rev(sub_12$value), 'prop' = rep(1.0, nrow(sub_12))),
                  cbind('value' = rep(10086.56, nrow(sub_12)), 'prop' = rev(sub_12$prop)))
shade_12$year = '2012-2013'

adj = 0.1
ggplot(all, aes(x = prop, y = value, color = year)) +  
  geom_polygon(data = shade_19, aes(prop, value), fill = alpha(yr_cols[2],0.2), lwd = 0) +
  geom_polygon(data = shade_12, aes(prop, value), fill = "white", lwd = 0) +
  geom_polygon(data = shade_12, aes(prop, value), fill = alpha(yr_cols[1],0.2), lwd = 0) +
  geom_segment(aes(y=117, yend=117, x=0, xend= hec100_top),
               linetype='dashed', size=0.5, color = 'gray') +
  geom_segment(aes(y=0, yend=117, x=hec100_top, xend= hec100_top),
               linetype='dashed', size=0.5, color = 'gray') +
  geom_segment(aes(y=0, yend=117, x=hec100_bottom, xend= hec100_bottom),
               linetype='dashed', size=0.5, color = 'gray') +
  geom_segment(aes(y=10086.56, yend=10086.56, x=0, xend= hec10000_top),
               linetype='dashed', size=0.5, color = 'gray') +
  geom_segment(aes(y=0, yend=10086.56, x=hec10000_top, xend= hec10000_top),
               linetype='dashed', size=0.5, color = 'gray') +
  geom_segment(aes(y=0, yend=10086.56, x=hec10000_bottom, xend= hec10000_bottom),
               linetype='dashed', size=0.5, color = 'gray') +
  geom_step(size = 1.3) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_log10( breaks = trans_breaks("log10", function(x) 10^x),
                 labels = trans_format("log10", math_format(10^.x))) +
  scale_color_manual(values = yr_cols)+
  labs(y ="size of fire (hectares)", x = "percentage of total area burned", color = "fire season") +
  theme_classic(base_size = 15) +
  annotate("text", x = hec10000_bottom+adj, y = 12000,
           label = paste0(100-round(hec10000_bottom*100,1),'%'), col = yr_cols[2] ) +
  annotate("text", x = hec10000_top+adj, y = 12000 ,
           label = paste0(100-round(hec10000_top*100,1),'%'), col = yr_cols[1] ) +
  annotate("text", x = 0.05, y = 15000,
           label = paste('large fires'), col = 'gray' ) +
  annotate("text", x = 0.05, y = 200 ,
           label = paste('moderate\nfires'), col = 'gray') +
  theme(legend.position = c(0.15, 0.85))

######
# Figure 2 [Extent and forest impacts in focal years]

# Figure 2c
focal_burned = read.csv('data/focal_burned_stats.csv', header = TRUE)

ggplot(data = focal_burned, aes(y=BurnedArea_Mha, x = Year, fill = Vegetation, color = Vegetation)) +
  geom_bar(position = "stack", stat = "identity") +
  theme_bw() +
  ylab("total burned area (Mha)") +
  xlab("fire season") +
  scale_fill_manual(values = c("darkgray", "white")) +
  scale_color_manual(values = c("black", "black")) +
  theme_classic(base_size = 16)

# Figure 2d

patches_wide = read.csv('data/large_forests_summary_wide.csv', header = TRUE)
patches_wide$bin_relev3 = as.factor(patches_wide$bin_relev3)
patches_wide$bin_relev3 = factor(patches_wide$bin_relev3,
                                   levels = c('(500,5e+03]', '(5e+03,5e+04]',
                                              '(5e+04,5e+05]', '(5e+05,1e+06]', '(1e+06, Inf]'))

pos1 = position_dodge(width = 0.5)
ggplot(patches_wide,aes(x=bin_relev3,y=propdiff, color =fire_sea))+
  geom_point(shape = 1,size=4, lwd = 3, position = pos1, stroke = 2)+
  geom_line()+
  scale_y_continuous(limits=c(-1.0,1.0), breaks = seq(-1,1, by =0.1),
                     labels = c('-1','','-0.8','','-0.6', '','-0.4','','-0.2','','0','',
                                '0.2','','0.4','','0.6','','0.8','','1.0')) +
  scale_color_manual(values = yr_cols, name = 'fire season')+
  geom_segment(aes(y=0, yend=0, x=-Inf, xend= Inf),
               size=0.6, color = 'black') +
  scale_x_discrete(labels= c('(500,\n5000]', '(5000,\n50000]', '(50000,\n500000]', 
                             '(500000,\n1000000]','>1000000'))+
  labs(x = 'forest patch size (hectares)', y = 'change in forest area post-fire') +
  theme_classic(base_size = 15)+
  theme(legend.position = c(0.2, 0.2))




