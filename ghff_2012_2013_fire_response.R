###################################
# Response of GHFF roost occupation 
# to 2012-2013 fires
# Queensland roost data publicly available:
# https://www.data.qld.gov.au/dataset/flying-fox-monitoring-program
###################################

###################################
# libraries 
library(glmmTMB)
library(scales)
library(ggplot2)
library(DHARMa)
library(lme4)
library(dplyr)

###################################
# import dataset with presence/absence and covariates
# checking and defining columns vectors
locdir = '~/Documents/workspace/aus20192020_fires'
ff_all = read.csv(file.path(locdir, 'output/complete_covariate_df_20122013_md200414.csv'), header = T)
#ff_all = read.csv('ff_covariate_df_20122013.csv', header = TRUE)

sum(is.na(ff_all$season)) #checking for NAs
sum(is.na(ff_all$ghff.count))

ff_all$date = as.Date(ff_all$date)
ff_all$year = format(ff_all$date, format = '%Y')
ff_all$julian = as.numeric(strftime(ff_all$date, format="%j"))

#visualizing counts; warning: not in temporal order
ggplot(ff_all, aes(julian, ghff.count)) +
        geom_point(pch = 19, alpha = 0.5) +
        facet_wrap(~fire_rel, ncol = 1)+
        scale_y_log10()+
        theme_bw()

##########################################################
# rescaling variables 
ff_all$wint.hab.burn.rsc = rescale(ff_all$wint.hab.burn)
hist(ff_all$wint.hab.burn.rsc)

ff_all$tot.hab.burn.rsc= rescale(ff_all$tot.hab.burn)
hist(ff_all$tot.hab.burn.rsc)

ff_all$wint.hab.rsc = rescale(ff_all$wint.hab)
hist(ff_all$wint.hab.burn)

ff_all$tot.hab.rsc= rescale(ff_all$tot.hab)
hist(ff_all$tot.hab.burn.rsc)

ff_all$dist.fire.m.rsc= rescale(ff_all$dist.fire.m)
hist(ff_all$dist.fire.m.rsc)

ff_all$days_fire = ifelse(ff_all$rel_fire == 'pre', 
                          ff_all$date - as.Date('2011-09-01'),
                          ff_all$date - as.Date('2013-03-01'))
range(ff_all$days_fire)

##########################################################
# summarizing each roost
# identifying satellite roosts (occupied part of the time)
ghff_summary = as.data.frame(ff_all %>%
                                     group_by(camp.name) %>%
                                     summarise(occupied = sum(ghff.presence),
                                               sample_n = length(ghff.presence),
                                               mean = mean(ghff.count),
                                               sd = sd(ghff.count)))
range(ghff_summary$sample_n) #4 to 26 observations per roost
ghff_summary$prop_occ = ghff_summary$occupied/ghff_summary$sample_n

# identifying permanently occupied and unoccupied roosts
omit = ghff_summary[ghff_summary$prop_occ == 0 |ghff_summary$prop_occ == 1,]
sum(omit$prop_occ == 0) # 29 roosts
mean(omit[omit$prop_occ == 0,'sample_n']) #checked on avg 12.8 times
sum(omit$prop_occ == 1) # 11 roosts permanently occupied
mean(omit[omit$prop_occ == 1,'sample_n']) #checked on avg 16.3 times
outliers = omit$camp.name 

perm_camps = ghff_summary[ghff_summary$prop_occ == 1,]
perm_camps_loc = merge(perm_camps, ff_loc, all.x = TRUE)
#write.csv(perm_camps_loc, 'output/permanent_ghff_camps_2012_2014.csv', row.names = FALSE)
# permanent roosts primarily in urban areas

ff_trim = ff_all[!(ff_all$camp.name %in% outliers),] # removes 550 observations 

##############
# cleaning and defining new variables
ff_trim$rel_fire = factor(ff_trim$rel_fire, levels = c('pre', 'post'))
ff_trim$bff.presence = as.logical(ff_trim$bff.presence)
ff_trim$month = as.factor(format(ff_trim$date, format = '%m'))
ff_trim$year = as.factor(format(ff_trim$date, format = '%Y'))
ff_trim$month_year = as.factor(paste0(ff_trim$year, ff_trim$month))


means <- ff_trim %>% # means season and relative fire
        group_by(season, camp.name, rel_fire) %>% 
        dplyr::summarise(mean.count = mean(ghff.count, na.rm = TRUE))
means$occ = ifelse(means$mean.count>0,1,0)

#####################
# construction univariate logistic mixed effects models 
# for occupation of GHFF
ghffout.glmmtmb0 = glmmTMB(ghff.presence~ 1,
                           data = ff_trim,
                           family = binomial)

ghffout.glmmtmb = glmmTMB(ghff.presence~ + (1 | camp.name),
                       data = ff_trim,
                       family = binomial)
summary(ghffout.glmmtmb)
anova(ghffout.glmmtmb0, ghffout.glmmtmb)

ghffout.glmmtmb1 = glmmTMB(ghff.presence~ rel_fire +  (1 | camp.name),
                        data = ff_trim,
                        family = binomial)
summary(ghffout.glmmtmb1) # 0.2612*

ghffout.glmmtmb2 = glmmTMB(ghff.presence~ season + (1 | camp.name),
                        data = ff_trim,
                        family = binomial)
summary(ghffout.glmmtmb2)

ghffout.glmmtmb3 = glmmTMB(ghff.presence~ wint.hab.rsc + (1 | camp.name),
                        data = ff_trim,
                        family = binomial)
summary(ghffout.glmmtmb3)

ghffout.glmmtmb4 = glmmTMB(ghff.presence~ tot.hab.burn.rsc + (1 | camp.name),
                           data = ff_trim,
                           family = binomial)
summary(ghffout.glmmtmb4)

ghffout.glmmtmb4 = glmmTMB(ghff.presence~ prop.tot.burn + (1 | camp.name),
                           data = ff_trim,
                           family = binomial)
summary(ghffout.glmmtmb4)

ghffout.glmmtmb5 = glmmTMB(ghff.presence~ bff.presence + (1 | camp.name),
                           data = ff_trim,
                           family = binomial)
summary(ghffout.glmmtmb5)

ghffout.glmmtmb6 = glmmTMB(ghff.presence~ days_fire + (1 | camp.name),
                           data = ff_trim,
                           family = binomial)
summary(ghffout.glmmtmb6)

ghffout.glmmtmb7 = glmmTMB(ghff.presence~ wint.hab.rsc + (1 | camp.name),
                           data = ff_trim,
                           family = binomial)
summary(ghffout.glmmtmb7)

ghffout.glmmtmb8 = glmmTMB(ghff.presence~ month + (1 | camp.name),
                           data = ff_trim,
                           family = binomial)
summary(ghffout.glmmtmb8)

ghffout.glmmtmb9 = glmmTMB(ghff.presence~ lrff.presence + (1 | camp.name),
                           data = ff_trim,
                           family = binomial)
summary(ghffout.glmmtmb9)

ghffout.glmmtmb10 = glmmTMB(ghff.presence~ lat + (1 | camp.name),
                           data = ff_trim,
                           family = binomial)
summary(ghffout.glmmtmb10)

ghffout.glmmtmb11 = glmmTMB(ghff.presence~ prop.wint.burn + (1 | camp.name),
                            data = ff_trim,
                            family = binomial)
summary(ghffout.glmmtmb11)

# checking residuals of univariate model
simulation_uni <- simulateResiduals(fittedModel = ghffout.glmmtmb10, plot = T, n = 1000)
residuals(simulation_uni)
testResiduals(simulation_uni)
autocorrel_uni = ff_trim[,c('lat','long', 'date', 'rel_fire', 'season')]
autocorrel_uni$resid = residuals(simulation_uni)
ggplot() +
        geom_point(data = autocorrel_uni, mapping = aes(x = long, y = lat, col = resid)) +
        coord_quickmap() + 
        facet_grid(rel_fire~season) +
        theme_bw()

simulation_uni_camp = recalculateResiduals(simulation_uni, group = ff_trim$camp.name)
hist(simulation_uni_camp)


#####################
# construction of multivariate logistic mixed effects models 
ghffout.m1 = glmmTMB(ghff.presence~ wint.hab.rsc + season + bff.presence + (1 | camp.name),
                            data = ff_trim,
                            family = binomial)
summary(ghffout.m1)

ghffout.m2 = glmmTMB(ghff.presence~ prop.wint.burn + season + bff.presence + (1 | camp.name),
                     data = ff_trim,
                     family = binomial)
summary(ghffout.m2)

ghffout.m3 = glmmTMB(ghff.presence~ tot.hab.rsc + season + bff.presence + (1 | camp.name),
                     data = ff_trim,
                     family = binomial)
summary(ghffout.m3)

ghffout.m3 = glmmTMB(ghff.presence~  rel_fire + wint.hab.rsc+ season + bff.presence + (1 | camp.name),
                     data = ff_trim,
                     family = binomial)
summary(ghffout.m3)

ghffout.m3 = glmmTMB(ghff.presence~  rel_fire + tot.hab.rsc+ season + bff.presence + (1 | camp.name),
                     data = ff_trim,
                     family = binomial)
summary(ghffout.m3)
confid_df = as.data.frame(confint(ghffout.m3,parm="beta_",method="Wald"))
aor_df = exp(confid_df)
print(aor_df)

ghffout.m4 = glmmTMB(ghff.presence~  lat + tot.hab.rsc+ season + bff.presence + (1 | camp.name),
                     data = ff_trim,
                     family = binomial)
summary(ghffout.m4)

ghffout.m5 = glmmTMB(ghff.presence~  lat + tot.hab.rsc+ season + bff.presence + (1 | camp.name),
                     data = ff_trim,
                     family = binomial)
summary(ghffout.m5)

simulation_multi <- simulateResiduals(fittedModel = ghffout.m3, plot = T)
autocorrel_multi = ff_trim[,c('lat','long', 'date', 'rel_fire', 'season')]
autocorrel_multi$resid = residuals(simulation_multi)
ggplot() +
        geom_point(data = autocorrel, mapping = aes(x = long, y = lat, col = resid)) +
        coord_quickmap() + 
        facet_grid(rel_fire~season) +
        theme_bw()
