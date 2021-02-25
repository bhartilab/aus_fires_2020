###########################################################
# Investigating fire impact on flying fox habitat and roost occupancy
# GAMs for focal fire seasons: 2012-2013; 2019-2020
##########################################################
# libraries
library(dplyr) #manipulating dfs
library(mgcv) #gams
library(ggplot2)

##########################################################
#importing data for proportion winter habitat
prop_2012 = read.csv("data/Prop_WintHabBurn440_2012.csv")
prop_2012$year =2012
names(prop_2012)
prop_2019 = read.csv("data/Prop_WintHabBurn440_2019.csv")
prop_2019$year = 2019
names(prop_2019)

ff_loc = read.csv("data/ff_roost_locations.csv", header = TRUE)

long_roosts = plyr::rbind.fill(prop_2012, prop_2019)
long_roosts_loc = merge(long_roosts,ff_loc,by.x = 'Camp',by.y = 'camp.name',  all.x = TRUE)
long_roosts_loc$year_fac = as.factor(long_roosts_loc$year)


# GAMMS
gam_mod1 <- gam(Prop_HabBurn440 ~ s(lat, by = year_fac), data = long_roosts_loc, family = betar(link="logit")) #, family = 'betar'
summary(gam_mod1)
coef(gam_mod1)
plot(gam_mod1, pages=1)
exp(gam_mod1$family$getTheta())
plot(gam_mod1, residuals = TRUE, pch = 1, pages=1)

gam_mod2 <- gam(Prop_HabBurn440 ~ s(lat, by = year_fac) + year_fac, data = long_roosts_loc, family = betar(link="logit")) #, family = 'betar'
summary(gam_mod2) #allows for different intercepts
coef(gam_mod2)
plot(long_roosts_loc$lat, fitted(gam_mod2))
long_roosts_loc$fit = fitted(gam_mod2)
print(gam_mod2)
plot(gam_mod2, pages=1)
exp(gam_mod2$family$getTheta())

plot.gam(gam_mod2, pages=1)
p <- predict(gam_mod2, long_roosts_loc, type = "link", se.fit = TRUE)
upr <- p$fit + (2 * p$se.fit)
lwr <- p$fit - (2 * p$se.fit)
upr <- gam_mod2$family$linkinv(upr)
lwr <- gam_mod2$family$linkinv(lwr)
long_roosts_loc$upr = upr
long_roosts_loc$lwr = lwr
# point CIs based on: https://stats.stackexchange.com/questions/33327/confidence-interval-for-gam-model/33328

yr_cols = 
range(long_roosts_loc$lat)
xbrks = seq(-37,-23, by = 1)
ybrks = seq(0,0.6, by = 0.1)
ggplot(long_roosts_loc, aes(x=lat, y=Prop_HabBurn440, color=year_fac))+
  geom_ribbon(aes(ymin=lwr, ymax=upr, color = year_fac), alpha=0.08, lty =0) +
  geom_point(alpha = 0.4) +
  geom_line(aes(x = lat, y = fit, color = year_fac), lwd = 1.5) + 
  geom_line(aes(x = lat, y = upr), lty =2) + 
  geom_line(aes(x = lat, y = lwr), lty =2) + 
  scale_x_continuous(breaks = xbrks) +
  scale_y_continuous(breaks = ybrks) +
  scale_color_manual(values=yr_cols, labels = c('typical', 'anomalous'))+
  coord_flip()+
  labs(fill = "Year", y = 'proportion winter habitat burned',
       x= 'latitude', color = 'fire season')+
  theme_classic(base_size = 14)+
  theme(legend.position = c(0.85, 0.15)) 

##########################################################
# FF total burned habitat 
hab_dir = "~/Box/Aus_fires_2020/Shapefile_outputs/New_Shapefile_Outputs/GHFF_Habitat_Tables/"
hab_2012 = read.csv(file.path(hab_dir,"2012_2013/Total_HabitatBurn440_2012.csv"))
hab_2012$year =2012
names(hab_2012)
hab_2012 = hab_2012[,c('Camp','Burn440_Area','year')]
names(hab_2012) <- c('camp.name', 'tot.hab.burn','year')

hab_2019 = read.csv(file.path(hab_dir,"2019_2020/Total_HabitatBurn440_2019.csv"))
hab_2019$year = 2019
names(hab_2019)
hab_2019 = hab_2019[,c('Camp','Burn440_Area','year')]
names(hab_2019) <- c('camp.name', 'tot.hab.burn','year')

ff_dir = '~/Documents/workspace/hendra_env/output/'
ff_loc = read.csv(file.path(ff_dir,"/ff_roost_locations.csv"), header = TRUE)

long_roosts = plyr::rbind.fill(hab_2012, hab_2019)
long_roosts_loc = merge(long_roosts,ff_loc,by = 'camp.name',  all.x = TRUE)
long_roosts_loc$year_fac = as.factor(long_roosts_loc$year)
# GAMMS
gam_mod1 <- gam(tot.hab.burn ~ s(lat, by = year_fac), data = long_roosts_loc) 
summary(gam_mod1)
coef(gam_mod1)
plot(gam_mod1, pages=1)
exp(gam_mod1$family$getTheta())
plot(gam_mod1, residuals = TRUE, pch = 1, pages=1)

gam_mod2 <- gam(tot.hab.burn ~ s(lat, by = year_fac) + year_fac, data = long_roosts_loc) #, family = 'betar'
summary(gam_mod2) #allows for different intercepts
coef(gam_mod2)
plot(long_roosts_loc$lat, fitted(gam_mod2))
long_roosts_loc$fit = fitted(gam_mod2)
print(gam_mod2)
plot(gam_mod2, pages=1)
exp(gam_mod2$family$getTheta())

plot.gam(gam_mod2, pages=1)
p <- predict(gam_mod2, long_roosts_loc, type = "link", se.fit = TRUE)
upr <- p$fit + (2 * p$se.fit)
lwr <- p$fit - (2 * p$se.fit)
upr <- gam_mod2$family$linkinv(upr)
lwr <- gam_mod2$family$linkinv(lwr)
long_roosts_loc$upr = upr
long_roosts_loc$lwr = lwr
# point CIs based on: https://stats.stackexchange.com/questions/33327/confidence-interval-for-gam-model/33328

yr_cols = wes_palette('Darjeeling1', 3, type = c("discrete"))
yr_cols = yr_cols[2:3]
range(long_roosts_loc$lat)
xbrks = seq(-37,-23, by = 1)
#ybrks = seq(0,1000000000, by = 10000)
ggplot(long_roosts_loc, aes(x=lat, y=tot.hab.burn, color=year_fac))+
  geom_ribbon(aes(ymin=lwr, ymax=upr, color = year_fac), alpha=0.08, lty =0) +
  geom_point(alpha = 0.4) +
  geom_line(aes(x = lat, y = fit, color = year_fac), lwd = 1.5) + 
  geom_line(aes(x = lat, y = upr), lty =2) + 
  geom_line(aes(x = lat, y = lwr), lty =2) + 
  scale_x_continuous(breaks = xbrks) +
  scale_y_continuous() +
  scale_color_manual(values=yr_cols, labels = c('typical', 'anomalous'))+
  coord_flip()+
  labs(fill = "Year", y = 'total habitat burned',
       x= 'latitude', color = 'fire season')+
  theme_classic(base_size = 14)+
  theme(legend.position = c(0.85, 0.15)) 

##########################################################
# FF roost response to fires in 2012-2013
locdir = '~/Documents/workspace/aus20192020_fires'
ff_all = read.csv(file.path(locdir, 'output/complete_covariate_df_20122013_md200414.csv'), header = T)
ff_clean = ff_all[!is.na(ff_all$tot.hab),]
names(ff_clean)
sum(is.na(ff_clean$state))
sum(is.na(ff_clean$season))
sum(is.na(ff_clean$ghff.count))

##########################################################
# GLMMs
# nsw = ff_clean[ff_clean$state == 'NSW',]
# dim(nsw) # 0  27
# qld = ff_clean[ff_clean$state == 'QLD',]
# dim(qld) # 2299   27
# hist(log10(qld$ghff.count+1))
# hist(log10(nsw$ghff.count+1))

##########################################################
### New South Wales data - none in new data
# table(nsw$ghff.presence)
# plot(nsw$date, nsw$ghff.count)
# names(nsw)
# # "season"         "year"    # only sampled in summer      
# # "year_season"    "tot.hab"        "wint.hab"       "rel_fire"       "tot.hab.burn"   "prop.tot.burn" 
# # "wint.hab.burn"  "prop.wint.burn" "dist.fire.m"   
# nsw_logmemod <- glmer(ghff.presence ~ bff.presence +  
#                         (1 | camp.name), data = nsw, family = binomial, 
#                       control = glmerControl(optimizer = "bobyqa"))
# summary(nsw_logmemod)
# 
# nsw_logmemod1 <- glmer(ghff.presence ~ bff.presence + rel_fire + 
#                          (1 | camp.name), data = nsw, family = binomial, 
#                        control = glmerControl(optimizer = "bobyqa"))
# summary(nsw_logmemod1)
# 
# head(nsw)
# 
# nsw_logmemod2 <- glmer(ghff.presence ~ bff.presence + log10(tot.hab) + log10(wint.hab) +
#                          (1 | camp.name), data = nsw, family = binomial, 
#                        control = glmerControl(optimizer = "bobyqa"))
# summary(nsw_logmemod2)
# 
# nsw_logmemod3 <- glmer(ghff.presence ~ bff.presence + log10(tot.hab) + log10(wint.hab) +
#                          prop.wint.burn + log10(dist.fire.m+1) + rel_fire + 
#                          (1 | camp.name), data = nsw, family = binomial, 
#                        control = glmerControl(optimizer = "bobyqa"))
# summary(nsw_logmemod3)
# 
# 
# nsw_logmemod4 <- glmer(ghff.presence ~ log10(tot.hab) + 
#                          prop.tot.burn + rel_fire + 
#                          (1 | camp.name), data = nsw, family = binomial, 
#                        control = glmerControl(optimizer = "bobyqa"))
# summary(nsw_logmemod4)
# 
# nsw_logmemod5 <- glmer(ff.presence ~ log10(tot.hab) + 
#                          prop.tot.burn + rel_fire + 
#                          (1 | camp.name), data = nsw, family = binomial, 
#                        control = glmerControl(optimizer = "bobyqa"))
# summary(nsw_logmemod5)


##########################################################
### Queensland data 

# qld_logmemod1 <- glmer(ghff.presence ~ log10(tot.hab)*prop.tot.burn +
#                          season +
#                          rel_fire + 
#                          (1 | camp.name), data = qld, family = binomial, 
#                        control = glmerControl(optimizer = "bobyqa"))
# summary(qld_logmemod1)
# 
# qld_logmemod2 <- glmer(ghff.presence ~ log10(tot.hab)+prop.tot.burn +
#                          season +
#                          rel_fire + 
#                          (1 | camp.name), data = qld, family = binomial, 
#                        control = glmerControl(optimizer = "bobyqa"))
# summary(qld_logmemod2)
# #
# qld_logmemod1 <- glmer(ghff.presence ~ log10(tot.hab)*prop.tot.burn +
#                          season +
#                          rel_fire + 
#                          (1 | camp.name), data = qld, family = binomial, 
#                        control = glmerControl(optimizer = "bobyqa"))
# summary(qld_logmemod1)
# 
# qld_zipoiss = glmmadmb(ghff.count ~ season + bff.presence + rel_fire
#                        (1|camp.name),
#                        data=qld,
#                        zeroInflation=TRUE,
#                        family="poisson", debug = TRUE)
# summary(qld_zipoiss)
# 
# qld_zipoiss1 = glmmadmb(ghff.count~ rel_fire+
#                           (1|camp.name),
#                         data=qld,
#                         zeroInflation=TRUE,
#                         family="poisson",
#                         debug=TRUE)
# summary(qld_zipoiss1)
# #https://rspatial.org/raster/analysis/3-spauto.html

# revisits  >10 roosts
ff_clean$date = as.Date(ff_clean$date)
ff_clean_sum = as.data.frame(ff_clean %>%
                               group_by(camp.name, lat, long) %>%
                               dplyr::summarise(sample.dates = n(),
                                                sample.pre = sum(date<"2012-12-01"),
                                                sample.post = sum(date>"2012-12-01"),
                                                state = which.max(table(state))))

min_samples = ff_clean_sum$sample.pre>3 & ff_clean_sum$sample.post>3
min_samples_name =as.vector(ff_clean_sum[min_samples,'camp.name'])
ff_clean = ff_clean[ff_clean$camp.name %in% min_samples_name, ]
unique(ff_clean$camp.name) #only QLD

ff_clean$ghff.count = as.integer(ff_clean$ghff.count)


### first check for zero inflatted...
# you can use a likelihood ratio test between the regular and zero-inflated version of the model, 
# but be aware of boundary issues (search “boundary” elsewhere on this page …) – 
# the null value (no zero inflation) is on the boundary of the feasible space

# hist(ff_clean$ghff.count)
# hist(log10(ff_clean$ghff.count+1))
# 
# 
# rescale
# summary(m1 <- zeroinfl(ghff.count ~ rel_fire , data = ff_clean))
# summary(p1 <- glm(ghff.count ~ rel_fire, family = poisson, data = ff_clean))
# vuong(p1, m1) # Vuong Non-Nested Hypothesis Test-Statistic: # looks like non zero inflated works
# qld_poiss_zi = glmmadmb(ghff.count ~ (1|camp.name),
#                         data=ff_clean,
#                         zeroInflation=TRUE,
#                         family="poisson", debug = TRUE)
# qld_poiss_nozi = glmmadmb(ghff.count ~ (1|camp.name),
#                           data=ff_clean,
#                           zeroInflation=FALSE,
#                           save.dir = "admb_dir",
#                           family="poisson", debug = TRUE)
# summary(qld_poiss_nozi)
# 
# ### Univariate models : Poisson regression
# names(ff_clean)
# #"ghff.presence"  "bff.presence"  
# # "lrff.presence"  "sff.presence"   "season"         "year"           "year_season"   
# #"tot.hab"        "wint.hab"       "rel_fire"       "tot.hab.burn"   "prop.tot.burn" 
# # "wint.hab.burn"  "prop.wint.burn" "dist.fire.m"   
# table(ff_clean$season)
# ff_clean$season = as.character(ff_clean$season)
# p1a <- glmer(ghff.count ~ 1 + (1 | camp.name), 
#              data = ff_clean, family = poisson(link = "log"))
# summary(p1a)
# lattice::dotplot(ranef(p1a, postVar = TRUE))
# 
# class(ff_clean$season)
# 
# 
# p2a <- glmer(ghff.count ~ season + (1 | camp.name), 
#              data = ff_clean, family = poisson(link = "log"))
# summary(p2a) #significant & pos ; all higher than autumn
# model_parameters(p2a, standardize = "refit")
# 
# p3a <- glmer(ghff.count ~ rel_fire + (1 | camp.name), 
#              data = ff_clean, family = poisson(link = "log"))
# summary(p3a) #significant & pos
# 
# p4a <- glmer(ghff.count ~ log10(wint.hab) + (1 | camp.name), 
#              data = ff_clean, family = poisson(link = "log"))
# summary(p4a) #significant & pos
# 
# p5a <- glmer(ghff.count ~ log10(tot.hab) + (1 | camp.name), 
#              data = ff_clean, family = poisson(link = "log"))
# summary(p5a) #significant & pos
# 
# p6a <- glmer(ghff.count ~ log10(tot.hab.burn+1) + (1 | camp.name), 
#              data = ff_clean, family = poisson(link = "log"))
# summary(p6a) #significant & neg
# 
# p7a <- glmer(ghff.count ~ log10(wint.hab.burn+1) + (1 | camp.name), 
#              data = ff_clean, family = poisson(link = "log"))
# summary(p7a) #significant & negative
# 
# p8a <- glmer(ghff.count ~ prop.tot.burn + (1 | camp.name), 
#              data = ff_clean, family = poisson(link = "log"))
# summary(p8a) #significant & negative
# 
# p9a <- glmer(ghff.count ~ prop.wint.burn + (1 | camp.name), 
#              data = ff_clean, family = poisson(link = "log"))
# summary(p9a) #significant & negative (larger)
# p9a_admb = glmmadmb(ghff.count ~ prop.wint.burn + (1 | camp.name), data = ff_clean, 
#                     family = "poisson", link = "log")
# summary(p9a_admb)
# p9a_adm_b_zib = glmmadmb(ghff.count ~ prop.wint.burn + (1 | camp.name), data = ff_clean, 
#                          family = "poisson", zeroInflation=TRUE, link = "log")
# summary(p9a_adm_b_zib)
# 
# p10a <- glmer(ghff.count ~ log(dist.fire.m+1) + (1 | camp.name), 
#               data = ff_clean, family = poisson(link = "log"))
# summary(p10a) #significant & postive (v small)
# 
# p11a <- glmer(ghff.count ~ bff.presence + (1 | camp.name), 
#               data = ff_clean, family = poisson(link = "log"))
# summary(p11a) #significant & positive
# 
# 
# ### Multivariate models : Poisson regression
# rescale... 

# 
# mp1a <- glmer(ghff.count ~ prop.wint.burn + bff.presence +(1 | camp.name), 
#               data = ff_clean, family = poisson(link = "log"))
# summary(mp1a)
# plot(mp1a)
# plot(residuals(mp1a, type = "pearson") ~ as.numeric(ff_clean$prop.wint.burn))
# plot(residuals(mp1a, type = "pearson") ~ predict(mp1a, type = "link"))
# 
# 
# mp2a <- glmer(ghff.count ~ season +  prop.wint.burn + (1 | camp.name), 
#               data =
#                 ff_clean, family = poisson(link = "log"))
# summary(mp2a)
# 
# mp3a <- glmer(ghff.count ~ season +  rel_fire + prop.wint.burn + (1 | camp.name), 
#               data = ff_clean, family = poisson(link = "log"))
#summary(mp3a)
# qld_zipoiss1 = glmmadmb(ghff.count ~ rel_fire +
#                        (1|camp.name),
#                        data=ff_clean,
#                        zeroInflation=TRUE,
#                        family="poisson", debug = TRUE)
# summary(qld_zipoiss1)
# 
# qld_zipoiss2 = glmmadmb(ghff.count ~ prop.wint.burn +
#                         (1|camp.name),
#                         data=ff_clean,
#                         zeroInflation=TRUE,
#                         family="poisson", debug = TRUE)
# summary(qld_zipoiss2)
# 
# qld_zipoiss3 = glmmadmb(ghff.count ~ prop.tot.burn +
#                         (1|camp.name),
#                         data=ff_clean,
#                         zeroInflation=TRUE,
#                         family="poisson", debug = TRUE)
# summary(qld_zipoiss3)
# 
# qld_zipoiss4 = glmmadmb(ghff.count ~ dist.fire.m +
#                         (1|camp.name),
#                         data=ff_clean,
#                         zeroInflation=TRUE,
#                         family="poisson", debug = TRUE)
# summary(qld_zipoiss4)
# 
# qld_zipoiss5 = glmmadmb(ghff.count ~ rel_fire
#                         (1|camp.name),
#                         data=ff_clean,
#                         zeroInflation=TRUE,
#                         family="poisson", debug = TRUE)
# summary(qld_zipoiss5)
# 
# qld_zipoiss4 = glmmadmb(ghff.count ~ tot
#                         (1|camp.name),
#                         data=ff_clean,
#                         zeroInflation=TRUE,
#                         family="poisson", debug = TRUE)
# summary(qld_zipoiss4)

##########
# univariate
# ff_clean
library(scales)

head(ff_clean)
ff_clean$wint.hab.burn.rsc = rescale(ff_clean$wint.hab.burn)
hist(ff_clean$wint.hab.burn.rsc)

ff_clean$tot.hab.burn.rsc= rescale(ff_clean$tot.hab.burn)
hist(ff_clean$tot.hab.burn.rsc)

ff_clean$wint.hab.rsc = rescale(ff_clean$wint.hab)
hist(ff_clean$wint.hab.burn)

ff_clean$tot.hab.rsc= rescale(ff_clean$tot.hab)
hist(ff_clean$tot.hab.burn.rsc)

ff_clean$dist.fire.m.rsc= rescale(ff_clean$dist.fire.m)
hist(ff_clean$dist.fire.m.rsc)

sum_var = as.data.frame(matrix(NA, ncol = 3, nrow = 12))
num = c(19,20,21, 23:31)
for (i in 1:length(num)){
  sum_var[i,1] = names[num[i]]
  sum_var[i,2] = min(ff_clean[,num[i]])
  sum_var[i,3] = max(ff_clean[,num[i]])
  print(i)
}

names = colnames(ff_clean)
print()
print(min(ff_clean[,19]))
print(max(ff_clean[,19]))

logme <- glmer(ff.presence ~ 1 +
                 (1 | camp.name), data = ff_clean, family = binomial, 
               control = glmerControl(optimizer = "bobyqa"),
               nAGQ = 10)
summary(logme)

logme2 <- glmer(ff.presence ~ season +
                  (1 | camp.name), data = ff_clean, family = binomial, 
                control = glmerControl(optimizer = "bobyqa"),
                nAGQ = 10)
summary(logme2) #NS

logme1a <- glmer(ghff.presence ~ 1 + (1 | camp.name), 
                 data = ff_clean, family = binomial,             
                 control = glmerControl(optimizer = "bobyqa"),            
                 nAGQ = 10)
summary(logme1a)
lattice::dotplot(ranef(logme1a, postVar = TRUE))

class(ff_clean$season)


logme2a <- glmer(ghff.presence ~ season + (1 | camp.name), 
                 data = ff_clean, family = binomial,             
                 control = glmerControl(optimizer = "bobyqa"),            
                 nAGQ = 10)
summary(logme2a) #spring and winter significant
#model_parameters(logme2a, standardize = "refit")

logme3a <- glmer(ghff.presence ~ fire_rel + (1 | camp.name), 
                 data = ff_clean, family = binomial,             
                 control = glmerControl(optimizer = "bobyqa"),            
                 nAGQ = 10)
summary(logme3a) #NS

logme4a <- glmer(ghff.presence ~ wint.hab.rsc + (1 | camp.name), 
                 data = ff_clean, family = binomial,             
                 control = glmerControl(optimizer = "bobyqa"),            
                 nAGQ = 10)
summary(logme4a) #NS

logme5a <- glmer(ghff.presence ~ tot.hab.rsc + (1 | camp.name), 
                 data = ff_clean, family = binomial,             
                 control = glmerControl(optimizer = "bobyqa"),            
                 nAGQ = 10)
summary(logme5a) #sNS


logme6a <- glmer(ghff.presence ~ tot.hab.burn.rsc + (1 | camp.name), 
                 data = ff_clean, family = binomial,             
                 control = glmerControl(optimizer = "bobyqa"),            
                 nAGQ = 10)
summary(logme6a) # NS

logme7a <- glmer(ghff.presence ~ wint.hab.burn.rsc + (1 | camp.name), 
                 data = ff_clean, family = binomial,             
                 control = glmerControl(optimizer = "bobyqa"),            
                 nAGQ = 10)
summary(logme7a) #sNS

logme8a <- glmer(ghff.presence ~ prop.tot.burn + (1 | camp.name), 
                 data = ff_clean, family = binomial,             
                 control = glmerControl(optimizer = "bobyqa"),            
                 nAGQ = 10)
summary(logme8a) #Significant

logme9a <- glmer(ghff.presence ~ prop.wint.burn + (1 | camp.name), 
                 data = ff_clean, family = binomial,             
                 control = glmerControl(optimizer = "bobyqa"),            
                 nAGQ = 10)
summary(logme9a) #significant 
names(ff_clean)

logme10a <- glmer(ghff.presence ~ dist.fire.m.rsc + (1 | camp.name), 
                  data = ff_clean, family = binomial,             
                  control = glmerControl(optimizer = "bobyqa"),            
                  nAGQ = 10)
summary(logme10a) #negative and significant

logme11a <- glmer(ghff.presence ~ bff.presence + (1 | camp.name), 
                  data = ff_clean, family = binomial,             
                  control = glmerControl(optimizer = "bobyqa"),            
                  nAGQ = 10)
summary(logme11a) #significant & positive

### Multiple logistic regression
logme1m <- glmer(ghff.presence ~ season + lat + log(dist.fire.m+1) + prop.wint.burn + bff.presence  +
                   (1 | camp.name), 
                 data = ff_clean, family = binomial,             
                 control = glmerControl(optimizer = "bobyqa"),            
                 nAGQ = 10)
summary(logme1m) #prop wint and prop wint significantly corleted 
# gives a rescale error

logme2m <- glmer(ghff.presence ~ season + log(dist.fire.m+1) + prop.wint.burn + bff.presence +
                   (1 | camp.name), 
                 data = ff_clean, family = binomial,             
                 control = glmerControl(optimizer = "bobyqa"),            
                 nAGQ = 10)
summary(logme2m)
logme3m <- glmer(ghff.presence ~ season + prop.wint.burn + bff.presence +
                   (1 | camp.name), 
                 data = ff_clean, family = binomial,             
                 control = glmerControl(optimizer = "bobyqa"),            
                 nAGQ = 10)
summary(logme3m)
lattice::dotplot(ranef(logme3m, postVar = TRUE))

logme4m <- glmer(ghff.presence ~ season + prop.wint.burn  +
                   (1 | camp.name), 
                 data = ff_clean, family = binomial,             
                 control = glmerControl(optimizer = "bobyqa"),            
                 nAGQ = 10)
summary(logme4m)


logme5m <- glmer(ghff.presence ~ season + log(tot.hab+1)*prop.tot.burn  +
                   (1 | camp.name), 
                 data = ff_clean, family = binomial,             
                 control = glmerControl(optimizer = "bobyqa"),            
                 nAGQ = 10)
summary(logme5m)
lattice::dotplot(ranef(logme1a, postVar = TRUE))

logme6m <- glmer(ghff.presence ~ season + wint.hab.rsc + prop.wint.burn  + #interaction is insignificant
                   (1 | camp.name), 
                 data = ff_clean, family = binomial,             
                 control = glmerControl(optimizer = "bobyqa"),            
                 nAGQ = 10)
summary(logme6m)
plot(logme6m)

logme7m <- glmer(ghff.presence ~ season + tot.hab.rsc + prop.tot.burn  + #interaction is insignificant
                   (1 | camp.name), 
                 data = ff_clean, family = binomial,             
                 control = glmerControl(optimizer = "bobyqa"),            
                 nAGQ = 10)
summary(logme7m)


logme8m <- glmer(ghff.presence ~ season + lat + prop.tot.burn  + #interaction is insignificant
                   (1 | camp.name), 
                 data = ff_clean, family = binomial,             
                 control = glmerControl(optimizer = "bobyqa"),            
                 nAGQ = 10)
summary(logme8m)
plot(logme8m)

logme9m <- glmer(ghff.presence ~ season + lat + prop.wint.burn  + #interaction is insignificant
                   (1 | camp.name), 
                 data = ff_clean, family = binomial,             
                 control = glmerControl(optimizer = "bobyqa"),            
                 nAGQ = 10)
summary(logme9m)
plot(logme9m)

logme10m <- glmer(ghff.presence ~ season + lat + wint.hab.rsc + prop.wint.burn  + #interaction is insignificant
                   (1 | camp.name), 
                 data = ff_clean, family = binomial,             
                 control = glmerControl(optimizer = "bobyqa"),            
                 nAGQ = 10)
summary(logme10m)

logme11m <- glmer(ghff.presence ~ season + lat + rel_fire + #interaction is insignificant
                   (1 | camp.name), 
                 data = ff_clean, family = binomial,             
                 control = glmerControl(optimizer = "bobyqa"),            
                 nAGQ = 10)
summary(logme11m)

logme12m <- glmer(ghff.presence ~ season + lat + bff.presence + #interaction is insignificant
                   (1 | camp.name), 
                 data = ff_clean, family = binomial,             
                 control = glmerControl(optimizer = "bobyqa"),            
                 nAGQ = 10)
summary(logme12m)

logme13m <- glmer(ghff.presence ~ season + bff.presence + #interaction is insignificant
                    (1 | camp.name), 
                  data = ff_clean, family = binomial,             
                  control = glmerControl(optimizer = "bobyqa"),            
                  nAGQ = 10)
summary(logme13m)
#ff_clean$tot.hab.remain = ff_clean$tot.hab - ff_clean$tot.hab.burn
hist(ff_clean$tot.hab.remain)
#ff_clean$tot.hab.remain.rsc = rescale(ff_clean$tot.hab.remain)
#ff_clean$wint.hab.remain = ff_clean$wint.hab - ff_clean$wint.hab.burn
#ff_clean$wint.hab.remain.rsc = rescale(ff_clean$wint.hab.remain)
logme11m <- glmer(ghff.presence ~ season + bff.presence + tot.hab.rsc +  
                    (1 | camp.name), 
                  data = ff_clean, family = binomial,             
                  control = glmerControl(optimizer = "bobyqa"),            
                  nAGQ = 10)
summary(logme11m)

logme12m <- glmer(ghff.presence ~ season + bff.presence + wint.hab.rsc +  
                    (1 | camp.name), 
                  data = ff_clean, family = binomial,             
                  control = glmerControl(optimizer = "bobyqa"),            
                  nAGQ = 10)
summary(logme12m)

logme13m <- glmer(ghff.presence ~ season + bff.presence + wint.hab.rsc + lat +
                    (1 | camp.name), 
                  data = ff_clean, family = binomial,             
                  control = glmerControl(optimizer = "bobyqa"),            
                  nAGQ = 10)
summary(logme13m)
plot(logme13m)

logme14m <- glmer(ghff.presence ~ season + bff.presence + wint.hab.rsc + lat +
                    (1 | camp.name), 
                  data = ff_clean, family = binomial,             
                  control = glmerControl(optimizer = "bobyqa"),            
                  nAGQ = 10)
summary(logme14m)

ff_clean$dist.fire.m.rsc = rescale(ff_clean$dist.fire.m)
logme15m <- glmer(ghff.presence ~ season + bff.presence + dist.fire.m.rsc + lat +
                    (1 | camp.name), 
                  data = ff_clean, family = binomial,             
                  control = glmerControl(optimizer = "bobyqa"),            
                  nAGQ = 10)
summary(logme15m)

logme16m <- glmer(ghff.presence ~ season*wint.hab.rsc + bff.presence + lat +
                    (1 | camp.name), 
                  data = ff_clean, family = binomial,             
                  control = glmerControl(optimizer = "bobyqa"),            
                  nAGQ = 10)
summary(logme16m)

AIC(logme1m,
    logme2m,
    logme3m,
    logme4m,
    logme5m,
    logme6m,
    logme7m,
    logme8m,
    logme9m,
    logme10m,
    logme11m,
    logme12m,
    logme13m)

plot(ff_clean$prop.wint.burn, ff_clean$prop.tot.burn)

cc <- confint(logme13m,parm="beta_") 

cc <- confint(logme13m,parm="beta_",method="Wald") 
ctab <- cbind(est=fixef(logme13m),cc)
rtab <- exp(ctab)
print(rtab,digits=3)

###### plotting cleaned data
library(raster)
library(rgdal)
library(scales)
library(rgeos)
library(mapview)
library(sf)
library(plyr)

ff_loc_xy = ff_all[,c('long','lat')]
ff_spdf = SpatialPointsDataFrame(coords = ff_loc_xy, data = ff_all[,-which(names(ff_all) %in% c('long','lat'))],
                                    proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

ausdir = "~/Dropbox/Projects/Hendra/workspace/sp_dat/AUS_adm"
east = readOGR(ausdir,'east') #country boundary
crs(east)
plot(east, border = 'black', ylim = c(-29,-24), xlim = c(150,154))
#reproject for distance matrix
library(scales)
plot(ff_spdf, add =TRUE, col = alpha('darkred', 0.2), pch = 19)
ff_spdf$season.yr = paste0(ff_spdf$season, ff_spdf$rel_fire)

ff_spdf$season.yr = as.factor(ff_spdf$season.yr)

ff_spdf$season.yr2 = factor(ff_spdf$season.yr,levels(ff_spdf$season.yr)[c(6,2,8,4,5,1,7,3)])
seasons = levels(ff_spdf$season.yr2)
# c('summerpre','autumnpre','winterpre','springpre',
#   'summerpost', 'autumnpost', 'winterpost', 'springpost') 


ff.spdf.season = split(ff_spdf,ff_spdf$season.yr2)

par(mfrow=c(2,4)) 
for (i in 1:8){
  #i = 1
  plot(east,  main = seasons[i], ylim = c(-29,-24), xlim = c(150,154))
  plot(ff.spdf.season[[i]][ff.spdf.season[[i]]$ghff.presence == 0,], add = T, pch =20,  cex = 2, col = alpha('gold', 0.3))
  plot(ff.spdf.season[[i]][ff.spdf.season[[i]]$ghff.presence == 1,], add = T, pch =20, cex = 2, col = alpha('deepskyblue', 0.2))
  # legend('topleft', legend=c('grey-headed', 'black'), 
  #        pch=c(19,19),col=c('cyan4','darkgoldenrod'))
}

