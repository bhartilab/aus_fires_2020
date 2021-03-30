###########################################################
# Investigating fire impact on flying fox habitat and roost occupancy
# GAMs for focal fire seasons: 2012-2013; 2019-2020
##########################################################
# libraries
library(dplyr) #manipulating dfs
library(mgcv) #gams
library(ggplot2)

##########################################################
# Proportion winter habitat burned in focal years
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
upr <- gam_mod2$family$linkinv(upr) #point CIs
lwr <- gam_mod2$family$linkinv(lwr)
long_roosts_loc$upr = upr
long_roosts_loc$lwr = lwr

yr_cols = c("#00A08A", "#F2AD00") #originally derived from wesanderson Darjeeling1 color pallete
range(long_roosts_loc$lat)
xbrks = seq(-37,-23, by = 1)
ybrks = seq(0,0.6, by = 0.1)
ggplot(long_roosts_loc, aes(x=lat, y=Prop_HabBurn440, color=year_fac))+
  geom_ribbon(aes(ymin=lwr, ymax=upr, color = year_fac), alpha=0.08, lty =0) +
  geom_point(alpha = 0.4, stroke = 0, size =2) +
  geom_line(aes(x = lat, y = fit, color = year_fac), lwd = 1.5) + 
  geom_line(aes(x = lat, y = upr), lty =2) + 
  geom_line(aes(x = lat, y = lwr), lty =2) + 
  scale_x_continuous(breaks = xbrks) +
  scale_y_continuous(breaks = ybrks) +
  scale_color_manual(values=yr_cols, labels = c('previous', 'anomalous'))+
  coord_flip()+
  labs(fill = "Year", y = 'proportion winter habitat burned',
       x= 'latitude', color = 'fire season')+
  theme_classic(base_size = 14)+
  theme(legend.position = c(0.85, 0.15)) 

cairo_ps(filename = "panel-i.eps",
         width = 4, height = 9.14, pointsize = 12,
         fallback_resolution = 500)

ggsave(filename = "survival-curves.eps",
       plot = print(p),
       device = cairo_eps)

ggsave(filename = 'panel_i_png_v2.png',
  plot = last_plot(),
  device = 'png',
  scale = 1,
  width = 4.00,
  height = 9.14,
  dpi = 1000,
  limitsize = FALSE)


##########################################################
# Total burned foraging habitat 
hab_2012 = read.csv("data/Total_HabitatBurn440_2012.csv", header = TRUE)
hab_2012$year =2012
names(hab_2012)
hab_2012 = hab_2012[,c('Camp','Burn440_Area','year')]
names(hab_2012) <- c('camp.name', 'tot.hab.burn','year')

hab_2019 = read.csv("data/Total_HabitatBurn440_2019.csv", header = TRUE)
hab_2019$year = 2019
names(hab_2019)
hab_2019 = hab_2019[,c('Camp','Burn440_Area','year')]
names(hab_2019) <- c('camp.name', 'tot.hab.burn','year')

long_forg_hab = plyr::rbind.fill(hab_2012, hab_2019)
long_forg_hab_loc = merge(long_forg_hab,ff_loc,by = 'camp.name',  all.x = TRUE)
long_forg_hab_loc$year_fac = as.factor(long_forg_hab_loc$year)
# GAMMS
gam_mod1 <- gam(tot.hab.burn ~ s(lat, by = year_fac), data = long_forg_hab_loc) 
summary(gam_mod1)
coef(gam_mod1)
plot(gam_mod1, pages=1)
exp(gam_mod1$family$getTheta())
plot(gam_mod1, residuals = TRUE, pch = 1, pages=1)

gam_mod2 <- gam(tot.hab.burn ~ s(lat, by = year_fac) + year_fac, data = long_forg_hab_loc) #, family = 'betar'
summary(gam_mod2) #allows for different intercepts
coef(gam_mod2)
plot(long_forg_hab_loc$lat, fitted(gam_mod2))
long_forg_hab_loc$fit = fitted(gam_mod2)
print(gam_mod2)
plot(gam_mod2, pages=1)

plot.gam(gam_mod2, pages=1)
p <- predict(gam_mod2, long_forg_hab_loc, type = "link", se.fit = TRUE)
upr <- p$fit + (2 * p$se.fit)
lwr <- p$fit - (2 * p$se.fit)
upr <- gam_mod2$family$linkinv(upr)
lwr <- gam_mod2$family$linkinv(lwr)
long_forg_hab_loc$upr = upr
long_forg_hab_loc$lwr = lwr


range(long_forg_hab_loc$lat)
xbrks = seq(-37,-23, by = 1)
#ybrks = seq(0,1000000000, by = 10000)
ggplot(long_forg_hab_loc, aes(x=lat, y=tot.hab.burn, color=year_fac))+
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
