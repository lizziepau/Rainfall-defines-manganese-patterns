getwd()


library(vegan)
library(ggtext)
library(gam)
library(mgcv)
library(car)

map_aet<-c(144.68939209,134.713226318,153.635040283,166,193.367828369,2.78582763700001,21.48348999,61.990478516,84.419769287,131.785827637,446.85357666,362.726745605,390.71697998,429.462280273,470.800537109,296.85357666,362.726745605,390.71697998,429.462280273,335.800537109,453.46264648,499.1307373,546.92260742,629.33374023,680.03955078,781.83703613,866.97570801,792.55187988,925.61401367,974.0534668,882.75280762,827.74255371,1110.12414551,1197.21411133,1237.69360352,1251.13134766,1313.2409668,1060.17749023,1179.27124023,1510.28564453,1611.23364258,1893.21679688,1991.22290039,1768.20703125,2198.35327148,1987.98974609)
ph<-c(	6.3888889,6.3777778,6.1388889,6.4,6.5777778,6.3888889,5.9944444,6.3055556,6.3222222,6.0222222,6.4555556,6.6388889,6.0933333,6.0888889,6.1833333,5.9666667,5.8388889,6.1666667,6.2555556,6.0388889,6.0555556,6.2555556,5.6777778,5.8444444,5.7444444,5.3833333,5.5777778,5.3277778,5.3277778,5.1111111,5.4,5.1166667,5.0333333,4.5055556,4.4722222,4.4722222,4.4111111,4.5555556,4.7277778,4.5833333,4.4222222,4.2444444,4.1333333,4.0611111,4.5166667,4.15)
map<-c(	294.68939209,284.713226318,303.635040283,316,343.367828369,377.785827637,396.48348999,436.990478516,459.419769287,506.785827637,584.314208984,613.122802734,670.684570313,698.421508789,759.401550293,821.85357666,887.726745605,915.71697998,954.462280273,995.800537109,1113.46264648,1159.1307373,1206.92260742,1289.33374023,1340.03955078,1441.83703613,1526.97570801,1577.55187988,1710.61401367,1759,1807.75280762,1902.74255371,2035.12414551,2122.21411133,2162.69360352,2176.13134766,2238.2409668,2310.17749023,2429.27124023,2585.28564453,2686.23364258,2818.21679688,2916.22290039,3018.20703125,3123.35327148,3237.98974609)
soilmoisture<-c(	8.13021379868061,7.5218848435972,7.83844824166808,6.83113755649376,10.9665885655149,8.61428525987926,8.01077612795958,9.1193962305264,18.2432256684164,19.2226689183303,11.5804647073898,13.2750267750253,11.4692733902546,11.2890018537871,10.217399182938,9.52993506056816,12.0225538783929,15.1199091334819,14.5617292068745,12.2530864165239,14.178198856579,12.3913668670745,18.5794773751595,17.3806794611585,24.9912575538127,29.279320801777,31.0571265752669,34.4413727960088,45.4568195482135,43.4078645719165,42.2512892749653,45.7023836160293,42.2767607387348,39.1241037194266,43.7257306962748,43.6383277520677,43.6525003395365,39.9464714727148,46.7495222249587,42.5025482737767,42.9090660933084,47.5593533943846,59.9861043911349,62.9566044990022,68.2959410943316,66.0422926691985)
soilmn<-c(	2431,2521,2719,2472.66666666667,2449.33333333333,2330.33333333333,2677.33333333333,2423.33333333333,2305,2230,2281,2020,1994.66666666667,2006,2208.6,2411.8,1892.5,2427.8,2796.8,2587.8,3499.4,2260.4,3308.8,2901.88888888889,2903.4,2870.22222222222,2609.8,2718.8,2423.2,3330,2655,1389.8,1846.2,1175.8,1179.2,1185.04,823.5,463.6,1098.53333333333,544.033333333333,634.7,814.733333333333,851.92,452.7,479.8,644.1)
grassmn<-c(	30.68,26.0857142857143,30.3,18.22,21.98,18.08,28.18,20.0571428571429,26.1,27.28,14.24,15.1,24.2714285714286,28.46,51.6,64.08,33.12,42.16,57.06,46.0285714285714,55.8,50.8857142857143,48.6875,37.35,43.8714285714286,52.4,33.14,73.08,71.72,199.26,86.42,396.24,441.36,535.9,311.34,404.68,729.82,526.133333333333,357.86,395.6,567.66,517.32,476.48,200.52,226.4875,223.3)
extractmn	<-c(16.3806283408849,12.3731389078118,10.8790007440503,19.0643166000596,6.27610091534029,30.8403242577927,42.1046270194626,12.4145825294226,8.03738766578375,23.9133558477589,8.8814836217827,12.4756412937551,5.36165533796007,17.8859503433453,9.40122540787767,92.2836841376132,15.5763907026428,111.579369731176,67.0859084027,79.9714176913535,121.649362672808,71.7469175093556,211.474411423571,100.500805439851,97.3550939869146,200.086684538458,138.001213121893,234.643311050234,202.731145901232,438.581300432925,306.271817679704,218.092406925216,365.835719182934,126.283217832459,201.463966487071,255.56213561613,63.4080637046533,50.4274637078522,116.70225279461,24.2859691734521,56.7792467629184,22.2860103084259,21.7966262514108,7.8585529244824,276.228189122752,81.7883155427267)

shapiro.test(ph)#not normal
shapiro.test(map)#not normal
shapiro.test(soilmoisture)#not normal
shapiro.test(soilmn)#not normal
shapiro.test(grassmn)#not normal
shapiro.test(extractmn)#not normal
shapiro.test(map_aet)#not normal

gradient.df<-data.frame(ph,map,soilmoisture,soilmn,grassmn,extractmn,map_aet)
gradient.df

#Gamma distribution: (0, infinity), non-integers
soilmn.glm <- glm(soilmn ~ ph * map * soilmoisture, data=gradient.df, family = Gamma)
summary(soilmn.glm)
soilmn.gam <- gam(soilmn ~ ph * map * soilmoisture, data=gradient.df, family = Gamma)
summary(soilmn.gam)
soilmn.mix.log <- glm(soilmn ~ ph * map * soilmoisture, data=gradient.df, family = Gamma(log))
summary(soilmn.mix.log)
soilmn.mix.gam <- gam(soilmn ~ ph * map * soilmoisture, data=gradient.df, family = Gamma(log))
summary(soilmn.mix.gam)
soilmn.mix.addglm <- glm(soilmn ~ ph + map + soilmoisture, data=gradient.df, family = Gamma(log))
summary(soilmn.mix.addglm)
soilmn.mix.addgam <- gam(soilmn ~ ph + map + soilmoisture, data=gradient.df, family = Gamma(log))
summary(soilmn.mix.addgam)

grassmn.mix <- glm(grassmn ~ ph * map * soilmoisture, data=gradient.df, family = Gamma)
summary(grassmn.mix)
grassmn.mix.log <- glm(grassmn ~ ph * map * soilmoisture, data=gradient.df, family = Gamma(log))
summary(grassmn.mix.log)
grassmn.mix.gam <- gam(grassmn ~ ph * map * soilmoisture, data=gradient.df, family = Gamma(log))
summary(grassmn.mix.gam)

extractmn.mix <- glm(extractmn ~ ph * map * soilmoisture, data=gradient.df, family = Gamma)
summary(extractmn.mix)
extractmn.mix.log <- glm(extractmn ~ ph * map * soilmoisture, data=gradient.df, family = Gamma(log))
summary(extractmn.mix.log)
extractmn.mix.gam <- gam(extractmn ~ ph * map * soilmoisture, data=gradient.df, family = Gamma(log))
summary(extractmn.mix.gam)

#Adding map_aet to GLM and GAM
#ph, soil moisture, map, and map_aet are inextrictably linked--interactions must be included in the glm and gam (asterisks indicate effects and their interactions)
#soil Mn
soilmn.glm.int <- glm(soilmn ~ ph * map * soilmoisture * map_aet, data=gradient.df, family = Gamma)
summary(soilmn.glm.int)
plot(soilmn.glm.int)
confint(soilmn.glm.int)
soilmn.gam.int <- gam(soilmn ~ map * map_aet * ph * soilmoisture, data=gradient.df, family = Gamma)
summary(soilmn.gam.int)
anova(soilmn.glm.int,soilmn.gam.int, test = "Chisq")
anova(soilmn.mix.addgam,soilmn.gam.int, test = "Chisq")
vif(soilmn.gam.int, data=gradient.df)
soilmn.gam_simple <- gam(soilmn ~ s(map) + s(map_aet) + s(ph) + s(soilmoisture), 
                         data = gradient.df, family = Gamma)
summary(soilmn.gam_simple)
#lagniappe
soilmn.mix.log <- glm(soilmn ~ ph * map * soilmoisture * map_aet, data=gradient.df, family = Gamma(log))
summary(soilmn.mix.log) 
soilmn.mix.gam <- gam(soilmn ~ ph * map * soilmoisture * map_aet, data=gradient.df, family = Gamma(log))
summary(soilmn.mix.gam)
soilmn.mix.addglm <- glm(soilmn ~ ph + map + soilmoisture + map_aet, data=gradient.df, family = Gamma(log))
summary(soilmn.mix.addglm)
soilmn.mix.addgam <- gam(soilmn ~ ph + map + soilmoisture + map_aet, data=gradient.df, family = Gamma)
summary(soilmn.mix.addgam)
anova(soilmn.mix, soilmn.mix.log,soilmn.mix.gam, soilmn.mix.addglm, soilmn.mix.addgam, test = "Chisq")
anova(soilmn.mix.addglm, soilmn.mix.addgam, test = "Chisq")
anova(soilmn.mix.addgam,soilmn.mix.addglm, test = "Chisq")
anova(soilmn.mix, soilmn.mix.log,soilmn.mix.gam, test = "Chisq")

#Grass Mn
grassmn.glm <- glm(grassmn ~ ph * map * soilmoisture  * map_aet, data=gradient.df, family = Gamma)
summary(grassmn.glm)
grassmn.gam <- gam(grassmn ~ ph * map * soilmoisture  * map_aet, data=gradient.df, family = Gamma)
summary(grassmn.gam)
anova(grassmn.glm, grassmn.gam, test = "Chisq")
grassmn.gam_simple <- gam(grassmn ~ s(map) + s(map_aet) + s(ph) + s(soilmoisture), 
                         data = gradient.df, family = Gamma)
summary(grassmn.gam_simple)
#lagniappe, below not used in grassmn final analyses
grassmn.mix.log <- glm(grassmn ~ ph * map * soilmoisture  * map_aet, data=gradient.df, family = Gamma(log))
summary(grassmn.mix.log)
grassmn.mix.gam <- gam(grassmn ~ ph * map * soilmoisture  * map_aet, data=gradient.df, family = Gamma(log))
summary(grassmn.mix.gam)
anova(grassmn.mix, grassmn.mix.log,grassmn.mix.gam, test = "Chisq")

#Extract Mn
extractmn.glm <- glm(extractmn ~ ph * map * soilmoisture  * map_aet, data=gradient.df, family = Gamma)
summary(extractmn.glm)
extractmn.gam <- gam(extractmn ~ ph * map * soilmoisture  * map_aet, data=gradient.df, family = Gamma)
summary(extractmn.gam)
anova(extractmn.glm, extractmn.gam, test = "Chisq")
extractmn.gam_simple <- gam(extractmn ~ s(map) + s(map_aet) + s(ph) + s(soilmoisture), 
                         data = gradient.df, family = Gamma)
summary(extractmn.gam_simple)
#lagniappe, below not used in extractmn final analyses
extractmn.mix.log <- glm(extractmn ~ ph * map * soilmoisture  * map_aet, data=gradient.df, family = Gamma(log))
summary(extractmn.mix.log)
extractmn.mix.gam <- gam(extractmn ~ ph * map * soilmoisture  * map_aet, data=gradient.df, family = Gamma(log))
summary(extractmn.mix.gam)




#Redundancy Analyses (RDA) (vegan package)

#soil Mn
soilmn.rda.all<-rda(soilmn~map+ph+soilmoisture+map_aet)
summary(soilmn.rda.all)
plot(soilmn.rda.all)
anova.cca(soilmn.rda.all)

soilmn.rda.map<-rda(soilmn~map)
summary(soilmn.rda.map)
anova.cca(soilmn.rda.map)

soilmn.rda.map_aet<-rda(soilmn~map_aet)
summary(soilmn.rda.map_aet)
anova.cca(soilmn.rda.map_aet)

soilmn.rda.ph<-rda(soilmn~ph)
summary(soilmn.rda.ph)
anova.cca(soilmn.rda.ph)

soilmn.rda.soilmoisture<-rda(soilmn~soilmoisture)
summary(soilmn.rda.soilmoisture)
anova.cca(soilmn.rda.soilmoisture)

soilmn.r2.all<-RsquareAdj(soilmn.rda.all)
soilmn.r2.all
soilmn.r2.map<-RsquareAdj(soilmn.rda.map)
soilmn.r2.map
soilmn.r2.ph<-RsquareAdj(soilmn.rda.ph)
soilmn.r2.ph
soilmn.r2.soilmoisture<-RsquareAdj(soilmn.rda.soilmoisture)
soilmn.r2.soilmoisture
soilmn.r2.map_aet<-RsquareAdj(soilmn.rda.map_aet)
soilmn.r2.map_aet

#grass Mn
grassmn.rda.all<-rda(grassmn~map+ph+soilmoisture+map_aet)
summary(grassmn.rda.all)
plot(grassmn.rda.all)
anova.cca(grassmn.rda.all)

grassmn.rda.map<-rda(grassmn~map)
summary(grassmn.rda.map)
anova.cca(grassmn.rda.map)

grassmn.rda.map_aet<-rda(grassmn~map_aet)
summary(grassmn.rda.map_aet)
anova.cca(grassmn.rda.map_aet)

grassmn.rda.ph<-rda(grassmn~ph)
summary(grassmn.rda.ph)
anova.cca(grassmn.rda.ph)

grassmn.rda.soilmoisture<-rda(grassmn~soilmoisture)
summary(grassmn.rda.soilmoisture)
anova.cca(grassmn.rda.soilmoisture)

grassmn.r2.all<-RsquareAdj(grassmn.rda.all)
grassmn.r2.all
grassmn.r2.map<-RsquareAdj(grassmn.rda.map)
grassmn.r2.map
grassmn.r2.ph<-RsquareAdj(grassmn.rda.ph)
grassmn.r2.ph
grassmn.r2.soilmoisture<-RsquareAdj(grassmn.rda.soilmoisture)
grassmn.r2.soilmoisture
grassmn.r2.map_aet<-RsquareAdj(grassmn.rda.map_aet)
grassmn.r2.map_aet

#extract Mn
extractmn.rda.all<-rda(extractmn~map+ph+soilmoisture+map_aet)
summary(extractmn.rda.all)
plot(extractmn.rda.all)
anova.cca(extractmn.rda.all)

extractmn.rda.map<-rda(extractmn~map)
summary(extractmn.rda.map)
plot(extractmn.rda.map)
anova.cca(extractmn.rda.map)

extractmn.rda.map_aet<-rda(extractmn~map_aet)
summary(extractmn.rda.map_aet)
anova.cca(extractmn.rda.map_aet)

extractmn.rda.ph<-rda(extractmn~ph)
summary(extractmn.rda.ph)
anova.cca(extractmn.rda.ph)

extractmn.rda.soilmoisture<-rda(extractmn~soilmoisture)
summary(extractmn.rda.soilmoisture)
anova.cca(extractmn.rda.soilmoisture)

extractmn.r2.all<-RsquareAdj(extractmn.rda.all)
extractmn.r2.all
extractmn.r2.map<-RsquareAdj(extractmn.rda.map)
extractmn.r2.map
extractmn.r2.ph<-RsquareAdj(extractmn.rda.ph)
extractmn.r2.ph
extractmn.r2.soilmoisture<-RsquareAdj(extractmn.rda.soilmoisture)
extractmn.r2.soilmoisture
extractmn.r2.map_aet<-RsquareAdj(extractmn.rda.map_aet)
extractmn.r2.map_aet

plot(soilmn.rda.all, scaling = 2)
plot(grassmn.rda.all, scaling = 2)
plot(extractmn.rda.all, scaling = 2)


##Variation partitioning (Vegan Package)

palette.colors()
soilmn.vp2<-varpart(soilmn,~map,~ph,~soilmoisture,~map_aet)
soilmn.vp2
plot(soilmn.vp2,
     Xnames = c("MAP","pH","         Moisture","         MAP-MAET" ), # name the partitions
     bg = c("dodgerblue", "red3", "orchid","seagreen2"), alpha = 90, # color the circles
     digits = 2, # only show 2 digits
     cex = 1)
anova.cca(rda(soilmn,ph))
anova.cca(rda(soilmn,map))
anova.cca(rda(soilmn,soilmoisture))
anova.cca(rda(soilmn,map_aet))
summary(soilmn.vp2)

#grass Mn
grassmn.vp<-varpart(grassmn,~map,~ph,~soilmoisture,~map_aet )
grassmn.vp
plot(grassmn.vp,
     Xnames = c("MAP","pH","         Moisture","         MAP-MAET" ), # name the partitions
     bg = c("dodgerblue", "red3", "orchid","seagreen2"), alpha = 90, # color the circles
     digits = 2, # only show 2 digits
     cex = 1)
anova.cca(rda(grassmn,ph))
anova.cca(rda(grassmn,map))
anova.cca(rda(grassmn, soilmoisture))
anova.cca(rda(grassmn,map_aet))
summary(grassmn.vp)


#extract Mn
extractmn.vp<-varpart(extractmn,~map ,~ph ,~soilmoisture,~map_aet)
extractmn.vp
plot(extractmn.vp,cutoff=0,
     Xnames = c("MAP","pH","         Moisture","         MAP-MAET" ), # name the partitions
     bg = c("dodgerblue", "red3", "orchid","seagreen2"), alpha = 90, # color the circles
     digits = 2, # only show 2 digits
     cex = 1)
anova.cca(rda(extractmn,ph))
anova.cca(rda(extractmn,map))
anova.cca(rda(extractmn, soilmoisture))
anova.cca(rda(extractmn,map_aet))
summary(extractmn.vp)


install.packages("relaimpo")
library(relaimpo)
soilmn_lm <- lm(soilmn ~ map * map_aet * soilmoisture * ph, data = gradient.df)
summary(soilmn_lm)
soilmn_lm2 <- lm(soilmn ~ map*map_aet + map*soilmoisture + map*ph + map_aet*soilmoisture + map_aet*ph + soilmoisture*ph)
calc.relimp(soilmn_lm2, type = "lmg")
soilmn_glm <- glm(soilmn ~ map * map_aet * soilmoisture * ph, data = gradient.df, family = Gamma)
summary(soilmn_glm)
soilmn_glm2 <- glm(soilmn ~ map*map_aet + map*soilmoisture + map*ph + map_aet*soilmoisture + map_aet*ph + soilmoisture*ph, data = gradient.df, family = Gamma)
calc.relimp(soilmn_glm2, type = "lmg")

grassmn_lm <- lm(grassmn ~ map * map_aet * soilmoisture * ph, data = gradient.df)
summary(grassmn_lm)
grassmn_lm2 <- lm(grassmn ~ map*map_aet + map*soilmoisture + map*ph + map_aet*soilmoisture + map_aet*ph + soilmoisture*ph)
calc.relimp(grassmn_lm2, type = "lmg")

extractmn_lm <- lm(extractmn ~ map * map_aet * soilmoisture * ph, data = gradient.df)
summary(extractmn_lm)
extractmn_lm2 <- lm(extractmn ~ map*map_aet + map*soilmoisture + map*ph + map_aet*soilmoisture + map_aet*ph + soilmoisture*ph)
calc.relimp(extractmn_lm2, type = "lmg")



######################################################################################################################
######################################################################################################################
######################################################################################################################
# ============================================================
# Permutation-based variable importance for three GAMs
# with normalized importance scores (0-1) and
# confidence intervals on the normalized importance plot
# ============================================================

# Packages
library(mgcv)
library(dplyr)
library(ggplot2)

# ------------------------------------------------------------
# Fit GAMs
# ------------------------------------------------------------
soilmn.gam_simple <- gam(
  soilmn ~ s(map) + s(map_aet) + s(ph) + s(soilmoisture),
  data = gradient.df,
  family = Gamma(link = "log"),
  method = "REML"
)

grassmn.gam_simple <- gam(
  grassmn ~ s(map) + s(map_aet) + s(ph) + s(soilmoisture),
  data = gradient.df,
  family = Gamma(link = "log"),
  method = "REML"
)

extractmn.gam_simple <- gam(
  extractmn ~ s(map) + s(map_aet) + s(ph) + s(soilmoisture),
  data = gradient.df,
  family = Gamma(link = "log"),
  method = "REML"
)

# ------------------------------------------------------------
# Function to calculate explained deviance
# ------------------------------------------------------------
explained_deviance <- function(model, data, response_var) {
  y <- data[[response_var]]
  
  # Predicted response on original scale
  mu <- predict(model, newdata = data, type = "response")
  
  # Family object
  fam <- family(model)
  
  # Residual deviance from fitted predictions
  resid_dev <- sum(fam$dev.resids(y = y, mu = mu, wt = rep(1, length(y))))
  
  # Null deviance from intercept-only model
  mu_null <- rep(mean(y, na.rm = TRUE), length(y))
  null_dev <- sum(fam$dev.resids(y = y, mu = mu_null, wt = rep(1, length(y))))
  
  # Explained deviance
  1 - (resid_dev / null_dev)
}

# ------------------------------------------------------------
# Permutation importance function
# ------------------------------------------------------------
permutation_importance <- function(model, data, response_var, predictors,
                                   nperm = 10000, seed = 123) {
  set.seed(seed)
  
  baseline_devexp <- explained_deviance(model, data, response_var)
  
  results <- vector("list", length(predictors))
  names(results) <- predictors
  
  for (v in predictors) {
    drops <- numeric(nperm)
    
    for (i in seq_len(nperm)) {
      perm_data <- data
      perm_data[[v]] <- sample(perm_data[[v]], replace = FALSE)
      
      perm_devexp <- explained_deviance(model, perm_data, response_var)
      drops[i] <- baseline_devexp - perm_devexp
    }
    
    results[[v]] <- data.frame(
      response = response_var,
      variable = v,
      permutation = seq_len(nperm),
      importance = drops
    )
  }
  
  bind_rows(results)
}

# ------------------------------------------------------------
# Predictor names
# ------------------------------------------------------------
predictors <- c("map", "map_aet", "ph", "soilmoisture")

# ------------------------------------------------------------
# Run permutation importance for all three GAMs
# ------------------------------------------------------------
vip_soilmn <- permutation_importance(
  model = soilmn.gam_simple,
  data = gradient.df,
  response_var = "soilmn",
  predictors = predictors,
  nperm = 10000,
  seed = 42
)

vip_grassmn <- permutation_importance(
  model = grassmn.gam_simple,
  data = gradient.df,
  response_var = "grassmn",
  predictors = predictors,
  nperm = 10000,
  seed = 42
)

vip_extractmn <- permutation_importance(
  model = extractmn.gam_simple,
  data = gradient.df,
  response_var = "extractmn",
  predictors = predictors,
  nperm = 10000,
  seed = 42
)

vip_all <- bind_rows(vip_soilmn, vip_grassmn, vip_extractmn)

##Pairwise permutation tests

library(dplyr)

# Function to test MAP vs each other predictor
test_map_importance <- function(data, response_name) {
  
  df <- data %>% filter(response == response_name)
  
  map_vals <- df %>% filter(variable == "map") %>% pull(importance)
  
  other_vars <- setdiff(unique(df$variable), "map")
  
  results <- lapply(other_vars, function(v) {
    
    other_vals <- df %>% filter(variable == v) %>% pull(importance)
    
    test <- wilcox.test(map_vals, other_vals, alternative = "greater")
    
    data.frame(
      response = response_name,
      comparison = paste("MAP >", v),
      p_value = test$p.value
    )
  })
  
  bind_rows(results)
}

# Run tests for each response variable
test_soilmn <- test_map_importance(vip_all, "soilmn")
test_grassmn <- test_map_importance(vip_all, "grassmn")
test_extractmn <- test_map_importance(vip_all, "extractmn")

map_tests <- bind_rows(test_soilmn, test_grassmn, test_extractmn)

# Correct for multiple comparisons
map_tests <- map_tests %>%
  mutate(p_adj = p.adjust(p_value, method = "BH"))

print(map_tests)

# ------------------------------------------------------------
# Summarize mean importance and raw 95% CIs
# ------------------------------------------------------------
vip_summary <- vip_all %>%
  group_by(response, variable) %>%
  summarise(
    mean_importance = mean(importance, na.rm = TRUE),
    sd_importance   = sd(importance, na.rm = TRUE),
    lower_ci        = quantile(importance, 0.025, na.rm = TRUE),
    upper_ci        = quantile(importance, 0.975, na.rm = TRUE),
    .groups = "drop"
  )

# ------------------------------------------------------------
# Normalize mean importance and CIs within each response
#    so that scores range from 0 to 1
# ------------------------------------------------------------
vip_summary <- vip_summary %>%
  group_by(response) %>%
  mutate(
    min_mean = min(mean_importance, na.rm = TRUE),
    max_mean = max(mean_importance, na.rm = TRUE),
    range_mean = max_mean - min_mean,
    norm_importance = ifelse(
      range_mean == 0,
      0,
      (mean_importance - min_mean) / range_mean
    ),
    norm_lower_ci = ifelse(
      range_mean == 0,
      0,
      (lower_ci - min_mean) / range_mean
    ),
    norm_upper_ci = ifelse(
      range_mean == 0,
      0,
      (upper_ci - min_mean) / range_mean
    )
  ) %>%
  ungroup() %>%
  select(response, variable, mean_importance, sd_importance,
         lower_ci, upper_ci, norm_importance, norm_lower_ci, norm_upper_ci) %>%
  arrange(response, desc(norm_importance))

print(vip_summary)

# ------------------------------------------------------------
# Write summary table to CSV
# ------------------------------------------------------------
write.csv(
  vip_summary,
  file = "Mn_permutation_variable_importance_summary.csv",
  row.names = FALSE
)

# ------------------------------------------------------------
# Plot normalized importance with normalized 95% CIs
#    (this is the confidence-interval modification)
# ------------------------------------------------------------

vip_summary_plot <- vip_summary %>%
  mutate(
    variable = recode(variable,
                      map = "MAP",
                      map_aet = "MAP-AET",
                      ph = "pH",
                      soilmoisture = "Soil Moisture"
    ),
    
    response = recode(response,
                      soilmn = "Soil Mn",
                      grassmn = "Grass Mn",
                      extractmn = "PP Mn"
    )
  )



plot_all_norm <- vip_summary_plot %>%
  ggplot(aes(x = reorder(variable, norm_importance),
             y = norm_importance,
             fill = response)) +
  geom_col() +
  geom_errorbar(
    aes(ymin = norm_lower_ci, ymax = norm_upper_ci),
    width = 0.2,
    color = "black"
  ) +
  coord_flip() +
  facet_wrap(~ response, scales = "free_y") +
  
  # Set colors for each Mn pool
  scale_fill_manual(
    values = c(
      "Grass Mn" = "#C97B84",                        # rose
      "PP Mn" = "#E6D3A3",  # beige
      "Soil Mn" = "#3A3A3A"                          # charcoal
    )
  ) +
  
  labs(
    x = "Predictor",
    y = "Normalized importance (0–1)"
  ) +
  theme_classic(base_size = 12) +
  theme(
    text = element_text(color = "black", size = 12, family = "Arial"),
    axis.text = element_text(color = "black", size = 12),
    axis.title = element_text(color = "black", size = 12),
    strip.text = element_text(color = "black", size = 12),
    strip.background = element_blank(),
    legend.position = "none"
  )

print(plot_all_norm)
# ------------------------------------------------------------
# Save publication-quality figure
# ------------------------------------------------------------

ggsave(
  filename = "Mn_permutation_importance_plot_improved.png",
  plot = plot_all_norm,
  width = 8,
  height = 6,
  dpi = 600
)



# ------------------------------------------------------------
# 1. Make significance labels and match response names exactly
# ------------------------------------------------------------
map_sig_summary <- map_tests %>%
  group_by(response) %>%
  summarise(
    map_label = case_when(
      all(p_adj < 0.001, na.rm = TRUE) ~ "***",
      all(p_adj < 0.01,  na.rm = TRUE) ~ "**",
      all(p_adj < 0.05,  na.rm = TRUE) ~ "*",
      TRUE ~ "ns"
    ),
    .groups = "drop"
  ) %>%
  mutate(
    response = recode(response,
                      soilmn = "Soil Mn",
                      grassmn = "Grass Mn",
                      extractmn = "Pyrophosphate-\nextractable Mn"
    )
  )

# ------------------------------------------------------------
# 2. Build star annotation table and remove bad rows
# ------------------------------------------------------------
# ------------------------------------------------------------
# Build star annotation table with panel-specific offset
# ------------------------------------------------------------
map_star_df <- vip_summary_plot %>%
  group_by(response) %>%
  mutate(
    panel_range = max(norm_upper_ci, na.rm = TRUE) - min(norm_lower_ci, na.rm = TRUE),
    panel_offset = ifelse(panel_range == 0, 0.10, panel_range * 0.25)
  ) %>%
  ungroup() %>%
  filter(variable == "MAP") %>%
  left_join(map_sig_summary, by = "response") %>%
  mutate(
    map_label = ifelse(is.na(map_label), "ns", map_label),
    star_y = norm_upper_ci + panel_offset
  ) %>%
  filter(!is.na(variable), !is.na(star_y), !is.na(map_label))

print(map_star_df)

# ------------------------------------------------------------
# 3. Compute plot upper limit with headroom
# ------------------------------------------------------------
y_top <- max(c(vip_summary_plot$norm_upper_ci, map_star_df$star_y), na.rm = TRUE) + 0.03

# ------------------------------------------------------------
# 4. Plot with stars
# ------------------------------------------------------------
plot_all_norm_sig <- vip_summary_plot %>%
  ggplot(aes(x = reorder(variable, norm_importance),
             y = norm_importance,
             fill = response)) +
  geom_col() +
  geom_errorbar(
    aes(ymin = norm_lower_ci, ymax = norm_upper_ci),
    width = 0.2,
    color = "black"
  ) +
  geom_text(
    data = map_star_df,
    aes(x = variable, y = star_y, label = map_label),
    inherit.aes = FALSE,
    size = 4,
    color = "black"
  ) +
  coord_flip(clip = "off") +
  facet_wrap(~ response, scales = "free_y") +
  scale_fill_manual(
    values = c(
      "Grass Mn" = "#C97B84",
      "PP Mn" = "#E6D3A3",
      "Soil Mn" = "#3A3A3A"
    )
  ) +
  scale_y_continuous(
    n.breaks = 3,
    expand = expansion(mult = c(0, 0.35))
  ) +
  labs(
    x = "Predictor",
    y = "Normalized importance (0–1)"
  ) +
  theme_classic(base_size = 12) +
  theme(
    text = element_text(color = "black", size = 12),
    axis.text = element_text(color = "black", size = 12),
    axis.title = element_text(color = "black", size = 12),
    strip.text = element_text(color = "black", size = 12),
    strip.background = element_blank(),
    legend.position = "none",
    plot.margin = margin(10, 25, 10, 10)
  )

print(plot_all_norm_sig)

# ------------------------------------------------------------
# 5. Save high-quality exports without clipping
# ------------------------------------------------------------
ggsave(
  filename = "Mn_permutation_importance_with_significance.png",
  plot = plot_all_norm_sig,
  width = 8,
  height = 6,
  dpi = 600,
  bg = "white"
)


##########################################################################################################################
########################################################################################################################
# ============================================================
# Bootstrap dominance analysis for three GAMs
# Responses:
#   1) soilmn
#   2) grassmn
#   3) extractmn
#
# For each bootstrap sample:
#   - resample rows with replacement
#   - refit GAM
#   - calculate permutation variable importance
#   - rank predictors
#
# Outputs:
#   - bootstrap importance table
#   - summary table with probability of being best predictor
#   - CSV exports
#   - dominance plot
# ============================================================

# -----------------------------
# Packages
# -----------------------------
library(mgcv)
library(dplyr)
library(tidyr)
library(ggplot2)

# -----------------------------
# User settings
# -----------------------------
predictors <- c("map", "map_aet", "ph", "soilmoisture")
n_boot <- 500       # increase to 1000 for final analysis
n_perm <- 100       # increase to 500 or 1000 for final analysis
set.seed(42)

# -----------------------------
# 1. Function: explained deviance
# -----------------------------
explained_deviance <- function(model, data, response_var) {
  y <- data[[response_var]]
  mu <- predict(model, newdata = data, type = "response")
  fam <- family(model)
  
  resid_dev <- sum(fam$dev.resids(y = y, mu = mu, wt = rep(1, length(y))))
  mu_null <- rep(mean(y, na.rm = TRUE), length(y))
  null_dev <- sum(fam$dev.resids(y = y, mu = mu_null, wt = rep(1, length(y))))
  
  1 - (resid_dev / null_dev)
}

# -----------------------------
# 2. Function: permutation importance for one fitted model
# -----------------------------
permutation_importance_once <- function(model, data, response_var, predictors,
                                        nperm = 100, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  
  baseline_devexp <- explained_deviance(model, data, response_var)
  
  out <- lapply(predictors, function(v) {
    drops <- numeric(nperm)
    
    for (i in seq_len(nperm)) {
      perm_data <- data
      perm_data[[v]] <- sample(perm_data[[v]], replace = FALSE)
      
      perm_devexp <- explained_deviance(model, perm_data, response_var)
      drops[i] <- baseline_devexp - perm_devexp
    }
    
    data.frame(
      variable = v,
      mean_importance = mean(drops, na.rm = TRUE),
      median_importance = median(drops, na.rm = TRUE),
      lower_ci = quantile(drops, 0.025, na.rm = TRUE),
      upper_ci = quantile(drops, 0.975, na.rm = TRUE)
    )
  })
  
  bind_rows(out)
}

# -----------------------------
# 3. Function: fit one GAM
# -----------------------------
fit_gam_model <- function(data, response_var) {
  form <- as.formula(
    paste0(response_var, " ~ s(map) + s(map_aet) + s(ph) + s(soilmoisture)")
  )
  
  gam(
    formula = form,
    data = data,
    family = Gamma(link = "log"),
    method = "REML"
  )
}

# -----------------------------
# 4. Function: bootstrap dominance for one response
# -----------------------------
bootstrap_dominance <- function(data, response_var, predictors,
                                n_boot = 500, n_perm = 100, seed = 42) {
  set.seed(seed)
  
  boot_results <- vector("list", n_boot)
  
  for (b in seq_len(n_boot)) {
    # bootstrap resample rows
    idx <- sample(seq_len(nrow(data)), size = nrow(data), replace = TRUE)
    boot_data <- data[idx, , drop = FALSE]
    
    # fit GAM
    model_b <- try(
      fit_gam_model(boot_data, response_var),
      silent = TRUE
    )
    
    if (inherits(model_b, "try-error")) {
      boot_results[[b]] <- NULL
      next
    }
    
    # permutation importance within this bootstrap sample
    vi_b <- try(
      permutation_importance_once(
        model = model_b,
        data = boot_data,
        response_var = response_var,
        predictors = predictors,
        nperm = n_perm,
        seed = seed + b
      ),
      silent = TRUE
    )
    
    if (inherits(vi_b, "try-error")) {
      boot_results[[b]] <- NULL
      next
    }
    
    # rank within bootstrap replicate
    vi_b <- vi_b %>%
      arrange(desc(mean_importance)) %>%
      mutate(
        response = response_var,
        bootstrap = b,
        rank = row_number(),
        best = rank == 1
      )
    
    boot_results[[b]] <- vi_b
  }
  
  bind_rows(boot_results)
}

# -----------------------------
# 5. Run bootstrap dominance analysis
# -----------------------------
boot_soilmn <- bootstrap_dominance(
  data = gradient.df,
  response_var = "soilmn",
  predictors = predictors,
  n_boot = n_boot,
  n_perm = n_perm,
  seed = 100
)

boot_grassmn <- bootstrap_dominance(
  data = gradient.df,
  response_var = "grassmn",
  predictors = predictors,
  n_boot = n_boot,
  n_perm = n_perm,
  seed = 200
)

boot_extractmn <- bootstrap_dominance(
  data = gradient.df,
  response_var = "extractmn",
  predictors = predictors,
  n_boot = n_boot,
  n_perm = n_perm,
  seed = 300
)

boot_all <- bind_rows(boot_soilmn, boot_grassmn, boot_extractmn)

# -----------------------------
# 6. Summarize dominance results
# -----------------------------
dominance_summary <- boot_all %>%
  group_by(response, variable) %>%
  summarise(
    mean_importance = mean(mean_importance, na.rm = TRUE),
    median_importance = median(mean_importance, na.rm = TRUE),
    sd_importance = sd(mean_importance, na.rm = TRUE),
    lower_boot_ci = quantile(mean_importance, 0.025, na.rm = TRUE),
    upper_boot_ci = quantile(mean_importance, 0.975, na.rm = TRUE),
    prob_best = mean(best, na.rm = TRUE),
    mean_rank = mean(rank, na.rm = TRUE),
    n_boot_success = n(),
    .groups = "drop"
  ) %>%
  arrange(response, desc(prob_best), desc(mean_importance))

print(dominance_summary)

# -----------------------------
# 7. Add normalized importance within response
# -----------------------------
dominance_summary <- dominance_summary %>%
  group_by(response) %>%
  mutate(
    norm_importance =
      (mean_importance - min(mean_importance)) /
      (max(mean_importance) - min(mean_importance))
  ) %>%
  ungroup()

print(dominance_summary)

# -----------------------------
# 8. Recode labels for publication-ready tables/plots
# -----------------------------
dominance_summary_plot <- dominance_summary %>%
  mutate(
    variable = recode(variable,
                      map = "MAP",
                      map_aet = "MAP-AET",
                      ph = "pH",
                      soilmoisture = "Soil Moisture"
    ),
    response = recode(response,
                      soilmn = "Soil Mn",
                      grassmn = "Grass Mn",
                      extractmn = "Pyrophosphate-\nextractable Mn"
    )
  )

# -----------------------------
# 9. Write CSV files
# -----------------------------
write.csv(
  boot_all,
  file = "Mn_bootstrap_dominance_all_iterations.csv",
  row.names = FALSE
)

write.csv(
  dominance_summary,
  file = "Mn_bootstrap_dominance_summary.csv",
  row.names = FALSE
)

# -----------------------------
# 10. Plot: probability of being the best predictor
# -----------------------------
plot_prob_best <- dominance_summary_plot %>%
  ggplot(aes(x = reorder(variable, prob_best), y = prob_best, fill = response)) +
  geom_col() +
  geom_errorbar(
    aes(
      ymin = pmax(prob_best - 0, 0),
      ymax = pmin(prob_best + 0, 1)
    ),
    width = 0.2,
    alpha = 0
  ) +
  coord_flip() +
  facet_wrap(~ response, scales = "free_y") +
  scale_fill_manual(
    values = c(
      "Grass Mn" = "#C97B84",
      "PP Mn" = "#E6D3A3",
      "Soil Mn" = "#3A3A3A"
    )
  ) +
  labs(
    x = "Predictor",
    y = "Probability of being best predictor"
  ) +
  theme_classic(base_size = 12) +
  theme(
    text = element_text(color = "black", size = 12),
    axis.text = element_text(color = "black", size = 12),
    axis.title = element_text(color = "black", size = 12),
    strip.text = element_text(color = "black", size = 12),
    strip.background = element_blank(),
    legend.position = "none"
  )

print(plot_prob_best)

# -----------------------------
# 11. Plot: mean bootstrap importance with 95% bootstrap CI
# -----------------------------
plot_boot_importance <- dominance_summary_plot %>%
  ggplot(aes(x = reorder(variable, mean_importance), y = mean_importance, fill = response)) +
  geom_col() +
  geom_errorbar(
    aes(ymin = lower_boot_ci, ymax = upper_boot_ci),
    width = 0.2,
    color = "black"
  ) +
  coord_flip() +
  facet_wrap(~ response, scales = "free_y") +
  scale_fill_manual(
    values = c(
      "Grass Mn" = "#C97B84",
      "Pyrophosphate-\nextractable Mn" = "#E6D3A3",
      "Soil Mn" = "#3A3A3A"
    )
  ) +
  labs(
    x = "Predictor",
    y = "Bootstrap mean permutation importance"
  ) +
  theme_classic(base_size = 12) +
  theme(
    text = element_text(color = "black", size = 12),
    axis.text = element_text(color = "black", size = 12),
    axis.title = element_text(color = "black", size = 12),
    strip.text = element_text(color = "black", size = 12),
    strip.background = element_blank(),
    legend.position = "none"
  )

print(plot_boot_importance)

# -----------------------------
# 12. Save plots
# -----------------------------
ggsave(
  "Mn_bootstrap_dominance_prob_best.png",
  plot_prob_best,
  width = 8,
  height = 6,
  dpi = 600
)

ggsave(
  "Mn_bootstrap_dominance_mean_importance.png",
  plot_boot_importance,
  width = 8,
  height = 6,
  dpi = 600
)



# ============================================================
# Faster bootstrap dominance analysis for three GAMs
# ============================================================

library(mgcv)
library(dplyr)
library(ggplot2)

# ------------------------------------------------------------
# Settings
# ------------------------------------------------------------
predictors <- c("map", "map_aet", "ph", "soilmoisture")
#fast test
#n_boot <- 200      
#n_perm <- 20       
#set.seed(42)
n_boot <- 1000       # increase to 1000 for final analysis
n_perm <- 1000       # increase to 500 or 1000 for final analysis
set.seed(42)

# ------------------------------------------------------------
# 1. Explained deviance
# ------------------------------------------------------------
explained_deviance <- function(model, data, response_var) {
  y <- data[[response_var]]
  mu <- predict(model, newdata = data, type = "response")
  fam <- family(model)
  
  resid_dev <- sum(fam$dev.resids(y = y, mu = mu, wt = rep(1, length(y))))
  mu_null <- rep(mean(y, na.rm = TRUE), length(y))
  null_dev <- sum(fam$dev.resids(y = y, mu = mu_null, wt = rep(1, length(y))))
  
  1 - (resid_dev / null_dev)
}

# ------------------------------------------------------------
# 2. Fit GAM
# ------------------------------------------------------------
fit_gam_model <- function(data, response_var) {
  form <- as.formula(
    paste0(response_var, " ~ s(map, k = 4) + s(map_aet, k = 4) + s(ph, k = 4) + s(soilmoisture, k = 4)")
  )
  
  gam(
    formula = form,
    data = data,
    family = Gamma(link = "log"),
    method = "REML",
    select = TRUE
  )
}

# ------------------------------------------------------------
# 3. Permutation importance for one fitted model
# ------------------------------------------------------------
permutation_importance_once <- function(model, data, response_var, predictors,
                                        nperm = 20, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  
  baseline_devexp <- explained_deviance(model, data, response_var)
  
  out <- lapply(predictors, function(v) {
    drops <- numeric(nperm)
    
    for (i in seq_len(nperm)) {
      perm_data <- data
      perm_data[[v]] <- sample(perm_data[[v]], replace = FALSE)
      
      perm_devexp <- explained_deviance(model, perm_data, response_var)
      drops[i] <- baseline_devexp - perm_devexp
    }
    
    data.frame(
      variable = v,
      mean_importance = mean(drops, na.rm = TRUE)
    )
  })
  
  bind_rows(out)
}

# ------------------------------------------------------------
# 4. Bootstrap dominance for one response
# ------------------------------------------------------------
bootstrap_dominance_fast <- function(data, response_var, predictors,
                                     n_boot = 200, n_perm = 20, seed = 42) {
  set.seed(seed)
  
  results <- vector("list", n_boot)
  
  for (b in seq_len(n_boot)) {
    if (b %% 10 == 0) {
      message("[", response_var, "] bootstrap ", b, " / ", n_boot)
    }
    
    idx <- sample(seq_len(nrow(data)), size = nrow(data), replace = TRUE)
    boot_data <- data[idx, , drop = FALSE]
    
    model_b <- try(
      fit_gam_model(boot_data, response_var),
      silent = TRUE
    )
    
    if (inherits(model_b, "try-error")) {
      results[[b]] <- NULL
      next
    }
    
    vi_b <- try(
      permutation_importance_once(
        model = model_b,
        data = boot_data,
        response_var = response_var,
        predictors = predictors,
        nperm = n_perm,
        seed = seed + b
      ),
      silent = TRUE
    )
    
    if (inherits(vi_b, "try-error")) {
      results[[b]] <- NULL
      next
    }
    
    vi_b <- vi_b %>%
      arrange(desc(mean_importance)) %>%
      mutate(
        response = response_var,
        bootstrap = b,
        rank = row_number(),
        best = rank == 1
      )
    
    results[[b]] <- vi_b
  }
  
  bind_rows(results)
}

# ------------------------------------------------------------
# 5. Run the analysis
# ------------------------------------------------------------
boot_soilmn <- bootstrap_dominance_fast(
  data = gradient.df,
  response_var = "soilmn",
  predictors = predictors,
  n_boot = n_boot,
  n_perm = n_perm,
  seed = 100
)

boot_grassmn <- bootstrap_dominance_fast(
  data = gradient.df,
  response_var = "grassmn",
  predictors = predictors,
  n_boot = n_boot,
  n_perm = n_perm,
  seed = 200
)

boot_extractmn <- bootstrap_dominance_fast(
  data = gradient.df,
  response_var = "extractmn",
  predictors = predictors,
  n_boot = n_boot,
  n_perm = n_perm,
  seed = 300
)

boot_all <- bind_rows(boot_soilmn, boot_grassmn, boot_extractmn)

# ------------------------------------------------------------
# 6. Summarize
# ------------------------------------------------------------
dominance_summary <- boot_all %>%
  group_by(response, variable) %>%
  summarise(
    mean_importance = mean(mean_importance, na.rm = TRUE),
    median_importance = median(mean_importance, na.rm = TRUE),
    sd_importance = sd(mean_importance, na.rm = TRUE),
    lower_boot_ci = quantile(mean_importance, 0.025, na.rm = TRUE),
    upper_boot_ci = quantile(mean_importance, 0.975, na.rm = TRUE),
    prob_best = mean(best, na.rm = TRUE),
    mean_rank = mean(rank, na.rm = TRUE),
    n_boot_success = n(),
    .groups = "drop"
  ) %>%
  arrange(response, desc(prob_best), desc(mean_importance))

print(dominance_summary)

# ------------------------------------------------------------
# Add binomial confidence intervals for probability of being best
# ------------------------------------------------------------
dominance_summary <- dominance_summary %>%
  rowwise() %>%
  mutate(
    prob_best_lower = prop.test(
      x = round(prob_best * n_boot_success),
      n = n_boot_success,
      correct = FALSE
    )$conf.int[1],
    prob_best_upper = prop.test(
      x = round(prob_best * n_boot_success),
      n = n_boot_success,
      correct = FALSE
    )$conf.int[2]
  ) %>%
  ungroup()

# ------------------------------------------------------------
# 7. Recode for plotting
# ------------------------------------------------------------
dominance_summary_plot <- dominance_summary %>%
  mutate(
    variable = recode(variable,
                      map = "MAP",
                      map_aet = "MAP-AET",
                      ph = "pH",
                      soilmoisture = "Soil Moisture"
    ),
    response = recode(response,
                      soilmn = "Soil Mn",
                      grassmn = "Grass Mn",
                      extractmn = "PP Mn"
    )
  )

# ------------------------------------------------------------
# 8. Plot probability of being best predictor
# ------------------------------------------------------------
plot_prob_best <- dominance_summary_plot %>%
  ggplot(aes(x = reorder(variable, prob_best), y = prob_best, fill = response)) +
  geom_col() +
  geom_errorbar(
    aes(ymin = prob_best_lower, ymax = prob_best_upper),
    width = 0.2,
    color = "black"
  ) +
  coord_flip() +
  facet_wrap(~ response, scales = "free_y") +
  scale_fill_manual(
    values = c(
      "Grass Mn" = "#C97B84",
      "PP Mn" = "#E6D3A3",
      "Soil Mn" = "#3A3A3A"
    )
  ) +
  labs(
    x = "Predictor",
    y = "Probability of being best predictor"
  ) +
  theme_classic(base_size = 12) +
  theme(
    text = element_text(color = "black", size = 12),
    axis.text = element_text(color = "black", size = 12),
    axis.title = element_text(color = "black", size = 12),
    strip.text = element_text(color = "black", size = 12),
    strip.background = element_blank(),
    legend.position = "none"
  )

print(plot_prob_best)

ggsave(
  "Mn_bootstrap_dominance_prob_best_CI.png",
  plot_prob_best,
  width = 8,
  height = 6,
  dpi = 600
)



# ------------------------------------------------------------
# 9. Save outputs
# ------------------------------------------------------------
write.csv(
  boot_all,
  file = "Mn_bootstrap_dominance_all_iterations.csv",
  row.names = FALSE
)

write.csv(
  dominance_summary,
  file = "Mn_bootstrap_dominance_summary.csv",
  row.names = FALSE
)


################################################################################################
#Diagnostic check: frequency that MAP is the top predictor
# Proportion of bootstrap runs in which each predictor ranked first
boot_all %>%
  group_by(response, variable) %>%
  summarise(
    n_best = sum(rank == 1, na.rm = TRUE),
    n_total = n(),
    prob_best = n_best / n_total,
    .groups = "drop"
  ) %>%
  arrange(response, desc(prob_best))

#Quick visual check of the bootstrap analysis
boot_all %>%
  mutate(
    variable = recode(variable,
                      map = "MAP",
                      map_aet = "MAP-AET",
                      ph = "pH",
                      soilmoisture = "Soil Moisture"
    ),
    response = recode(response,
                      soilmn = "Soil Mn",
                      grassmn = "Grass Mn",
                      extractmn = "PP Mn"
    )
  ) %>%
  group_by(response, variable) %>%
  summarise(prob_best = mean(rank == 1, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = variable, y = prob_best)) +
  geom_col() +
  facet_wrap(~ response) +
  theme_classic() +
  labs(x = "Predictor", y = "Probability of ranking first")



boot_all %>%
  mutate(
    variable = recode(variable,
                      map = "MAP",
                      map_aet = "MAP-AET",
                      ph = "pH",
                      soilmoisture = "Soil Moisture"
    ),
    response = recode(response,
                      soilmn = "Soil Mn",
                      grassmn = "Grass Mn",
                      extractmn = "PP Mn"
    )
  ) %>%
  group_by(response, variable) %>%
  summarise(prob_best = mean(rank == 1, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = variable, y = prob_best, fill = response)) +
  geom_col() +
  facet_wrap(~ response) +
  scale_fill_manual(
    values = c(
      "Grass Mn" = "#C97B84",
      "PP Mn" = "#E6D3A3",
      "Soil Mn" = "#3A3A3A"                        
    )
  ) +
  theme_classic(base_size = 12) +
  theme(
    text = element_text(color = "black", size = 12),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    strip.text = element_text(color = "black"),
    strip.background = element_blank(),
    legend.position = "none"
  ) +
  labs(
    x = "Predictor",
    y = "Probability of ranking first"
  )

library(dplyr)
library(ggplot2)

# ------------------------------------------------------------
# Summarize probability of ranking first and add 95% CIs
# ------------------------------------------------------------
prob_best_df <- boot_all %>%
  mutate(
    variable = recode(variable,
                      map = "MAP",
                      map_aet = "MAP-AET",
                      ph = "pH",
                      soilmoisture = "Soil Moisture"
    ),
    response = recode(response,
                      soilmn = "Soil Mn",
                      grassmn = "Grass Mn",
                      extractmn = "PP Mn"
    )
  ) %>%
  group_by(response, variable) %>%
  summarise(
    n_best = sum(rank == 1, na.rm = TRUE),
    n_total = n(),
    prob_best = n_best / n_total,
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    ci = list(prop.test(n_best, n_total, correct = FALSE)$conf.int),
    lower_ci = ci[[1]][1],
    upper_ci = ci[[1]][2]
  ) %>%
  ungroup() %>%
  select(-ci)

print(prob_best_df)

# ------------------------------------------------------------
# Plot probability of ranking first with 95% CIs
# ------------------------------------------------------------
plot_prob_best <- ggplot(
  prob_best_df,
  aes(x = variable, y = prob_best, fill = response)
) +
  geom_col() +
  geom_errorbar(
    aes(ymin = lower_ci, ymax = upper_ci),
    width = 0.2,
    color = "black"
  ) +
  coord_flip() +
   facet_wrap(~ response) +
  scale_fill_manual(
    values = c(
      "Grass Mn" = "#C97B84",
      "PP Mn" = "#E6D3A3",
      "Soil Mn" = "#3A3A3A"  
    )
  ) +
  theme_classic(base_size = 14) +
  theme(
    text = element_text(color = "black", size = 14),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    strip.text = element_text(color = "black"),
    strip.background = element_blank(),
    legend.position = "none"
  ) +
  labs(
    x = "Predictor",
    y = "Probability of ranking first"
  )

print(plot_prob_best)

# ------------------------------------------------------------
# Save high-quality PNG
# ------------------------------------------------------------
ggsave(
  filename = "Mn_bootstrap_dominance_probability_CI.png",
  plot = plot_prob_best,
  width = 8,
  height = 6,
  dpi = 600,
)

