# Code to combine all data and run analyses

library(tidyverse)
library(corrplot)
library(ggbiplot)
library(ordinal)
library(ggeffects)

#### Wrangle Response Variable ####
# First wrangle response variable: individual FAGR stem BLD severity metrics
# Basal area only includes trees >= 5 inches DBH

main = read.csv("./Raw.Data/Survey_Data/main_stem_severity.csv") %>% 
  dplyr::select(plot_type, plot_id, plot_id_new, date, species, dbh, crown_class,
                overall_dieback, percent_bld, dominant_symptom_expression,comments)
# deleted a degree symbol from notes of row 892 b/c RStudio wouldn't display dataframe
subplot = read.csv("./Raw.Data/Survey_Data/subplot_stem_severity.csv") %>% 
  dplyr::select(plot_type, plot_id, plot_id_new, date, species, dbh, dieback, 
                percent_infected_bld, dominant_symptom_expression,remarks)
# add the crown_class column to subplot, all are I
subplot$crown_class = "I"

# rename columns so merge works
subplot =  subplot %>% 
  rename(overall_dieback = dieback,
         percent_bld = percent_infected_bld,
         comments = remarks)

# merge main and subplot
all.stems = full_join(main,subplot)

# need to change FRGA to FAGR
all.stems$species[all.stems$species == "FRGA"] <- "FAGR"

# slim down to just FAGR for 2025
FAGR.stems = all.stems %>% 
  filter(species == "FAGR",
         date == 2025)
# (403) will slim down further to the ones in Holden plots

# write.csv(FAGR.stems, file = "./Formatted.Data/FAGR.stems.csv")

#### Wrangle plots data ####
# Should be 35 plots censuses at Holden in 2025
# I think we need to keep the analysis to only these plots or we have to account
# for differences in the infection timing of plots across Ohio, PA, and WV.

clim = read.csv("./Formatted.Data/BLD.plot.variables.csv", row.names = 1) %>% 
  dplyr::select(-date)

# add leading zeros to plot IDs to match other data sheets
clim <- clim %>%
  mutate(plot_id = trimws(plot_id)) %>% 
  mutate(
    parts  = str_match(plot_id, "^([A-Za-z]+)-(\\d+)([A-Za-z]*)$"),
    prefix = parts[, 2],
    num    = parts[, 3],
    suffix = parts[, 4],
    plot_id_new = paste0(prefix, "-", str_pad(num, width = 2, pad = "0"), suffix)
  ) %>%
  dplyr::select(-parts, -prefix, -num, -suffix) %>% 
  relocate(plot_id_new, .after = plot_id) 

BA = read.csv("./Formatted.Data/BLD_ForestMonitoring_PlotBasalArea_2025_RS.csv", row.names = 1) %>% 
  dplyr::select(plot_id_new,plot_id,plot_type,date,plot_area_m2,living_basal_area_m2,living_n_trees,
         fagus_basal_area_m2,fagus_n_trees,nonfagus_basal_area_m2)

# filter for just main plot
BA.main = BA %>% 
  filter(plot_type == "Main")

# merge clim and BA, only keeping plots in clim b/c on Holden property
plot.data = left_join(clim,BA.main)
 
# write.csv(plot.data, file = "./Formatted.Data/plot_data.csv")

#### All Data Merge ####

# filter plots from FAGR that aren't on Holden property
FAGR.stems.2 = FAGR.stems %>% 
  filter(plot_id_new %in% plot.data$plot_id_new)

all.data = left_join(FAGR.stems.2,plot.data, by = "plot_id_new")

# write.csv(all.data, file = "./Formatted.Data/all.data.csv")

#### Analyses ####
# Two versions of analyses 1: all plots, 2: only APEX control plots
# control plots are: PC-01, PC-06, PC-12, SC-05, SC-09, SC-11,

all.data = read.csv("./Formatted.Data/all.data.csv", row.names = 1)

# response: overall_dieback, percent_bld, dominant_symptom_expression

cor.test(all.data$overall_dieback,all.data$percent_bld)
# r = 0.22, p = 0.002223
cor.test(all.data$overall_dieback,all.data$percent_bld, method = "spearman")
# r = 0.23, p = 0.001117

cor.test(all.data$overall_dieback,all.data$dominant_symptom_expression)
# r = 0.21, p = 0.003189
cor.test(all.data$overall_dieback,all.data$dominant_symptom_expression, method = "spearman")
# r = 0.25, p = 0.0004238

cor.test(all.data$percent_bld,all.data$dominant_symptom_expression)
# r = 0.73, p = < 2.2e-16
cor.test(all.data$percent_bld,all.data$dominant_symptom_expression, method = "spearman")
# r = 0.65, p = < 2.2e-16

ggplot(all.data, aes(x = overall_dieback,y = percent_bld))+
  geom_point()
ggplot(all.data, aes(x = overall_dieback,y = dominant_symptom_expression))+
  geom_point()
ggplot(all.data, aes(x = percent_bld,y = dominant_symptom_expression))+
  geom_point()

range(all.data$overall_dieback) # 2 - 6
range(all.data$percent_bld) # 0 - 4
range(all.data$dominant_symptom_expression) # 0 - 4

# correlation among environmental predictors
plot.data = all.data %>%
  distinct(plot_id_new, .keep_all = TRUE)

# scale and center plot data
plot.data = plot.data %>%
  mutate(across(c(12:25,43,45,47), 
                ~ as.numeric(scale(.)), 
                .names = "{.col}_scaled"))

# correlation among basal area predictors
cor.BA = cor(plot.data[,c(62,63,64)], use = "pairwise")
corrplot(cor.BA,method = "number")
# living ~ nonfagus = 0.82
# living ~ fagus = 0.02
# fagus ~ nonfagus = -0.55

# macro_bio1 = Mean Annual Temperature
# macro_bio5 = Max Temp of Warmest Month
# macro_bio6 = Min Temp of Coldest Month
# micro_bio12 = Total Annual Precipitation
# northness = ranges from -1 to 1, how steeply a site is inclined toward the north
# larger negative values = steeper south slopes
# eastness = does the same as northness for the east-west direction, 
# a plot with southeast aspect would have positive eastness and negative northness
# mTPI = how a site's elevation compares to the elevation of the surrounding neighborhood
# positive values = hilltops, negative values = valley bottoms
# high temp = summer daytime max temperature
# low temp = winter nighttime minimum temperature
# annual precipitation

cor.enviro = cor(plot.data[,c(48:61)], use = "pairwise")
corrplot(cor.enviro,method = "number")
# combos include variables with r < 0.4
# combo 1: high temp, slope, eastness, mTPI
# combo 2: low temp, slope, mTPI, northness, eastness
# combo 3: moisture, slope, mTPI,

# PCA of climate variables
pc = princomp(plot.data[,c(48:61)], cor = TRUE)
summary(pc) # PC1-PC3 80% of variation
#screeplot
plot(pc, type = "lines")
pc$loadings
ggbiplot(pc)

# pc excluding macro variables
pc.2 = princomp(plot.data[,c(48:56,61)], cor = TRUE)
summary(pc.2) # PC1-PC4 89% of variation, PC1-PC3 = 77%
#screeplot
plot(pc.2, type = "lines")
pc.2$loadings
ggbiplot(pc.2)

pcvarexplained = tibble(var_explained = (pc.2$sdev^2) / (sum(pc.2$sdev^2))) %>%
  mutate(pc.3 = seq(1,length(var_explained), 1)) %>%
  mutate(cumvarexplained = cumsum(var_explained))
pcvarexplained

rownames(pc.2$loadings) = c("High Temp.","Low Temp.","Annual Precip.","Northness","Eastness","Windward Exposure","Elevational Position",
                            "Slope", "Aspect","Elevation")

plot.enviro.pc = ggbiplot(pc.2,  varname.adjust = 1.1, varname.size = 4) +
  geom_point() + 
  theme_classic() + 
  labs(x = paste0("Standardized PC1\n (", round(pcvarexplained$var_explained[1]*100,1), "% explained var.)"), 
       y = paste0("Standardized PC2\n (", round(pcvarexplained$var_explained[2]*100,1), "% explained var.)")) + 
  theme_classic(base_size = 18)+
  coord_cartesian( clip="off") #this keeps it from clipping off the stuff outside the plot
plot.enviro.pc

#ggsave("./Plots/enviro.pc1.pc2.png", width = 8.5, height = 8.5, dpi = 300)

plot.enviro.pc3.4 = ggbiplot(pc.2,  choices = 3:4, varname.adjust = 1.1, varname.size = 4) +
  geom_point() + 
  theme_classic() + 
  labs(x = paste0("Standardized PC3\n (", round(pcvarexplained$var_explained[3]*100,1), "% explained var.)"), 
       y = paste0("Standardized PC4\n (", round(pcvarexplained$var_explained[4]*100,1), "% explained var.)")) + 
  theme_classic(base_size = 18)+
  coord_cartesian( clip="off") #this keeps it from clipping off the stuff outside the plot
plot.enviro.pc3.4

#ggsave("./Plots/enviro.pc3.pc4.png", width = 8.5, height = 8.5, dpi = 300)

scores = as.data.frame(pc.2$scores)
plot.data.2 = cbind(plot.data,scores)
plot.data.3 = plot.data.2 %>% 
  dplyr::select(3,48:73)

# merge plot data back with all.data

all.data.2 = left_join(all.data,plot.data.3, by = "plot_id_new")

# predictors: DBH,living_basal_area_m2,fagus_basal_area_m2, PC1, PC2, PC3, PC4
# combos include variables with r < 0.4
# combo 1: high temp, slope, eastness, mTPI
# combo 2: low temp, slope, mTPI, northness, eastness
# combo 3: moisture, slope, mTPI,

# scale dbh
all.data.2 = all.data.2 %>% 
  mutate(across(c(6), 
                ~ as.numeric(scale(.)), 
                .names = "{.col}_scaled"))

# correlation between all predictors
cor.final = cor(all.data.2[,c(6,48:52,54,55,62,63,65:68,74)], use = "pairwise")
corrplot(cor.final,method = "number")

# correlation with crown_class
cor.test(all.data.2$dbh_scaled, as.numeric(as.factor(all.data.2$crown_class)))
# r = -0.57, p < 2.2e-16

test = lm(all.data.2$dbh_scaled ~ as.factor(all.data.2$crown_class))
summary(test) # R2 = 0.534

write.csv(all.data.2, file = "./Formatted.Data/final.data.csv")

#### Models ####
# response variable is ordinal, categorical so need ordered-logit model

final.data = read.csv("./Formatted.Data/final.data.csv", row.names = 1)

hist(final.data$overall_dieback) # range 2-6, possible range 1-6
hist(final.data$percent_bld) # may need to change to start scale at 1 instead of 0, range 0-4
hist(final.data$dominant_symptom_expression) # # may need to change to start scale at 1 instead of 0, range 0-4

# making the response variables ordered factors:
final.data$overall_dieback_factor = factor(final.data$overall_dieback, 
                                           levels = c("2","3","4","5","6"), ordered = TRUE)
final.data$percent_bld_plus = final.data$percent_bld + 1 # range now 1 - 5
final.data$percent_bld_plus_factor = factor(final.data$percent_bld_plus, 
                                           levels = c("1","2","3","4","5"), ordered = TRUE)

final.data$plot_id_new_factor = as.factor(final.data$plot_id_new)

#### Canopy Dieback

dieback_mod_all = clmm(overall_dieback_factor ~ dbh_scaled + living_basal_area_m2_scaled +
                          fagus_basal_area_m2_scaled + Comp.1 + Comp.2 +
                          (1|plot_id_new_factor), data = final.data, Hess = TRUE)
summary(dieback_mod_all)
# lower dbh = higher canopy dieback

exp(coef(dieback_mod_all)["dbh_scaled"])
# A one standard deviation increase in DBH was associated with a 71% decrease in
# the odds of being in a higher canopy dieback category, holding the other predictors constant.
# Larger trees have significantly lower odds of experiencing higher levels of canopy
# dieback.
# A one standard deviation decrease in DBH is associated with approximately 3.5 fold
# (1/0.2865497) higher odds of being in a higher canopy dieback category.

beta = coef(dieback_mod_all)
OR = exp(beta)
CI = exp(confint(dieback_mod_all))
cbind(OR = OR,CI_lower = CI[, 1],CI_upper = CI[, 2])

dieback_mod_NoRE = clm(overall_dieback_factor ~ dbh_scaled + living_basal_area_m2_scaled +
                         fagus_basal_area_m2_scaled + Comp.1 + Comp.2, data = final.data, Hess = TRUE)
summary(dieback_mod_NoRE)
# dbh, Comp.1, Comp.2 significant without RE structure

dieback_mod_RE = clmm(overall_dieback_factor ~ 1 + (1|plot_id_new_factor), data = final.data, Hess = TRUE)
summary(dieback_mod_RE)

all.v.NoRE = anova(dieback_mod_NoRE,dieback_mod_all) # model with RE is better fit
all.v.RE = anova(dieback_mod_RE,dieback_mod_all) # full model is better fit

# Plotting
ggplot(final.data, aes(x = dbh_scaled, fill = overall_dieback_factor))+
  geom_histogram(binwidth = 0.1)

# best observed data display plot
ggplot(final.data, aes(x = cut(dbh,5), fill = overall_dieback_factor))+
  geom_bar(position = "fill")+
  labs(x = "Observed DBH (five bins)", y = "Proportion of Trees",
    fill = "Canopy Dieback") +
  theme_classic(base_size = 18)

ggsave("./Plots/canopy.dieback.obs.plot.png", width = 8.5, height = 8.5, dpi = 300)

overall_dieback_predict = ggpredict(dieback_mod_all, terms = "dbh_scaled [all]", type = "fixed")

ggplot(overall_dieback_predict, aes(x = x, y = predicted, color = response.level, group = response.level))+
  geom_line(linewidth = 1)+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
    alpha = 0.12,color = NA)+
  scale_color_discrete(
    labels = c("2", "3", "4", "5", "6")) +
  labs(x = "Standardized DBH", y = "Predicted Probability",
       color = "Canopy Dieback")+
  theme_classic(base_size = 18)

ggsave("./Plots/canopy.dieback.dbh.plot.png", width = 8.5, height = 8.5, dpi = 300)

#### Percent BLD
percentBLD_mod_all = clmm(percent_bld_plus_factor ~ dbh_scaled + living_basal_area_m2_scaled +
                         fagus_basal_area_m2_scaled + Comp.1 + Comp.2 +
                         (1|plot_id_new_factor), data = final.data, Hess = TRUE)
summary(percentBLD_mod_all)
# lower dbh = higher canopy dieback
# higher Comp.2 = higher canopy dieback (higher slope, aspect, windward exposure, lower eastness, mTPI)

exp(coef(percentBLD_mod_all)["dbh_scaled"])
# A one standard deviation increase in DBH was associated with a 38% decrease in
# the odds of being in a higher BLD percent category, holding the other predictors constant.
# Larger trees have significantly lower odds of experiencing higher levels of percent BLD.
# A one standard deviation decrease in DBH is associated with approximately 1.6 fold
# (1/0.6228279) higher odds of being in a higher percent BLD category.

exp(coef(percentBLD_mod_all)["living_basal_area_m2_scaled"])
# A one standard deviation increase in basal area per m2 was associated with a 26% decrease in
# the odds of being in a higher BLD percent category, holding the other predictors constant.
# Tree surrounded my more basal area have significantly lower odds of experiencing higher levels of percent BLD.
# A one standard deviation decrease in basal area is associated with approximately 1.3 fold
# (1/0.7439354) higher odds of being in a higher percent BLD category.

exp(coef(percentBLD_mod_all)["Comp.1"])
# A one unit increase in Comp.1 was associated with a 15% decrease in
# the odds of being in a higher BLD percent category, holding the other predictors constant.
# Trees in plots with higher Comp.1 values have significantly lower odds of experiencing higher levels of percent BLD.
# A one unit decrease in basal area is associated with approximately 1.2 fold
# (1/0.8527069) higher odds of being in a higher percent BLD category.

exp(coef(percentBLD_mod_all)["Comp.2"])
# A one unit increase in Comp.2 was associated with a 53% increase in the odds of
# being in a higher percent BLD category, holding all other predictors constant.
# Trees in plots with higher Comp.2 values have significantly higher odds of experiencing higher levels of percent BLD.
# A one unit increase in Comp.2 is associated with approximately a 1.5 fold
# higher odds of being in a higher percent BLD category.

beta = coef(percentBLD_mod_all)
OR = exp(beta)
CI = exp(confint(percentBLD_mod_all))
cbind(OR = OR,CI_lower = CI[, 1],CI_upper = CI[, 2])

percentBLD_mod_NoRE = clm(percent_bld_plus_factor ~ dbh_scaled + living_basal_area_m2_scaled +
                         fagus_basal_area_m2_scaled + Comp.1 + Comp.2, data = final.data, Hess = TRUE)
summary(percentBLD_mod_NoRE)
# dbh, total BA, Comp.1, Comp.2 significant without RE

percentBLD_mod_RE = clmm(percent_bld_plus_factor ~ 1 + (1|plot_id_new_factor), data = final.data, Hess = TRUE)
summary(percentBLD_mod_RE)

all.v.NoRE = anova(percentBLD_mod_NoRE,percentBLD_mod_all) # models are equivalent
all.v.RE = anova(percentBLD_mod_RE,percentBLD_mod_all) # full model is better fit

# Plotting
ggplot(final.data, aes(x = dbh_scaled, fill = percent_bld_plus_factor))+
  geom_histogram(binwidth = 0.1)
ggplot(final.data, aes(x = living_basal_area_m2_scaled, fill = percent_bld_plus_factor))+
  geom_histogram(binwidth = 0.1)
ggplot(final.data, aes(x = Comp.1, fill = percent_bld_plus_factor))+
  geom_histogram(binwidth = 0.1)
ggplot(final.data, aes(x = Comp.2, fill = percent_bld_plus_factor))+
  geom_histogram(binwidth = 0.1)

# best observed display plot
ggplot(final.data, aes(x = cut(dbh,5), fill = percent_bld_plus_factor))+
  geom_bar(position = "fill")+
  labs(x = "Observed DBH (five bins)", y = "Proportion of trees",
       fill = "Percent BLD") +
  theme_classic()
ggplot(final.data, aes(x = cut(living_basal_area_m2,5), fill = percent_bld_plus_factor))+
  geom_bar(position = "fill")+
  labs(x = "Basal Area per m2 (five bins)", y = "Proportion of trees",
       fill = "Percent BLD") +
  theme_classic()
ggplot(final.data, aes(x = cut(Comp.1,5), fill = percent_bld_plus_factor))+
  geom_bar(position = "fill")+
  labs(x = "Comp.1 (five bins)", y = "Proportion of trees",
       fill = "Percent BLD") +
  theme_classic()
ggplot(final.data, aes(x = cut(Comp.2,5), fill = percent_bld_plus_factor))+
  geom_bar(position = "fill")+
  labs(x = "Comp.2 (five bins)", y = "Proportion of trees",
       fill = "Percent BLD") +
  theme_classic()

percent_BLD_DBH_predict = ggpredict(percentBLD_mod_all, terms = "dbh_scaled [all]", type = "fixed")
percent_BLD_BA_predict = ggpredict(percentBLD_mod_all, terms = "living_basal_area_m2_scaled [all]", type = "fixed")
percent_BLD_Comp1_predict = ggpredict(percentBLD_mod_all, terms = "Comp.1 [all]", type = "fixed")
percent_BLD_Comp2_predict = ggpredict(percentBLD_mod_all, terms = "Comp.2 [all]", type = "fixed")

ggplot(percent_BLD_DBH_predict, aes(x = x, y = predicted, color = response.level, group = response.level))+
  geom_line(linewidth = 1)+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.12,color = NA)+
  labs(x = "Standardized DBH", y = "Predicted Probability",
       color = "Percent BLD")+
  theme_classic(base_size = 18)

ggsave("./Plots/percent.BLD.dbh.plot.png", width = 8.5, height = 8.5, dpi = 300)

ggplot(percent_BLD_BA_predict, aes(x = x, y = predicted, color = response.level, group = response.level))+
  geom_line(linewidth = 1)+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.12,color = NA)+
  labs(x = "Standardized Basal Area per m2", y = "Predicted Probability",
       color = "Percent BLD")+
  theme_classic(base_size = 18)

ggsave("./Plots/percent.BLD.BA.plot.png", width = 8.5, height = 8.5, dpi = 300)

ggplot(percent_BLD_Comp1_predict, aes(x = x, y = predicted, color = response.level, group = response.level))+
  geom_line(linewidth = 1)+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.12,color = NA)+
  labs(x = "PC 1", y = "Predicted Probability",
       color = "Percent BLD")+
  theme_classic(base_size = 18)

ggsave("./Plots/percent.BLD.PC1.plot.png", width = 8.5, height = 8.5, dpi = 300)

ggplot(percent_BLD_Comp2_predict, aes(x = x, y = predicted, color = response.level, group = response.level))+
  geom_line(linewidth = 1)+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.12,color = NA)+
  labs(x = "PC 2", y = "Predicted Probability",
       color = "Percent BLD")+
  theme_classic(base_size = 18)

ggsave("./Plots/percent.BLD.PC2.plot.png", width = 8.5, height = 8.5, dpi = 300)

