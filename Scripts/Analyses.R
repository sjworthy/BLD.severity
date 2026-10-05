# Code to combine all data and run analyses

library(tidyverse)
library(corrplot)
library(ggbiplot)
library(MASS)

#### Wrangle Response Variable ####
# First wrangle response variable: individual FAGR stem BLD severity metrics
# Basal area only includes trees >= 5 inches DBH

main = read.csv("./Raw.Data/Survey_Data/main_stem_severity.csv") %>% 
  select(plot_type, plot_id, plot_id_new, date, species, dbh, overall_dieback, percent_bld, comments)
# deleted a degree symbol from notes of row 892 b/c RStudio wouldn't display dataframe
subplot = read.csv("./Raw.Data/Survey_Data/subplot_stem_severity.csv") %>% 
  select(plot_type, plot_id, plot_id_new, date, species, dbh, dieback, percent_infected_bld, remarks)

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
# will slim down further to the ones in Holden plots

# write.csv(FAGR.stems, file = "./Formatted.Data/FAGR.stems.csv")

#### Wrangle plots data ####
# Should be 35 plots censuses at Holden in 2025
# I think we need to keep the analysis to only these plots or we have to account
# for differences in the infection timing of plots across Ohio, PA, and WV.

clim = read.csv("./Formatted.Data/BLD.plot.variables.csv", row.names = 1) %>% 
  select(-date)

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
  select(-parts, -prefix, -num, -suffix) %>% 
  relocate(plot_id_new, .after = plot_id) 

BA = read.csv("./Formatted.Data/BLD_ForestMonitoring_PlotBasalArea_2025_RS.csv", row.names = 1) %>% 
  select(plot_id_new,plot_id,plot_type,date,plot_area_m2,living_basal_area_m2,living_n_trees,
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

write.csv(all.data, file = "./Formatted.Data/all.data.csv")

#### Analyses ####
# Two versions of analyses 1: all plots, 2: only APEX control plots
# control plots are: PC-01, PC-06, PC-12, SC-05, SC-09, SC-11,

all.data = read.csv("./Formatted.Data/all.data.csv", row.names = 1)

# response: overall_dieback, percent_bld

cor.test(all.data$overall_dieback,all.data$percent_bld)
# r = 0.22, p = 0.002223

cor.test(all.data$overall_dieback,all.data$percent_bld, method = "spearman")
# r = 0.23, p = 0.001117

ggplot(all.data, aes(x = overall_dieback,y = percent_bld))+
  geom_point()

range(all.data$overall_dieback) # 2 - 6
range(all.data$percent_bld) # 0 - 4

# correlation among environmental predictors
plot.data = all.data %>%
  distinct(plot_id_new, .keep_all = TRUE)

# scale and center plot data
plot.data = plot.data %>%
  mutate(across(c(10:23,41,43,45), 
                ~ as.numeric(scale(.)), 
                .names = "{.col}_scaled"))

# correlation among basal area predictors
cor.BA = cor(plot.data[,c(60,61,62)], use = "pairwise")
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

cor.enviro = cor(plot.data[,c(46:59)], use = "pairwise")
corrplot(cor.enviro,method = "number")
# combos include variables with r < 0.4
# combo 1: high temp, slope, eastness, mTPI
# combo 2: low temp, slope, mTPI, northness, eastness
# combo 3: moisture, slope, mTPI,

# PCA of climate variables
pc = princomp(plot.data[,c(46:59)], cor = TRUE)
summary(pc) # PC1-PC3 80% of variation
#screeplot
plot(pc, type = "lines")
pc$loadings
ggbiplot(pc)

# pc excluding macro variables
pc.2 = princomp(plot.data[,c(46:54,59)], cor = TRUE)
summary(pc.2) # PC1-PC4 89% of variation
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

ggsave("./Plots/enviro.pc1.pc2.png", width = 8.5, height = 8.5, dpi = 300)

plot.enviro.pc3.4 = ggbiplot(pc.2,  choices = 3:4, varname.adjust = 1.1, varname.size = 4) +
  geom_point() + 
  theme_classic() + 
  labs(x = paste0("Standardized PC3\n (", round(pcvarexplained$var_explained[3]*100,1), "% explained var.)"), 
       y = paste0("Standardized PC4\n (", round(pcvarexplained$var_explained[4]*100,1), "% explained var.)")) + 
  theme_classic(base_size = 18)+
  coord_cartesian( clip="off") #this keeps it from clipping off the stuff outside the plot
plot.enviro.pc3.4

ggsave("./Plots/enviro.pc3.pc4.png", width = 8.5, height = 8.5, dpi = 300)

scores = as.data.frame(pc.2$scores)
plot.data.2 = cbind(plot.data,scores)
plot.data.3 = plot.data.2 %>% 
  select(3,46:72)

# merge plot data back with all.data

all.data.2 = left_join(all.data,plot.data.3, by = "plot_id_new")

# predictors: DBH,living_basal_area_m2,fagus_basal_area_m2, PC1, PC2, PC3, PC4
# combo 1: high temp, slope, eastness, mTPI, macro_bio12
# combo 2: low temp, slope, mTPI, northness
# combo 3: moisture, slope, mTPI,

# scale dbh
all.data.2 = all.data.2 %>% 
  mutate(across(c(6), 
                ~ as.numeric(scale(.)), 
                .names = "{.col}_scaled"))

# correlation between all predictors

cor.final = cor(all.data.2[,c(6,46:50,52,53,60,61,63:66)], use = "pairwise")
corrplot(cor.final,method = "number")

write.csv(all.data.2, file = "./Formatted.Data/final.data.csv")

#### Models ####
# response variable is ordinal, categorical so need ordered-logit model

final.data = read.csv("./Formatted.Data/final.data.csv", row.names = 1)

hist(final.data$overall_dieback) # range 2-6, possible range 1-6
hist(final.data$percent_bld) # may need to change to start scale at 1 instead of 0, range 0-4

# making the response variables ordered factors:
final.data$overall_dieback_factor = factor(final.data$overall_dieback, 
                                           levels = c("2","3","4","5","6"), ordered = TRUE)
final.data$percent_bld_plus = final.data$percent_bld + 1 # range now 1 - 5
final.data$percent_bld_plus_factor = factor(final.data$percent_bld_plus, 
                                           levels = c("1","2","3","4","5"), ordered = TRUE)

dieback_mod = polr()
