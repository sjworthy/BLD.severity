# Code to combine all data and run analyses

library(tidyverse)


#### Wrangle Response Variable ####
# First wrangle response variable: individual FAGR stem BLD severity metrics
# Basal area only includes trees >= 5 inches DBH

# response
# overall_dieback # in both main and subplot
# percent bld # # in both main and subplot

# predictors
# crown class
# dbh

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


# Two versions of analyses 1: all plots, 2: only APEX control plots
# control plots are: PC-01, PC-06, PC-12, SC-05, SC-09, SC-11,
