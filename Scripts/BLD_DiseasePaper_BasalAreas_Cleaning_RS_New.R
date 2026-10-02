#Script Started June 2026 By R. Schiafo
#script edited October 2026 by S. Worthy

#Analysis of 2023-2026 BLD Forest Monitoring Plots

#Loading Packages
library(tidyverse)

#Reading in Data
#Data source: CSV was made from: "2023BLDFrstCom Data Entry - A Table.xlsx", "2024BLDFrstCom Data Entry - A Table.xlsx", "2025BLDFrstCom Data Entry - A Table.xlsx"
subplot_23 <- read.csv("./Raw.Data/Survey_Data/BLD Beech Data_2023_SubPlots_Full_RS.csv")
# had to remove the extra space after BW-2 in csv file
subplot_24 <- read.csv("./Raw.Data/Survey_Data/BLD Beech Data_2024_SubPlots_Full_RS.csv")
subplot_25 <- read.csv("./Raw.Data/Survey_Data/BLD Beech Data_2025_SubPlots_Full_RS.csv")
main_23 <- read.csv("./Raw.Data/Survey_Data/BLD Beech Data_2023_MainPlots_Full_RS.csv")
main_24 <- read.csv("./Raw.Data/Survey_Data/BLD Beech Data_2024_MainPlots_Full_RS.csv")
main_25 <- read.csv("./Raw.Data/Survey_Data/BLD Beech Data_2025_MainPlots_Full_RS.csv")

#site/ plot info
plots_23 <- read.csv("./Raw.Data/Survey_Data/BLD Beech Data_2023_SiteData_RS.csv")
plots_23 <- plots_23 %>% 
  rename(date_full = date) %>% 
  mutate(date = 2023) %>% 
  mutate(plot_id = trimws(plot_id))

plots_24 <- read.csv("./Raw.Data/Survey_Data/BLD Beech Data_2024_SiteData_RS.csv")
plots_24 <- plots_24 %>% 
  rename(date_full = date) %>% 
  mutate(date = 2024) %>% 
  mutate(plot_id = trimws(plot_id))

plots_25 <- read.csv("./Raw.Data/Survey_Data/BLD Beech Data_2025_SiteData_RS.csv")
plots_25 <- plots_25 %>% 
  rename(date_full = date) %>% 
  mutate(date = 2025) %>% 
  mutate(plot_id = trimws(plot_id))

plots_26 <- read.csv("./Raw.Data/Survey_Data/BLD Beech Data_2026_SiteData_RS.csv")
plots_26 <- plots_26 %>% 
  rename(date_full = date) %>% 
  mutate(date = 2026) %>% 
  mutate(plot_id = trimws(plot_id))


#merging data
subplot<-rbind(subplot_23, subplot_24, subplot_25) #129 plots
# Plot BW-2 is in subplot but not in main plot
main<-rbind(main_23, main_24, main_25) #129 plots_23
plots<-rbind(plots_23, plots_24, plots_25, plots_26)


###########cleaning data########################################################
#
################################################################################

#cleaning data

# ------------ Trimming any extra spaces in plot_id names  ------------------ #

main <- main %>% 
  mutate(plot_id = trimws(plot_id))

subplot <- subplot %>% 
  mutate(plot_id = trimws(plot_id))

plots <- plots %>% 
  mutate(plot_id = trimws(plot_id))


# ------------- Adding leading zeros the plot_ids where they are missing -------# 
#e.g., plots show up as both BP-01 and BP-1

#keeping old character strings and making now column


main <- main %>%
  mutate(
    parts  = str_match(plot_id, "^([A-Za-z]+)-(\\d+)([A-Za-z]*)$"),
    prefix = parts[, 2],
    num    = parts[, 3],
    suffix = parts[, 4],
    plot_id_new = paste0(prefix, "-", str_pad(num, width = 2, pad = "0"), suffix)
  ) %>%
  select(-parts, -prefix, -num, -suffix) %>% 
  relocate(plot_id_new, .after = plot_id) 



subplot <- subplot %>%
  mutate(
    parts  = str_match(plot_id, "^([A-Za-z]+)-(\\d+)([A-Za-z]*)$"),
    prefix = parts[, 2],
    num    = parts[, 3],
    suffix = parts[, 4],
    plot_id_new = paste0(prefix, "-", str_pad(num, width = 2, pad = "0"), suffix)
  ) %>%
  select(-parts, -prefix, -num, -suffix) %>% 
  relocate(plot_id_new, .after = plot_id) 



plots <- plots %>%
  mutate(
    parts  = str_match(plot_id, "^([A-Za-z]+)-(\\d+)([A-Za-z]*)$"),
    prefix = parts[, 2],
    num    = parts[, 3],
    suffix = parts[, 4],
    plot_id_new = paste0(prefix, "-", str_pad(num, width = 2, pad = "0"), suffix)
  ) %>%
  select(-parts, -prefix, -num, -suffix) %>% 
  relocate(plot_id_new, .after = plot_id) 



#how many plots?
length(unique(main$plot_id_new))
#118
length(unique(subplot$plot_id_new))
#118




##################MAIN DATA#####################################################


# ------------ Filling in Missing Data in SPECIES column --------------------- #

#1.1. Filling in missing data for main plots

#looking where I am missing data in the species column
c_main_issues_species<-main %>% 
  filter(species == "None" | species == "")
#two instances where the species is not IDed
#C-1 2023 entry has BLD symptom info filled in so safe to assume this is FAGUS... filling this in.
#PC-8 is ACRU


#Filling in missing treeIDS
main<- main %>%
mutate(species = case_when(
  plot_id == "C-1" & dbh == 15.4  ~ "FAGR",
  TRUE                        ~ species # Catches all unlisted cases
))

main<- main %>%
  mutate(species = case_when(
    plot_id == "PC-8" & dbh == 44.5 & date == 2023 ~ "ACRU", #Confirmed species ID with D. Jenkins and Burke Lab
    TRUE                        ~ species # Catches all unlisted cases
  ))



# ------------ Fixing issues with > in the DBH columns ----------------------- #

#2.1 Fixing DBH for main plots
c_main_issues_dbh<-main %>% #1 issue >50
  filter(grepl(">", dbh))
c_main_issues_dbh2<-main %>% #no issues
  filter(grepl("<", dbh))



#Dealing with this by dropping the </> signs... Double check this with co-authors
main <- main %>%
  mutate(dbh = case_when(
    dbh == ">50"  ~ "50.0",
    TRUE                        ~ dbh # Catches all unlisted cases
  ))



# ------------ Checking what plot/years i am missing data ---------------------------------- #

#These are the years that certain plots were not visited/ data was not recorded  for them 
# i believe these are distinct from plots that simply had no trees.. there are some of those and they are dealt with below.
#this code helps differentiate the two 


#Main

#build every possible plot_id x year combination
all_plot_years_main <- expand_grid(
  plot_id_new = unique(main$plot_id_new),
  date = c(2023, 2024, 2025))

#what plot/year combos actually exist in data
existing_plot_years_main <- main %>% distinct(plot_id_new, date)

#antijoin to find whats missing
missing_plot_years_main <- anti_join(all_plot_years_main, existing_plot_years_main, by = c("plot_id_new", "date"))



#there are 89 plot/years that have no record - these are filled in as missing data for now- need to confirm this does not mean there were no trees
# The Upper Baldwin plots were only censused the first year and then again 2026
# These would be dropped anyway b/c they have a treatment overlay that complicates findings.


# ----------- Determining total basal area, total basal area of beeches, and total basal area of other species)----------- #

#Doing this for plots that have records of trees (not missing_plot_years_main- will add those back in after)

#note that there are trees in every MAIN plot, but not when breaking down by fagus only or nonfagus, each plot does not necesarily contain both types
#this is accounted for and the count and basal area is recorded as zero- this is distinct from data marked as "missing" 

#Main: 

#some slight data prep first

#remove "no data" - just 4 lines (3 of which are SNAGs)
#these are rows that have a tree record but no numbers associated- removing them from the data
main <- main %>% 
  filter(dbh != "no data" )

#turning DBH to numeric
main$dbh <- as.numeric(main$dbh)


#Calculating Total Basal area in MAIN plots: 

#Data frame with per-tree basal area
main <- main %>%
  mutate(basal_area_m2 = (pi/4) * (dbh/100)^2) #double checked with online basal area calculator (0.00007854 * dbh^2 is another convention seen- tested this and they match)

#write.csv(main, file = "./Raw.Data/Survey_Data/main_stem_severity.csv")

#calculating plot-level basal area 

# grab every valid/distinct plot_id x date combo from main- needed to fill in where there are zero trees recorede
valid_plot_years <- main %>% distinct(plot_id_new, date) #265


#1. total basal area for all stems (alive and dead)
main_plot_basal_total <- main %>%
  group_by(plot_id_new, date) %>%  
  summarise(total_basal_area_m2 = sum(basal_area_m2, na.rm = TRUE),
            total_n_trees = n()) %>% 
  mutate(plot_area_m2 = pi * 11.3^2,   #Main plots are 37.2 ft/11.3 m radius;  Circular plot area (m2) = pi * r^2
         plot_area_ha = plot_area_m2 / 10000) %>% 
  mutate(total_basal_area_m2_per_ha = total_basal_area_m2 / plot_area_ha)

#2. Total basal area of all living stems
main_plot_basal_living <- main %>% 
  filter(!(species == "SNAG" | crown_class == "DEAD")) %>% #removing SNAGs and anything recorded as DEAD
  group_by(plot_id_new, date) %>%
  summarise(living_basal_area_m2 = sum(basal_area_m2, na.rm = TRUE),
            living_n_trees = n(), 
            .groups = "drop") %>% 
  right_join(valid_plot_years, by = c("plot_id_new", "date")) %>%   # <- brings back every plot/date combo
  mutate(living_basal_area_m2 = replace_na(living_basal_area_m2, 0), #replace NA with zero
         living_n_trees = replace_na(living_n_trees, 0)) %>%
  mutate(plot_area_m2 = pi * 11.3^2,   #Main plots are 37.2 ft/11.3 m radius;  Circular plot area (m2) = pi * r^2
         plot_area_ha = plot_area_m2 / 10000,
  living_basal_area_m2_per_ha = living_basal_area_m2 / plot_area_ha)

#3. Total basal area of all living FAGUS stems
main_plot_basal_fagus <- main %>% 
  filter(species == "FAGR") %>% #keeping only fagus
  filter(!(species == "SNAG" | crown_class == "DEAD")) %>% #removing SNAGs and anything recorded as DEAD
  group_by(plot_id_new, date) %>%
  summarise(fagus_basal_area_m2 = sum(basal_area_m2, na.rm = TRUE),
            fagus_n_trees = n(), 
            .groups = "drop") %>% 
  right_join(valid_plot_years, by = c("plot_id_new", "date")) %>%   # <- brings back every plot/date combo
  mutate(fagus_basal_area_m2 = replace_na(fagus_basal_area_m2, 0), #replace NA with zero
         fagus_n_trees = replace_na(fagus_n_trees, 0)) %>%
  mutate(plot_area_m2 = pi * 11.3^2,   #Main plots are 37.2 ft/11.3 m radius;  Circular plot area (m2) = pi * r^2
         plot_area_ha = plot_area_m2 / 10000,
         fagus_basal_area_m2_per_ha = fagus_basal_area_m2 / plot_area_ha)


#4. Total basal area of all living NON-FAGUS stems
main_plot_basal_nonfagus <- main %>% 
  filter(species != "FAGR") %>% #keeping only non-Fagus trees
  filter(!(species == "SNAG" | crown_class == "DEAD")) %>% #removing SNAGs and anything recorded as DEAD
  group_by(plot_id_new, date) %>%
  summarise(nonfagus_basal_area_m2 = sum(basal_area_m2, na.rm = TRUE),
            nonfagus_n_trees = n(), 
            .groups = "drop") %>% 
  right_join(valid_plot_years, by = c("plot_id_new", "date")) %>%   # <- brings back every plot/date combo
  mutate(nonfagus_basal_area_m2 = replace_na(nonfagus_basal_area_m2, 0), #replace NA with zero
         nonfagus_n_trees = replace_na(nonfagus_n_trees, 0)) %>%
  mutate(plot_area_m2 = pi * 11.3^2,   #Main plots are 37.2 ft/11.3 m radius;  Circular plot area (m2) = pi * r^2
         plot_area_ha = plot_area_m2 / 10000,
         nonfagus_basal_area_m2_per_ha = nonfagus_basal_area_m2 / plot_area_ha)



#combining data frames 1-4 (leaving out dead for now)
main_plot_basal <- merge(main_plot_basal_total, main_plot_basal_living, 
                         by = c("plot_id_new", "date", "plot_area_m2", "plot_area_ha"), all = TRUE) %>%
  merge(main_plot_basal_fagus, by = c("plot_id_new", "date", "plot_area_m2", "plot_area_ha"), all = TRUE) %>%
  merge(main_plot_basal_nonfagus, by = c("plot_id_new", "date", "plot_area_m2", "plot_area_ha"), all = TRUE)


#reordering columns
main_plot_basal <- main_plot_basal %>% 
  relocate(total_n_trees, .after = total_basal_area_m2_per_ha) %>% 
  relocate(living_n_trees, .after = living_basal_area_m2_per_ha) %>% 
  relocate(fagus_n_trees, .after = fagus_basal_area_m2_per_ha) %>% 
  relocate(nonfagus_n_trees, .after = nonfagus_basal_area_m2_per_ha)
  


#filling in missing data- this is data where there is no record of data being taken- not where trees are mising in a particular year. 
main_plot_basal <- main_plot_basal %>% 
  bind_rows(missing_plot_years_main %>% select(plot_id_new, date)) %>% 
  mutate(data_status = if_else(is.na(living_basal_area_m2), "missing", "collected")) %>% 
  arrange(plot_id_new, date)
#354 (118 plots x 3 years)


######END

#Break
#Break



#SUBPLOT CODE##########################################################################

#doing the same thing with subplots


###########cleaning data########################################################
#
################################################################################



# ------------ Filling in Missing Data in SPECIES column --------------------- #

#1.2. Filling in missing data for subplots

#Adding in "None" for plots that do not have any trees recorded in subplots
#This is often written in the notes (if not already added to the data)
subplot <- subplot %>%
  mutate(species = case_when(
    remarks %in% c("None", "NONE", "None in subplot", "no saplings", "none") ~ "None",
    TRUE                        ~ species # Catches all unlisted cases
  ))

#looking where else I am missing data
c_subplot_issues_species<-subplot %>% 
  filter(species == "None" | species == "")

#C-11 is missing 2024 data but there is only 1 small beech in 2023 and None recorded in 2025, so assuming this was None in 2024 too
#C-15 has None for 2023 and 2025 
subplot <- subplot %>%
  mutate(species = case_when(
    plot_id_new == "C-11" & date == "2024"  ~ "None",
    TRUE                        ~ species # Catches all unlisted cases
  ))
subplot <- subplot %>%
  mutate(species = case_when(
    plot_id_new == "C-15"  ~ "None",
    TRUE                        ~ species # Catches all unlisted cases
  ))



# ------------ Fixing issues with > in the DBH columns ----------------------- #

#2.2. Fixing DBH for subplots
c_subplot_issues_dbh<-subplot %>% #no issues
  filter(grepl(">", dbh))
c_subplot_issues_dbh2<-subplot %>% #58 issues.. all <1 except one <4
  filter(grepl("<", dbh))


subplot <- subplot %>%
  mutate(dbh = case_when(
    dbh == "<1"  ~ "1.0",
    TRUE                        ~ dbh # Catches all unlisted cases
  )) %>%
  mutate(dbh = case_when(
    dbh == "<4"  ~ "4.0",
    TRUE                        ~ dbh # Catches all unlisted cases
  ))




# ------------ Checking what plot/years i am missing data ---------------------------------- #

#These are the years that certain plots were not visited/ data was not recorded  for them 
# i believe these are distinct from plots that simply had no trees.. there are some of those and they are dealt with below.
#this code helps differentiate the two 


#subplot

#build every possible plot_id x year combination
all_plot_years_subplot <- expand_grid(
  plot_id_new = unique(subplot$plot_id_new),
  date = c(2023, 2024, 2025))

#what plot/year combos actually exist in data
existing_plot_years_subplot <- subplot %>% distinct(plot_id_new, date)

#antijoin to find whats missing
missing_plot_years_subplot <- anti_join(all_plot_years_subplot, existing_plot_years_subplot, by = c("plot_id_new", "date"))



#there are 89 plot/years that have no record - these are filled in as missing data for now- need to confirm this does not mean there were no trees



# ----------- Determining total basal area, total basal area of beeches, and total basal area of other species)----------- #

#Doing this for plots that have records of trees (not missing_plot_years_subplot- will add those back in after)

#Note: Not every subplot has trees in it- this is accounted for and per tree basal area is recorded as zero when "None" is recorded in subplots
#-this is distinct from data marked as "missing" 

#subplot: 

#some slight data prep first


#turning DBH to numeric
subplot$dbh <- as.numeric(subplot$dbh)


#Calculating Total Basal area in subplot plots: 

#Data frame with per-tree basal area
subplot <- subplot %>%
  mutate(basal_area_m2 = (pi/4) * (dbh/100)^2) %>% #double checked with online basal area calculator (0.00007854 * dbh^2 is another convention seen- tested this and they match)
  mutate(basal_area_m2 = replace_na(basal_area_m2, 0)) #replace na to zero where the plot is empty

#write.csv(subplot, file = "./Raw.Data/Survey_Data/subplot_stem_severity.csv")

#calculating plot-level basal area 

# grab every valid/distinct plot_id x date combo from subplot- needed to fill in where there are zero trees recorede
valid_plot_years <- subplot %>% distinct(plot_id_new, date) #387


#1. total basal area for all stems (alive and dead)
subplot_plot_basal_total <- subplot %>%
  group_by(plot_id_new, date) %>%  
  summarise(total_basal_area_m2 = sum(basal_area_m2, na.rm = TRUE),
            total_n_trees = n()) %>% 
  mutate(total_n_trees = if_else(total_basal_area_m2 == 0, 0, total_n_trees)) %>% #change any tree count where there is zero basal area to 0.. code counts rows so lines with "None" get counted but there should really be zero trees 
  mutate(plot_area_m2 = pi * 3.6^2,   #subplot plots are 11.8 ft/3.6 m radius;  Circular plot area (m2) = pi * r^2
         plot_area_ha = plot_area_m2 / 10000) %>% 
  mutate(total_basal_area_m2_per_ha = total_basal_area_m2 / plot_area_ha)

#2. Total basal area of all living stems
subplot_plot_basal_living <- subplot %>% 
  filter(!(species == "SNAG" )) %>% #removing SNAGs and anything recorded as DEAD
  group_by(plot_id_new, date) %>%
  summarise(living_basal_area_m2 = sum(basal_area_m2, na.rm = TRUE),
            living_n_trees = n(), 
            .groups = "drop") %>% 
  mutate(living_n_trees = if_else(living_basal_area_m2 == 0, 0, living_n_trees)) %>% #change any tree count where there is zero basal area to 0.. code counts rows so lines with "None" get counted but there should really be zero trees 
  right_join(valid_plot_years, by = c("plot_id_new", "date")) %>%   # <- brings back every plot/date combo
  mutate(living_basal_area_m2 = replace_na(living_basal_area_m2, 0), #replace NA with zero
         living_n_trees = replace_na(living_n_trees, 0)) %>%
  mutate(plot_area_m2 = pi * 3.6^2,   #subplot plots are 11.8 ft/3.6 m radius;  Circular plot area (m2) = pi * r^2
         plot_area_ha = plot_area_m2 / 10000,
         living_basal_area_m2_per_ha = living_basal_area_m2 / plot_area_ha)

#3. Total basal area of all living FAGUS stems
subplot_plot_basal_fagus <- subplot %>% 
  filter(species == "FAGR") %>% #keeping only fagus
  filter(!(species == "SNAG" )) %>% #removing SNAGs and anything recorded as DEAD
  group_by(plot_id_new, date) %>%
  summarise(fagus_basal_area_m2 = sum(basal_area_m2, na.rm = TRUE),
            fagus_n_trees = n(), 
            .groups = "drop") %>% 
  mutate(fagus_n_trees = if_else(fagus_basal_area_m2 == 0, 0, fagus_n_trees)) %>% #change any tree count where there is zero basal area to 0.. code counts rows so lines with "None" get counted but there should really be zero trees 
  right_join(valid_plot_years, by = c("plot_id_new", "date")) %>%   # <- brings back every plot/date combo
  mutate(fagus_basal_area_m2 = replace_na(fagus_basal_area_m2, 0), #replace NA with zero
         fagus_n_trees = replace_na(fagus_n_trees, 0)) %>%
  mutate(plot_area_m2 = pi * 3.6^2,   #subplot plots are 11.8 ft/3.6 m radius;  Circular plot area (m2) = pi * r^2
         plot_area_ha = plot_area_m2 / 10000,
         fagus_basal_area_m2_per_ha = fagus_basal_area_m2 / plot_area_ha)


#4. Total basal area of all living NON-FAGUS stems
subplot_plot_basal_nonfagus <- subplot %>% 
  filter(species != "FAGR") %>% #keeping only non-Fagus trees
  filter(!(species == "SNAG" )) %>% #removing SNAGs and anything recorded as DEAD
  group_by(plot_id_new, date) %>%
  summarise(nonfagus_basal_area_m2 = sum(basal_area_m2, na.rm = TRUE),
            nonfagus_n_trees = n(), 
            .groups = "drop") %>% 
  mutate(nonfagus_n_trees = if_else(nonfagus_basal_area_m2 == 0, 0, nonfagus_n_trees)) %>% #change any tree count where there is zero basal area to 0.. code counts rows so lines with "None" get counted but there should really be zero trees 
  right_join(valid_plot_years, by = c("plot_id_new", "date")) %>%   # <- brings back every plot/date combo
  mutate(nonfagus_basal_area_m2 = replace_na(nonfagus_basal_area_m2, 0), #replace NA with zero
         nonfagus_n_trees = replace_na(nonfagus_n_trees, 0)) %>%
  mutate(plot_area_m2 = pi * 3.6^2,   #subplot plots are 11.8 ft/3.6 m radius;  Circular plot area (m2) = pi * r^2
         plot_area_ha = plot_area_m2 / 10000,
         nonfagus_basal_area_m2_per_ha = nonfagus_basal_area_m2 / plot_area_ha)



#combining data frames 1-4 (leaving out dead for now)
subplot_plot_basal <- merge(subplot_plot_basal_total, subplot_plot_basal_living, 
                            by = c("plot_id_new", "date", "plot_area_m2", "plot_area_ha"), all = TRUE) %>%
  merge(subplot_plot_basal_fagus, by = c("plot_id_new", "date", "plot_area_m2", "plot_area_ha"), all = TRUE) %>%
  merge(subplot_plot_basal_nonfagus, by = c("plot_id_new", "date", "plot_area_m2", "plot_area_ha"), all = TRUE)


#reordering columns
subplot_plot_basal <- subplot_plot_basal %>% 
  relocate(total_n_trees, .after = total_basal_area_m2_per_ha) %>% 
  relocate(living_n_trees, .after = living_basal_area_m2_per_ha) %>% 
  relocate(fagus_n_trees, .after = fagus_basal_area_m2_per_ha) %>% 
  relocate(nonfagus_n_trees, .after = nonfagus_basal_area_m2_per_ha)



#filling in missing data- this is data where there is no record of data being taken- not where trees are mising in a particular year. 
subplot_plot_basal <- subplot_plot_basal %>% 
  bind_rows(missing_plot_years_subplot %>% select(plot_id_new, date)) %>% 
  mutate(data_status = if_else(is.na(living_basal_area_m2), "missing", "collected")) %>% 
  arrange(plot_id_new, date)
#354 (118 plots x 3 years)



#END

#BREAK
#BREAK


#Combining the main and subplot data

main_plot_basal$plot_type<-"Main"
subplot_plot_basal$plot_type<-"Subplot"


plot_basal_area <- rbind(main_plot_basal, subplot_plot_basal)

#reordering columns
plot_basal_area <- plot_basal_area %>% 
  relocate(plot_type, .after = plot_id_new) 

#how many plots?
length(unique(plot_basal_area$plot_id_new))


#Adding in plot location information
plot_basal_area <- left_join(plot_basal_area, plots, by= join_by(plot_id_new, date))


#Reordering and keeping only columns that I want
plot_basal_area <- plot_basal_area %>% 
  select(plot_id_new, plot_id, plot_type, date, plot_area_m2, plot_area_ha, living_basal_area_m2, living_basal_area_m2_per_ha, living_n_trees, fagus_basal_area_m2, fagus_basal_area_m2_per_ha, fagus_n_trees, nonfagus_basal_area_m2, nonfagus_basal_area_m2_per_ha, nonfagus_n_trees, basal_area, aspect, slope_percent, slope_shape, slope_position, state, county, local.name, landowner, latitude, longitude, data_status, date_full, surveyors, site_remarks)


#Break

#Keeping only the 2025 data
plot_basal_area_25 <- plot_basal_area %>% 
  filter(date == 2025)

#how many plots?
length(unique(plot_basal_area_25$plot_id_new))
#118- before removing "missing: 

#removing missing data
plot_basal_area_25 <- plot_basal_area_25 %>% 
  filter(data_status == "collected")

#how many plots?
length(unique(plot_basal_area_25$plot_id_new))
#76- after removing "missing: 

#outputting

#Setting Working Directory
setwd("C:/Users/rschiafo/OneDrive - The Holden Arboretum dba Holden Forests and Gardens/Stuble Lab - BLD Forest Monitoring/Forest Monitoring/Data/Data exploration for mortality paper/Rory messing/R output data")
#write.csv(plot_basal_area_25, file="BLD_ForestMonitoring_PlotBasalArea_2025_RS.csv")

