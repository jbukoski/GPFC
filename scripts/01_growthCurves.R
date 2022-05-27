###########################################
## Fit Chapman-Richards growth functions ##
###########################################

library(ModelMetrics)  # v 1.2.2.2
library(readxl)        # v 1.3.1
library(saemix)        # v 2.4
library(tidyverse)     # v 1.3.1

#--------------------------
# Load in the dataset

sites <- read_excel("./data/GPFC_database.xlsx", sheet = 2)
obs <- read_excel("./data/GPFC_database.xlsx", sheet = 3)

mdl_dat <- obs %>%
  left_join(sites) %>%
  dplyr::select(agc, age, binomial, plot_id:study_id, qaqc, type, 
                endemism, planting_spacing:thinned, prior, biome, 
                leaf_type:wood_density) %>%
  separate(binomial, c("genus"), sep = " ", remove = F)

rm(obs, sites)

source("./scripts/99_parameterizeModels.R")

grps <- c("Temperate needleleaf", "Temperate broadleaf", "Tropical needleleaf",
          "Tropical broadleaf", "Mediterranean needleleaf", "Boreal needleleaf",
          "Pinus", "Eucalyptus", "Cunninghamia", "Picea",
          "Populus", "Pseudotsuga", "Larix", "Cryptomeria", "Acacia")

##-----------------------------------------------------------------
## Repeat the above section for different values of m
## NOTE: mdlParameterization_wrapper() wraps lines 30:179 above, allowing
## different m parameters to be set within the C-R model.

df_full_m2 <- mdlParameterization_wrapper(mdl_dat, m = 2, grps)
df_full_m3 <- mdlParameterization_wrapper(mdl_dat, m = 3, grps)
df_full_m4 <- mdlParameterization_wrapper(mdl_dat, m = 4, grps)

# Drop a study with rapidly growing pines to achieve model convergence for m == 4

df_full_TeNe <- mdlParameterization_wrapper(filter(mdl_dat, study_id != 3969), 
                                            m = 4, grps = "Temperate needleleaf")

# Replace temperate needleleaf row with above

df_full_m4 <- bind_rows(df_full_TeNe, df_full_m4[2:15,])

# Write to csv

write_csv(df_full_m2, "./data/params_m2.csv")
write_csv(df_full_m3, "./data/params_m3.csv")
write_csv(df_full_m4, "./data/params_m4.csv")

##-------------------------------------------------------------------
## Rerun everything with plot_id replacing site_id in chronosequences to test
## influence of coding chronos as coming from single sites
#
mdl_dat <- mdl_dat %>%
  mutate(site_id = ifelse(qaqc == 4, plot_id, site_id))

params_m3_chronosAsPlots <- mdlParameterization_wrapper(mdl_dat, m = 3, grps)

write_csv(params_m3_chronosAsPlots, "./data/params_m3_chronosAsPlots.csv")

#------------------------
# Clean up workspace

rm(list = ls())
