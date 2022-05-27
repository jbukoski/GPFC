########################
## Summary statistics ##
########################

# A simple script to generate some of the summary statistics
# found in the main text of the article. This script also
# produces a CSV file that describes the structure of the database,
# as requested by one of the reviewers.

library(readxl)      # v 1.3.1
library(tidyverse)   # v 1.3.1

#----------------------------

sites <- read_excel("./data/GPFC_database.xlsx", sheet = 2)
obs <- read_excel("./data/GPFC_database.xlsx", sheet = 3)

smry_dat <- obs %>%
  left_join(sites) %>%
  dplyr::select(agc, age, binomial, plot_id:study_id, qaqc, type, 
                endemism, planting_spacing:thinned, prior, biome, 
                leaf_type:wood_density, country) %>%
  separate(binomial, c("genus"), sep = " ", remove = F)

rm(obs, sites)

#------------------------------------
# Generate key summary statistics as found in the main text.

# N observations
n_obs <- nrow(smry_dat)
n_obs

n_distinct(smry_dat$study_id)

# N plots
n_distinct(smry_dat$plot_id)

# N sites
n_distinct(smry_dat$site_id)

smry_dat %>%
  pull(genus) %>%
  n_distinct()

smry_dat %>%
  pull(binomial) %>%
  n_distinct()

smry_dat %>%
  group_by(genus) %>%
  summarize(n = n()) %>%
  filter(n < 3)

smry_dat %>%
  group_by(genus) %>%
  summarize(n = n()) %>%
  arrange(-n)

mean(smry_dat$age)
median(smry_dat$age)
max(smry_dat$age)
min(smry_dat$age)

mean(smry_dat$agc)
median(smry_dat$agc)
max(smry_dat$agc)
min(smry_dat$agc)

smry_dat %>%
  group_by(country) %>%
  summarize(n = n() / n_obs) %>%
  arrange(-n)

gnra <- c("Pinus", "Cunninghamia", "Eucalyptus", "Populus", "Acacia", 
          "Larix", "Picea", "Tectona", "Castanea", "Quercus")

n <- smry_dat %>%
  filter(genus %in% gnra) %>%
  nrow

n / n_obs

#-------------------------------------------------------------------
# Summarize the database structure, as requested by the reviewers

gnra <- c("Pinus", "Cunninghamia", "Eucalyptus", "Populus", "Acacia", 
          "Larix", "Picea", "Pseudotsuga", "Cryptomeria")

colnames(smry_dat)

smry_dat %>% 
  group_by(biome, genus, age) %>%
  summarize(n_site = n_distinct(site_id),
            n_obs = n()) %>%
  mutate(obs_per_site = n_obs / n_site)

smry_dat %>%
  filter(genus %in% gnra) %>%
  group_by(site_id) %>%
  summarize(n = n_distinct(plot_id)) %>%
  ggplot() +
  geom_histogram(aes(x = n), binwidth = 1) +
  theme_bw() +
  xlab("Number of observations")

smry_dat %>%
  separate(type, into = "rgn", sep = " ", remove = FALSE) %>%
  filter(genus %in% gnra, rgn %in% c("Tropical", "Temperate", "Boreal")) %>%
  filter(!(is.na(biome))) %>%
  group_by(biome, genus, age) %>%
  summarize(n_site = n_distinct(site_id),
            n_plot = n_distinct(plot_id),
            n_obs = n()) %>%
  mutate(obs_per_site = round(n_obs / n_site, 0)) %>%
  arrange(-n_obs) %>%
  arrange(genus, age) %>%
  ggplot() +
  geom_histogram(aes(x = obs_per_site)) +
  theme_bw() +
  xlab("Observations per site for biome x genus x age class groupings") +
  ylab("Count")


smry_dat %>%
  filter(genus %in% gnra) %>%
  group_by(genus, site_id) %>%
  summarize(n = n()) %>%
  mutate(max_n = max(n)) %>%
  select(genus, max_n) %>%
  distinct() %>%
  arrange(genus, -max_n)

# Summarize number of observations, plots, and sites by biome x genus x age

db_structure <- smry_dat %>%
  filter(genus %in% gnra) %>%
  filter(!is.na(biome)) %>%
  group_by(biome, genus, age) %>%
  summarize(n_obs = n(),
            n_plot = n_distinct(plot_id),
            n_site = n_distinct(site_id)) %>%
  distinct() %>%
  ungroup() %>%
  arrange(biome, genus, age)

# Write database structure to file

write_csv(db_structure, file = "./data/db_structure.csv")

#-----------------------------
# Clean up workspace

rm(list = ls())


