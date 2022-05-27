#####################
## Driver analyses ##
#####################

library(caret)       # v 6.0.88
library(FactoMineR)  # v 2.4
library(factoextra)  # v 1.0.7
library(lmerTest)    # v 3.1.3
library(MASS)        # v 7.3.54
library(nlme)        # v 3.1.153
library(readxl)      # v 1.3.1
library(tidyverse)   # v 1.3.1

#---------------------------------------- 
# Load in data and select appropriate variables

sites <- read_excel("./data/GPFC_database.xlsx", sheet = 2)
obs <- read_excel("./data/GPFC_database.xlsx", sheet = 3)

mdl_dat <- obs %>%
  left_join(sites) %>%
  dplyr::select(agc, age, binomial, plot_id:study_id, qaqc, type, 
                endemism, planting_spacing:thinned, prior, biome, 
                leaf_type:wood_density) %>%
  separate(binomial, c("genus"), sep = " ", remove = F)

rm(obs, sites)

#------------------------------------------
##############################
## FAMD of Plant Trait Data ##
##############################

# Build dataframe for analysis

fa_dat <- mdl_dat %>%
  mutate(Leaf.Type = factor(leaf_type, labels = c("Broadleaf", "Needleleaf", "Scaled")),
         Leaf.Phenology = factor(leaf_phenology, labels = c("Deciduous", "Evergreen")),
         Leaf.Compoundness = factor(leaf_compoundness, labels = c("Compound", "Simple")),
         N.Fixation = factor(n_fixer, labels = c("N-Fixing", "Non N-Fixing")),
         genus_f = factor(genus)) %>%
  dplyr::select(binomial, Leaf.Type, Leaf.Phenology, Leaf.Compoundness, N.Fixation, Wood.Density = wood_density) %>%
  distinct() %>%
  drop_na()

fa_dat <- data.frame(fa_dat, row.names = "binomial")

#write_csv(fa_dat, "./data/famd_df.csv")

# Run factor analysis

famd.res <- FAMD(fa_dat, graph = FALSE)
famd.res$eig

# fviz_screeplot(famd.res)
# fviz_famd_var(famd.res, repel = TRUE)
# 
# fviz_contrib(famd.res, "var", axes = 1)
# fviz_contrib(famd.res, "var", axes = 2)

#---------------------------------------  
#############################
## Test drivers of biomass ##
#############################     

#--------------------
###########
## GENUS ##
###########

genus_dat <- mdl_dat %>%
  dplyr::select(agc, age, site_id, genus) %>%
  drop_na() %>%
  group_by(genus) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  filter(n > 20)

genus_mdl <- lmer(sqrt(agc) ~ sqrt(age)*genus + (1|site_id), data = genus_dat)

plot(residuals(genus_mdl, type = "pearson"))

nrow(genus_dat)
summary(genus_mdl)
anova(genus_mdl)


#--------------------
##############
## ENDEMISM ##
##############
  
endemism_dat <- mdl_dat %>%
  dplyr::select(agc, age, site_id, endemism) %>%
  drop_na() %>%
  filter(!(endemism %in% c("unknown", "unkown")))

endemism_dat %>%
  group_by(endemism) %>%
  summarize(n = n())

endemism_mdl <- lmer(sqrt(agc) ~ sqrt(age) * endemism + (1|site_id), data = endemism_dat)

plot(residuals(endemism_mdl, type = "pearson"))

nrow(endemism_dat)
summary(endemism_mdl)
anova(endemism_mdl)

#--------------------
##################
## PLANT TRAITS ##
##################

# Leaf type

trait_dat <- mdl_dat %>%
  dplyr::select(agc:site_id, -plot_id, leaf_type:n_fixer) %>%
  drop_na()

lftype_mdl <- lmer(sqrt(agc) ~ sqrt(age) * leaf_type + (1|site_id), data = trait_dat)

plot(residuals(lftype_mdl, type = "pearson"))

nrow(trait_dat)
summary(lftype_mdl)
anova(lftype_mdl)

# Leaf compoundness

lfcmpd_mdl <- lmer(sqrt(agc) ~ sqrt(age) * leaf_compoundness + (1|site_id), data = trait_dat)

summary(lfcmpd_mdl)
anova(lfcmpd_mdl)

# Leaf phenology

lfphen_mdl <- lmer(sqrt(agc) ~ sqrt(age) * leaf_phenology + (1|site_id), data = trait_dat)

summary(lfphen_mdl)
anova(lfphen_mdl)

# Nitrogen fixation

lfnfix_mdl <- lmer(sqrt(agc) ~ sqrt(age) * n_fixer + (1|site_id), data = trait_dat)

summary(lfnfix_mdl)
anova(lfnfix_mdl)

# Wood density

wd <- mdl_dat %>%
  dplyr::select(agc:site_id, wood_density, -plot_id) %>%
  drop_na()

wd_mdl <- lmer(sqrt(agc) ~ sqrt(age) * wood_density + (1|site_id), data = wd)

plot(residuals(wd_mdl, type = "pearson"))

nrow(wd)
summary(wd_mdl)
anova(wd_mdl)

#--------------------
####################
## PRIOR LAND USE ##
####################

landuse_dat <- mdl_dat %>%
  dplyr::select(agc, age, site_id, prior, biome) %>%
  group_by(prior) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  filter(n > 5 & !is.na(prior))

landuse_dat %>%
  pull(prior) %>%
  unique()

landuse_mdl1 <- lmer(sqrt(agc) ~ sqrt(age)*prior + (1|site_id), data = landuse_dat)

plot(residuals(landuse_mdl1, type = "pearson"))

nrow(landuse_dat)
summary(landuse_mdl1)
anova(landuse_mdl1)

#--------------------
# MANAGEMENT PRACTICES

# 1. Planting density (stems/ha)
# 2. Use of fertilizer (y/n)
# 3. Use of irrigation (y/n)
# 4. Weeding/vegetation management (y/n)
# 5. Thinning (y/n)

# Planting density

spacing_dat <- mdl_dat %>%
  dplyr::select(agc, age, site_id, planting_spacing, genus) %>%
  group_by(genus) %>%
  mutate(n = n_distinct(planting_spacing)) %>%
  ungroup() %>%
  filter(n > 5) %>%
  drop_na()

space_mdl <- lmer(sqrt(agc) ~ sqrt(age)*planting_spacing + (1|site_id), data = spacing_dat)

nrow(spacing_dat)
summary(space_mdl)
anova(space_mdl)

# Fertilizer

fert_dat <- mdl_dat %>%
  dplyr::select(agc, age, site_id, fertilized, genus) %>%
  drop_na() %>%
  group_by(genus) %>%
  mutate(n = n_distinct(fertilized)) %>%
  ungroup() %>%
  filter(n > 1)

fert_mdl <- lmer(sqrt(agc) ~ sqrt(age)*fertilized + (1|site_id), data = fert_dat)

nrow(fert_dat)
summary(fert_mdl)
anova(fert_mdl)

# Irrigation

irrg_dat <- mdl_dat %>%
  dplyr::select(agc, age, site_id, irrigated, genus) %>%
  drop_na() %>%
  group_by(genus) %>%
  mutate(n = n_distinct(irrigated)) %>%
  ungroup() %>%
  filter(n > 1)

nrow(irrg_dat)

irrg_mdl <- lmer(sqrt(agc) ~ sqrt(age)*irrigated + (1|site_id), data = irrg_dat)

summary(irrg_mdl)
anova(irrg_mdl)

# Vegetation control

weed_dat <- mdl_dat %>%
  dplyr::select(agc, age, site_id, weeded, genus) %>%
  drop_na() %>%
  group_by(genus) %>%
  mutate(n = n_distinct(weeded)) %>%
  ungroup() %>%
  filter(n > 1)

weed_mdl <- lmer(sqrt(agc) ~ sqrt(age)*weeded + (1|site_id), data = weed_dat)

nrow(weed_dat)
summary(weed_mdl)
anova(weed_mdl)

# Thinning

thin_dat <- mdl_dat %>%
  dplyr::select(agc, age, site_id, thinned, genus) %>%
  drop_na() %>%
  group_by(genus) %>%
  mutate(n = n_distinct(thinned)) %>%
  ungroup() %>%
  filter(n > 1)

thin_mdl <- lmer(sqrt(agc) ~ sqrt(age)*thinned + (1|site_id), data = thin_dat)

nrow(thin_dat)
summary(thin_mdl)
anova(thin_mdl)

#--------------------
# Biome

biome_dat <- mdl_dat %>%
  group_by(biome) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  filter(n >= 75) %>%
  dplyr::select(agc, age, site_id, biome) %>%
  drop_na()

unique(biome_dat$biome)

biome_mdl <- lmer(sqrt(agc) ~ sqrt(age)*biome + (1|site_id), data = biome_dat)

plot(residuals(biome_mdl, type = "pearson"))

nrow(biome_dat)
summary(biome_mdl)
anova(biome_mdl)

#------------------------------------------
# Full model

famd_dat <- data.frame(famd.res$ind[[1]][, 1:2]) %>%
  rename(axis1 = Dim.1, axis2 = Dim.2)

famd_dat$binomial <- row.names(famd_dat)

row.names(famd_dat) <- NULL

ktchn_snk_dat <- mdl_dat %>%
  dplyr::select(-plot_id) %>%
  left_join(famd_dat) %>%
  mutate(rate = agc / age) %>%
    filter(!is.na(prior), !is.na(site_id),
           !is.na(biome), !is.na(axis1)) %>%
  filter(prior != "M") %>%
  group_by(genus) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  filter(n >= 20)
  
nrow(ktchn_snk_dat)

ktchn_snk <- lmer(sqrt(agc) ~ sqrt(age) + genus + endemism + prior + biome + 
                    axis1 + axis2 + (1|site_id), data = ktchn_snk_dat)
ktchn_snk_s <- lmer(sqrt(agc) ~ sqrt(age) + genus + endemism + prior + 
                      biome + axis1 + axis2 + planting_spacing  + (1|site_id), 
                    data = filter(ktchn_snk_dat, !is.na(planting_spacing)))
ktchn_snk_f <- lmer(sqrt(agc) ~ sqrt(age) + genus + endemism + prior + biome +
                     axis1 + axis2 + fertilized + (1|site_id), 
                   data = filter(ktchn_snk_dat, !is.na(fertilized)))


plot(residuals(ktchn_snk, type = "pearson"))

anova(ktchn_snk)
summary(ktchn_snk)

anova(ktchn_snk_s)
summary(ktchn_snk_s)
nrow(filter(ktchn_snk_dat, !is.na(planting_spacing)))

anova(ktchn_snk_f)
summary(ktchn_snk_f)
nrow(filter(ktchn_snk_dat, !is.na(fertilized)))

#---------------------
# Clean up work space

rm(list = ls())
