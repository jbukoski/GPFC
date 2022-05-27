#############################################
## Extended & Supplementary Visualizations ##
#############################################

#-------------------------
# Load necessary libraries

library(FactoMineR) # v 2.4
library(factoextra) # v 1.0.7
library(ggpubr)     # v 0.4.0
library(ggthemes)   # v 4.2.4
library(grid)       # v 4.0.4
library(gridExtra)  # v 2.3
library(nlme)       # v 3.1.153
library(plotrix)    # v 3.8.2
library(saemix)     # v 2.4
library(readxl)     # v 1.3.1
library(tidyverse)  # v 1.3.1


#--------------------------------
# Load in necessary data

sites <- read_excel("./data/GPFC_database.xlsx", sheet = 2)
obs <- read_excel("./data/GPFC_database.xlsx", sheet = 3)

viz_dat <- obs %>%
  left_join(sites) %>%
  dplyr::select(agc, age, binomial, plot_id:study_id, qaqc, type, 
                endemism, planting_spacing:thinned, prior, biome, 
                leaf_type:wood_density, country) %>%
  separate(binomial, c("genus"), sep = " ", remove = F)

rm(obs, sites)

#---------------------------------------------------------------------
#######################################################################
## Supp Fig 1. Parameters for plots with and without chronosequences ##
#######################################################################


dat1 <- read_csv("./data/params_m2.csv") %>%
  mutate(chronos = "+ Chronosequences")

dat2 <- read_csv("./data/params_m2_chronosAsPlots.csv") %>%
  mutate(chronos = "- Chronosequences")

dat <- bind_rows(dat1, dat2)

grp1 <- c("Temperate needleleaf", "Temperate broadleaf", "Tropical needleleaf",
          "Tropical broadleaf", "Boreal needleleaf")

grp2 <- c("Pinus", "Eucalyptus", "Cunninghamia", "Picea", "Populus", 
          "Pseudotsuga", "Larix", "Cryptomeria", "Acacia")

p1 <- dat %>%
  filter(grp %in% grp1) %>%
  ggplot(aes(x = a, y = chronos, col = chronos)) +
  geom_point() +
  geom_errorbar(aes(xmin = a-(1.96*a_se), xmax = a+(1.96*a_se), 
                    y = chronos, width = 0.1, col = chronos)) +
  facet_grid(grp ~ ., switch = "y") +
  theme_bw() + 
  theme(panel.grid = element_blank(),
        strip.text.y.left = element_text(angle = 0),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        legend.title = element_blank())

p2 <- dat %>%
  filter(grp %in% grp1) %>%
  ggplot(aes(x = k, y = chronos, col = chronos)) +
  geom_point() +
  geom_errorbar(aes(xmin = k-(1.96*k_se), xmax = k+(1.96*k_se), 
                    y = chronos, width = 0.1, col = chronos)) +
  facet_grid(grp ~ ., switch = "y") +
  theme_bw() + 
  theme(panel.grid = element_blank(),
        strip.text.y.left = element_text(angle = 0),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        legend.title = element_blank())

p3 <- dat %>%
  filter(grp %in% grp2) %>%
  ggplot(aes(x = a, y = chronos, col = chronos)) +
  geom_point() +
  geom_errorbar(aes(xmin = a-(1.96*a_se), xmax = a+(1.96*a_se), 
                    y = chronos, width = 0.1, col = chronos)) +
  facet_grid(grp ~ ., switch = "y") +
  xlab('"A" parameter') +
  theme_bw() + 
  theme(panel.grid = element_blank(),
        strip.text.y.left = element_text(angle = 0),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        legend.title = element_blank())

p4 <- dat %>%
  filter(grp %in% grp2) %>%
  ggplot(aes(x = k, y = chronos, col = chronos)) +
  geom_point() +
  geom_errorbar(aes(xmin = k-(1.96*k_se), xmax = k+(1.96*k_se), 
                    y = chronos, width = 0.1, col = chronos)) +
  facet_grid(grp ~ ., switch = "y") +
  xlab('"k" parameter') +
  theme_bw() + 
  theme(panel.grid = element_blank(),
        strip.text.y.left = element_text(angle = 0),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        legend.title = element_blank())

s_fig1A <- grid.arrange(p1, p2, nrow = 1)
s_fig1B <- grid.arrange(p3, p4, nrow = 1)

ggsave("./figs/supp_fig1_panelA.jpg", s_fig1A, device = "jpg", width = 8, height = 3)
ggsave("./figs/supp_fig1_panelB.jpg", s_fig1B, device = "jpg", width = 8, height = 5)


#-------------------------------------------------------
######################################################
## Supp Fig 2. Visualization of data transformation ##
######################################################

fig_s2 <- viz_dat %>%
  ggplot(aes(x = sqrt(age), y = sqrt(agc))) +
  ylab(bquote('Aboveground carbon'~(Mg~ha^-1))) +
  xlab("Stand age (years)") +
  geom_point() +
  theme_tufte() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(colour = "black", size = 0.25),
        text = element_text(family = "sans"))

ggsave("./figs/supp_fig2_sqrtTransform.jpg", fig_s2, device = "jpg", height = 5, width = 5)


#-------------------------------------------------------
#######################################
## Supp Fig 3. Model form comparison ##
#######################################

source("./scripts/99_compareFunctions.R")

pt_dat <- viz_dat %>%
  mutate(agc_ln = log(agc), age_inv = 1/age) %>%
  filter(type == "Temperate needleleaf" & genus == "Pinus")

fits <- compareCurves(pt_dat)

s2_plot <- fits %>%
  ggplot() +
  geom_point(data = pt_dat, aes(x = age, y = agc), col = "dark grey", size = 0.75) +
  geom_line(aes(x = x, y = y_log), col = "red") +
  geom_line(aes(x = x, y = y_lgstc), col = "blue") +
  geom_line(aes(x = x, y = y_nl), col = "dark green") +
  geom_line(aes(x = x, y = y_cr), col = "purple") +
  xlab("Stand age (yrs)") +
  ylab(expression(paste("Aboveground Carbon (Mg ", " ha"^-1, ")"))) +
  ylim(c(0, 200)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())

ggsave("./figs/supp_fig3_modelForms.jpg", device = "jpg", width = 7, height = 5)


#-------------------------------------------------------
############################
## Supp Fig 4. FAMD plots ##
############################

famd_dat <- read_csv("./data/famd_df.csv")

famd.res <- FAMD(famd_dat, graph = FALSE)

df <- data.frame(famd.res$ind$coord)

p1 <- fviz_famd_var(famd.res, repel = T) +
  ylim(c(0, 1.0)) +
  xlim(c(0, 1.0)) +
  ggtitle("a) Loading of FAMD axes") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

p2 <- fviz_famd_var(famd.res, "quali.var", labelsize = 4, col.var = "red", repel = TRUE) +
  geom_point(data = df, aes(x = Dim.1, y = Dim.2), col = "dark grey" , alpha = 0.5) +
  ggtitle("b) Species groupings by categorical traits") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

famd_fig <- grid.arrange(p1, p2, nrow = 1)

ggsave("./figs/supp_fig4_famd.jpg", famd_fig, device = "jpg", width = 10, height = 4.5)


#-------------------------------------------------
##################################################
## Supp Fig 5. C-R curves for boreal needleleaf ##
##################################################

# See "03_mainVisualizations.R"


#-------------------------------------------------
##################################################
## Supp Fig 6. Predicted vs. observed for Pinus ##
##################################################
#
# This figure shows the C-R model predictions on individual observations 
# (incorporating site-level random effects), and plots it against the
# observed data.

f6_df <- read_csv("./data/fig6_data_pinus_predObs.csv")

f6 <- ggplot(f6_df) +
  geom_point(aes(x = obs, y = pred)) +
  geom_line(aes(x = obs, y = obs), col = "red", size = 1.05) +
  xlab(bquote('Observed'~(Mg~AGC~ha^-1))) +
  ylab(bquote('Predicted'~(Mg~AGC~ha^-1))) +
  theme_bw() +
  theme(panel.grid = element_blank())

ggsave("./figs/supp_fig6_predObs.jpg", f6, device = "jpg", width = 5, height = 5)


#------------------------------------
# Clean up working space

rm(list = ls())
