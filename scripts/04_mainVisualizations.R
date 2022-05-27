#########################
## Main Visualizations ##
#########################

## Contents:
## 
## Figure 1. Map of site locations
## Figure 2. Map of As and ks
## Figure 3. C-R Fits against data for plant functional types
## Figure 4. C-R Fits against data for genera
## Figure 5. Comparison of C-R fits across PFTs and genera
## Figure 6. Mean annualized carbon accumulation rates

## NOTE: We have NOT provided all data necessary to recreate all of the figures.
## Some of the data are large geospatial datasets that are already available 
## in the public domain. If you need to reproduce these figures and require the
## data, please reach out to the corresponding author.

#-------------------------
# Load necessary libraries & data

library(ggpubr)             # v 0.4.0
library(ggthemes)           # v 4.2.4
library(grid)               # v 4.0.4
library(gridExtra)          # v 2.3
library(lme4)               # v 1.1.27.1
library(officer)            # v 0.4.2
library(rvg)                # v 0.2.5
library(rnaturalearth)      # v 0.1.0
library(rnaturalearthdata)  # v 0.1.0
library(readxl)             # v 1.3.1
library(sf)                 # v 1.0.3
library(tidyverse)          # v 1.3.1
library(units)              # v 0.7.2

#------------------------
# Load in main dataset

sites <- read_excel("./data/GPFC_database.xlsx", sheet = 2)
obs <- read_excel("./data/GPFC_database.xlsx", sheet = 3)

viz_dat <- obs %>%
  left_join(sites) %>%
  dplyr::select(agc, age, binomial, plot_id:study_id, qaqc, type, 
                endemism, planting_spacing:thinned, prior, biome, 
                leaf_type:wood_density, country) %>%
  separate(binomial, c("genus"), sep = " ", remove = F)

rm(obs, sites)


#-----------------------------------
#########################
## Fig 1. Map of sites ##
#########################

world <- ne_countries(scale = "medium", returnclass = "sf")
world_proj <- st_transform(world, crs = "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs")

sites <- read_sf("./data/figData/plantation_sites.shp") %>%
  mutate(qaqc = ifelse(qaqc == 4, 1, qaqc))

ecoregions <- read_sf("./data/figData/Ecoregions2017.shp")

frsts <- c("Boreal Forests/Taiga", "Mangroves", "Mediterranean Forests, Woodlands & Scrub",
           "Temperate Broadleaf & Mixed Forests", "Temperate Conifer Forests",
           "Tropical & Subtropical Coniferous Forests", "Tropical & Subtropical Dry Broadleaf Forests",
           "Tropical & Subtropical Moist Broadleaf Forests")

grsld <- c("Tropical & Subtropical Grasslands, Savannas & Shrublands",
           "Temperate Grasslands, Savannas & Shrublands",
           "Montane Grasslands & Shrublands",
           "Flooded Grasslands & Savannas")

forests <- filter(ecoregions, BIOME_NAME %in% frsts)
grasslands <- filter(ecoregions, BIOME_NAME %in% grsld)

fig1_map <- ggplot(data = world_proj) +
  geom_sf(color = "black", fill = "white", size = 0.1) +
  geom_sf(data = forests, fill = "#117733", col = NA, alpha = 0.3) +
  geom_sf(data = grasslands, fill = "#ddcc77", col = NA, alpha = 0.3) +
  geom_sf(data = sites, size = 0.2, color = "#cc6677", fill = "#cc6677") +
  guides(colour = guide_legend(override.aes = list(size = 2.5))) +
  ylim(-6300000, 8374700) +
  theme_bw() +
  theme(axis.title = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = c(0.15, 0.3),
        legend.title = element_blank(),
        panel.border = element_blank(),
        legend.key = element_rect(fill = "transparent", colour = "transparent"),
        text = element_text(family = "sans"))

# Save as .jpg

ggsave("./figs/fig1.pdf", fig1_map, width = 7.08661, height = 3.5) 

##-------------------------------------------
#############################
## Fig 2. Map of As and ks ##
#############################

# Load in necessary datasets

params <- read_csv("./data/params_m2.csv")
world <- ne_countries(scale = "medium", returnclass = "sf")

cntrys <- c("arg", "aus", "bra", "chl", "chn", "cmr", "cod", "col", "cri", "ecu", 
            "eu", "gtm", "idn", "ind", "jpn", "ken", "khm", "kor", "lbr", "mex", 
            "mmr", "mwi", "mys", "npl", "nzl", "pak", "per", "phl", "png", "rwa", 
            "tha", "ury", "usa", "vnm", "zaf")

eu_cntrys <- c("aut", "bel", "bgr", "hrv", "cyp", "dnk", "fra", "hun", "irl", 
               "ita", "lux", "mlt", "prt", "rou", "svn", "esp", "gbr")

world_proj <- st_transform(world, crs = "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs")
world_proj2 <- filter(world_proj, iso_a3 %in% c(toupper(cntrys)) | iso_a3 %in% c(toupper(eu_cntrys)))

oceania <- read_sf("./data/figData/oceania.shp")
eu_usa <- read_sf("./data/figData/eu_usa.shp")
afr <- read_sf("./data/figData/afr.shp")
sam <- read_sf("./data/figData/sam.shp")
asia <- read_sf("./data/figData/asia.shp")
s_asia <- read_sf("./data/figData/s_asia.shp")

# Build map panels

k_map <- ggplot(data = world_proj) +
  geom_sf(color = "white", fill = "#ECECEC", size = 0.01) +
  geom_sf(data = world_proj2, color = "white", fill = "light grey", size = 0.01) +
  geom_sf(data = oceania, aes(fill = k, col = k), size = 0.01) +
  geom_sf(data = eu_usa, aes(fill = k, col = k), size = 0.01) +
  geom_sf(data = afr, aes(fill = A, col = A), size = 0.01) +
  geom_sf(data = sam, aes(fill = k, col = k), size = 0.01) +
  geom_sf(data = asia, aes(fill = k, col = k), size = 0.01) +
  geom_sf(data = s_asia, aes(fill = k, col = k), size = 0.01) +
  scale_color_gradient(low = "#A1E3F7", high = "#3C65D1", limits = c(0, 0.35)) +
  scale_fill_gradient(low = "#A1E3F7", high = "#3C65D1", limits = c(0, 0.35)) +
  ylim(-6300000, 8374700) +
  theme_bw() +
  theme(axis.title = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.border = element_blank(),
        legend.position = c(0.1, 0.5),
        legend.text = element_text(size=5),
        legend.title = element_text(size=6),
        legend.key.width = unit(0.12, "inch"),
        legend.key.height = unit(0.12, "inch"))

A_map <- ggplot(data = world_proj) +
  geom_sf(color = "white", fill = "#ECECEC", size = 0.01) +
  geom_sf(data = world_proj2, color = "white", fill = "light grey", size = 0.01) +
  geom_sf(data = oceania, aes(fill = A, col = A), size = 0.01) +
  geom_sf(data = eu_usa, aes(fill = A, col = A), size = 0.01) +
  geom_sf(data = afr, aes(fill = A, col = A), size = 0.01) +
  geom_sf(data = sam, aes(fill = A, col = A), size = 0.01) +
  geom_sf(data = asia, aes(fill = A, col = A), size = 0.01) +
  geom_sf(data = s_asia, aes(fill = A, col = A), size = 0.01) +
  scale_color_gradient(low = "#C5E3BF", high = "#006400", limits = c(0, 225)) +
  scale_fill_gradient(low = "#C5E3BF", high = "#006400", limits = c(0, 225)) +
  ylim(-6300000, 8374700) +
  theme_bw() +
  theme(axis.title = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.border = element_blank(),
        legend.position = c(0.1, 0.5),
        legend.text = element_text(size=5),
        legend.title = element_text(size=6),
        legend.key.width = unit(0.12, "inch"),
        legend.key.height = unit(0.12, "inch"))

# Produce bar charts

params$grp <- factor(params$grp, levels = c("Picea", "Cunninghamia", "Cryptomeria", "Pinus", "Larix", "Pseudotsuga",  
                                            "Populus", "Eucalyptus", "Acacia", "Boreal needleleaf", "Temperate needleleaf", 
                                            "Tropical needleleaf", "Temperate broadleaf", "Tropical broadleaf"))

k_bar <- params %>%
  mutate(k = as.numeric(k), k_se = as.numeric(k_se)) %>%
  drop_na() %>%
  ggplot() +
  geom_point(aes(x = grp, y = k, col = k), size = 0.7) +
  geom_errorbar(aes(x = grp, ymin = k - k_se, ymax = k + k_se, col = k), width = NA, size = 0.25) +
  ylab(expression(paste("Rate of growth (", italic(k), "; unitless)"))) +
  scale_color_gradient(low = "#A1E3F7", high = "#3C65D1", limits = c(0, 0.35)) +
  coord_flip() +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(size = 0.1),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line.y = element_line(size = 0.25),
        legend.position = "none",
        text = element_text(size = 5, family = "sans"),
        axis.ticks = element_blank(),
        plot.margin = margin(t = 12, r = 5.5, b = 5.5, l = 10, unit = "pt"))

params$grp <- factor(params$grp, levels = c("Acacia", "Larix", "Pinus", "Populus", "Pseudotsuga", 
                                            "Eucalyptus", "Cryptomeria",  "Cunninghamia", "Picea",
                                            "Boreal needleleaf", "Tropical broadleaf", "Temperate broadleaf", 
                                            "Temperate needleleaf", "Tropical needleleaf"))

A_bar <- params %>%
  mutate(A = as.numeric(a), A_se = as.numeric(a_se)) %>%
  drop_na() %>%
  ggplot() +
  geom_point(aes(x = grp, y = A, col = A), size = 0.7) +
  geom_errorbar(aes(x = grp, ymin = A - A_se, ymax = A + A_se, col = A), width = NA, size = 0.25) +
  ylab(expression(paste("Asymptotic Growth Limit (", italic(A), "; Mg C ", "ha"^-1, ")"))) +
  scale_color_gradient(low = "#C5E3BF", high = "#006400", limits = c(0, 275)) +
  coord_flip() +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(size = 0.1),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line.y = element_line(size = 0.25),
        legend.position = "none",
        text = element_text(size = 5, family = "sans"),
        axis.ticks = element_blank(),
        plot.margin = margin(t = 12, r = 5.5, b = 5.5, l = 10, unit = "pt"))

# Combine panels and write out as PDF

all <- ggarrange(k_map, k_bar, A_map, A_bar, nrow = 2, ncol = 2, widths = c(4, 2.5), 
                 labels = c("a", "b", "c", "d"), font.label = list(size = 8))

ggsave("./figs/fig2.pdf", all, width = 7, height = 4, units = "in", device = cairo_pdf)

#----------------------------------------------------
##############################
## Fig 3 & 4. Growth curves ##
## (+ Supp Fig 5 - Boreal)  ##
##############################

# Load in helper function

source("./scripts/99_buildPlots.R")

# By PFT

p1 <- build_plot(filter(viz_dat, type == "Temperate broadleaf"), subset = "Temperate broadleaf")
p2 <- build_plot(filter(viz_dat, type %in% c("Temperate needleleaf", "Temperate scaled")), subset = "Temperate needleleaf")
p3 <- build_plot(filter(viz_dat, type == "Tropical broadleaf"), subset = "Tropical broadleaf")
p4 <- build_plot(filter(viz_dat, type %in% c("Tropical needleleaf", "Tropical scaled")), subset = "Tropical needleleaf")
p6 <- build_plot(filter(viz_dat, type == "Boreal needleleaf"), subset = "Boreal needleleaf") +
  xlab("Stand age (years)") +
  ylab(expression(paste("Aboveground Carbon (Mg ", "ha"^-1, ")"))) +
  ylim(c(0, 300))

# By genus

p7 <- build_plot(filter(viz_dat, genus == "Acacia"), subset = "Acacia")
p8 <- build_plot(filter(viz_dat, genus == "Cryptomeria"), subset = "Cryptomeria")
p9 <- build_plot(filter(viz_dat, genus == "Cunninghamia"), subset = "Cunninghamia")
p10 <- build_plot(filter(viz_dat, genus == "Eucalyptus"), subset = "Eucalyptus")
p11 <- build_plot(filter(viz_dat, genus == "Larix"), subset = "Larix")
p12 <- build_plot(filter(viz_dat, genus == "Picea"), subset = "Picea")
p13 <- build_plot(filter(viz_dat, genus == "Pinus"), subset = "Pinus")
p14 <- build_plot(filter(viz_dat, genus == "Populus"), subset = "Populus")
p15 <- build_plot(filter(viz_dat, genus == "Pseudotsuga"), subset = "Pseudotsuga")

y_axis <- textGrob(expression(paste("Aboveground Carbon (Mg ", "ha"^-1, ")")), rot = 90, gp = gpar(fontsize = 7))
x_axis <- textGrob(expression(paste("Stand age (years)")), gp = gpar(fontsize = 7))

curvesPlotPFT <- grid.arrange(arrangeGrob(p1, p2, p3, p4, left = y_axis, bottom = x_axis), nrow = 1)
curvesPlotGenera <- grid.arrange(arrangeGrob(p7, p8, p9, p10, p11, p12, p13, p14, p15, left = y_axis, bottom = x_axis), nrow = 1)
  
# ggsave("./figs/fig2_curvesPFT.jpg", curvesPlotPFT, width = 5.5, height = 5.5)
# ggsave("./figs/fig3_curvesGenera.jpg", curvesPlotGenera, width = 7.08331, height = 7.08331)
ggsave("./figs/supp_fig5_borealCurve.jpg", p6, width = 4, height = 3)


# Write out Figure 3

doc <- read_pptx()
doc <- add_slide(doc, 'Title and Content', 'Office Theme')
doc <- ph_with(doc, value = dml(ggobj = as_ggplot(curvesPlotPFT)), location = ph_location(width = 5.5, height = 5.5))  
print(doc, target = "./figs/fig3.pptx")


# Write out Figure 4

doc <- read_pptx()
doc <- add_slide(doc, 'Title and Content', 'Office Theme')
doc <- ph_with(doc, value = dml(ggobj = as_ggplot(curvesPlotGenera)), location = ph_location(width = 7.08331, height = 7.08331))
print(doc, target = "./figs/fig4.pptx")


#-------------------------------------------------
########################################################
## Fig. 5 - Comparisons of y(t)~t, dydt~t, and dydt~y ##
########################################################

params_m3 <- read_csv("./data/params_m3.csv") %>%
  filter(grp != "Mediterranean needleleaf") %>%
  mutate(m_real = 0.67)

gnra <- c("Acacia", "Cunninghamia", "Cryptomeria", "Eucalyptus", 
          "Larix", "Picea", "Pinus", "Populus", "Pseudotsuga")

pfts <- c("Temperate broadleaf", "Temperate needleleaf", 
          "Tropical broadleaf", "Tropical needleleaf", "Boreal needleleaf")

gnra_ages <- viz_dat %>%
  filter(genus %in% c(gnra)) %>%
  group_by(genus) %>%
  summarize(max_age = max(age))

pfts_ages <- viz_dat %>%
  filter(type %in% c(pfts)) %>%
  group_by(type) %>%
  summarize(max_age = max(age))

a <- params_m3$a
k <- params_m3$k
m <- params_m3$m_real

params_m3 <- params_m3 %>%
  left_join(gnra_ages, by = c("grp" = "genus")) %>%
  left_join(pfts_ages, by = c("grp" = "type")) %>%
  mutate(max_age = ifelse(is.na(max_age.x), max_age.y, max_age.x)) %>%
  dplyr::select(-max_age.x, -max_age.y)


df <- data.frame()

for(i in 1:14) {
  
  grp <- params_m3$grp[i]
  
  x <- seq(0, params_m3$max_age[i], 0.5)
  y <- a[i] * (1-exp(-k[i]*x)) ^ (1/(1-m[i])) 
  rely <- y / a[i]
  dydt <- (k[i]/m[i])*y*((a[i]/y)^m[i] - 1)
  dydt_y <- dydt/y
  mai <- y / x
  jeff <- k[i] * 3*((rely ^ (1/3)) - 1)
  
  sbst <- as.data.frame(bind_cols(grp = grp,
                                  rely = as.numeric(rely),
                                  x = as.numeric(x), 
                                  y = as.numeric(y), 
                                  dydt = as.numeric(dydt),
                                  dydt_y = as.numeric(dydt_y),
                                  mai = as.numeric(mai),
                                  jeff = jeff))
  
  df <- bind_rows(df, sbst)
  
}


# For genera & pfts

cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")

sbst <- pfts
ltype = 2

p1 <- df %>%
  filter((grp %in% sbst)) %>%
  ggplot() +
  geom_line(aes(x = x, y = y, col = as.factor(grp)), lty = ltype, size = 0.75) +
  xlab("Age (years)") +
  ylab(bquote('Aboveground carbon (Mg ha-1)')) +
  scale_color_manual(values = cbp1) +
  ylim(c(0, 200)) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        text = element_text(size = 7, family = "Arial"))

p2 <- df %>%
  filter((grp %in% sbst)) %>%
  ggplot() +
  geom_line(aes(x = x, y = dydt, col = as.factor(grp)), lty = ltype, size = 0.75) +
  xlab("Age (years)") +
  ylab("dy/dt") +
  scale_color_manual(values = cbp1) +
  guides(color = guide_legend(byrow = TRUE)) +
  ylim(c(0, 17)) +
  theme_bw() +
  theme(legend.position = c(0.7, 0.8),
        legend.title = element_blank(),
        legend.background = element_rect(fill = NA),
        legend.key.size = unit(0.5, 'lines'),
        panel.grid = element_blank(),
        text = element_text(size = 7, family = "Arial"))

p3 <- df %>%
  filter((grp %in% sbst)) %>%
  ggplot() +
  geom_line(aes(x = y, y = dydt, col = as.factor(grp)), lty = ltype, size = 0.75) +
  xlab(bquote('Aboveground carbon (Mg ha-1)')) +
  ylab("dy/dt") +
  scale_color_manual(values = cbp1) +
  ylim(c(0, 17)) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        text = element_text(size = 7, family = "Arial"))


modelForms_pfts <- grid.arrange(arrangeGrob(p1, p2, p3, ncol = 3))


sbst <- gnra
ltype = 1

p1 <- df %>%
  filter((grp %in% sbst)) %>%
  ggplot() +
  geom_line(aes(x = x, y = y, col = as.factor(grp)), lty = ltype, size = 0.75) +
  xlab("Age (years)") +
  ylab(bquote('Aboveground carbon (Mg ha-1)')) +
  scale_color_manual(values = cbp1) +
  ylim(c(0, 200)) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        text = element_text(size = 7, family = "Arial"))

p2 <- df %>%
  filter((grp %in% sbst)) %>%
  ggplot() +
  geom_line(aes(x = x, y = dydt, col = as.factor(grp)), lty = ltype, size = 0.75) +
  xlab("Age (years)") +
  ylab("dy/dt") +
  scale_color_manual(values = cbp1) +
  guides(color = guide_legend(byrow = TRUE)) +
  ylim(c(0, 17)) +
  theme_bw() +
  theme(legend.position = c(0.65, 0.7),
        legend.title = element_blank(),
        legend.background = element_rect(fill = NA),
        legend.key.size = unit(0.5, 'lines'),
        panel.grid = element_blank(),
        text = element_text(size = 7, family = "Arial"))

p3 <- df %>%
  filter((grp %in% sbst)) %>%
  ggplot() +
  geom_line(aes(x = y, y = dydt, col = as.factor(grp)), lty = ltype, size = 0.75) +
  xlab(bquote('Aboveground carbon (Mg ha-1)')) +
  ylab("dy/dt") +
  scale_color_manual(values = cbp1) +
  ylim(c(0, 17)) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        text = element_text(size = 7, family = "Arial"))


modelForms_gnra <- grid.arrange(arrangeGrob(p1, p2, p3, ncol = 3))

doc <- read_pptx()
doc <- add_slide(doc, 'Title and Content', 'Office Theme')
doc <- ph_with(doc, value = dml(ggobj = as_ggplot(modelForms_pfts)), location = ph_location(width = 7, height = 3))  
doc <- add_slide(doc, 'Title and Content', 'Office Theme')
doc <- ph_with(doc, value = dml(ggobj = as_ggplot(modelForms_gnra)), location = ph_location(width = 7, height = 3))  
print(doc, target = "./figs/fig5.pptx")

#------------------------------------------------
###########################################
## Fig 6. Mean carbon accumulation rates ##
###########################################

# Calculate mean rates

genera <- c("Eucalyptus", "Acacia", "Populus", "Tectona", "Cryptomeria",
            "Pseudotsuga", "Cunninghamia", "Pinus", "Picea")

gns_rates <- viz_dat  %>%
  filter(age <= 20) %>%
  mutate(rate = agc/age) %>%
  group_by(genus) %>%
  mutate(rate_avg = mean(rate),
         rate_se = plotrix::std.error(rate)) %>%
  dplyr::select(genus, age, rate, rate_avg, rate_se) %>%
  filter(genus %in% genera) %>%
  drop_na() %>%
  mutate(genus = factor(genus, levels = genera))

pft_rates <- viz_dat %>%
  filter(age <= 20) %>%
  mutate(type_abrv = case_when(type == "Tropical broadleaf" ~ "TrB",
                               type == "Temperate broadleaf" ~ "TeB",
                               type == "Tropical needleleaf" ~ "TrN",
                               type == "Temperate needleleaf" ~ "TeN",
                               type == "Mediterranean needleleaf" ~ "MeN",
                               type == "Boreal needleleaf" ~ "BoN")) %>%
  mutate(rate = agc/age) %>%
  group_by(type_abrv) %>%
  mutate(rate_avg = mean(rate),
         rate_se = plotrix::std.error(rate)) %>%
  dplyr::select(type_abrv, rate, rate_avg, rate_se) %>%
  mutate(type_abrv = factor(type_abrv, levels = c("TrB", "TeB", "TrN", "TeN", "MeN", "BoN"))) %>%
  drop_na()

biome_rates <- viz_dat %>%
  filter(age <= 20) %>%
  mutate(biome_abrv = case_when(biome == "Mediterranean Forests, Woodlands & Scrub" ~ "MFWS",
                                biome == "Tropical & Subtropical Dry Broadleaf Forests" ~ "TSDBF",
                                biome == "Tropical & Subtropical Moist Broadleaf Forests" ~ "TSMBF",
                                biome == "Tropical & Subtropical Grasslands, Savannas & Shrublands" ~ "TSGSS",
                                biome == "Temperate Conifer Forests" ~ "TCF",
                                biome == "Deserts & Xeric Shrublands" ~ "DXS",
                                biome == "Temperate Grasslands, Savannas & Shrublands" ~ "TGSS",
                                biome == "Temperate Broadleaf & Mixed Forests" ~ "TBMF")) %>%
  mutate(rate = agc/age) %>%
  group_by(biome_abrv) %>%
  mutate(rate_avg = mean(rate),
         rate_se = plotrix::std.error(rate),
         n = n()) %>%
  ungroup() %>%
  filter(n > 30) %>%
  dplyr::select(biome_abrv, rate, rate_avg, rate_se) %>%
  filter(!(biome_abrv %in% c("DXS"))) %>%
  drop_na() %>%
  mutate(biome_abrv = factor(biome_abrv, levels = c("TSGSS", "TSDBF", "TSMBF", "TBMF", "TGSS", "TCF", "MFWS")))

a <- gns_rates %>%
  ggplot(aes(x = genus, y = rate)) +
  geom_jitter(alpha = 0.5, width = 0.1, col = "grey", size = 0.5) +
  geom_boxplot(fill = NA, width = 0.25) +
  geom_point(aes(x = genus, y = rate_avg), col = "red") +
  theme_tufte() +
  ggtitle("a") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line = element_line(colour = "black", size = 0.25),
        plot.title = element_text(size = 11),
        plot.margin = unit(c(4, 4, 4, 4), "mm"),
        text = element_text(family = "sans", size = 7))

b <- pft_rates %>%
  ggplot(aes(x = type_abrv, y = rate)) +
  geom_jitter(alpha = 0.5, width = 0.1, col = "grey", size = 0.5) +
  geom_boxplot(fill = NA, width = 0.25) +
  geom_point(aes(x = type_abrv, y = rate_avg), col = "red") +
  theme_tufte() +
  ggtitle("b") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line = element_line(colour = "black", size = 0.25),
        plot.title = element_text(size = 11),
        plot.margin = unit(c(4, 4, 4, 4), "mm"),
        text = element_text(family = "sans", size = 7))

c <- biome_rates %>%
  ggplot(aes(x = biome_abrv, y = rate)) +
  geom_jitter(alpha = 0.5, width = 0.1, col = "grey", size = 0.5) +
  geom_boxplot(fill = NA, width = 0.25) +
  geom_point(aes(x = biome_abrv, y = rate_avg), col = "red") +
  theme_tufte() +
  ggtitle("c") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line = element_line(colour = "black", size = 0.25),
        plot.title = element_text(size = 11),
        plot.margin = unit(c(4, 4, 4, 4), "mm"),
        text = element_text(family = "sans", size = 7))

y_axis <- textGrob(expression(paste("Carbon accumulation (Mg ha-1 yr)")), 
                   rot = 90, gp = gpar(fontfamily = "sans", fontsize = 10))

ratesPlot <- grid.arrange(arrangeGrob(a, b, c, left = y_axis), nrow = 1)

# Save as vector-based editable figure.

doc <- read_pptx()
doc <- add_slide(doc, 'Title and Content', 'Office Theme')
doc <- ph_with(doc, value = dml(ggobj = as_ggplot(ratesPlot)), location = ph_location(width = 7, height = 9))  

print(doc, target = "./figs/fig6.pptx")


# Get summary values for results section

gns_rates %>% 
  dplyr::select(genus, rate_avg, rate_se) %>% 
  unique() %>% 
  arrange(-rate_avg)

pft_rates %>% 
  dplyr::select(type_abrv, rate_avg, rate_se) %>% 
  unique() %>% 
  arrange(-rate_avg)

biome_rates %>% 
  dplyr::select(biome_abrv, rate_avg, rate_se) %>% 
  unique() %>% 
  arrange(-rate_avg)

#------------
# Cleaning up the workspace 

rm(list = ls())
