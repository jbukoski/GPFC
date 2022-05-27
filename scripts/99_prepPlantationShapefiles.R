#######################################################
## Prep Spatial Database of Planted Trees shapefiles ##
#######################################################

library(sf)         # v 1.0.3
library(tidyverse)  # v 1.3.1

#-------------------
# Load in necessary datasets and process

params <- read_csv("./data/params_m2.csv")

arg <- read_sf("./data/figData/shps/arg_plant/arg_plant.shp")
aus <- read_sf("./data/figData/shps/aus_plant/aus_plant.shp")
bra <- read_sf("./data/figData/shps/bra_plant/bra_plant.shp")
chl <- read_sf("./data/figData/shps/chl_plant/chl_plant.shp")
chn <- read_sf("./data/figData/shps/chn_plant/chn_plant.shp")
cmr <- read_sf("./data/figData/shps/cmr_plant/cmr_plant.shp")
cod <- read_sf("./data/figData/shps/cod_plant/cod_plant.shp")
col <- read_sf("./data/figData/shps/col_plant/col_plant.shp")
cri <- read_sf("./data/figData/shps/cri_plant/cri_plant.shp")
ecu <- read_sf("./data/figData/shps/ecu_plant/ecu_plant.shp")
eur <- read_sf("./data/figData/shps/eu_plant/eu_plant.shp")
gtm <- read_sf("./data/figData/shps/gtm_plant/gtm_plant.shp")
idn <- read_sf("./data/figData/shps/idn_plant/idn_plant.shp")
ind <- read_sf("./data/figData/shps/ind_plant/ind_plant.shp")
jpn <- read_sf("./data/figData/shps/jpn_plant/jpn_plant.shp")
ken <- read_sf("./data/figData/shps/ken_plant/ken_plant.shp")
khm <- read_sf("./data/figData/shps/khm_plant/khm_plant.shp")
kor <- read_sf("./data/figData/shps/kor_plant/kor_plant.shp")
lbr <- read_sf("./data/figData/shps/lbr_plant/lbr_plant.shp")
mex <- read_sf("./data/figData/shps/mex_plant/mex_plant.shp")
mmr <- read_sf("./data/figData/shps/mmr_plant/mmr_plant.shp")
mwi <- read_sf("./data/figData/shps/mwi_plant/mwi_plant.shp")
mys <- read_sf("./data/figData/shps/mys_plant/mys_plant.shp")
npl <- read_sf("./data/figData/shps/npl_plant/npl_plant.shp")
nzl <- read_sf("./data/figData/shps/nzl_plant/nzl_plant.shp")
pak <- read_sf("./data/figData/shps/pak_plant/pak_plant.shp")
per <- read_sf("./data/figData/shps/per_plant/per_plant.shp")
phl <- read_sf("./data/figData/shps/phl_plant/phl_plant.shp")
png <- read_sf("./data/figData/shps/png_plant/png_plant.shp")
rwa <- read_sf("./data/figData/shps/rwa_plant/rwa_plant.shp")
tha <- read_sf("./data/figData/shps/tha_plant/tha_plant.shp")
ury <- read_sf("./data/figData/shps/ury_plant/ury_plant.shp")
usa <- read_sf("./data/figData/shps/usa_plant/usa_plant.shp")
vnm <- read_sf("./data/figData/shps/vnm_plant/vnm_plant.shp")
zaf <- read_sf("./data/figData/shps/zaf_plant/zaf_plant.shp")

prepPolys <- function(df, params) {
  
  df %>%
    dplyr::select(model) %>%
    left_join(params, by = c("model" = "grp")) %>%
    mutate(k = as.numeric(k), A = as.numeric(a)) %>%
    select(A, k) %>%
    filter(st_is_valid(geometry)) %>%       # Drop invalid geometries
    mutate(area = st_area(geometry)) %>%
    filter(area > set_units(1000000, m^2))  # Drop any polygons that are < 1 sq km
  
}

oceania <- prepPolys(bind_rows(aus, idn, nzl, phl, png), params)
eu_usa <- prepPolys(bind_rows(eur, usa), params)
afr <- prepPolys(bind_rows(cmr, cod, ken, lbr, mwi, rwa, zaf), params)
sam <- prepPolys(bind_rows(arg, bra, chl, col, cri, ecu, gtm, mex, per, ury), params)
asia <- prepPolys(bind_rows(chn, jpn, ken, khm, kor, mys, tha, vnm), params)
s_asia <- prepPolys(bind_rows(ind, mmr, npl, pak), params)


#_---------------------
# Write to file

st_write(oceania, "./data/figData/oceania.shp")
st_write(eu_usa, "./data/figData/eu_usa.shp")
st_write(afr, "./data/figData/afr.shp")
st_write(sam, "./data/figData/sam.shp")
st_write(asia, "./data/figData/asia.shp")
st_write(s_asia, "./data/figData/s_asia.shp")

#----------------------
# Clean up environment

rm(list = ls())
