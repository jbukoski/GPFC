# Global Plantation Forest Carbon (GPFC)

[![DOI](https://zenodo.org/badge/492990743.svg)](https://zenodo.org/badge/latestdoi/492990743)

This repository houses the Global Plantation Forest Carbon database and associated analyses (published in _Nature Communications_, Bukoski et al., 2022).

The database was compiled through a systematic review of the literature (the [GROA](https://github.com/forc-db/GROA) project), led by The Nature Conservancy and a team of scientists from 19 institutions. 

The data presented here are for monoculture (single-species) plantation forests. The database is structured similarly to the [GROA](https://github.com/forc-db/GROA) database and [ForC-db](https://github.com/forc-db). However, note that this database has not been formally integrated into ForC.

---

# Data Use Policy and Guidelines

### License

The GPFC database is licensed under the Creative Commons Zero v1.0 Universal (see License file).

### Database citation

Any publications using these data should cite both Bukoski et al. 2022 (which describes the construction of the database), as well as the DOI for the database itself: . If the data changes since the original publication, arising publications should cite the specific version used, ideally with a DOI associated with that version. Authors may contact Jacob J. Bukoski (Conservation International) to generate a release and associated DOI that matches the database version used.

### Citation

Bukoski, J.J., Cook-Patton, S.C., Melikov, C., Ban, H., Liu, J.C., Goldman, L., Harris, N., & Potts, M.D. 2022. Rates and drivers of aboveground carbon accumulation in global monoculture plantation forests. _Nature Communications_. In Press.

---

# Explanation of files in this repository

We provide all scripts associated with our analysis for reproducibility here. The R scripts included in this repository are:

- **01_growthCurves.R** - parameterization and validation of Chapman-Richard growth functions for each plant functional type and plantation genus.
- **02_driverAnalysis.R** - statistical analyses of potential biological, environmental, and management-based drivers of variation in plantation carbon accumulation.
- **03_summaryStats.R** - simple summary stats that are included in the manuscript (e.g., number of observations by country).
- **04_mainVisualizations.R** - R code to produce the figures that have been included in our manuscript.
- **05_supplementaryVisualizations.R** - R code to produce the figures found in our Supplementary Information file.
- **99_buildPlots.R** - a helper function used to create Figures 2 & 3.
- **99_compareFunctions.R** - a helper function used to compare different model forms.
- **99_parameterizeModels.R** - a wrapper function for the Chapman-Richards growth function parameterization, which facilitates for our bootstrapped validation procedure.
- **99_prepPlantationShapefiles.R** - a helper script to preprocess the Spatial Database of Planted Trees shapefiles for incorporation into Figure 2.

The first three scripts (01-03) should be fully reproducible after downloading the Global Plantation Forest Carbon database (GPFC_database.xlsx) from Zenodo (https://doi.org/10.5281/zenodo.6555216).

The visualization scripts (04-05) are not fully reproducible as we have not republished the spatial datasets included in the maps, as well as intermediate datasets. If you want access to these files, please reach out to Jacob (jbukoski@berkeley.edu) for assistance in accessing the files.

The current version of this repository has been archived on Zenodo: [![DOI](https://zenodo.org/badge/492990743.svg)](https://zenodo.org/badge/latestdoi/492990743)


---

### Contacts

Jacob J. Bukoski, Conservation International (jbukoski@conservation.org; jbukoski@berkeley.edu)


