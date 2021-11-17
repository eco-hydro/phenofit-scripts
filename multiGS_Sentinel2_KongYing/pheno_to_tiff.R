# setwd("~/github/eco-hydro")
source("scripts/main_pkgs.R")

# load("~/github/eco-hydro/MOD13A2_Henan_Input_2015_2020.rda") # INPUT
# load("MOD13A2_Henan_pheno_V2.rda")
load("scripts/data/MOD13A2_Henan_2015_2020.rda")
d_coord %<>% data.table()
load("pheno_V4.rda")

# which.isnull(res) %>% length()
# i=275, j = 460, cell = 207330;
# lon = 114.17917, lat = 34.0875, val = NaN
# cellsize = 1/120
# d = d_coord[abs(lon - 114.17917) <= cellsize/2 & abs(lat - 34.0875) <= cellsize/2, id]

# job suit for the case of small memory used and long time consumed task.
# Only one method used in the test case
df = res %>% rm_empty() %>% melt_list("gridId") #%>% dplyr::select(-meth)
df[, gridId := as.integer(gridId)]
df[, meth := as.factor(meth)]
inds_bad = df[grep("_4", flag)]$gridId %>% unique() %>% sort()

# df <- fread("pheno_V4.csv")
df2 = dump_4th_season(df)
# fwrite(df2, "pheno_V4.csv")

# MOD13A2_Henan_Pheno
point2rast(df2, d_coord, outdir = "phenofit_V0.3.4_wHANTS",
           prefix = "MOD13A2_Henan_Pheno", overwrite = TRUE)

source("scripts/main_pkgs.R")
files = dir("phenofit_V0.3.4_wHANTS/", full.names = TRUE)
foreach(tif = files, i = icount()) %do% {
    plot_phenomap(tif, show = F)
}

# library(rasterInspect)
# rasterInspect(raster(r))
