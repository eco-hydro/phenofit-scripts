# setwd("~/github/eco-hydro")
# load("~/github/eco-hydro/MOD13A2_Henan_Input_2015_2020.rda") # INPUT
# load("MOD13A2_Henan_pheno_V2.rda")
source("scripts/main_pkgs.R")
outdir = "OUTPUT/phenofit_V0.3.4_KongYing_wHANTS_V2"

file_vi <- path.mnt("I:/Research/phenology/gee_whittaker/data-raw/NorthChina/sentinel2_KongYing_2019-2021_EVI2.tif")
file_qc <- path.mnt("I:/Research/phenology/gee_whittaker/data-raw/NorthChina/sentinel2_KongYing_2019-2021_SCL.tif")

l <- read_rast(file_vi) %>% rast2mat()
l_qc <- read_rast(file_qc) %>% rast2mat()
dates <- l$dates
data <- listk(VI = l$mat, QC = l_qc$mat, dates, qcFUN = qc_sentinel2)

load("sentinel2_kong_V2.rda")

df = res %>% rm_empty() %>% melt_list("gridId") %>% cbind(meth = "rough", .) #%>% dplyr::select(-meth)
df[, gridId := as.integer(gridId)]
# df[, meth := as.factor(meth)]
inds_bad = df[grep("_4", flag)]$gridId %>% unique() %>% sort()

# df <- fread("pheno_V4.csv")
# fwrite(df2, "pheno_V4.csv")
df2 = select_first_nGS(df, nGS = 3)
point2rast(df2, l$d_coord, outdir = outdir,
           prefix = "Sentinel2_KongYing_Pheno", overwrite = TRUE)


files = dir(outdir, "*.tif", full.names = TRUE)

library(lattice.layers)
{
    source("scripts/main_pkgs.R")
    tmp =foreach(file = files, i = icount(3)) %do% {
        # file = files[2]
        season = basename(file) %>% str_extract("\\d(?=\\.tif)") %>% as.numeric()
        brks = switch(season,
                    `1` = list(sos = seq(10, 120, 10), eos = seq(110, 170, 5)),
                    `2` = list(sos = seq(170, 220, 5), eos = seq(230, 280, 5)),
                    list(sos = seq(180, 220, 5), eos = seq(220, 300, 10)))
        # outfile = gsub(".tif$", ".pdf", file)
        outfile = basename(file)

        g = plot_phenomap(file, brks = brks, overwrite = TRUE)
        write_fig(g, outfile, 9.6, 7*2/3, show = T)
    }
}

# library(rasterInspect)
# rasterInspect(raster(r))
