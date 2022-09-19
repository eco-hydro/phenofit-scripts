# setwd("~/github/eco-hydro")
source("scripts/main_pkgs.R")
outdir = "OUTPUT/phenofit_V0.3.5_wHANTS_MODIS_Henan/PhenoData" %>% mkdir()

load("scripts/data/MOD13A2_Henan_2015_2020.rda")
load(glue("{dirname(outdir)}/pheno_Henan_MODIS_V6.1.rda"))

# which.isnull(res) %>% length()
# i=275, j = 460, cell = 207330;
# lon = 114.17917, lat = 34.0875, val = NaN
# cellsize = 1/120
# d = d_coord[abs(lon - 114.17917) <= cellsize/2 & abs(lat - 34.0875) <= cellsize/2, id]

# job suit for the case of small memory used and long time consumed task.
# Only one method used in the test case
df = res %>% rm_empty() %>% melt_list("gridId") #%>% dplyr::select(-meth)
# df = df[meth == "Beck"]
df[, gridId := as.integer(gridId)]
df[, meth := as.factor(meth)]
# inds_bad = df[grep("_4", flag)]$gridId %>% unique() %>% sort()

# df <- fread("pheno_V4.csv")
df2 = select_first_nGS(df, nGS = 3)
# fwrite(df2, "pheno_V4.csv")

# MOD13A2_Henan_Pheno
point2rast(df2, d_coord, outdir = outdir,
           prefix = "MOD13A2_Henan_Pheno", overwrite = TRUE)

## Part2: Visualization --------------------------------------------------------
outdir = "OUTPUT/phenofit_V0.3.4_wHANTS_MODIS_Henan"
# outdir = "OUTPUT/phenofit_V0.3.5_wHANTS_MODIS_Henan/PhenoData" %>% mkdir()
files = dir(outdir, "*.tif", full.names = TRUE)

## nGS in Henan 2015

source("scripts/main_pkgs.R")
library(lattice.layers)

poly = vect("/mnt/i/Research/phenology/phenofit.R/scripts/data/shp/poly_Henan.shp")

lst = map(files[1:3], ~ !is.na(sum(rast(.x)[[1:2]])) )
r_nGS = do.call(c, lst) %>% sum()
grid_nGS = as_SpatialPixelsDataFrame(raster::brick(r_nGS))

write_fig({
    plot(mask(r_nGS, poly))
}, "Henan_nGS.pdf", 7, 5)


{
    p <- sp_plot(grid_nGS,
                 axes = TRUE,
                 sp.layout = sp_layout,
                 # colors = c("green", "yellow", "red"),
                 brks = c(0, 1, 2, 3) + 0.5, key.num2factor = TRUE)
    write_fig(p, "Fig_S4_growing season numbers.pdf", 7, 6)
}
# r = rast(files[1])

tmp = foreach(file = files, i = icount(3)) %do% {
    if (i > 3) return()
    season = basename(file) %>% str_extract("\\d(?=\\.tif)") %>% as.numeric()
    brks = switch(season,
                `1` = list(sos = seq(10, 120, 10), eos = seq(110, 170, 5)),
                `2` = list(sos = seq(170, 220, 5), eos = seq(230, 280, 5)),
                `3` = list(sos = seq(300, 340, 5), eos = seq(360, 400, 5)))
    odir = dirname(dirname(file))
    name = basename(file) %>% gsub(".tif$", ".pdf", .)
    outfile = sprintf("%s/%s", odir, name)

    g = plot_phenomap(file, brks = brks,
        sp_layout = sp_layout,
        xlim = c(110.3, 116.8),
        ylim = c(31.3, 36.5),
        overwrite = TRUE)
    write_fig(g, outfile, 9.75, 7, show = T) # , use.file_show = TRUE
    # pdf_view(outfile)
}

# library(rasterInspect)
# rasterInspect(raster(r))
