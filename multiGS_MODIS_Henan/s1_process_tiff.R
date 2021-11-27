source("scripts/main_pkgs.R")

indir = path.mnt("I:/Research/phenology/gee_whittaker/data-raw/NorthChina")
file_vi  = glue("{indir}/MOD13A2_NorthChina_2016_2020_EVI.tif")
file_qc  = glue("{indir}/MOD13A2_NorthChina_2016_2020_SummaryQA.tif")
file_doy = glue("{indir}/MOD13A2_NorthChina_2016_2020_DayOfYear.tif")

## 1. clip Henan Province from NorthChina
rast_clip <- function(r, mask) {
    r %>% crop(mask) %>% mask(mask)
}

prefix = "Henan"
r = rast_clip(rast(file_vi), vect)
writeRaster(r, glue("{indir}/MOD13A2_{prefix}_2016_2020_EVI.tif"), overwrite = TRUE)

system.time({
    writeRaster(rast_clip(rast(file_qc), vect), 
        glue("{indir}/MOD13A2_{prefix}_2016_2020_SummaryQA.tif"), overwrite = TRUE)
    writeRaster(rast_clip(rast(file_doy), vect), 
        glue("{indir}/MOD13A2_{prefix}_2016_2020_DayOfYear.tif"), overwrite = TRUE)
})
