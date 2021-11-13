library(stars)
library(terra)
library(job)
library(rhdf5)
library(zeallot)

library(JuliaCall)
julia_setup()
julia_source("/mnt/i/GitHub/geo-julia/stars.jl/src/stars.jl")

infile = path.mnt("I:/Research/phenology/gee_whittaker/data-raw/NorthChina/MOD13A2_NorthChina_2016_2020_EVI.tif")
system.time({
    d <- julia_call("stars.st_as_sf", infile) %>%
        set_names(c("data", "mask_lgl", "bbox"))
})

b = julia_call("stars.bbox2vec", d$bbox)
## add function for the reverse proces

job({
    s = read_stars(infile)
    x = s[, ,,1:20]
    d_R <- st_as_sf(x, as_points = TRUE) # , downsample = 30
}, import = "auto")


# load("scripts/data/MOD13A2_Henan_2015_2020.rda")
{
    file = path.mnt("I:/Research/phenology/phenofit.R/scripts/data/MOD13A2_Henan_2015_2020.jld2")
    h5ls(file)
    mask_lgl = h5read(file, "mask_lgl")
    bbox = h5read(file, "bbox")
    h5closeAll()
}


s = read_stars(infile)
# x = s[, ,,1:2]

# 1. removes NA areas
# 2. default fs polygon
system.time({
    d <- st_as_sf(x, as_points = TRUE, downsample = 30)
})


{
    r = rast("/mnt/i/GitHub/geo-julia/stars.jl/MOD13A2_Henan_2015_2020.tif")
    r2 = aggregate(r, fact = 20)
    writeRaster(r2[[1:10]], "MOD13A2_Henan_2015_2020_10km.tif", overwrite = T)
}
