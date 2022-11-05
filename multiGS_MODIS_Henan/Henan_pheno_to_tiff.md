
``` r
source("main_pkgs.R")
# Registered S3 method overwritten by 'Ipaper':
#   method           from      
#   print.data.table data.table
# 
# Attaching package: 'Ipaper'
# The following object is masked from 'package:phenofit':
# 
#     melt_list
# 
# Attaching package: 'sf2'
# The following object is masked from 'package:graphics':
# 
#     plot
# The following object is masked from 'package:base':
# 
#     plot
# Registered S3 method overwritten by 'lattice.layers':
#   method    from        
#   +.trellis latticeExtra
# 
# Attaching package: 'lattice.layers'
# The following object is masked from 'package:rcolors':
# 
#     get_color
# The following object is masked from 'package:Ipaper':
# 
#     dev_off
# The following objects are masked from 'package:phenofit':
# 
#     get_options, set_options
# 
# Attaching package: 'lubridate'
# The following objects are masked from 'package:base':
# 
#     date, intersect, setdiff, union
# 
# Attaching package: 'dplyr'
# The following objects are masked from 'package:stats':
# 
#     filter, lag
# The following objects are masked from 'package:base':
# 
#     intersect, setdiff, setequal, union
# 
# Attaching package: 'data.table'
# The following objects are masked from 'package:dplyr':
# 
#     between, first, last
# The following objects are masked from 'package:lubridate':
# 
#     hour, isoweek, mday, minute, month, quarter, second, wday, week,
#     yday, year
# The following objects are masked from 'package:Ipaper':
# 
#     first, last, transpose
# terra 1.6.17
# 
# Attaching package: 'terra'
# The following object is masked from 'package:data.table':
# 
#     shift
# The following object is masked from 'package:grid':
# 
#     depth
# The following objects are masked from 'package:lattice.layers':
# 
#     rotate, wrap
# The following object is masked from 'package:Ipaper':
# 
#     clamp
# The following objects are masked from 'package:magrittr':
# 
#     extract, inset

mkdir("Figures")
# [1] "Figures"
outdir <- "OUTPUT/phenofit_V0.3.5_wHANTS_MODIS_Henan/PhenoData" %>% mkdir()
```

# 1. Load Data

``` r
load("data/MOD13A2_Henan_2015_2020.rda")
load(glue("{dirname(outdir)}/pheno_Henan_MODIS_V6.1.rda"))
```

# 2. tidy data

> Convert point scale phenological metrics into tiff file

``` r
# this step is very slow, at 10~20s
l = res %>% rm_empty()
gridId = names(l) %>% as.integer()
df = melt_list(l, gridId = gridId) #%>% dplyr::select(-meth)
df[, meth := as.factor(meth)]
head(df)
# [data.table]: 
# # A tibble: 6 × 17
#   gridId meth  flag   origin     TRS5.sos TRS5.eos DER.sos DER.pos DER.eos    UD
#    <int> <fct> <chr>  <date>        <dbl>    <dbl>   <dbl>   <dbl>   <dbl> <dbl>
# 1      1 Beck  2015_1 2015-01-01      114      272     114     141     273   108
# 2      1 Beck  2016_1 2016-01-01      101      283     102     173     283    79
# 3      1 Beck  2017_1 2017-01-01      113      277     113     158     279   100
# 4      1 Beck  2018_1 2018-01-01      102      282     103     175     281    76
# 5      1 Beck  2019_1 2019-01-01      113      269     113     166     269    94
# 6      1 Beck  2020_1 2020-01-01      121      271     121     182     271    99
# # … with 7 more variables: SD <dbl>, DD <dbl>, RD <dbl>, Greenup <dbl>,
# #   Maturity <dbl>, Senescence <dbl>, Dormancy <dbl>
```

``` r
## some pixels may have more than 3 growing seasons, eliminate them
# inds_bad = df[grep("_4", flag)]$gridId %>% unique() %>% sort()

# only select the first `nGS` growing seasons
df2 = select_first_nGS(df, nGS = 3)

# rda to tiff
point2rast(df2, d_coord, outdir = outdir,
           prefix = "MOD13A2_Henan_Pheno", overwrite = TRUE)
# OUTPUT/phenofit_V0.3.5_wHANTS_MODIS_Henan/PhenoData/MOD13A2_Henan_Pheno_Beck_2015_1.tif 
# OUTPUT/phenofit_V0.3.5_wHANTS_MODIS_Henan/PhenoData/MOD13A2_Henan_Pheno_Beck_2015_2.tif 
# OUTPUT/phenofit_V0.3.5_wHANTS_MODIS_Henan/PhenoData/MOD13A2_Henan_Pheno_Beck_2015_3.tif 
# OUTPUT/phenofit_V0.3.5_wHANTS_MODIS_Henan/PhenoData/MOD13A2_Henan_Pheno_Beck_2016_1.tif 
# OUTPUT/phenofit_V0.3.5_wHANTS_MODIS_Henan/PhenoData/MOD13A2_Henan_Pheno_Beck_2016_2.tif 
# OUTPUT/phenofit_V0.3.5_wHANTS_MODIS_Henan/PhenoData/MOD13A2_Henan_Pheno_Beck_2016_3.tif 
# OUTPUT/phenofit_V0.3.5_wHANTS_MODIS_Henan/PhenoData/MOD13A2_Henan_Pheno_Beck_2017_1.tif 
# OUTPUT/phenofit_V0.3.5_wHANTS_MODIS_Henan/PhenoData/MOD13A2_Henan_Pheno_Beck_2017_2.tif 
# OUTPUT/phenofit_V0.3.5_wHANTS_MODIS_Henan/PhenoData/MOD13A2_Henan_Pheno_Beck_2017_3.tif 
# OUTPUT/phenofit_V0.3.5_wHANTS_MODIS_Henan/PhenoData/MOD13A2_Henan_Pheno_Beck_2018_1.tif 
# OUTPUT/phenofit_V0.3.5_wHANTS_MODIS_Henan/PhenoData/MOD13A2_Henan_Pheno_Beck_2018_2.tif 
# OUTPUT/phenofit_V0.3.5_wHANTS_MODIS_Henan/PhenoData/MOD13A2_Henan_Pheno_Beck_2018_3.tif 
# OUTPUT/phenofit_V0.3.5_wHANTS_MODIS_Henan/PhenoData/MOD13A2_Henan_Pheno_Beck_2019_1.tif 
# OUTPUT/phenofit_V0.3.5_wHANTS_MODIS_Henan/PhenoData/MOD13A2_Henan_Pheno_Beck_2019_2.tif 
# OUTPUT/phenofit_V0.3.5_wHANTS_MODIS_Henan/PhenoData/MOD13A2_Henan_Pheno_Beck_2019_3.tif 
# OUTPUT/phenofit_V0.3.5_wHANTS_MODIS_Henan/PhenoData/MOD13A2_Henan_Pheno_Beck_2020_1.tif 
# OUTPUT/phenofit_V0.3.5_wHANTS_MODIS_Henan/PhenoData/MOD13A2_Henan_Pheno_Beck_2020_2.tif 
# OUTPUT/phenofit_V0.3.5_wHANTS_MODIS_Henan/PhenoData/MOD13A2_Henan_Pheno_Beck_2020_3.tif 
# OUTPUT/phenofit_V0.3.5_wHANTS_MODIS_Henan/PhenoData/MOD13A2_Henan_Pheno_NA_NA_1.tif 
# OUTPUT/phenofit_V0.3.5_wHANTS_MODIS_Henan/PhenoData/MOD13A2_Henan_Pheno_NA_NA_2.tif 
# OUTPUT/phenofit_V0.3.5_wHANTS_MODIS_Henan/PhenoData/MOD13A2_Henan_Pheno_NA_NA_3.tif
```

## Part2: Visualization ——————————————————–

``` r
## how many growing seasons in Henan 2015
source("main_pkgs.R")
poly <- vect("data-raw/shp/poly_Henan.shp")

# outdir = "OUTPUT/phenofit_V0.3.4_wHANTS_MODIS_Henan"
# outdir = "OUTPUT/phenofit_V0.3.5_wHANTS_MODIS_Henan/PhenoData" %>% mkdir()
files <- dir(outdir, "*.tif", full.names = TRUE)

lst = map(files[1:3], ~ !is.na(sum(rast(.x)[[1:2]])) )
r_nGS = do.call(c, lst) %>% sum()
grid_nGS = as_SpatialPixelsDataFrame(raster::brick(r_nGS))

write_fig({
    plot(mask(r_nGS, poly))
}, "Figures/Figure_S4_Henan_growing_season_numbers.svg", 7, 5)
```

[](Figures/Figure_S4_Henan_growing_season_numbers.svg)

``` r
# r = rast(files[1])
ps = foreach(file = files, i = icount(3)) %do% {
    if (i > 3) return()
    season = basename(file) %>% str_extract("\\d(?=\\.tif)") %>% as.numeric()
    brks = switch(season,
                `1` = list(sos = seq(10, 120, 10), eos = seq(110, 170, 5)),
                `2` = list(sos = seq(170, 220, 5), eos = seq(230, 280, 5)),
                `3` = list(sos = seq(300, 340, 5), eos = seq(360, 400, 5)))
    odir = dirname(dirname(file))
    name = basename(file) %>% gsub(".tif$", ".pdf", .) # confirm figure type 
    outfile = sprintf("%s/%s", odir, name)

    g = plot_phenomap(file, brks = brks,
        sp_layout = sp_layout,
        xlim = c(110.3, 116.8),
        ylim = c(31.3, 36.5),
        overwrite = TRUE)
    write_fig(g, outfile, 9.75, 7, show = T) # , use.file_show = TRUE
    g
    # pdf_view(outfile)
}
# [1] "OUTPUT/phenofit_V0.3.5_wHANTS_MODIS_Henan/PhenoData/MOD13A2_Henan_Pheno_Beck_2015_1.pdf"
# [1] "OUTPUT/phenofit_V0.3.5_wHANTS_MODIS_Henan/PhenoData/MOD13A2_Henan_Pheno_Beck_2015_2.pdf"
# [1] "OUTPUT/phenofit_V0.3.5_wHANTS_MODIS_Henan/PhenoData/MOD13A2_Henan_Pheno_Beck_2015_3.pdf"
## show the first figure
g = ps[[1]]
grid.draw(g)
```

<img src="Figures/unnamed-chunk-8-1.svg" style="display: block; margin: auto;" />
