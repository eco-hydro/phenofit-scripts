# source("main_pkgs.R")
library(phenofit)
library(Ipaper) # remotes::install_github("rpkgs/Ipaper")
library(sf2)    # remotes::install_github("rpkgs/sf2")
library(grid)
library(ggplot2)
library(ggnewscale)
library(lubridate)
library(zeallot)
library(stringr)

library(dplyr)
library(job)
library(sp)
library(sf)
library(terra)

library(data.table)
library(dplyr)
library(rcolors)
library(lattice.layers)


prj84 = sp::CRS("+proj=longlat +datum=WGS84 +no_defs")
df2sp <- function (d, formula = ~lon + lat, prj) {
    if (missing(prj))
        prj <- prj84
    coordinates(d) <- formula
    proj4string(d) <- prj
    return(d)
}

#' read sentinel2 tiff file
read_rast <- function(file) {
    r = rast(file)
    # guess date from band names
    dates = names(r) %>% substr(1, 8) %>% as.Date("%Y%m%d")
    terra::time(r) = dates
    names(r) = dates
    r
}

#' convert `rast` to 2d data, with the dimension of [`ngrid`, `ntime`].
#'
#' @param r rast object
#' @param bareval the VI value of bare land. Pixels with mutli-annual mean of VI
#' less than `bareval` will be eliminated.
#'
#' @return
#' - `mat`: matrix (with the dimension of [`ngrid`, `ntime`]), Vegetation index (VI).
#' - `x_mean`: vector (with the length of `ngrid`), mutli-annual mean of VI.
#' - `dates`: Date vector (with the length of `ntime`), corresponding dates of VI.
#' - `d_coords`: A data.table, with the coordinates of each pixel.
#' - `I_grid`: Integer vector, pixels with `x_mean` greater than 0.1. If not, this pixel will be
#'    eleminated in phenology extraction.
#' - `grid`: SpatiaPixelsDataFrame object.
#'
#' @export
rast2mat <- function(r, backval = 0.1) {
    d_coord = rast_coord(r[[1]])
    arr = rast_array(r)
    mat = array_3dTo2d(arr)

    range = ext(r) %>% as.vector() # [xmin, xmax, ymin, ymax]
    cellsize = res(r)
    grid <- make_grid(range, cellsize)

    x_mean = mat %>% rowMeans(na.rm = TRUE)
    I_grid = which(!(x_mean <= backval | is.na(x_mean)))
    d_coord = grid@coords %>% as.data.table() %>% set_colnames(c("lon", "lat")) %>% cbind(I = 1:nrow(.), .)
    listk(mat, dates = terra::time(r), x_mean,
        d_coord, I_grid, grid)
}

modis_date <- function(date_begin, date_end, dn = 8) {
    year_begin = year(date_begin)
    year_end = year(date_end)
    dates = map(year_begin:year_end, function(year) {
        sprintf("%d%03d", year, seq(1, 366, dn)) %>% as.Date("%Y%j")
    }) %>% do.call(c, .)
    dates[dates >= date_begin & dates <= date_end]
}

#' @param data A list object, with the elements of
#' - `VI`: vegetation index
#' - `QC`: quality control variable
#' - `DOY`: (optional) Day of year
#' - `dates`: corresponding dates of VI
get_input <- function(i, data, wmin = 0.2, wmid = 0.5) {
    d = data.table(VI = data$VI[i,], QC = data$QC[i, ])
    if (!is.null(data$DOY)) {
        # this is for MODIS DOY
        d %<>% mutate(t = getRealDate(dates, data$DOY[i, ]))
    }
    c(d$QC_flag, d$w) %<-% data$qcFUN(d$QC, wmin = wmin, wmid = wmid)
    d
}

#' phenofit_point
#'
#' @param d A data.table or data.frame object, with the elements of
#' - `VI`: vegetation index
#' - `QC`: quality control variable
#' - `t`: corresponding dates of VI
#' @param dates corresponding dates of VI. If `d$t` is missing, `dates` will be used.
#' @param period used to constrain in plot
#'
#' @export
phenofit_point <- function(d, dates = NULL,
    plot = FALSE, title = NULL, show.legend = TRUE,
    verbose = FALSE,
    period = c(2015, 2020)) {

    if (!is.null(d$t)) dates = d$t
    input <- check_input(dates, d$VI, d$w, QC_flag = d$QC_flag,
                         nptperyear = get_options("nptperyear"),
                         maxgap = nptperyear / 4, wmin = 0.2)
    brks <- season_mov(input)
    # browser()

    # plot_season(input, brks)
    ## 2.4 Curve fitting
    fit  <- curvefits(input, brks, constrain = T)
    ## check the curve fitting parameters
    l_param <- get_param(fit)
    dfit   <- get_fitting(fit)

    ## 2.5 Extract phenology
    TRS = 0.5 #c(0.1, 0.2, 0.5)
    l_pheno <- get_pheno(fit, TRS = TRS, IsPlot = FALSE) #%>% map(~melt_list(., "meth"))
    pheno <- l_pheno$doy %>% melt_list("meth")

    if (plot) {
        years = period[1]:period[2]
        layer_extra = list(
            scale_x_date(breaks = make_date(years), labels = years,
                         limits = c(make_date(years[1]), make_date(last(years), 12, 31)),
                         expand = c(1, 1)*0.07)
            # scale_y_continuous(limits = c(0, 0.6), breaks = seq(0, 0.6, 0.3))
        )
        # BUG: unknown reason, `scale_y_continuous` leads color's order changing.
        # fine fitting
        g <- plot_curvefits(dfit, brks, title = title, cex = 1.5, ylab = "EVI",
                            layer_extra = layer_extra, angle = 0, show.legend = show.legend)
        # grid.newpage()
        grid.draw(g)
    }
    listk(pheno, fit, brks)
    # pheno
}

#' dump 4th season
#' @export
dump_4th_season <- function(df) {
    ind_valid = rowMeans(df[, -(1:4)], na.rm = TRUE) %>% which.notna()
    df = df[ind_valid, ] %>% cbind(I = 1:nrow(.), .) # rm all NA records

    inds_bad = df[, grep("_4", flag)]
    info_bad = df[inds_bad, .N, .(gridId, meth, origin)] %>% select(-N)

    df_bad = merge(df, info_bad) %>%
        mutate(TRS5.los = TRS5.eos - TRS5.sos) %>%
        reorder_name(c("gridId", "meth", "origin", "gridId", "flag", "TRS5.los"))
    df_good = df[-df_bad$I, ]

    df_mutiGS = dt_ddply(df_bad, .(gridId, meth, origin), function(d) {
       los = d$TRS5.los
       # if the smallest GS in the head or tail
       i_bad = which.min(los)
       n = length(los)
       if (n < 4) {
           # d
       } else if (i_bad %in% c(1, 4)) {
           d = d[-i_bad, ]
       } else {
           # rm head or tail
           perc_bad_head = pmax(0 - d$TRS5.sos[1], 0) / d$TRS5.eos[1]
           perc_bad_tail = pmax(d$TRS5.eos[n] - 365, 0) / d$TRS5.eos[n]
           i_bad = ifelse(perc_bad_head >= perc_bad_tail, 1, n)
           d = d[-i_bad, ]
       }
       d %>% mutate(flag = sprintf("%s_%d", year(origin), 1:nrow(.)))
    })
    df2 = rbind(
        df_good %>% select(-I),
        df_mutiGS %>% select(-I, -TRS5.los)
    )
}


#' point2rast
#'
#' @param df A data.table returned by `phenofit_point`, with one more column in
#' the head.
#' @param d_coord A data.table, at least with the column of `lon` and `lat`
#'
#' @export
point2rast <- function(df, d_coord,
                       outdir = "OUTPUT", prefix = "phenofit", overwrite = TRUE)
{
    mkdir(outdir)
    sp2 <- as(df2sp(d_coord), "SpatialPixelsDataFrame")
    # flags <- unique(df$flag) %>% sort()
    d_grp = unique(df[, .(meth, flag)])[order(flag)]

    for (i in 1:nrow(d_grp)) {
        # if (i != 1) next()
        METH = d_grp$meth[i]
        FLAG = d_grp$flag[i]

        outfile <- glue("{outdir}/{prefix}_{METH}_{FLAG}.tif")
        if (file.exists(outfile) && !overwrite) next()
        cat(outfile, "\n")

        d <- df[flag == FLAG & meth == METH, ]

        ind <- match(1:nrow(d_coord), d$gridId)
        data <- d[ind, -(1:4)]
        sp2@data <- data

        r <- raster::brick(sp2) %>% rast() %>% set_names(names(sp2))
        terra::writeRaster(r, outfile, overwrite = TRUE)
        # rgdal::setCPLConfigOption("GDAL_PAM_ENABLED", "FALSE")
        # file.remove(paste0(outfile, ".aux.xml")) # rm the .aux.xml file
    }
}


shp <- read_sf(path.mnt("D:/Documents/ArcGIS/china/bou2_4p_ChinaProvince.shp")) %>%
    dplyr::filter(NAME == "河南省") %>%
    sf::as_Spatial()
sp_layout <- list("sp.lines", shp, lwd = 0.5, first = FALSE)

plot_phenomap <- function(tif, outfile = NULL, show = TRUE, overwrite = FALSE){
    if (is.null(outfile)) outfile = gsub(".tif$", ".pdf", tif)
    if (file.exists(outfile) && !overwrite) return()

    print(outfile)
    r = rast(tif) %>%
        raster::brick() %>%
        as_SpatialPixelsDataFrame() %>%
        .[, -4]

    brks_sos = seq(170, 210, 10)
    brks_eos = seq(240, 300, 10)
    brks = c(-Inf, brks_sos, brks_eos, Inf)

    # colors = get_color("BlGrYeOrReVi200")
    # col_sos = get_color(colors[(1:100)], length(brks_sos))
    # col_eos = get_color(colors[-(1:100)], length(brks_eos))
    # cols = c(col_sos, "grey", col_eos)

    nbrk = length(brks)
    cols = get_color("BlGrYeOrReVi200", nbrk)
    p <- sp_plot(r, colors = cols, brks = brks,
                 sp.layout = sp_layout,
                 xlim = c(110.3, 116.8),
                 ylim = c(31.3, 36.6),
                aspect = 1) +
        layer_title(x = 0, y = 0.97) +
        theme_lattice(plot.margin = c(0, 0, 0, 0),
                      key.margin = c(0, 1.7, 0, 0))
        # layer_barchart()
    write_fig(p, outfile, 10, 6.8, show = F)
    if (show) pdf_view(outfile)
}
