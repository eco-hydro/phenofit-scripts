# source("main_pkgs.R")
library(phenofit)
library(Ipaper) # remotes::install_github("rpkgs/Ipaper")
library(grid)
library(ggplot2)
library(ggnewscale)
library(lubridate)
library(zeallot)

library(dplyr)
library(job)
library(sp)
library(terra)

modis_date <- function(date_begin, date_end, dn = 8) {
    year_begin = year(date_begin)
    year_end = year(date_end)
    dates = map(year_begin:year_end, function(year) {
        sprintf("%d%03d", year, seq(1, 366, dn)) %>% as.Date("%Y%j")
    }) %>% do.call(c, .)
    dates[dates >= date_begin & dates <= date_end]
}

get_input <- function(i) {
    d = data.table(EVI = EVI[i,], doy = DOY[i,], qc = QC[i, ]) %>%
        mutate(t = getRealDate(dates, doy))
}

phenofit_point <- function(i, plot = FALSE, verbose = FALSE,
                           period = c(2015, 2020), wmid = 0.5) {

    d = data.table(EVI = EVI[i,], doy = DOY[i,], qc = QC[i, ]) %>%
        mutate(t = getRealDate(dates, doy))
    if (!is.null(period)) d <- d[year(t) >= period[1] & year(t) <= period[2]]

    c(QC_flag, w) %<-% qc_summary(d$qc, wmin = 0.2, wmid = wmid)
    input <- check_input(d$t, d$EVI/1e4, d$w, QC_flag = QC_flag,
                         nptperyear = nptperyear, maxgap = nptperyear / 4, wmin = 0.2)
    brks <- season_mov(input)

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
        years = 2010:2021
        layer_extra = list(
            scale_x_date(breaks = make_date(years), labels = years,
                         limits = c(make_date(years[1]), make_date(last(years), 12, 31)),
                         expand = c(1, 1)*0.07)
            # scale_y_continuous(limits = c(0, 0.6), breaks = seq(0, 0.6, 0.3))
        )
        # BUG: unknown reason, `scale_y_continuous` leads color's order changing.
        # fine fitting
        # browser()
        g <- plot_curvefits(dfit, brks, title = NULL, cex = 1.5, ylab = "EVI",
                            layer_extra = layer_extra, angle = 0)
        Ipaper::write_fig(g, "Figure5_curvefitting.pdf", 8,
                          length(get_options("fitting")$methods)*2, show = TRUE)
    }
    listk(pheno, fit, brks)
    # pheno
}

prj84 = sp::CRS("+proj=longlat +datum=WGS84 +no_defs")
df2sp <- function (d, formula = ~lon + lat, prj) {
    if (missing(prj))
        prj <- prj84
    coordinates(d) <- formula
    proj4string(d) <- prj
    return(d)
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
