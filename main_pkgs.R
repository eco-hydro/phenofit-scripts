# source("main_pkgs.R")
suppressMessages({
    library(phenofit)
    library(Ipaper) # remotes::install_github("rpkgs/Ipaper")
    library(sf2) # remotes::install_github("rpkgs/sf.extra")
    library(rcolors)
    # library(lattice.layers)

    library(grid)
    library(ggplot2)
    library(ggnewscale)
    library(lubridate)
    library(zeallot)
    library(stringr)

    library(dplyr)
    library(job)
    library(sp)
    # library(sf)
    # library(terra)

    library(data.table)
    library(dplyr)
})
source("main_terra.R")

poly = sf::read_sf("data-raw/shp/bou2_4p_ChinaProvince.shp") %>%
    dplyr::filter(NAME == "河南省")
vect = vect(poly)
shp <- sf::as_Spatial(poly)
sp_layout <- list("sp.lines", shp, lwd = 0.5, first = FALSE)

modis_date <- function(date_begin, date_end, dn = 8) {
    year_begin = year(date_begin)
    year_end = year(date_end)
    dates = map(year_begin:year_end, function(year) {
        sprintf("%d%03d", year, seq(1, 366, dn)) %>% as.Date("%Y%j")
    }) %>% do.call(c, .)
    dates[dates >= date_begin & dates <= date_end]
}

#' @param data A list object, with the elements of
#' - `VI`: vegetation index, in the range of `[-1, 1]`
#' - `QC`: quality control variable
#' - `DOY`: (optional) Day of year
#' - `dates`: corresponding dates of VI
get_input <- function(i, data, wmin = 0.2, wmid = 0.5) {
    d = data.table(VI = data$VI[i,], QC = data$QC[i, ])
    if (!is.null(data$DOY)) {
        # this is for MODIS DOY
        d %<>% mutate(t = getRealDate(data$dates, data$DOY[i, ]))
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

    nptperyear = phenofit::get_options("nptperyear")
    input <- check_input(dates, d$VI, d$w, QC_flag = d$QC_flag,
                         nptperyear = nptperyear,
                         maxgap = nptperyear / 4, wmin = 0.2)
    brks <- season_mov(input)

    # plot_season(input, brks)
    ## 2.4 Curve fitting
    fit  <- curvefits(input, brks, constrain = T)
    # ## check the curve fitting parameters
    # l_param <- get_param(fit)

    # ## 2.5 Extract phenology
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
        dfit <- get_fitting(fit)
        g <- plot_curvefits(dfit, brks, title = title, cex = 1.5, ylab = "EVI",
                            layer_extra = layer_extra, angle = 0, show.legend = show.legend)
        # grid.newpage()
        grid.draw(g)
    }
    listk(pheno, fit, brks)
}

#' select_first_nGS
#' @export
select_first_nGS <- function(df, nGS = 3) {
    ind_valid = rowMeans(df[, -(1:4)], na.rm = TRUE) %>% which.notna()
    df = df[ind_valid, ] %>% cbind(I = 1:nrow(.), .) # rm all NA records

    # inds_bad = df[, grep("_4", flag)]
    gs = as.numeric(substr(df$flag, 6, 6))
    inds_bad = gs %>% { which(. > nGS) }

    if (length(inds_bad) == 0) {
        return(df %>% select(-I, -TRS5.los))
    }
    info_bad = df[inds_bad, .N, .(gridId, meth, origin)] %>% select(-N)

    df_bad = merge(df, info_bad) %>%
        reorder_name(c("gridId", "meth", "origin", "flag"))
    df_good = df[-df_bad$I, ]

    df_mutiGS = dt_ddply(df_bad, .(gridId, meth, origin), function(d) {
        los = d$TRS5.eos - d$TRS5.sos
        n = length(los)
        if (n > nGS) {
            i_bad = which.min(los)
            # if the smallest GS in the head or tail
            if (i_bad %in% c(1, n)) {
                d = d[-i_bad, ]
            } else {
                # rm the GS with the largest ratio of unintersect period with the current year
                perc_bad_head = pmax(0 - d$TRS5.sos[1], 0) / d$TRS5.eos[1]
                perc_bad_tail = pmax(d$TRS5.eos[n] - 365, 0) / d$TRS5.eos[n]
                i_bad = ifelse(perc_bad_head >= perc_bad_tail, 1, n)
                d = d[-i_bad, ]
            }
            n = nrow(d)
            # if still > nGS, then only kept the l
            if (n > nGS) {
                ind = order(los, decreasing = TRUE)[1:nGS]
                d = d[ind, ]
            }
        }
        d %>% mutate(flag = sprintf("%s_%d", year(origin), 1:nrow(.)))
        # d
    })
    rbind(
        df_good %>% select(-I),
        df_mutiGS %>% select(-I)
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
        mkdir(dirname(outfile))

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


#' @param ... other parameters to sp_plot, e.g. `xlim`, `ylim`.
plot_phenomap <- function(tif,
    outfile = NULL,
    brks = list(sos = seq(10, 120, 10), eos = seq(120, 200, 10)),
    sp_layout = NULL,
    ...,
    show = TRUE, overwrite = FALSE)
{
    if (is.null(outfile)) outfile = gsub(".tif$", ".pdf", tif)
    if (file.exists(outfile) && !overwrite) return()

    print(outfile)
    r = rast(tif) %>%
        raster::brick() %>%
        as_SpatialPixelsDataFrame()
    r@data %<>% as.data.frame()

    names = names(r)
    names_sos = c("TRS5.sos", "DER.sos", "UD", "SD", "Greenup", "Maturity") %>% intersect(names)
    names_eos = c("TRS5.eos", "DER.eos", "DD", "RD", "Senescence", "Dormancy") %>% intersect(names)
    brks_sos = brks$sos %>% c(-Inf, ., Inf)
    brks_eos = brks$eos %>% c(-Inf, ., Inf)

    layout = if(length(names_sos) <= 4) c(2, 2) else c(3, 2)

    # colors = get_color("BlGrYeOrReVi200")
    # col_sos = get_color(colors[(1:100)], length(brks_sos))
    # col_eos = get_color(colors[-(1:100)], length(brks_eos))
    # cols = c(col_sos, "grey", col_eos)
    cols_all = get_color("MPL_RdYlGn") #MPL_RdYlGn
    # cols_eos = cols_all[1:64]
    cols_eos = get_color("BlGrYeOrReVi200") # BlGrYeOrReVi200
    cols_sos = get_color("MPL_RdYlGn") %>% rev()

    cols_eos = get_color("BlGrYeOrReVi200") # BlGrYeOrReVi200
    cols_sos = get_color("MPL_RdYlGn") %>% rev()

    # for color blind people
    # PRGn
    cols_sos = c('#40004b','#762a83','#9970ab','#c2a5cf','#e7d4e8','#d9f0d3','#a6dba0','#5aae61','#1b7837','#00441b') %>% rev()
    # PIYg
    cols_eos = c('#8e0152','#c51b7d','#de77ae','#f1b6da','#fde0ef','#e6f5d0','#b8e186','#7fbc41','#4d9221','#276419') %>% rev()

    cols_sos = rcolors$RdYlBu %>% rev()
    cols_eos = rcolors$RdYlBu

    plot_sub <- function(names, cols, brks, NO_begin = 1) {
        nbrk = length(brks)
        cols = get_color(cols, nbrk)

        sp_plot(r[, names], cols = cols, brks = brks,
            NO_begin = NO_begin,
            strip = TRUE,
            par.strip.text = list(cex = 1.2),
            par.settings2 = list(axis.line = list(col = "black"),
            layout.heights=list(strip=1.2)),
            layout = layout,
            sp.layout = sp_layout, ...,
            aspect = 1) +
            # layer_barchart() +
            # layer_title(x = 0, y = 0.97) +
            theme_lattice(
                font_family = "Times",
                font_size = 14,
                plot.margin = c(1.5, 2, 0, -0.5)*1,
                key.margin = c(0, 1.4, 0, 0)
            )
    }
    p1 <- plot_sub(names_sos, cols_sos, brks_sos, NO_begin = 1)
    p2 <- plot_sub(names_eos, cols_eos, brks_eos, NO_begin = length(names_sos) + 1)
    g <- gridExtra::arrangeGrob(grobs = list(p1, p2), nrow = 1)
    g
    # write_fig(p1, outfile, 4.8, 6.8, show = T)
    # if (show) pdf_view(outfile)
}

