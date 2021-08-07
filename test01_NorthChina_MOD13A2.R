library(phenofit)
library(Ipaper) # remotes::install_github("rpkgs/Ipaper")
library(grid)
library(ggplot2)
library(ggnewscale)
library(lubridate)
library(zeallot)

# library(JuliaCall)
# julia_setup()

infile = path.mnt("I:/Research/phenology/gee_whittaker/data-raw/NorthChina/MOD13A2_NorthChina_2016_2020_EVI.tif")
# dates = terra::rast(infile) %>% names() %>%
#     substr(1, 10) %>% as.Date("%Y_%m_%d")
modis_date <- function(date_begin, date_end, dn = 8) {
    year_begin = year(date_begin)
    year_end = year(date_end)
    dates = map(year_begin:year_end, function(year) {
        sprintf("%d%03d", year, seq(1, 366, dn)) %>% as.Date("%Y%j")
    }) %>% do.call(c, .)
    dates[dates >= date_begin & dates <= date_end]
}
dates = modis_date("2015-01-01", "2020-12-31", 16)
load("scripts/data/MOD13A2_Henan_2015_2020.rda")

{
    # phenofit parameters
    nptperyear     <- 23
    wFUN           <- wTSM # wBisquare
    wmin           <- 0.2
    methods_fine   <- c("AG", "Zhang", "Beck", "Elmore", "Gu")[3]
    # data("CA_NS6")
}

i = 1
set_options(verbose_season_mov = F)
phenofit_point <- function(i, plot = FALSE, verbose = FALSE) {
    d = data.table(EVI = EVI[i,], doy = DOY[i,], qc = QC[i, ])
    t = getRealDate(dates, d$doy)
    c(QC_flag, w) %<-% qc_summary(d$qc, wmin = 0.2)

    input <- check_input(t, d$EVI/1e4, d$w, QC_flag = QC_flag,
                         nptperyear = nptperyear, maxgap = nptperyear / 4, wmin = 0.2)
    brks <- season_mov(input,
                       options = list(
                           FUN = smooth_wWHIT, wFUN = wFUN,
                           verbose = verbose,
                           maxExtendMonth = 12,
                           wmin = wmin, r_min = 0.1))
    # plot_season(input, brks)
    ## 2.4 Curve fitting
    fit  <- curvefits(input, brks,
                      options = list(
                          methods = methods_fine, #,"klos",, 'Gu'
                          wFUN = wFUN, iters = 2,
                          wmin = wmin,
                          nextend = 2,
                          verbose = verbose,
                          minExtendMonth = 0.5, maxExtendMonth = 1, minPercValid = 0
                      ), constrain = T)

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
                         expand = c(1, 1)*0.07),
            scale_y_continuous(limits = c(0, 0.6), breaks = seq(0, 0.6, 0.3))
        )
        # fine fitting
        g <- plot_curvefits(dfit, brks, title = NULL, cex = 1.5, ylab = "EVI",
                            layer_extra = layer_extra, angle = 0)
        Ipaper::write_fig(g, "Figure5_curvefitting.pdf", 8, 6, show = TRUE)
    }
    pheno
}

i = 877

set_options(verbose_season_mov = T, season = list(lambda = 2))
get_options("season")$lambda

n = nrow(EVI)
phenofit_point(i)

system.time({
    res = foreach(i = 1:100 %>% set_names(., .), icount()) %do% {
        runningId(i)
        tryCatch({
            phenofit_point(i)
        }, error = function(e) {
            message(sprintf('%s', e$message))
        })
    }
})


## 2. Visualization ------------------------------------------------------------
# growing season dividing
# Ipaper::write_fig({
#     par(cex = 1.1)
#     plot_season(INPUT, brks, ylab = "EVI", margin = 0.2, show.shade = FALSE)
# }, "Figure4_seasons.pdf", 9, 3.8)
