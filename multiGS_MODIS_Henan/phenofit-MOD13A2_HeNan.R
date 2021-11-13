# library(JuliaCall)
# julia_setup()
source("scripts/main_pkgs.R")

# infile = path.mnt("I:/Research/phenology/gee_whittaker/data-raw/NorthChina/MOD13A2_NorthChina_2016_2020_EVI.tif")
# dates = terra::rast(infile) %>% names() %>%
#     substr(1, 10) %>% as.Date("%Y_%m_%d")
dates = modis_date("2015-01-01", "2020-12-31", 16)
load("scripts/data/MOD13A2_Henan_2015_2020.rda")
n = nrow(EVI)

{
    source("scripts/main_pkgs.R")
    # phenofit parameters
    # data("CA_NS6")
    # lambda = 0.5 suit for regions with multiple growing season
    nptperyear = 23
    opt_old <- get_options()
    set_options(
        wFUN = wTSM, wmin = 0.2,
        verbose = FALSE,
        season = list(
            # rFUN = "smooth_wWHIT", lambda = 0.5,
            rFUN = "smooth_wHANTS", nf = 6,
            maxExtendMonth = 12, r_min = 0.0),
        # fine fitting parameters
        fitting = list(nextend = 1, minExtendMonth = 0, maxExtendMonth = 0.5,
            methods = c("AG", "Zhang", "Beck", "Elmore", "Gu")[3], #,"klos",, 'Gu'
            iters = 1,
            minPercValid = 0)
    )
    # get_options("season")$lambda
    # i <- 877
    # n <- nrow(EVI)
    # id_bads = c(19564, 25769, 68760, 79072, 79073, 83378, 83379, 91274, 99172, 99173,
    #             113144, 119239, 125169, 125810, 132338, 136962, 136963, 138684)
    gridId_bads = c(1049, 1270, 1271, 1273, 1391, 1511, 1512, 1513, 1514, 1642, 1644,
                    1654, 2240, 2614, 2645, 2649, 2650, 2651, 2652, 2825)
    i <- gridId_bads[1]
    r <- phenofit_point(i, plot = TRUE, period = c(2015, 2020))
    set_options(opt_old)
}

InitCluster(10, kill = FALSE)
ind = 1:n
# ind = 1:100
t = system.time({
    res = foreach(i = ind %>% set_names(., .), icount()) %dopar% {
        runningId(i, 1000)
        tryCatch({
            phenofit_point(i)$pheno
        }, error = function(e) {
            message(sprintf('%s', e$message))
        })
    }
})
save(t, res, file = "pheno_V4.rda")

## 2. Visualization ------------------------------------------------------------
# growing season dividing
# Ipaper::write_fig({
#     par(cex = 1.1)
#     plot_season(INPUT, brks, ylab = "EVI", margin = 0.2, show.shade = FALSE)
# }, "Figure4_seasons.pdf", 9, 3.8)

