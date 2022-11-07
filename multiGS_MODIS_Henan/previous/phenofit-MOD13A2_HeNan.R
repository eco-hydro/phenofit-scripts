source("main_pkgs.R", encoding = "UTF-8")

infile = "data/MOD13A2_Henan_2015_2020.rda"

if (!file.exists(infile)) {
    indir = path.mnt("I:/Research/phenology/gee_whittaker/data-raw/NorthChina")
    file_vi  = glue("{indir}/MOD13A2_Henan_2016_2020_EVI.tif")
    file_qc  = glue("{indir}/MOD13A2_Henan_2016_2020_SummaryQA.tif")
    file_doy = glue("{indir}/MOD13A2_Henan_2016_2020_DayOfYear.tif")

    # dates = terra::rast(infile) %>% names() %>%
    #     substr(1, 10) %>% as.Date("%Y_%m_%d")
    dates = modis_date("2015-01-01", "2020-12-31", 16)
    l = read_rast(file_vi) %>% rast2mat()
    l_qc = read_rast(file_qc) %>% rast2mat()
    l_doy = read_rast(file_doy) %>% rast2mat()

    I_grid = l$I_grid
    d_coord = l$d_coord[I_grid, ]
    data <- listk(
        VI = l$mat[I_grid, ] / 1e4,
        QC = l_qc$mat[I_grid, ],
        DOY = l_doy$mat[I_grid, ],
        dates = l$dates, qcFUN = qc_summary)
    save(data, d_coord, file = infile)
} else {
    load(infile)
    n = nrow(data$QC)
}

## Parameter setting
# If provided, LCs should have the same length as `data$VI`
LCs <- NULL
nptperyear <- 23
opt_old <- phenofit::get_options()

# Different parameters for different land covers (LCs) refered by TIMESAT
# Name of `list_options` corresponds to `LCs` code
list_options <- list(
    "1" = list(),
    "2" = list(),
    default = list(
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
)
do.call(phenofit::set_options, list_options$default)

# 1. check the performance at random sampled points
# only run in debug mode
if (0) {
    source("main_pkgs.R", encoding = "UTF-8")
    set.seed(1)
    inds = sample(1:nrow(data$VI), 10) %>% sort()
    # id_bads = c(19564, 25769, 68760, 79072, 79073, 83378, 83379, 91274, 99172, 99173,
    #             113144, 119239, 125169, 125810, 132338, 136962, 136963, 138684)
    # inds = c(1049, 1270, 1271, 1273, 1391, 1511, 1512, 1513, 1514, 1642, 1644,
    #                 1654, 2240, 2614, 2645, 2649, 2650, 2651, 2652, 2825)
    # i <- inds[1]
    ofile = "test_v0.3.5_Henan_MODIS.pdf"
    dev_open(ofile, 10, 3, use.cairo_pdf = FALSE)
    # library(proffer)
    do.call(phenofit::set_options, list_options$default)

    # proffer::pprof({
        res = foreach(i = inds, k = icount(10)) %do% {
            runningId(k, 1)
            d = get_input(i, data)
            tryCatch({
                r <- phenofit_point(d, plot = T, period = c(2015, 2020))#$pheno
                if (k < length(inds)) grid.newpage()
            }, error = function(e) {
                message(sprintf('%s', e$message))
            })
        }
    # })
    dev_off()
    Ipaper::pdf_view(ofile)
    # set_options(opt_old)
}

# If need parallel mode, should be `linux` or `wsl` system
InitCluster(10, kill = FALSE)
inds = 1:n
t = system.time({
    res = foreach(i = inds %>% set_names(., .), icount()) %dopar% {
        runningId(i, 1000)
        if (!is.null(LCs)) {
            LC = LCs[i]
            do.call(set_options, list_options[[LC]])
        }
        tryCatch({
            d = get_input(i, data)
            # r <- phenofit_point(d, plot = TRUE, period = c(2015, 2020))$pheno
            phenofit_point(d)$pheno
        }, error = function(e) {
            message(sprintf('%s', e$message))
        })
    }
})

save(res, t, file = "OUTPUT/phenofit_V0.3.5_wHANTS_MODIS_Henan/pheno_Henan_MODIS_V6.1.rda")

## 2. Visualization ------------------------------------------------------------
# growing season dividing
# Ipaper::write_fig({
#     par(cex = 1.1)
#     plot_season(INPUT, brks, ylab = "EVI", margin = 0.2, show.shade = FALSE)
# }, "Figure4_seasons.pdf", 9, 3.8)
