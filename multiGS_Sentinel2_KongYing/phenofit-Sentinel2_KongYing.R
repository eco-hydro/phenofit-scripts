# library(JuliaCall)
# julia_setup()
source("scripts/main_pkgs.R")

file_vi = path.mnt("I:/Research/phenology/gee_whittaker/data-raw/NorthChina/sentinel2_KongYing_2019-2021_EVI2.tif")
file_qc = path.mnt("I:/Research/phenology/gee_whittaker/data-raw/NorthChina/sentinel2_KongYing_2019-2021_SCL.tif")

l = read_rast(file_vi) %>% rast2mat()
l_qc = read_rast(file_qc) %>% rast2mat()
dates = l$dates
data <- listk(VI = l$mat, QC = l_qc$mat, dates, qcFUN = qc_sentinel2)

if (0) {
    # explore raw file
    b = read_rast(file_vi)
    r = b[[6]]
    file_raw = "sentinel2_kongying_raw.pdf"
    write_fig({
        i = 1; terra::plot(b[[seq((i-1)*16+1, i*16)]])
        i = 2; terra::plot(b[[seq((i-1)*16+1, i*16)]])
        i = 3; terra::plot(b[[seq((i-1)*16+1, i*16)]])
        i = 4; terra::plot(b[[seq((i-1)*16+1, nlyr(b))]])
    }, file_raw, 10, 8, use.cairo_pdf = FALSE)
    # pdf_view(file_raw)
}

# info = data.table(date = dates, year = year(dates))
# info[, .N, year] # -> nptperyear ~= 20

# expr <- quote(
{
    # dev_open("sentinel2_test.pdf", 10, 10, use.cairo_pdf = FALSE)
    source("scripts/main_pkgs.R")

    opt_old <- get_options()
    opt <- list(
        nptperyear = 20,
        wFUN = wTSM, wmin = 0.2,
        verbose = FALSE,
        season = list(
            # rFUN = "smooth_wWHIT", smooth_wHANTS, smooth_wSG, lambda = 0.5,
            rFUN = "smooth_wHANTS", nf = 6, lambda = 10,
            maxExtendMonth = 12, r_min = 0.0, r_max = 0.1),
        # fine fitting parameters
        fitting = list(nextend = 1, minExtendMonth = 0, maxExtendMonth = 0.5,
                       methods = c("AG", "Zhang", "Beck", "Elmore", "Gu")[3], #,"klos",, 'Gu'
                       iters = 1,
                       minPercValid = 0)
    )

    set.seed(1)
    inds = sample(1:nrow(data$VI), 100) %>% sort()
    inds = 1:nrow(data$VI)
    # inds = c(858, 878, 1017, 1129, 1222, 1328, 3101) # bads, 878 is the last error

    par(mfrow = c(4, 1), mar = c(0, 3, 1.6, 1), mgp = c(1.2, 0.6, 0))
    set_options(options = opt, season = list(rtrough_max = 0.7))

    InitCluster(8, kill = FALSE)
    res = foreach(i = inds %>% set_names(.,.), icount()) %dopar% {
        runningId(i, 1e2)
        d = get_input(i, data)
        # d$t = dates
        input <- check_input(dates, d$VI, d$w, QC_flag = d$QC_flag,
                             nptperyear = get_options("nptperyear"),
                             maxgap = get_options("nptperyear") / 4, wmin = 0.2)
        tryCatch({
            # r = phenofit_point(d, dates)$pheno
            brks <- season_mov(input, years.run = NULL)
            rfit <- brks2rfit(brks) # used rough fitting directly
            r <- get_pheno(rfit)$doy
        }, error = function(e) {
            message(sprintf('%s', e$message))
        })

        # plot_season(input, brks, title = i, show.legend = F)
    }
    # dev_off()
    save(res, file = "sentinel2_kong_V2.rda")
}
# )

# write_fig(expr, "sentinel2_test.pdf", 10, 10, use.cairo_pdf = FALSE)

# file_test = "sentinel2_test.pdf"
# write_fig({
#     foreach(i = inds, icount(20)) %do% {
#         runningId(i)
#         r = phenofit_point(d, dates, plot = TRUE, title = i,
#                            show.legend = FALSE, period = c(2018, 2021))
#         r$brks$dt
#     }
# }, file_test, 10, 2, use.cairo_pdf = FALSE, show = FALSE)
# pdf_view(file_test)

# InitCluster(10, kill = FALSE)
# ind = 1:n
# # ind = 1:100
# t = system.time({
#     res = foreach(i = ind %>% set_names(., .), icount()) %dopar% {
#         runningId(i, 1000)
#         tryCatch({
#             phenofit_point(i)$pheno
#         }, error = function(e) {
#             message(sprintf('%s', e$message))
#         })
#     }
# })
# save(t, res, file = "pheno_V4.rda")

## 2. Visualization ------------------------------------------------------------
# growing season dividing
# Ipaper::write_fig({
#     par(cex = 1.1)
#     plot_season(INPUT, brks, ylab = "EVI", margin = 0.2, show.shade = FALSE)
# }, "Figure4_seasons.pdf", 9, 3.8)

