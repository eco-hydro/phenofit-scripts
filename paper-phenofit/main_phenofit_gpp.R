divide_season_GPP <- function(df_part, info, sites_multi, sites_single,
    options = list(),
    .options = list(
        iters = 1,
        r_min = 0.0, r_max = 0.2,
        calendarYear = FALSE,
        ypeak_min = 1,
        # rm.closed = FALSE,
        is.continuous = FALSE,
        .check_season = FALSE
    ),
    varname = c("GPP_DT", "GPP_NT")[1])
{
    options = modifyList(.options, options)
    calendarYear = options$calendarYear
    sites <- if (options$calendarYear) sites_single else sites_multi

    df_part$y = df_part[[varname]] # GPP_DT or GPP_NT
    ## 1. get all peaks
    inds = seq_along(sites) %>% set_names(sites)
    lst <- lapply(inds, function(i) {
        runningId(i)
        sitename = sites[i]
        # sitename = "IT-Ro1"
        # sitename = "DE-Kli"
        sp <- info[site == sitename, ]
        d <- df_part[site == sitename, .(site, t = date, y , w = 1 - is.na(y))]
        # tryCatch({
            l <- process_season(d,
                options = options,
                # wFUN = wBisquare,
                # lambda = 100, 
                nptperyear = 365
            )
            l$titlestr <- with(sp[1, ], sprintf(
                "%s, %s, [%.2f, %.2f] lambda = %.1f",
                site, IGBP, lon, lat, l$lambda
            )) # for the subsequent plot
            l
        # }, error = function(e) {
        #     message(sprintf("[e] %d %s: %s", i, sitename, e$message))
        # })
    })
    lst
}

# @param lst variables returned by `divide_season_GPP`
plot_seasons <- function(lst, outfile = "phenofit_multi_seasons_v4.pdf", 
    calendarYear = FALSE, ...)
{
    Cairo::CairoPDF(outfile, 9, 9)
    cex = 0.8
    par(mfrow = c(5, 1), oma = c(3, 1, 1, 1) / 4, cex = cex, cex.main = 1)
    on.exit({ dev.off(); SumatraPDF(outfile) })
    
    grps <- seq_along(lst) # [1:10]
    # grps = 14
    for (i in grps) {
        Ipaper::runningId(i)
        if (!calendarYear) {
            # rtrough_max <- 0.5
            # if (sites[i] == "IT-Ro2") rtrough_max <- 0.4
            lst[[i]]$brks$dt %<>% cheak_season_list()
        }

        l <- lst[[i]]
            title_gpp <- expression("GPP ( gC "*mm^-1*d^-1*" )")
            title = sprintf("(%s) %s", letters[i], l$titlestr)
            plot_season(l$INPUT, l$brks, ylab = title_gpp,
                title = title, show.legend = FALSE)
    }
    lst
}

seq_along2 <- function(x) {
    seq_along(x) %>% set_names(names(x))
}

# # source("test/main_phenofit.R")
# #' get_fitting2
# #'
# #' @param l list with the element of c(`brks`, `fit`, `pheno`)
# #'
# #' @importFrom phenofit cheak_season_list curvefits get_pheno get_fitting get_GOF
# #' plot_phenofit
# #' @export
# get_fitting2 <- function(l){
#     x <- rbind(l$brks$whit[, .(t, y, ziter1, ziter2, meth = "whit")],
#                l$dfit[, .(t, y, ziter1, ziter2, meth)])
#     y = dcast(unique(x)[, -3], t+y~meth, value.var = "ziter2", fun=mean, na.rm = TRUE)
#     y
# }

# #' @export
# main_phenofit <- function(lst_brks, TRS = c(0.1, 0.2, 0.5, 0.6, 0.8),
#                           show = TRUE,
#                           verbose = TRUE,
#                           outfile) {
#     if (show) {
#         Cairo::CairoPDF(outfile, 10, 8)
#         par(mfrow = c(5, 1), oma = c(3, 1, 1, 1) / 4)
#         on.exit({ dev.off(); SumatraPDF(outfile) })
#     }

#     lst_pheno <- foreach(l = lst_brks, i = icount()) %dopar% {
#         runningId(i)
#         first.fig = i == 1
#         fit <- curvefits(l$INPUT, l$brks,
#             methods = c("AG", "Zhang", "Beck", "Elmore"), # ,"klos",, 'Gu'
#             wFUN = wBisquare,
#             nextend = 2, maxExtendMonth = 3, minExtendMonth = 1, minPercValid = 0.2,
#             print = verbose, verbose = FALSE
#         )

#         ## check the curve fitting parameters
#         l_param <- get_param(fit)
#         # print(str(l_param, 1))
#         # print(l_param$AG)
#         d_fit <- get_fitting(fit)
#         ## Get GOF information
#         d_gof <- get_GOF(fit)
#         # fit$stat <- stat
#         # print(head(d_gof))
#         # print(fit$fits$AG$`2002_1`$ws)
#         # print(fit$`2002_1`$fFIT$AG$ws)

#         # theme = coord_cartesian(xlim = c(ymd("2000-04-01"), ymd("2017-07-31")))
#         # write_fig(expression({
#         if (show) {
#             g <- plot_phenofit(d_fit, l$brks, l$titlestr,
#                 title.ylab = "NDVI", "Time",
#                 shape = "point", cex = 0.4,
#                 theme = NULL
#             )
#             if (!first.fig) grid::grid.newpage()
#             grid::grid.draw(g) # plot to check the curve fitting
#         }
#         ## 2.5 Extract phenology
#         l_pheno <- get_pheno(fit, TRS = TRS, IsPlot = F) # %>% map(~melt_list(., "meth"))
#         l_pheno
#     }
#     lst_pheno
# }
