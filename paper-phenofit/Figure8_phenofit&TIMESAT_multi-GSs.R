# source("test/main_pkgs.R")
library(lubridate)
library(phenofit)
library(ggplot2)
library(data.table)
library(Ipaper)   # rpkgs/Ipaper
library(rTIMESAT) # eco-hydro/rTIMESAT
library(patchwork)

# devtools::load_all("../phenofit.R")
devtools::load_all("../rTIMESAT.R")
# devtools::load_all()

simu_VI <- function(SOS = 50, EOS = 100,
                    rate = 0.1, mx = 0.6, mn = 0.1, year = 2010,
                    wmin = 0.2) {
    par <- c(mn, mx, SOS, rate, EOS, rate)

    t <- seq(1, 365, 8)
    w <- rep(1, length(t))

    noise <- rnorm(n = length(t), mean = 0, sd = 0.05)
    I_noise <- noise < 0
    noise[!I_noise] <- 0
    w[I_noise] <- wmin
    y0 <- doubleLog.Beck(par, t)
    y  <- y0 + noise
    data.table(year, doy = t, t = as.Date(sprintf("%d%03d", year, t), "%Y%j"),
               y, y0, w)
}

# two growing season
{
    # d2_1 <- simu_VI(50, 120, 0.05, mn = 0.1, year = 2012)
    # d2_2 <- simu_VI(180, 250, 0.1, mn = 0.2, year = 2012)
    # d2_a = rbind(d2_1[doy < 150, ], d2_2[doy >= 150, ])
    d2_a <- simu_VI(100, 250, 0.1, year = 2010)
    d2_a[doy >= 250, y := y + 0.2]

    t = d2_a$doy
    tout = 1:366
    r <- with(d2_a, FitDL.AG2(y, doy, tout))
    d =  c(list(t = r$tout), r$zs) %>% as.data.table()

    ggplot(d2_a, aes(doy, y)) +
        geom_line(aes(y = y0), color = "black") +
        geom_line(aes(y = y), color = "green") +
        geom_line(data = d, aes(t, iter2), color = "red")
}

{
    set.seed(0)
    d1_a <- simu_VI(150, 250, 0.1, year = 2010)
    d1_b <- simu_VI(150, 250, 0.15, year = 2011)

    # two growing season
    d2_1 <- simu_VI(50, 120, 0.05, year = 2012)
    d2_2 <- simu_VI(180, 250, 0.1, year = 2012)
    d2_a = rbind(d2_1[doy < 150, ], d2_2[doy >= 150, ])

    d2_1 <- simu_VI(50, 120, 0.1, year = 2013)
    d2_2 <- simu_VI(180, 250, 0.05, year = 2013)
    d2_b = rbind(d2_1[doy < 150, ], d2_2[doy >= 150, ])

    # triple growing season
    d3_1 <- simu_VI(25, 75, 0.05, year = 2014)
    d3_2 <- simu_VI(100, 150, 0.1, year = 2014)
    d3_3 <- simu_VI(200, 300, 0.1, year = 2014)
    d3_a = rbind(d3_1[doy < 85, ],
                 d3_2[doy %in% c(85:175), ],
                 d3_3[doy > 175, ])

    d3_1 <- simu_VI(25, 75, 0.05, year = 2015)
    d3_2 <- simu_VI(100, 150, 0.1, year = 2015)
    d3_3 <- simu_VI(200, 300, 0.1, year = 2015)
    d3_b = rbind(d3_1[doy < 85, ],
                 d3_2[doy %in% c(85:175), ],
                 d3_3[doy > 175, ])

    dat = rbind(d1_a, d1_b, d2_a, d2_b, d3_a, d3_b)
    # dat$w %<>% as.factor()
    nptperyear = 46
    ggplot(dat, aes(t, y)) +
        geom_line(aes(y = y0), color = "black") +
        geom_line(aes(y = y), color = "green")
    # geom_point(aes(color = w, shape = w))
}

{
    devtools::load_all("../phenofit.R/")
    r_pheno = process_phenofit(dat,
                        nptperyear = nptperyear,
                        nextend = 0
                        # wFUN = wBisquare_julia,
                        # wFUN = wTSM,
                        )
}

{
    source("scripts/paper-phenofit/main_phenofit_plot.R")
    meths = c("wWHIT", "AG", "Zhang")
    l_kong = tidy_phenofit(r_pheno)
    l_kong$d_fit$meth %<>% factor(meths)
    r_kong <- l_kong[c("d_obs", "d_season", "d_fit")]

    # devtools::load_all("../rTIMESAT.R/")
    r_TS = TIMESAT_process(dat, nptperyear, half_win = 4, p_trs = 0.02,
                        # methods = "SG",
                        seasonpar = 0.0,
                        cache = FALSE)
    d_obs    = dat
    d_season = r_TS$pheno[meth == 'SG'] %>% dplyr::rename(flag = season)
    d_fit    = r_TS$fit
    d_fit$meth %<>% factor(c("SG", "AG", "DL"))
    r_TS <- listk(d_obs, d_season, d_fit)

    r <- r_kong
    d_fit <- rbind(
        r$d_fit[meth %in% c("wWHIT", "AG"), .(meth, t, z)],
        r_TS$d_fit[meth == "AG", ] %>% mutate(meth = "TIMESAT"))
    d_fit$meth %<>% as.character() %>%
        factor(c("AG", "TIMESAT", "wWHIT") %>% rev(),
               c("phenofit", "TIMESAT", "wWHIT") %>% rev())
    r$d_fit <- d_fit
}

{
    source("scripts/paper-phenofit/main_phenofit_plot.R")
    r$base_size = 16
    cols_line = c(phenofit = "red", TIMESAT = "blue", wWHIT = "black")
    p <- do.call(plot_phenofit_V3, r) +
        labs(x = "Time", y = "Simulated VI") +
        theme(
            axis.title.x = element_text(margin = margin(t = 0.3, b = -0.2, unit='cm')),
            # plot.margin = margin(t = 0, unit='cm'),
            legend.title  = element_blank(),
            legend.text = element_text(size = 16),
            legend.margin = margin(t = -0.3, unit='cm')
        ) +
    scale_color_manual(values = cols_line,
                       labels = c(
                           expression(italic(phenofit)), "TIMESAT",
                           expression(italic(wWHIT))
                       ))
    write_fig( p, "Figure8_phenofit&TIMESAT_multi-GSs_v3.pdf", 10, 4)
}

# {
#     # previous version, 2021-07-30
#     p_kong = do.call(plot_phenofit2, r_kong) + mytheme +
#         labs(x = "Time", y = "EVI",
#              color = "Fitting:",
#              title = expression("(a)"~italic(phenofit))) +
#         theme(legend.key.size = unit(0.6, 'cm'))

#     p_TS = do.call(plot_phenofit2, r_TS) +
#         mytheme +
#         labs(x = "Time", y = "EVI", color = "Fitting:",
#              title = expression("(b) TIMESAT"))
#     # write_fig(p_TS, "Figure3b_TIMESAT_multi-GSs.pdf", 10, 3)
#     write_fig( p_kong / p_TS, "Figure8_phenofit&TIMESAT_multi-GSs_v2.pdf", 10, 6.5)
# }
