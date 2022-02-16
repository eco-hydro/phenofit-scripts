# source("test/main_pkgs.R")
library(lubridate)
library(phenofit)
library(ggplot2)
library(data.table)
library(Ipaper)   # rpkgs/Ipaper
library(rTIMESAT) # eco-hydro/rTIMESAT
library(patchwork)

# devtools::load_all("../phenofit.R")
# devtools::load_all("../rTIMESAT.R")
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
    # simulate data for AG2
    d2_a[doy >= 250, y := y + 0.2]

    t = d2_a$doy
    tout = 1:366
    r <- with(d2_a, FitDL.AG2(y, doy, tout, iters = 3))
    d =  c(list(t = r$tout), r$zs) %>% as.data.table()

    dat = melt(d, "t", variable.name = "iter")
    ggplot(dat, aes(t, value, color = iter)) +
        geom_line() +
        geom_line(data = d2_a, aes(doy, y0), color = "black") +
        geom_line(data = d2_a, aes(doy, y), color = "green")
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
