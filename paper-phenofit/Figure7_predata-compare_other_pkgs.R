library(phenofit)
library(phenopix)
library(rTIMESAT) # install_github("eco-hydro/rTIMESAT")

library(tidyverse)
library(data.table)
library(plyr)
library(lubridate)

## 1. TIMESAT ##################################################################
{
    nptperyear <- 23
    options <- list(
        ylu                 = c(0, 9999),     # Valid data range (lower upper)
        qc_1                = c(0, 0, 1),     # Quality range 1 and weight
        qc_2                = c(1, 1, 0.5),   # Quality range 2 and weight
        qc_3                = c(2, 3, 0.2),   # Quality range 3 and weight
        A                   = 0.1,            # Amplitude cutoff value
        output_type         = c(1, 1, 0),     # Output files (1/0 1/0 1/0)
        seasonpar           = 1.0,            # Seasonality parameter (0-1)
        iters               = 2,              # No. of envelope iterations (3/2/1)
        FUN                 = c(2),              # Fitting method (3/2/1)
        half_win            = 7,              # half Window size for Sav-Gol.
        meth_pheno          = 1,              # Season start / end method (4/3/2/1)
        trs                 = c(0.5, 0.5)     # Season start / end values
    )

    data("MOD13A1")
    sitename <- "CA-NS6"
    d <- subset(MOD13A1$dt, date >= as.Date("2010-01-01") &
        date <= as.Date("2017-12-31") & site == sitename)
    r_ts = TSF_main(y = d$EVI/1e4, qc = d$SummaryQA, nptperyear, jobname = sitename, options, cache = FALSE)
    r_ts
}
d <- d[, .(site, date, year = year(date), SummaryQA, y = EVI/1e4)]
d$TIMESAT <- r_ts$fit[, 2]

## 2. PHENOPIX #################################################################
{
    #' Curve fitting by phenopix
    phenopix_year <- function(YEAR, d){
        di <- d[year == YEAR, ]
        x  <- di$y
        tout <- t <- difftime(di$date, date.origin, units="days") %>% as.numeric()

        r <- FitDoubleLogBeck(x, t, tout, return.par = T)
        as.numeric(r$predicted)
    }
}

years <- 2010:2017
date.origin <- ymd('2001-01-01')
l <- llply(years, phenopix_year, d = d, .progress = "text")
d$phenopix <- do.call(c, l)

## 3. show result ##############################################################
write_fig({
    plot(y ~ date, d[year >= 2011], type = "b")
    lines(TIMESAT ~ date, d[year >= 2011], type = "l", col = "red")
    lines(phenopix ~ date, d[year >= 2011], type = "l", col = "blue")
}, "a.pdf", show = TRUE)

# fwrite(d, "data-raw/dat_Figure7_comparison_with_others-CA-NS6.csv")
