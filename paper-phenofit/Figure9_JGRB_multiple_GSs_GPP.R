## multiple seasons
# Processed by the following scripts
# https://github.com/eco-hydro/PhenoAsync/blob/multiGSs/test/data-prepare/s1_dat0_divide_growing_season.R
# give me a email if you need this file
# source("test/data-prepare/s1_dat0_divide_growing_season.R")
library(glue)
library(Ipaper)
library(lubridate)
devtools::load_all(".")

infile <- "../PhenoAsync/INPUT/df_north_109st.rda"
load(infile)
df_part$date %<>% as_date()
calendarYear <- TRUE

## 1. phenofit -----------------------------------------------------------------
if (1) {
    # devtools::load_all("")
    source("scripts/paper-phenofit/main_phenofit_gpp.R")
    set_options(debug = TRUE)

    # InitCluster(12)
    sites2 <- c(sites_multi, sites_rm) %>% set_names(., .)
    sites_single <- sites_single %>% set_names(., .)
    info <- info_full

    nptperyear <- 365
    varname <- "GPP_NT"
    version <- glue("({varname})v0.3.1") # test version

    sites_test = c("CH-Oe2", "CZ-BK2", "IT-BCi", "IT-Ro1", "IT-Ro2", "IT-Noe",
                   "US-ARM", "US-SRG", "US-SRM")
    ## 1. divide growing seasons
    lst_brks.multi <- divide_season_GPP(df_part, info,
        sites_multi = sites_test,
        # sites_multi = "CZ-BK2",
        # sites_multi = "IT-Noe",
        sites_single,
        options = list(
            # rm.closed = TRUE,
            r_max = 0.1,
            r_min = 0.05,
            calendarYear = FALSE, verbose = FALSE),
        varname = varname
    )
    outfile = glue("Figure9_phenofit_FLUXNET_GPP_multi_{version}.pdf")
    lst2 = plot_seasons(lst_brks.multi, outfile)
    # opt <- phenofit:::.options$season
}

## 2. TIMESAT ------------------------------------------------------------------
# TIMESAT not work at some site, hence not used in the last figure
df = df_part[site %in% sites_test, .(site, t = date, y = GPP_NT, w = 1 - is.na(GPP_NT))]

{
    devtools::load_all("../rTIMESAT.R/")
    dat = df[site == sites_test[1]]
    r_TS = TIMESAT_process(dat, nptperyear, half_win = 4, p_trs = 0.02,
                           methods = c("AG"),
                           seasonpar = 0.0,
                           cache = FALSE)
    # d_obs    = dat
    # d_season = r_TS$pheno[meth == 'SG'] %>% dplyr::rename(flag = season)
    # d_fit    = r_TS$fit
    # d_fit$meth %<>% factor(c("SG", "AG", "DL"))
    # r_TS <- listk(d_obs, d_season, d_fit)
}


