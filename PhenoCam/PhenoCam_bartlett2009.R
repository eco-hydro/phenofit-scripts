library(phenopix)
library(data.table)
library(magrittr)
library(phenofit)
library(zoo)

# data(bartlett2009)
# df = bartlett2009 %>% data.table()
# df$date %<>% as.Date()
# d = df[, .(gcc = mean(gcc)), .(date)]

data("bartlett2009.filtered")
x = bartlett2009.filtered
t = index(x) %>% {difftime(.,  .[1], units = "day")} %>% as.numeric()
r = curvefit(x, t)

par(mfrow = c(1, 5), mgp = c(3, 0.6, 0), mar = rep(0, 4), yaxt = "n", xaxt = "n")
r_pheno = get_pheno(r, method = "Elmore", IsPlot = TRUE)
