library(phenofit)
library(Ipaper) # remotes::install_github("rpkgs/Ipaper")
library(grid)
library(ggplot2)
library(ggnewscale)
library(lubridate)

{
    # phenofit parameters
    nptperyear     <- 23
    wFUN           <- wTSM # wBisquare
    wmin           <- 0.2
    methods_fine   <- c("AG", "Zhang", "Beck", "Elmore", "Gu")

    data("CA_NS6")
    d = CA_NS6
}

{
    INPUT <- check_input(d$t, d$y, d$w, QC_flag = d$QC_flag,
        nptperyear = nptperyear,
        maxgap = nptperyear / 4, wmin = 0.2)

    brks <- season_mov(INPUT,
                       options = list(
                           FUN = smooth_wWHIT, wFUN = wFUN,
                           extendMonthMin = 3,
                           wmin = wmin, r_min = 0.1
                       ))

    ## 2.4 Curve fitting
    fit  <- curvefits(INPUT, brks,
        options = list(
            methods = methods_fine, #,"klos",, 'Gu'
            wFUN = wFUN, iters = 2,
            wmin = wmin,
            nextend = 2,
            minExtendMonth = 0.5, maxExtendMonth = 1, minPercValid   = 0
        ),
        constrain = F)

    ## check the curve fitting parameters
    l_param <- get_param(fit)
    dfit   <- get_fitting(fit)

    ## 2.5 Extract phenology
    TRS = c(0.1, 0.2, 0.5)
    l_pheno <- get_pheno(fit, TRS = TRS, IsPlot = FALSE) #%>% map(~melt_list(., "meth"))
    pheno <- l_pheno$doy %>% melt_list("meth")
# }
## 2. Visualization ------------------------------------------------------------
    # growing season dividing
    # Ipaper::write_fig({
    #     par(cex = 1.1)
    #     plot_season(INPUT, brks, ylab = "EVI", margin = 0.2, show.shade = FALSE)
    # }, "Figure4_seasons.pdf", 9, 3.8)

    years = 2010:2017
    layer_extra = list(
        scale_x_date(breaks = make_date(years), labels = years,
                     limits = c(make_date(years[1]), make_date(2017, 12, 31)),
                     expand = c(1, 1)*0.07),
        scale_y_continuous(limits = c(0, 0.6), breaks = seq(0, 0.6, 0.3))
    )
    # fine fitting
    g <- plot_curvefits(dfit, brks, title = NULL, cex = 1.5, ylab = "EVI",
                        layer_extra = layer_extra, angle = 0)
    Ipaper::write_fig(g, "Figure5_curvefitting_unconstrain.jpg", 8, 6, show = TRUE)
}

# extract phenology metrics, only the first 3 year showed at here
write_fig({
    # par(family = "Times")
    par(oma = c(2.5, 5.5, 2, 1)) # begin from b
    l_pheno <- get_pheno(fit[1:5], method = "AG", TRS = TRS,
                         IsPlot = TRUE, show.title = FALSE)

    # add yaxis
    grid.draw(grid.text(x = 0.016, "EVI", rot = 90,
                        gp = gpar(fontfamily = "Arial", cex = 1.2)))
    # add legend
    pos.y <- 0.032
    pushViewport(viewport(y = unit(pos.y, "npc"),
                          width = unit(0.4, "npc"), height = unit(pos.y*2, "npc")))
    lgd_short <- phenofit:::make_legend(NULL, NULL, 1, nmax_points = 4)
    grid.draw(lgd_short)
}, "Figure6_phenology_metrics.jpg", 8, 5, show = TRUE)

{
    # on the top of `Figure7_predata...`
    d_comp = fread("data-raw/dat_Figure7_comparison_with_others-CA-NS6.csv")
    d_comp = merge(d[, .(date, t)], d_comp[, .(date, TIMESAT, phenopix)]) %>%
        merge(dfit[meth == "AG", .(t, phenofit = ziter2)], by = "t") %>%
        melt(c("date", "t"), variable.name = "meth")
    # d_comp$meth %<>% mapvalues(c("phenofit", "TIMESAT", "phenopix"),
                              # c(expression(italic(phenofit)), "TIMESAT", "phenopix"))
    labels = c("good", "marginal", "snow", "cloud")
    theme_set(theme_grey(base_size = 16))
    cols_line = c(phenofit = "red", TIMESAT = "blue", phenopix = "black")
    p <- ggplot(dfit, aes(t, y)) +
        geom_point(aes(color = QC_flag, fill = QC_flag, shape = QC_flag), size = 3) +
        scale_shape_manual(values = qc_shapes[labels], guide = guide_legend(order = 1)) +
        scale_color_manual(values = qc_colors[labels], guide = guide_legend(order = 1)) +
        scale_fill_manual(values = qc_colors[labels], guide = guide_legend(order = 1)) +
        new_scale_color() +
        geom_line(data = d_comp, aes(t, value, color = meth)) +
        # geom_line(data = d_comp[meth == "phenofit"], aes(t, value),
        #           size = 1, show.legend = FALSE, color = "red") +
        scale_color_manual(values = cols_line, guide = guide_legend(order = 2)) +
        labs(x = "Time", y = "EVI") +
        theme(
            axis.title.x = element_text(margin = margin(t = 0, unit='cm')),
            # plot.margin = margin(t = 0, unit='cm'),
            legend.text = element_text(size = 16),
            legend.position = "bottom",
              legend.title  = element_blank(),
              legend.margin = margin(t = -0.3, unit='cm'))
    write_fig(p, "Figure7_comparison_with_others.pdf", 10, 4, show = TRUE)
}
