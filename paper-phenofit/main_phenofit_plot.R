mytheme <- theme(
    axis.text = element_text(size = 12),
    plot.title = element_text(size = 14, family = "Times"),
    axis.title = element_text(size = 14),
    # legend.position = "bottom",
    legend.key.size = unit(0.6, "cm"),
    legend.text = element_text(size = 12),
    legend.margin = margin(1, 1, 1, 1),
    plot.margin = margin(0, 1, -4, 1) * 2
)

tidy_phenofit <- function(obj){
    meths = c("wWHIT", "AG", "Zhang")
    d_obs = obj$data
    d_season = obj$brks$dt
    colnames(d_season)[1:6] = c("time_start", "time_peak", "time_end", "val_start", "val_peak", "val_end") # rename

    select_zlast <- function(d){
        z = d %>% select(starts_with("ziter")) %>% dplyr::last()
        if ("meth" %in% colnames(d)) {
            d %>% select(t, y, meth) %>% cbind(z) %>%
                .[meth %in% meths]
        } else {
            d %>% select(t, y) %>% cbind(meth = "wWHIT", z)
        }
    }
    dfit <- get_fitting(obj$fit)
    d_fit <- rbind(
        obj$brks$fit %>% select_zlast(),
        dfit %>% select_zlast() )
    d_fit$meth %<>% factor(meths)

    listk(d_obs, d_season, d_fit)
}

expr_GPP = expression("GPP (gC"~m^-2~d^-1*")")

plot_phenofit2 <- function(d_obs, d_season, d_fit, base_size = 12) {
    # colnames(d_season)[1:6] = c("time_start", "time_peak", "time_end", "val_start", "val_peak", "val_end") # rename
    # d_season = brks$dt
    # d_season  = r$pheno[meth == "SG"]
    # d_season %<>% select(-season) %>% dplyr::rename(season = flag)
    date_begin = d_obs$t %>% first() %>% {make_date(year(.), 1, 1)}
    date_end   = d_obs$t %>% last() %>% {make_date(year(.), 12, 31)}
    brks_year = seq(date_begin, date_end, by = "year")

    # d_obs <- listk(t, y, w, QC_flag) %>% as.data.table()
    if (!("QC_flag" %in% colnames(d_obs))) {
        d_obs %<>% mutate(QC_flag = ifelse(w >= 0.5, "good", "cloud"))
    }
    nptperyear = d_obs[, .N, .(year(t))]$N %>% mean()

    qc_name = "QC Flag:"
    I_qc = 1:4
    lwd = 0.3
    l_alpha = 0.4

    p = ggplot(d_obs, aes(t, y))
    if ("SG" %in% d_fit$meth) {
        p = p + geom_vline(data = d_season, aes(xintercept = time_start), color = "blue", linetype = 2, size = lwd, alpha = l_alpha) +
            geom_vline(data = d_season, aes(xintercept = time_end), color = "red", linetype = 2, size = lwd, alpha = l_alpha)
    } else {
        # geom_vline(data = d_season, aes(xintercept = time_start), color = "blue", linetype = 2, size = lwd, alpha = l_alpha) +
        p = p + geom_vline(data = d_season, aes(xintercept = time_end),
            color = "red", linetype = 2, size = lwd, alpha = l_alpha, show.legend = F)
    }
    if (nptperyear >= 100) {
        p = p + geom_line(color = "grey60", size = 0.6)
    } else {
        p = p + geom_point(aes(color = QC_flag, shape = QC_flag, fill = QC_flag),
                           size = 3) +
            scale_color_manual(values = qc_colors[I_qc], drop = F, guide = guide_legend(order = 1)) +
            scale_fill_manual(values = qc_colors[I_qc], drop = F, guide = guide_legend(order = 1))
    }

    p +
        # geom_rect(data = d_ribbon, aes(x = NULL, y = NULL, xmin = xmin, xmax = xmax, group = I, fill = crop),
        #     ymin = -Inf, ymax = Inf, alpha = 0.2, show.legend = F) +
        # geom_rect(data = d_season, aes(x = NULL, y = NULL, xmin = time_start, xmax = time_end, group = flag),
        #     ymin = -Inf, ymax = Inf, alpha = 0.2, show.legend = F, linetype = 1,
        #     fill = alpha("grey", 0.2),
        #     color = alpha("grey", 0.4)) +
        # geom_vline(xintercept = brks_year, color = "yellow3") +
        scale_shape_manual(values = qc_shapes[I_qc], drop = F, guide = guide_legend(order = 1)) +
        labs(color = qc_name, fill = qc_name, shape = qc_name) +
        guides(shape = guide_legend(override.aes = list(size=3), order = 1)) +
        ggnewscale::new_scale_color() +
        # geom_line(color = "black", size = 0.4) +
        geom_line(data = d_fit, aes(t, z, color = meth)) +
        geom_point(data = d_season, aes(time_start, val_start), color = "blue") +
        geom_point(data = d_season, aes(time_end, val_end), color = "blue") +
        geom_point(data = d_season, aes(time_peak, val_peak), color = "red") +
        theme_bw(base_size = base_size, base_family = "Times") +
        theme(
            axis.text = element_text(color = "black"),
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_line(linetype = 3, size = 0.5),
            panel.grid.major.y = element_blank()
        ) +
        scale_color_manual(values = c("black", "blue", "red"), drop = F) +
        scale_x_date(limits = c(date_begin, date_end), expand = c(0, 0))
}

plot_phenofit_V3 <- function(d_obs, d_season, d_fit, base_size = 12) {
    # colnames(d_season)[1:6] = c("time_start", "time_peak", "time_end", "val_start", "val_peak", "val_end") # rename
    # d_season = brks$dt
    # d_season  = r$pheno[meth == "SG"]
    # d_season %<>% select(-season) %>% dplyr::rename(season = flag)
    date_begin <- d_obs$t %>%
        first() %>% { make_date(year(.), 1, 1) }
    date_end <- d_obs$t %>%
        last() %>% { make_date(year(.), 12, 31) }
    brks_year <- seq(date_begin, date_end, by = "year")

    # d_obs <- listk(t, y, w, QC_flag) %>% as.data.table()
    if (!("QC_flag" %in% colnames(d_obs))) {
        d_obs %<>% mutate(QC_flag = ifelse(w >= 0.5, "good", "cloud"))
    }
    nptperyear <- d_obs[, .N, .(year(t))]$N %>% mean()

    qc_name <- "QC Flag:"
    I_qc <- 1:4
    lwd <- 0.3
    l_alpha <- 0.4

    p <- ggplot(d_obs, aes(t, y))
    if ("SG" %in% d_fit$meth) {
        p <- p + geom_vline(data = d_season, aes(xintercept = time_start), color = "blue", linetype = 2, size = lwd, alpha = l_alpha) +
            geom_vline(data = d_season, aes(xintercept = time_end), color = "red", linetype = 2, size = lwd, alpha = l_alpha)
    } else {
        # geom_vline(data = d_season, aes(xintercept = time_start), color = "blue", linetype = 2, size = lwd, alpha = l_alpha) +
        # p <- p + geom_vline(
        #     data = d_season, aes(xintercept = time_end),
        #     color = "red", linetype = 2, size = lwd, alpha = l_alpha, show.legend = F
        # )
    }
    if (nptperyear >= 100) {
        p <- p + geom_line(color = "grey60", size = 0.6)
    } else {
        p <- p + geom_point(aes(color = QC_flag, shape = QC_flag, fill = QC_flag),
                            size = 2.5) +
            scale_color_manual(values = qc_colors[I_qc], drop = F, guide = guide_legend(order = 1)) +
            scale_fill_manual(values = qc_colors[I_qc], drop = F, guide = guide_legend(order = 1))
    }

    qc_name = "";
    p +
        # geom_rect(data = d_ribbon, aes(x = NULL, y = NULL, xmin = xmin, xmax = xmax, group = I, fill = crop),
        #     ymin = -Inf, ymax = Inf, alpha = 0.2, show.legend = F) +
        # geom_rect(data = d_season, aes(x = NULL, y = NULL, xmin = time_start, xmax = time_end, group = flag),
        #     ymin = -Inf, ymax = Inf, alpha = 0.2, show.legend = F, linetype = 1,
        #     fill = alpha("grey", 0.2),
        #     color = alpha("grey", 0.4)) +
        # geom_vline(xintercept = brks_year, color = "yellow3") +
        scale_shape_manual(values = qc_shapes[I_qc], drop = F, guide = guide_legend(order = 1)) +
        labs(color = qc_name, fill = qc_name, shape = qc_name) +
        guides(shape = guide_legend(override.aes = list(size = 3), order = 1)) +
        ggnewscale::new_scale_color() +
        # geom_line(color = "black", size = 0.4) +
        geom_line(data = d_fit, aes(t, z, color = meth)) +
        # geom_point(data = d_season, aes(time_start, val_start), color = "blue") +
        # geom_point(data = d_season, aes(time_end, val_end), color = "blue") +
        # geom_point(data = d_season, aes(time_peak, val_peak), color = "red") +
        # theme_bw(base_size = base_size, base_family = "") +
        theme_grey(base_size = base_size) +
        theme(
            # axis.text = element_text(color = "black"),
            legend.position =  "bottom"
            # panel.grid.minor = element_blank(),
            # panel.grid.major.x = element_line(linetype = 3, size = 0.5),
            # panel.grid.major.y = element_blank()
        ) +
        scale_color_manual(values = c("black", "blue", "red"), drop = F) +
        scale_x_date(limits = c(date_begin, date_end), expand = c(0, 0))
}

plot_rough <- function(brks, show.arrow = FALSE, ...) {
    # brks <- divide_seasons(dat_gpp, 365, is.plot = TRUE,
    #     rFUN = rFUN,
    #     .v_curve = TRUE, iters = 3, ...)$brks
    d_fit = brks$fit %>% dplyr::select(t, y, starts_with("ziter")) %>%
        melt(c("t", "y"), variable.name = "ziter")
    d_season = brks$dt

    p <- ggplot(d, aes(date, GPP)) +
        # geom_point() +
        geom_rect(data = d_ribbon, aes(x = NULL, y = NULL, xmin = xmin, xmax = xmax,
                                       group = I, fill = crop),
                  ymin = -Inf, ymax = Inf, alpha = 0.2, show.legend = F) +
        geom_line(color = "grey60") + # aes(color = cut(GPP_QC, brks_qc), group = 1)
        # geom_point(data = d[GPP_QC < 0.7], aes(shape = qc_lev, color = qc_lev), size = 1) +
        # facet_wrap(~type, nrow = 2) +
        scale_shape_manual(values = c(4, 8, 16)) +
        geom_vline(xintercept = brks_year, color = "yellow3") +
        # geom_vline(xintercept = brks_mid, color = "red", linetype = 2, size = 0.2) +
        anno_cropInfo(show.arrow = show.arrow) +
        new_scale_color() +
        geom_line(data = d_fit,aes(t, value, color = ziter)) +
        scale_color_manual(values = c("blue", "green", "red")) +
        geom_point(data = d_season, aes(beg, y_beg), color = "blue") +
        geom_point(data = d_season, aes(end, y_end), color = "blue") +
        geom_point(data = d_season, aes(peak, y_peak), color = "red") +
        labs(y = expression("GPP (gC"~m^-2~d^-1*")"))
    p
}
