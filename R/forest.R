#'  Forest plot in ggplot2
#'
#' @param data A matrix or data.frame-like data. Excluding the `ylabels` column,
#' the first three column will be regarded as the forest point estimate and
#' confidence interval (lower and higher).
#' @param left_table,right_table Add a table in the left or right, details see
#' [ggtable].
#' @param nudge_y Vertical adjustment to nudge all errorbar or table text by.
#' @param y_labels_nudge Vertical adjustment in \[0, 1\] of y-axis labels.
#' @param y_labels_position The position of the y-axis labels, left or right for
#' `y_labels_position`.
#' @param point_shape,point_size,point_color The shape, size, color of the
#' point estimate.
#' @param null_line_at,null_linetype Add a vertical line in the
#' forest plot to indiate null hypothesis.
#' @param null_line_params Other argument passed to
#' [geom_vline][ggplot2::geom_vline].
#' @param errorbar_width Width of errorbar. Passed to
#' [geom_errorbar][ggplot2::geom_errorbar].
#' @param errorbar_params Other arguments passed to
#' [geom_errorbar][ggplot2::geom_errorbar].
#' @param xlab,xlim,xbreaks,xlabels,x_scale_trans,x_scale_expand Arguments
#' passed as `name`, `limits`, `breaks`, `labels`, `trans`, `expand` in
#' [scale_x_continuous][ggplot2::scale_x_continuous]. Just a note: use xlab =
#' `expression(Protective %<->% Hazardous)` to add directed arrow in x labs. But
#' in this function, we just assembly an arrow vertically.
#' @param y_scale_expand A vector of range expansion
#' constants used to add some padding around the data to ensure that they are
#' placed some distance away from the axes. Will also control the
#' `y_scale_expand` of [ggtable].
#' @param left_table_params,right_table_params Other arguments passed to
#' [ggtable].
#' @param forest_width The relative width of the forest plot, only used when
#' `widths` is `NULL`. The relative width of talbe will be calculated as the
#' column number.
#' @param add_arrow A bool, if `TRUE`, will add arrow below x-axis.
#' @param arrow_labels String Vector, length 2. Labels for the arrows. Set
#' arrows to TRUE or this will have no effect.
#' @param arrow_weights A numeric of length 2, reduce this if you want to
#' shorten the arrow, and increase this if you want to lengthen the arrow.
#' @param grid_arrow An [arrow][grid::arrow] object created.
#' @param widths,heights The relative widths and heights of each column and row
#' in the grid. See [wrap_plots][patchwork::wrap_plots].
#' @inheritParams ggtable
#' @return A [ggplot][ggplot2::ggplot] object or
#' [patchwork][patchwork::wrap_plots].
#' @export
ggforest <- function(
    data, left_table = NULL, right_table = NULL, ylabels = NULL,
    nudge_y = 0.5, y_labels_nudge = 0.5, y_labels_position = "left",
    point_shape = 16L, point_size = 4L, point_color = "darkred",
    null_line_at = 1L, null_linetype = "dashed", null_line_params = list(),
    errorbar_width = 0.15, errorbar_params = list(),
    xlab = NULL, xlim = NULL, xbreaks = waiver(),
    xlabels = scales::number_format(accuracy = 0.1),
    x_scale_trans = "log10", x_scale_expand = c(0, 0), y_scale_expand = c(0, 0),
    left_table_params = list(), right_table_params = list(),
    add_arrow = TRUE,
    arrow_labels = c("Lower", "Higher"),
    arrow_weights = 35L, # increase will have a longer arrow
    grid_arrow = grid::arrow(
        angle = 15, type = "closed",
        length = grid::unit(0.1, "in")
    ),
    add_band = TRUE, band_col = c("white", "#eff3f2"),
    forest_width = 5L, widths = NULL, heights = c(30L, 1L)) {
    if (!(inherits(data, "data.frame") && ncol(data) >= 3L)) {
        cli::cli_abort("{.arg data} must be a {.cls data.frame} with at least 3 columns")
    }
    y_labels_position <- match.arg(y_labels_position, c("left", "right"))
    ylabels_sel <- rlang::enquo(ylabels)
    if (!rlang::quo_is_null(ylabels_sel)) {
        ylabels_idx <- tidyselect::vars_pull(data, var = !!ylabels_sel)
        ylabels <- data[[ylabels_idx]]
        data[[ylabels_idx]] <- NULL
    }
    assert_bool(add_arrow)
    assert_bool(add_band)
    if (add_arrow && !rlang::is_character(arrow_labels, n = 2L)) {
        cli::cli_abort("{.arg arrow_labels} must be a character of length 2")
    }
    if (add_band && !rlang::is_character(band_col, n = 2L)) {
        cli::cli_abort("{.arg band_col} must be a color character of length 2")
    }
    data <- data.frame(
        estimate = data[[1L]], ci_low = data[[2L]], ci_high = data[[3L]]
    )
    data$y <- seq_len(nrow(data)) - 1L
    if (rlang::quo_is_null(ylabels_sel)) {
        ybreaks <- NULL
    } else {
        ybreaks <- data$y + y_labels_nudge
    }
    p <- ggplot2::ggplot(data, mapping = aes(
        x = .data$estimate,
        y = .data$y - 1L + .env$nudge_y
    ))
    if (isTRUE(add_band)) {
        if (startsWith(x_scale_trans, "log")) {
            band_min <- 0L
        } else {
            band_min <- -Inf
        }
        p <- p +
            # lowest band
            ggplot2::geom_rect(ggplot2::aes(
                xmin = .env$band_min, xmax = Inf,
                ymin = -Inf, ymax = 0L,
                fill = "1"
            )) +
            # plot band
            ggplot2::geom_rect(ggplot2::aes(
                xmin = .env$band_min, xmax = Inf,
                ymin = .data$y, ymax = .data$y + 1L,
                fill = factor(.data$y %% 2L)
            )) +
            # highest band
            ggplot2::geom_rect(ggplot2::aes(
                xmin = .env$band_min, xmax = Inf,
                ymin = max(.data$y) + 1L,
                ymax = Inf,
                fill = "0"
            )) +
            ggplot2::scale_fill_manual(values = band_col, guide = "none")
    }

    p <- p +
        ggplot2::geom_point(
            shape = point_shape,
            size = point_size, color = point_color
        ) +
        rlang::inject(ggplot2::geom_errorbar(
            mapping = aes(
                xmin = .data$ci_low,
                xmax = .data$ci_high
            ),
            width = errorbar_width,
            !!!errorbar_params
        )) +
        rlang::inject(ggplot2::geom_vline(
            xintercept = null_line_at,
            linetype = null_linetype,
            !!!null_line_params
        )) +
        ggplot2::scale_x_continuous(
            name = xlab, limits = xlim, breaks = xbreaks, labels = xlabels,
            trans = x_scale_trans, expand = x_scale_expand
        ) +
        ggplot2::scale_y_continuous(
            name = NULL, limits = c(0L, max(data$y) + 1L),
            expand = y_scale_expand,
            # breaks should be lab coord value
            breaks = ybreaks,
            labels = ylabels,
            # minor_breaks are the table separator line
            minor_breaks = c(0L, seq_len(max(data$y) + 1L)),
            position = y_labels_position
        ) +
        ggplot2::coord_cartesian(clip = "off") +
        ggplot2::theme(
            axis.ticks.y = ggplot2::element_blank(),
            panel.grid.major.y = ggplot2::element_blank()
        )
    if (isTRUE(add_arrow)) {
        xdata <- c(data[[1L]], data[[2L]], data[[3L]])
        xranges <- range(xdata, na.rm = TRUE, finite = TRUE)
        # plot arrow
        # this df has the text labels
        small_amount <- (max(xranges) - min(xranges)) /
            rep_len(arrow_weights, 2L)
        arrow_text_df <- data.frame(
            text = arrow_labels,
            y = c(0L, 0L),
            hjust = c(0.5, 0.5)
        )
        arrow_df <- data.frame(
            xstart = c(
                null_line_at - small_amount[1L],
                null_line_at + small_amount[2L]
            ),
            xend = c(
                min(xranges) + small_amount[1L],
                max(xranges) - small_amount[2L]
            ),
            y = c(1L, 1L)
        )
        good_idx <- c(
            arrow_df$xstart[1L] > arrow_df$xend[1L],
            arrow_df$xstart[2L] < arrow_df$xend[2L]
        )
        arrow_text_df$x <- (arrow_df$xstart + arrow_df$xend) / 2L
        arrow_df <- arrow_df[good_idx, , drop = FALSE]
        arrow_text_df <- arrow_text_df[good_idx, , drop = FALSE]
        # if (null_line_at - small_amount <= xranges[1L]) {
        #     p <- p +
        #         ggplot2::annotate("segment",
        #             x = null_line_at - small_amount,
        #             xend = -Inf, y = 0L, yend = 0L,
        #             arrow = grid::arrow()
        #         )
        # }
        # if (null_line_at + small_amount >= xranges[2L]) {
        #     p <- p +
        #         ggplot2::annotate("segment",
        #             x = null_line_at + small_amount,
        #             xend = Inf, y = 0L, yend = 0L,
        #             arrow = grid::arrow()
        #         )
        # }
        if (nrow(arrow_text_df)) {
            # create the arrow/label ggplot object
            arrows_plot <- ggplot2::ggplot() +
                ggplot2::geom_segment(
                    data = arrow_df,
                    aes(
                        x = .data$xstart, xend = .data$xend,
                        y = .data$y, yend = .data$y
                    ),
                    arrow = grid_arrow
                ) +
                ggplot2::geom_text(
                    data = arrow_text_df,
                    aes(
                        x = .data$x, y = .data$y,
                        label = .data$text, hjust = .data$hjust
                    )
                ) +
                ggplot2::expand_limits(x = xdata) +
                ggplot2::scale_x_continuous(
                    name = NULL,
                    limits = xlim, breaks = NULL, labels = NULL,
                    trans = x_scale_trans, expand = x_scale_expand
                ) +
                ggplot2::scale_y_continuous(
                    name = NULL, expand = c(0L, 0L),
                    limits = c(-0.5, 1.5), breaks = NULL, labels = NULL
                )
        } else {
            arrows_plot <- NULL
        }
    } else {
        arrows_plot <- NULL
    }

    ggleft_table <- ggright_table <- NULL
    if (!is.null(left_table)) {
        ggleft_table <- rlang::inject(ggtable(
            data = left_table, add_band = add_band, band_col = band_col,
            nudge_y = nudge_y, y_scale_expand = y_scale_expand,
            !!!left_table_params
        ))
    }
    if (!is.null(right_table)) {
        ggright_table <- rlang::inject(ggtable(
            data = right_table, add_band = add_band, band_col = band_col,
            nudge_y = nudge_y, y_scale_expand = y_scale_expand,
            !!!right_table_params
        ))
    }
    lst <- list(
        left = ggleft_table, center = p,
        right = ggright_table, arrow = arrows_plot
    )
    idx <- !vapply(lst, is.null, logical(1L))
    lst <- lst[idx]
    if (length(lst) > 1L) {
        if (idx[1L]) {
            design <- list(
                patchwork::area(1, 1),
                patchwork::area(1, 2),
                patchwork::area(1, 3),
                patchwork::area(2, 2)
            )
        } else {
            design <- list(
                NULL,
                patchwork::area(1, 1),
                patchwork::area(1, 2),
                patchwork::area(2, 1)
            )
        }
        design <- do.call(c, design[idx])
        if (is.null(widths)) {
            widths <- c(ncol(left_table), forest_width, ncol(right_table))
        }
        patchwork::wrap_plots(lst,
            widths = widths, heights = heights, design = design
        ) &
            ggplot2::theme(
                plot.background = ggplot2::element_blank(),
                panel.background = ggplot2::element_blank(),
                panel.border = ggplot2::element_blank(),
                plot.margin = ggplot2::margin()
            )
    } else {
        p
    }
}
