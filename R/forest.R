#'  Forest plot in ggplot2
#'
#' @param data A matrix or data.frame-like data. Excluding the `ylabels` column,
#' the first three column will be regarded as the forest point estimate and
#' confidence interval (lower and higher).
#' @param left_table,right_table Add a table in the left or right, details see
#' [ggtable].
#' @param ylabels A string, in `colnames(data)`, specifies the column used as
#' the y-axis text.
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
#' @param xlabs,xlim,xbreaks,xlabels,x_scale_trans,x_scale_expand Arguments
#' passed as `name`, `limits`, `breaks`, `labels`, `trans`, `expand` in
#' [scale_x_continuous][ggplot2::scale_x_continuous]. 
#' @param y_scale_expand A vector of range expansion
#' constants used to add some padding around the data to ensure that they are
#' placed some distance away from the axes. Will also control the
#' `y_scale_expand` of [ggtable].
#' @param left_table_params,right_table_params Other arguments passed to
#' [ggtable]. 
#' @param widths The relative widths for all plots. See
#' [wrap_plots][patchwork::wrap_plots]. 
#' @inheritParams ggtable
#' @return A [ggplot][ggplot2::ggplot] object or
#' [patchwork][patchwork::wrap_plots]. 
#' @export 
ggforest <- function(
    data, left_table = NULL, right_table = NULL, ylabels = NULL,
    nudge_y = 0.5, y_labels_nudge = 0.5, y_labels_position = "left",
    point_shape = 16L, point_size = 4L, point_color = "darkred",
    null_line_at = 0L, null_linetype = "dashed", null_line_params = list(),
    errorbar_width = 0.15, errorbar_params = list(),
    xlabs = waiver(), xlim = NULL, xbreaks = waiver(),
    xlabels = scales::number_format(accuracy = 0.1),
    x_scale_trans = "log10", x_scale_expand = c(0, 0), y_scale_expand = c(0, 0),
    left_table_params = list(), right_table_params = list(),
    add_band = TRUE, band_col = c("white", "#eff3f2"),
    widths = NULL) {
    if (!(inherits(data, "data.frame") && ncol(data) >= 3L)) {
        cli::cli_abort("{.arg data} must be a {.cls data.frame} with at least 3 columns")
    }
    y_labels_position <- match.arg(y_labels_position, c("left", "right"))
    if (!is.null(ylabels)) {
        if (!rlang::is_string(ylabels, names(data))) {
            cli::cli_abort("Cannot find {.val {ylabels}} in {.arg data}")
        }
        id <- ylabels
        ylabels <- data[[id]]
        data[[id]] <- NULL
    }
    data <- data.frame(
        estimate = data[[1L]], ci_low = data[[2L]], ci_high = data[[3L]]
    )
    data$y <- seq_len(nrow(data))
    if (is.null(ylabels)) {
        ybreaks <- NULL
    } else {
        ybreaks <- seq_len(max(data$y)) - 1L + y_labels_nudge
    }
    p <- ggplot2::ggplot(data, mapping = aes(
        x = .data$estimate,
        y = .data$y - 1L + .env$nudge_y
    ))
    if (isTRUE(add_band)) {
        p <- p + ggadd_band(band_col)
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
            name = xlabs, limits = xlim, breaks = xbreaks, labels = xlabels,
            trans = x_scale_trans, expand = x_scale_expand
        ) +
        ggplot2::scale_y_continuous(
            name = NULL, limits = c(0L, max(data$y)),
            labels = ylabels,
            expand = y_scale_expand,
            # breaks should be lab coord value
            breaks = ybreaks,
            # minor_breaks are the table separator line
            minor_breaks = c(0L, seq_len(max(data$y))),
            position = y_labels_position
        ) +
        ggplot2::theme(
            axis.ticks.y = ggplot2::element_blank(),
            panel.grid.major.y = ggplot2::element_blank()
        )
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
    lst <- list(left = ggleft_table, center = p, right = ggright_table)
    lst <- lst[!vapply(lst, is.null, logical(1L))]
    if (length(lst) > 1L) {
        if (is.null(widths)) {
            widths <- c(ncol(left_table), 30L, ncol(right_table))
        }
        patchwork::wrap_plots(lst, nrow = 1L, widths = widths) &
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
