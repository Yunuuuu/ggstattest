#' Graphical display of a textual table
#'
#' Create a table in ggplot2 style
#'
#' @param data A matrix or data.frame-like data.
#' @param cols Columns used to create table. If `NULL`, will select all columns
#' except column specified by `ylabels`.
#' @param ylabels Extract a single column used as the y-axis text, passed to
#' `var` argument in [pull][dplyr::pull].
#' @param mapping List of aesthetic mappings to use for plot, created by [aes].
#' This will be added into the default [ggplot][ggplot2::ggplot] object to
#' change the default aesthetic mappings. The column selected in `cols` will be
#' collected in `.__value__.` with column names in `.__name__.`. This will be
#' helpful if you want to adjust aesthetic by specific column or row.
#' @param nudge_x,nudge_y Horizontal and vertical adjustment \[0, 1\] to nudge
#' text by in a table cell.
#' @param hjust,vjust Horizontal and vertical justification \[0, 1\] of text in
#' each cell of table.
#' @param x_scale_expand,y_scale_expand A vector of range expansion
#' constants used to add some padding around the data to ensure that they are
#' placed some distance away from the axes. See
#' [scale_continuous][ggplot2::scale_x_continuous].
#' @param x_labels_nudge,y_labels_nudge Horizontal and vertical adjustment in
#' \[0, 1\] of table header.
#' @param x_labels_position,y_labels_position The position of the header,
#' left or right for `y_labels_position`, top or bottom for
#' `x_labels_position`.
#' @param x_labels_element,y_labels_element A
#' [element_text][ggplot2::element_text] object specifies the header attributes.
#' @param clip Should drawing be clipped to the extent of the plot panel? A
#' setting of "on" means yes, and a setting of "off" (the default) means no.
#' @param add_band A bool, if `TRUE`, will add band interval in the background.
#' @param band_col Color of background band interval.
#' @return A [ggplot][ggplot2::ggplot] object.
#' @export
ggtable <- function(
    data, cols = NULL, ylabels = NULL, mapping = aes(),
    nudge_x = 0.5, nudge_y = 0.5, hjust = 0.5, vjust = 0.5,
    x_scale_expand = c(0L, 0L), y_scale_expand = c(0L, 0L),
    x_labels_nudge = 0.5, x_labels_position = NULL, x_labels_element = NULL,
    y_labels_nudge = 0.5, y_labels_position = NULL, y_labels_element = NULL,
    clip = "off", add_band = TRUE, band_col = c("white", "#eff3f2")) {
    if (is.matrix(data)) {
        data <- as.data.frame(data,
            make.names = FALSE,
            stringsAsFactors = FALSE
        )
    } else if (!inherits(data, "data.frame")) {
        cli::cli_abort(
            "{.arg data} must be a {.cls data.frame} or {.cls matrix}"
        )
    }
    if (!missing(mapping) && !inherits(mapping, "uneval")) {
        cli::cli_abort(c(
            "{.arg mapping} should be created with {.fn aes}.",
            "x" = "You've supplied a {.cls {class(mapping)[1]}} object"
        ))
    }
    x_labels_position <- match.arg(x_labels_position, c("bottom", "top"))
    y_labels_position <- match.arg(y_labels_position, c("left", "right"))

    ylabels_sel <- rlang::enquo(ylabels)
    if (!rlang::quo_is_null(ylabels_sel)) {
        ylabels <- dplyr::pull(data, var = !!ylabels_sel)
    }
    cols <- rlang::enquo(cols)
    if (rlang::quo_is_null(cols)) {
        if (rlang::quo_is_null(ylabels_sel)) {
            cols <- rlang::expr(everything())
        } else {
            cols <- rlang::expr(-!!ylabels_sel)
        }
    }
    pos <- tidyselect::eval_select(cols, data, allow_empty = FALSE)
    # used as the table column names
    colnms <- names(pos)
    data <- dplyr::mutate(data, dplyr::across(!!cols, .fns = as.character))
    # use complex name to avoid exist columns
    data$.__y__. <- seq_len(nrow(data)) - 1L
    data <- tidyr::pivot_longer(
        data,
        cols = tidyselect::all_of(colnms),
        names_to = ".__name__.",
        values_to = ".__value__."
    )
    data$.__x__. <- as.integer(factor(data$.__name__., levels = colnms)) - 1L
    data$.__hjust__. <- hjust
    data$.__vjust__. <- vjust
    p <- ggplot2::ggplot(
        data = data,
        ggplot2::aes(
            x = .data$.__x__. + .env$nudge_x,
            y = .data$.__y__. + .env$nudge_y,
            hjust = .data$.__hjust__.,
            vjust = .data$.__vjust__.,
            label = .data$.__value__.
        )
    )
    p <- p + mapping
    if (isTRUE(add_band)) {
        p <- p +
            # lowest band
            ggplot2::geom_rect(ggplot2::aes(
                xmin = -Inf, xmax = Inf,
                ymin = -Inf, ymax = 0L,
                fill = "1"
            )) +
            # plot band
            ggplot2::geom_rect(ggplot2::aes(
                xmin = -Inf, xmax = Inf,
                ymin = .data$.__y__., ymax = .data$.__y__. + 1L,
                fill = factor(.data$.__y__. %% 2L)
            )) +
            # highest band
            ggplot2::geom_rect(ggplot2::aes(
                xmin = -Inf, xmax = Inf,
                ymin = max(.data$.__y__.) + 1L,
                ymax = Inf,
                fill = "0"
            )) +
            ggplot2::scale_fill_manual(values = band_col, guide = "none")
    }
    p +
        ggplot2::geom_text() +
        ggplot2::scale_x_continuous(
            name = NULL,
            limits = c(0L, max(data$.__x__.) + 1L),
            # breaks should be header coord value
            breaks = seq_len(max(data$.__x__.) + 1L) - 1L + x_labels_nudge,
            # minor_breaks are the table separator line
            minor_breaks = c(0L, seq_len(max(data$.__x__.) + 1L)),
            labels = colnms, expand = x_scale_expand,
            position = x_labels_position
        ) +
        ggplot2::scale_y_continuous(
            name = NULL,
            limits = c(0L, max(data$.__y__.) + 1L),
            breaks = seq_len(max(data$.__y__.) + 1L) - 1L + y_labels_nudge,
            minor_breaks = c(0L, seq_len(max(data$.__y__.) + 1L)),
            labels = ylabels,
            expand = y_scale_expand,
            position = y_labels_position
        ) +
        ggplot2::coord_cartesian(clip = clip) +
        ggplot2::theme(
            axis.ticks = ggplot2::element_blank(),
            axis.text.x = x_labels_element %||% ggplot2::element_text(),
            axis.text.y = y_labels_element %||% ggplot2::element_text(),
            panel.grid.major = ggplot2::element_blank()
        )
}

utils::globalVariables("everything")
