#' Graphical display of a textual table
#'
#' Create a table in ggplot2 style
#'
#' @param data A matrix or data.frame-like data.
#' @param x,y Axis value in \[0, 1\] of text in each cell of table.
#' @param hjust,vjust Horizontal and vertical justification \[0, 1\] of text in
#' each cell of table.
#' @param row_scale_expand,col_scale_expand A vector of range expansion
#' constants used to add some padding around the data to ensure that they are
#' placed some distance away from the axes. See
#' [scale_continuous][ggplot2::scale_x_continuous]. 
#' @param row_header,col_header Axis value in \[0, 1\] of table header.
#' @param row_header_position,col_header_position The position of the header,
#' left or right for `col_header_position`, top or bottom for
#' `row_header_position`.
#' @param row_header_text,column_header_text A
#' [element_text][ggplot2::element_text] object specifies the header attributes.
#' @param clip Should drawing be clipped to the extent of the plot panel? A
#' setting of "on" means yes, and a setting of "off" (the default) means no.
#' @param add_band A bool, if `TRUE`, will add band interval in the background.
#' @param band_col Color of background band interval.
#' @return A [ggplot][ggplot2::ggplot] object.
#' @export
ggtable <- function(
    data, x = 0.5, y = 0.5, hjust = 0.5, vjust = 0.5,
    row_scale_expand = c(0L, 0L), col_scale_expand = c(0L, 0L),
    row_header = 0.5, row_header_position = NULL, row_header_text = NULL,
    col_header = 0.5, col_header_position = NULL, column_header_text = NULL,
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
    row_header_position <- match.arg(row_header_position, c("bottom", "top"))
    col_header_position <- match.arg(col_header_position, c("left", "right"))
    colnms <- rlang::names2(data)
    names(data) <- colnms
    rownms <- rownames(data)
    rowids <- rownms %||% seq_len(nrow(data))
    data <- dplyr::mutate(
        data, dplyr::across(everything(), .fns = as.character)
    )
    data$..__y__.. <- factor(rowids, levels = rowids)
    data <- tidyr::pivot_longer(data,
        cols = !.data$..__y__..,
        values_ptypes = character()
    )
    data$y <- as.integer(data$..__y__..)
    data$x <- as.integer(factor(data$name, levels = colnms))
    data$hjust <- hjust
    data$vjust <- vjust
    p <- ggplot2::ggplot(
        data = data,
        ggplot2::aes(
            x = .data$x - 1L + .env$x,
            y = .data$y - 1L + .env$y,
            hjust = .data$hjust,
            vjust = .data$vjust,
            label = .data$value
        )
    )
    if (isTRUE(add_band)) {
        p <- p + ggadd_band(band_col)
    }
    p +
        ggplot2::geom_text() +
        ggplot2::scale_x_continuous(
            name = NULL,
            limits = c(0L, max(data$x)),
            # breaks should be header coord value
            breaks = seq_len(max(data$x)) - 1L + row_header,
            # minor_breaks are the table separator line
            minor_breaks = c(0L, seq_len(max(data$x))),
            labels = colnms, expand = row_scale_expand,
            position = row_header_position
        ) +
        ggplot2::scale_y_continuous(
            name = NULL,
            limits = c(0L, max(data$y)),
            breaks = seq_len(max(data$y)) - 1L + col_header,
            minor_breaks = c(0L, seq_len(max(data$y))),
            labels = rownms,
            expand = col_scale_expand,
            position = col_header_position
        ) +
        ggplot2::coord_cartesian(clip = clip) +
        ggplot2::theme(
            axis.ticks = ggplot2::element_blank(),
            axis.text.x = row_header_text %||% ggplot2::element_text(),
            axis.text.y = column_header_text %||% ggplot2::element_text(),
            panel.grid.major = ggplot2::element_blank()
        )
}

ggadd_band <- function(band_col = c("white", "#eff3f2")) {
    list(
        # lowest band
        ggplot2::geom_rect(ggplot2::aes(
            xmin = -Inf, xmax = Inf,
            ymin = -Inf, ymax = 0L,
            fill = "0"
        )),
        # plot band
        ggplot2::geom_rect(ggplot2::aes(
            xmin = -Inf, xmax = Inf,
            ymin = .data$y - 1, ymax = .data$y,
            fill = factor(.data$y %% 2L)
        )),
        # highest band
        ggplot2::geom_rect(ggplot2::aes(
            xmin = -Inf, xmax = Inf,
            ymin = max(.data$y),
            ymax = Inf,
            fill = "1"
        )),
        ggplot2::scale_fill_manual(values = band_col, guide = "none")
    )
}

utils::globalVariables("everything")
