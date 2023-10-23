#' Graphical display of a textual table
#'
#' Create a table in ggplot2 style
#'
#' @param data A matrix or data.frame-like data.
#' @param x,y Axis value in \[0, 1\] of text in each cell of table.
#' @param hjust,vjust Horizontal and vertical justification \[0, 1\] of text in
#' each cell of table.
#' @param row_header,col_header Axis value in \[0, 1\] of table header.
#' @param row_header_text,column_header_text A
#' [element_text][ggplot2::element_text] object specifies the header attributes.
#' @param add_band A bool, if `TRUE`, will add band interval in the background.
#' @param band_col Color of background band interval.
#' @return A [ggplot][ggplot2::ggplot] object.
#' @export
ggtable <- function(
    data, x = 0.5, y = 0.5, hjust = 0.5, vjust = 0.5,
    row_header = 0.5, row_header_text = NULL,
    col_header = 0.5, column_header_text = NULL,
    add_band = TRUE, band_col = c("white", "#eff3f2")) {
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
    colnms <- rlang::names2(data)
    rownms <- rownames(data)
    rowids <- rownms %||% seq_len(nrow(data))
    data$..__y__.. <- factor(rowids, levels = rowids)
    data <- tidyr::pivot_longer(data, cols = !.data$..__y__..)
    data$y <- as.integer(data$..__y__..)
    data$x <- as.integer(factor(data$name, levels = colnms))
    data$hjust <- hjust
    data$vjust <- vjust
    p <- ggplot2::ggplot(
        data = data,
        ggplot2::aes(
            x = as.integer(.data$x) - 0.5 + .env$x,
            y = .data$y - 0.5 + .env$y,
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
            limits = range(data$x),
            breaks = sort(unique(data$x)) - 0.5 + row_header,
            labels = colnms
        ) +
        ggplot2::scale_y_continuous(
            name = NULL,
            breaks = sort(unique(data$y)) - 0.5 + col_header,
            labels = rownms,
            expand = rep_len(0L, 2L)
        ) +
        ggplot2::theme(
            axis.ticks = ggplot2::element_blank(),
            axis.text.x = row_header_text %||% ggplot2::element_text(),
            axis.text.y = column_header_text %||% ggplot2::element_text()
        )
}

ggadd_band <- function(band_col = c("white", "#eff3f2")) {
    list(
        # lowest band
        ggplot2::geom_rect(ggplot2::aes(
            xmin = -Inf, xmax = Inf,
            ymin = -Inf,
            ymax = min(.data$y) - 0.5,
            fill = "0"
        )),
        # plot band
        ggplot2::geom_rect(ggplot2::aes(
            xmin = -Inf, xmax = Inf,
            ymin = .data$y - 0.5,
            ymax = .data$y + 0.5,
            fill = factor(.data$y %% 2L)
        )),
        # highest band
        ggplot2::geom_rect(ggplot2::aes(
            xmin = -Inf, xmax = Inf,
            ymin = max(.data$y) + 0.5,
            ymax = Inf,
            fill = "1"
        )),
        ggplot2::scale_fill_manual(values = band_col, guide = "none")
    )
}
