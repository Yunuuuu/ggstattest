#' @export
#' @rdname geom_linerange
geom_comparetest <- function(mapping = NULL, data = NULL,
                             stat = "comparetest", position = "identity",
                             height = ggplot2::rel(0.05),
                             step_increase = 1,
                             tip_length = ggplot2::rel(0.01),
                             ..., nudge_x = NULL, nudge_y = NULL,
                             parse = FALSE, arrow = NULL, arrow_fill = NULL,
                             lineend = "butt", linejoin = "round",
                             na.rm = FALSE,
                             orientation = NA,
                             show.legend = NA,
                             inherit.aes = TRUE) {
    ggplot2::layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomComparetest,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = rlang::list2(
            height = height,
            step_increase = step_increase,
            tip_length = tip_length,
            nudge_x = nudge_x,
            nudge_y = nudge_y,
            parse = parse,
            arrow = arrow,
            arrow_fill = arrow_fill,
            lineend = lineend,
            linejoin = linejoin,
            na.rm = na.rm,
            orientation = orientation,
            ...
        )
    )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomComparetest <- ggplot2::ggproto("GeomComparetest", ggplot2::Geom,
    required_aes = c("xmin|ymin", "xmax|ymax", "y|x"),
    default_aes = ggplot2::aes(
        colour = "black",
        size = 3.88, # text size
        angle = 0,
        hjust = 0.5,
        vjust = 0,
        alpha = NA,
        family = "",
        fontface = 1,
        linewidth = 0.5,
        linetype = 1,
        lineheight = 1.2
    ),
    setup_params = function(self, data, params) {
        params$flipped_aes <- ggplot2::has_flipped_aes(
            data, params,
            main_is_continuous = FALSE
        )
        if (is.null(params$baseline)) {
            params$baseline <- max(data[[
            ggplot2::flipped_names(params$flipped_aes)$y
            ]])
        }
        if (params$flipped_aes) {
            if (is.null(params$nudge_x)) params$nudge_x <- 0.05
            if (is.null(params$nudge_y)) params$nudge_y <- 0
        } else {
            if (is.null(params$nudge_x)) params$nudge_x <- 0
            if (is.null(params$nudge_y)) params$nudge_y <- 0.05
        }
        params
    },
    extra_params = c("na.rm", "orientation"),
    draw_key = function(...) {
        ggplot2::zeroGrob()
    },
    setup_data = function(data, params) {
        # for height and step_increase will chanage the range of x and y
        # so we shouldn't caculate them in `draw_panel`, otherwise, the scale
        # won't fit the plot
        height <- params$height
        step_increase <- params$step_increase

        # for label data, there are two things
        # one for label - c(x, y, label)
        # Another for label segments - c(xmin, xmax, y0, y0)
        data <- ggplot2::flip_data(data, params$flipped_aes)
        if (is.null(data$x)) {
            data$x <- (data$xmin + data$xmax) / 2L
        }
        if (is_rel(height)) {
            height <- params$baseline * unclass(height)
        }
        if (is_rel(step_increase)) {
            step_increase <- params$baseline * unclass(step_increase)
        }
        step_increase <- (seq_len(nrow(data)) - 1L) * step_increase
        data$y <- data$y + height + step_increase
        data$y0 <- data$y # y0 coordinate is for segments
        data <- ggplot2::flip_data(data, params$flipped_aes)
        # nudge label coordinates
        data$x <- data$x + params$nudge_x
        data$y <- data$y + params$nudge_y
        data
    },
    draw_panel = function(data, panel_params, coord, height, step_increase,
                          tip_length, nudge_x = NULL, nudge_y = NULL,
                          parse = FALSE, arrow = NULL, arrow_fill = NULL,
                          lineend = "butt", linejoin = "round", baseline,
                          na.rm, flipped_aes = FALSE) {
        label_data <- ggplot2::flip_data(data, flipped_aes)
        # for tip data, yend should equal to label_data$y
        # yend - y = tip_length
        # c(x, yend - tip_length, x, yend)
        # keep non-position aesthetic
        horizontal_seg <- label_data[
            c(
                "xmin", "xmax", "y0",
                setdiff(names(label_data), c(x_aes, y_aes))
            )
        ]
        horizontal_seg$yend <- horizontal_seg$y0
        horizontal_seg <- rename(
            horizontal_seg,
            c(xmin = "x", xmax = "xend", y0 = "y")
        )
        vertical_seg <- label_data[
            setdiff(names(label_data), c(x_aes, y_aes))
        ]
        if (is.null(vertical_seg$tip)) {
            # for NULL tip data, the tip should run vertically along the `x` and
            # `xend` of horizontal lines
            vertical_seg$x <- c(horizontal_seg$x, horizontal_seg$xend)
            vertical_seg$y <- vertical_seg$yend <- rep(
                horizontal_seg$yend,
                times = 2L
            )
        } else {
            vertical_seg$tip <- lapply(
                vertical_seg$tip, ggplot2::flip_data,
                flipped_aes
            )
            vertical_seg$yend <- horizontal_seg$yend
            vertical_seg <- tidyr::unnest(vertical_seg, all_of("tip"))
        }
        vertical_seg$xend <- vertical_seg$x
        if (is_rel(tip_length)) {
            vertical_seg$y <- vertical_seg$yend - unclass(tip_length) *
                baseline * baseline / vertical_seg$y
        } else {
            vertical_seg$y <- vertical_seg$yend - tip_length
        }
        seg_columns <- intersect(
            colnames(horizontal_seg),
            colnames(vertical_seg)
        )
        seg_data <- rbind(
            horizontal_seg[seg_columns],
            vertical_seg[seg_columns]
        )
        label_data <- ggplot2::flip_data(label_data, flipped_aes)
        seg_data <- ggplot2::flip_data(seg_data, flipped_aes)

        if (flipped_aes) {
            label_data$angle <- label_data$angle - 90
        }
        grid::gList(
            # draw label
            ggplot2::GeomText$draw_panel(
                data = label_data,
                panel_params = panel_params,
                coord = coord,
                parse = parse,
                na.rm = na.rm,
                check_overlap = FALSE
            ),
            # draw segments
            ggplot2::GeomSegment$draw_panel(
                data = seg_data,
                panel_params = panel_params,
                coord = coord,
                arrow = arrow, arrow.fill = arrow_fill,
                lineend = lineend, linejoin = linejoin,
                na.rm = na.rm
            )
        )
    }
)
