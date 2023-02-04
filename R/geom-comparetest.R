#' Statistical Compare test
#'
#' @param height A list or a numeric vector with length `1` or PANEL number
#' indicating the value where label start. use [rel][ggplot2::rel] to signal
#' values as the fraction of maximal height.
#' @param step_increase A list or a numeric vector  with length `1` or PANEL
#' number indicating the increase for every additional comparison to minimize
#' overlap, use [rel][ggplot2::rel] to signal values as the fraction of maximal
#' height .
#' @param tip_length A list or a numeric vector  with length `1` or PANEL number
#' indicating the length of the beard, use [rel][ggplot2::rel] to signal values
#' as the fraction of the maximal height.
#' @param nudge_x,nudge_y Horizontal and vertical adjustment to nudge labels by.
#' @param parse If `TRUE`, the labels will be parsed into expressions and
#'   displayed as described in `?plotmath`.
#' @param arrow specification for arrow heads, as created by [grid::arrow()].
#' @param arrow_fill fill colour to use for the arrow head (if closed). `NULL`
#'        means use `colour` aesthetic.
#' @param lineend Line end style (round, butt, square).
#' @param linejoin Line join style (round, mitre, bevel).
#' @param na.rm If `FALSE` (the default), removes missing values with a warning.
#'    If `TRUE` silently removes missing values.
#' @param orientation The orientation of the layer. The default (‘NA’)
#' automatically determines the orientation from the aesthetic mapping.
#' In the rare event that this fails it can be given explicitly by setting
#' 'orientation' to either "x" or "y"
#' @param ... Other arguments passed on to [ggplot2::layer()]. These are often
#'   aesthetics, used to set an aesthetic to a fixed value, like `colour =
#'   "red"` or `size = 3`. They may also be parameters to the paired geom/stat.
#' @inheritParams ggplot2::layer
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(ggstattest)
#' ggplot(mpg, aes(class, hwy)) +
#'     geom_boxplot() +
#'     geom_comparetest()
#' ggplot(mpg, aes(class, hwy)) +
#'     geom_boxplot() +
#'     geom_comparetest(
#'         compare_list = list(
#'             c("compact", "pickup"),
#'             c("subcompact", "suv")
#'         )
#'     )
#' ggplot(mpg, aes(class, hwy)) +
#'     geom_boxplot() +
#'     geom_comparetest(
#'         aes(xmin = xmin, xmax = xmax, y = y, label = label),
#'         stat = "identity",
#'         data = data.frame(
#'             y = c(30, 40),
#'             xmin = c(4, 1),
#'             xmax = c(5, 3),
#'             label = c("**", "*")
#'         ),
#'         inherit.aes = FALSE
#'     )
#' }
#' @export
#' @rdname geom_comparetest
geom_comparetest <- function(mapping = NULL, data = NULL,
                             stat = "comparetest", position = "identity",
                             height = rel(0.05),
                             step_increase = rel(0.2),
                             tip_length = rel(0.05),
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
#' @aliases GeomComparetest
#' @export
GeomComparetest <- ggplot2::ggproto("GeomComparetest", ggplot2::Geom,
    required_aes = c("xmin|ymin", "xmax|ymax", "y|x", "label"),
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
    optional_aes = c("tip"),
    setup_params = function(self, data, params) {
        params$flipped_aes <- ggplot2::has_flipped_aes(
            data, params,
            main_is_continuous = FALSE
        )
        if (params$flipped_aes) {
            if (is.null(params$nudge_x)) params$nudge_x <- 0.05
            if (is.null(params$nudge_y)) params$nudge_y <- 0
        } else {
            if (is.null(params$nudge_x)) params$nudge_x <- 0
            if (is.null(params$nudge_y)) params$nudge_y <- 0.05
        }
        panel_number <- length(unique(data$PANEL))
        fail_parms <- character()
        for (i in c("height", "step_increase", "tip_length")) {
            value <- params[[i]]
            i_len <- length(value)
            if (i_len == 1L) {
                params[[i]] <- rep_len(list(value), panel_number)
            } else if (i_len != panel_number) {
                fail_parms <- c(fail_parms, i)
            }
        }
        if (length(fail_parms)) {
            cli::cli_abort("The length of {.arg {fail_parms}} must equal to 1L or the panel numbers ({.val {panel_number}})")
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

        # prepare data
        data <- ggplot2::flip_data(data, params$flipped_aes)
        data <- ggplot2::remove_missing(
            data,
            vars = c("xmin", "xmax", "y", "label"),
            na.rm = params$na.rm,
            name = "geom_comparetest"
        )
        data <- data[order(data$y), ]

        # for label data, there are two things
        # one for label: c(x, y, label)
        # Another for label horizontal segments - c(xmin, xmax, y0, y0)

        data <- data[data$label != "...hide...", , drop = FALSE]
        if (is.null(data$x)) {
            data$x <- (data$xmin + data$xmax) / 2L
        }
        # we should increase the segment y value in each panel individually
        data <- unname(split(data, ~PANEL, drop = TRUE))
        data <- lapply(seq_along(data), function(panel) {
            temp <- data[[panel]]
            baseline <- max(temp$y, na.rm = TRUE)
            height <- height[[panel]]
            step_increase <- step_increase[[panel]]
            if (is_rel(height)) {
                height <- baseline * unclass(height)
            }
            temp$y <- temp$y + height

            # if step_increase is smaller than the difference of y value between
            # current label and lower label. If so, we shouldn't increase the
            # current label y value.
            label_number <- length(temp$y)
            if (label_number >= 2L) {
                if (is_rel(step_increase)) {
                    step_increase <- baseline * unclass(step_increase)
                }
                for (i in 2:label_number) {
                   temp$y[i] <- max(temp$y[i - 1L] + step_increase, temp$y[i])
                }
            }
            temp$baseline <- baseline
            temp
        })
        data <- do.call("rbind", data)
        data$y0 <- data$y
        # y0 coordinate is for segments
        # nudge_x and nudge_y will nudge the coordinates of label but not the
        # coordinates of segments
        data <- ggplot2::flip_data(data, params$flipped_aes)
        # nudge label coordinates
        data$x <- data$x + params$nudge_x
        data$y <- data$y + params$nudge_y
        data
    },
    draw_panel = function(data, panel_params, coord, height, step_increase,
                          tip_length, nudge_x = NULL, nudge_y = NULL,
                          parse = FALSE, arrow = NULL, arrow_fill = NULL,
                          lineend = "butt", linejoin = "round",
                          na.rm, flipped_aes = FALSE) {
        label_data <- ggplot2::flip_data(data, flipped_aes)
        # for horizontal segments data, yend should equal to `horizontal_seg$y0`
        # the same time, we should keep non-position aesthetic
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

        # For vertical segments, `tip` gave the x coordinates of the tip,
        # the tip length is reverse to the y value

        # since vertical segments will not exceed the corresponding horizontal
        # segment, the yend should equal to `y0`, so the y should equal to
        # `yend - tip_length`.  c(x, yend - tip_length, x, yend)
        tip_length <- tip_length[[data$PANEL[[1L]]]]
        # we keep all the non-position aesthetic
        vertical_seg <- label_data[
            c("y0", setdiff(names(label_data), c(x_aes, y_aes)))
        ]
        if (is.null(vertical_seg$tip)) {
            # for NULL tip data, the tip should run vertically along the `x` and
            # `xend` of horizontal lines
            vertical_seg$tip <- lapply(
                seq_len(nrow(horizontal_seg)), function(i) {
                    c(horizontal_seg$x[[i]], horizontal_seg$xend[[i]])
                }
            )
        }
        vertical_seg <- tidyr::unnest(vertical_seg, all_of("tip"))
        vertical_seg <- rename(vertical_seg, c(y0 = "yend", tip = "x"))
        vertical_seg$xend <- vertical_seg$x
        if (is_rel(tip_length)) {
            vertical_seg$y <- vertical_seg$yend - unclass(tip_length) *
                vertical_seg$baseline
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
