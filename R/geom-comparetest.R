#' Statistical Compare test
#'
#' @param height A list or a numeric vector indicating the value where label
#' start, this will be recycled to fit the number of panel. Using
#' [rel][ggplot2::rel] to signal values as the fraction of maximal height of the
#' panel. Will elevate both label and segment. Default: `rel(0.05)`
#' @param step_increase A list or a numeric vector indicating the increase for
#' every additional comparison to minimize overlap, this will be recycled to fit
#' the number of panel. Using [rel][ggplot2::rel] to signal values as the
#' fraction of maximal height of the panel. Default: `rel(0.1)`
#' @param tip_length A list or a numeric vector indicating the length of the
#' beard which is drawn down the comparison group, this will be recycled to fit
#' the number of panel. Using [rel][ggplot2::rel] to signal values as the
#' fraction of the difference between the horizontal segment and the maximal
#' value of current group.  Default: `rel(0.01)`
#' @param nudge_x,nudge_y Horizontal and vertical adjustment to nudge labels by.
#'   Useful for offsetting text from segments, particularly on discrete scales.
#'   A list or a numeric vector, this will be recycled to fit the number of
#'   panel. Using [rel][ggplot2::rel] to signal values as the fraction of
#'   maximal height of the panel. Will just nudge the label but not the segment.
#'   This different from [`geom_text`][ggplot2::geom_text].
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
#' @section Aesthetics:
#' `geom_segment()` understands the following aesthetics (required aesthetics
#' are in bold):
#' \describe{
#'   \item{`xmin` or `ymin`}{The left (or lower) side of horizontal (or
#'   vertical) segments underneath label}
#'   \item{`xmax` or `ymax`}{The right (or upper) side of horizontal (or
#'   vertical) segments underneath label}
#'   \item{`y` or `x`}{The y (or x) coordinates for labels, usually equal
#'   to the max y-axis (x-axis) value span from xmin (ymin) to xmax (ymax)}
#'   \item{`label`}{The statistical test results}
#'   \item{`x` or `y`}{The x (or y) coordinates for labels, usually equal to
#'   (xmin + xmax) / 2 or (ymin + ymax) / 2}
#'   \item{`tip`}{A list of data.frame gives the coordinates of tip where column
#'   x (or y) corresponds to the scaled discrete variable and column x0 (or y0)
#'   is the actual value of the discrete variable (one of x or x0 (y or y0) is
#'   required) and column y (or x) (required) corresponds to the maximal values
#'   of current comparison group. the tip length is reverse to the y value}
#' }
# Learn more about setting these aesthetics in vignette("ggplot2-specs").
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
                             height = NULL, step_increase = NULL,
                             tip_length = NULL,
                             nudge_x = 0, nudge_y = 0,
                             ...,
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
        vjust = 0.5,
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
        if (is.null(params$height)) params$height <- rel(0.05)
        if (is.null(params$step_increase)) params$step_increase <- rel(0.1)
        if (is.null(params$tip_length)) params$tip_length <- rel(0.01)
        if (is.null(params$nudge_x)) params$nudge_x <- 0L
        if (is.null(params$nudge_y)) params$nudge_y <- 0L
        # make sure each panel have a arguments
        panel_number <- max(unique_n(data$PANEL), data$PANEL)
        for (i in c("height", "step_increase", "tip_length", "nudge_x", "nudge_y")) {
            value <- params[[i]]
            if (length(value) == 1L) value <- list(params[[i]])
            params[[i]] <- rep_len(value, panel_number)
        }
        params
    },
    extra_params = c("na.rm", "orientation", "height", "step_increase", "nudge_x", "nudge_y"),
    draw_key = function(...) {
        ggplot2::zeroGrob()
    },
    setup_data = function(self, data, params) {
        # for height and step_increase will chanage the range of x and y
        # so we shouldn't caculate them in `draw_panel`, otherwise, the scale
        # won't fit the plot
        height <- params$height
        step_increase <- params$step_increase
        nudge_x <- params$nudge_x
        nudge_y <- params$nudge_y

        # prepare data
        data <- ggplot2::flip_data(data, params$flipped_aes)
        if (is.null(data$x)) {
            data$x <- (data$xmin + data$xmax) / 2L
        }
        data <- ggplot2::remove_missing(
            data,
            vars = c("xmin", "xmax", "x", "y", "label"),
            na.rm = params$na.rm,
            name = "geom_comparetest"
        )
        data <- data[data$label != "...hide...", , drop = FALSE]
        data <- data[order(data$y), ]

        # for label data, there are two things
        # one for label: c(x, y, label)
        # Another for label horizontal segments - c(xmin, xmax, yend, yend)
        # we should increase the segment y value in each panel individually
        data <- split(data, ~PANEL, drop = TRUE)
        data <- lapply(names(data), function(panel) {
            out <- data[[panel]]
            panel_idx <- as.integer(panel)
            baseline <- max(out$y, na.rm = TRUE)
            height <- height[[panel_idx]]
            step_increase <- step_increase[[panel_idx]]
            nudge_x <- nudge_x[[panel_idx]]
            nudge_y <- nudge_y[[panel_idx]]

            if (is_rel(height)) {
                height <- baseline * unclass(height)
            }
            out$y <- out$y + height

            # if step_increase is smaller than the difference of y value between
            # current label and lower label. If so, we shouldn't increase the
            # current label y value.
            label_number <- length(out$y)
            if (label_number >= 2L) {
                if (is_rel(step_increase)) {
                    step_increase <- baseline * unclass(step_increase)
                }
                for (i in 2:label_number) {
                    out$y[i] <- max(
                        out$y[i - 1L] + step_increase, out$y[i],
                        na.rm = TRUE
                    )
                }
            }
            # yend coordinate is for segments
            out$yend <- out$y

            # nudge_x and nudge_y will nudge the coordinates of label but not
            # the coordinates of segments
            out <- ggplot2::flip_data(out, params$flipped_aes)
            # nudge label coordinates
            if (is_rel(nudge_x)) {
                nudge_x <- baseline * unclass(nudge_x)
            }
            out$x <- out$x + nudge_x
            if (is_rel(nudge_y)) {
                nudge_y <- baseline * unclass(nudge_y)
            }
            out$y <- out$y + nudge_y
            out
        })
        data <- do.call("rbind", data)
        data
    },
    draw_panel = function(self, data, panel_params, coord,
                          tip_length = NULL,
                          parse = FALSE, arrow = NULL, arrow_fill = NULL,
                          lineend = "butt", linejoin = "round",
                          na.rm, flipped_aes = FALSE) {
        data <- ggplot2::flip_data(data, flipped_aes)

        non_pos_aes <- setdiff(names(data), c(x_aes, y_aes))
        label_data <- data[
            c("x", "y", setdiff(non_pos_aes, "tip"))
        ]

        # horizontal segments data, we should keep non-position aesthetic
        horizontal_seg <- data[c("xmin", "xmax", "yend", non_pos_aes)]
        horizontal_seg$y <- horizontal_seg$yend
        horizontal_seg <- rename(
            horizontal_seg, c(xmin = "x", xmax = "xend")
        )

        # since vertical segments will not exceed the corresponding horizontal
        # segment, so the y should equal to `yend - tip_length`.  c(x, yend -
        # tip_length, x, yend)
        tip_length <- tip_length[[data$PANEL[[1L]]]]
        # we keep all the non-position aesthetic
        vertical_seg <- data[c("yend", non_pos_aes)]

        # For vertical segments, `tip` gave the coordinates of the tip, where x
        # corresponds to the x axis of current group, and y corresponds to the
        # maximal values of current group.
        if (is.null(vertical_seg$tip)) {
            # for NULL tip data, the tip should vertically down the `x` and
            # `xend` of horizontal lines
            tip_data <- lapply(seq_len(nrow(horizontal_seg)), function(i) {
                data.frame(
                    x = c(horizontal_seg$x[[i]], horizontal_seg$xend[[i]]),
                    y = rep.int(horizontal_seg$yend[[i]], times = 2L),
                    stringsAsFactors = FALSE
                )
            })
        } else {
            tip_data <- lapply(vertical_seg$tip, function(data, flipped_aes) {
                data <- ggplot2::flip_data(data, flip = flipped_aes)
                data[intersect(names(data), c("x", "x0", "y"))]
            }, flipped_aes)
            vertical_seg$tip <- NULL
        }
        # unnest vertical_seg data with reference to tip_data
        row_numbers <- vapply(tip_data, nrow, integer(1L))
        tip_data <- do.call("rbind", tip_data)
        vertical_seg <- vertical_seg[
            rep(seq_len(nrow(vertical_seg)), times = row_numbers),
        ]
        vertical_seg <- cbind(vertical_seg, tip_data)

        # check data
        if (is.null(vertical_seg$y)) {
            cli::cli_abort("{ggplot2::flipped_names(flipped_aes)$y} must exist in {.field tip}")
        }
        if (is.null(vertical_seg$x)) {
            if (is.null(vertical_seg$x0)) {
                cli::cli_abort("One of {ggplot2::flipped_names(flipped_aes)$x} or {ggplot2::flipped_names(flipped_aes)$x0} must exist in {.field tip}") # nolint
            }
            # get scale objects from the draw function
            # modified from https://github.com/tidyverse/ggplot2/issues/3116#issue-406130577
            scales <- NULL
            pos <- 1L
            while (is.null(scales)) {
                env <- parent.frame(pos)
                has_scale <- any("plot" == names(env))
                has_scale <- has_scale && any("scales" == names(env$plot))
                has_scale <- inherits(env$plot$scales, "ScalesList")
                if (has_scale) {
                    scales <- env$plot$scales
                } else {
                    pos <- pos + 1L
                }
            }
            vertical_seg$x <- scales$get_scales("x")$map(vertical_seg$x0)
        }
        vertical_seg$xend <- vertical_seg$x
        if (is_rel(tip_length)) {
            vertical_seg$y <- vertical_seg$yend - unclass(tip_length) *
                (vertical_seg$yend - vertical_seg$y)
        } else {
            vertical_seg$y <- vertical_seg$yend - tip_length
        }

        seg_columns <- c(
            "x", "xend", "y", "yend",
            setdiff(non_pos_aes, c("label", "tip"))
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
