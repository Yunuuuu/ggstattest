#' Layer with Grid or Function
#'
#' Draw ggplot2 layer with a grod or function.
#'
#' @param draw Either a [grob][grid::grob] object or a function which accepts
#'   two arguments (\code{data} and \code{coords}) and returns a
#'   [grob][grid::grob]. \cr \cr \code{data} contains value from parameter
#'   \code{data}, coords is value that have already been transformed to the plot
#'   scales. \cr \cr It is important to note that the columns names for the data
#'   and coords come from the aesthetic mappings in the \code{ggplot2} plot.
#' @inheritParams ggplot2::layer
#' @param ... Other arguments passed on to [ggplot2::layer()]. These are often
#'   aesthetics, used to set an aesthetic to a fixed value, like `colour =
#'   "red"` or `size = 3`. They may also be parameters to the paired geom/stat.
#'   Other arguments of function `draw` can be passed here.
#' @return a ggplot2 layer object
#' @details If you want to combine the functionality of multiple geoms it can
#'   usually be achieved by preparing the data for each of the geoms inside the
#'   `draw_*()` call and send it off to the different geoms, collecting the
#'   output in a [`grid::gList`] (a list of grobs) if the call is `draw_group()`
#'   or a [`grid::gTree`] (a grob containing multiple children grobs) if the
#'   call is `draw_panel()`.
#' @examples
#' ggdraw_text <- grid::textGrob(
#'     "ggdraw",
#'     x = c(0, 0, 0.5, 1, 1),
#'     y = c(0, 1, 0.5, 0, 1),
#'     hjust = c(0, 0, 0.5, 1, 1),
#'     vjust = c(0, 1, 0.5, 0, 1)
#' )
#' ggplot2::ggplot(data.frame(x = 1, y = 2)) +
#'     geom_draw(ggdraw_text)
#' @export
geom_draw <- function(draw = grid::nullGrob(),
                      mapping = NULL, data = NULL, stat = "identity",
                      position = "identity", ...,
                      na.rm = FALSE,
                      orientation = NA,
                      inherit.aes = TRUE) {
    ggplot2::layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomDraw,
        position = position,
        show.legend = FALSE,
        inherit.aes = inherit.aes,
        params = rlang::list2(
            draw = draw,
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
GeomDraw <- ggplot2::ggproto(
    "GeomDraw", ggplot2::Geom,
    ## No required_aes
    ## No default_aes
    ## No draw_key
    extra_params = c("na.rm", "orientation"),
    draw_group = function(data, panel_params, coord, draw, ..., 
                          na.rm, flipped_aes = FALSE) {
        if (grid::is.grob(draw)) {
            draw
        } else {
            draw <- rlang::as_function(draw)
            draw(data, panel_params, coord, ...)
        }
    }
)

#' @inherit ggplot2::Geom title seealso
#' @inheritSection  ggplot2::Geom Geoms
#' @inheritSection  ggplot2::Coord Coordinate systems
#' @name ggplot2-ggproto
NULL
