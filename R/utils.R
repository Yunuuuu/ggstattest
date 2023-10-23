#' @importFrom ggplot2 aes
#' @export
ggplot2::aes

#' @importFrom ggplot2 rel
#' @export
ggplot2::rel

#' @importFrom ggplot2 waiver
#' @export
ggplot2::waiver

is_rel <- function(x) inherits(x, "rel")

`%||%` <- function(a, b) {
    if (!is.null(a)) a else b
}

#' Rename elements in a list, data.frame or vector
#'
#' This is akin to `dplyr::rename` and `plyr::rename`. It renames elements given
#' as names in the `replace` vector to the values in the `replace` vector
#' without touching elements not referenced.
#'
#' @param x A data.frame or a named vector or list
#' @param replace A named character vector. The names identifies the elements in
#' `x` that should be renamed and the values gives the new names.
#'
#' @return `x`, with new names according to `replace`
#'
#' @keywords internal
#' @noRd
rename <- function(x, replace) {
    current_names <- names(x)
    old_names <- names(replace)
    missing_names <- setdiff(old_names, current_names)
    if (length(missing_names) > 0) {
        replace <- replace[!old_names %in% missing_names]
        old_names <- names(replace)
    }
    names(x)[match(old_names, current_names)] <- as.vector(replace)
    x
}

# Use chartr() for safety since toupper() fails to convert i to I in Turkish locale
lower_ascii <- "abcdefghijklmnopqrstuvwxyz"
upper_ascii <- "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

snake_class <- function(x) {
    snakeize(class(x)[1])
}
snakeize <- function(x) {
    x <- gsub("([A-Za-z])([A-Z])([a-z])", "\\1_\\2\\3", x)
    x <- gsub(".", "_", x, fixed = TRUE)
    x <- gsub("([a-z])([A-Z])", "\\1_\\2", x)
    to_lower_ascii(x)
}
to_lower_ascii <- function(x) chartr(upper_ascii, lower_ascii, x)
to_upper_ascii <- function(x) chartr(lower_ascii, upper_ascii, x)


has_groups <- function(data) {
    # If no group aesthetic is specified, all values of the group column equal
    # to NO_GROUP. On the other hand, if a group aesthetic is specified, all
    # values are different from NO_GROUP (since they are a result of
    # plyr::id()). NA is returned for 0-row data frames.
    data$group[1L] != NO_GROUP
}

# This needs to be less than 1, to distinguish it from "regular" return values
# of plyr::id() used by add_group()
NO_GROUP <- -1L

x_aes <- c(
    "x", "xmin", "xmax", "xend", "xintercept",
    "xmin_final", "xmax_final", "xlower", "xmiddle", "xupper", "x0"
)
y_aes <- c(
    "y", "ymin", "ymax", "yend", "yintercept",
    "ymin_final", "ymax_final", "lower", "middle", "upper", "y0"
)

all_aesthetics <- c(
    "adj", "alpha", "angle", "bg", "cex", "col", "color",
    "colour", "fg", "fill", "group", "hjust", "label", "linetype", "lower",
    "lty", "lwd", "max", "middle", "min", "pch", "radius", "sample", "shape",
    "size", "srt", "upper", "vjust", "weight", "width", "x", "xend", "xmax",
    "xmin", "xintercept", "y", "yend", "ymax", "ymin", "yintercept", "z"
)

# Look up the scale that should be used for a given aesthetic
aes_to_scale <- function(var) {
    var[var %in% x_aes] <- "x"
    var[var %in% y_aes] <- "y"
    var
}

# Figure out if an aesthetic is a position aesthetic or not
is_position_aes <- function(vars) {
    aes_to_scale(vars) %in% c("x", "y")
}

unique_n <- function(x) {
    length(unique(x))
}
