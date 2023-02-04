#' @param method the method used to implement test. Defaults to "nonparametric",
#' which will use "wilcox.test" to compare two groups and use "kruskal.test" to
#' compare three or more groups. Support lamda created from a formula. This can
#' also be "none" indicates no test will be implemented. The method should
#' always accept a **formula** argument whose value will be set as `y ~ x`, the
#' `x` is the discrete variable and the `y` is the continuous variable.
#' @param compare_list A list of atomic vectors with a length of at least 2. The
#' entries in the vector are the values on the x-axis (or y-axis) indicating the
#' comparison among what groups. the order will be regarded as the level of `x`
#' variable.
#' @param hide_ns A logical value or a function (can be purrr-style) which take
#' statistical result as an argument and return a logical value indicating
#' whether hide this result. If TRUE, this will flag the statistical result
#' whose `p.value > 0.05` ("p.value" is obtained by `stat_result$p.value`, so
#' ensure the results returned by `method` have a "p.value" item) with
#' "...hide..." and `geom_comparetest` will remove rows with "...hide...".
#' @param method_args other arguments passed to function specified in
#' `method`.
#' @param tidy_fn A function or formula which accepts results returned by
#' function in **method** and return a scalar character.
#'
#'   If a **function**, it is used as is.
#'
#'   If a **formula**, e.g. `~ .x + 2`, it is converted to a function with up to
#'   two arguments: `.x` (single argument) or `.x` and `.y` (two arguments). The
#'   `.` placeholder can be used instead of `.x`.  This allows you to create
#'   very compact anonymous functions (lambdas) with up to two inputs. Functions
#'   created from formulas have a special class. Use `is_lambda()` to test for
#'   it.
#'
#'   If a **string**, the function is looked up in `globalenv()`.
#' Notes: if **method** is "none", this can be a list (whose length should equal
#' to the number of **PANEL**) of labels corresponding to each comparisons in
#' compare_list, this can be matched by position or names if both **tidy_fn**
#' and **compare_list** have names.
#' @inheritParams ggplot2::stat_identity
#' @section Computed variables:
#' `stat_comparetest()` provides the following variables, some of which depend on the orientation:
#' \describe{
#'   \item{xmin *or* ymin}{the left (or lower) side of horizontal (or vertical)
#'   segments underneath label}
#'   \item{xmax *or* ymax}{the right (or upper) side of horizontal (or vertical)
#'   segments underneath label}
#'   \item{x *or* y}{the x (or y) coordinates for labels, usually equal to
#'   (xmin + xmax) / 2 or (ymin + ymax) / 2}
#'   \item{y *or* x}{the y (or x) coordinates for labels, usually equal to
#'   the max y-axis (x-axis) value span from xmin (ymin) to xmax (ymax)}
#'   \item{label}{the statistical test results}
#'   \item{tip}{a list of numerical vector gives the x-axis (discrete position
#'   aesthetic) coordinates of tip, the tip will be drawn down the position.
#'   }
#' }
#' @rdname geom_comparetest
#' @export
stat_comparetest <- function(mapping = NULL, data = NULL, method = NULL,
                             compare_list = NULL, hide_ns = NULL,
                             method_args = NULL, tidy_fn = NULL,
                             geom = "comparetest", position = "identity",
                             na.rm = FALSE, show.legend = NA,
                             inherit.aes = TRUE, ...) {
    ggplot2::layer(
        stat = StatComparetest, data = data, mapping = mapping, geom = geom,
        position = position, show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = rlang::list2(
            method = method,
            hide_ns = hide_ns,
            compare_list = compare_list,
            method_args = method_args,
            tidy_fn = tidy_fn,
            na.rm = na.rm, ...
        )
    )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatComparetest <- ggplot2::ggproto("StatComparetest", ggplot2::Stat,
    required_aes = c("x", "y"),
    setup_params = function(self, data, params) {
        # orientation should be the axis with Categorical variable
        params$flipped_aes <- ggplot2::has_flipped_aes(
            data, params,
            main_is_continuous = FALSE
        )
        data <- ggplot2::flip_data(data, params$flipped_aes)

        # check data with proper values
        has_x <- !(is.null(data$x) && is.null(params$x))
        has_y <- !(is.null(data$y) && is.null(params$y))
        if (!has_x && !has_y) {
            cli::cli_abort("{.fn {snake_class(self)}} requires an {.field x}, {.field y} aesthetic.") # nolint
        }

        msg <- character()
        if (is.null(params$hide_ns)) {
            if (is.null(params$method) || identical(params$method, "auto")) {
                params$hide_ns <- TRUE
            } else {
                params$hide_ns <- FALSE
            }
            msg <- c(msg, paste0("`hide_ns = \"", params$hide_ns, "\"`"))
        }
        # check method and tidy_fn
        if (is.null(params$method) || identical(params$method, "auto")) {
            params$method <- "nonparametric"
            msg <- c(msg, paste0("`method = \"", params$method, "\"`"))
        }
        if (is.null(params$tidy_fn)) {
            params$tidy_fn <- function(x) {
                stats::symnum(
                    x$p.value,
                    cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1),
                    symbols = c("****", "***", "**", "*", "ns")
                )
            }
        }

        if (identical(params$method, "none") && is.list(params$tidy_fn)) {
            if (!identical(length(params$tidy_fn), length(unique(data$PANEL)))) {
                cli::cli_abort(
                    "{.arg tidy_fn} should be a list with the same number ({length(unique(data$PANEL))}) of {.field PANEL}" # nolint
                )
            }
        }

        # check there has at least 2 unique x values to implement test
        unique_values <- unique(data$x)
        unique_numbers <- length(unique_values)
        if (unique_numbers < 2L) {
            cli::cli_warn(
                c(
                    "Cannot implment statistical test.",
                    "!" = "the number of unique values in {.field {ggplot2::flipped_names(params$flipped_aes)$x}} is less than 2." # nolint
                )
            )
        }

        # set default value
        if (length(msg)) {
            cli::cli_inform("{.fn stat_comparetest} using {msg}")
        }
        params
    },
    extra_params = c("na.rm", "orientation"),
    setup_data = function(data, params) {
        data <- ggplot2::remove_missing(
            data,
            vars = c("x", "y"),
            na.rm = params$na.rm,
            name = "stat_comparetest"
        )
        data
    },
    compute_panel = function(data, scales, method = NULL, hide_ns,
                             compare_list = NULL, method_args = list(),
                             tidy_fn = NULL, na.rm = FALSE,
                             flipped_aes = FALSE) {
        data <- ggplot2::flip_data(data, flipped_aes)
        scales <- ggplot2::flip_data(scales, flipped_aes)
        if (!scales$x$is_discrete()) {
            cli::cli_warn(c(
                "Continuous {.field {ggplot2::flipped_names(params$flipped_aes)$x}} aesthetic",
                "!" = "{.fn stat_comparetest} will always regard {.field {ggplot2::flipped_names(params$flipped_aes)$x}} as a discrete variable" # nolint
            ))
        }
        unique_values <- unclass(unique(data$x))
        unique_numbers <- length(unique_values)

        # set defaul value for compare_list
        # if NULL, all paired comparison will be performed
        if (is.null(compare_list)) {
            compare_list <- utils::combn(unique_values, 2L, simplify = FALSE)
        } else {
            # if user provides compare_list, ensure it is in the same scale of x
            # axis
            compare_list <- lapply(compare_list, function(comparison) {
                unclass(scales$x$map(comparison))
            })
        }

        if (identical(method, "wilcox.test") || identical(method, "t.test")) {
            compare_list <- compare_list[
                lengths(compare_list) == 2L
            ]
        }
        compare_list <- compare_list[
            vapply(compare_list, function(x) {
                all(x %in% unique_values)
            }, logical(1L), USE.NAMES = FALSE)
        ]

        # define y postion - the max value
        max_y_df <- aggregate(y ~ x, data = data, FUN = max, na.rm = TRUE)
        x_to_maxy <- max_y_df[["y"]]
        names(x_to_maxy) <- as.character(max_y_df[["x"]])
        # browser()
        stat_data <- lapply(compare_list, function(comparison) {
            tip <- comparison
            h_segments <- range(comparison)
            tibble::tibble(
                xmin = h_segments[[1L]],
                xmax = h_segments[[2L]],
                # Since the horizontal segments will span across xmin:xmax
                # the y value should be maximal y among xmin:xmax
                y = max(unname(
                    x_to_maxy[as.character(xmin:xmax)]
                ), na.rm = TRUE),
                tip = list(tip)
            )
        })
        stat_data <- do.call("rbind", stat_data)

        # define label
        if (identical(method, "none")) {
            if (is.list(tidy_fn)) tidy_fn <- tidy_fn[[data$PANEL[[1L]]]]
            if (is.character(tidy_fn) || is.list(tidy_fn)) {
                label <- vapply(seq_along(compare_list), function(i) {
                    if (is.null(names(tidy_fn)) || is.null(names(compare_list))) {
                        label <- tidy_fn[[i]]
                    } else {
                        label <- tidy_fn[[names(compare_list)[[i]]]]
                    }
                    label
                }, character(1L))
            } else {
                label <- rlang::rep_along(compare_list, NA_character_)
            }
        } else {
            if (identical(method, "nonparametric")) {
                if (unique_numbers > 2L) {
                    method <- "kruskal.test"
                } else if (identical(unique_numbers, 2L)) {
                    method <- "wilcox.test"
                }
            } else if (identical(method, "parametric")) {
                if (unique_numbers > 2L) {
                    method <- "aov"
                } else if (identical(unique_numbers, 2L)) {
                    method <- "t.test"
                }
            }
            method <- rlang::as_function(method)
            label <- vapply(compare_list, function(comparison) {
                test_data <- data[data$x %in% comparison, ]
                # since in ggplot2, position aesthetics are always regarded as
                # numerical value, we transform it into factor to perform
                # comparison,
                test_data$x <- factor(test_data$x, levels = comparison)
                test_res <- rlang::inject(method(
                    formula = y ~ x,
                    data = test_data,
                    !!!method_args
                ))
                if (is.logical(hide_ns) && isTRUE(hide_ns)) {
                    if (!is.null(test_res$p.value) && test_res$p.value > 0.05) {
                        return("...hide...")
                    }
                } else if (!is.logical(hide_ns)) {
                    if (isTRUE(rlang::as_function(hide_ns)(test_res))) {
                        return("...hide...")
                    }
                }
                as.character(rlang::as_function(tidy_fn)(test_res))
            }, character(1L))
        }
        stat_data$x <- (stat_data$xmin + stat_data$xmax) / 2L
        stat_data$label <- label
        stat_data$flipped_aes <- flipped_aes
        stat_data
    }
)
