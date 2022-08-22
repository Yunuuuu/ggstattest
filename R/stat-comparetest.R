stat_comparetest <- function(mapping = NULL, data = NULL, method = NULL,
                             compare_list = NULL,
                             method_args = list(), tidy_fn = NULL,
                             geom = "comparetest", position = "identity",
                             na.rm = FALSE, show.legend = NA,
                             inherit.aes = TRUE, ...) {
    ggplot2::layer(
        stat = StatComparetest, data = data, mapping = mapping, geom = geom,
        position = position, show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = rlang::list2(
            method = method,
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
        # check method and tidy_fn
        if (is.null(params$method) || identical(params$method, "auto")) {
            params$method <- "nonparametric"
            msg <- c(msg, paste0("method = \"", params$method, "\""))
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
                rlang::abort(
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
                    "!" = "the number of unique values in {.field {ggplot2::flipped_names(params$flipped_aes)$x}} is below than 2." # nolint
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
    compute_panel = function(data, scales, method = NULL,
                             compare_list = NULL, method_args = list(),
                             tidy_fn = NULL, na.rm = FALSE,
                             flipped_aes = FALSE) {
        data <- ggplot2::flip_data(data, flipped_aes)
        scales <- ggplot2::flip_data(scales, flipped_aes)
        unique_values <- unclass(unique(data$x))
        unique_numbers <- length(unique_values)

        # set defaul value for compare_list
        if (is.null(compare_list)) {
            compare_list <- utils::combn(
                unique_values, 2L,
                simplify = FALSE
            )
        } else {
            # if user provide compare_list, ensure it is in the same scale of x
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
            }, logical(1L))
        ]
        # define y postion - the max value
        max_y_df <- aggregate(y ~ x, data = data, FUN = max)
        x_to_maxy <- max_y_df[["y"]]
        names(x_to_maxy) <- as.character(max_y_df[["x"]])

        stat_data <- lapply(compare_list, function(comparison) {
            tip <- tibble::tibble(
                x = comparison,
                y = unname(x_to_maxy[as.character(comparison)])
            )
            h_segments <- range(comparison)
            tibble::tibble(
                xmin = h_segments[[1L]],
                xmax = h_segments[[2L]],
                y = max(tip$y),
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
            label <- vapply(compare_list, function(comparison) {
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
                test_data <- data[data$x %in% comparison, ]
                test_data$x <- factor(test_data$x)
                test_res <- rlang::inject(method(
                    formula = y ~ x,
                    data = test_data,
                    !!!method_args
                ))
                as.character(rlang::as_function(tidy_fn)(test_res))
            }, character(1L))
        }
        stat_data$x <- (stat_data$xmin + stat_data$xmax) / 2L
        stat_data$label <- label
        stat_data$tip <- lapply(
            stat_data$tip, ggplot2::flip_data,
            flip = flipped_aes
        )
        stat_data$flipped_aes <- flipped_aes
        ggplot2::flip_data(
            stat_data[order(stat_data$y), ], 
            flipped_aes
        )
    },
    required_aes = c("x", "y")
)
