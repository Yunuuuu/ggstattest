#' Annotation: statistical test
#'
#' Most useful for adding statistical test results.
#'
#' @param mapping Set of aesthetic mappings created by [aes()]. If specified and
#'   `inherit.aes = TRUE` (the default), it is combined with the default mapping
#'   at the top level of the plot. You must supply `mapping` if there is no plot
#'   mapping. This is not the same with other `ggplot2` geoms, which is just for
#'   statistical test but not for plot.
#' @param data The data to be displayed in this layer. There are three
#'    options:
#'
#'    If `NULL`, the default, the data is inherited from the plot
#'    data as specified in the call to [ggplot()].
#'
#'    A `data.frame`, or other object, will override the plot
#'    data. All objects will be fortified to produce a data frame. See
#'    [fortify()] for which variables will be created.
#'
#'    A `function` will be called with a single argument,
#'    the plot data. The return value must be a `data.frame`, and
#'    will be used as the layer data. A `function` can be created
#'    from a `formula` (e.g. `~ head(.x, 10)`).
#' @param method the method used to implement test.
#' @param formula passed to function specified in **method**.
#' @param label_x,label_y the coordinates of the label, usually in "npc" units.
#' @param hjust,vjust The justification of the text relative to its (x, y)
#' location. Possible string values are: "left", "right", "centre", "center",
#' "bottom", and "top". For numeric values, 0 means left (bottom) alignment and
#' 1 means right (top) alignment. **hjust** for horizontal justification and
#' **vjust** for vertical justification.
#' @param method_args other arguments passed to function specified in
#' **method**.
#' @param label_fn A function or formula which accepts results returned by
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
#' @param angle The angle to rotate the text.
#' @param units A string indicating the default units `labelx` and `label_y`
#' use.
#' @param label_gp other arguments to define the text attributes.
#' @param na.rm If `FALSE`, the default, missing values are removed with a
#'   warning. If `TRUE`, missing values are silently removed.
#' @inheritParams ggplot2::layer
#' @inheritParams geom_comparetest
#' @export
#' @rdname annotation_test
annotation_test <- function(mapping = NULL, data = NULL,
                            method = NULL, formula = NULL,
                            label_x, label_y, hjust = 0.5, vjust = 0.5,
                            method_args = NULL, label_fn = NULL,
                            ..., angle = 0,
                            units = "npc", label_gp = NULL,
                            na.rm = FALSE,
                            orientation = NA,
                            inherit.aes = TRUE) {
    ggplot2::layer(
        data = data,
        mapping = mapping,
        stat = StatAnnotest,
        geom = GeomAnnotest,
        position = "identity",
        show.legend = FALSE,
        inherit.aes = inherit.aes,
        params = rlang::list2(
            label_x = label_x, label_y = label_y,
            method = method,
            formula = formula,
            method_args = method_args,
            label_fn = label_fn,
            hjust = hjust, vjust = vjust, angle = angle,
            units = units, label_gp = label_gp,
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
GeomAnnotest <- ggplot2::ggproto("GeomAnnotest", ggplot2::Geom,
    setup_params = function(self, data, params) {
        params$flipped_aes <- ggplot2::has_flipped_aes(
            data, params,
            ambiguous = TRUE
        )
        label_gp <- list(
            colour = "black", size = 3.88,
            alpha = NA, family = "", fontface = 1,
            lineheight = 1.2
        )
        if (is.null(params$label_gp)) {
            params$label_gp <- label_gp
        } else {
            params$label_gp <- params$label_gp[
                intersect(names(params$label_gp), names(label_gp))
            ]
            params$label_gp <- c(
                as.list(params$label_gp),
                label_gp[setdiff(names(label_gp), names(params$label_gp))]
            )
        }
        params
    },
    extra_params = c("na.rm", "orientation"),
    draw_key = function(...) {
        ggplot2::zeroGrob()
    },
    draw_panel = function(data, panel_params, coord, hjust, vjust,
                          angle, label_gp, units = "npc",
                          na.rm, flipped_aes = FALSE) {
        grid::textGrob(
            label = data$label,
            x = data$label_x,
            y = data$label_y,
            default.units = units,
            hjust = hjust,
            vjust = vjust,
            rot = angle,
            gp = grid::gpar(
                col = scales::alpha(label_gp$colour, label_gp$alpha),
                fontsize = label_gp$size * ggplot2::.pt,
                fontfamily = label_gp$family,
                fontface = label_gp$fontface,
                lineheight = label_gp$lineheight
            ),
            check.overlap = FALSE
        )
    }
)

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatAnnotest <- ggplot2::ggproto("StatAnnotest", ggplot2::Stat,
    setup_params = function(self, data, params) {
        params$flipped_aes <- ggplot2::has_flipped_aes(
            data, params,
            ambiguous = TRUE
        )
        msg <- character()
        # check formula
        if (is.null(params$formula)) {
            params$formula <- y ~ x
            msg <- c(msg, paste0("formula = \"", deparse(params$formula), "\""))
        }
        if (length(msg)) {
            cli::cli_inform("{.fn stat_annotest} using {msg}")
        }
        formula_symbols <- get_expr_symbols(params$formula)
        if (!all(formula_symbols %in% names(data))) {
            cli::cli_abort(
                "Not all variables in {.arg formula = {params$formula}} exist in {.var data}}"
            )
        }
        params
    },
    extra_params = c("na.rm", "orientation"),
    # let statistical test to handle NA value
    # setup_data = function(data, params) {
    #     data <- ggplot2::remove_missing(
    #         data,
    #         vars = get_expr_symbols(params$formula),
    #         na.rm = params$na.rm,
    #         name = "stat_annotest"
    #     )
    #     data
    # },
    optional_aes = c(
        "adj", "alpha", "angle", "bg", "cex", "col", "color",
        "colour", "fg", "fill", "group", "hjust", "label", "linetype", "lower",
        "lty", "lwd", "max", "middle", "min", "pch", "radius", "sample",
        "shape", "size", "srt", "upper", "vjust", "weight", "width", "x",
        "xend", "xmax", "xmin", "xintercept", "y", "yend", "ymax", "ymin",
        "yintercept", "z"
    ),
    compute_panel = function(data, scales, method = NULL, formula,
                             method_args = list(), label_fn = NULL,
                             label_x, label_y,
                             na.rm = FALSE,
                             flipped_aes = FALSE) {
        # set defaul value for method and label_fn
        # if (scales$x$is_discrete() && is.numeric(data$y)) {
        #     data$x <- factor(data$x)
        # }
        # if (scales$y$is_discrete() && is.numeric(data$y)) {
        #     data$y <- factor(data$y)
        # }
        if (is.null(method)) {
            rhs_symbols <- get_expr_symbols(rlang::f_rhs(formula))
            lhs_symbols <- get_expr_symbols(rlang::f_lhs(formula))
            if (length(lhs_symbols) == 1L) {
                if (!aes_is_discrete(data, scales, lhs_symbols)) {
                    if (length(rhs_symbols) == 1L) {
                        if (aes_is_discrete(data, scales, rhs_symbols)) {
                            if (unique_n(data[[rhs_symbols]]) > 2L) {
                                method <- "kruskal.test"
                            } else {
                                method <- "wilcox.test"
                            }
                        } else {
                            method <- "cor.test"
                        }
                    } else if (length(rhs_symbols) > 1L) {
                        method <- "lm"
                    }
                } else {
                    method <- "glm"
                }
            }
            if (is.null(method)) {
                cli::cli_abort("A {.arg method} argument is needed.")
            }
        }
        if (is.null(label_fn)) {
            label_fn <- annotest_label_fn(method)
        }
        if (is.null(method_args)) {
            if (identical(method, "glm") || identical(method, stats::glm)) {
                method_args <- list(
                    family = stats::binomial("logit")
                )
            }
        }
        method <- rlang::as_function(method)
        stat_res <- rlang::inject(
            method(formula = !!formula, data = data, !!!method_args)
        )
        label_fn <- rlang::as_function(label_fn)
        label <- as.character(label_fn(stat_res))
        if (length(label) != 1L) {
            cli::cli_abort(
                "Function {.arg label_fn} should return a scalar string."
            )
        }
        data.frame(
            label = label, label_x = label_x,
            label_y = label_y,
            stringsAsFactors = FALSE
        )
    }
)

aes_is_discrete <- function(data, scales, aes_name) {
    scale <- aes_to_scale(aes_name)
    if (any(scale == c("x", "y"))) {
        scales[[scale]]$is_discrete()
    } else {
        is.character(data[[aes_name]]) || is.factor(data[[aes_name]])
    }
}

get_expr_symbols <- function(x) {
    type_x <- expr_type(x)
    symbols <- switch(type_x,
        # for missing argument in pairlist
        missing = ,
        # seems like this will always live in the end of anonymous function call
        integer = ,
        constant = character(0L),
        symbol = rlang::as_string(x),
        call = lapply(x[-1L], get_expr_symbols),
        pairlist = lapply(x, get_expr_symbols),
        cli::cli_abort("Don't know how to handle type {.cls {type_x}}.")
    )
    unlist(symbols, recursive = TRUE, use.names = FALSE)
}

expr_type <- function(x) {
    if (rlang::is_missing(x)) {
        "missing"
    } else if (rlang::is_syntactic_literal(x)) {
        "constant"
    } else if (is.symbol(x)) {
        "symbol"
    } else if (is.call(x)) {
        "call"
    } else if (is.pairlist(x)) {
        "pairlist"
    } else {
        typeof(x)
    }
}

annotest_label_fn <- function(method) {
    switch(method,
        cor = function(x) {
            sprintf("Cor: %.2g", x)
        },
        cor.test = function(x) {
            paste(
                c(names(x$estimate), "Pvalue"),
                c(sprintf("%.2g", x$estimate), format_pval(x$p.value)),
                sep = ": ",
                collapse = "\n"
            )
        },
        chisq.test = ,
        t.test = ,
        kruskal.test = ,
        wilcox.test = function(x) {
            paste(
                c(names(x$statistic), "Pvalue"),
                c(sprintf("%.2g", x$statistic), format_pval(x$p.value)),
                sep = ": ",
                collapse = "\n"
            )
        },
        function(x) {
            sprintf("Pvalue: %.2g", format_pval(x$p.value))
        }
    )
}

format_pval <- function(x) {
    ifelse(x < 0.01, sprintf("%.2e", x),
        ifelse(x < 0.05, sprintf("%.3f", x), sprintf("%.2f", x))
    )
}
