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
