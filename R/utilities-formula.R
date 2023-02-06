get_expr_symbols <- function(x) {
    symbols <- switch(expr_type(x),
        # special cases
        # for missing argument in pairlist
        missing = ,
        constant = character(0L),
        symbol = rlang::as_string(x),
        call = lapply(x[-1L], get_expr_symbols),
        pairlist = lapply(x, get_expr_symbols)
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
