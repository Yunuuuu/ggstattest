annotest_tidy_fn <- function(method) {
    switch(method,
        cor = function(x) {
            paste("Cor", sprintf("%.2g", x), sep = ": ")
        },
        cor.test = function(x) {
            paste(
                c(names(x$estimate), "Pvalue"),
                sprintf("%.2g", c(x$estimate, x$p.value)),
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
                sprintf("%.2g", c(x$statistic, x$p.value)),
                sep = ": ",
                collapse = "\n"
            )
        },
        function(x) {
            paste("Pvalue", sprintf("%.2g", x$p.value), sep = ": ")
        }
    )
}
