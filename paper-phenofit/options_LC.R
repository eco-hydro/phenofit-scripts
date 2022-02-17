library(purrr)
library(tibble)

list_option <- tibble::tribble(
    ~code, ~par,
    ## ------------
    c(-99), # LC codes, deault
    list(1, 2), # corresponding parameters
    ## ------------
    c(1, 2, 3), # LC codes
    list(1, 2), # corresponding parameters
    ## ------------
    c(4, 5), # LC codes
    list(1, 2) # corresponding parameters
)
codes <- list_option$code
.LC_code <- map(seq_along(codes), function(i) {
    data.table(I = i, code = codes[[i]])
}) %>% do.call(rbind, .)

find_lc <- function(x, default = 1L) {
    i <- .LC_code[code == x, I]
    if (length(i) == 0) i <- default
    i
}
