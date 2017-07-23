#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @importFrom jsonlite fromJSON
#' @importFrom dplyr filter_
#' @importFrom purrr as_function
#' @export
dplyr::filter_
#' @importFrom lazyeval interp
#' @export
lazyeval::interp

## paste pipes
`%+%`  <- function(a,b) paste(a, b, sep = '')
`%+|%` <- function(a,b) paste(a, b, sep = '|')
`%+&%` <- function(a,b) paste(a, b, sep = '&')

## capture_error 
capture_error <- function(code, otherwise = NULL, quiet = TRUE) {
    tryCatch(
        list(result = code, error = NULL),
        error = function(e) {
            if (!quiet)
                message("Error: ", e$message)
            
            list(result = otherwise, error = e)
        }
    )
}

## persistently
persistently <- function(.f, otherwise = NULL, quiet = TRUE, max_attempts = 5, wait_seconds = 0) {
    .f <- as_function(.f)
    force(otherwise)
    force(quiet)
    force(max_attempts)
    force(wait_seconds)
    function(...) {
        for (i in seq_len(max_attempts)) {
            answer <- capture_error(.f(...), quiet = quiet)
            if (is.null(answer$error)) {
                return(answer$result)
            }
            if (wait_seconds > 0) {
                actual_wait_seconds <- runif(1, 0, wait_seconds * 2 ^ (i - 1))
                if (!quiet) {
                    message(sprintf("Retrying in %.3g seconds.", actual_wait_seconds))
                }
                Sys.sleep(actual_wait_seconds)
            }
        }
        if (!quiet) {
            msg <- sprintf(
                "%s failed after %d tries; returning %s.",
                deparse(match.call()),
                max_attempts,
                format(otherwise)
            )
            message(msg)
        }
        otherwise
    }
}
