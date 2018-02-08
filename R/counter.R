#' Create a counter function.
#'
#' @param init An initial number for the counter.
#' @param count_by A number for skip counting.
#' @return A counter function with a counter initialized to \code{init} that increments by \code{count_by} each time the function is called.
#' @examples
#' counter <- make_counter()
#' counter()
#' counter()
#'
#' count_from_1 <- make_counter(init = 1)
#' count_from_1()
#' count_from_1()
#'
#' count_down_from_10 <- make_counter(count_by = -1)
#' count_down_from_10()
#' count_down_from_10()
#' @export
make_counter <- function(init = 0, count_by = 1) {
  stopifnot(!is.na(init), !is.na(count_by),
            is.numeric(count_by), length(count_by) == 1)
  i <- init
  function() {
    i <<- i + count_by
    i
  }
}
