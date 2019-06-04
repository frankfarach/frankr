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


#' Safe execution of a function that might fail
#'
#' @param .f A function
#' @param n_tries Number of times to try executing the function
#' @param sleep_for Number of seconds to make system sleep after each unsuccessful attempt
#' @param ... Arguments for .f
#' @examples
#' # Function success rate is `p`
#' get_data <- function(p = 0.8) {
#'   x <- rbinom(1, 1, p)
#'   ifelse(x == 0, "OK", stop("Error: too many calls!"))
#'   }
#'
#' # Success on 3rd attempt
#' set.seed(556)
#' do_fun_wait(get_data, 10, 1)
#'
#' # NULL if failed after n_tries
#' set.seed(55)
#' do_fun_wait(get_data, 10, 1)
#'
#' # Pass .f arguments into ...
#' do_fun_wait(get_data, 10, 1, p = 0.6)
#' @seealso \url{https://www.brodrigues.co/blog/2018-03-12-keep_trying/}
#' @export
do_fun_wait <- function(.f, n_tries, sleep_for = 1L, ...){
  attempt::stop_if_not(.f, is.function, "`.f` must be a function")
  attempt::stop_if_not(n_tries, is.numeric,
                       "`n_tries` must be numeric")
  attempt::stop_if_not(sleep_for, is.numeric,
                       "`sleep_for` must be numeric")

  possibly_fn <- purrr::possibly(.f, otherwise = NULL)

  result <- NULL
  try_count <- 1

  while(is.null(result) && try_count <= n_tries) {
    msg <- paste0("Attempt ", try_count, " of ", n_tries, "...")
    message(msg)
    try_count = try_count + 1
    result <- possibly_fn(...)
    Sys.sleep(sleep_for)
  }

  return(result)
}
