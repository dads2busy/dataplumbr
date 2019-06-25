#' Trampoline
#'
#' A trampoline is a function or set of functions that together give us the tools we
#' need to write code in a recursive style, in a way that doesn't overflow the stack.
#' Here's an awesome trampoline by Jim Hester:
#' @param f A function.
#' @param ... Any number of named or unnamed arguments.
#' @examples
#' countdown <- trampoline(function(n) {
#'   if (n > 0) recur(n-1) else "done"
#' })
#' countdown(10000)
#' [1] "done"
trampoline <- function(f, ...) {
    function(...) {
        ret <- f(...)
        while (inherits(ret, "recursion")) {
            ret <- eval(as.call(c(f, unclass(ret))))
        }
        ret
    }
}

#' Recur
#'
#' @param ... Any number of named or unnamed arguments.
recur <- function(...) {
    structure(list(...), class = "recursion")
}

