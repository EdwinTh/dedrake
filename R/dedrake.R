#' Alternate between regular R code and drake_plan
#'
#' \code {dedrake} takes a drake plan and strip it from the commas to separate
#'   the targets and commands and replace "=" by "<-".
#'   The result can run as regular R code without a dependency on drake.
#'   Reversely, \code{redrake} takes a piece of R code (as a character string)
#'   and turns it into a drake plan.
#'
#' @param plan An object of class \code{drake_plan}.
#' @param code A character string.
#' @return Functions do not return anything but is called for its side effects.
#'   The output is printed in the console and can be subsequently copy-pasted
#'   to a script.
#' @examples
#' some_plan <- drake::drake_plan(
#'   a = subset(mtcars, cyl == 4),
#'   a_further = plot(a$hp)
#' )
#' dedrake(some_plan)
#'
#' some_code <-
#'"a         <- subset(mtcars, cyl == 4)
#' a_further <- plot(a$hp)"
#' redrake(some_code)
#' @export
dedrake <- function(plan) {
  stopifnot(inherits(plan, "drake_plan"))
  targets  <- plan$target
  commands <- plan$command

  max_char <- max(nchar(targets))
  targets  <-
    substr(paste0(targets, paste0(rep(" ", max_char), collapse = "")), 1, max_char)

  for (i in seq_along(targets)) {
    cat(paste(targets[i], "<-", commands[i], "\n"))
  }
}

redrake <- function(code) {
  stopifnot(is.character(code))
  splitted     <- strsplit(code, "\n")[[1]]
  with_eq_sign <- paste0("  ", sub("<-", "=", splitted))
  all_but_last <- paste0(with_eq_sign[1:(length(with_eq_sign)-1)], ",\n")
  cat("drake_plan(\n")
  for (a in all_but_last) cat(a)
  cat(paste0(with_eq_sign[length(with_eq_sign)]), "\n")
  cat(")")
}


