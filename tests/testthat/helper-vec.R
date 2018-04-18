
catch_wngs <- function(expr) {
  wngs <- list()
  withCallingHandlers(expr, warning = function(wng) {
    wngs <<- c(wngs, list(wng))
    invokeRestart("muffleWarning")
  })
  wngs
}

catch_warning_msgs <- function(expr) {
  wngs <- catch_wngs(expr)
  flatten_chr(pluck(wngs, "message"))
}
