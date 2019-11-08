pull_meta <- function(XL, xlfile) {
  nms <- c(
    "Light Leak Test",
    "Serial number:",
    "Date:",
    "Time Start:",
    "Time End:",
    "Final result:",
    "High Limit Multiplicator:",
    "Low Limit Multiplicator:",
    "Num Points:"
  )
  met <-
    setNames(as.data.frame(lapply(nms, get_term_value, data = XL),stringsAsFactors = FALSE), gsub("[ ]|[:]", "", nms))
  met <- met[, order(names(met))]
  met$file <- xlfile
  met
}


