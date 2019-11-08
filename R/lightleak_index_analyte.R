index_analyte <- function(XL) {
  data.frame(i = which(XL[, 1] == "Analyte:"),
             a = get_term_value(XL, "Analyte:"))
}
