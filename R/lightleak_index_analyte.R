index_analyte <- function(XL) {
  data.frame(i = which(XL[, 1] == "Analyte:"),
             a = getIT(XL, "Analyte:"))
}
