# Using Meta Data subset for correct Number of columns
# because data format from Utility is inconsistent
nofCol <- function(u) {
  x <- substr(rm_leading_zero(u),1,2)
  unlist(list(
    "42" = 6,
    "41" = 12,
    "43" = 1,
    "21" = 12,
    "10" = 6,
    "44" = 1,
    "45" = 12
  )[x])
}
