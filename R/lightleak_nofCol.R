# Using Meta Data subset for correct Number of columns
# because data format from Utility is inconsistent
nofCol <- function(Meta) {
  x <- substr(Meta$Serialnumber, 1, 2)
  list(
    "42" = 6,
    "41" = 12,
    "43" = 1,
    "21" = 12,
    "10" = 6
  )[x]
}
