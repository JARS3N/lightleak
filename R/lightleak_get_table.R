get_table <- function(df, thing,ncolM) {
  CAL <- which(df == thing)
  loc_A <- which(df[, 1] == "A")
  loc_pD <- which(df[, 1] == "% Diff")
  i0 <- (loc_A[loc_A > CAL][1]) - 1
  i1 <- loc_pD[loc_pD > CAL][1]
  end_col <- ncolM
  col_names <- c("Row", paste0("X", 1:end_col))
  df2 <- setNames(df[i0:i1, 1:(end_col + 1)], col_names)
  ###################
  tidyr::gather(df2[grepl("^[A-Z]$", df2$Row),], 'Col', 'Val', -Row) %>%
    mutate(Col = sprintf("%02d", as.numeric(gsub("X", "", Col)))) %>%
    tidyr::unite(., Well, Row, Col, sep = "") %>%
    mutate(., Val = as.numeric(Val))
}
