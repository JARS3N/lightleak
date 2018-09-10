split_analytes <- function(XL, w) {
  rng_O2<-(w$i[1] - 1):(w$i[2] - 1)
  rng_pH<-(w$i[2] - 1):(nrow(XL))
 list(
    O2=XL[rng_O2,],
   pH= XL[rng_pH,]
  )
}


