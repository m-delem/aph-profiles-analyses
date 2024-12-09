# if (!requireNamespace("pacman")) install.packages("pacman")
pacman::p_load(diceR)

cluster_vars <- function(
    df, k = 2:4, 
    consensus_f = c("kmodes", "CSPA", "majority"),
    distance = "maximum",
    ...
){
  clustering <-
    df |>
    dice(
      nk       = k,
      k.method = "all",
      p.item   = 0.95,
      reps     = 100,
      algorithms = c(
        "hc",
        "gmm",
        "pam",
        "km",
        "cmeans"
      ),
      hc.method = "complete",
      distance  = distance,
      cons.funs = consensus_f,
      trim      = TRUE,
      reweigh   = TRUE
    )
  
  return(clustering)
}