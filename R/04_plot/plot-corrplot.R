# ------------------------------------------------------------------------------
# Alternative method to plot the correlation matrix using "corrplot"
# ------------------------------------------------------------------------------

# pacman::p_load(corrplot)
# needs to run trace(corrplot,edit=TRUE) and add + 0.25 to line 449 to modify
# the function and put the stars on top of the coefficients

# p_mat_raw <- 
#   df |> 
#   scale_vars() |> 
#   select(vviq:score_comprehension) |> 
#   rename_vars() |> 
#   cor() |> 
#   cor.mtest(conf.level = .95)

# p_mat_adj <- p.adjust(c(p_mat_raw[[1]]), method = "bonferroni")

# p_mat <- matrix(
#   p_mat_adj, 
#   ncol = dim(p_mat_raw[[1]])[1],
#   dimnames = list(
#     colnames(p_mat_raw[[1]]), 
#     colnames(p_mat_raw[[1]])
#   )
# )

# if (params$save_figures) {
#   png(
#     filename = here("figures/correlation-matrix.png"), 
#     width = 14, 
#     height = 12, 
#     units = "in", 
#     res = 300
#   )
# }
# 
# df |>
#   scale_vars() |> 
#   select(vviq:score_comprehension) |> 
#   rename_vars() |> 
#   cor() |> 
#   corrplot(
#     method = "color",
#     type = "upper",
#     diag = FALSE,
#     order = "hclust",
#     hclust.method = "ward.D2",
#     addCoef.col = "black",
#     tl.col = "black",
#     tl.srt = 45,
#     tl.cex = 1.4,
#     tl.offset = 0.8,
#     number.cex = 1.1,
#     cl.cex = 1.1,
#     cl.ratio = 0.1,
#     p.mat = p_mat_raw$p,
#     insig = "label_sig",
#     sig.level = c(.05/153, 0.001/153,0.01/153),
#     pch.cex = 1.4
#   )
# 
# if (params$save_figures) dev.off()