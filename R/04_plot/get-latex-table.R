# if (!requireNamespace("pacman")) install.packages("pacman")
pacman::p_load(glue, Hmisc)

get_latex_table <- function(
    models,
    groups, # "Group", "Cluster" or "Subcluster"
    var_selection,
    label = "tbl:test", rowname = NULL, ...) {
  
  if (groups == "Group") {
    models$group_models |> 
      filter(Variable %in% var_selection) |> 
      select(!Group:Comparison) |> 
      latex(
        booktabs = TRUE, rowname = NULL, file = "", title = "", 
        insert.bottom = glue("\\label{{{label}}}"), ...
      )
    
  } else if (groups == "Cluster") {
    models$cluster_models |> 
      filter(Variable %in% var_selection) |> 
      select(Comparison:last_col()) |> 
      latex(
        booktabs = TRUE, rowname = "", file = "", title = "", 
        insert.bottom = glue("\\label{{{label}}}"), rgroup = var_selection, ...
      )
    
  } else if (groups == "Subcluster") {
    models$subcluster_models |> 
      filter(Variable %in% var_selection) |> 
      select(Comparison:last_col()) |> 
      latex(
        booktabs = TRUE, rowname = "", file = "", title = "", 
        insert.bottom = glue("\\label{{{label}}}"), rgroup = var_selection, ...
      )
    
  } else {
    stop("The 'groups' argument must be either 'Group', 'Cluster' or 'Subcluster'")
  }
}

# Typically:
# get_latex_table(models, "Group",      var_selection("original"))
# get_latex_table(models, "Cluster",    var_selection("reduced"))
# get_latex_table(models, "Subcluster", var_selection("reduced"))