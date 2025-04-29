get_latex_table <- function(
    models,
    groups, # "Group", "Cluster" or "Subcluster"
    type    = "original",
    label   = "tbl:test", 
    rowname = NULL, 
    ...
) {
  if (type == "original") {
    vars <- c(
      # Original scores
      "VVIQ",
      "OSIVQ-Object",
      "OSIVQ-Spatial",
      "OSIVQ-Verbal",
      "Psi-Q Vision",
      "Psi-Q Audition",
      "Psi-Q Smell",
      "Psi-Q Taste",
      "Psi-Q Touch",
      "Psi-Q Sensations",
      "Psi-Q Feelings",
      "Raven matrices",
      "SRI",
      "Digit span",
      "Spatial span",
      "Similarities test",
      # Complex tasks
      "WCST",
      "Reading\ncomprehension"
    )
  } else if (type == "reduced") {
    vars <- c(
      # Reduced variables
      "Visual imagery",
      "Auditory imagery",
      "Sensory imagery",
      "Spatial imagery",
      "Verbal strategies",
      "Raven +\nDigit Span",
      # "Non-verbal\nreasoning",
      "Verbal reasoning",
      "Spatial span std.",
      # Complex tasks
      "WCST",
      "Reading\ncomprehension"
    )
  } else {
    stop("type must be either 'original' or 'reduced'.")
  }
  
  if (groups == "Group") {
    models$group_models |> 
      dplyr::filter(Variable %in% vars) |> 
      dplyr::select(!Group:Comparison) |> 
      Hmisc::latex(
        booktabs = TRUE, rowname = NULL, file = "", title = "", 
        insert.bottom = glue::glue("\\label{{{label}}}"), ...
      )
    
  } else if (groups == "Cluster") {
    models$cluster_models |> 
      dplyr::filter(Variable %in% vars) |> 
      dplyr::select(Comparison:tidyselect::last_col()) |> 
      Hmisc::latex(
        booktabs = TRUE, rowname = "", file = "", title = "", 
        insert.bottom = glue::glue("\\label{{{label}}}"), rgroup = vars, ...
      )
    
  } else if (groups == "Subcluster") {
    models$subcluster_models |> 
      dplyr::filter(Variable %in% vars) |> 
      dplyr::select(Comparison:tidyselect::last_col()) |> 
      Hmisc::latex(
        booktabs = TRUE, rowname = "", file = "", title = "", 
        insert.bottom = glue::glue("\\label{{{label}}}"), rgroup = vars, ...
      )
    
  } else {
    stop(
      "The 'groups' argument must be either 'Group', 'Cluster' or 'Subcluster'"
      )
  }
}

# Typically:
# get_latex_table(models, "Group",      type = "original")
# get_latex_table(models, "Cluster",    type = "reduced")
# get_latex_table(models, "Subcluster", type = "reduced")
