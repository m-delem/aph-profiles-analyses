
# Renv --------------------------------------------------------------------

# Don't forget to use `renv` for a reproducible environment!
# see https://rstudio.github.io/renv/articles/renv.html for more details

# install.packages("renv")  # if you don't have it yet
# library("renv")           # same as above

# renv::init() has already been used to create the renv.lock file (the file that
# contains the exact details of the packages you used) in this template, so now 
# the project only needs to be restored each time you start working. You will
# be asked to install the packages that I added down below upon running this 
# script, but you change this to suit your needs.
renv::restore()


# Packages ----------------------------------------------------------------

# pacman allows to check/install/load packages with a single call
# if (!require("pacman")) install.packages("pacman") # already in renv.lock
library("pacman")

# packages to load (and install if needed) -------------------------------
pacman::p_load(
  here,       # easy file paths
  glue,       # string interpolation
  see,        # theme_modern and okabeito palette
  report,     # reporting various info 
  labelled,   # labelled data
  # ─── data management ─────────────────
  fs,         # listing files
  readxl,     # reading excel files
  jsonlite,   # reading json files
  openxlsx,   # exporting xlsx files
  # ─── modelling ───────────────────────
  mclust,      # mixture clustering
  rstanarm,    # bayesian models
  BayesFactor, # BFs
  emmeans,     # marginal estimates  
  easystats,   # data analysis framework
  NbClust,     # number of clusters
  scales,      # data transformation
  #  data visualization ──────────────
  # plot types and geoms
  factoextra, # multivariate data plots
  ggradar,    # radar plots
  # layout and options
  ggtext,     # text in ggplot
  latex2exp,  # LaTeX in ggplot
  patchwork,  # layout control
  
  # Should remain last to avoid conflicts with other packages
  tidyverse   # modern R ecosystem
)

# The ggradar package comes from GitHub and needs special treatment
# if (!require("devtools")) install.packages("devtools")
# if (!require("ggradar")) devtools::install_github("ricardo-bion/ggradar")

# Custom functions shared across scripts ----------------------------------
source(here("scripts/_functions.R"))


# Global cosmetic theme ---------------------------------------------------

theme_set(theme_modern(base_size = 14)) # from see in easystats

# setting my favourite palettes as ggplot2 defaults
options( 
  ggplot2.discrete.colour   = scale_colour_okabeito,
  ggplot2.discrete.fill     = scale_fill_okabeito,
  ggplot2.continuous.colour = scale_colour_viridis_c,
  ggplot2.continuous.fill   = scale_fill_viridis_c
)


# Fixing a seed for reproducibility ---------------------------------------
set.seed(14051998)


# Adding all packages' citations to a .bib --------------------------------
knitr::write_bib(c(.packages()), file = here("bibliography/packages.bib"))
