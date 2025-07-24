# Load required packages 
required_packages <- c(
  "targets", "tarchetypes", "here",
  "dplyr", "ggplot2"
)

new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if(length(new_packages)) install.packages(new_packages)

lapply(required_packages, library, character.only = TRUE)

# Source your functions (data loaders, utils)
source(here("R", "functions.R"))
source(here("R", "load_data.R"))
source(here("R", "utils.R"))
source(here("R", "task1.R"))  
source(here("R", "task2.R"))  
source(here("R", "task3.R"))
# Set target options (like packages used in targets)
tar_option_set(packages = required_packages)

# Define the targets pipeline
list(
  # Target 1: load pedestrian casualty data
  tar_target(
    casualty_classification,
    load_pedestrian_casualty_data()
  ),
  
  # Target 2: load fire rescue data
  tar_target(
    fire_rescue_analysis,
    load_fire_rescue_data()
  ),
  
  # Target 3: load olive oil data or similar
  tar_target(
    unsupervised_analysis,
    load_Olive_oil_data()
  ),
  
  # Target 4: render the R Markdown report
  tar_render(
    report,
    "vignettes/Report.Rmd"
  )
)

