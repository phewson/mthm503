library(targets)
library(tarchetypes)
library(here)

source(here("R", "functions.R"))
source(here("R", "load_data.R"))
source(here("R", "utils.R"))
source(here("R", "clean_extrications.R"))
source(here("R", "supervised_classification.R"))


tar_option_set(packages = c("dplyr", "DBI", "RPostgres"))

list(
  tar_target(
    raw_data,
    load_data()
  ),
  tar_target(
    summary_data,
    summarise_data(raw_data)
  ),
  tar_render(
    report,
    "vignettes/Report.Rmd"
  ),
  tar_target(
    storms_data,
    {storms <- nasaweather::storms}
  ),
  tar_target(
    storms_by_storm_type,
    simple_count(storms)
  ),
  tar_target(
    plot_storms,
    plot_storm_data(storms_data)
  ),
  tar_target(
    storms_part,
    prepare_split(storms_data)
  ),
  tar_target(
    storms_train, 
    get_train(storms_part)
  ),
  tar_target(
    storms_test, 
    get_test(storms_part)
  ),
  tar_target(
    storms_classifier,
    fit_rpart(storms_train, 0.01),
  ),
  tar_target(
    plot_classifier,
    plot_rpart(storms_classifier)
  ),
  tar_target(
    storm_preds, 
    make_preds(storms_test, storms_classifier)
  ),
  tar_target(
    roc_hurricane,
    make_roc_curve(storm_preds, "Hurricane")
  ),
  tar_target(
    roc_depression,
    make_roc_curve(storm_preds, "Tropical Depression")
  ),
  tar_target(
    roc_tropical_depression,
    make_roc_curve(storm_preds, "Tropical Storm")
  ),
  tar_target(
    roc_extra_tropical,
    make_roc_curve(storm_preds, "Extratropical")
  ),
  tar_target(
    roc_all_aucs,
    roc_auc(storm_preds, truth = type, starts_with(".pred_"))
  )
)