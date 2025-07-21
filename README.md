


# MTHM053 - Applications of Data Science and Statistics

**University of Exeter**  
**Module Code**: MTHM053  
**Author**: Pooja Sajeevan K K  
**Repository**: https://github.com/IDK20IT032/mthm503



## Project Overview

This repository contains the data, code, and documentation for the MTHM053 coursework, which demonstrates the application of supervised and unsupervised machine learning to real-world datasets. The project is structured as a **fully reproducible R pipeline** using modern tools (`targets`, `renv`, `testthat`, Git). The main objectives are:

- **Supervised classification**: Predicting pedestrian-involved crash injury severity using features such as weather, lighting, age, sex, and location.
- **Regression modelling**: Analysing the effect of age and sex on the method of extrication used by the fire brigade in road traffic collisions.
- **Unsupervised learning**: Characterising natural variation in authentic Italian olive oil composition using principal component analysis and clustering.


---


## Getting Started

### Prerequisites

- **R** (≥ 4.5.1) and **RStudio** 
- **Git** (for version control)
- The **renv** R package for dependency management

### Installation

1. **Clone the repository:**
   ```
   git clone https://github.com/IDK20IT032/mthm503.git
   cd mthm503
   ```

2. **Restore the R environment:**
   Open the project in RStudio or your preferred IDE. Run the following in the R console to install all required packages at the correct versions:
   ```
   install.packages("renv")
   renv::restore()
   ```

3. **Run the pipeline:**
   Reproduce all analyses, figures, and the final report by executing:
   ```
   targets::tar_make()
   ```
   This will run the entire workflow defined in `_targets.R`, from data loading to report generation.

---

## Project Details

### Supervised Classification Task

**Objective**: Predict injury severity (Fatal, Serious, Slight) in pedestrian-involved collisions using accident and casualty characteristics.

**Key Steps**:
- Data merged from `stats19_casualties`, `stats19_accidents`, and `stats19_vehicles` tables.
- Two models compared: **multinomial logistic regression** and **random forest**.
- Performance evaluated by accuracy, per-class AUC, and confusion matrices.
- **Results**: Both models perform well, with random forest achieving slightly higher accuracy and better discrimination for the rare 'Fatal' class. Confusion matrices for each model are included in the report.

### Regression Task

**Objective**: Model the effect of casualty age and sex on the method of extrication used by fire services, adjusted for annual collision rates.

**Key Steps**:
- Data from `fire_rescue_extrication_casualties`, with police-reported collision totals for rate calculations.
- **Multinomial logistic regression** for extrication type probabilities by age and sex.
- **Negative binomial regression** for extrication rates (counts per collision).
- **Results**: Young males are at higher risk of requiring complex extrication. Negative binomial regression is preferred due to overdispersion in count data.

### Unsupervised Learning Task

**Objective**: Profile authentic Italian olive oils by their fatty acid composition, establishing a baseline for detecting adulteration.

**Key Steps**:
- Data from `olive_oil` table (eight fatty acids, 572 samples).
- **Exploratory analysis**: Correlation matrices, pairwise plots.
- **Principal component analysis (PCA)** for dimension reduction.
- **Clustering**: k-means and DBSCAN applied to reveal natural groupings.
- **Results**: Three main clusters identified, corresponding to distinct compositional profiles. Outliers flagged for further investigation.

---










