library(rmarkdown)
library(readr)

# Load the CSV
marks <- read_csv(here::here("vignettes", "marks.csv"))

# Loop through each row
for (i in 1:nrow(marks)) {
  student <- marks[i, ]
  
  render(here::here("vignettes", "feedback_and_marking.Rmd"),
         params = list(
           student_number = student$student_number,
           supervised_score = student$supervised_score,
           supervised_comment = student$supervised_comment,
           supervised_grade = student$supervised_grade,
           regression_score = student$regression_score,
           regression_comment = student$regression_comment,
           regression_grade = student$regression_grade,
           unsupervised_score = student$unsupervised_score,
           unsupervised_comment = student$unsupervised_comment,
           unsupervised_grade = student$unsupervised_grade,
           repro_score = student$repro_score,
           repro_comment = student$repro_comment,
           repro_grade = student$repro_grade
         ),
         output_file = paste0("feedback_", gsub(" ", "_", student$student_number), ".html")
  )
}