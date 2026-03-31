library(dplyr)
library(ggplot2)
library(stringr)

get_script_dir <- function() {
  command_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", command_args, value = TRUE)

  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[1]))))
  }

  current_frame <- sys.frames()[[1]]
  if (!is.null(current_frame$ofile)) {
    return(dirname(normalizePath(current_frame$ofile)))
  }

  normalizePath(getwd())
}

script_dir <- get_script_dir()
repo_dir <- normalizePath(file.path(script_dir, "..", ".."), mustWork = FALSE)
raw_file <- file.path(repo_dir, "week11-project", "data", "raw", "Tabular_DS_Jobs.csv")
output_dir <- file.path(repo_dir, "big-task-learning", "outputs", "my-practice-run")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Step 1: Load the raw dataset.
jobs_raw <- read.csv(raw_file, stringsAsFactors = FALSE)

# Step 2: Write your own research question and hypotheses.
research_question <- "Write your own question here"
null_hypothesis <- "Write your null hypothesis here"
alternative_hypothesis <- "Write your alternative hypothesis here"

# Step 3: Clean the data.
# TODO:
# - standardise column names
# - remove duplicates
# - trim whitespace
# - convert placeholders to NA
# - create any helper variables you need

# Step 4: Visual exploration.
# TODO:
# - create at least two useful plots
# - save them into output_dir with ggsave()

# Step 5: Hypothesis test.
# TODO:
# - choose one clear hypothesis
# - justify the test
# - run it
# - record the interpretation

# Step 6: Linear regression.
# TODO:
# - choose a continuous outcome
# - choose sensible predictors
# - fit the model
# - inspect the summary and diagnostics

# Step 7: Classification.
# TODO:
# - choose a binary target
# - create a train/test split
# - fit a simple classifier
# - calculate confusion matrix and metrics

# Step 8: Clustering.
# TODO:
# - choose numeric features
# - scale them
# - choose k
# - explain what the clusters mean

# Step 9: Reflection.
# TODO:
# - note assumptions
# - note limitations
# - note potential improvements
