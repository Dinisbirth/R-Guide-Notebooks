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

safe_divide <- function(numerator, denominator) {
  if (denominator == 0) {
    return(NA_real_)
  }
  numerator / denominator
}

clean_names <- function(x) {
  x <- str_to_lower(str_trim(x))
  x <- str_replace_all(x, "[^a-z0-9]+", "_")
  x <- str_replace_all(x, "^_|_$", "")
  x
}

script_dir <- get_script_dir()
repo_dir <- normalizePath(file.path(script_dir, "..", ".."), mustWork = FALSE)
raw_file <- file.path(repo_dir, "week11-project", "data", "raw", "Tabular_DS_Jobs.csv")
output_dir <- file.path(repo_dir, "big-task-learning", "outputs")
clean_output_file <- file.path(output_dir, "training_jobs_clean.csv")
summary_file <- file.path(output_dir, "big_task_training_summary.txt")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(raw_file)) {
  stop("The training script expected the raw dataset at week11-project/data/raw/Tabular_DS_Jobs.csv.")
}

# Step 1: Load and inspect the raw data.
jobs_raw <- read.csv(raw_file, stringsAsFactors = FALSE)

# Step 2: Clean the data in a simple, teachable way.
jobs_clean <- jobs_raw %>%
  rename_with(clean_names) %>%
  distinct() %>%
  mutate(across(where(is.character), str_trim)) %>%
  mutate(across(where(is.character), ~ na_if(.x, "-1"))) %>%
  mutate(across(where(is.numeric), ~ na_if(.x, -1))) %>%
  mutate(
    job_state = str_trim(job_state),
    seniority = if_else(str_to_lower(coalesce(seniority, "")) == "na", NA_character_, seniority),
    job_simp = if_else(str_to_lower(coalesce(job_simp, "")) == "na", NA_character_, job_simp),
    rating_clean = if_else(rating == 0, NA_real_, rating)
  )

for (column_name in c("industry", "sector", "job_simp", "seniority")) {
  jobs_clean[[column_name]][is.na(jobs_clean[[column_name]])] <- "Unknown"
}

jobs_clean <- jobs_clean %>%
  mutate(
    python_required = factor(if_else(python == 1, "Yes", "No"), levels = c("No", "Yes")),
    senior_role = factor(if_else(seniority == "senior", "Senior", "Not Senior")),
    is_data_scientist = if_else(job_simp == "data scientist", 1L, 0L)
  )

write.csv(jobs_clean, clean_output_file, row.names = FALSE)

# Step 3: Create one simple visual for the distribution.
salary_plot <- ggplot(jobs_clean, aes(x = avg_salary)) +
  geom_histogram(binwidth = 10, fill = "steelblue", colour = "black") +
  labs(
    title = "Training Plot: Average Salary Distribution",
    x = "Average salary",
    y = "Count"
  ) +
  theme_minimal()

ggsave(
  filename = file.path(output_dir, "training_salary_histogram.png"),
  plot = salary_plot,
  width = 8,
  height = 5
)

# Step 4: Hypothesis test.
python_test <- t.test(avg_salary ~ python_required, data = jobs_clean)

python_boxplot <- ggplot(jobs_clean, aes(x = python_required, y = avg_salary, fill = python_required)) +
  geom_boxplot(show.legend = FALSE) +
  labs(
    title = "Training Plot: Salary by Python Requirement",
    x = "Python required",
    y = "Average salary"
  ) +
  theme_minimal()

ggsave(
  filename = file.path(output_dir, "training_python_boxplot.png"),
  plot = python_boxplot,
  width = 8,
  height = 5
)

# Step 5: Linear regression.
regression_data <- jobs_clean %>%
  transmute(
    avg_salary,
    rating_clean,
    company_age,
    python,
    aws,
    spark,
    tableau,
    senior_role
  ) %>%
  na.omit()

salary_model <- lm(
  avg_salary ~ rating_clean + company_age + python + aws + spark + tableau + senior_role,
  data = regression_data
)

# Step 6: Classification.
classification_data <- jobs_clean %>%
  transmute(
    is_data_scientist,
    avg_salary,
    rating_clean,
    company_age,
    python,
    excel,
    spark,
    aws,
    senior_role
  ) %>%
  na.omit()

set.seed(7202)
train_index <- sample(seq_len(nrow(classification_data)), size = floor(0.7 * nrow(classification_data)))
train_data <- classification_data[train_index, ]
test_data <- classification_data[-train_index, ]

role_model <- glm(
  is_data_scientist ~ avg_salary + rating_clean + company_age + python + excel + spark + aws + senior_role,
  data = train_data,
  family = binomial()
)

test_data$predicted_probability <- predict(role_model, newdata = test_data, type = "response")
test_data$predicted_class <- factor(
  if_else(test_data$predicted_probability >= 0.5, "Data Scientist", "Other Role"),
  levels = c("Other Role", "Data Scientist")
)
test_data$actual_class <- factor(
  if_else(test_data$is_data_scientist == 1, "Data Scientist", "Other Role"),
  levels = c("Other Role", "Data Scientist")
)

confusion_matrix <- table(Actual = test_data$actual_class, Predicted = test_data$predicted_class)

true_negative <- as.numeric(confusion_matrix["Other Role", "Other Role"])
false_positive <- as.numeric(confusion_matrix["Other Role", "Data Scientist"])
false_negative <- as.numeric(confusion_matrix["Data Scientist", "Other Role"])
true_positive <- as.numeric(confusion_matrix["Data Scientist", "Data Scientist"])

classification_metrics <- data.frame(
  metric = c("accuracy", "precision", "recall", "specificity", "f1_score"),
  value = c(
    safe_divide(true_positive + true_negative, sum(confusion_matrix)),
    safe_divide(true_positive, true_positive + false_positive),
    safe_divide(true_positive, true_positive + false_negative),
    safe_divide(true_negative, true_negative + false_positive),
    safe_divide(2 * true_positive, (2 * true_positive) + false_positive + false_negative)
  )
)

# Step 7: Clustering.
cluster_data <- jobs_clean %>%
  transmute(
    avg_salary,
    rating_clean,
    company_age,
    python,
    excel,
    spark,
    aws
  )

cluster_data$rating_clean[is.na(cluster_data$rating_clean)] <- median(cluster_data$rating_clean, na.rm = TRUE)
cluster_data$company_age[is.na(cluster_data$company_age)] <- median(cluster_data$company_age, na.rm = TRUE)

scaled_cluster_data <- scale(cluster_data)

set.seed(7202)
training_clusters <- kmeans(scaled_cluster_data, centers = 3, nstart = 25)

pca_results <- prcomp(scaled_cluster_data)
cluster_plot_data <- data.frame(
  PC1 = pca_results$x[, 1],
  PC2 = pca_results$x[, 2],
  cluster = factor(training_clusters$cluster)
)

cluster_plot <- ggplot(cluster_plot_data, aes(x = PC1, y = PC2, colour = cluster)) +
  geom_point(alpha = 0.7, size = 2) +
  labs(
    title = "Training Plot: Cluster Map",
    x = "Principal component 1",
    y = "Principal component 2"
  ) +
  theme_minimal()

ggsave(
  filename = file.path(output_dir, "training_cluster_plot.png"),
  plot = cluster_plot,
  width = 8,
  height = 5
)

cluster_summary <- jobs_clean %>%
  mutate(cluster = factor(training_clusters$cluster)) %>%
  group_by(cluster) %>%
  summarise(
    jobs = n(),
    mean_salary = mean(avg_salary, na.rm = TRUE),
    mean_rating = mean(rating_clean, na.rm = TRUE),
    python_share = mean(python, na.rm = TRUE),
    .groups = "drop"
  )

capture.output(
  {
    cat("Big Task Learning Summary\n")
    cat("=========================\n\n")
    cat("Rows in raw data:", nrow(jobs_raw), "\n")
    cat("Rows after cleaning:", nrow(jobs_clean), "\n\n")

    cat("Hypothesis test\n")
    cat("---------------\n")
    print(python_test)
    cat("\n")

    cat("Linear regression\n")
    cat("-----------------\n")
    print(summary(salary_model))
    cat("\n")

    cat("Classification metrics\n")
    cat("----------------------\n")
    print(confusion_matrix)
    cat("\n")
    print(classification_metrics)
    cat("\n")

    cat("Cluster summary\n")
    cat("---------------\n")
    print(cluster_summary)
  },
  file = summary_file
)

cat("Training walkthrough completed.\n")
cat("Outputs saved in", normalizePath(output_dir, mustWork = FALSE), "\n")
