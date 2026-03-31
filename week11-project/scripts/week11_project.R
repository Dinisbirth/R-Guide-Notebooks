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
project_dir <- normalizePath(file.path(script_dir, ".."), mustWork = FALSE)
input_file <- file.path(project_dir, "data", "raw", "Tabular_DS_Jobs.csv")
output_file <- file.path(project_dir, "data", "processed", "Jobs_clean.csv")
output_dir <- file.path(project_dir, "outputs")

if (!file.exists(input_file)) {
  stop("Required raw dataset was not found at week11-project/data/raw/Tabular_DS_Jobs.csv.")
}

dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

clean_names <- function(x) {
  x <- str_to_lower(str_trim(x))
  x <- str_replace_all(x, "[^a-z0-9]+", "_")
  x <- str_replace_all(x, "^_|_$", "")
  x
}

safe_divide <- function(numerator, denominator) {
  if (denominator == 0) {
    return(NA_real_)
  }
  numerator / denominator
}

Jobs <- read.csv(input_file, stringsAsFactors = FALSE)

JobsClean <- Jobs %>%
  rename_with(clean_names)

duplicate_rows <- sum(duplicated(JobsClean))

JobsClean <- JobsClean %>%
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

categorical_columns <- c(
  "headquarters",
  "size",
  "type_of_ownership",
  "industry",
  "sector",
  "revenue",
  "job_simp",
  "seniority"
)

for (column_name in categorical_columns) {
  JobsClean[[column_name]][is.na(JobsClean[[column_name]])] <- "Unknown"
}

JobsClean <- JobsClean %>%
  mutate(
    python_required = factor(if_else(python == 1, "Yes", "No"), levels = c("No", "Yes")),
    senior_role = factor(if_else(seniority == "senior", "Senior", "Not Senior")),
    is_data_scientist = if_else(job_simp == "data scientist", 1L, 0L)
  )

salary_q1 <- quantile(JobsClean$avg_salary, 0.25, na.rm = TRUE)
salary_q3 <- quantile(JobsClean$avg_salary, 0.75, na.rm = TRUE)
salary_iqr <- salary_q3 - salary_q1
salary_lower <- salary_q1 - 1.5 * salary_iqr
salary_upper <- salary_q3 + 1.5 * salary_iqr

JobsClean <- JobsClean %>%
  mutate(
    avg_salary_outlier_flag = if_else(
      avg_salary < salary_lower | avg_salary > salary_upper,
      TRUE,
      FALSE,
      missing = FALSE
    )
  )

write.csv(JobsClean, output_file, row.names = FALSE)

# Descriptive visuals
salary_histogram <- ggplot(JobsClean, aes(x = avg_salary)) +
  geom_histogram(binwidth = 10, fill = "steelblue", colour = "black") +
  labs(
    title = "Average Salary Distribution",
    x = "Average salary",
    y = "Count"
  ) +
  theme_minimal()

ggsave(
  filename = file.path(output_dir, "salary_distribution.png"),
  plot = salary_histogram,
  width = 8,
  height = 5
)

# Hypothesis test: jobs requiring Python have different mean salaries.
hypothesis_plot <- ggplot(JobsClean, aes(x = python_required, y = avg_salary, fill = python_required)) +
  geom_boxplot(alpha = 0.8, show.legend = FALSE) +
  labs(
    title = "Average Salary by Python Requirement",
    x = "Python required",
    y = "Average salary"
  ) +
  theme_minimal()

ggsave(
  filename = file.path(output_dir, "hypothesis_python_salary_boxplot.png"),
  plot = hypothesis_plot,
  width = 8,
  height = 5
)

salary_python_test <- t.test(avg_salary ~ python_required, data = JobsClean)

# Linear regression: explain average salary using ratings, experience proxy, skills, and job type.
regression_data <- JobsClean %>%
  filter(job_simp != "Unknown") %>%
  transmute(
    avg_salary,
    rating_clean,
    company_age,
    python,
    spark,
    aws,
    tableau,
    big_data,
    senior_role,
    job_simp = factor(job_simp)
  ) %>%
  na.omit()

lm_model <- lm(
  avg_salary ~ rating_clean + company_age + python + spark + aws + tableau + big_data + senior_role + job_simp,
  data = regression_data
)

regression_data <- regression_data %>%
  mutate(
    fitted_salary = predict(lm_model),
    residuals = residuals(lm_model)
  )

regression_plot <- ggplot(regression_data, aes(x = fitted_salary, y = avg_salary)) +
  geom_point(alpha = 0.6, colour = "darkred") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", colour = "black") +
  labs(
    title = "Linear Regression: Actual vs Fitted Salary",
    x = "Fitted salary",
    y = "Actual salary"
  ) +
  theme_minimal()

ggsave(
  filename = file.path(output_dir, "regression_actual_vs_fitted.png"),
  plot = regression_plot,
  width = 8,
  height = 5
)

png(file.path(output_dir, "regression_diagnostics.png"), width = 1200, height = 1200)
par(mfrow = c(2, 2))
plot(lm_model)
dev.off()
par(mfrow = c(1, 1))

# Classification: predict whether a role is a data scientist role.
classification_data <- JobsClean %>%
  filter(job_simp != "Unknown") %>%
  transmute(
    is_data_scientist,
    avg_salary,
    rating_clean,
    company_age,
    python,
    excel,
    hadoop,
    spark,
    aws,
    tableau,
    big_data,
    senior_role
  ) %>%
  na.omit()

set.seed(7202)
train_index <- sample(seq_len(nrow(classification_data)), size = floor(0.7 * nrow(classification_data)))

train_data <- classification_data[train_index, ]
test_data <- classification_data[-train_index, ]

classification_model <- glm(
  is_data_scientist ~ avg_salary + rating_clean + company_age + python + excel + hadoop + spark + aws + tableau + big_data + senior_role,
  data = train_data,
  family = binomial()
)

test_data$predicted_probability <- predict(classification_model, newdata = test_data, type = "response")
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

confusion_plot_data <- as.data.frame(confusion_matrix)

confusion_plot <- ggplot(confusion_plot_data, aes(x = Predicted, y = Actual, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), colour = "white", size = 5) +
  scale_fill_gradient(low = "#9ecae1", high = "#08519c") +
  labs(
    title = "Classification Confusion Matrix",
    x = "Predicted class",
    y = "Actual class"
  ) +
  theme_minimal()

ggsave(
  filename = file.path(output_dir, "classification_confusion_matrix.png"),
  plot = confusion_plot,
  width = 7,
  height = 5
)

# Clustering: group jobs by salary, rating, company age, and skills.
clustering_data <- JobsClean %>%
  transmute(
    avg_salary,
    rating_clean,
    company_age,
    python,
    excel,
    hadoop,
    spark,
    aws,
    tableau,
    big_data
  )

clustering_data$rating_clean[is.na(clustering_data$rating_clean)] <- median(clustering_data$rating_clean, na.rm = TRUE)
clustering_data$company_age[is.na(clustering_data$company_age)] <- median(clustering_data$company_age, na.rm = TRUE)

scaled_clustering_data <- scale(clustering_data)

set.seed(7202)
wss_data <- data.frame(
  k = 1:6,
  tot_withinss = sapply(1:6, function(k_value) {
    kmeans(scaled_clustering_data, centers = k_value, nstart = 20)$tot.withinss
  })
)

elbow_plot <- ggplot(wss_data, aes(x = k, y = tot_withinss)) +
  geom_line(colour = "darkgreen") +
  geom_point(size = 2, colour = "darkgreen") +
  labs(
    title = "Elbow Plot for K-means",
    x = "Number of clusters",
    y = "Total within-cluster sum of squares"
  ) +
  theme_minimal()

ggsave(
  filename = file.path(output_dir, "clustering_elbow_plot.png"),
  plot = elbow_plot,
  width = 7,
  height = 5
)

cluster_model <- kmeans(scaled_clustering_data, centers = 3, nstart = 25)

JobsClustered <- JobsClean %>%
  mutate(cluster = factor(cluster_model$cluster))

cluster_summary <- JobsClustered %>%
  group_by(cluster) %>%
  summarise(
    job_count = n(),
    mean_avg_salary = mean(avg_salary, na.rm = TRUE),
    mean_rating = mean(rating_clean, na.rm = TRUE),
    mean_company_age = mean(company_age, na.rm = TRUE),
    python_share = mean(python, na.rm = TRUE),
    spark_share = mean(spark, na.rm = TRUE),
    aws_share = mean(aws, na.rm = TRUE),
    .groups = "drop"
  )

cluster_job_mix <- JobsClustered %>%
  count(cluster, job_simp, name = "n") %>%
  group_by(cluster) %>%
  slice_max(order_by = n, n = 3, with_ties = FALSE) %>%
  ungroup()

pca_results <- prcomp(scaled_clustering_data)

cluster_plot_data <- data.frame(
  PC1 = pca_results$x[, 1],
  PC2 = pca_results$x[, 2],
  cluster = factor(cluster_model$cluster)
)

cluster_plot <- ggplot(cluster_plot_data, aes(x = PC1, y = PC2, colour = cluster)) +
  geom_point(alpha = 0.7, size = 2) +
  labs(
    title = "K-means Clusters Projected onto Principal Components",
    x = "Principal component 1",
    y = "Principal component 2"
  ) +
  theme_minimal()

ggsave(
  filename = file.path(output_dir, "clustering_pca_plot.png"),
  plot = cluster_plot,
  width = 8,
  height = 5
)

summary_file <- file.path(output_dir, "week11_results_summary.txt")

capture.output(
  {
    cat("Week 11 Mini Project Summary\n")
    cat("============================\n\n")
    cat("Rows in raw data:", nrow(Jobs), "\n")
    cat("Rows after cleaning:", nrow(JobsClean), "\n")
    cat("Duplicate rows removed:", duplicate_rows, "\n")
    cat("Saved cleaned data to:", normalizePath(output_file, mustWork = FALSE), "\n\n")

    cat("Hypothesis test\n")
    cat("---------------\n")
    cat("Question: Do jobs requiring Python have a different mean salary from jobs that do not require Python?\n")
    print(salary_python_test)
    cat("\n")

    cat("Linear regression\n")
    cat("-----------------\n")
    print(summary(lm_model))
    cat("\n")

    cat("Classification\n")
    cat("--------------\n")
    print(confusion_matrix)
    cat("\n")
    print(classification_metrics)
    cat("\n")

    cat("Clustering\n")
    cat("----------\n")
    print(cluster_summary)
    cat("\nTop job types per cluster\n")
    print(cluster_job_mix)
  },
  file = summary_file
)

cat("Mini-project workflow completed.\n")
cat("Cleaned dataset saved to", normalizePath(output_file, mustWork = FALSE), "\n")
cat("Plots and summary outputs saved in", normalizePath(output_dir, mustWork = FALSE), "\n")
