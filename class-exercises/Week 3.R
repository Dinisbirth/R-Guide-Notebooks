library(dplyr)
library(stringr)
library(ggplot2)

if (!file.exists("student_info.csv") || !file.exists("student_result.csv")) {
  stop("Required files 'student_info.csv' and 'student_result.csv' were not found in this folder.")
}

students_og <- read.csv("student_info.csv", stringsAsFactors = FALSE)
result_og <- read.csv("student_result.csv", stringsAsFactors = FALSE)

glimpse(students_og)
glimpse(result_og)

#format merge point of datasets

students <- students_og %>% #this is same as colon
  rename_with( ~ str_trim (str_to_lower(.x)))
  #rename_with():
  
results <- result_og %>%
    rename_with( ~ str_trim (str_to_lower(.x)))

names(students)
names(results)

#Prep column name for merge with each other

students <- students %>%
  rename(student_id = studentid)

students <- students %>%
  mutate(student_id = str_to_upper(as.character(student_id)))

results <- results %>%
  mutate(student_id = str_to_upper(as.character(student_id)))

head(students$student_id)
head(results$student_id)

#The actual merge

merged <- students %>%
  left_join(results, by = "student_id")

glimpse(merged)

#standeris

merged <- merged %>%
  mutate(
    age = as.numeric(age), 
    final_mark = as.numeric(final_mark)
  )

glimpse(merged$age)


merged <- merged %>%
  mutate (
    name = str_trim(name),
    gender = str_to_lower(str_trim(gender)),
    programme = str_to_lower(str_trim(programme)),
    programme = str_replace_all(programme, "-", " ")
  )

glimpse(merged$programme)

# work nicely with categorical data
merged <- merged %>%
  mutate(
    gender = case_when(
      gender == "m" ~ "male",
      gender == "f" ~ "female",
      TRUE ~ gender 
    )
  )


merged <- merged %>%
  mutate(
    final_mark = if_else(is.na (final_mark), 0, final_mark)
  )

#merged <- merged %>%
  #mutate (
    #final_mark = is.na(final_mark)
  #)

ggplot(merged, aes(x = final_mark)) +
  geom_histogram(binwidth = 5,
                 fill = "steelblue",
                 colour = "black"
                 ) +
  labs(
    title = "This is a test title",
    x = "The x axis",
    y = "The y axis"
  ) +
  theme_minimal()# this is good for large plot


 ggplot(data = merged, aes(x = gender)) +
   geom_bar(
     fill = "darkred"
   )
 
 ggplot (data = merged, aes(x = programme)) +
    geom_bar(
      fill = "seagreen"
    )
    
ggplot(data = merged, aes (x = age, y = final_mark)) +
    geom_point(
      fill = "orange",
      alpha = 0.6
    )
