library(dplyr)
library(stringr)
library(ggplot2)

if (!file.exists("tours.csv")) {
  stop("Required file 'tours.csv' was not found in this folder.")
}

toursDataSet <- read.csv("tours.csv", stringsAsFactors = FALSE)

glimpse(toursDataSet)

#format the data set, is there any other way to rename columns at once
tours <- toursDataSet %>%
  rename_with(~ str_trim(str_to_lower(.x))) %>%
  rename(
    all_time_peak = all.time.peak,
    actual_gross = actual.gross,
    adjusted_gross_2022_dollars = adjusted.gross..in.2022.dollars.,
    tour_title = tour.title,
    years = year.s.,
    average_gross = average.gross,
    ref = ref.
  )

head(tours)

#Standardises values
standardise_tours <- tours %>%
  mutate(
    peak = str_remove_all(as.character(peak), "[\\[\\]†‡]"),
    all_time_peak = suppressWarnings(as.numeric(str_remove_all(as.character(all_time_peak), ","))),
    years = str_trim(str_replace_all(as.character(years), "-", " - ")),
    artist = str_to_lower(str_trim(artist)),
    tour_title = str_to_lower(str_trim(tour_title)),
    tour_title = str_remove_all(tour_title, "[†‡]")
  )

glimpse(standardise_tours)

#Fill missing values
standardise_tours <- standardise_tours %>%
  filter(!is.na(all_time_peak))

glimpse(standardise_tours$all_time_peak)
