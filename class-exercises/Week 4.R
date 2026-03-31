library(dplyr)
library(ggplot2)
library(palmerpenguins)
library(psych)

data("penguins")

head(penguins)

#filter by columns

Peng <- penguins %>%
  select(species, island, sex, body_mass_g, flipper_length_mm)

head(Peng)

# filter by sepefic raw , befor do that we need to standerise 
female_peng <- penguins %>%
  filter(sex == "female")
head(female_peng)

# we can do this as well
female_peng <- penguins %>%
  filter(sex == "female",
             body_mass_g > 5000)
head(female_peng)

female_peng <- penguins %>%
  filter(sex == "female" & flipper_length_mm <= 180)
head(female_peng)



table(penguins)

table (Peng$species)

#propotion if we want to find out corleation 
prop.table (table(Peng$species))


table(Peng$species, Peng$sex)
prop.table(table(Peng$species, Peng$sex))

table(Peng$species, Peng$sex, Peng$island)

range(Peng$body_mass_g, na.rm = TRUE)
mean(Peng$body_mass_g, na.rm = TRUE)
sd(Peng$body_mass_g, na.rm = TRUE)
IQR(Peng$body_mass_g, na.rm = TRUE)

ggplot(Peng, aes(x = body_mass_g)) +
  geom_histogram(bins = 30)

describe (penguins[, c("body_mass_g",
               "flipper_length_mm",
               "bill_length_mm")])


penguins %>%
  group_by(species) %>%
  summarise(
    mean_mass = mean(body_mass_g, na.rm = TRUE),
    sd_mass = sd(body_mass_g, na.rm = TRUE),
    n = n()
  )

ggplot(penguins, aes(x = species, y = body_mass_g)) +
  geom_boxplot()

cor(penguins$body_mass_g, 
    penguins$flipper_length_mm, 
    use = "complete.obs")   

ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
