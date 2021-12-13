library(tidyverse)

# Variables Width / Height pour les plots (en pouces)
w = 10
h = 10

# Ouverture du dataset
raw_df <- read.csv(file="AB_NYC_2019.csv", sep=",")

# On supprime les NA du dataframe
df <- raw_df %>% drop_na()

# Plot 1: Dans quel quartier trouve t-on le plus de AirBNB
appt_par_quartier <- 
  df %>%
  count(neighbourhood_group) %>%
  rename("Nombre d\'appartements" = n, Quartier = neighbourhood_group)

# Affichage / Sauvegarde du Plot 1:
plot1 <- appt_par_quartier %>%
  ggplot(aes(x = Quartier, y=`Nombre d'appartements`, fill=Quartier)) + 
  geom_bar(stat="identity") + 
  scale_fill_manual(values = c("Bronx" = "#e2b420",
                                   "Brooklyn" = "#cb7329",
                                   "Manhattan" = "#728ea4",
                                   "Queens" = "#9da339",
                                   "Staten Island" = "#a9718a"))

ggsave(plot1, file="plots/appart_par_quartier.pdf", width = w, height = h)


