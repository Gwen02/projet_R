library(tidyverse)

# Variables Width / Height pour les plots (en pouces)
w = 10
h = 10

# Ouverture du dataset
df <- read.csv(file="AB_NYC_2019.csv", sep=",")

# On regarde dans quels colonnes on trouve des NA, pour savoir dans quels traitements on doit les prendre en compte
names(which(colSums(is.na(df)) > 0))
# Seul review_per_month a des NA


#################################### PLOT 1 ####################################
# Dans quels arrondissements trouve t-on le plus de AirBNB
appt_par_arrond <- 
  df %>%
  count(neighbourhood_group) %>%
  rename("Nombre d\'appartements" = n, Arrondissements = neighbourhood_group)

# Affichage / Sauvegarde du Plot 1:
plot1 <- appt_par_arrond %>%
  ggplot(aes(x = Arrondissements, y=`Nombre d'appartements`, fill=Arrondissements)) + 
  geom_bar(stat="identity") + 
  scale_fill_manual(values = c("Bronx" = "#e2b420",
                                   "Brooklyn" = "#cb7329",
                                   "Manhattan" = "#728ea4",
                                   "Queens" = "#9da339",
                                   "Staten Island" = "#a9718a")) +
  ggtitle("Nombre de AirBnb par arrondissement")

ggsave(plot1, file="plots/appart_par_quartier.pdf", width = w, height = h)


#################################### PLOT 2 ####################################
# Quel est le prix / nuit moyen des appartements par arrondissements et par type d'appartement (chambre privée / chambre partagée / logement entier)
prix_par_arrond_type <-
  df %>%
  group_by(neighbourhood_group, room_type) %>%
  summarise("Prix moyen" = round(mean(price))) %>%
  rename(Arrondissements = neighbourhood_group, "Type de logement" = room_type)

plot2 <- prix_par_arrond_type %>%
  ggplot(aes(x = Arrondissements, y = `Prix moyen`, fill = `Type de logement`, label = `Prix moyen`)) +
  geom_bar(stat="identity", position = "dodge2") +
  coord_flip() +
  geom_text(size=3, position = position_dodge2(width = 0.9)) +
  ggtitle("Prix moyen des appartements par nuit, par arrondissement et par type") +
  labs(y="Prix moyen par nuit (en $)", x="Arrondissements")

ggsave(plot2, file="plots/prix_par_quartier_type.pdf", width = w, height = h)
 
#################################### PLOT 3 ####################################
# Quartiers les plus disponibles (top 6) par arrondissements

# On prend seulement les appartements de Manhattan
appt_manhattan <- df[df$neighbourhood_group == 'Manhattan',] 

# On compte le nombre d'appt par quartier
appt_par_quartier_man <- 
  appt_manhattan %>%
  group_by(neighbourhood) %>%
  count(neighbourhood)

# On garde les 6 plus peuplés
top6Manhattan = head(appt_par_quartier_man[order(appt_par_quartier_man$n, decreasing =  T),], 6)
appt_top6 <- appt_manhattan[appt_manhattan$neighbourhood %in% top6Manhattan$neighbourhood,]

# Disponibilité moyenne des 6 premiers quartiers de Manhattan
moyenne_dispo_quartier <- 
  appt_top6 %>%
  group_by(neighbourhood) %>%
  summarise(dispo_moyenne = mean(availability_365) / 3.65)


