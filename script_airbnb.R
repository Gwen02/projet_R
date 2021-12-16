#install.packages("tidyverse")
#install.packages("fmsb")
#install.packages("leaflet")
#install.packages("maps")
library(tidyverse)
library(fmsb)
library(leaflet)
library(maps)

# Variables Width / Height pour les plots (en pouces)
w = 10
h = 10

# Ouverture du dataset
df <- read.csv(file="AB_NYC_2019.csv", sep=",")

# On regarde dans quels colonnes on trouve des NA, pour savoir dans quels traitements on doit les prendre en compte
names(which(colSums(is.na(df)) > 0))
# Seul review_per_month a des NA

arrond_colors = c("Bronx" = "#e2b420",
  "Brooklyn" = "#cb7329",
  "Manhattan" = "#728ea4",
  "Queens" = "#9da339",
  "Staten Island" = "#a9718a")

#################################### PLOT 1 ####################################
# Dans quels arrondissements trouve t-on le plus de AirBNB
appt_par_arrond <- 
  df %>%
  count(neighbourhood_group) %>%
  rename("Nombre d\'appartements" = n, Arrondissements = neighbourhood_group)

# Affichage / Sauvegarde du Plot 1:
plot1 <- appt_par_arrond %>%
  ggplot(aes(x = reorder(Arrondissements, `Nombre d'appartements`), y=`Nombre d'appartements`, fill=Arrondissements)) + 
  geom_bar(stat="identity") + 
  scale_fill_manual(values = arrond_colors) +
  ggtitle("Nombre de AirBnb par arrondissement") +
  labs(x="Arrondissements") +
  theme(text = element_text(size = 20))

ggsave(plot1, file="plots/appart_par_quartier.pdf", width = w, height = h/2)


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

# On récupère les catégories
categories <- unique(df$neighbourhood_group)
colors <- c("#cb7329", "#728ea4", "#9da339", "#a9718a", "#e2b420")
# iterateur sur les couleurs
i <- 1
# Pour chaque arrondissement, on fait un plot
for(cat in categories){
  if(i == 0){
    i <- 1
  }
  # On prend seulement les appartements de l'arrondissement
  appt_arr <- df[df$neighbourhood_group == cat,] 
  
  # On compte le nombre d'appt par quartier
  appt_par_quartier_arr <- 
    appt_arr %>%
    group_by(neighbourhood) %>%
    count(neighbourhood)
  
  # On garde les 6 plus peuplés
  top6_arr = head(appt_par_quartier_arr[order(appt_par_quartier_arr$n, decreasing =  T),], 6)
  appt_top6 <- appt_arr[appt_arr$neighbourhood %in% top6_arr$neighbourhood,]
  
  # Disponibilité moyenne des 6 premiers quartiers de l'arrondissement
  moyenne_dispo_quartier <- 
    appt_top6 %>%
    group_by(neighbourhood) %>%
    summarise(dispo_moyenne = round(mean(availability_365) / 3.65))
  
  # On transpose le df pour avoir les noms de quartier en nom de colonnes,  puis une ligne des valeurs de dispo moyennes
  neigh_names = moyenne_dispo_quartier$neighbourhood
  moyenne_dispo_quartier_transpose = as.data.frame(t(moyenne_dispo_quartier[, -1]))
  colnames(moyenne_dispo_quartier_transpose) <- neigh_names
  # On ajoute le min / max pour chaque valeur pour pouvoir utiliser le radarchart
  moyenne_dispo_quartier_transpose <- rbind(rep(100,1) , rep(0,6) , moyenne_dispo_quartier_transpose)
  
  # Création du radar chart
  name = paste("plots/radar_dispo_moyenne/radar_disp_moyenne_", cat, ".pdf", sep="")
  pdf(name, width = 1.4*w, height = 1.4*h)
  radarchart(moyenne_dispo_quartier_transpose,
             axistype = 1,
             pcol=paste(colors[i], "AF", sep=""),
             pfcol=paste(colors[i], "AF", sep=""),
             plwd=3,
             axislabcol="#878787",
             cglwd=1,
             vlcex=2.5,
             calcex = 2.8,
             title=paste("Disponibilitées à ", cat, "", " (% de l'année)", sep=""),
             cex.main = 3
             )
  dev.off()
  i <- (i + 1)%%6
}

#################################### PLOT 4 ####################################
# Carte des 20 AirBNB les moins chères (avec prix) dans le quartier de Midtown (Empire State Building), où le nombre minimum de nuits est 
# <= 2, avec une diusponibilité d'au moin 30 jours dans l'année, et un prix d'au maximum 100$ / nuit

# On selectionne les airbnb de midtown, puis les 20 moins chères
midtown_Top20 <-
  df %>%
  filter(neighbourhood == "Midtown") %>%
  filter(minimum_nights <= 2) %>%
  filter(price <= 100) %>%
  filter(availability_365 >= 30) %>%
  arrange(price) %>%
  head(20)

# Ajout image du Pin
custom_pin = makeIcon(
  iconUrl = "icon/custom_pin.png",
  iconWidth = 30, iconHeight = 30,
  iconAnchorX = 0, iconAnchorY = 0
)

# Creation de la carte
map <- leaflet() %>% setView(lng = -73.985428, lat = 40.748817, zoom = 15.45)


map <- map %>%
  # Ajout d'un marker Empire State Building
  addMarkers(lng = -73.985428, lat = 40.748817,
                     label = "Empire State Building",
                     icon = custom_pin,
                     labelOptions = labelOptions(noHide = T,
                                                 style = list(
                                                   "color" = "red",
                                                   "font-size" = "15px"
                                                 )),
  ) %>%
  # Ajout d'un marker Rockfeller Center
  addMarkers(lng = -73.978798, lat = 40.758678,
             label = "Rockefeller Center",
             icon = custom_pin,
             labelOptions = labelOptions(noHide = T,
                                         style = list(
                                           "color" = "red",
                                           "font-size" = "15px"
                                         )),
  ) %>%
  # Ajout des markers (Latitude / longitude des logements)
  addMarkers(lng = midtown_Top20$longitude, lat =  midtown_Top20$latitude,
             label = paste(midtown_Top20$price, "$"),
             icon = custom_pin,
             labelOptions = labelOptions(noHide = T,
                                         style = list(
                                           "color" = "green",
                                           "font-size" = "12px"
                                         )),
             )

map %>% addTiles()

# Pour sauvegarder la carte, on prend un screenshot (car c'est une view et non un plot, donc pas de sauvegarde en pdf possible nativement)

#################################### PLOT 5 ####################################

# Top des 5 des hôtes par arrondissement
rank_hostnames <- 
  df %>%
  group_by(neighbourhood_group) %>%
  count(host_name) %>%
  rename(Nombre = n, Arrondissement = neighbourhood_group) %>%
  mutate(Rank = rank(-Nombre, ties.method = "random")) %>%
  subset(Rank <= 5) %>%
  arrange(Arrondissement, -Nombre)

# Plot des 5 premiers host_names / arrondissement pour chaque arrondissement

plot5 <-
  rank_hostnames %>%
  ggplot(aes(x = reorder(host_name, -Rank), y=Nombre, fill=Arrondissement)) +
  geom_bar(stat="identity") +
  coord_flip() +
  facet_wrap("Arrondissement", scales = "free") +
  scale_fill_manual(values = arrond_colors) + 
  labs(x="Nom d'hote (ou d'entreprise)", y="Occurence") + 
  ggtitle("Top 5 des noms d'hôte (ou entreprise) par arrondissement")
  
ggsave(plot5, file="plots/Top5_prenoms_par_arrond.pdf", width = w, height = h/3)
  
  
   





