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
  ggtitle("Fig1: Nombre de AirBnb par arrondissement") +
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
  geom_text(size=5, position = position_dodge2(width = 0.9)) +
  ggtitle("Fig2: Prix moyen des appartements par nuit, par arrondissement et par type") +
  labs(y="Prix moyen par nuit (en $)", x="Arrondissements") + 
  theme(text = element_text(size = 20))

ggsave(plot2, file="plots/prix_par_quartier_type.pdf", width = w*1.5, height = h/2)
 

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
  pdf(name, width = 1.2*w, height = h)
  radarchart(moyenne_dispo_quartier_transpose,
             axistype = 1,
             pcol=paste(colors[i], "AF", sep=""),
             pfcol=paste(colors[i], "AF", sep=""),
             plwd=3,
             axislabcol="#878787",
             cglwd=0.5,
             vlcex=2.5,
             calcex = 3,
             title=cat,
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

midtown_shared <-
  midtown_Top20 %>%
  filter(room_type == "Shared room")

midtown_private <-
  midtown_Top20 %>%
  filter(room_type == "Private room")

midtown_Entire <-
  midtown_Top20 %>%
  filter(room_type == "Entire home/apt")

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
                                                   "color" = "#1f1c2d",
                                                   "font-size" = "15px"
                                                 )),
  ) %>%
  # Ajout d'un marker Rockfeller Center
  addMarkers(lng = -73.978798, lat = 40.758678,
             label = "Rockefeller Center",
             icon = custom_pin,
             labelOptions = labelOptions(noHide = T,
                                         style = list(
                                           "color" = "#1f1c2d",
                                           "font-size" = "15px"
                                         )),
  ) %>%
  # Ajout des markers (Latitude / longitude des logements)
  ## Shared room
  addMarkers(lng = midtown_shared$longitude, lat =  midtown_shared$latitude,
             label = paste(midtown_shared$price, "$"),
             icon = custom_pin,
             labelOptions = labelOptions(noHide = T,
                                         style = list(
                                           "color" = "#619cff",
                                           "font-size" = "12px"
                                         )),
  ) %>%
  ## Private room
  addMarkers(lng = midtown_private$longitude, lat =  midtown_private$latitude,
             label = paste(midtown_private$price, "$"),
             icon = custom_pin,
             labelOptions = labelOptions(noHide = T,
                                         style = list(
                                           "color" = "#00b528",
                                           "font-size" = "12px"
                                         )),
  ) %>%
  ## Logement entiers
  addMarkers(lng = midtown_Entire$longitude, lat =  midtown_Entire$latitude,
             label = paste(midtown_Entire$price, "$"),
             icon = custom_pin,
             labelOptions = labelOptions(noHide = T,
                                         style = list(
                                           "color" = "#f8766d",
                                           "font-size" = "12px"
                                         )),
  )

map %>% addTiles()

# Pour sauvegarder la carte, on prend un screenshot (car c'est une view et non un plot, donc pas de sauvegarde en pdf possible nativement)

#################################### PLOT 5 ####################################

# df avec seulement les noms et ids, sans r�p�tition
id_names <-
  df %>%
  select(c("host_id", "host_name")) %>%
  unique()

# Top des 5 des hôtes par arrondissement
rank_hostids <- 
  df %>%
  group_by(neighbourhood_group) %>%
  count(host_id) %>%
  rename(Nombre = n, Arrondissement = neighbourhood_group) %>%
  mutate(Rank = rank(-Nombre, ties.method = "random")) %>%
  subset(Rank <= 5) %>%
  arrange(Arrondissement, -Nombre) %>%
  inner_join(id_names, by = "host_id")
  

# Plot des 5 premiers host_names / arrondissement pour chaque arrondissement

plot5 <-
  rank_hostids %>%
  ggplot(aes(x = reorder(host_name, -Rank), y=Nombre, fill=Arrondissement)) +
  geom_bar(stat="identity") +
  coord_flip() +
  facet_wrap("Arrondissement", scales = "free") +
  scale_fill_manual(values = arrond_colors) + 
  labs(x="H�te", y="Occurence") + 
  ggtitle("Fig4: Top 5 des h�tes ayant le plus d'appartements par arrondissement") +
  theme(text = element_text(size = 17))
  
ggsave(plot5, file="plots/Top5_prenoms_par_arrond.pdf", width = 1.5*w, height = h/2)
  
  
   





