#Trabajo con taxonomía de peces
#2025-10-28
#Camila Neder & Julián Caro, usando fork e Catalina Marin

#settings####

#library
library(rgbif)
library(dplyr)
library(tidyverse)
library(ggplot2)

#datos----

estudio_taxon <- read.csv(file="fish_joint_data_2000_2025.txt", sep="\t")


#marcar datos conflictivos-----

aves_marcado <- estudio_aves %>% 
  mutate(conflicto = 
           case_when(orden_sacc != orden_sag | 
                     nombre_cientifico_sacc != nombre_cientifico_sag ~ "conflicto_taxonomico",
                     orden_sacc == orden_sag ~ "sin_conflicto"
           ))
#conteo de datos conflictivos---
n_conflictos <- aves_marcado %>% 
                group_by(conflicto) %>% 
                summarise( conteo = n())

#conteo de datos de categoría de peces por diferentes fuentes---
n_origendatos <- taxon_check %>% 
  group_by(Category, preferred_source) %>% 
  summarise(conteo = n(), .groups = "drop")


#gráfico por categoría----- 
grafico_category_source <- ggplot(n_origendatos, aes(x = Category, y = conteo, fill = preferred_source)) +
  geom_bar(stat = "identity", 
           position = position_dodge(width = 0.8),
           width = 0.7) +
  scale_fill_manual(
    name = "Fuente",
    values = c("GBIF" = "green", "OBIS" = "blue3", "Desconocido" = "grey70")
  ) +
  labs(
    title = "Comparativo de registros por Categoría y Fuente",
    x = "Categoría",
    y = "Número de registros"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 25, hjust = 1),
    legend.position = "right"
  )

print(grafico_category_source)


#gráfico comparativo-----
#-----
#selección de ocurrencias de GBIF
gbif_taxa <- taxon_check %>% 
  filter(preferred_source == "GBIF")
  
#selección de datos preferidos-----
taxon_df<-gbif_taxa
print(colnames(taxon_df))
columns_to_keep<- c("scientificName","Long" ,"Lat","Category")
taxon_df<-taxon_df[columns_to_keep]

#gráfico por categoría----- 
#paleta de colores
color_category <- c(
  "Antarctica" = "#1F78B4",
  "Antarctica/Subantarctica" = "yellow",
  "Temperate" = "#E31A1C")

grafico_category <- taxon_df %>%
  count(Category) %>%
  ggplot(aes(x = Category, y = n, fill = Category)) +
  geom_bar(stat = "identity", width = 0.6) +
  scale_fill_manual(values = color_category) +
  labs(title = "Número de registros por categoría",
    x = "Categoría",
    y = "Número de registros") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none",
    axis.text.x = element_text(angle = 25, hjust = 1)
  )

print(grafico_category)

## FIN EJEMPLO

