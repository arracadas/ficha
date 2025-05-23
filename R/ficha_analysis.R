##########################################
## Ficha Night Domino                   ##
## Summary of statistics for the season ##
## Last season:  Winter 2024            ##
## April 2025, jortega                  ##
##                                      ##
##########################################

## Load libraries
library(tidyverse)
library(patchwork)
library(showtext)
library(waffle)
library(ggtext)
library(colorspace)
library(RColorBrewer)
# par(mar=c(3,4,2,2))
# display.brewer.all()
library(ggthemes)
library(scales)    # For percent formatting
library(forcats)   # For reordering factor levels
library(paletteer) # Color palettes
library(ggrepel)
library(lubridate)

## Load fonts
## Loading Google fonts (https://fonts.google.com/)
font_add_google("Love Ya Like A Sister", "sister")  # text and title
font_add_google("Shadows Into Light Two", "siltwo")
font_add_google("Gloria Hallelujah", "gloria")  # labels
font_add_google("Englebert", "engle")  # labels

## Automatically use showtext to render text
showtext_auto()

## Load functions
source("./R/functions.R")

## Load data
partida <- readRDS("./data/20244_winter/ficha_trend_clean.rds")  # detail of partida
corona <- readRDS("./data/20244_winter/ficha_corona.rds")       # aggregate data with total de puntos
box_data <- readRDS("./data/20244_winter/ficha_box_clean.rds")  # aggregate data for jugada i.e. ficha night
equipo <- readRDS("./data/20244_winter/ficha_equipo.rds")   # aggregate data for desempeño de equipo

## Transform data
# orden levels
jugadorx <- c("Jorge", "Piztache", "Rober", "Kilo", "Jerry") 

# add factor
partida_df <- partida %>% 
  mutate(Jugador = factor(Jugador, levels = jugadorx )) %>% 
  mutate(JugadaMes = month(Fecha, label = TRUE))

# add factor
corona_df <- corona %>% 
  mutate(Jugador = factor(Jugador, levels = jugadorx))

# add factor
box_df <- box_data %>%
  filter(Temporada %in% c("Fall 2024", "Winter 2025")) %>% 
  mutate(Jugador = factor(Jugador, levels = jugadorx))



################################
## Summary of the season      ##
## Marcador Corona            ##
################################
# plot heatmap of marcador corona
rk <- marcador_corona(corona_df)

# Ganados en porcentaje
ww <- puntos_acum(partida_df)

# Total de juegos
tdj <- total_juegos(corona_df)
print(rk + ww + tdj)


################################
## Tendencia por juego        ##
################################
tt <- tendencia(partida_df)

# create subset of data for each player to later add labels
jugpar1 <- partida_df %>% 
  select(Fecha, Jugador, Partida, PuntosCum) %>% 
  filter(Jugador %in% c("Kilo", "Rober", "Jorge") & Partida == 112)

jugpar2 <- partida_df %>% 
  select(Fecha, Jugador, Partida, PuntosCum) %>% 
  filter(Jugador %in% c("Jerry", "Piztache") & Partida == 102)

jugpar_merged <- bind_rows(jugpar1, jugpar2)

# add labels
tt <- tt +
  geom_label(data = jugpar_merged, aes(label = Jugador)
             ,family = "engle"
             ,size = 6
             ) +
  geom_hline(yintercept = c(-30, 30), linetype = "dotted", color = "gray")

print(tt)


###################################
## Saldo por partida y jugador   ##
## Violin plot distribution cool ##
###################################
# get median values
median_values <- aggregate(Saldo ~ Jugador, data = box_df, FUN = median)

# filter outliers
box_outliers <- box_df %>% 
  filter(abs(Saldo) > 10)

# create plot
bx <- saldo_box(box_df, box_outliers)
print(bx)


###################################
## Saldo por pareja              ##
## Crosstab                      ##
###################################
# plot table
eqq <- eqq_dmp(equipo)  
print(eqq)
  

####################################
## Puntos por juego y ficha night ##
## plot chart                     ##
####################################
# plot juegos
jj <- juegos_puntos(partida_df)
print(jj)

