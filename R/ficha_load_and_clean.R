##########################################
## Ficha Night Domino                   ##
## Summary of statistics for the season ##
## Last season:  Fall 2025              ##
## December 2025, jortega               ##
##                                      ##
##########################################


# load libraries
library(tidyverse)
library(patchwork)


#####################################
## Load and clean raw data         ##
## Data set: ficha datos trend     ##
#####################################
# define season's path
path <- "./data/20254_fall/"

# load trend data
partida <- read_csv(paste0(path, "ficha_datos_trend.csv"))

# transform datatypes
partida <- partida %>% 
  mutate(Fecha = as.Date(partida$Fecha, format = "%m/%d/%Y")) %>% 
  mutate(GanadosFlag = ifelse(Ganados == 1, TRUE, FALSE))

# check nulls
null_check <- partida %>%
  summarize(across(everything(), ~ sum(is.na(.))))

print(null_check)


# add field to group scores
partida <- partida %>%
  mutate(Marca = case_when(
    abs(Puntos) == 1 ~ "sencillo"
    ,abs(Puntos) == 2 ~ "doble"
    ,abs(Puntos) == 3 ~ "zapato"
    ,abs(Puntos) > 3 ~ "rosita"
    # ,TRUE ~ NA_character_ # Optional: handle any other cases if needed
  )) %>% 
  mutate (Marca = factor(Marca
                 ,ordered = TRUE
                 ,levels = c("sencillo", "doble", "zapato", "rosita")
                 )
          )


# sort data by Jugador and Partida
partida <- partida %>% 
  arrange(Jugador, Partida)

# add cumulative points
partida <- partida %>% 
  group_by(Jugador) %>% 
  mutate(PuntosCum = cumsum(Puntos)) %>% 
  ungroup()


# check data
glimpse(partida)

# save file
saveRDS(partida, file = paste0(path,"ficha_trend_clean.rds"))


#####################################
## Load and clean raw data         ##
## Data set: ficha datos box       ##
#####################################

# load scores by match game
box_data <- read_csv(paste0(path,"ficha_datos_box.csv"))

# transform datatypes
box_data <- box_data %>% 
  mutate(Fecha = as.Date(box_data$Fecha, format = "%m/%d/%Y"))

# check data
glimpse(box_data)

# save file
saveRDS(box_data, file = paste0(path,"ficha_box_clean.rds"))


###############################
## Aggregate detail partida ##
## Data set: corona         ##
##############################
corona <- partida %>%
  select(Jugador, Puntos, Ganados, Jugados) %>%
  group_by(Jugador) %>%
  summarize(TotalGanados = sum(Ganados, na.rm = TRUE)
            ,TotalPuntos = sum(Puntos, na.rm = TRUE)
            ,TotalJugados = sum(Jugados, na.rm = TRUE)
            ,GanadosPerc = round(TotalGanados / TotalJugados,2)
  ) %>%
  ungroup() %>%
  arrange(desc(TotalPuntos)) %>% 
  mutate(SE = sqrt(GanadosPerc * (1-GanadosPerc)/TotalJugados)) %>% 
  mutate(CE = 1.96*SE)

# save file
saveRDS(corona, file = paste0(path,"ficha_corona.rds"))



###############################
## Saldo por pareja         ##
## Data set: equipo         ##
##############################
equipo <- partida %>%
  select(Jugador, Pareja, Puntos, Ganados, Jugados) %>%
  group_by(Jugador, Pareja) %>%
  summarize(TotalGanados = sum(Ganados, na.rm = TRUE)
            ,TotalPuntos = sum(Puntos, na.rm = TRUE)
            ,TotalJugados = sum(Jugados, na.rm = TRUE)
            ,GanadosPerc = round(TotalGanados / TotalJugados,2)
            ,.groups = "drop") %>%  # This removes the grouping after summarization
  ungroup()

# save file
saveRDS(equipo, file = paste0(path,"ficha_equipo.rds"))

