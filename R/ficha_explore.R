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

## Load fonts
## Loading Google fonts (https://fonts.google.com/)
font_add_google("Love Ya Like A Sister", "sister")  # text and title
font_add_google("Shadows Into Light Two", "siltwo")
font_add_google("Gloria Hallelujah", "gloria")  # labels
font_add_google("Englebert", "engle")  # labels

## Automatically use showtext to render text
showtext_auto()


## Add palette
# pal <- choose_palette()
pal51 <- c( "#80146E", "#6474B9", "#34B8C0", "#A4E0B7", "#F5F2D8")
pal52 <- c("#001889", "#91008D", "#D24E71", "#EDA200", "#DAFF47")
pal2 <- c("#F5F2D8", "steelblue3")


## Load functions
source("./R/functions.R")

################################
## Summary of the season      ##
## Marcador Corona            ##
################################
# load data
partida <- readRDS("./data/20244_winter/ficha_trend_clean.rds")

# add factor
partida <- partida %>% 
  mutate(Jugador = factor(Jugador, levels = c("Jorge", "Piztache", "Rober", "Kilo", "Jerry")))

# table with Puntos
corona <- readRDS("./data/20244_winter/ficha_corona.rds")
# print the formatted table
print(corona)


################################
## Puntos Acumulados          ##
################################
# plot heatmap of marcador corona
rk <- marcador_corona(corona)
print(rk)


################################
## Ganados en porcentaje      ##
################################
# plot Won as % of total
ww <- puntos_acum(partida)
print(rk + ww)


################################
## Total de juegos            ##
################################
tdj <- total_juegos(corona)
print(rk + ww + tdj)



################################
## Tendencia por juego        ##
################################
tt <- tendencia(partida)

# create subset of data for each player to later add labels
jugpar1 <- partida %>% 
  select(Fecha, Jugador, Partida, PuntosCum) %>% 
  filter(Jugador %in% c("Kilo", "Rober", "Jorge") & Partida == 112)

jugpar2 <- partida %>% 
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
# load data
box_data <- readRDS("./data/20244_winter/ficha_box_clean.rds")

# add factor
box_data <- box_data %>%
  filter(Temporada %in% c("Fall 2024", "Winter 2025")) %>% 
  mutate(Jugador = factor(Jugador, levels = c("Jorge", "Piztache", "Rober", "Kilo", "Jerry")))

# Get median values
median_values <- aggregate(Saldo ~ Jugador, data = box_data, FUN = median)

# filter outliers
box_outliers <- box_data %>% 
  filter(abs(Saldo) > 10)

# create plot
bx <- ggplot(data = box_data
             ,aes(x = Jugador
                  ,y = Saldo
                  ,color = Jugador))

# add boxplot with violin
bx <- bx + geom_violin(width = 0.8
                  ,linewidth = 0.2
                  ,alpha = 0.6) +
  scale_color_paletteer_d("PNWColors::Bay") +
  geom_jitter(size = 2.2
              ,width = 0.1
              ,alpha = 0.75) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  stat_summary(fun.y = median
               ,geom = "point"
               ,shape = 23
               ,size = 3) +
  ylim(c(-20, 20)) +
  labs(
    x = "",
    ,y = "Puntos"
    ,title = "Saldo por ficha night"
    ,subtitle = "Últimas dos temporadas"
    ,caption = "Temporada invierno 2025"
  )

# add outliers
bx <- bx + geom_label_repel(data = box_outliers, aes(label = Saldo)
               ,size = 4
               ,fontface = "bold"
               ,family = "engle"
               ,nudge_x = 0.3
                ) +
  geom_hline(yintercept = c(-10, 10), linetype = "dotted", color = "gray")


# add formatting
  #  coord_flip() + # This switch X and Y axis and allows to get the horizontal version
bx <- bx + theme_minimal() +
  theme(
    plot.title = element_text(family = "sister"
                              ,face = "bold"
                              # ,margin = margin(10, 0, 10, 0)
                              ,size = 24
                              ,color = "tomato3"
                              ,hjust = 0.2
    )
    ,plot.subtitle = element_text(family = "sister"
                                  ,size = 14
                                  ,color = "gray55"
                                  ,hjust = 0.2
    )
    ,plot.title.position = "plot"
    ,legend.position = "none"
    ,axis.text.x = element_text(family = "engle"
                                ,size = 20
                                ,face = "bold")  # Change y-axis text font
    ,axis.text.y = element_text(size = 12
                                ,color = "darkgray"
                                ,face = "bold")
    ,axis.title.y = element_text(size = 12
                                 ,color = "darkgray"
                                 ,face = "bold")
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
  )

print(bx)



###################################
## Rendimiento por pareja       ##
## Total score by team           ##
###################################
bp <- ggplot(partida, aes(x = Jugador))

# option 1
bp + stat_summary(aes(y = Puntos, group = Pareja)
                  ,fun = "sum"
                  ,geom = "point"
                  ,size = 4
                  ,alpha = 0.5)

# option 2
bp + geom_point(aes(y = Puntos, group = Pareja)
                ,stat = "summary"
                ,fun = "sum"
                ,size = 4
                ,alpha = 0.5)

# option 3 (to add color)
bp + stat_summary(fun.data = ~ data.frame(total = sum(.x), tot_color = sum(.x) > 0)
                  ,aes(y = stage(Puntos,after_stat = total)
                       ,group = Pareja
                       ,label = Pareja
                       ,color = after_stat(tot_color)
                      )
                  ,geom = "text"
                  )

# optio 4: prepare summary dataframe


# one more example
ggplot(mpg, aes(class, displ)) +
  geom_violin() +
  stat_summary(
    aes(
      y = stage(displ, after_stat = mean),
      label = after_stat(paste(mean, "±", sd))
    ),
    geom = "text",
    fun.data = ~ round(data.frame(mean = mean(.x), sd = sd(.x)), 2)
  )





# new bar chart
equipo_agg <- equipo_data %>%
  group_by(Jugador, Pareja) %>%
  summarize(Puntos = sum(Puntos, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(Puntos))


equipo_agg <- partida %>%
  group_by(Jugador) %>%
  summarize(Puntos = sum(Puntos, na.rm = TRUE)) %>%
  arrange(desc(Puntos))



ggplot(data = equipo_data
       ,aes(x = Jugador
            ,y = Pareja)
       ) +
  geom_point(aes(fill = Puntos > 1
                 ,size = abs(Puntos)
                 )
            ,shape = 22
            ,alpha = 0.5) +
  scale_size(range = c(5, 30))  # Adjust the range to make points bigger




# won and lost as percentage by player
# add perdidos
waffle <- corona %>% 
  mutate(PerdidosPerc = 1 - GanadosPerc) %>% 
  mutate(PerdidosPerc = PerdidosPerc * 100) %>% 
  mutate(GanadosPerc = GanadosPerc * 100)

# pivot data
waffle <- waffle %>% 
  select(-TotalGanados, -TotalPuntos, -TotalJugados, -SE, -CE) %>%
  arrange(desc(GanadosPerc)) %>% 
  pivot_longer(
    cols = c(GanadosPerc, PerdidosPerc)
    ,names_to = "GP"
    ,values_to = "Percent"
  ) %>% 
  mutate(GP = factor(GP))

glimpse(waffle)

# set fonts
font_add_google("Staatliches","sta")
font_add_google("Raleway","ral")
showtext_auto()

# path = 'https://raw.githubusercontent.com/holtzy/R-graph-gallery/master/DATA/share-cereals.csv'
# data <- read_csv(path)


# plot waffle char
ggplot(waffle, aes(fill = GP, values = Percent)) +
  geom_waffle(na.rm=TRUE, n_rows=4, flip=F, size = 0.33, colour = "white") +
  facet_wrap(~reorder(Jugador, Percent), ncol=1,strip.position = "left") +
  coord_equal() +
  guides(fill='none') +
  labs(
    title="<b>Ganados porcentaje <span style='color:#f72585;'>animal feeds</span></b>",
    caption="<b>Data</b> OWID (year 2021) <b>| Plot</b> Juan Ortiz"
  ) +
  scale_fill_manual(values=c('#f72585','#4F0325'))+
  theme_void()+
  theme(
    plot.background = element_rect(fill="#222725",color=NA),
    plot.title = element_markdown(size=18,family='sta',margin= margin(0.5,0,0.5,-0.75,'cm'),color='white'),
    strip.text = element_markdown(hjust=0.5,size=12,family='ral',angle=90,margin=margin(0,0,0,0,'cm'),lineheight = 0.45,color='white'),
    plot.caption = element_markdown(size=12,family='ral',margin=margin(0.5,0,0.5,-0.75,'cm'),hjust=0,color='white'),
  )


# plot won vs lost
won_lost_plot <- ggplot(df_score, aes(x = Pareja, y = Jugados)) +
  geom_segment(aes(
    x = Jugador, xend = Jugador,
    y = Perdidos, yend = Ganados
  ), color = "darkgray") +
  geom_point(aes(x = Jugador, y = Ganados), size = 8, color = "skyblue3", alpha = 0.85) +
  geom_text(aes(x = Jugador, y = Ganados, label = Ganados), color = "white", size = 4, fontface = "bold") +
  geom_point(aes(x = Jugador, y = Perdidos), size = 8, color = "deeppink3", alpha = 0.85) +
  geom_text(aes(x = Jugador, y = Perdidos, label = Perdidos), color = "white", size = 4, fontface = "bold") +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    plot.title = element_text(color = "indianred3", hjust = 0.5, size = 12),
    aspect.ratio = 0.4,
    strip.background = element_rect(
      colour = "gray",
      fill = "skyblue3"
    ),
    strip.text = element_text(size = 14, face = "bold", family = "Ubuntu", color = "white")
  ) +
  labs(x = "", y = "", title = "") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgray")


print(won_lost_plot)






# create bar plot
ganados_bar <- ggplot(corona, aes(x = GanadosPerc, y = Jugador)) +
  geom_bar(stat = "identity", fill = "skyblue3", width = 0.6, alpha = 0.8) +
  geom_pointrange( aes(xmin=GanadosPerc-CE, xmax=GanadosPerc+CE, y=Jugador), colour="orange", alpha=0.7, size=0.8) +
  geom_text(aes(label = paste0(round(GanadosPerc * 100, 0), "%"))
            ,vjust = -0.3
            ,hjust = -0.3
            ,color = "skyblue4"
            ,fontface = "bold") +
  scale_x_continuous(limits = c(0, 0.8)) +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "darkgray") +
  annotate("text", x = 0.5, y = length(corona$Jugador) + 0.5, label = "50%", color = "darkgray", fontface = "bold") +
  labs(title = "Ganados %",
       x = "",
       y = "") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,plot.title = element_text(face = "bold", color = "indianred3")
    ,axis.text.x = element_blank()
    ,axis.text.y = element_text(size = 12, color = "skyblue4", face = "bold")
  )

print(ganados_bar)





############################
## Analisis de tendencia  ##
############################


##############################
## Zapatazos as % of juegos ##
##############################
# summary by marca
marca_table <- data %>%
  filter(Ganados == 1) %>% 
  group_by(Jugador, Marca) %>%
  summarize(Total_Juegos = sum(Jugados, na.rm = TRUE), .groups = 'drop') %>%
  group_by(Jugador) %>%
  mutate(Total_Juegos_Pct = Total_Juegos / sum(Total_Juegos) * 100) %>% 
  ungroup()
  
# Print the summary table
print(marca_table %>% filter(Marca == "sencillo") %>% arrange(desc(Total_Juegos_Pct)))

# reorder factor
marca_table$Jugadorx <- factor(marca_table$Jugador, ordered = TRUE, levels = c("Rober", "Kilo", "Jerry", "Jorge", "Piztache"))

# show table
zapato <- ggplot(marca_table, aes(x = Jugadorx, y = Total_Juegos_Pct, fill = Marca)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = scales::percent(Total_Juegos_Pct / 100, accuracy = 1)),
            position = position_stack(vjust = 0.5), size = 5, fontface = "bold", color = "darkgray") +
  coord_flip() +
  labs(title = "Distribution of Total Juegos Percentage by Jugador and Marca",
       x = "Jugador",
       y = "Percentage of Total Juegos") +
  theme_minimal()

print(zapato)


library(dplyr)
library(distributional)

theme_set(theme_ggdist())

set.seed(1234)
df = data.frame(
  group = c("a", "b", "c"),
  value = rnorm(1500, mean = c(5, 7, 9), sd = c(1, 1.5, 1))
)
df %>%
  ggplot(aes(x = value, y = group)) +
  stat_halfeye()


###################################
## Saldo por partida y jugador   ##
## Violin plot distribution cool ##
###################################
# Plot saldo de puntos por partida
box_data <- read_csv("ficha_datos_box.csv")

# Convert Fecha to date format
box_data$Fecha <- as.Date(box_data$Fecha, format = "%m/%d/%Y")

# Get median values
median_values <- aggregate(Saldo ~ Jugador, data = box_data, FUN = median)

# Filter the data to include only values greater than 10 or lower than -10
filtered_data <- box_data[abs(box_data$Saldo) > 10, ]

# Create the boxplot
ggplot(box_data, aes(x = Jugador, y = Saldo, color = Jugador)) +
  #  geom_boxplot(width=0.4, color="grey", alpha=0.1) +
  geom_violin(width = 0.8, linewidth = 0.2, alpha = 0.6) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Saldo por jugador y partida - 2024",
    y = "Saldo de partida",
    x = "",
    color = "Jugador"
  ) +
  #  geom_point(alpha = 0.7, size = 3, position = position_jitter(width = 0.2)) +
  geom_jitter(size = 2.2, width = 0.2, alpha = 0.75) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  stat_summary(fun.y = median, geom = "point", shape = 23, size = 3) +
  ylim(-25, 25) +
  #  coord_flip() + # This switch X and Y axis and allows to get the horizontal version
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold", color = "darkred"),
    axis.text.x = element_text(size = 12, color = "darkgray", face = "bold"),
    axis.text.y = element_text(size = 12, color = "darkgray", face = "bold")
  ) +
  geom_text(
    data = filtered_data, aes(x = Jugador, y = Saldo, label = Saldo),
    size = 3, nudge_y = 1, fontface = "bold"
  )
# geom_text(data = median_values, aes(x = Jugador, y = Saldo, label = round(Saldo, 2)),
# fontface = "bold", size = 4)




###########################
## Tendencia por jugador ##
###########################

# Load the dataset
data <- read_csv("ficha_datos_trend.csv")

# Convert Fecha to date format
data$Fecha <- as.Date(data$Fecha, format = "%m/%d/%Y")

# Create a line chart
ggplot(data, aes(x = Partida, y = Total, color = Jugador, group = Jugador)) +
  geom_step(linewidth = 1, alpha = 0.7) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Ficha de Otoño 2024 - Rendimiento Acumulado",
    x = "Partida",
    y = "Rendimiento",
    color = "Jugador"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", color = "darkred")
  ) +
  geom_text(aes(label = ifelse(Partida == max(Partida), as.character(Total), "")),
            hjust = 0.05, vjust = 1.5, size = 4
  ) +
  geom_hline(yintercept = 0, linetype = "dashed")




#################################
## Puntos por jugador y pareja ##
## Horizontal bar chart        ##
#################################
# Load data
equipo_data <- read_csv("ficha_datos_equipo.csv")

# Tranform
equipo_data <- equipo_data %>%
  mutate(Jugador = factor(Jugador)) %>%
  # Remove zero values
  filter(Puntos != 0) %>%
  # Order by Jugador and Puntos
  arrange(Jugador, desc(Puntos))

# Aggregate data
equipo_agg <- equipo_data %>%
  aggregate(Puntos ~ Jugador, FUN = sum) %>%
  arrange(desc(Puntos))

# Create the stacked bar chart
ggplot(data = equipo_data, aes(x = Jugador, y = Puntos)) +
  geom_col(aes(fill = Pareja), width = 0.7, alpha = 0.85) +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  # geom_text(aes(label = Puntos), position = position_stack(vjust = 0.5), color = "white") +
  scale_x_discrete(limits = equipo_agg$Jugador) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    plot.title = element_text(face = "bold", color = "darkred"),
    axis.text.y = element_text(size = 12, color = "darkgray", face = "bold"),
    aspect.ratio = 1.3 / 2.2
  ) +
  coord_flip() +
  geom_text(aes(label = Puntos, group = Pareja), position = position_stack(vjust = 0.5), color = "white", fontface = "bold") +
  labs(x = "", y = "", fill = "Pareja") +
  ggtitle("Puntos por Jugador y Pareja")



############################################
## Puntos y Ganados por jugador y pareja  ##
## Lollipop chart with a bit more style   ##
############################################
# Load data
equipo_dot <- read_csv("ficha_datos_equipo.csv")

# Transform
equipo_dot <- equipo_dot %>%
  mutate(Equipo = NULL) %>%
  mutate(Neto = ifelse(Puntos > 0, "positive", "negative")) %>%
  mutate(Neto = factor(Neto))

# Select player (Kilo, Jorge, Jerry, Rober, Piztache)
player <- "Piztache"

# Stacked bar plot de puntos
p1 <- equipo_dot %>%
  filter(Jugador == player) %>%
  arrange(Puntos) %>%
  mutate(Pareja = factor(Pareja, unique(Pareja))) %>%
  ggplot() +
  aes(x = Pareja, y = Puntos) +
  geom_segment(aes(
    x = Pareja, xend = Pareja,
    y = 0, yend = Puntos, color = Neto
  ), linewidth = 1.3, alpha = 0.8) +
  geom_point(aes(x = Pareja, y = Puntos, color = Neto), size = 8) +
  geom_text(aes(x = Pareja, y = Puntos, label = Puntos), color = "white", size = 4, fontface = "bold") +
  theme_light() +
  ylim(-15, 15) +
  coord_flip() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 12, color = "gray40", face = "bold.italic", family = "Ubuntu Mono"),
    plot.title = element_text(color = "indianred3", hjust = 0.5, size = 12),
    aspect.ratio = 0.4,
    strip.text = element_text(size = 14, face = "bold")
  ) +
  labs(x = "", y = "", title = "") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgray")

# Lollipop chart of Puntos
p2 <- equipo_dot %>%
  filter(Jugador == player) %>%
  arrange(Puntos) %>%
  mutate(Pareja = factor(Pareja, unique(Pareja))) %>%
  mutate(Perdidos = Jugados - Ganados) %>%
  ggplot() +
  aes(x = Pareja, y = Puntos) +
  geom_segment(aes(
    x = Pareja, xend = Pareja,
    y = Perdidos, yend = Ganados
  ), color = "darkgray") +
  geom_point(aes(x = Pareja, y = Ganados), size = 8, color = "skyblue3", alpha = 0.85) +
  geom_text(aes(x = Pareja, y = Ganados, label = Ganados), color = "white", size = 4, fontface = "bold") +
  geom_point(aes(x = Pareja, y = Perdidos), size = 8, color = "deeppink3", alpha = 0.85) +
  geom_text(aes(x = Pareja, y = Perdidos, label = Perdidos), color = "white", size = 4, fontface = "bold") +
  theme_light() +
  ylim(0, 15) +
  coord_flip() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    plot.title = element_text(color = "indianred3", hjust = 0.5, size = 12),
    aspect.ratio = 0.4,
    strip.background = element_rect(
      colour = "gray",
      fill = "skyblue3"
    ),
    strip.text = element_text(size = 14, face = "bold", family = "Ubuntu", color = "white")
  ) +
  labs(x = "", y = "", title = "") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgray") +
  facet_grid(Jugador ~ .)

# Merge plots
p1 + p2


# final score by player
df_score <- partida %>%
           group_by(Jugador) %>%
           summarize(Puntos = sum(Puntos, na.rm = TRUE)
                     ,Jugados = sum(Jugados, na.rm = TRUE)
                     ,Ganados = sum(Ganados, na.rm = TRUE)
                     ,.groups = 'drop') %>%
           mutate(Ganados_perc = round(Ganados/Jugados, 3)) %>% 
           mutate(SE = sqrt(Ganados_perc * (1-Ganados_perc)/Jugados)) %>% 
           mutate(CE = 1.96*SE) %>% 
           arrange(desc(Puntos))
