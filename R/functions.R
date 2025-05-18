#######################################
## Statistical Analyisis Ficha Night ##
#######################################

# add palettes
# pal <- choose_palette()
pal51 <- c( "#80146E", "#6474B9", "#34B8C0", "#A4E0B7", "#F5F2D8")
pal52 <- c("#001889", "#91008D", "#D24E71", "#EDA200", "#DAFF47")
pal2 <- c("#F5F2D8", "steelblue3")


##################################
## Functin: Marcador Corona    ##
##################################
marcador_corona <- function(dframe) {
  # plot heatmap of marcador corona
  rk <- ggplot(dframe, aes(x = 0.5, y = Jugador, fill = TotalPuntos))

  # add geom, tiles, text and color
  rk <- rk + geom_tile(alpha = 0.8) +
    geom_text(aes(label = TotalPuntos),
      colour = "gray10",
      fontface = "bold",
      size = 10,
      family = "engle"
    ) +
    coord_fixed(xlim = c(0, 1)) +
    scale_fill_distiller(
      palette = "RdYlGn",
      direction = 1
    ) +
    labs(
      x = NULL,
      y = NULL,
      title = "Marcador Corona",
      subtitle = "Puntos Acumulados"
    )

  # add formatting
  rk <- rk +
    theme_minimal() +
    theme(
      plot.title = element_text(
        family = "sister",
        face = "bold"
        # ,margin = margin(10, 0, 10, 0)
        ,size = 36,
        ,color = "tomato3"
      )
      ,plot.subtitle = element_text(
        family = "sister"
        ,size = 24
        ,color = "gray55"
      )
      ,plot.title.position = "plot"
      ,axis.text.x = element_blank() # Remove x-axis text
      ,axis.ticks.x = element_blank() # Remove x-axis ticks
      ,axis.text.y = element_text(
        family = "engle",
        size = 24,
        face = "bold"
      ) # Change y-axis text font
      ,legend.position = "none" # Remove legend
      ,panel.grid.major = element_blank() # Remove major grid lines
      ,panel.grid.minor = element_blank() # Remove minor grid lines
    ) +
    theme(
      plot.background = element_rect(fill = "snow", color = NA)
    )
  return(rk)
}


###################################
## Function: Puntos Acumulados  ##
##################################
puntos_acum <- function(dframe) {
  ww <- ggplot(dframe, aes(x = Jugador))

  # add geoms
  ww <- ww +
    geom_bar(aes(fill = GanadosFlag),
      position = "fill",
      alpha = 0.8
    ) +
    scale_fill_manual(values = pal2) +
    geom_text(
      data = corona, aes(
        x = Jugador,
        y = GanadosPerc,
        label = scales::percent(GanadosPerc)) # add labels to Ganados %
      ,vjust = 0.3
      ,hjust = 1.3
      ,color = "white"
      ,fontface = "bold"
      ,size = 10
      ,family = "engle") +
    # add confidence interval with pointrange
    # geom_pointrange(data = corona, aes(x = Jugador
    #                                    ,y = GanadosPerc
    #                                    ,ymin = GanadosPerc - CE
    #                                    ,ymax = GanadosPerc + CE)  # Add CE
    #                 ,colour = "lightsteelblue2"
    #                 ,alpha = 0.4
    #                 ,size = 0.6
    #                 ) +
    # flip coordinates
    coord_flip() + 
    # add dotted line at 50%
    geom_hline(
      yintercept = 0.5,
      linetype = "dashed",
      color = "orange",
      linewidth = 0.8) + 
    # add title
    labs(
      x = NULL,
      y = NULL,
      title = "",
      subtitle = "Porcentaje de juegos ganados")
  
  # add formatting
  ww <- ww +
    # format x-axis as percentages with specific breaks
    scale_y_continuous(
      labels = scales::percent,
      breaks = c(0, 0.5, 1)
    ) + 
    theme_minimal() +
    theme(
      plot.title = element_text(
        family = "sister"
        ,face = "bold"
        # ,margin = margin(10, 0, 10, 0)
        ,size = 36,
        ,color = "tomato3"
        ,hjust = 0.2
      ),
      plot.subtitle = element_text(
        family = "sister"
        ,size = 24
        ,color = "gray55"
        ,hjust = 0.2
      ),
      axis.text.y = element_blank() # Remove y-axis labels
      # ,axis.ticks.y = element_blank()  # Remove y-axis ticks
      # ,axis.title.y = element_blank()  # Remove y-axis title
      # ,axis.ticks.x = element_blank()  # Remove x-axis ticks
      # ,axis.title.x = element_blank()  # Remove x-axis title
      # axis.text.x = element_blank()
      ,legend.position = "none" # Remove legend
      # ,axis.text.y = element_text(family = "engle"
      #                             ,size = 20
      #                             ,face = "bold")  # Change y-axis text font
      ,axis.text.x = element_text(size = 16, face = "bold")
      ,panel.grid.major = element_blank() # Remove major grid lines
      ,panel.grid.minor = element_blank() # Remove minor grid lines
    ) +
    theme(
      plot.background = element_rect(fill = "snow", color = NA)
    )
  return(ww)
}



###################################
## Function: Total de Juegos    ##
##################################
total_juegos <- function(dframe) {
  tdj <- ggplot(dframe, aes(x = 1
                     ,y = Jugador)
                )
  
  # add geoms
  tdj <- tdj + geom_label(aes(label = TotalJugados)
                          ,color = "#91008D"
                          ,size = 10
                          ,family = "engle") +
    coord_fixed(xlim = c(0,2)) +
    # add labels
    labs(x = NULL
         ,y = NULL
         ,title = ""
         ,subtitle = "Total de juegos"
         ,caption = "Temporada invierno 2025")
  
  # add formatting
  tdj <- tdj + 
    theme_minimal() +
    theme(
      plot.title = element_text(family = "sister"
                                ,face = "bold"
                                # ,margin = margin(10, 0, 10, 0)
                                ,size = 36
                                ,color = "tomato3"
                                ,hjust = 0.2)
      ,plot.subtitle = element_text(family = "sister"
                                    ,size = 24
                                    ,color = "gray55"
                                    ,hjust = 0.2)
      ,axis.text.y = element_blank()  # Remove y-axis labels
      ,axis.text.x = element_blank()       # Remove x-axis text
      ,axis.ticks.x = element_blank()      # Remove x-axis ticks
      ,panel.grid.major = element_blank()   # Remove major grid lines
      ,panel.grid.minor = element_blank()  # Remove minor grid lines
    ) +
    theme(
      plot.background = element_rect(fill = "snow", color = NA)
      )
  return(tdj)
}


##############################################
## Function: Tendencia de Puntos Acumulados ##
##############################################
tendencia <- function(dframe) {
  tt <- ggplot(dframe, aes(x = Partida
                      ,y = PuntosCum
                      ,color = Jugador)
  )
  
  # add geoms
  tt <- tt +
    geom_step(linewidth = 1, alpha = 0.75) +
    # add color scale
    scale_color_paletteer_d("PNWColors::Bay") +
    # scale_color_paletteer_d("fishualize::Koumansetta_rainfordi") +
    # add titles
    labs(
      x = "Partida",
      ,y = "Puntos Acumulados"
      ,title = "Rendimiento Acumulado"
      ,caption = "Temporada invierno 2025"
    ) +
    # add labels to final score
    geom_text(aes(label = ifelse(Partida == max(Partida), as.character(PuntosCum), ""))
              ,hjust = -0.25
              ,vjust = 1
              ,size = 12
              ,family = "engle") +
    geom_hline(yintercept = 0
               ,linetype = "dashed"
               ,color = "gray42") +
    ylim(c(-55,55))
  
    # add formatting
    tt <- tt + 
    theme_minimal() +
    theme(
      plot.title = element_text(family = "sister"
                                ,face = "bold"
                                # ,margin = margin(10, 0, 10, 0)
                                ,size = 36
                                ,color = "tomato3"
                                ,hjust = 0.2
      )
      ,plot.subtitle = element_text(family = "sister"
                                    ,size = 24
                                    ,color = "gray55"
                                    ,hjust = 0.2)
      ,axis.text = element_text(size = 18
                                  ,color = "darkgray"
                                  ,face = "bold")
      ,axis.title = element_text(size = 20
                                   ,color = "darkgray"
                                   ,face = "bold")
      ,panel.grid.major = element_blank()
      ,panel.grid.minor = element_blank()
      ,legend.position = "none"
    ) +
    theme(
      plot.background = element_rect(fill = "snow", color = NA)
      )
    return(tt)
}

###############################################
## Function: Saldo por jugada dos temporadas ##
###############################################
saldo_box <- function(dframe, doutlier) {
  bx <- ggplot(data = dframe
               ,aes(x = Jugador
                    ,y = Saldo
                    ,color = Jugador))
  
  # add geoms with violin
  bx <- bx + geom_violin(width = 0.8
                         ,linewidth = 0.2
                         ,alpha = 0.6) +
    scale_color_paletteer_d("PNWColors::Bay") +
    geom_jitter(size = 2.2
                ,width = 0.1
                ,alpha = 0.75) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
    # calculate median
    stat_summary(fun = median
                 ,geom = "point"
                 ,shape = 23
                 ,size = 3) +
    ylim(c(-20, 20)) +
    # add titles
    labs(
      x = "",
      ,y = "Puntos"
      ,title = "Saldo por ficha night"
      ,subtitle = "Últimas dos temporadas"
      ,caption = "Temporada invierno 2025")
  
  # add outliers 
  bx <- bx + geom_label_repel(data = doutlier, aes(label = Saldo)
                              ,size = 10
                              ,fontface = "bold"
                              ,family = "engle"
                              ,nudge_x = 0.3) +
    geom_hline(yintercept = c(-10, 10), linetype = "dotted", color = "gray")
  
  # add formatting
  #  coord_flip() + # This switch X and Y axis and allows to get the horizontal version
  bx <- bx + theme_minimal() +
    theme(
      plot.title = element_text(family = "sister"
                                ,face = "bold"
                                # ,margin = margin(10, 0, 10, 0)
                                ,size = 36
                                ,color = "tomato3"
                                ,hjust = 0.2
      )
      ,plot.subtitle = element_text(family = "sister"
                                    ,size = 24
                                    ,color = "gray55"
                                    ,hjust = 0.2
      )
      ,plot.title.position = "plot"
      ,legend.position = "none"
      ,axis.text.x = element_text(family = "engle"
                                  ,size = 24
                                  ,face = "bold")  # Change y-axis text font
      ,axis.text.y = element_text(size = 18
                                  ,color = "darkgray"
                                  ,face = "bold")
      ,axis.title.y = element_text(size = 20
                                   ,color = "darkgray"
                                   ,face = "bold")
      ,panel.grid.major = element_blank()
      ,panel.grid.minor = element_blank()
    ) +
    theme(
      plot.background = element_rect(fill = "snow", color = NA)
    )
  return(bx)
}


################################################
## Function: Rendimiento acumulado por pareja ##
################################################
# set colors
color_tp <- c("FALSE" = "indianred1"
              ,"TRUE" = "lightgreen")

eqq_dmp <- function(dframe) {
  # plot crosstab of rendimiento por pareja
  eqq <- ggplot(dframe, aes(x = Jugador
                            ,y = Pareja)) +
    geom_point(aes(color = TotalPuntos > 0
                   ,size = abs(TotalPuntos))
               ,shape = 21
               ,stroke = 7
               ,alpha = 0.85) +
    geom_text(aes(label = ifelse(abs(TotalPuntos) > 3, as.character(TotalPuntos), "")
                  ,color = TotalPuntos > 0)
              ,size = 10
              ,fontface = "bold"
              ,alpha = 0.85) +
    # add scaling
    scale_size(range = c(4, 18)) +  # Adjust the range to make points bigger
    scale_color_manual(values = color_tp) +
    # add title
    labs(
      x = NULL
      ,y = NULL
      ,title = "Rendimiento por pareja"
      ,subtitle = "Total de puntos"
      ,caption = "Temporada invierno 2025")
  
  # add formating
  eqq <- eqq + theme_minimal() +
    theme(
      plot.title = element_text(
        ,family = "sister"
        ,face = "bold"
        # ,margin = margin(10, 0, 10, 0)
        ,size = 36
        ,color = "tomato3")
      ,plot.subtitle = element_text(
        family = "sister"
        ,size = 24
        ,color = "gray55")
      ,plot.title.position = "plot"
      ,axis.text = element_text(
        family = "engle",
        size = 24,
        face = "bold")
      ,legend.position = "none" # Remove legend
      ,panel.grid.major = element_blank() # Remove major grid lines
      ,panel.grid.minor = element_blank() # Remove minor grid lines
    ) +
    theme(
      plot.background = element_rect(fill = "snow", color = NA)
    )
  return (eqq)
}


#################################
## Function: puntos por juego  ##
#################################
color_mm <- c("#A4E0B7", "#34B8C0", "#6474B9", "#80146E")
pal52 <- c("#001889", "#91008D", "#D24E71", "#EDA200", "#DAFF47")

juegos_puntos <- function(dframe) {
  # plot puntos por juego
  jx <- ggplot(dframe, aes(
    x = Fecha
    ,y = Puntos
    ,color = Marca
    ,size = abs(Puntos)
  ))
  
  # add geom, tiles, text and color
  jx <- jx + 
    geom_jitter(
      width = 0.7,
      alpha = 0.7
    ) +
    # add scaling
    scale_size(range = c(2, 8)) +
    scale_color_manual(values = color_mm) +
    # add labelss
    labs(
      x = NULL,
      y = "Puntos",
      title = "Rosita y mas allá",
      subtitle = "Puntos por juego",
      caption = "Temporada invierno 2025"
    ) +
    # add facets
    facet_wrap(~ Jugador
               ,nrow = 1) +
    # add outliers
    geom_label_repel(
      aes(label = ifelse(abs(Puntos) > 5, Pareja, NA))
      ,size = 10
      ,fontface = "bold"
      ,family = "engle"
      ,nudge_x = 0.3
    ) +
    scale_y_continuous(breaks = -8:8) +
    # remove legend for size
    guides(size = "none")
  
  # add formatting
  jx <- jx + theme(
    # panel.background = element_blank(),  # Remove plot background
    # panel.grid.major.y = element_line(color = "gray90", linewidth = 0.2),  # Add y-axis grid lines
    panel.grid.major.x = element_blank()  # Remove x-axis grid lines
    ,panel.grid.minor = element_blank()    # Remove minor grid lines
    # strip.background = element_rect(fill = "gray95", color = NA),  # Keep facet background
    # axis.line = element_line(color = "black")  # Keep axis lines
    ,legend.position = "bottom"  # Move legend to bottom
    ,legend.box = "horizontal"    # Arrange legend items horizontally
    ,legend.title = element_blank()  # Remove legend header
    ,legend.key = element_blank()     # Remove legend key background
    ,strip.text = element_text(family = "engle"
                               ,size = 24
                               ,face = "bold")
  ) + # Change facet header format
    theme(
      plot.title = element_text(family = "sister"
                                ,face = "bold"
                                # ,margin = margin(10, 0, 10, 0)
                                ,size = 36
                                ,color = "tomato3"
                                ,hjust = 0.2
      )
      ,plot.subtitle = element_text(family = "sister"
                                    ,size = 24
                                    ,color = "gray55"
                                    ,hjust = 0.2
      )
      ,plot.title.position = "plot"
      ,axis.text.x = element_text(family = "engle"
                                  ,size = 20
                                  ,face = "bold")  # Change y-axis text font
      ,axis.text.y = element_text(size = 18
                                  ,color = "darkgray"
                                  ,face = "bold")
      ,axis.title.y = element_text(size = 18
                                   ,color = "darkgray"
                                   ,face = "bold")
    ) +
    theme(
      plot.background = element_rect(fill = "snow", color = NA)
    )
  return(jx)
}

