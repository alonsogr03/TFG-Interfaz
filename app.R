# Carga de paquetes ------------------------------------------------------------
library(dplyr)
library(tidyr)
library(shiny)
library(shinythemes)
library(bs4Dash)
library(ggplot2)
library(cowplot)
library(patchwork)
library(magick)
library(leaflet)
library(DT)
library(glue)
library(stringr)
library(countrycode)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(hms)
library(dtw)
library(proxy)
library(reshape2)
library(fontawesome) 
library(shinyWidgets)
library(plotly)
# Carga de funciones -----------------------------------------------------------
dunnIndex <- function (clusters, observaciones){
  #Calculo de la distancia máxima.(denominador)
  clusterMax<- max(clusters)
  posicionInicial <- 1
  distanciaMax <- 0
  for(i in 1:clusterMax){
    vectorAux<-clusters[clusters==i]
    posicionFinal<- posicionInicial + length(vectorAux)-1
    #Cojo la primera y la última observacion:
    if((observaciones[posicionFinal]-observaciones[posicionInicial]) > distanciaMax ) {
      #Actualizo datos 
      distanciaMax<-observaciones[posicionFinal]-observaciones[posicionInicial]
    }
    #Actualizo la posición Inicial.
    posicionInicial <- posicionFinal+1
  }
  
  #Calculo la distancia mínima.(numerador)
  # La inicalizo como la distancia máxima. La distancia entre el primer cluster y el último.
  distanciaMin<- observaciones[length(observaciones)] - observaciones[1]
  for( i in 1:(length(clusters) - 1)){
    if(clusters[i]<clusters[i+1]){
      #Tengo una distancia:
      distancia<-observaciones[i+1] - observaciones [i]
      if (distancia < distanciaMin){
        #Nueva distancia mínima:
        distanciaMin<- distancia
      }
    }
  }
  return (distanciaMin / distanciaMax)
}

# Carga de dataframes ----------------------------------------------------------

estilos <- c("FREE", "BACK", "BREAST", "FLY", "MEDLEY")

load( "DATOS/2024/WORLDS2024BUDAPEST25mINTERFAZ.RData")

worlds2024Budapest25m$numericSwimtime <- as.numeric(worlds2024Budapest25m$swimtime)

load("DATOS/2024/imagenesNadadores.RData")

nadadoresParticipantes<-worlds2024Budapest25m %>% distinct(athleteid, .keep_all = TRUE)

nadadoresPruebas<-worlds2024Budapest25m %>% 
  distinct(athleteid,heatid, .keep_all = TRUE)

medalleroMundial <- nadadoresPruebas %>%
  filter(place %in% c(1, 2, 3) & round %in% c("FIN", "FHT", "TIM"))

source("evolveCluster.R")


# Paletas de colores -----------------------------------------------------------
colores_wa <- list(
  azul_oscuro = "#0033A0",
  azul_medio  = "#005EB8",
  azul_claro  = "#00B5E2",
  amarillo    = "#FFD100",
  blanco      = "#FFFFFF",
  gris_claro  = "#F5F5F5",
  fondo = "#F5F9FF"
)

# Estilo CSS personalizado global ----------------------------------------------
estilos_css <- tags$head(
  tags$style(HTML(glue::glue("
    body {{
      background-color: {colores_wa$azul_medio};
      color: {colores_wa$blanco};
    }}
    .content-wrapper, .right-side {{
      background-color: {colores_wa$azul_medio};
    }}
    .main-header .logo {{
      background-color: {colores_wa$azul_oscuro} !important;
      color: {colores_wa$blanco} !important;
      font-weight: bold;
    }}
    .main-header .navbar {{
      background-color: {colores_wa$azul_oscuro};
    }}
    .main-sidebar {{
      background-color: {colores_wa$azul_oscuro};
    }}
    .box {{
      border-radius: 15px;
      box-shadow: 0 4px 10px rgba(0,0,0,0.15);
      background-color: {colores_wa$blanco};
      color: black;
    }}
    .box.box-primary > .box-header {{
      background-color: {colores_wa$azul_claro};
      color: black;
      font-weight: bold;
    }}
    .box.box-info > .box-header {{
      background-color: {colores_wa$amarillo};
      color: black;
      font-weight: bold;
    }}
    .box.box-warning > .box-header {{
      background-color: {colores_wa$azul_oscuro};
      color: white;
      font-weight: bold;
    }}
    .selectize-input {{
      border-radius: 8px;
      padding: 6px;
    }}
    .tab-content {{
      padding: 10px;
    }}
  ")))
)

# UI----------------------------------------------------------------------------



ui <- dashboardPage(
  title = "From Splash to Stats",
  fullscreen = TRUE,
  
## Encabezado ------------------------------------------------------------------
  header = dashboardHeader(
    title = dashboardBrand(
      title = span("From Splash to Stats", style = "font-weight: bold; font-size: 20px;"),
      color = "navy",  # Color del header
      image = "logo.jpg",
      href = NULL
    )
  ),
  
## Menú lateral estilizado------------------------------------------------------
  sidebar = dashboardSidebar(
    skin = "light",           
    status = "primary",       
    elevation = 4,
    width = 280,
    
    sidebarMenu(
      menuItem("Contexto: Budapest 2024", tabName = "contexto", icon = icon("globe")),
      menuItem("Resultados",             tabName = "resultados", icon = icon("table")),
      menuItem("Perfil de Nadadores",    tabName = "perfil_nadadores", icon = icon("user-circle")),
      
      menuItem("Ránkings", tabName = "rankings", icon = icon("ranking-star")),
      
      menuItem("Medallero", icon = icon("trophy"),
               menuSubItem("Medallero Nadadores", tabName = "medallero_nadadores", icon = icon("user")),
               menuSubItem("Medallero Países",    tabName = "medallero_paises", icon = icon("flag"))
      ),
      
      menuItem("Análisis Exploratorio", icon = icon("search"),
               menuSubItem("Nadadores",        tabName = "eda_nadadores", icon = icon("users")),
               menuSubItem("Pruebas",          tabName = "eda_pruebas", icon = icon("water")),
               menuSubItem("Road to Champion", tabName = "eda_campeon", icon = icon("medal"))
      ),
      
      menuItem("Análisis de Estrategias", icon = icon("chart-line"),
               menuSubItem("Velocidad",     tabName = "cluster_pruebas_cortas", icon = icon("bolt")),
               menuSubItem("Medio-Fondo",   tabName = "cluster_pruebas_medio", icon = icon("water-ladder")),
               menuSubItem("Fondo",         tabName = "cluster_pruebas_largas", icon = icon("water"))
      ),
      
      menuItem("Visualización de Carreras", tabName = "carreras", icon = icon("stopwatch"))
    )
  ),
  body = dashboardBody(
    # Aplicamos color de fondo general
    tags$style(HTML(sprintf("body { background-color: %s; }", colores_wa$fondo))),
    
    tabItems(
      
      
## Página 1. Inicio ------------------------------------------------------------
      tabItem(tabName = "contexto",
              fluidRow(
                column(
                  width = 8, offset = 2,
                  box(
                    title = tagList(shiny::icon("water"), "Bienvenido a From Splash to Stats"),
                    width = 12, 
                    status = "primary", 
                    solidHeader = TRUE,
                    elevation = 4,
                    collapsible = FALSE,
                    background = "white",
                    tags$div(
                      style = "text-align:center;",
                      img(src = "logo.jpg", height = "200px", style = "margin-bottom: 25px;"),
                      h2(style = paste0("color:", colores_wa$azul_oscuro, "; font-weight: 700;"), "From Splash to Stats"),
                      p(
                        style = "font-size:16px; line-height:1.6; color:#333333; text-align:justify;",
                        "Esta aplicación interactiva se centra en el análisis de las ",
                        strong("World Swimming Championships 2024"),
                        " (piscina corta, 25 metros), celebradas en la ",
                        em("Duna Arena de Budapest"),
                        " bajo la organización de ",
                        strong("World Aquatics"),
                        " durante los días 10 al 15 de diciembre de 2024. Los datos han sido recopilados desde la plataforma oficial ",
                        a(href = "https://www.omegatiming.com", "Omega Timing", target = "_blank"),
                        ", procesados y limpiados con el objetivo de ofrecer una visión amplia y rigurosa del rendimiento competitivo."
                      ),
                      p(
                        style = "font-size:16px; line-height:1.6; color:#333333; text-align:justify;",
                        "A través de esta plataforma podrás acceder a resultados oficiales por serie, rankings, medalleros, y perfiles individuales de los nadadores. Además, se incluyen análisis exploratorios de los datos (EDA), y módulos de aprendizaje automático como ",
                        strong("clustering"),
                        " para detectar estrategias de competición en distintas pruebas. También se incorpora una herramienta de clustering dinámico en streaming para visualizar carreras en tiempo real y estudiar la evolución de los nadadores durante la prueba."
                      ),
                      tags$hr(style = "border-top: 2px solid #0033A0; width: 60px; margin: 30px auto;")
                    )
                  )
                )
              )
      ),
      
      

## Página 2. Resultados --------------------------------------------------------
      tabItem(tabName = "resultados",
              fluidRow(
                column(
                  width = 12,
                  tags$div(
                    style = "text-align: center; margin-bottom: 20px;",
                    h2(style = "font-weight: 800; color: #002F6C;", "🏊 Resultados"),
                    p(style = "font-size: 16px; max-width: 800px; margin: auto; color: #333;",
                      "Aquí podrás encontrar los resultados individuales completos de la competición de cada serie nadada, ordenados por orden de llegada, con sus tiempos finales y parciales.")
                  )
                )
              ),
              
              fluidRow(
                column(
                  width = 3,
                  box(
                    title = "Filtros",
                    width = 12,
                    status = "primary",
                    solidHeader = TRUE,
                    selectInput("Resultados_FiltroEstilo", "Estilo", estilos),
                    selectInput("Resultados_FiltroDistancia", "Distancia", choices = NULL),
                    selectInput("Resultados_FiltroGenero", "Sexo", choices = NULL),
                    selectInput("Resultados_FiltroRonda", "Ronda", choices = NULL),
                    selectInput("Resultados_FiltroHeat", "Heat", choices = NULL)
                  )
                ),
                column(
                  width = 9,
                  box(
                    title = textOutput("Resultados_Titulo"),
                    width = 12,
                    status = "primary",
                    solidHeader = TRUE,
                    uiOutput("Resultados_Tabla")
                  )
                )
              )
      ), 

## Página 3. Perfil de nadadores -----------------------------------------------

      tabItem(tabName = "perfil_nadadores",
              fluidRow(
                column(
                  width = 12,
                  tags$div(
                    style = "text-align: center; margin-bottom: 20px;",
                    h2(style = "font-weight: 800; color: #002F6C;", "🔎 Perfil de Nadadores"),
                    p(style = "font-size: 16px; max-width: 800px; margin: auto; color: #333;",
                      "Busca nadadores por nombre o apellido y explora en orden cronológico todas las pruebas que han nadado, junto con sus datos personales relevantes.")
                  )
                )
              ),
              
              fluidRow(
                column(
                  width = 3,
                  box(
                    title = "Buscador de Nadadores",
                    width = 12,
                    status = "primary",
                    solidHeader = TRUE,
                    textInput("firstname_input", "Nombre", ""),
                    textInput("lastname_input", "Apellido", "")
                  )
                ),
                
                column(
                  width = 9,
                  box(
                    title = "Resultados de Búsqueda",
                    width = 12,
                    status = "primary",
                    solidHeader = TRUE,
                    uiOutput("output_nadadores")
                  )
                )
              )
      ),
      
## Página 4. Medalleros --------------------------------------------------------

### Página 4.1 Medallero por nadadores -----------------------------------------

tabItem(
  tabName = "medallero_nadadores",

  fluidRow(
    column(
      width = 12,
      div(
        style = "text-align: center; margin-top: 10px;",
        h2(style = "font-weight: 800; color: #002F6C;", "👤 Medallero. Nadadores"),
        p(
          "Explora qué nadadores han obtenido más medallas durante la competición. ",
          "Puedes filtrar los resultados para mostrar únicamente los mejores clasificados.",
          style = "font-size: 16px; color: #555;"
        )
      )
    )
  ),
  
  fluidRow(
    column(
      width = 3,
      box(
        title = "Filtros", status = "primary", solidHeader = TRUE, width = 12,
        selectInput(
          inputId = "top_nadadores",
          label = "Mostrar:",
          choices = c("Top 3" = 3, "Top 5" = 5, "Top 10" = 10, "Todos" = "All"),
          selected = 5
        )
      )
    ),
    column(
      width = 9,
      uiOutput("medallero_nadadores_ui")
    )
  )
)

,
      
### Página 4.2 Medallero por países --------------------------------------------

tabItem(
  tabName = "medallero_paises",
  
  fluidRow(
    column(
      width = 12,
      div(
        style = "text-align: center; margin-top: 10px;",
        h2(style = "font-weight: 800; color: #002F6C;", "🌍 Medallero. Países"),
        p(
          "Consulta el medallero acumulado por países en los Campeonatos del Mundo. ",
          "Puedes filtrar los resultados para mostrar únicamente los países con más medallas.",
          style = "font-size: 16px; color: #555;"
        ) 
      )
    )
  ),
  
  fluidRow(
    column(
      width = 3,
      box(
        title = "Filtros", status = "primary", solidHeader = TRUE, width = 12,
        selectInput(
          inputId = "top_paises",
          label = "Mostrar:",
          choices = c("Top 3" = 3, "Top 5" = 5, "Top 10" = 10, "Todos" = "All"),
          selected = 5
        )
      )
    ),
    column(
      width = 9,
      uiOutput("medallero_paises_ui")
    )
  )
)

,
      

## Página 5. Ránkings ----------------------------------------------------------

      tabItem(tabName = "rankings",
              fluidRow(
                column(
                  width = 12, offset = 0,
                  tags$div(
                    style = "text-align: center; margin-bottom: 20px;",
                    h2(style = "font-weight: 800; color: #002F6C;", "🏆 RÁNKINGS"),
                    p(style = "font-size: 16px; max-width: 800px; margin: auto; color: #333;",
                      "Consulta los rankings de nadadores y países basados en la media de puntos FINA obtenida durante el Mundial de Budapest 2024. 
           Puedes personalizar la vista seleccionando el tipo de ranking y el número de posiciones que deseas visualizar.")
                  )
                )
              ),
              
              fluidRow(
                column(
                  width = 3,
                  box(
                    title = "Filtros", status = "primary", solidHeader = TRUE, width = 12,
                    selectInput("ranking_tipo", "Ver ranking por media de puntos de:",
                                choices = c("Nadadores", "Paises"),
                                selected = "Nadadores"),
                    selectInput("ranking_top", "Selecciona el Top:",
                                choices = c("Top 3" = 3, "Top 5" = 5, "Top 10" = 10, "Todos" = 999),
                                selected = 3)
                  )
                ),
                
                column(
                  width = 9,
                  box(
                    title = textOutput("ranking_titulo"), width = 12,
                    status = "primary", solidHeader = TRUE,
                    uiOutput("ranking_vista")
                  )
                )
              )
      )
      
      ,
      
## Página 6. EDA ---------------------------------------------------------------

### Página 6.1 EDA. Nadadores --------------------------------------------------

tabItem(
  tabName = "eda_nadadores",
  
  # Título principal
  fluidRow(
    column(
      width = 12,
      div(
        style = "text-align: center; margin-top: 10px;",
        h2(
          style = "font-weight: 800; color: #002F6C;",
          "📊 Análisis de Nadadores"
        ),
        uiOutput("eda_nadadores_explicacion")
      )
    )
  ), 
  br(),
  
  fluidRow(
    column(
      width = 8,
      box(
        title = "Gráficos sobre la edad de los nadadores", width = 12, status = "primary", solidHeader = TRUE,
        fluidRow(
          column(width = 6, plotlyOutput("grafico_densidad")),
          column(width = 6, plotlyOutput("grafico_densidad_genero"))
        )
      )
    ),

    column(
      width = 4,
      box(
        title = "Resumen", width = 12, status = "primary", solidHeader = TRUE,
        uiOutput("eda_nadadores_resumen")
      )
    )
  ), 
  br(),
  
  fluidRow(
    column(
      width = 12,
      box(
        title = "Mapa de nadadores por país", width = 12, status = "primary", solidHeader = TRUE,
        leafletOutput("eda_nadadores_mapa", height = 500)
      )
    )
  )
)
      ,
      
### Página 6.2 EDA. Pruebas ----------------------------------------------------

tabItem(
  tabName = "eda_pruebas",
  
  fluidRow(
    column(
      width = 12,
      div(
        style = "text-align: center; margin-top: 10px;",
        h2(
          style = "font-weight: 800; color = #002F6C;",
          "🔍 Análisis de Pruebas"
        ),
        uiOutput("eda_pruebas_explicacion")
      )
    )
  ),
  br(),
  

  fluidRow(
    column(
      width = 8,
      box(
        title = "Gráficos sobre la densidad del tiempo de Reacción.", 
        width = 12, status = "primary", solidHeader = TRUE,
        fluidRow(
          column(width = 6, plotlyOutput("eda_pruebas_grafico1a", height = "300px")),
          column(width = 6, plotlyOutput("eda_pruebas_grafico1b", height = "300px"))
        )
      )
    ),
    column(
      width = 4,
      box(
        title = "Resumen",
        width = 12, status = "primary", solidHeader = TRUE,
        uiOutput("eda_pruebas_resumen1")
      )
    )
  ),
  br(),
  
  fluidRow(
    column(
      width = 8,
      box(
        title = "Boxplots sobre el tiempo de reacción agrupado por prueba/estilo", 
        width = 12, status = "primary", solidHeader = TRUE,
        fluidRow(
          column(width = 6, plotlyOutput("eda_pruebas_grafico2a", height = "300px")),
          column(width = 6, plotlyOutput("eda_pruebas_grafico2b", height = "300px"))
        )
      )
    ),
    column(
      width = 4,
      box(
        title = "Resumen",
        width = 12, status = "primary", solidHeader = TRUE,
        uiOutput("eda_pruebas_resumen2")
      )
    )
  ),
  br(),
  
  fluidRow(
    column(
      width = 8,
      box(
        title = "Edad óptima según la media de puntos", 
        width = 12, status = "primary", solidHeader = TRUE,
        plotlyOutput("eda_pruebas_grafico3", height = "300px")
      )
    ),
    column(
      width = 4,
      box(
        title = "Resumen",
        width = 12, status = "primary", solidHeader = TRUE,
        uiOutput("eda_pruebas_resumen3")
      )
    )
  )
)

,

### Página 6.3 EDA. Road to Champion --------------------------------------------


tabItem(tabName = "eda_campeon",
        
        fluidRow(
          column(
            width = 12,
            align = "center",
            h2("🚀 Road to Champion", 
               style = "color: #002F6C; font-weight: 700; font-size: 28px; 
            text-align: center; margin-top: 10px; margin-bottom: 5px;")
            ,
            p("Navega entre los campeones mundiales de cada prueba para conocer más acerca de sus estrategias para hacerse con el oro.",
              style = "font-size: 16px; font-style: italic; text-align: center;")
          )
        ),
        
        br(),
        
        fluidRow(
          column(
            width = 12,
            tags$div(style = "display: flex; align-items: center; gap: 25px; padding-bottom: 20px;",
                     htmlOutput("texto_campeon", style = "color: #002F6C; font-weight: bold; font-size: 24px; margin: 0;"),
                     uiOutput("foto_campeona")  # El tamaño se ajusta desde el server
            )
          )
        ),
        
        fluidRow(
          column(
            width = 4,
            selectInput(
              inputId = "filtro_distancia",
              label = "Selecciona la distancia:",
              choices = NULL,
              selected = NULL,
              width = "100%"
            )
          ),
          column(
            width = 4,
            selectInput(
              inputId = "filtro_stroke",
              label = "Selecciona el estilo:",
              choices = NULL,
              selected = NULL,
              width = "100%"
            )
          ),
          column(
            width = 4,
            selectInput(
              inputId = "filtro_genero",
              label = "Selecciona el género:",
              choices = NULL,
              selected = NULL,
              width = "100%"
            )
          )
        ),
        
        br(),
        
        fluidRow(
          bs4Card(
            title = "Comparativa de parciales. Pruebas nadadas hasta la final.",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            elevation = 2,
            plotlyOutput("plot_parciales_round", height = "400px")
          )
        ),
        
        fluidRow(
          bs4Card(
            title = "Análisis del rendimiento del nadador",
            width = 12,
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            elevation = 2,
            htmlOutput("texto_analisis_campeon")
          )
        ),
        
        fluidRow(
          bs4Card(
            title = "Evolución de medallas durante la final.",
            width = 6,
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            elevation = 2,
            plotlyOutput("plot_evolucion_carrera", height = "350px")
          ),
          bs4Card(
            title = "Animación de la carrera",
            width = 6,
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            elevation = 2,
            uiOutput("gif_animacion_carrera")
          )
        )
)



,


## Página 7. Clustering ---------------------------------------------------------

### Página 7.1 Pruebas Cortas. --------------------------------------------------

tabItem(tabName = "cluster_pruebas_cortas",
        
        fluidRow(
          column(
            width = 12,
            tags$h2("🧠 Pruebas Cortas (50-100m)", 
                    style = "font-weight: bold; color: #003366; margin-bottom: 10px;"),
            tags$p("En esta página podrás explorar estrategias de carrera en pruebas cortas de natación (50 y 100 metros). 
                    Usa los filtros para seleccionar una prueba concreta del Mundial. A partir de ahí:",
                   tags$ul(
                     tags$li("Se generará un dendrograma aglomerativo que agrupa a los nadadores según la similitud en su estrategia."),
                     tags$li("Podrás elegir el número de grupos a formar y analizar sus características."),
                     tags$li("Finalmente, puedes introducir tus propios parciales para ver a qué grupo perteneces y obtener un consejo personalizado.")
                   ),
                   style = "font-size: 16px; color: #444; margin-bottom: 25px;")
          )
        ),
        
        fluidRow(
          box(width = 4, title = "⚙️ Configuración inicial", status = "primary", solidHeader = TRUE,
              selectInput("cortas_prueba", "Selecciona la prueba", choices = c("50", "100")),
              selectInput("cortas_genero", "Selecciona un género", choices =  c("F", "M")),
              selectInput("cortas_estilo", "Selecciona el estilo", choices = c("FREE", "BACK", "BREAST", "FLY", "MEDLEY")),
              selectInput("cortas_metrica", "Métrica de distancia", choices = c("Euclidean", "DTW")),
              actionButton("cortas_generar_dendrograma", "Generar Dendrograma", icon = icon("project-diagram"))
          ),
          
          box(width = 8, title = "🌿 Dendrograma interactivo", status = "primary", solidHeader = TRUE,
              plotOutput("cortas_dendrograma_plot"),
              sliderInput("cortas_k_clusters", "Número de clusters", min = 2, max = 10, value = 3),
              actionButton("cortas_aplicar_clustering", "Aplicar Clustering", icon = icon("layer-group"))
          )
        ),
        
        fluidRow(
          box(width = 8, status = "primary", solidHeader = TRUE,
              title = "📊 Análisis de clusters",
              tabsetPanel(
                tabPanel("Centroides detallados", plotOutput("cortas_centroides_plot_nuevo")),
                tabPanel("Resumen clásico", plotOutput("cortas_centroides_plot"))
              )
          ),
          
          box(width = 4, title = "📝 Análisis automático", status = "primary", solidHeader = TRUE,
              uiOutput("cortas_texto_automatico")
          )
        ),

        fluidRow(
          box(width = 4, title = "🧍 Introduce tus parciales", status = "info", solidHeader = TRUE,
              textInput("cortas_input_parciales", 
                        label = "Introduce tus parciales separados por punto y coma (;)", 
                        placeholder = "Ej: 6.3;12.8;18.9;25.0"),
              actionButton("cortas_asignar_cluster", "Calcular cluster más similar", icon = icon("user-check")),
              br(), br(),
              uiOutput("cortas_cluster_asignado_texto")
          ),
          
          box(width = 8, title = "📈 Comparación visual con el cluster asignado", status = "info", solidHeader = TRUE,
              plotOutput("cortas_grafico_cluster_usuario"))
        )
        
        
)
, 
      

### Página 7.2 Pruebas Media-Distancia ------------------------------------------

tabItem(tabName = "cluster_pruebas_medio",
        fluidRow(
          column(
            width = 12,
            tags$h2("🧠 Pruebas Medias (200-400m)", 
                    style = "font-weight: bold; color: #003366; margin-bottom: 10px;"),
            tags$p("En esta página podrás explorar estrategias de carrera en pruebas cortas de natación (200 y 400 metros). 
                    Usa los filtros para seleccionar una prueba concreta del Mundial. A partir de ahí:",
                   tags$ul(
                     tags$li("Se generará un dendrograma aglomerativo que agrupa a los nadadores según la similitud en su estrategia."),
                     tags$li("Podrás elegir el número de grupos a formar y analizar sus características."),
                     tags$li("Finalmente, puedes introducir tus propios parciales para ver a qué grupo perteneces y obtener un consejo personalizado.")
                   ),
                   style = "font-size: 16px; color: #444; margin-bottom: 25px;")
          )
        ),
        
        fluidRow(
          box(width = 4, title = "⚙️ Configuración inicial", status = "primary", solidHeader = TRUE,
              selectInput("medio_prueba", "Selecciona la prueba", choices = c("200", "400")),
              selectInput("medio_genero", "Selecciona un género", choices = c("F", "M")),
              selectInput("medio_estilo", "Selecciona el estilo", choices = c("FREE", "BACK", "BREAST", "FLY", "MEDLEY")),
              selectInput("medio_metrica", "Métrica de distancia", choices = c("Euclidean", "DTW", "DTW (suavizado)")),
              actionButton("medio_generar_dendrograma", "Generar Dendrograma", icon = icon("project-diagram"))
          ),
          
          box(width = 8, title = "🌿 Dendrograma interactivo", status = "primary", solidHeader = TRUE,
              plotOutput("medio_dendrograma_plot"),
              sliderInput("medio_k_clusters", "Número de clusters", min = 2, max = 10, value = 3),
              actionButton("medio_aplicar_clustering", "Aplicar Clustering", icon = icon("layer-group"))
          )
        ),
        
        fluidRow(
          box(width = 8, status = "primary", solidHeader = TRUE,
              title = "📊 Análisis de clusters",
              tabsetPanel(
                tabPanel("Centroides detallados", plotOutput("medio_centroides_plot_nuevo")),
                tabPanel("Resumen clásico", plotOutput("medio_centroides_plot"))
              )
          ),
          
          box(width = 4, title = "📝 Análisis automático", status = "primary", solidHeader = TRUE,
              uiOutput("medio_texto_automatico")
          )
        )
),

### Página 7.3 Pruebas Larga Distancia ------------------------------------------

tabItem(tabName = "cluster_pruebas_largas",
        fluidRow(
          column(
            width = 12,
            tags$h2("🧠 Pruebas Largas (800-1500m)", 
                    style = "font-weight: bold; color: #003366; margin-bottom: 10px;"),
            tags$p("En esta página podrás explorar estrategias de carrera en pruebas cortas de natación (800 y 1500 metros). 
                    Usa los filtros para seleccionar una prueba concreta del Mundial. A partir de ahí:",
                   tags$ul(
                     tags$li("Se generará un dendrograma aglomerativo que agrupa a los nadadores según la similitud en su estrategia."),
                     tags$li("Podrás elegir el número de grupos a formar y analizar sus características."),
                     tags$li("Finalmente, puedes introducir tus propios parciales para ver a qué grupo perteneces y obtener un consejo personalizado.")
                   ),
                   style = "font-size: 16px; color: #444; margin-bottom: 25px;")
          )
        ),
        
        fluidRow(
          box(width = 4, title = "⚙️ Configuración inicial", status = "primary", solidHeader = TRUE,
              selectInput("larga_prueba", "Selecciona la prueba", choices = c("800", "1500")),
              selectInput("larga_genero", "Selecciona un género", choices = c("F", "M")),
              selectInput("larga_estilo", "Selecciona el estilo", choices = c("FREE")),
              selectInput("larga_metrica", "Métrica de distancia", choices = c("Euclidean","Euclidean (suavizado)", "DTW", "DTW (suavizado)")),
              actionButton("larga_generar_dendrograma", "Generar Dendrograma", icon = icon("project-diagram"))
          ),
          
          box(width = 8, title = "🌿 Dendrograma interactivo", status = "primary", solidHeader = TRUE,
              plotOutput("larga_dendrograma_plot"),
              sliderInput("larga_k_clusters", "Número de clusters", min = 2, max = 10, value = 3),
              actionButton("larga_aplicar_clustering", "Aplicar Clustering", icon = icon("layer-group"))
          )
        ),
        
        fluidRow(
          box(width = 8, status = "primary", solidHeader = TRUE,
              title = "📊 Análisis de clusters",
              tabsetPanel(
                tabPanel("Centroides detallados", plotOutput("larga_centroides_plot_nuevo")),
                tabPanel("Resumen clásico", plotOutput("larga_centroides_plot"))
              )
          ),
          
          box(width = 4, title = "📝 Análisis automático", status = "primary", solidHeader = TRUE,
              uiOutput("larga_texto_automatico")
          )
        )
),



## Página 8. EvolveCluster ------------------------------------------------------

tabItem(
  tabName = "carreras",
  

  fluidRow(
    column(
      width = 12,
      div(
        style = "text-align: center; margin-top: 10px;",
        h2(style = "font-weight: 800; color: #002F6C;", "🏊‍♂️ Visualización de Carreras con Clustering en Streaming")
      )
    )
  ),
  

  fluidRow(
    column(
      width = 10, offset = 1,
      p("En esta página podrás explorar una forma innovadora de agrupar nadadores en distintos momentos de la prueba mediante un algoritmo de clustering adaptado a datos en streaming."),
      p("Filtra para ver una prueba concreta, elige un umbral adecuado basado en el Dunn Index, y genera la visualización."),
      p("🔎 Un valor alto de Dunn Index indica mejor separación entre clústeres.")
    )
  ),
  

  fluidRow(
    column(
      width = 10, offset = 1,
      box(
        title = tagList(icon("filter"), "Configuración de la Prueba"),
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        background = "white",
        fluidRow(
          column(4, selectInput("filtro_distance", "Distancia", choices = sort(unique(worlds2024Budapest25m$distance)), selected = 100)),
          column(4, selectInput("filtro_stroke", "Estilo", choices = sort(unique(worlds2024Budapest25m$stroke)), selected = "Breaststroke"))
        ),
        fluidRow(
          column(4, selectInput("filtro_gender", "Género", choices = sort(unique(worlds2024Budapest25m$gender)), selected = "Men")),
          column(4, sliderInput("slider_parcial", "Parciales (cada cuántos metros):", min = 25, max = 400, step = 25, value = 50))
        ),
        fluidRow(
          column(4, numericInput("k_inicial", "Número de Clústers Inicial (primer parcial):", value = 1, min = 1, max = 10, step = 1))
        ),
        fluidRow(
          column(4,
                 br(),
                 actionButton("calcular_dunn", "Calcular Índice Dunn", icon = icon("calculator"), class = "btn btn-primary")
          )
        )
      )
    )
  ),
  

  fluidRow(
    column(
      width = 6, offset = 1,
      box(
        title = tagList(icon("chart-bar"), "Índices de Validación de Clúster (Dunn Index)"),
        width = 12,
        status = "info",
        solidHeader = TRUE,
        background = "white",
        plotlyOutput("plot_indices_cluster", height = "400px")
      )
    ),
    column(
      width = 4,
      box(
        title = tagList(icon("sliders-h"), "Parámetro de Separación y Visualización"),
        width = 12,
        status = "info",
        solidHeader = TRUE,
        background = "white",
        numericInput("gap_cluster", "Gap de separación entre clústers:", value = 5, min = 1, max = 100),
        helpText("Este parámetro ajusta la sensibilidad del algoritmo de agrupamiento."),
        br(),
        actionButton("calcular_carrera", "Calcular Visualización", icon = icon("chart-line"), class = "btn btn-success btn-block")
      )
    )
  ),
  
  fluidRow(
    column(
      width = 10, offset = 1,
      box(
        title = tagList(icon("project-diagram"), "Visualización de la Carrera con Clustering Streaming"),
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        background = "white",
        plotOutput("grafica_clusters", height = "500px")
      )
    )
  )
)











    )
  )
)

      
# SERVER -----------------------------------------------------------------------

server <- function(input, output, session) {

  ## Página 2. Resultados ------------------------------------------------------
  
  ### Filtro Distancia. --------------------------------------------------------
  observeEvent(input$Resultados_FiltroEstilo, {
    distancias <- worlds2024Budapest25m %>%
      filter(stroke == input$Resultados_FiltroEstilo) %>%
      select(distance, round, heat)
  

    updateSelectInput(session, "Resultados_FiltroDistancia", choices = sort(unique(distancias$distance)))
    updateSelectInput(session, "Resultados_FiltroRonda", choices = sort(unique(distancias$round)))
    updateSelectInput(session, "Resultados_FiltroHeat", choices = sort(unique(distancias$heat)))
  })
  
  ### Filtro Sexo. -------------------------------------------------------------
  observeEvent(input$Resultados_FiltroDistancia, {
    if (!is.null(input$Resultados_FiltroDistancia)) {
      sexos <- worlds2024Budapest25m %>%
        filter(stroke == input$Resultados_FiltroEstilo,
               distance == input$Resultados_FiltroDistancia) %>%
        select(gender, round, heat)
      
      updateSelectInput(session, "Resultados_FiltroGenero", choices = sort(unique(sexos$gender)))
      updateSelectInput(session, "Resultados_FiltroRonda", choices = sort(unique(sexos$round)))
      updateSelectInput(session, "Resultados_FiltroHeat", choices = sort(unique(sexos$heat)))
    }
  })
  
  ### Filtro Ronda. ------------------------------------------------------------
  observeEvent(input$Resultados_FiltroGenero, {
    if (!is.null(input$Resultados_FiltroDistancia)) {
      rondas <- worlds2024Budapest25m %>%
        filter(stroke == input$Resultados_FiltroEstilo,
               distance == input$Resultados_FiltroDistancia,
               gender == input$Resultados_FiltroGenero) %>%
        select(round, heat) 
      
      updateSelectInput(session, "Resultados_FiltroRonda", choices = sort(unique(rondas$round)))
      updateSelectInput(session, "Resultados_FiltroHeat", choices = sort(unique(rondas$heat)))
    }
  })
  
  ### Filtro Serie. ------------------------------------------------------------
  observeEvent(input$Resultados_FiltroRonda, {
    if (!is.null(input$Resultados_FiltroGenero)) {
      series <- worlds2024Budapest25m %>%
        filter(stroke == input$Resultados_FiltroEstilo,
               distance == input$Resultados_FiltroDistancia,
               gender == input$Resultados_FiltroGenero,
               round == input$Resultados_FiltroRonda) %>%
        pull(heat) %>%
        unique()
      updateSelectInput(session, "Resultados_FiltroHeat", choices = sort(series))
    }
  })
  
  
  ### Texto Título. ------------------------------------------------------------
  output$Resultados_Titulo <- renderText({
    paste(input$Resultados_FiltroDistancia, "m ", input$Resultados_FiltroEstilo," ", input$Resultados_FiltroGenero," Serie:", 
          input$Resultados_FiltroHeat, " Ronda: ", input$Resultados_FiltroRonda)
  })
  
  ### Filtrado y muestra de los resultados. ------------------------------------
  
  output$Resultados_Tabla <- renderUI({
    df_filtrado <- worlds2024Budapest25m %>%
      filter(
        stroke == input$Resultados_FiltroEstilo,
        distance == input$Resultados_FiltroDistancia,
        gender == input$Resultados_FiltroGenero,
        round == input$Resultados_FiltroRonda,
        heat == input$Resultados_FiltroHeat
      ) %>%
      arrange(numericSwimtime)
    
    df_con_imagenes <- df_filtrado %>%
      left_join(imagenesNadadores, by = "athleteid") %>%
      arrange(numericSwimtime)
    
    df_con_imagenes <- df_con_imagenes %>%
      select(-firstname.y, -lastname.y) %>%
      rename(
        firstname = firstname.x,
        lastname = lastname.x
      )
    
    # Identificar si hay WR (más de 1000 puntos) y cuál es el primero
    df_con_imagenes <- df_con_imagenes %>%
      arrange(desc(points))  # ordenamos descendente por puntos para saber el primero con >1000
    
    wr_index <- which(df_con_imagenes$points > 1000)[1]  
    
    nadadores <- split(df_con_imagenes, factor(df_con_imagenes$athleteid, levels = unique(df_con_imagenes$athleteid)))
    
    ui_output <- lapply(seq_along(nadadores), function(i) {
      nadador <- nadadores[[i]]
      
      full_name <- paste(nadador$firstname[1], nadador$lastname[1])
      
      orden <- order(as.numeric(nadador$splitdistance))
      distancias <- nadador$splitdistance[orden]
      cums <- nadador$cumswimtime[orden]
      parciales <- nadador$parcialswimtime[orden]
      
      collapse_id <- paste0("parciales_", i)
      
      # ¿Es este nadador el WR?
      is_wr <- !is.na(wr_index) && (nadador$athleteid[1] == df_con_imagenes$athleteid[wr_index])
      
      tagList(
        tags$div(
          style = "
        border:1px solid #ccc; 
        border-radius:15px; 
        padding:20px; 
        margin-bottom:20px; 
        background-color:#002F6C; 
        color:white; 
        position:relative; 
        overflow:hidden;",
          
        
          tags$div(
            style = "position:absolute; top:10px; right:10px; display:flex; align-items:center; gap:8px;",
            
            # WR dorado a la izquierda de la foto
            if (is_wr) {
              tags$div(
                style = "
              background-color: gold; 
              color: black; 
              font-weight: bold; 
              padding: 6px 10px; 
              border-radius: 12px;
              box-shadow: 0 0 8px gold;
              white-space: nowrap;",
                "WR"
              )
            },
            
            # Foto en círculo
            tags$div(
              style = "width:80px; height:80px; border-radius:50%; overflow:hidden; background-color:white;",
              tags$img(src = nadador$foto[1], style = "width:100%; height:100%; object-fit:cover;")
            )
          ),
          
         
          tags$h3(
            style = "margin-bottom:5px; margin-right:110px;",  # margen derecho para no chocar con foto+WR
            paste0(i, ". ", full_name)
          ),
          
          tags$p(style = "margin-top:0; font-size:14px;",
                 paste0("País: ", nadador$clubname[1], " | Edad: ", nadador$edad[1], " | Puntos FINA: ", round(nadador$points[1], 1))
          ),
          
          tags$hr(style = "border-color: #ffffff55;"),
          
          tags$button(
            type = "button",
            class = "btn btn-sm",
            `data-toggle` = "collapse",
            `data-target` = paste0("#", collapse_id),
            style = "background-color:#00BFFF; color:white; border:none; padding:6px 12px; border-radius:8px;",
            "Mostrar parciales ⯈"
          ),
          
          tags$div(
            id = collapse_id,
            class = "collapse",
            style = "margin-top:15px;",
            
            tags$div(
              style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(120px, 1fr)); gap: 10px;",
              lapply(seq_along(distancias), function(j) {
                tags$div(
                  style = "background-color:#ffffff11; padding:10px; border-radius:8px;",
                  tags$p(
                    style = "margin:0; font-size:13px; font-weight:bold;",
                    paste0(distancias[j], " m — ", gsub("^00:", "", as.character(cums[j])))
                  ),
                  tags$p(
                    style = "margin:0; font-size:12px; color:#FFD700;",
                    paste0("Parcial: ", sprintf("%.2f", parciales[j]), "s")
                  )
                )
              })
            )
          ),
          
          tags$hr(style = "border-color: #ffffff33;"),
          
          tags$p(
            style = "font-size:24px; font-weight:bold; text-align:center; margin-top:15px;",
            paste("⏱ Tiempo Final:", gsub("^00:", "", as.character(nadador$swimtime[1])))
          )
        )
      )
    })
    
    return(ui_output)
  })
  
  
  
  
  ## Página 3. Perfil de nadadores. --------------------------------------------
  
  ### Buscado y muestreo de perfiles. ------------------------------------------
  output$output_nadadores <- renderUI({
    
    fname <- trimws(tolower(input$firstname_input))
    lname <- trimws(tolower(input$lastname_input))
    
    if (fname == "" && lname == "") {
      return(tags$p("Introduce al menos nombre o apellido para buscar."))
    }
    
    df_filtrado <- nadadoresPruebas %>%
      filter(
        (fname == "" | grepl(fname, tolower(firstname), fixed = TRUE)) &
          (lname == "" | grepl(lname, tolower(lastname),  fixed = TRUE))
      ) %>%
      left_join(imagenesNadadores, by = c("firstname", "lastname"))
    
    if (!nrow(df_filtrado)) {
      return(tags$p("No se encontraron coincidencias."))
    }
    
    nadadores_unicos <- df_filtrado %>%
      group_by(firstname, lastname, clubname, gender, edad, foto) %>%
      summarise(info = list(cur_data_all()), .groups = "drop")
    
    lapply(seq_len(nrow(nadadores_unicos)), function(i) {
      nadador <- nadadores_unicos[i, ]
      info <- nadador$info[[1]] %>%
        arrange(daySesion, daytime)    # orden cronológico
      
      id <- paste0("collapse_", i)
      nombre <- paste(nadador$firstname, nadador$lastname)
      
      tags$div(
        class = "card mb-3",
        style = "background-color:#002F6C; color:white; padding:15px; border-radius:12px;",
        
        fluidRow(
          column(2,
                 tags$img(src = nadador$foto,
                          style = "width:100%; border-radius:50%; border:2px solid white;")
          ),
          column(10,
                 tags$h4(nombre, style = "margin-bottom:5px;"),
                 tags$p(paste("Edad:", nadador$edad)),
                 tags$p(paste("Club:", nadador$clubname)),
                 tags$p(paste("Género:", nadador$gender)),
                 
                 tags$button(
                   class = "btn btn-light btn-sm mt-2",
                   type = "button",
                   `data-toggle` = "collapse",
                   `data-target` = paste0("#", id),
                   "Más información"
                 ),
                 
                 tags$div(
                   class = "collapse mt-2",
                   id = id,
                   tags$ul(style = "padding-left:20px;",
                           lapply(seq_len(nrow(info)), function(j) {
                             fila <- info[j, ]
                             
                             medalla <- NULL
                             place <- as.integer(fila$place)
                             distancia <- as.integer(fila$distance)
                             ronda <- fila$round
                             
                             if (!is.na(place) && place %in% 1:3) {
                               if ((ronda == "FIN") || (distancia %in% c(800, 1500))) {
                                 medalla <- switch(as.character(place),
                                                   "1" = "🥇 ",
                                                   "2" = "🥈 ",
                                                   "3" = "🥉 ",
                                                   "")
                               }
                             }
                             
                             tags$li(
                               paste0(
                                 "El día ", as.character(fila$daySesion),
                                 ", a las ", fila$daytime,
                                 ", nadó la prueba de ", fila$distance, " m ", fila$stroke,
                                 " en la ronda ", fila$round,
                                 " con un tiempo de ", fila$swimtime,
                                 " obteniendo la posición ", place,
                                 if (!is.null(medalla)) medalla else "","."
                               )
                             )
                           })
                   )
                 )
          )
        )
      )
    })
  })
  
  
  
  ## Página 4. Medalleros. -----------------------------------------------------
  
  ### Página 4.1 Medallero por nadadores. --------------------------------------
  
  #### Filtrado de nadadores ---------------------------------------------------
  medallero_nadadores_filtrado <- reactive({
    medalleroNadadores <- medalleroMundial %>%
      mutate(
        ORO = ifelse(place == 1, 1, 0),
        PLATA = ifelse(place == 2, 1, 0),
        BRONCE = ifelse(place == 3, 1, 0)
      ) %>%
      group_by(lastname, firstname) %>%
      summarise(
        ORO = sum(ORO),
        PLATA = sum(PLATA),
        BRONCE = sum(BRONCE),
        .groups = "drop"
      ) %>%
      arrange(desc(ORO), desc(PLATA), desc(BRONCE))
    
    
    medallero <- merge(medalleroNadadores, imagenesNadadores, by = c("firstname", "lastname"), all.x = TRUE)
    
    # Lo ordeno: 
    
    medallero<- medallero  %>% arrange(desc(ORO), desc(PLATA), desc(BRONCE))
    
    
    df <- medallero  
    df$posicion <- seq_len(nrow(df))  
    
    if (input$top_nadadores != "All") {
      df <- head(df, as.numeric(input$top_nadadores))
    }
    df
  })
  
  #### Muestra de los resultados -----------------------------------------------
  output$medallero_nadadores_ui <- renderUI({
    df <- medallero_nadadores_filtrado()
    
    tags$div(
      class = "medallero-tabla",
      style = "width: 100%; background-color: #001f3f; padding: 20px; border-radius: 12px; color: white; font-family: 'Segoe UI', sans-serif;",
      
      # Cabecera
      tags$div(
        style = "display: flex; font-weight: bold; padding: 12px 0; border-bottom: 2px solid #00bfff; background-color: #002b5c;",
        tags$div(style = "width: 6%; text-align: center;", "Pos."),
        tags$div(style = "width: 14%; text-align: center;", "Foto"),
        tags$div(style = "width: 30%;", "Nombre"),
        tags$div(style = "width: 12%; text-align: center;", "🥇 Oro"),
        tags$div(style = "width: 12%; text-align: center;", "🥈 Plata"),
        tags$div(style = "width: 12%; text-align: center;", "🥉 Bronce"),
        tags$div(style = "width: 14%; text-align: center;", "Total")
      ),
      
      # Filas de nadadores
      lapply(seq_len(nrow(df)), function(i) {
        nadador <- df[i, ]
        
        tags$div(
          style = "display: flex; align-items: center; padding: 12px 0; border-bottom: 1px solid #003366;",
          tags$div(style = "width: 6%; text-align: center; font-weight: bold; color: #00bfff;", nadador$posicion),
          tags$div(
            style = "width: 14%; text-align: center;",
            tags$img(src = nadador$foto, height = "60px", style = "border-radius: 50%; border: 2px solid #00bfff;")
          ),
          tags$div(style = "width: 30%; font-size: 16px;", paste(nadador$firstname, nadador$lastname)),
          tags$div(style = "width: 12%; text-align: center;", nadador$ORO),
          tags$div(style = "width: 12%; text-align: center;", nadador$PLATA),
          tags$div(style = "width: 12%; text-align: center;", nadador$BRONCE),
          tags$div(style = "width: 14%; text-align: center; font-weight: bold;", nadador$ORO + nadador$PLATA + nadador$BRONCE)
        )
      })
    )
  })
  
  
  ### Página 4.2 Medallero por países. -----------------------------------------
  
  #### Filtrado de países ------------------------------------------------------
  
  medallero_paises_filtrado <- reactive({
    medalleroPaises <- medalleroMundial %>%
      mutate(
        ORO = ifelse(place == 1, 1, 0),
        PLATA = ifelse(place == 2, 1, 0),
        BRONCE = ifelse(place == 3, 1, 0)
      ) %>%
      group_by(clubname, clubnation, daySesion) %>%
      summarise(
        ORO = sum(ORO),
        PLATA = sum(PLATA),
        BRONCE = sum(BRONCE),
        .groups = "drop"
      ) %>%
      arrange(daySesion,desc(ORO), desc(PLATA), desc(BRONCE))
    # Hago el medallero total: 
    medalleroPaisesAcumulado <- medalleroPaises %>%
      group_by(clubname) %>%  # Agrupar por país
      summarise(
        ORO = sum(ORO, na.rm = TRUE),
        PLATA = sum(PLATA, na.rm = TRUE),
        BRONCE = sum(BRONCE, na.rm = TRUE)
      ) %>%
      arrange(desc(ORO), desc(PLATA), desc(BRONCE))  # Ordenar por importancia
    
    
    medalleroPaisesAcumulado$Code <- tolower(countrycode(medalleroPaisesAcumulado$clubname, "country.name", "iso2c"))
    #Los neutral athletes los elimino. 
    medalleroPaisesAcumulado<- na.omit(medalleroPaisesAcumulado)
    
    #Meto los códigos Iso. 
    medalleroPaisesAcumulado <- medalleroPaisesAcumulado %>%
      mutate(flag_url = paste0("https://flagcdn.com/w40/", Code, ".png"))
    
    df <- medalleroPaisesAcumulado
    if (input$top_paises != "All") {
      df <- head(df, as.numeric(input$top_paises))
    }
    df
  })
  
  #### Muestra de los resultados -----------------------------------------------
  output$medallero_paises_ui <- renderUI({
    df <- medallero_paises_filtrado() 
    
    
    df$posicion <- seq_len(nrow(df))
    
    tags$div(
      class = "medallero-tabla",
      style = "width: 100%; background-color: #001f3f; padding: 20px; border-radius: 12px; color: white; font-family: 'Segoe UI', sans-serif;",
      
      # Cabecera
      tags$div(
        style = "display: flex; font-weight: bold; padding: 12px 0; border-bottom: 2px solid #00bfff; background-color: #002b5c;",
        tags$div(style = "width: 6%; text-align: center;", "Pos."),
        tags$div(style = "width: 14%; text-align: center;", "Bandera"),
        tags$div(style = "width: 30%;", "País"),
        tags$div(style = "width: 12%; text-align: center;", "🥇 Oro"),
        tags$div(style = "width: 12%; text-align: center;", "🥈 Plata"),
        tags$div(style = "width: 12%; text-align: center;", "🥉 Bronce"),
        tags$div(style = "width: 14%; text-align: center;", "Total")
      ),
      
      # Filas de países
      lapply(seq_len(nrow(df)), function(i) {
        pais <- df[i, ]
        
        tags$div(
          style = "display: flex; align-items: center; padding: 12px 0; border-bottom: 1px solid #003366;",
          tags$div(style = "width: 6%; text-align: center; font-weight: bold; color: #00bfff;", pais$posicion),
          tags$div(
            style = "width: 14%; text-align: center;",
            tags$img(src = pais$flag_url, height = "40px", style = "border-radius: 4px; border: 1px solid #00bfff;")
          ),
          tags$div(style = "width: 30%; font-size: 16px;", pais$clubname),
          tags$div(style = "width: 12%; text-align: center;", pais$ORO),
          tags$div(style = "width: 12%; text-align: center;", pais$PLATA),
          tags$div(style = "width: 12%; text-align: center;", pais$BRONCE),
          tags$div(style = "width: 14%; text-align: center; font-weight: bold;", pais$ORO + pais$PLATA + pais$BRONCE)
        )
      })
    )
  })
  

  ## Página 5. Ránkings
  
  ### Creación del Ránking según los filtros -----------------------------------
  
  ranking_df <- reactive({
    top_n <- as.numeric(input$ranking_top)
    
    if (input$ranking_tipo == "Nadadores") {
      ranking <- nadadoresPruebas %>%
        group_by(lastname, firstname) %>%
        summarize(mediaPuntos = mean(points, na.rm = TRUE), .groups = "drop") %>%
        left_join(imagenesNadadores, by = c("firstname", "lastname")) %>%
        arrange(desc(mediaPuntos)) %>%
        mutate(posicion = row_number()) %>%
        slice_head(n = top_n)
      
    } else {
      ranking <- nadadoresPruebas %>%
        group_by(clubname) %>%
        summarize(mediaPuntos = mean(points, na.rm = TRUE), .groups = "drop") %>%
        mutate(Code = tolower(countrycode(clubname, "country.name", "iso2c"))) %>%
        na.omit() %>%
        mutate(flag = paste0("https://flagcdn.com/w80/", Code, ".png")) %>%
        arrange(desc(mediaPuntos)) %>%
        mutate(posicion = row_number()) %>%
        slice_head(n = top_n)
    }
    
    return(ranking)
  })
  
  
  ### Título según filtros. ----------------------------------------------------
  output$ranking_titulo <- renderText({
    tipo <- if (input$ranking_tipo == "Nadadores") "nadadores" else "países"
    
    if (input$ranking_top == 999) {
      paste0("Ránking por media de puntos de ", tipo, ".")
    } else {
      paste0("Ránking por media de puntos de ", tipo, ". Top ", input$ranking_top)
    }
  })
  
  ### Muestra de los ránkings. -------------------------------------------------
  output$ranking_vista <- renderUI({
    df <- ranking_df()
    
    cards <- lapply(1:nrow(df), function(i) {
      if (input$ranking_tipo == "Nadadores") {
        nombre <- paste(df$firstname[i], df$lastname[i])
        imagen <- df$foto[i]
      } else {
        nombre <- df$clubname[i]
        imagen <- df$flag[i]
      }
      
      # Colores de fondo para top 3
      bg_color <- case_when(
        df$posicion[i] == 1 ~ "#ffe066",  
        df$posicion[i] == 2 ~ "#d6d6d6",  
        df$posicion[i] == 3 ~ "#cd7f32",  
        TRUE ~ "#002b5c"                 
      )
      
      # Color del texto
      text_color <- ifelse(df$posicion[i] <= 3, "#000", "#f0f0f0")
      
      # Posición con emoji solo para top 3
      posicion_texto <- case_when(
        df$posicion[i] == 1 ~ "🥇",
        df$posicion[i] == 2 ~ "🥈",
        df$posicion[i] == 3 ~ "🥉",
        TRUE ~ paste0("#", df$posicion[i])
      )
      
      # Tarjeta del ranking
      div(class = "ranking-card",
          style = paste0("
          background-color:", bg_color, ";
          color:", text_color, ";
          border-radius: 20px;
          padding: 20px;
          text-align: center;
          box-shadow: 0 4px 12px rgba(0, 0, 0, 0.15);
          width: 100%;
          max-width: 280px;
        "),
          
          # Posición
          tags$div(style = "font-size: 24px; font-weight: bold; margin-bottom: 10px;", posicion_texto),
          
          # Imagen circular
          tags$img(src = imagen,
                   style = "height: 90px; width: 90px; object-fit: cover; border-radius: 50%; border: 3px solid white; margin-bottom: 10px;"),
          
          # Nombre
          tags$div(style = "font-size: 18px; font-weight: 600; margin-bottom: 5px;", nombre),
          
          # Media de puntos
          tags$div(style = "font-size: 15px; font-style: italic;", paste("Media de puntos:", round(df$mediaPuntos[i], 2)))
      )
    })
    
    
    div(style = "
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(260px, 1fr));
        gap: 25px;
        justify-items: center;
        padding: 20px;
      ", 
        cards
    )
  })
  
  
  ## Página 6. EDA. ------------------------------------------------------------
  
  ### Página 6.1. EDA. Nadadores -----------------------------------------------

  #### Resumen inicial. --------------------------------------------------------
  output$eda_nadadores_explicacion <- renderUI({
    numNadadores <- dim(nadadoresParticipantes)[1]
    numNadadores_Genero <- summary(nadadoresParticipantes$gender)
    paste("En la actual sección, se analizan datos de la competición relativo a los nadadores
          participantes. Se podrán observar gráficas y mapas para obtener información acerca de la edad de los participantes
          o el número de nadadores por país. En los Worlds Aquatic Budapest 2024 p25 participaron ", numNadadores, " participantes, de los cuales, ",
          numNadadores_Genero[1], " mujeres y ", numNadadores_Genero[2], " hombres")
  })
  
  #### Colores para los gráficos. ----------------------------------------------
  colores_we <- c(
    "Masculino" = "#002F6C",  
    "Femenino" = "#0072CE"    
  )
  
  
  #### Gráfico 1: Densidad simple ----------------------------------------------
  output$grafico_densidad <- renderPlotly({
    p1 <- ggplot(nadadoresParticipantes, aes(x = edad)) +
      geom_density(fill = "#B3D7F5", color = "#0072CE", alpha = 0.6) +
      labs(title = "Gráfico de densidad", x = "Edad", y = "Densidad") +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", color = "#002F6C", size = 14)
      )
    
    ggplotly(p1) %>% layout(margin = list(t = 50))  
  })
  
  #### Gráfico 2: Densidad agrupada por género ---------------------------------
  output$grafico_densidad_genero <- renderPlotly({
    p2 <- ggplot(nadadoresParticipantes, aes(
      x = edad,
      fill = factor(gender, levels = c("M", "F"), labels = c("Masculino", "Femenino"))
    )) +
      geom_density(alpha = 0.5) +
      scale_fill_manual(values = colores_we) +
      labs(title = "Gráfico de densidad agrupada por género", x = "Edad", y = "Densidad", fill = "Género") +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", color = "#002F6C", size = 14)
      )
    
    ggplotly(p2) %>% layout(margin = list(t = 50))
  })
  
  #### Resumen sobre Edad. -----------------------------------------------------
  output$eda_nadadores_resumen <- renderUI({
    tags$ul(
      tags$li("La primera gráfica muestra la distribución general de la variable edad para todos los nadadores participantes. Se observa una distribución unimodal con asimetría hacia la derecha (sesgo positivo), indicando que hay más nadadores jóvenes, pero también algunos más mayores."),
      tags$li("En la segunda gráfica, se visualizan las distribuciones de edad agrupadas por género (Masculino y Femenino), mostrando diferencias claras en sus patrones."),
      tags$li("Un T-Test para muestras independientes confirma que existe una diferencia estadísticamente significativa entre las medias de edad de ambos grupos (p-valor < 0.001). La media de edad para mujeres es aproximadamente 20.7 años, mientras que para hombres es de 21.85 años, indicando que las nadadoras son, en promedio, más jóvenes que los nadadores en el Mundial de Budapest.")
    )
  })
  
  
  #### Mapa de nadadores por país. ---------------------------------------------
  output$eda_nadadores_mapa <- renderLeaflet({
    # Primero cuento los nadadores por país: 
    paisesParticipantes_eda_nadadores<- nadadoresParticipantes%>%
      group_by(clubname) %>%
      summarise(nadadores = n())
    # Los ordeno de mayor a menor: 
    paisesParticipantes_eda_nadadores <- paisesParticipantes_eda_nadadores %>%
      arrange(desc(nadadores))
    # Creo un dataframe nuevo: 
    world <- ne_countries(scale = "medium", returnclass = "sf")
    # Diccionario de equivalencias
    correcciones <- c(
      "United States" = "United States of America",
      "Korea" = "South Korea",
      "Hong Kong, China" = "Hong Kong",
      "Chinese Taipei" = "Taiwan",
      "Türkiye" = "Turkey",
      "Great Britain" = "United Kingdom",
      "Bosnia and Herzegovina" =  "Bosnia and Herz.",
      "Antigua and Barbuda"=  "Antigua and Barb.",
      "DPR Korea" = "North Korea",
      "Dominican Republic"= "Dominican Rep.",
      "Lao PDR" = "Laos",
      "Macau" =   "Macao",
      "Northern Mariana Islands" =   "N. Mariana Is.",
      "Republic of Moldova" = "Moldova",
      "Brunei Darussalam" = "Brunei",
      "Cayman Islands" =   "Cayman Is.",
      "Cook Islands" =   "Cook Is.",
      "Faroe Islands" =   "Faeroe Is.",
      "U.S. Virgin Islands" =   "U.S. Virgin Is.",
      "Curacao" = "Curaçao",
      "Democratic Republic of the Congo" =   "U.S. Virgin Is.",
      "UA Emirates" = "United Arab Emirates",
      "Central African Republic" =   "Central African Rep.",
      "Equatorial Guinea" =   "Eq. Guinea",
      "Eswatini" =   "eSwatini",
      "Gibraltar" = "Gibraltar",
      "IR Iran" = "Iran",
      "Marshall Islands" = "Marshall Is.",
      "Saint Kitts and Nevis" =   "St. Kitts and Nevis",
      "Saint Vincent and the Grenadines" = "St. Vin. and Gren."
    )
    
    # Aplicar correcciones al dataframe
    paisesParticipantes_eda_nadadores <- paisesParticipantes_eda_nadadores %>%
      mutate(clubname = recode(clubname, !!!correcciones))
    
    
    # Hago un left join para unir mis datos con los de world. 
    world <- left_join(world, paisesParticipantes_eda_nadadores, by = c("name" = "clubname"))
    # Reemplazo valores nulos por 0: 
    world$nadadores[is.na(world$nadadores)] <- 0
    
    # Creo el mapa: 
    leaflet(world) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~colorNumeric("Blues", nadadores)(nadadores),  # Colorea según cantidad de nadadores
        weight = 1,
        opacity = 1,
        color = "white",
        fillOpacity = 0.7,
        highlight = highlightOptions(weight = 2, color = "#666", bringToFront = TRUE),
        label = ~paste0(name, ": ", nadadores, " nadadores")  # Mostrar país y cantidad de nadadores
      )
  })
  
  
  
  ### Página 6.2. EDA. Pruebas. ------------------------------------------------
  colores_wa <- c("F" = "#0072CE", "M" = "#002F6C")
  #### Texto Introductorio. ----------------------------------------------------
  output$eda_pruebas_explicacion <- renderUI({
    numNadadoresPruebas <- dim(nadadoresPruebas)[1]
    numNadadores <- dim(nadadoresParticipantes)[1]
    porcentaje <- numNadadoresPruebas / numNadadores
    paste("En la actual sección, se encuentra información acerca de algunas variables como lo son la Edad, los puntos FINA y los tiempos de reacción (reactiontime)
          de los distintos nadadores en cada una de sus pruebas nadadas. Se podrán observar gráficos que nos ayudarán a sacar conclusiones
          acerca de algunas características de este deporte. Hubo ", numNadadores, " participantes y entre todos los participantes, 
          se nadó en ", numNadadoresPruebas, " ocasiones, lo que supone un ", round(porcentaje,2), " de pruebas nadadas por cada nadador aproximadamente.")
    
  })
  
  #### Gráficos 1 --------------------------------------------------------------
  output$eda_pruebas_grafico1a <- renderPlotly({
    ggplotly(
      ggplot(nadadoresPruebas, aes(x = reactiontime)) +
        geom_density(fill = "lightblue", color = "blue", alpha = 0.5) +
        labs(title = "Densidad", x = "Tiempo de reacción (s)", y = "Densidad. Tiempo de reacción") +
        theme_minimal()
    )
  })

  output$eda_pruebas_grafico1b <- renderPlotly({
    ggplotly(
      ggplot(nadadoresPruebas, aes(x = reactiontime, fill = gender)) +
        geom_density(alpha = 0.5) +
        scale_fill_manual(
          values = colores_wa,
          name = "Género",
          labels = c("F" = "Femenino", "M" = "Masculino")
        ) +
        labs(
          title = "Densidad por Género",
          x = "Tiempo de reacción (s)", y = "Densidad"
        ) +
        theme_minimal()
    ) %>%
      layout(
        legend = list(
          orientation = "v",   
          x = 1.02,           
          y = 1,             
          xanchor = "left",   
          yanchor = "top"
        ),
        margin = list(r = 120)  
      )
  })
  
  
  #### Gráficos 2 --------------------------------------------------------------
  output$eda_pruebas_grafico2a <- renderPlotly({
    ggplotly(
      ggplot(nadadoresPruebas, aes(x = as.factor(distance), y = reactiontime, fill = distance)) + 
        geom_boxplot(color = "black", alpha = 0.7) +
        labs(title = "Tiempo de reacción por Distancia", x = "Distancia", y = "Tiempo de reacción(s)") +
        theme_minimal() +
        coord_flip() +
        scale_fill_gradient(low = "darkblue", high = "skyblue") +
        theme(legend.position = "none")
    )
  })
  
  output$eda_pruebas_grafico2b <- renderPlotly({
    ggplotly(
      ggplot(nadadoresPruebas, aes(x = as.factor(stroke), y = reactiontime, fill = stroke)) + 
        geom_boxplot(color = "black", alpha = 0.7) +
        labs(title = "Tiempo de reacción por Estilo", x = "Estilo", y = "Tiempo de reacción(s)") +
        theme_minimal() +
        coord_flip() +
        scale_fill_manual(values = c("lightblue", "darkblue", "lightblue", "lightblue", "lightblue")) +
        theme(legend.position = "none")
    )
  })
  
  #### Gráficos 3 ---------------------------------------------------------------
  output$eda_pruebas_grafico3 <- renderPlotly({
    rendimientoEdades <- nadadoresPruebas %>%
      group_by(edad) %>%
      summarize(media_points = mean(points, na.rm = TRUE))
    
    max_edad <- rendimientoEdades$edad[which.max(rendimientoEdades$media_points)]
    max_puntos <- max(rendimientoEdades$media_points, na.rm = TRUE)
    
    ggplotly(
      ggplot(rendimientoEdades, aes(x = edad, y = media_points)) +
        geom_line(color = "blue", size = 1) +
        annotate("point", x = max_edad, y = max_puntos, color = "red", size = 3) +
        labs(title = "Media de Puntos por Edad", x = "Edad", y = "Media de puntos") +
        theme_minimal()
    )
  })
  
  #### Resumen 1 ---------------------------------------------------------------
  output$eda_pruebas_resumen1 <- renderUI({
    HTML("
  <ul>
    <li>En la primera gráfica, podemos observar la densidad del tiempo de reacción de los nadadores en las pruebas nadadas. Podemos observar que presenta un patrón concentrado en torno a los 0.65s y ligeramente asimétrico.</li>
    <li>En la segunda gráfica, podemos ver que, las nadadoras presentan, en promedio, un mayor tiempo de reacción que los nadadores. Esta diferencia, tras evaluarse mediante un T-Test, y obteniéndose un p-valor suficientemente bajo, sugiere que existe evidencia estadísticamente significativa para afirmar que la media de ambos grupos es distinta. El tiempo de reacción de las mujeres es aproximadamente un 5% mayor que el de los hombres.</li>
  </ul>
")
    
  })
  
  #### Resumen 2 ---------------------------------------------------------------
  output$eda_pruebas_resumen2 <- renderUI({
    HTML("
  <ul>
    <li>En estas gráficas presentamos la distribución del tiempo de reacción mediante boxplots, agrupando tanto por estilo como por distancia de nado.</li>
    <li>En la primera gráfica, podemos observar que, las distancias más cortas presentan menores tiempos de reacción. Esto sugiere que, en las pruebas de velocidad, una buena salida puede ser decisiva.</li>
    <li>En la segunda gráfica, se puede ver claramente que el estilo <strong>ESPALDA</strong> presenta tiempos de reacción significativamente más bajos en comparación al resto. Este resultado es esperable si tenemos en cuenta que los espaldistas son los únicos nadadores que no comienzan la carrera desde el poyete.</li>
  </ul>
")
    
  })
  
  #### Resumen 3 ---------------------------------------------------------------
  output$eda_pruebas_resumen3 <- renderUI({
    HTML("
  <ul>
    <li>En la siguiente gráfica podemos observar el rendimiento de los nadadores, agrupado por edad, respecto a los puntos FINA cosechados.</li>
    <li>Podemos observar que el rendimiento se incrementa rápidamente hasta los 23-24 años, momento a partir del cual comienza a estabilizarse.</li>
    <li>La edad donde más rendimiento ofrecieron los nadadores fue a los 32 años, con una media de 886 puntos FINA. A partir de esa edad, se produce una caída significativa en el rendimiento por puntos.</li>
  </ul>
")
    
  })
  
  
  
  ### Página 6.3 EDA. Road to Champion -----------------------------------------

  #### 1. Filtro DISTANCIA -----------------------------------------------------
  observe({
    distancias <- sort(unique(worlds2024Budapest25m$distance))
    updateSelectInput(session, "filtro_distancia", choices = distancias)
  })
  
  #### 2. Filtro STROKE --------------------------------------------------------
  observeEvent(input$filtro_distancia, {
    req(input$filtro_distancia)
    
    strokes <- worlds2024Budapest25m %>%
      filter(distance == input$filtro_distancia) %>%
      pull(stroke) %>%
      unique() %>%
      sort()
    
    updateSelectInput(session, "filtro_stroke", choices = strokes)
  })
  
  #### 3. Filtro GÉNERO --------------------------------------------------------
  observeEvent(c(input$filtro_distancia, input$filtro_stroke), {
    req(input$filtro_distancia, input$filtro_stroke)
    updateSelectInput(session, "filtro_genero", choices = c("F","M"))
  })
  
  #### 4. Texto dinámico. ------------------------------------------------------
  output$texto_campeon <- renderUI({
    req(input$filtro_distancia, input$filtro_stroke, input$filtro_genero)
    
    nadador <- nadadoresPruebas %>%
      filter(distance == input$filtro_distancia,
             stroke == input$filtro_stroke,
             gender == input$filtro_genero,
             round  %in% c("FIN", "FHT", "TIM"), 
             place == 1)
    
    if (nrow(nadador) == 0) return(NULL)
    
    nombre <- paste(nadador$firstname, nadador$lastname)
    
    genero_texto <- ifelse(input$filtro_genero == "F", "Campeona", "Campeón")
    estilo <- tools::toTitleCase(tolower(input$filtro_stroke))
    genero <- ifelse(input$filtro_genero == "F", "femenino", "masculino")
    
    HTML(paste0(
      "<h4 style='color: #002F6C; font-weight: bold;'>🏆 ", nombre, "</h4>",
      "<p style='font-size: 16px; margin-top: -10px;'>", genero_texto,
      " de: ", input$filtro_distancia, " metros ", estilo, " ", genero, ".</p>"
    ))
  })
  
  
  #### 5. Gráfico: Comparación de parciales por ronda --------------------------
  
  output$plot_parciales_round <- renderPlotly({
    req(input$filtro_distancia, input$filtro_stroke, input$filtro_genero)
    
    # 1. Filtrar el nadador ganador en nadadoresPruebas
    nadador_campeon <- nadadoresPruebas %>%
      filter(distance == input$filtro_distancia,
             stroke == input$filtro_stroke,
             gender == input$filtro_genero,
             round %in% c("FIN", "FHT", "TIM"),
             place == 1) %>%
      slice(1) 
    
    if (nrow(nadador_campeon) == 0) {
      
      return(plotly_empty(type = "scatter", mode = "markers") %>% 
               layout(title = list(text = "No hay datos del campeón para esta prueba", x = 0.5)))
    }
    
    athlete_id <- nadador_campeon$athleteid
    
    # 2. Filtrar datos en worlds2024Budapest25m para ese atleta y prueba
    datos_parciales <- worlds2024Budapest25m %>%
      filter(athleteid == athlete_id,
             distance == input$filtro_distancia,
             stroke == input$filtro_stroke) %>%
      arrange(round, splitdistance)
    
    if (nrow(datos_parciales) == 0) {
      return(plotly_empty(type = "scatter", mode = "markers") %>% 
               layout(title = list(text = "No hay parciales para este nadador en esta prueba", x = 0.5)))
    }
    
    # 3. Convertir parcialswimtime a segundos (numérico)
    if (inherits(datos_parciales$parcialswimtime, "difftime")) {
      datos_parciales$parcialswimtime_sec <- as.numeric(datos_parciales$parcialswimtime, units = "secs")
    } else if (inherits(datos_parciales$parcialswimtime, "hms")) {
      datos_parciales$parcialswimtime_sec <- as.numeric(datos_parciales$parcialswimtime)
    } else {
      datos_parciales$parcialswimtime_sec <- as.numeric(datos_parciales$parcialswimtime)
    }
    
    # Crear etiqueta del título con primera letra del nombre, apellido y prueba
    nombre_titulo <- paste0(
      substr(nadador_campeon$firstname, 1, 1), ". ", 
      nadador_campeon$lastname, " - ",
      input$filtro_distancia, "m ", input$filtro_stroke
    )
    
    # 4. Graficar con ggplot para luego convertir con ggplotly
    p <- ggplot(datos_parciales, aes(x = splitdistance, y = parcialswimtime_sec, color = round, group = round,
                                     text = paste0("Ronda: ", round, "<br>",
                                                   "Distancia parcial: ", splitdistance, " m<br>",
                                                   "Tiempo: ", round(parcialswimtime_sec, 2), " seg"))) +
      geom_line(size = 1.2) +
      geom_point(size = 3) +
      theme_minimal() +
      scale_color_manual(
        values = c(
          "TIM" = "#002F6C",
          "FHT" = "#002F6C",
          "PRE" = "#00B2E3",
          "SEM" = "#0072CE",
          "FIN" = "#002F6C"
        ),
        labels = c("TIM" = "Heats", "FHT" = "Heats", "SEM" = "Semifinal", "FIN" = "Final")
      ) +
      labs(
        title = nombre_titulo,
        x = "Distancia parcial (m)",
        y = "Tiempo parcial (segundos)",
        color = "Ronda"
      ) +
      theme(
        text = element_text(size = 14),
        plot.title = element_text(face = "bold", hjust = 0.5),
        legend.position = "top"
      )
    
   
    ggplotly(p, tooltip = "text") %>%
      layout(title = list(text = nombre_titulo, x = 0.5))
  })
  
  
  #### 6. Texto de análisis del nadador ----------------------------------------
  output$texto_analisis_campeon <- renderUI({
    req(input$filtro_distancia, input$filtro_stroke, input$filtro_genero)
    
    # 1. Campeona de la prueba
    campeona <- nadadoresPruebas %>%
      filter(distance == input$filtro_distancia,
             stroke == input$filtro_stroke,
             gender == input$filtro_genero,
             round %in% c("FIN", "FHT", "TIM"),
             place == 1) %>%
      slice(1)
    
    if (nrow(campeona) == 0) {
      return(HTML("<p style='color: grey;'>No se encontró información del nadador.</p>"))
    }
    
    athlete_id <- campeona$athleteid
    nombre <- paste(campeona$firstname, campeona$lastname)
    pais <- campeona$clubname
    edad <- campeona$edad
    prueba <- paste0(input$filtro_distancia, "m ", input$filtro_stroke)
    genero <- ifelse(input$filtro_genero == "M", "masculino", "femenino")
    
    # 2. Info de parciales
    parciales <- worlds2024Budapest25m %>%
      filter(athleteid == athlete_id,
             distance == input$filtro_distancia,
             stroke == input$filtro_stroke) %>%
      mutate(
        tiempo = case_when(
          inherits(parcialswimtime, "difftime") ~ as.numeric(parcialswimtime, units = "secs"),
          inherits(parcialswimtime, "hms") ~ as.numeric(parcialswimtime),
          TRUE ~ as.numeric(parcialswimtime)
        )
      )
    
    if (nrow(parciales) == 0) {
      return(HTML("<p style='color: grey;'>No se encontraron parciales para este nadador.</p>"))
    }
    
    # 3. Info de rondas y sesiones + puntos y récords
    sesiones <- parciales %>%
      distinct(round, daySesion, daytime, points) %>%
      arrange(factor(round, levels = c("TIM", "FHT","PRE","SEM", "FIN")))  # orden típico
    
    texto_rondas <- paste0(
      "<ul>",
      paste0(
        apply(sesiones, 1, function(row) {
          ronda <- row["round"]
          dia <- row["daySesion"]
          hora <- row["daytime"]
          puntos <- as.numeric(row["points"])
          
          texto_record <- if (!is.na(puntos) && puntos > 1000) {
            "<b> Batió el WR en esa ronda.</b>"
          } else {
            ""
          }
          
          paste0("<li>Nadó la ronda <b>", ronda, "</b> el día <b>", dia,
                 "</b> a las <b>", hora, "</b>.", texto_record, "</li>")
        }),
        collapse = ""
      ),
      "</ul>"
    )
    
    
    # 4. Análisis por tramos
    distancia_total <- as.numeric(input$filtro_distancia)
    partes <- ifelse(distancia_total == 50, 2, 4)
    longitud_parte <- distancia_total / partes
    
    parciales <- parciales %>%
      mutate(
        tramo = ceiling(splitdistance / longitud_parte)
      )
    
    resumen_tramos <- parciales %>%
      group_by(tramo, round) %>%
      summarise(tiempo_medio = mean(tiempo, na.rm = TRUE), .groups = "drop") %>%
      group_by(tramo) %>%
      slice_min(tiempo_medio, n = 1, with_ties = FALSE) %>%
      arrange(tramo)
    
    texto_tramos <- paste0(
      "<ul>",
      paste0(
        apply(resumen_tramos, 1, function(row) {
          t <- row["tramo"]
          r <- row["round"]
          tiempo <- round(as.numeric(row["tiempo_medio"]), 2)
          paste0("<li>En la parte ", t, " de la prueba, el nadador fue más rápido en la ronda <b>", r,
                 "</b> con un parcial medio de <b>", tiempo, " segundos</b></li>")
        }),
        collapse = ""
      ),
      "</ul>"
    )
    
    
    # 5. Texto final
    texto_final <- glue::glue("
    <p><b>{nombre}</b>, representando a <b>{pais}</b> con <b>{edad} años</b>, consiguió la medalla de oro en la prueba de <b>{prueba}</b> ({genero}).</p>
    
    <p>Su camino hacia la final se desarrolló de la siguiente forma: {texto_rondas}</p>
    
    <p>Si observamos detenidamente sus parciales, podemos notar lo siguiente: {texto_tramos}.</p>
  ")
    
    HTML(texto_final)
  })
  
  
  #### 7. Gráfico de evolución de la carrera -----------------------------------
  output$plot_evolucion_carrera <- renderPlotly({
    req(input$filtro_distancia, input$filtro_stroke, input$filtro_genero)
    
    # Distancia numérica
    distancia_total <- as.numeric(input$filtro_distancia)
    
    # Filtrar prueba base
    datos_prueba <- worlds2024Budapest25m %>%
      filter(distance == distancia_total,
             stroke == input$filtro_stroke,
             gender == input$filtro_genero,
             round %in% c("FIN", "FHT", "TIM"))
    
    # Si es prueba larga, obtener solo nadadores de la serie del ganador
    if (distancia_total > 200) {
      # Obtener ronda y serie del ganador
      serie_ganadora <- datos_prueba %>%
        filter(place == 1) %>%
        slice(1) %>%
        select(round, heat)
      
      # Filtrar solo nadadores de esa serie y ronda
      datos_prueba <- datos_prueba %>%
        filter(round == serie_ganadora$round,
               heat == serie_ganadora$heat)
      
      # Reducir a X parciales representativos
      divisiones <- ifelse(distancia_total == 1500, 6, 8)
      paso <- distancia_total / divisiones
      
      # Asegurar que los splitdistance estén redondeados según tus datos
      splits_filtrados <- round(seq(paso, distancia_total, by = paso))
      
      datos_prueba <- datos_prueba %>%
        filter(splitdistance %in% splits_filtrados)
    }
    
    
    # Ordenar y asignar posición
    datos_prueba <- datos_prueba %>%
      arrange(splitdistance, cumswimtime) %>%
      group_by(splitdistance) %>%
      mutate(position = row_number())
    
    # Etiqueta: Inicial. Apellido
    datos_prueba <- datos_prueba %>%
      mutate(nombre = paste0(substr(firstname, 1, 1), ". ", lastname))
    
    # Título dinámico
    estilo <- input$filtro_stroke
    genero <- ifelse(input$filtro_genero == "F", "Femenino", "Masculino")
    titulo_grafico <- paste0(distancia_total, "m ", estilo, " (", genero, ")")
    
    # Gráfico
    grafico <- ggplot(datos_prueba, aes(x = factor(splitdistance), y = reorder(nombre, lane))) +
      geom_point(aes(color = factor(position), text = paste("Posición:", position,
                                                            "<br>Parcial:", splitdistance, "m",
                                                            "<br>Tiempo:", round(cumswimtime, 2))), 
                 size = 6) +
      scale_color_manual(values = c("1" = "gold", "2" = "gray", "3" = "chocolate")) +
      geom_text(aes(label = position), color = "white", fontface = "bold") +
      labs(x = "Distancia acumulada (m)", y = "Finalistas", title = titulo_grafico, color = "Posición") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(face = "bold", hjust = 0.5),
            legend.position = "none")
    
    
    ggplotly(grafico, tooltip = "text")
  })
  
  
  
  
  #### 8. Animación de la carrera. ---------------------------------------------
  output$gif_animacion_carrera <- renderUI({
    req(input$filtro_distancia, input$filtro_stroke, input$filtro_genero)
    
    
    nombre_archivo <- paste0(input$filtro_distancia, "_", input$filtro_stroke, "_", input$filtro_genero, ".gif")
    
    # Ruta relativa dentro de www/
    ruta <- file.path("www", nombre_archivo)
    

    if (file.exists(ruta)) {
      tags$img(src = nombre_archivo, width = "100%")  # Solo nombre_archivo porque www es raíz estática
    } else {
      HTML("<p style='color: grey;'>Animación no disponible para esta prueba.</p>")
    }
  })
  
  
  #### 9. Foto del campeón. ----------------------------------------------------
  output$foto_campeona <- renderUI({
    req(input$filtro_distancia, input$filtro_stroke, input$filtro_genero)
    
    nadador_campeon <- nadadoresPruebas %>%
      filter(distance == input$filtro_distancia,
             stroke == input$filtro_stroke,
             gender == input$filtro_genero,
             round  %in% c("FIN", "FHT", "TIM"), 
             place == 1)
    
    if (nrow(nadador_campeon) == 0) {
      return(HTML("<p style='color: grey;'>Foto no disponible.</p>"))
    }
    
    nadador <- merge(nadador_campeon, imagenesNadadores, by = c("firstname", "lastname"), all.x = TRUE)
    
    if (is.na(nadador$foto) || nchar(nadador$foto) == 0) {
      return(HTML("<p style='color: grey;'>Foto no disponible.</p>"))
    }
    
    tags$img(
      src = nadador$foto,
      width = "70px",
      height = "70px",
      style = "border-radius: 50%; border: 3px solid #0072CE; box-shadow: 0 0 5px rgba(0,114,206,0.7);"
    )
  })
  
  
  
  

  ## Página 7. Clustering ------------------------------------------------------
  
  ### Página 7.1 Pruebas Cortas ------------------------------------------------
  
  #### 1. Datos reactivos filtrados según la prueba y estilo -------------------
  datos_filtrados_cortas <- reactive({
    req(input$cortas_prueba, input$cortas_estilo, input$cortas_genero)
    
    df <- worlds2024Budapest25m %>%
      filter(distance == as.numeric(input$cortas_prueba),
             stroke == input$cortas_estilo,
             round == "PRE",
             gender == input$cortas_genero) %>%
      select(athleteid, splitdistance, parcialswimtime, swimtime) %>%
      arrange(athleteid, splitdistance) %>%
      mutate(parcialswimtime = as.numeric(parcialswimtime) / as.numeric(swimtime)) %>%
      pivot_wider(names_from = splitdistance, values_from = parcialswimtime) %>%
      as.data.frame()
    
    return(df)
  })
  
  #### 2. Generar dendrograma al pulsar el botón. ------------------------------
  observeEvent(input$cortas_generar_dendrograma, {

    # Comprobar si la prueba es 50 y el estilo NO es válido
    if (input$cortas_prueba == "50" && input$cortas_estilo == "MEDLEY") {
      showNotification("NO EXISTE ESTA PRUEBA", type = "error", duration = 5)
      return()
    }
    df <- datos_filtrados_cortas()
    parciales <- df[, 3:ncol(df)]
    
    dist_matrix <- switch(input$cortas_metrica,
                          "Euclidean" = dist(parciales),
                          "DTW" = proxy::dist(parciales, method = "DTW"))
    
    hc <- hclust(dist_matrix)
    valores_cortas$hc <- hc
  })
  
  #### 3. Plot del dendrograma -------------------------------------------------
  output$cortas_dendrograma_plot <- renderPlot({
    req(valores_cortas$hc)
    plot(valores_cortas$hc, main = "Dendrograma jerárquico", xlab = "", sub = "")
  })
  
  #### 4. Aplicar clustering según el número de clusters -----------------------
  observeEvent(input$cortas_aplicar_clustering, {
    req(valores_cortas$hc)
    
    k <- input$cortas_k_clusters
    grupos <- cutree(valores_cortas$hc, k = k)
    
    df <- datos_filtrados_cortas()
    df$cluster <- grupos
    
    valores_cortas$df_clusterizado <- df
  })
  
  #### 5. Gráfico de centroides (perfil promedio por cluster con líneas individuales) -------------------
  output$cortas_centroides_plot <- renderPlot({
    req(valores_cortas$df_clusterizado)
    
    df <- valores_cortas$df_clusterizado
    parciales <- df[, 3:(ncol(df)-1)]
    rownames(parciales) <- df$athleteid
    
    parciales$cluster <- as.factor(df$cluster)
    parciales$athleteid <- df$athleteid
    
    parciales_long <- reshape2::melt(parciales, id.vars = c("athleteid", "cluster"))
    parciales_long$variable <- as.numeric(as.character(parciales_long$variable))
    
    centroides <- parciales_long %>%
      group_by(cluster, variable) %>%
      summarise(media = mean(value), .groups = "drop")
    
    ggplot() +
      geom_line(data = parciales_long, aes(x = variable, y = value, group = athleteid), alpha = 0.3, color = "grey40") +
      geom_line(data = centroides, aes(x = variable, y = media, color = cluster), size = 1.5) +
      facet_wrap(~ cluster, scales = "free_y") +
      labs(title = "Curvas de parciales por cluster con su centroide",
           x = "Distancia (m)",
           y = "Tiempo parcial relativo") +
      scale_x_continuous(breaks = sort(unique(parciales_long$variable))) +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  #### 6. Texto automático con resumen. ----------------------------------------
  output$cortas_texto_automatico <- renderUI({
    req(valores_cortas$df_clusterizado)
    df_clusterizado <- valores_cortas$df_clusterizado
    
    # 1. Filtrar finalistas de la misma prueba
    finalistas <- worlds2024Budapest25m %>%
      filter(
        round == "FIN",
        distance == as.numeric(input$cortas_prueba),
        stroke == input$cortas_estilo,
        gender == input$cortas_genero
      ) %>%
      select(athleteid) %>%
      distinct()
    
    # 2. Marcar si un nadador del cluster llegó a la final en esa prueba
    df_clusterizado$finalista <- ifelse(df_clusterizado$athleteid %in% finalistas$athleteid, 1, 0)
    
    # 3. Calcular resumen por cluster
    resumen <- df_clusterizado %>%
      group_by(cluster) %>%
      summarise(
        n_nadadores = n(),
        n_finalistas = sum(finalista),
        tiempo_medio = mean(as.numeric(swimtime), na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        texto = paste0(
          "🌀 El cluster ", cluster, " agrupa a ", n_nadadores, " nadadores, ",
          n_finalistas, " de los cuales llegaron a la final. ",
          "El tiempo medio de nado fue de ", as.hms(round(tiempo_medio, 2)), " segundos."
        )
      ) %>%
      pull(texto) %>%
      paste(collapse = "<br>")
    
    HTML(paste0("<h4>Resumen del clustering:</h4><p>", resumen, "</p>"))
  })
  #### 7. Gráfico de solo centroides (sin líneas individuales) -----------------
  output$cortas_centroides_plot_nuevo <- renderPlot({
    req(valores_cortas$df_clusterizado)
    
    df <- valores_cortas$df_clusterizado
    parciales <- df[, 3:(ncol(df)-1)]
    rownames(parciales) <- df$athleteid
    
    parciales$cluster <- as.factor(df$cluster)
    parciales$athleteid <- df$athleteid
    
    parciales_long <- reshape2::melt(parciales, id.vars = c("athleteid", "cluster"))
    parciales_long$variable <- as.numeric(as.character(parciales_long$variable))
    
    centroides <- parciales_long %>%
      group_by(cluster, variable) %>%
      summarise(media = mean(value), .groups = "drop")
    
    ggplot(centroides, aes(x = variable, y = media, color = cluster)) +
      geom_line(size = 1.5) +
      geom_point(size = 2) +
      labs(title = "Perfil promedio de cada cluster (centroides)",
           x = "Distancia (m)",
           y = "Tiempo parcial relativo") +
      scale_x_continuous(breaks = sort(unique(centroides$variable))) +
      theme_minimal()
  })
  
  observeEvent(input$cortas_asignar_cluster, {
  req(input$cortas_input_parciales, valores_cortas$df_clusterizado)


  parciales_str <- unlist(strsplit(input$cortas_input_parciales, ";"))
  parciales_num <- as.numeric(trimws(parciales_str))


  n_parciales_esperados <- if (input$cortas_prueba == "100") 4 else 2
  nombres_parciales <- if (input$cortas_prueba == "100") c("25", "50", "75", "100") else c("25", "50")


  if (length(parciales_num) != n_parciales_esperados || any(is.na(parciales_num))) {
    output$cortas_cluster_asignado_texto <- renderUI({
      tags$p("⚠️ Por favor, introduce exactamente los parciales esperados, separados por ';'.", 
             style = "color: red; font-weight: bold;")
    })
    return()
  }

  # 4. Normalización
  tiempo_total <- sum(parciales_num)
  obs_normalizada <- parciales_num / tiempo_total
  names(obs_normalizada) <- nombres_parciales

  # 5. Calcular centroides
  centroides <- valores_cortas$df_clusterizado %>%
    select(all_of(c("cluster", nombres_parciales))) %>%
    group_by(cluster) %>%
    summarise(across(all_of(nombres_parciales), mean, na.rm = TRUE), .groups = "drop") %>%
    mutate(tiempo_total = rowSums(across(all_of(nombres_parciales))),
           across(all_of(nombres_parciales), ~ .x / tiempo_total))  # Normalización

  # 6. Calcular distancias
  centroides$distancia <- apply(centroides[, nombres_parciales], 1, function(c) {
    sqrt(sum((obs_normalizada - c)^2))
  })

  cluster_asignado <- centroides$cluster[which.min(centroides$distancia)]

  # 7. Mostrar texto
  output$cortas_cluster_asignado_texto <- renderUI({
    tags$p(paste0("Tu estrategia se asemeja al cluster ", cluster_asignado, "."), 
           style = "color: #003366; font-weight: bold; font-size: 16px;")
  })

  # 8. Preparar datos para el gráfico
  mi_obs <- data.frame(
    athleteid = "Tú",
    cluster = cluster_asignado,
    parcial = nombres_parciales,
    valor = obs_normalizada
  )

  nadadores_cluster <- valores_cortas$df_clusterizado %>%
    filter(cluster == cluster_asignado) %>%
    select(athleteid, cluster, all_of(nombres_parciales)) %>%
    pivot_longer(cols = all_of(nombres_parciales), names_to = "parcial", values_to = "valor")
  
  nadadores_cluster$parcial <- as.numeric(nadadores_cluster$parcial)
  mi_obs$parcial <- as.numeric(mi_obs$parcial)

  output$cortas_grafico_cluster_usuario <- renderPlot({
    ggplot() +
      geom_line(data = nadadores_cluster, aes(x = parcial, y = valor, group = athleteid),
                color = "gray70", alpha = 0.4) +
      geom_line(data = mi_obs, aes(x = parcial, y = valor, group = 1), 
                color = "#0072B2", size = 1.5) +
      geom_point(data = mi_obs, aes(x = parcial, y = valor), color = "#0072B2", size = 3) +
      labs(title = paste("Comparación con el Cluster", cluster_asignado),
           x = "Parcial (m)", y = "Tiempo (s)") +
      theme_minimal(base_size = 14)
  })
})

  
  #### 8. Inicializar variables reactivas necesarias ---------------------------
  valores_cortas <- reactiveValues(
    hc = NULL,
    df_clusterizado = NULL
  )
  
  ### Página 7.2 Pruebas Media-Distancia ---------------------------------------
  
  #### 1. Datos reactivos filtrados según la prueba y estilo -------------------
  datos_filtrados_medio <- reactive({
    req(input$medio_prueba, input$medio_estilo, input$medio_genero)
    df <- worlds2024Budapest25m %>%
      filter(distance == as.numeric(input$medio_prueba),
             stroke == input$medio_estilo,
             round == "PRE",
             gender == input$medio_genero) %>%
      select(athleteid, splitdistance, parcialswimtime, swimtime) %>%
      arrange(athleteid, splitdistance) %>%
      mutate(parcialswimtime = as.numeric(parcialswimtime) / as.numeric(swimtime)) %>%
      pivot_wider(names_from = splitdistance, values_from = parcialswimtime) %>%
      as.data.frame()
    
    return(df)
  })
  
  #### 2. Generar dendrograma cuando se pulse el botón -------------------------
  observeEvent(input$medio_generar_dendrograma, {
    if (input$medio_prueba == "400" && !(input$medio_estilo %in% c("FREE", "MEDLEY"))) {
      showNotification("NO EXISTE ESTA PRUEBA", type = "error", duration = 5)
      return()
    }
    
    df <- datos_filtrados_medio()
    parciales <- df[, 3:ncol(df)]
    
    if (input$medio_metrica == "DTW (suavizado)") {
      parciales_suavizados <- t(apply(parciales, 1, function(x) {
        ss <- smooth.spline(x)
        predict(ss, seq(min(ss$x), max(ss$x), length.out = length(x)))$y
      }))
      
      df[, 3:ncol(df)] <- parciales_suavizados
      valores_medio$df_suavizado <- df
      
      dist_matrix <- proxy::dist(parciales_suavizados, method = "DTW")
    } else {
      valores_medio$df_suavizado <- df
      dist_matrix <- switch(input$medio_metrica,
                            "Euclidean" = dist(parciales),
                            "DTW" = proxy::dist(parciales, method = "DTW"))
    }
    
    hc <- hclust(dist_matrix)
    valores_medio$hc <- hc
  })
  
  #### 3. Plot del dendrograma
  output$medio_dendrograma_plot <- renderPlot({
    req(valores_medio$hc)
    plot(valores_medio$hc, main = "Dendrograma jerárquico", xlab = "", sub = "")
  })
  
  #### 4. Aplicar clustering según el número de clusters -----------------------
  observeEvent(input$medio_aplicar_clustering, {
    req(valores_medio$hc)
    
    k <- input$medio_k_clusters
    grupos <- cutree(valores_medio$hc, k = k)
    
    df <- valores_medio$df_suavizado
    df$cluster <- grupos
    
    valores_medio$df_clusterizado <- df
  })
  
  #### 5. Gráfico de centroides (perfil promedio por cluster con líneas individuales) ----------------
  output$medio_centroides_plot <- renderPlot({
    req(valores_medio$df_clusterizado)
    
    df <- valores_medio$df_clusterizado
    parciales <- df[, 3:(ncol(df)-1)]
    rownames(parciales) <- df$athleteid
    
    parciales$cluster <- as.factor(df$cluster)
    parciales$athleteid <- df$athleteid
    
    parciales_long <- reshape2::melt(parciales, id.vars = c("athleteid", "cluster"))
    parciales_long$variable <- as.numeric(as.character(parciales_long$variable))
    
    centroides <- parciales_long %>%
      group_by(cluster, variable) %>%
      summarise(media = mean(value), .groups = "drop")
    
    ggplot() +
      geom_line(data = parciales_long, aes(x = variable, y = value, group = athleteid), alpha = 0.3, color = "grey40") +
      geom_line(data = centroides, aes(x = variable, y = media, color = cluster), size = 1.5) +
      facet_wrap(~ cluster, scales = "free_y") +
      labs(title = "Curvas de parciales por cluster con su centroide",
           x = "Distancia (m)",
           y = "Tiempo parcial relativo") +
      scale_x_continuous(breaks = sort(unique(parciales_long$variable))) +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  #### 6. Texto automático con resumen -----------------------------------------
  output$medio_texto_automatico <- renderUI({
    req(valores_medio$df_clusterizado)
    df_clusterizado <- valores_medio$df_clusterizado
    
    finalistas <- worlds2024Budapest25m %>%
      filter(
        round == "FIN",
        distance == as.numeric(input$medio_prueba),
        stroke == input$medio_estilo,
        gender == input$medio_genero
      ) %>%
      select(athleteid) %>%
      distinct()
    
    df_clusterizado$finalista <- ifelse(df_clusterizado$athleteid %in% finalistas$athleteid, 1, 0)
    
    resumen <- df_clusterizado %>%
      group_by(cluster) %>%
      summarise(
        n_nadadores = n(),
        n_finalistas = sum(finalista),
        tiempo_medio = mean(as.numeric(swimtime), na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        texto = paste0(
          "🌀 El cluster ", cluster, " agrupa a ", n_nadadores, " nadadores, ",
          n_finalistas, " de los cuales llegaron a la final. ",
          "El tiempo medio de nado fue de ", as.hms(round(tiempo_medio, 2)), " segundos."
        )
      ) %>%
      pull(texto) %>%
      paste(collapse = "<br>")
    
    HTML(paste0("<h4>Resumen del clustering:</h4><p>", resumen, "</p>"))
  })
  
  #### 7. Gráfico de solo centroides (sin líneas individuales) -----------------
  output$medio_centroides_plot_nuevo <- renderPlot({
    req(valores_medio$df_clusterizado)
    
    df <- valores_medio$df_clusterizado
    parciales <- df[, 3:(ncol(df)-1)]
    rownames(parciales) <- df$athleteid
    
    parciales$cluster <- as.factor(df$cluster)
    parciales$athleteid <- df$athleteid
    
    parciales_long <- reshape2::melt(parciales, id.vars = c("athleteid", "cluster"))
    parciales_long$variable <- as.numeric(as.character(parciales_long$variable))
    
    centroides <- parciales_long %>%
      group_by(cluster, variable) %>%
      summarise(media = mean(value), .groups = "drop")
    
    ggplot(centroides, aes(x = variable, y = media, color = cluster)) +
      geom_line(size = 1.5) +
      geom_point(size = 2) +
      labs(title = "Perfil promedio de cada cluster (centroides)",
           x = "Distancia (m)",
           y = "Tiempo parcial relativo") +
      scale_x_continuous(breaks = sort(unique(centroides$variable))) +
      theme_minimal()
  })
  
  #### 8. Inicializar variables reactivas necesarias ---------------------------
  valores_medio <- reactiveValues(
    hc = NULL,
    df_clusterizado = NULL,
    df_suavizado = NULL
  )
  
  
  ### Página 7.3 Pruebas Larga Distancia --------------------------------------- 
  #### Inicializar variables reactivas necesarias ------------------------------
  valores_larga <- reactiveValues(
    hc = NULL,
    df_clusterizado = NULL,
    df_suavizado = NULL
  )
  
  #### 1. Datos reactivos filtrados --------------------------------------------
  datos_filtrados_larga <- reactive({
    req(input$larga_prueba, input$larga_estilo, input$larga_genero)
    df <- worlds2024Budapest25m %>%
      filter(distance == as.numeric(input$larga_prueba),
             stroke == input$larga_estilo,
             gender == input$larga_genero) %>%
      select(athleteid, splitdistance, parcialswimtime, swimtime) %>%
      arrange(athleteid, splitdistance) %>%
      mutate(parcialswimtime = as.numeric(parcialswimtime) / as.numeric(swimtime)) %>%
      pivot_wider(names_from = splitdistance, values_from = parcialswimtime) %>%
      as.data.frame()
    
    return(df)
  })
  
  #### 2. Generar dendrograma cuando se pulse el botón -------------------------
  observeEvent(input$larga_generar_dendrograma, {
    df <- datos_filtrados_larga()
    parciales <- df[, 3:ncol(df)]
    
    suavizar <- function(parciales) {
      t(apply(parciales, 1, function(x) {
        ss <- smooth.spline(x, spar = 0.4)
        predict(ss, seq(min(ss$x), max(ss$x), length.out = length(x)))$y
      }))
    }
    
    if (input$larga_metrica %in% c("Euclidean (suavizado)", "DTW (suavizado)")) {
      parciales_suavizados <- suavizar(parciales)
      df[, 3:ncol(df)] <- parciales_suavizados
      valores_larga$df_suavizado <- df
    } else {
      valores_larga$df_suavizado <- NULL
    }
    
    dist_matrix <- switch(input$larga_metrica,
                          "Euclidean" = dist(parciales),
                          "Euclidean (suavizado)" = dist(parciales_suavizados),
                          "DTW" = proxy::dist(parciales, method = "DTW"),
                          "DTW (suavizado)" = proxy::dist(parciales_suavizados, method = "DTW")
    )
    
    hc <- hclust(dist_matrix)
    valores_larga$hc <- hc
    valores_larga$df_clusterizado <- NULL
  })
  
  #### 3. Plot del dendrograma -------------------------------------------------
  output$larga_dendrograma_plot <- renderPlot({
    req(valores_larga$hc)
    plot(valores_larga$hc, main = "Dendrograma jerárquico", xlab = "", sub = "")
  })
  
  #### 4. Aplicar clustering según el número de clusters -----------------------
  observeEvent(input$larga_aplicar_clustering, {
    req(valores_larga$hc)
    
    k <- input$larga_k_clusters
    grupos <- cutree(valores_larga$hc, k = k)
    
    df <- if (!is.null(valores_larga$df_suavizado)) {
      valores_larga$df_suavizado
    } else {
      datos_filtrados_larga()
    }
    
    df$cluster <- grupos
    valores_larga$df_clusterizado <- df
  })
  
  #### 5. Gráfico de centroides ------------------------------------------------
  output$larga_centroides_plot <- renderPlot({
    req(valores_larga$df_clusterizado)
    
    df <- valores_larga$df_clusterizado
    parciales <- df[, 3:(ncol(df)-1)]
    rownames(parciales) <- df$athleteid
    
    parciales$cluster <- as.factor(df$cluster)
    parciales$athleteid <- df$athleteid
    
    parciales_long <- reshape2::melt(parciales, id.vars = c("athleteid", "cluster"))
    parciales_long$variable <- as.numeric(as.character(parciales_long$variable))
    
    centroides <- parciales_long %>%
      group_by(cluster, variable) %>%
      summarise(media = mean(value), .groups = "drop")
    
    ggplot() +
      geom_line(data = parciales_long, aes(x = variable, y = value, group = athleteid), alpha = 0.3, color = "grey40") +
      geom_line(data = centroides, aes(x = variable, y = media, color = cluster), size = 1.5) +
      facet_wrap(~ cluster, scales = "free_y") +
      labs(title = "Curvas de parciales por cluster con su centroide",
           x = "Distancia (m)", y = "Tiempo parcial relativo") +
      scale_x_continuous(breaks = sort(unique(parciales_long$variable))) +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  #### 6. Texto automático con resumen ampliado --------------------------------
  output$larga_texto_automatico <- renderUI({
    req(valores_larga$df_clusterizado, nadadoresParticipantes)
    
    df_clusterizado <- valores_larga$df_clusterizado
    
    # Podio con join para obtener nombre completo
    podium <- df_clusterizado %>%
      arrange(as.numeric(swimtime)) %>%
      slice(1:3) %>%
      mutate(medalla = c("🥇 Oro", "🥈 Plata", "🥉 Bronce")) %>%
      left_join(nadadoresParticipantes %>% select(athleteid, firstname, lastname), by = "athleteid") %>%
      mutate(nombre_completo = paste(firstname, lastname)) %>%
      select(medalla, nombre_completo, cluster, swimtime)
    
    texto_podium <- podium %>%
      mutate(
        texto = paste0(
          medalla, ": ", nombre_completo,
          " pertenece al cluster ", cluster,
          " con un tiempo de ", as.hms(round(as.numeric(swimtime), 2)), " segundos."
        )
      ) %>%
      pull(texto) %>%
      paste(collapse = "<br>")
    
    # Resumen por cluster (sin cambios)
    resumen <- df_clusterizado %>%
      group_by(cluster) %>%
      summarise(
        n_nadadores = n(),
        tiempo_medio = mean(as.numeric(swimtime), na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        texto = paste0(
          "🌀 El cluster ", cluster, " agrupa a ", n_nadadores, " nadadores. ",
          "El tiempo medio de nado fue de ", as.hms(round(tiempo_medio, 2)), " segundos."
        )
      ) %>%
      pull(texto) %>%
      paste(collapse = "<br>")
    
    # HTML final
    HTML(paste0(
      "<h4>Resumen del clustering:</h4><p>", resumen, "</p><br>",
      "<h4>Medallistas y sus clusters:</h4><p>", texto_podium, "</p>"
    ))
  })
  
  
  #### 7. Gráfico solo de centroides -------------------------------------------
  output$larga_centroides_plot_nuevo <- renderPlot({
    req(valores_larga$df_clusterizado)
    
    df <- valores_larga$df_clusterizado
    parciales <- df[, 3:(ncol(df)-1)]
    rownames(parciales) <- df$athleteid
    
    parciales$cluster <- as.factor(df$cluster)
    parciales$athleteid <- df$athleteid
    
    parciales_long <- reshape2::melt(parciales, id.vars = c("athleteid", "cluster"))
    parciales_long$variable <- as.numeric(as.character(parciales_long$variable))
    
    centroides <- parciales_long %>%
      group_by(cluster, variable) %>%
      summarise(media = mean(value), .groups = "drop")
    
    ggplot(centroides, aes(x = variable, y = media, color = cluster)) +
      geom_line(size = 1.5) +
      geom_point(size = 2) +
      labs(title = "Perfil promedio de cada cluster (centroides)",
           x = "Distancia (m)", y = "Tiempo parcial relativo") +
      scale_x_continuous(breaks = sort(unique(centroides$variable))) +
      theme_minimal()
  })
  

    
    
  ## Página 8. EvolveCluster ---------------------------------------------------
      
    
  ### Filtro Estilo (stroke)----------------
  observeEvent(input$filtro_stroke, {
    df <- worlds2024Budapest25m %>%
      filter(stroke == input$filtro_stroke)
    
    updateSelectInput(session, "filtro_distance", choices = sort(unique(df$distance)))
  })
  
  ### Filtro Distancia -------------------------
  observeEvent(input$filtro_distance, {
    req(input$filtro_stroke)
    
    df <- worlds2024Budapest25m %>%
      filter(stroke == input$filtro_stroke, distance == input$filtro_distance)
    
    updateSelectInput(session, "filtro_gender", choices = sort(unique(df$gender)))
  })
  
  ### Filtro Género ------------------------------------
  observeEvent(input$filtro_gender, {
    req(input$filtro_stroke, input$filtro_distance)
    
    df <- worlds2024Budapest25m %>%
      filter(stroke == input$filtro_stroke,
             distance == input$filtro_distance,
             gender == input$filtro_gender)
  })
  
    
    ### ---------------------- Botón para calcular Dunn Index ----------------------
    valores_dunn <- eventReactive(input$calcular_dunn, {
      list(
        distancia = input$filtro_distance,
        stroke = input$filtro_stroke,
        round = "FIN",
        gender = input$filtro_gender,
        split = input$slider_parcial,
        k_ini = input$k_inicial
      )
    })
    
    ### ---------------------- Gráfico de Dunn Index ----------------------
    output$plot_indices_cluster <- renderPlotly({
      req(valores_dunn())
      datos <- valores_dunn()
      
      umbrales_division <- seq(0.8, 3.5, 0.1)
      puntuacionUmbral <- NULL
      
      for (umbral in umbrales_division) {
        resultados <- evolveCluster(datos$distancia, datos$stroke, "FIN", datos$gender, "1", datos$split, umbral, datos$k_ini)
        vectorValidacion <- c()
        
        for (intervalo in seq(from = datos$split, to = datos$distancia, by = datos$split)) {
          resultadosParciales <- resultados %>% filter(splitdistance == intervalo)
          if (nrow(resultadosParciales) >= 2) {
            observaciones <- resultadosParciales %>% arrange(diferenciaLider) %>% pull(diferenciaLider)
            clusters <- resultadosParciales$cluster
            vectorValidacion <- c(vectorValidacion, dunnIndex(clusters, observaciones))
          }
        }
        
        puntuacionUmbral <- rbind(puntuacionUmbral, vectorValidacion)
      }
      
      medias <- apply(puntuacionUmbral, 1, mean, na.rm = TRUE)
      
      plot_ly(
        x = ~umbrales_division,
        y = ~medias,
        type = 'scatter',
        mode = 'lines+markers',
        line = list(color = 'blue'),
        marker = list(color = 'blue', size = 6)
      ) %>%
        layout(
          title = paste0("Dunn Index | ", datos$distancia, "m - ", datos$stroke, 
                         " - FIN ", " - ", datos$gender, " - Serie 1"),
          xaxis = list(title = "Valores Umbral"),
          yaxis = list(title = "Dunn Index promedio")
        )
    })
    
    ### ---------------------- Guardar valores del botón de visualización ----------------------
    valores_calculo <- eventReactive(input$calcular_carrera, {
      list(
        distancia = input$filtro_distance,
        stroke = input$filtro_stroke,
        round = "FIN",
        gender = input$filtro_gender,
        heat = "1",
        split = input$slider_parcial,
        gap = input$gap_cluster,
        k_ini = input$k_inicial
      )
    })
    
    ### ---------------------- Visualización de la carrera con clustering ----------------------
    output$grafica_clusters <- renderPlot({
      req(valores_calculo())
      datos <- valores_calculo()
      
      resultados <- evolveCluster(
        datos$distancia, datos$stroke, "FIN", datos$gender,
        "1", datos$split, datos$gap, datos$k_ini
      )
      
      titulo_prueba <- paste0("Distancia: ", datos$distancia, "m | Estilo: ", datos$stroke,
                              " | Ronda: FIN", " | Género: ", datos$gender,
                              " | Serie: 1 ", " | Split cada ", datos$split, "m")
      
      ggplot(resultados, aes(x = splitdistance, y = -diferenciaLider,
                             color = as.factor(cluster), label = lane)) +
        geom_point(size = 5) +
        geom_text(size = 4, color = "white") +
        scale_color_manual(values = c("red", "blue", "green", "purple", "orange",
                                      "black", "lightblue", "darkgreen", "gold", "magenta")) +
        labs(x = "Parcial", y = "Diferencia respecto al líder",
             title = titulo_prueba, color = "Grupo") +
        theme_minimal() +
        theme(legend.title = element_blank(),
              legend.text = element_text(size = 8))
    })
  
    
    
}


shinyApp(ui, server)
