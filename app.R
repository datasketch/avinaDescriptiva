library(dsAppLayout)
library(shiny)
library(tidyverse)
library(hgchmagic)
# Data

data_trivia <- read_csv('data/dic_trivia.csv')
data_trivia <- data_trivia %>% filter(!question_id %in% c(3, 4))
results <- read_csv('https://avina-trivia-verdad.s3.amazonaws.com/data/db.csv')


data_summary <-  results %>% 
   separate(timestamp, c('Fecha', 'Hora'), sep = " ")


data_summary$Fecha <- as.Date(data_summary$Fecha, "%Y-%m-%d")

styles <- 
   "
@import url('https://fonts.googleapis.com/css?family=Barlow:400,600,700&display=swap');

*,
*:before,
*:after{
  box-sizing: inherit;
}

html {
  box-sizing: border-box;
  font-family: 'Barlow', sans-serif;
}

body {
  margin: 0;
}

.irs-grid {
width: 80% !important;
}

.irs-to {
left: 87.3531% !important;
}

.irs-from {
left: 0.39562% !important;
}

"
ui <- dsAppPanels( styles = styles,
                   panel(
                      title = h3(id = "panel-filtros", 'FILTROS DE BÚSQUEDA'), 
                      color = "olive", collapsed = FALSE, width = 500,
                      body = div(
                         uiOutput('resultado'),
                         uiOutput('preguntas'),
                         uiOutput('genero'),
                         uiOutput('tiempo'),
                         uiOutput('valor'),
                         uiOutput('descripcion')
                      )
                   ),
                   panel(
                      title = h3(id = "panel-viz", 'VISUALIZACIÓN'), 
                      color = "olive", collapsed = FALSE, 
                      body = div(
                         #verbatimTextOutput('aver'),
                         highchartOutput('plot_viz')
                      )
                   )
                   
)

server <- function(input, output, session) {
   
   
   output$resultado <- renderUI({
      radioButtons('id_res', 'Vista por', c('Pregunta', 'Resultado'))
   })
   
   output$preguntas <- renderUI({
      
      res_v <- input$id_res
      if (is.null(res_v)) return()
      if (res_v == 'Resultado') return()
      
      selectizeInput('id_q',  ' ', setNames( unique(data_trivia$question_id), unique(data_trivia$id_text)))
      
      
      
      
   })
   
   output$genero <- renderUI({
      
      checkboxGroupInput('id_gen', 'Clasificar por', c('Mujeres' = 'mujer', 'Hombres' = 'hombre'), selected = c('mujer', 'hombre'))
   })
   
   output$tiempo <- renderUI({
      
      min_f <-  min(data_summary$Fecha)
      max_f <- max(data_summary$Fecha)
      sliderInput('id_time', 'Selecciona rango de tiempo', 
                  min = min_f,
                  max = max_f,
                  value= c(min_f, 
                           max_f))
   })
   
   
   output$valor <- renderUI({
      radioButtons('id_valor', 'Mostrar', c('Proporción', 'Total'), inline = T)
   })
   
   data_filter <- reactive({
      
      id_genero <- input$id_gen
      if (is.null(id_genero)) return()
      
      tiempo_min <- input$id_time[1]
      if (is.null(tiempo_min)) return()
      
      tiempo_max <- input$id_time[2]
      if (is.null(tiempo_max)) return() 
      
      dt_f <- data_summary %>%
         filter(Fecha >= tiempo_min & Fecha <=tiempo_max,
                identifies_as %in% id_genero)
      
      id_vista <- input$id_res
      
      if (id_vista == 'Pregunta') {
         quest <- input$id_q
         dic_triv <- data_trivia %>% 
            filter(question_id %in% quest) %>% select(choices, answer_weight = choices_id)
         dt_f <- dt_f %>% filter(question_id %in% quest)
         dt_f <- dt_f %>% left_join(dic_triv)
         dt_f <- dt_f %>% select(identifies_as, choices)
         print(dt_f)
      } else {
         dt_f <- dt_f  %>% 
                  group_by(session_id, identifies_as) %>% 
                   summarise(mean = mean(answer_weight, na.rm = T))
         
         dt_f$Tipo <- ifelse(dt_f$mean > 55, 'Guardian@s', 'Viv@s')
         dt_f <- dt_f %>%
                  select(Tipo, identifies_as)
         dt_f <- dt_f[,-1]
      }
 
 
      if (length(unique(dt_f$identifies_as)) == 1) dt_f <- dt_f %>% select(-identifies_as)
      
      dt_f
      
      
   })
   
   
   output$descripcion <- renderUI({


      id_genero <- input$id_gen
      if (is.null(id_genero)) return('Debes seleccionar al menos una opción en Clasificar por')

      tiempo_min <- input$id_time[1]
      if (is.null(tiempo_min)) return()
      tiempo_max <- input$id_time[2]
      if (is.null(tiempo_max)) return()

      dt_r <- data_summary %>%
               filter(Fecha >= tiempo_min & Fecha <=tiempo_max)  %>% distinct(session_id, .keep_all = T)

      dt_g <- dt_r %>% group_by(identifies_as) %>% summarise(Total = n())
      t_m <- dt_g$Total[dt_g$identifies_as == 'mujer']
      t_h <- dt_g$Total[dt_g$identifies_as == 'hombre']
      if (identical(t_h, integer(0))) t_h <- 0
      if (nrow(data_filter()) == 0) tx <- 'Nadie a respondido la trivia en el periodo de tiempo seleccionado'

      tx <- HTML(paste0('Del ', tiempo_min, ' hasta ', tiempo_max,' han resuelto la trivia ', sum(dt_g$Total, na.rm = T), ' personas, de las cuales ', t_m ,' son mujeres y ', t_h ,' son hombres'))

      tx
    }) 
   
   
   output$aver <- renderPrint({
      data_filter()
   })
   
   output$plot_viz <- renderHighchart({

      dt_v <- data_filter()
      if (is.null(dt_v)) return()
      if (nrow(dt_v) == 0) return()

      pr <- input$id_valor
      if (is.null(pr)) return()

      if (pr == 'Proporción') {
         pr <- TRUE
      } else {
         pr <- FALSE
      }


      list_op <- list(percentage = pr,
                      agg_text = ' ',
                      labelWrap = 100,
                      labelWrapV = c(100, 100))

      if (ncol(dt_v) == 2) {
         v <- hgch_bar_CatCat(dt_v, opts = list_op)
      } else {
         v <- hgch_bar_Cat(dt_v, opts = list_op)
      }
      v

   })
   
}

shinyApp(ui, server)

