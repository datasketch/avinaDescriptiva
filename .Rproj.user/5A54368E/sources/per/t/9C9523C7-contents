library(dsAppLayout)
library(shiny)
library(tidyverse)
library(hgchmagic)
library(DT)
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


body {
  margin: 0;
   font-family: Barlow,sans-serif;
 font-weight: 400;
}


.irs.js-irs-0,.irs-with-grid {
 margin: 10px 10px !important;
}

.title-filters {
  font-size: 15px;
  font-weight: 600;
  letter-spacing: 1px;
  color: #424242;
}

h3 {
 color: #1E9B8C;
 font-size: 17px;
 margin-bottom: 15px;
}

span {
  font-size: 15px;
  font-weight: 400;
}

.panel {
 border-top: 2px solid #1E9B8C;
}
.control-label {
margin: 0px !important;
}

input[type='radio']:active, input[type='radio']:checked:active, input[type='radio']:checked {
    background-color: #1E9B8C;
}
input[type='radio'] {
    background-color: #C1C1C1;
    border: 3px solid #424242;
}

.form-group input, .form-group textarea {
 font-family: Barlow,sans-serif !important;
}

.margin-b {
margin-bottom: 10px;
}

.irs-bar {
    background-color: #1E9B8C !important;
    border: 1px solid #424242 !important;
}

.irs-slider {
    border: 1px solid #424242 !important;
   background-color: #1E9B8C !important;
}

.selectize-input {
  border: 1px solid #F0F4F5 !important;
  background: #F0F4F5 !important; 
  color: #424242;
  font-family: inherit;
  font-size: 15px;
  line-height: 20px;
 padding: 0px 25px !important;
}

#descripcion {
 border: 1px solid #FAD946;
 padding: 20px 20px;
 background: #fad94682;
 border-radius: 3px;
 margin-bottom: 15px;
}

.checkbox {
 margin-right: 37px;
}

.info-tool {
 display: flex;
}

.tooltip-inf {
 cursor: pointer;
 position: relative;
 margin-left: 3px;
}

.tooltip-inf .tooltiptext {
  visibility: hidden;
  width: 210px;
  higth: auto;
  background-color: #fafafa;
  color: #CE2955;
  position: absolute;
  z-index: 9999;
  top: 0;
  padding: 1rem;
  border: 1px solid #ccc;
  font-weight: 400;
  letter-spacing: normal;
  font-size: 0.75rem;
}

.tooltip-inf:hover .tooltiptext {
  visibility: visible;
}


@media (max-width: 426px) {
.app-container {
    display: block !important;
}


.panel#panel-filtros {
    width:  100% !important;
}

}


@media (max-width: 769px) {
.panel#panel-filtros {
width: 50% !important;
}
}


"
ui <- dsAppPanels( styles = styles,
                   # panel(
                   #    title = h3(id = "panel-viz", 'DATOS'), 
                   #    color = "olive", collapsed = T, width = 500,
                   #    body = div(
                   #       dataTableOutput('dataViz')
                   #    )
                   # ),
                   panel(
                      title = h3(id = "panel-filtros", 'FILTROS DE BÚSQUEDA'), 
                      color = "olive", collapsed = FALSE, id = 'panel-filtros', width = 400,
                      body = div(
                         uiOutput('descripcion'),
                         uiOutput('resultado'),
                         uiOutput('preguntas'),
                         uiOutput('genero'),
                         uiOutput('tiempo'),
                         uiOutput('valor')
                      )
                   ),
                   panel(
                      title = h3(id = "panel-viz", 'VISUALIZACIÓN'), 
                      color = "olive", collapsed = FALSE, 
                      body = div(
                         #verbatimTextOutput('aver'),
                         highchartOutput('plot_viz', height = 550)
                      )
                   )
                   
)

server <- function(input, output, session) {
   
   
   output$dataViz <- renderDataTable({
      datatable(results)
   })
   
   output$resultado <- renderUI({
      div(
         HTML('<div class= "info-tool title-filters"> VISTA POR <div class="tooltip-inf"> <i class="fa fa-info-circle"></i><span class="tooltiptext">Selecciona el tipo de vista que quieres, si seleccionas RESULTADO se grafica la conclusión promedio final de la trivia, si seleccionas PREGUNTAS podrás ver el detalle de cada una de las preguntas de la trivia</span</div></div></div>'),
      radioButtons('id_res', ' ', c('Resultado', 'Pregunta'))
      )
   })
   
   output$preguntas <- renderUI({
      
      res_v <- input$id_res
      if (is.null(res_v)) return()
      if (res_v == 'Resultado') return()
      
      selectizeInput('id_q',  ' ', setNames( unique(data_trivia$question_id), unique(data_trivia$id_text)))
      
   })
   
   output$genero <- renderUI({
      div(
         HTML('<div class= "info-tool title-filters margin-b"> CLASIFICAR POR  <div class="tooltip-inf"> <i class="fa fa-info-circle"></i><span class="tooltiptext">Aquí puedes seleccionar la agrupación de la gráfica por género, si no seleccionas ninguna se mostrará la gráfica del total de personas que han resuelto la trivia.</span</div></div></div>'),
      checkboxGroupInput('id_gen', ' ',c('Mujeres' = 'mujer', 'Hombres' = 'hombre'), selected = c('mujer', 'hombre'))
      )
   })
   
   output$tiempo <- renderUI({
      
      min_f <-  min(data_summary$Fecha)
      max_f <- max(data_summary$Fecha)
      div(
         HTML('<div class= "info-tool title-filters margin-b"> RANGO DE TIEMPO<div class="tooltip-inf"> <i class="fa fa-info-circle"></i><span class="tooltiptext">Aquí se muestran las fechas durante las cuales distintas personas han resuelto la trivia, puedes arrastrar los círculos y filtrar por las fechas que desees. </span</div></div></div>'),
      sliderInput('id_time', ' ', 
                  min = min_f,
                  max = max_f,
                  value= c(min_f, 
                           max_f))
      )
   })
   
   
   output$valor <- renderUI({
      div(
         HTML('<div class= "info-tool title-filters"> MOSTRAR POR <div class="tooltip-inf"> <i class="fa fa-info-circle"></i><span class="tooltiptext"> Número total o proporción de las personas que han resuelto la trivia</span</div></div></div>'),
      radioButtons('id_valor', ' ', c('Total', 'Proporción'), inline = T)
      )
   })
   
   data_filter <- reactive({
      
      id_genero <- input$id_gen
      if (is.null(id_genero)) id_genero <- c('mujer', 'hombre')
      
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
         dt_f <- dt_f %>% select(`Género` = identifies_as, Respuestas = choices) 
      } else {
         dt_f <- dt_f  %>% 
                  group_by(session_id, identifies_as) %>% 
                   summarise(mean = mean(answer_weight, na.rm = T))
         
         dt_f$Tipo <- ifelse(dt_f$mean > 55, 'Guardian@s', 'Viv@s')
         dt_f <- dt_f %>%
            select(Tipo, `Género` = identifies_as) 
         dt_f <- dt_f[,-1]
      }
 
 
      if (length(unique(dt_f$`Género`)) == 1 | is.null(input$id_gen)) dt_f <- dt_f %>% select(-`Género`)
      
      dt_f
      
      
   })
   
   
   output$descripcion <- renderUI({


      id_genero <- input$id_gen
      if (is.null(id_genero)) ig_genero <- c('mujer', 'hombre')

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

      tx <- HTML(paste0('Del ', tiempo_min, ' hasta ', tiempo_max,' han resuelto la trivia ', sum(dt_g$Total, na.rm = T), ' personas, de las cuales ', t_m ,' son mujeres y ', t_h ,' son hombres. <br/>  <br/> Puedes elegir que controles usar para filtrar los resultados de la trivia. Para más detalle de los filtros, pasa el cursor por el icono de información.'))

      tx
    }) 
   
   
   # output$aver <- renderPrint({
   #    data_filter()
   # })
   
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

      tm_l <- list (
         font_family = "Barlow",
         font_color = '#424242',
         stylesLabelY_fontSize = '13px',
         labsData_familyLabel = "Barlow",
         colors = c('#1E9B8C', '#CE2955', '#E0800E', '#A2ED66', '#2EC4B5', '#F2BF2F')
      )
      list_op <- list(percentage = pr,
                      agg_text = ' ',
                      horLabel = ' ',
                      verLabel = ' ',
                      orientation = 'hor',
                      labelWrap = 100,
                      labelWrapV = c(100, 100),
                      theme = tma(tm_l))

      if (ncol(dt_v) == 2) {
         v <- hgch_bar_CatCat(dt_v, opts = list_op)
      } else {
         v <- hgch_bar_Cat(dt_v, opts = list_op)
      }
      v

   })
   
}

shinyApp(ui, server)

