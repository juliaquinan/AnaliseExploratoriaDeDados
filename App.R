library(dplyr)
library(tidyr)
library(data.table)
library(scales)
library(markdown)
library(shiny)
library(zoo)
library(htmlwidgets)
library(shinyWidgets)
library(RColorBrewer)
library(knitr)
library(ggplot2)

world_happiness.data.table <- data.table::fread("World_Happiness_Report_2005-2021.csv", encoding = "Latin-1") %>% 
  dplyr::mutate(Year = as.integer(Year))

limpeza.nomes <- function(nomes)
{
  nomes.limpos <- nomes %>% stringr::str_replace_all("/", "") %>% stringr::str_replace_all("\\s", "_") %>% stringr::str_replace_all("__", "_")
  return(nomes.limpos)  
}
names(world_happiness.data.table) <- limpeza.nomes(names(world_happiness.data.table))

world_happiness.data.table.semna = na.omit(world_happiness.data.table)

ui <- shinyUI(
  fluidPage(
    tags$head(tags$style(
      "body { word-wrap: break-word; }"
    )),
    navbarPage("Análise exploratória de dados",
               tabPanel("Análise do World Happiness Report",
                        p("ÍNDICES RELACIONADOS À FELICIDADE NOS PAÍSES AO LONGO DOS ANOS"),
                        mainPanel(plotOutput("Happiness")),
                        flowLayout(
                          selectInput("Pais", label = "Escolha um país:",
                                      choices = unique(world_happiness.data.table.semna$Country_name), selected = "Brazil"),
                          uiOutput("Variavel"),
                          selectInput('cor', label = 'Escolha uma cor:',
                                      choices = c("red", "grey", "black", "blue", "green"), selected = "red")
                        ),
                        numericRangeInput("Year", label = "Selecione os anos:",
                                          value = c(min(2007), max(2021))
                        ),
                        numericRangeInput("Indice", label = "Explore os índices:",
                                          value = c(min(0), max(100)))
               )
    )
  )
)

server <- shinyServer(function(input, output, session){
  
  output$Variavel <- renderUI({
    req(input$Pais)
    df <- world_happiness.data.table.semna[Country_name == input$Pais]
    choices <- setdiff(names(df), c("Year", "Country_name"))
    selectInput("Variavel", label = "Escolha uma variável:", choices = choices, selected = choices[1])
  })
  
  observe({
    req(input$Pais)
    df <- world_happiness.data.table.semna[Country_name == input$Pais]
    choices <- setdiff(names(df), c("Year", "Country_name"))
    updateSelectInput(session, "Variavel", choices = choices)
  })
  
  output$Happiness <- renderPlot({
    req(input$Variavel)
    ggplot(data = world_happiness.data.table.semna[Country_name == input$Pais], aes_string(x = "Year", y = input$Variavel)) +
      geom_line(color = input$cor) +
      xlim(input$Year[1], input$Year[2]) +
      ylim(input$Indice[1], input$Indice[2]) +
      theme_classic()
  })
})

shinyApp(ui = ui, server = server)
