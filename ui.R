library(shiny)
library(tidyverse)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)

div_placeholder <- function(content = "PLACEHOLDER",
                            color = "#d2d2d2",
                            height = "3em") {
    css <- c(
        sprintf("background-color: %s;", color),
        "padding: 5px;",
        "margin: 5px;",
        "border-radius: 5px;",
        sprintf("height: %s;", height)
    )
    div(content, style = paste0(css, collapse = " "))
}


dashboardPage(
    dashboardHeader(title = "Specialized"),
    
    dashboardSidebar(
        br(),
        br(),
        pickerInput(
            "país",
            "País",
            choices = unique(data$country),
            multiple = T,
            options = list(`actions-box` = T),
            selected = unique(data$country)[1]
        ),
        pickerInput(
            "estado",
            "Estado",
            choices = NULL,
            multiple = T,
            options = list(`actions-box` = T)
        ),
        br(),
        pickerInput(
            "categoria",
            "Categoria",
            choices = unique(data$product_category),
            multiple = T,
            options = list(`actions-box` = T),
            selected = unique(data$product_category)[1]
        ),
        pickerInput(
            "sub_categoria",
            "Sub Categoria",
            choices = NULL,
            multiple = T,
            options = list(`actions-box` = T)
        ),
        br(),
        dateRangeInput(
            "datas",
            "Período Analisado",
            format = "dd/mm/yyyy",
            min = min(data$date),
            max = max(data$date),
            start = min(data$date),
            end = max(data$date)
        )
    ),
    dashboardBody(
        infoBoxOutput("pedidos"),
        infoBoxOutput("faturamento"),
        infoBoxOutput("lucro"),
        infoBoxOutput("quant_média"),
        infoBoxOutput("fat_médio"),
        infoBoxOutput("perda"),
        
        p(strong("Top produtos mais vendidos")),
        
        fluidRow(column(width = 6, tableOutput("populares")),
                 column(width = 6, plotOutput("calendar_heat"))),
        
        p(strong("Faturamento Histórico")),
        
        tabsetPanel(
            id = "período",
            tabPanel("Anual", plotOutput("anual")),
            tabPanel("Trimestral", plotOutput("trimestral")),
            tabPanel("Mensal", plotOutput("mensal"))
        ),
        
        br(),
        
        p(strong("Vendas por Gênero e Faixa Etária")),
        
        fluidRow(column(width = 6, tableOutput("genero")), column(width = 6, tableOutput("faixa_etaria")))
    )
)
