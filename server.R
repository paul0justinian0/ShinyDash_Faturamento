#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# library(shiny)
# library(tidyverse)
# library(shinydashboard)
# library(scales)
# library(zoo)
# library(chron)

#Load the function to the local through Paul Bleicher's GitHub page
#source("https://raw.githubusercontent.com/iascchen/VisHealth/master/R/calendarHeat.R")

function(input, output, session) {
    # # Importando a base de dados:
    # data <- read.csv("Dados_Dash.csv")
    # 
    # # Removendo variáveis indesejadas:
    # data <- data %>%
    #     mutate(
    #         Day = NULL,
    #         Month = NULL,
    #         Year = NULL,
    #         Profit = NULL,
    #         Cost = NULL
    #     )
    # 
    # ## Ajustando os tipos das variáveis:
    # data <- data %>%
    #     mutate(
    #         Date = as.Date(Date, tryFormats = c("%d/%m/%Y")),
    #         Age_Group = as.factor(Age_Group),
    #         Customer_Gender = as.factor(Customer_Gender),
    #         Country = as.factor(Country),
    #         State = as.factor(State),
    #         Product_Category = as.factor(Product_Category),
    #         Sub_Category = as.factor(Sub_Category),
    #         Product = as.factor(Product)
    #     )
    # 
    # ## Criando variáveis de interesse
    # data <- data %>%
    #     mutate(
    #         order_cost = Unit_Cost * Order_Quantity,
    #         expected_revenue = Unit_Price * Order_Quantity,
    #         expected_profit =  expected_revenue - order_cost,
    #         order_profit = Revenue - order_cost,
    #         value_lost = expected_profit - order_profit,
    #         weekday = wday(Date, label = T)
    #     )
    # 
    # ## Passando pela "limpeza de nomes"
    # names(data) <- janitor::make_clean_names(names(data))
    # 
    data_filtered <- reactive({
        filter(data, country %in% input$país)
    })
    observeEvent(data_filtered(), {
        choices <- unique(data_filtered()$state)
        updatePickerInput(inputId = "estado", choices = choices)
    })
    
    category <- reactive({
        filter(data, product_category %in% input$categoria)
    })
    observeEvent(category(), {
        choices <- unique(category()$sub_category)
        updatePickerInput(inputId = "sub_categoria", choices = choices)
    })
    
    dados_filtered <- reactive({
        dados <- data %>%
            filter(
                country %in% input$país,
                state %in% input$estado,
                product_category %in% input$categoria,
                sub_category %in% input$sub_categoria,
                date >= input$datas[1] & date <= input$datas[2]
            )
    })
    
    output$pedidos <- renderInfoBox({
        valor <- dados_filtered() %>%
            summarise(Total = n()) %>%
            select(Total) %>%
            unlist(use.names = F)
        infoBox(
            "Pedidos",
            valor,
            icon = icon("ok", lib = "glyphicon"),
            color = "navy"
        )
    })
    
    output$quant_média <- renderInfoBox({
        valor <- dados_filtered() %>%
            summarise(Total = round(mean(order_quantity))) %>%
            select(Total) %>%
            unlist(use.names = F)
        infoBox("Quantidade média por pedido",
                valor,
                icon = icon("ok", lib = "glyphicon"))
    })
    
    output$faturamento <- renderInfoBox({
        valor <- dados_filtered() %>%
            summarise(Total = sum(revenue)) %>%
            select(Total) %>%
            unlist(use.names = F) %>%
            dollar(
                big.mark = ".",
                decimal.mark = ",",
                suffix = " "
            )
        infoBox(
            "Faturamento",
            valor,
            icon = icon("tags", lib = "glyphicon"),
            color = "blue"
        )
    })
    
    output$fat_médio <- renderInfoBox({
        valor <- dados_filtered() %>%
            summarise(Total = round(mean(revenue))) %>%
            select(Total) %>%
            unlist(use.names = F) %>%
            dollar(big.mark = ".", decimal.mark = ",")
        infoBox(
            "Valor de venda médio",
            valor,
            icon = icon("tag", lib = "glyphicon"),
            color = "light-blue"
        )
    })
    
    output$lucro <- renderInfoBox({
        valor <- dados_filtered() %>%
            summarise(Total = sum(order_profit)) %>%
            select(Total) %>%
            unlist(use.names = F) %>%
            dollar(
                big.mark = ".",
                decimal.mark = ",",
                suffix = " "
            )
        infoBox(
            "Lucro",
            valor,
            icon = icon("usd", lib = "glyphicon"),
            color = "green"
        )
    })
    
    output$perda <- renderInfoBox({
        valor <- dados_filtered() %>%
            summarise(Total = sum(value_lost)) %>%
            select(Total) %>%
            unlist(use.names = F) %>%
            dollar(
                big.mark = ".",
                decimal.mark = ",",
                suffix = " "
            )
        infoBox(
            "Perda",
            valor,
            icon = icon("usd", lib = "glyphicon"),
            color = "red"
        )
    })
    
    output$populares <- renderTable({
        tab <- dados_filtered() %>%
            mutate(Produto = product) %>%
            group_by(Produto) %>%
            summarise(
                Quant.Total = sum(order_quantity),
                Faturamento = dollar(
                    sum(revenue),
                    big.mark = ".",
                    decimal.mark = ","
                ),
                Lucro = dollar(
                    sum(order_profit),
                    big.mark = ".",
                    decimal.mark = ","
                ),
                Perda = dollar(
                    sum(value_lost),
                    big.mark = ".",
                    decimal.mark = ","
                )
            ) %>%
            arrange(desc(Quant.Total)) %>%
            mutate(Quant.Total = prettyNum(
                Quant.Total,
                big.mark = ".",
                decimal.mark = ","
            )) %>% 
        head(10)
    })
    
    dados_heat <- reactive({
        req(input$país,
            input$estado,
            input$categoria,
            input$sub_categoria)
        dados <- data %>%
            filter(
                country %in% input$país,
                state %in% input$estado,
                product_category %in% input$categoria,
                sub_category %in% input$sub_categoria) %>% 
            group_by(date) %>% 
            summarise(Total = sum(revenue))
    })
    
    output$calendar_heat <- renderPlot({
        req(dados_heat())
        heat <- calendarHeat(
            dates = dados_heat()$date,
            values = dados_heat()$Total,
            varname = "Faturamento"
        )
    })
    
    output$anual <- renderPlot({
        serie <- dados_filtered() %>%
            mutate(yr = year(date)) %>%
            group_by(yr) %>%
            summarise(Faturamento = sum(revenue)) %>%
            ggplot(aes(x = yr, y = Faturamento)) +
            geom_line(color = "steelblue4") +
            theme_linedraw() +
            theme(
                axis.text.x = element_text(angle = 90, vjust = 0.5),
                panel.grid.minor = element_blank()
            ) +
            xlab("Ano")
        serie
    })
    
    output$trimestral <- renderPlot({
        serie <- dados_filtered() %>%
            mutate(qtr = floor_date(as_date(date), "quarter")) %>%
            group_by(qtr) %>%
            summarise(Faturamento = sum(revenue)) %>%
            ggplot(aes(x = qtr, y = Faturamento)) +
            geom_line(color = "steelblue4") +
            theme_linedraw() +
            theme(
                axis.text.x = element_text(angle = 90, vjust = 0.5),
                panel.grid.minor = element_blank()
            ) +
            xlab("Trimestre")
        serie
    })
    
    output$mensal <- renderPlot({
        serie <- dados_filtered() %>%
            mutate(ym = floor_date(as_date(date), "month")) %>%
            group_by(ym) %>%
            summarise(Faturamento = sum(revenue)) %>%
            ungroup() %>%
            ggplot(aes(x = ym, y = Faturamento)) +
            geom_line(color = "steelblue4") +
            scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
            theme_linedraw() +
            theme(
                axis.text.x = element_text(angle = 90, vjust = 0.5),
                panel.grid.minor = element_blank()
            ) +
            xlab("Mês")
        serie
    })
    
    output$genero <- renderTable({
        tab <- dados_filtered() %>%
            mutate(Gênero = customer_gender) %>%
            group_by(Gênero) %>%
            summarise(
                Pedidos = prettyNum(n(), big.mark = ".", decimal.mark = ","),
                Quant.Total = prettyNum(
                    sum(order_quantity),
                    big.mark = ".",
                    decimal.mark = ","
                ),
                Faturamento = dollar(
                    sum(revenue),
                    big.mark = ".",
                    decimal.mark = ","
                )
            )
    })
    
    output$faixa_etaria <- renderTable({
        tab <- dados_filtered() %>%
            mutate(Faixa_Etária = age_group) %>%
            group_by(Faixa_Etária) %>%
            summarise(
                Pedidos = n(),
                Quant.Total = prettyNum(
                    sum(order_quantity),
                    big.mark = ".",
                    decimal.mark = ","
                ),
                Faturamento = dollar(
                    sum(revenue),
                    big.mark = ".",
                    decimal.mark = ","
                )
            ) %>%
            arrange(desc(Pedidos)) %>% 
            mutate(Pedidos = prettyNum(Pedidos, big.mark = ".", decimal.mark = ","))
    })
}