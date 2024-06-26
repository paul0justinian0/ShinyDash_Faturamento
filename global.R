library(shiny)
library(tidyverse)
library(shinydashboard)
library(scales)
library(zoo)
library(chron)

source("https://raw.githubusercontent.com/iascchen/VisHealth/master/R/calendarHeat.R")


# Importando a base de dados:
data <- read.csv("Dados_Dash.csv")

# Removendo variáveis indesejadas:
data <- data %>%
    mutate(
        Day = NULL,
        Month = NULL,
        Year = NULL,
        Profit = NULL,
        Cost = NULL
    )

## Ajustando os tipos das variáveis:
data <- data %>%
    mutate(
        Date = as.Date(Date, tryFormats = c("%d/%m/%Y")),
        Age_Group = as.factor(Age_Group),
        Customer_Gender = as.factor(Customer_Gender),
        Country = as.factor(Country),
        State = as.factor(State),
        Product_Category = as.factor(Product_Category),
        Sub_Category = as.factor(Sub_Category),
        Product = as.factor(Product)
    )

## Criando variáveis de interesse
data <- data %>%
    mutate(
        order_cost = Unit_Cost * Order_Quantity,
        expected_revenue = Unit_Price * Order_Quantity,
        expected_profit =  expected_revenue - order_cost,
        order_profit = Revenue - order_cost,
        value_lost = expected_profit - order_profit,
        weekday = wday(Date, label = T)
    )

## Passando pela "limpeza de nomes"
names(data) <- janitor::make_clean_names(names(data))
