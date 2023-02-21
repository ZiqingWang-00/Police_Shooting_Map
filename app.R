library(shiny)
library(tidyverse)
library(ggplot2)
library(plotly)
library(shinythemes)
library(rsconnect)

# load cleaned policee shooting data
load(file = "./data/merged_dat.Rdata")
state_pop <- read.csv("./data/us-state-populations.csv", header = T) %>% 
  select(code, pop_2014)

# get choices for race/ethnicity
race_vec <- unique(merged_dat$race)

# Define UI for app 
ui <- fluidPage(theme = shinytheme("journal"),
  # add title
  titlePanel("2015-2023 US Police Shootings Dashboard"),
  
  # set tabs
  mainPanel(
    tabsetPanel(
      tabPanel("State-level Counts",
               fluidRow(
                 column(3,
                        selectInput(
                          inputId = "year",
                          label = h3("Year"),
                          choices = 2015:2023,
                          selected = 2015),
                        #selectInput(
                        #  inputId = "race",
                        #  label = h3("Race/Ethnicity"),
                        #  choices = race_vec,
                        #  selected = "White",
                        #  multiple = T),
                        checkboxGroupInput(inputId = "race", 
                                           label = h3("Race/Ethnicity"), 
                                           choices = race_vec,
                                           selected = "White"),
                        sliderInput("age", 
                                    label = h3("Age Range"), 
                                    min = min(merged_dat$age[which(!is.na(merged_dat$age))]), 
                                    max = max(merged_dat$age[which(!is.na(merged_dat$age))]), 
                                    value = c(20, 40)),
                        checkboxGroupInput(inputId = "gender", 
                                           label = h3("Gender"), 
                                           choices = c("male", "female"),
                                           selected = "male")
                        ),
                 
                 column(9, plotlyOutput("state_count", width = "100%"))
                 
               )
        
      ),
      
      tabPanel("National Trends",
               fluidRow(
                 column(width = 7, plotlyOutput("trend_by_race")),
                 column(5, plotlyOutput("trend_by_gender")),
                 column(7, plotlyOutput("trend_by_age")),
                 column(5, plotlyOutput("overall_trend"))
               ))
)))

server <- function(input, output){
  
  # state map
  output$state_count <- renderPlotly({
    # create data frame for plotting
    state_dat <- merged_dat %>%  
      filter(race %in% c(input$race),
             age >= input$age[1], age <= input$age[2],
             gender %in% c(input$gender),
             year == input$year) %>% 
      group_by(state) %>% 
      summarize(count = n()) %>% 
      left_join(., state_pop, by = c("state" = "code"))
    
    # create map
    plot_geo(state_dat, locationmode = 'USA-states',
             height = 800, width = 800) %>% 
      add_trace(
        z = ~count, locations = ~state,
        color = ~count, colors = 'Reds',
        text = ~paste("Cases per 1000000 persons: ", 
                      round(count/pop_2014*1000000, 2))
      ) %>% 
      colorbar(title = "Total case count") %>% 
      layout(geo = list(scope = "usa")) 
  
  })
  
  # national trends by race/ethnicity
  output$trend_by_race <- renderPlotly({
    # create dataset for plotting
    race_dat <- merged_dat %>% 
      filter(year != 2023) %>% 
      group_by(year, race) %>% 
      summarize(count = n()) %>% 
      ungroup()
    # line plot
    plot_ly(race_dat,
            x = ~year,
            y = ~count,
            color = ~race,
            type="scatter", mode="lines+markers")  %>% 
      layout(title = "Trend by race/ethnicity")
  })
  
  # national trends by gender
  output$trend_by_gender <- renderPlotly({
    # create dataset for plotting
    gender_dat <- merged_dat %>% 
      filter(year != 2023, gender %in% c("male", "female")) %>% 
      group_by(year, gender) %>% 
      summarize(count = n()) %>% 
      ungroup()
    # line plot
    plot_ly(gender_dat,
            x = ~year,
            y = ~count,
            color = ~gender,
            type="scatter", mode="lines+markers") %>% 
      layout(title = "Trend by gender")
  })
  
  # national trends by age
  output$trend_by_age <- renderPlotly({
    # create dataset for plotting
    age_dat <- merged_dat %>% 
      filter(year != 2023, !is.na(age)) %>% 
      mutate(age = case_when(
        age <= 18 ~ "18 or younger",
        age > 18 & age <= 26 ~ "19-26",
        age > 26 & age <= 49 ~ "27-49",
        age > 49 ~ "50 or older"
      )) %>% 
      group_by(year, age) %>% 
      summarize(count = n()) %>% 
      ungroup()
    # line plot
    plot_ly(age_dat,
            x = ~year,
            y = ~count,
            color = ~age,
            type="scatter", mode="lines+markers") %>% 
      layout(title = "Trend by age")
  })
  
  # national trends (total)
  output$overall_trend <- renderPlotly({
    # create dataset for plotting
    aggregated_dat <- merged_dat %>% 
      filter(year != 2023) %>% 
      group_by(year) %>% 
      summarize(count = n()) %>% 
      ungroup()
    # line plot
    plot_ly(aggregated_dat,
            x = ~year,
            y = ~count,
            type="scatter", mode="lines+markers") %>% 
      layout(title = "Trend in total count")
  })
    
}

shinyApp(ui = ui, server = server)


