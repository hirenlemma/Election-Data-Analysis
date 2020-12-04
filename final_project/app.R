#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(tidyverse)
library(ggthemes)
library(tidycensus)
library(readr)
library(readxl)
library(rstanarm)
library(gtsummary)
library(broom.mixed)
library(gt)
raw_data <- read_csv("raw_data.csv")
adjusted_pop_data <- read_excel("adjusted_pop_data.xlsx")
census_api_key("75b2f27ae13c75bf44dd783ba197c3c236988b0c")
county_data <- read_csv("countypres_2000-2016.csv")
poverty <- read_csv("poverty.csv",
                    col_types = cols("County ID" = col_integer()))
rural <- readRDS("mapdata.RDS")
statistical_data <- readRDS("statistical_data.rds")
maps_data <- readRDS("maps_data.rds")

ui <- navbarPage(
  "National Election Data",
  tabPanel("Longitudinal Analysis",
           fluidPage(
             theme = shinytheme("yeti"),
             titlePanel("Voter Data"),
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   inputId = "state1",
                   label = "Select State",
                   choices = state.name
                   
                   # setting the functionality of the drop down for states
                   
                 )),
               mainPanel(plotOutput("graph"),
                         plotOutput("other")))
             
             # set two distinct plot outputs by making them individualized within
             # the same mainPanel().
             
           )),
  tabPanel("Interactive Map",
           fluidPage(
             theme = shinytheme("yeti"),
             titlePanel("Voter Data"),
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   inputId = "party",
                   label = "Republican or Democrat",
                   choices = c("Republican", "Democrat")
                 ),
                 selectInput(
                   inputId = "state2",
                   label = "Select State",
                   choices = state.name[which(state.name != "Alaska")]
                 ),
                 selectInput(
                   inputId = "year",
                   label = "Select Year",
                   choices = c(2000, 2004, 2008, 2012, 2016)
                 )),
               mainPanel(plotOutput("map"))
                 )
           )),
  tabPanel("Comparitive Map",
           fluidPage(
             theme = shinytheme("yeti"),
             titlePanel("Comparisons between Demographic Features"),
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   inputId = "state5",
                   label = "Select State",
                   choices = state.name[which(state.name != "Alaska")]
                 ),
                 selectInput(
                   inputId = "partyy",
                   label = "Republican or Democrat",
                   choices = c("Republican" = "republican",
                               "Democrat" = "democrat")
                 ),
                 selectInput(
                   inputId = "compare",
                   label = "Select Point of Comparison",
                   choices = c("Population Under 20" = "under_20",
                               "Population Over 65" = "over_65",
                               "Rural Population" = "rural_pop",
                               "Poverty" = "poverty")
                 )),
               mainPanel(plotOutput("partymap"),
                         plotOutput("comparemap"))
           ))),
  tabPanel("Model",
           titlePanel("Regression Modeling"),
           gt_output("statmodel")),
  tabPanel("Discussion",
           titlePanel("Discussion Title"),
           p("Tour of the modeling choices you made and 
              an explanation of why you made them")),
  tabPanel("About", 
           titlePanel("About"),
           h3("Project Background and Motivations"),
           p("This project will analyze trends in election data and to what
               extent they are affected by race, ethnicity, region, historical
               trends, and other similar factors. Further inferences can be
               drawn by charting data over the course of multiple years and
               election cycles. As the most recent election season is upon us,
               analyses of electoral trends, especially in conversations
               surrounding voter supression and wider societal equity, are more
               vital than ever. The current political atmosphere in the United
               States is as conducive for critical historical analysis as if not
               more so than any other period in recent history."),
           h3("About Me"),
           p("My name is Hiren Lemma and I study Government and African &
               African American Studies. You can reach me at
               hirenlemma@college.harvard.edu.")))

server <- function(input, output) {
  output$graph <- renderPlot({
    raw_data %>%
      filter(state == input$state1) %>%
      
      # set the input$state value here for filtering
      
      select(year, state, totalvotes) %>%
      group_by(year) %>%
      summarise(total_vote = sum(totalvotes)) %>%
      
      # used a summarise function to find total number of votes per year
      
      ggplot(aes(x = year, y = total_vote)) +
      geom_line(color = "#8596a7",
                lwd = 1.5) +
      scale_fill_brewer(palette = "Pastel2") +
      scale_x_continuous(breaks = seq(1976, 2016, by = 4)) +
      scale_y_continuous(labels = scales::comma) +
      
      # used seq() to set the time range and four year sections, and used 
      # scales::comma to change how the values along the y-axis presented
      
      theme_pander() +
      labs(title = "Total Votes Cast for Presidential Elections",
           x = "Year",
           y = "Total Votes Cast",
           caption = "Data via MIT Election Data and Science Lab")
  })
  
  output$other <- renderPlot({
    adjusted_pop_data %>%
      filter(STATE == input$state1) %>%
      
      # again used the same input$state value here
      
      ggplot(aes(x = YEAR, y = ELIGIBLE, fill = RACE)) +
      geom_col(position = "dodge") +
      
      # set position to "dodge" so that the races/ethnicities could be compared
      # more effectively
      
      scale_fill_manual(values = c("#242440", "#8596a7", "#a6cccc",
                                   "#eeeedb")) +
      scale_x_continuous(breaks = c(2000, 2010, 2018),
                         labels = c(2000, 2010, 2018)) +
      scale_y_continuous(labels = scales::comma) +
      theme_pander() +
      labs(title = "Distribution of Eligible Voters, by Race",
           x = "Year",
           y = "Number of Eligible Voters",
           caption = "Data via Pew Research Center")
    
  })
  
  output$map <- renderPlot({
    if(input$party == "Democrat"){
      high_color = "dodgerblue"
      low_color = "white"
    }
    if(input$party == "Republican"){
      high_color = "indianred3"
      low_color = "white"
    }
    county_data %>%
      filter(party %in% c("democrat", "republican")) %>%
      mutate(vote_percentage = candidatevotes/totalvotes) %>%
      distinct(year, party, FIPS, .keep_all = TRUE) %>%
      pivot_wider(id_cols = c("year", "FIPS"),
                  names_from = "party",
                  values_from = "vote_percentage") %>%
      unnest() %>%
      left_join(rural, by = "FIPS") %>%
      mutate(state_name = trimws(str_extract(state, "(?<=,).*"))) %>%
      select(year, FIPS, democrat, republican, geometry, state_name) %>%
      rename(Republican = republican,
             Democrat = democrat) %>%
      filter(state_name == input$state2,
             year == input$year) %>%
      ggplot(aes_string(fill = input$party, geometry = "geometry")) +
        geom_sf() +
        scale_fill_gradient(high = high_color, low = low_color) +
        theme_map() +
        theme(legend.position = "right") +
        labs(caption = "Data via x")
    
    # mid = 
    
  })
    
  output$partymap <- renderPlot({ 
    if(input$partyy == "democrat"){
      high_color = "dodgerblue"
      low_color = "white"
    }
    if(input$partyy == "republican"){
      high_color = "indianred3"
      low_color = "white"
    }
    maps_data %>%
      filter(STATE == input$state5) %>%
      ggplot(aes_string(fill = input$partyy, geometry = "geometry")) +
      geom_sf() +
      scale_fill_gradient(high = high_color, low = low_color) +
      theme_map() +
      theme(legend.position = "right") +
      labs(caption = "Data via x")
    
  })
  
  output$comparemap <- renderPlot({ 
    maps_data %>%
      filter(STATE == input$state5) %>%
      ggplot(aes_string(fill = input$compare, geometry = "geometry")) +
      geom_sf() +
      scale_fill_gradient(high = "#8596a7", low = "white") +
      theme_map() +
      theme(legend.position = "right") +
      labs(caption = "Data via x")

  })
  
  output$statmodel <- render_gt({
    model_1 <- stan_glm(data = statistical_data,
                        formula = democrat ~ under_20 + over_65 + rural_pop +
                          YEAR,
                        refresh = 0)
    tbl_regression(model_1) %>%
      as_gt() %>%
      tab_header(title = "Regression of Predicted Democratic Leaning",
                 subtitle = "The Effect of Demographic Elements on Democratic Voting History")
  })
    
}

shinyApp(ui = ui, server = server)
