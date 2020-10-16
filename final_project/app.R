#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(ggthemes)
library(tidycensus)
raw_data <- read_csv("final_project/raw_data.csv")
adjusted_pop_data <- read_excel("final_project/adjusted_pop_data.xlsx")

ui <- navbarPage(
    "National Election Data",
    tabPanel("Models",
             fluidPage(
                 titlePanel("Voter Data"),
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(
                             inputId = "state",
                             label = "Select State",
                             choices = state.name
                         )),
                     mainPanel(plotOutput("graph"),
                               plotOutput("other")))
             )),
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
      filter(state == input$state) %>%
      select(year, state, totalvotes) %>%
      group_by(year) %>%
      summarise(total_vote = sum(totalvotes)) %>%
      ggplot(aes(x = year, y = total_vote)) +
      geom_line() +
      scale_x_continuous(breaks = seq(1976, 2016, by = 4)) +
      scale_y_continuous(labels = scales::comma) +
      theme_pander() +
      labs(title = "Total Votes Cast for Presidential Elections",
           x = "Year",
           y = "Total Votes Cast",
           caption = "Data via MIT Election Data and Science Lab")
  })
  
  output$other <- renderPlot({
    adjusted_pop_data %>%
      filter(STATE == input$state) %>%
      ggplot(aes(x = YEAR, y = ELIGIBLE, fill = RACE)) +
      geom_col(position = "dodge") +
      scale_fill_brewer(palette = "Pastel2") +
      scale_y_continuous(labels = scales::comma) +
      theme_pander() +
      labs(title = "Distribution of Eligible Voters, by Race",
           x = "Year",
           y = "Number of Eligible Voters",
           caption = "Data via Pew Research Center")
    
  })
}

shinyApp(ui = ui, server = server)

