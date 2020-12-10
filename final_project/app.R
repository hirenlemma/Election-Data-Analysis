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
model_1 <- readRDS("model_1.rds")

ui <- navbarPage(
  "National Election Data",
  tabPanel("About", 
           titlePanel("About"),
           h3("An Overview"),
           h5("Fundamentally, this project was meant to explore trends in the
               tendecies of counties in the United States to affiliate with a
               given political party. Especially following this most recent
               election cycle, Americans and engaged citizens around the world
               are extremely inclined to try to understand their political
               counterparts. Hopefully, the analysis of the demographic and
               historical trends highlighted within this project will allow some
               greater insight into the mindset of the American electorate."),
           h3("Project Background and Motivations"),
           h5("This project will analyze trends in election data and to what
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
           h5("Hello! My name is Hiren Lemma and I study Government and African &
               African American Studies as part of the Harvard College Class of
               2024. You can reach me at hirenlemma@college.harvard.edu.")),
  tabPanel("Longitudinal Analysis",
           titlePanel("Longitudinal Analysis"),
           fluidPage(
             theme = shinytheme("yeti"),
             h5("The best way to contextualize the larger scope of this project
                 was to begin by exploring the trends over the 40 years of data
                 that was accessible. The first graph provides the total votes
                 caste per year over each presidential election cycle, and the
                 data is organized by state. This format also allows for
                 comparisons between the states, as certain trend years--such as
                 1988-- demonstrate widespread impacts on the electoral process
                 that were notable across the United States"),
             h5("Similarly, the second graph provides a longitudinal approach to
                 exploring racial dynamics year-to-year. The data used from this
                 years with Congressional elections as well, and it only counted
                 those who were eligible to vote within a given year. These
                 graphical representations provide a valuable context for the
                 data presented later on in the format of a map."),
             titlePanel("Demographic Information"),
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   inputId = "state1",
                   label = "Select State",
                   choices = state.name
                   
                   # setting the functionality of the drop down for states
                   
                 )),
               mainPanel(plotOutput("graph"),
                         br(),
                         plotOutput("other"),
                         br())),
             
             # set two distinct plot outputs by making them individualized within
             # the same mainPanel().
             
           )),
  tabPanel("Interactive Map",
           titlePanel("Interactive Map"),
           h4("This map provides county data for each presidential election from
               2000-2016. The data presentation can be adjusted between each of
               the proinent political parties, by state*, and by year."),
           p("*Unfortunately, county data from Alaska was unavailable, so
               analysis of this state is not included within the map portions of
               this project."),
           fluidPage(
             theme = shinytheme("yeti"),
             titlePanel("Long-Term Voting Data"),
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
               mainPanel(plotOutput("map"),
                         br())
                 )
           )),
  tabPanel("Comparative Map",
           titlePanel("Comparative Map"),
           h4("This geographic representation allows for a similar comparison to
               that of the interactive map, but the compatible map provides
               additional demographic context for the time period in question.
               All variables have been controlled to only include data from 2000
               to allow for fair comparisons between the data sets. The top map
               can be adjusted by party and by state*, and the bottom state can
               be adjusted by various demographic characterizations."),
           p("*As with the previous map, unfortunately, county data from Alaska
               was unavailable, so analysis of this state is not included within 
               the map portions of this project."),
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
                         br(),
                         plotOutput("comparemap"),
                         br())
           ))),
  tabPanel("Model",
           titlePanel("Regression Modeling"),
           gt_output("statmodel"),
           h5("This statistical model analyzes each characteristic within the
                data in tandem to provide predictive values based on the extent
                to which each characteristic affects tendencies within the
                election processes."),
           h5("In statistical modeling, the 'Intercept' represents the base
                characteristics within which a predictive model creates a
                preliminary approximation. In this case, this model predicts the
                democratic share of a county's voting population based on age
                distributions, rural densities, and the year in which an
                election occurs. Under these circumstances, the 'Intercept' is
                the base approximation of democratic vote share assuming a given
                county has the average population of people under 20, the
                average population of people of 65, the average population of
                those living in rural settings, and that the year in question is
                2000."),
           h5("For the first three characteristics, the values under 'Beta'
                represent the change in the predicted Democratic share given a
                1% change in population share. For example, hypothetically, if
                the average share the population under 20 within given county
                was 50% and this county had a Democratic share of 'x', an
                adjustment for a similar county with 51% of its population under
                20 would, in turn, be approximately x - 0.71596."),
           h5("The final characteristic, year, adjusts as a factor. This means
                the predicted Democratic share would adjust in a similar way to
                the top characteristics except instead of adjusting by 1%, the
                county in question would be predicted based on four-year
                increments of historical context.")))

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
      high_color = "dodgerblue3"
      low_color = "white"
    }
    if(input$party == "Republican"){
      high_color = "firebrick"
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
        scale_fill_gradient(high = high_color, low = low_color,
                            limits = c(0, 1)) +
        theme_map() +
        theme(legend.position = "right") +
        labs(caption = "Data via MIT Election Data and Science Lab")
    
  })
    
  output$partymap <- renderPlot({
    if(input$partyy == "democrat"){
      high_color = "dodgerblue3"
      low_color = "white"
      NAME = "Democrat"
    }
    if(input$partyy == "republican"){
      high_color = "firebrick"
      low_color = "white"
      NAME = "Republican"
    }
    maps_data %>%
      filter(STATE == input$state5) %>%
      ggplot(aes_string(fill = input$partyy, geometry = "geometry")) +
      geom_sf() +
      scale_fill_gradient(high = high_color, low = low_color,
                          limits = c(0, 1),
                          name = NAME) +
      theme_map() +
      theme(legend.position = "right") +
      labs(caption = "Data via MIT Election Data and Science Lab")
    
  })
  
  output$comparemap <- renderPlot({
    if(input$compare == "under_20"){
      NAME = "Under 20"
    }
    if(input$compare == "over_65"){
      NAME = "Over 65"
    }
    if(input$compare == "rural_pop"){
      NAME = "Rural Pop"
    }
    if(input$compare == "poverty"){
      NAME = "Poverty"
    }
    maps_data %>%
      filter(STATE == input$state5) %>%
      ggplot(aes_string(fill = input$compare, geometry = "geometry")) +
      geom_sf() +
      scale_fill_gradient(high = "#242440", low = "white",
                          limits = c(0, 1),
                          name = NAME) +
      theme_map() +
      theme(legend.position = "right")

  })
  
  output$statmodel <- render_gt({
    tbl_regression(model_1, intercept = TRUE,
                   label = list("(Intercept)" ~ "Intercept",
                                "under_20" ~ "Under 20",
                                "over_65" ~ "Over 65",
                                "rural_pop" ~ "Rural Population",
                                "year" ~ "Year"),
                   estimate_fun = function(x) style_sigfig(x, digits = 5)) %>%
      as_gt() %>%
      tab_header(title = "Regression of Predicted Democratic Leaning",
                 subtitle = "The Effect of Demographic Elements on Democratic Voting History")
  })
    
}

shinyApp(ui = ui, server = server)
