
# Load libraries needed to create app, graphs, gt table

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
census_api_key("75b2f27ae13c75bf44dd783ba197c3c236988b0c")

# Load data sets needed, objects in RDS file format

raw_data <- read_csv("raw_data.csv")
adjusted_pop_data <- read_excel("adjusted_pop_data.xlsx")
county_data <- read_csv("county_data.csv")
poverty <- read_csv("poverty.csv",
                    col_types = cols("County ID" = col_integer()))
statistical_data <- readRDS("statistical_data.rds")
maps_data <- readRDS("maps_data.rds")
model_1_table <- readRDS("model_1_table.rds")
county_party <- readRDS("county_party.rds")

ui <- navbarPage(
  "National Election Data",
  
  # About section
  
  tabPanel("About", 
           titlePanel("About"),
           h3("An Overview"),
           h5("Fundamentally, this project was meant to explore trends in the
               tendencies of counties in the United States to affiliate with a
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
               surrounding voter suppression and wider societal equity, are more
               vital than ever. The current political atmosphere in the United
               States is as conducive for critical historical analysis as–if not
               more so than–any other period in recent history."),
           h3("Sources"),
           h5("The data included within this project stem from two main
               databases."),
           HTML("<h5> The data indicating the political affiliation of each
                 county based on voting records in presidential elections was
                 collected from the <a href='https://electionlab.mit.edu/data'>
                 MIT Election Data and Science Lab</a>. This database linked
                 directly to <a href='https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/42MVDX'>
                 this page within the Harvard Dataverse</a>.</h5>"),
           HTML("<h5> The other data, which provided county-level demographic
                 information across the United States, was collected within the
                 <a href='www.nhgis.org'> IPUMS National Historical Geographic
                 Information System database</a>. This site is a conglomeration
                 of U.S. Census data tables that was used within this project to
                 represent the ages of constituents and the density of poverty,
                 rural living, and educational attainment</a>.</h5>"),
           h3("About Me"),
           h5("Hello! My name is Hiren Lemma and I study Government and African
               & African American Studies as part of the Harvard College Class
               of 2024. You can reach me at hirenlemma@college.harvard.edu.")),
  
  # Section with data analysis over longer period of time
  
  tabPanel("Longitudinal Analysis",
           titlePanel("Longitudinal Analysis"),
           fluidPage(
             theme = shinytheme("yeti"),
             
             # Set theme, continued throughout all tabPanels
             
             h5("The best way to contextualize the larger scope of this project
                 was to begin by exploring the trends over the 40 years of data
                 that was accessible. The first graph provides the total votes
                 caste per year over each presidential election cycle, and the
                 data is organized by state. This format also allows for
                 comparisons between the states, as certain trend years–such as
                 the period 1984 to 1992–demonstrate widespread impacts on the
                 electoral process that were notable across the United States"),
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
                   
                   # Setting the functionality of the drop down for states
                   # state.name is a pre-created data set within R
                   
                 )),
               mainPanel(plotOutput("linegraph"),
                         br(),
                         plotOutput("race"),
                         br())),
             
             # Created two plot outputs based on the 'state1' input correlated
             # code included below in server
             # Included br() to add space between and below graphs

           )),
  tabPanel("Interactive Map",
           titlePanel("Interactive Map"),
           h5("This map provides county data for each presidential election from
               2000-2016. The data presentation can be adjusted between each of
               the prominent political parties, by state*, and by year."),
           h6("*Unfortunately, county data from Alaska was unavailable, so
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
               
               # Create selection input options for political party, state, and
               # year
               # Use new inputId because this input is interacting with a
               # distinct server from that of the last graph
               
               mainPanel(plotOutput("interactivemap"),
                         br())
                 )
           )),
  tabPanel("Comparative Map",
           titlePanel("Comparative Map"),
           h5("This geographic representation allows for a similar comparison to
               that of the interactive map, but the compatible map provides
               additional demographic context for the time period in question.
               All variables have been controlled to only include data from 2000
               to allow for fair comparisons between the data sets. The top map
               can be adjusted by party and by state*, and the bottom state can
               be adjusted by various demographic characterizations."),
           h6("*As with the previous map, unfortunately, county data from Alaska
               was unavailable, so analysis of this state is not included within 
               the map portions of this project."),
           h5("Please be aware that, unlike in the prior map, the scaling for
              density adjusts by state and by characteristic. Though this
              maintains the ability for comparative analysis between counties
              and among distinct county-level characterizations, it is crucial
              to be aware of when appraising the state as a whole."),
           fluidPage(
             theme = shinytheme("yeti"),
             titlePanel("Comparisons between Demographic Features"),
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   inputId = "state3",
                   label = "Select State",
                   choices = state.name[which(state.name != "Alaska")]
                 ),
                 selectInput(
                   inputId = "party2",
                   label = "Republican or Democrat",
                   choices = c("Republican", "Democrat")
                 ),
                 selectInput(
                   inputId = "compare",
                   label = "Select Point of Comparison",
                   choices = c("Population Under 20" = "under_20",
                               "Population Over 65" = "over_65",
                               "Rural Population" = "rural_pop",
                               "Poverty" = "poverty",
                               "No Secondary Education" = "no_hs",
                               "Bachelor's and Beyond" = "higheredu")
                 )),
               
               # Created new inputIds for state, political party, and metric of
               # comparison
               # Excluded Alaska from options for state selection because of
               # little available data on counties in the state
               # Used "" = "" format to make options for comparisons clearer
               
               mainPanel(plotOutput("partymap"),
                         br(),
                         plotOutput("comparemap"),
                         br())
           ))),
  tabPanel("Model",
           titlePanel("Regression Modeling"),
           gt_output("statmodel"),
           
           # Used gt_output in this case because of different data
           # representation format
           
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
                the average share of the population under 20 within given county
                was 50%, and this county had a Democratic share of 'x', an
                adjustment for a similar county with 51% of its population under
                20 would, in turn, be approximately x - 0.71596."),
           h5("The final characteristic, year, adjusts as a factor. This means
                the predicted Democratic share would adjust in a similar way to
                the top characteristics except instead of adjusting by 1%, the
                county in question would be predicted based on four-year
                increments of historical context.")))

server <- function(input, output) {
  output$linegraph <- renderPlot({
    raw_data %>%
      filter(state == input$state1) %>%
      
      # Used input$[variable] format here for filtering
      
      select(year, state, totalvotes) %>%
      group_by(year) %>%
      summarise(total_vote = sum(totalvotes)) %>%
      ggplot(aes(x = year, y = total_vote)) +
      geom_line(color = "#8596a7",
                lwd = 1.5) +
      scale_fill_brewer(palette = "Pastel2") +
      scale_x_continuous(breaks = seq(1976, 2016, by = 4)) +
      scale_y_continuous(labels = scales::comma) +
      
      # Used seq() to set the time range and four year sections
      # Used scales::comma to change how the values along the y-axis presented
      
      theme_pander() +
      labs(title = "Total Votes Cast for Presidential Elections",
           x = "Year",
           y = "Total Votes Cast",
           caption = "Data via MIT Election Data and Science Lab")
    
  })
  
  output$race <- renderPlot({
    adjusted_pop_data %>%
      filter(STATE == input$state1) %>%
      ggplot(aes(x = YEAR, y = ELIGIBLE, fill = RACE)) +
      geom_col(position = "dodge") +
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
  
  output$interactivemap <- renderPlot({
    if(input$party == "Democrat"){
      high_color = "dodgerblue3"
    }
    if(input$party == "Republican"){
      high_color = "firebrick"
    }
    
    # Used if(){} format here to adjust colors used to convey density on maps
    # based on political party
    # Created new objects 'high_color' and 'low_color' to be used as inputs
    # later in code
    
    county_party %>%
      filter(state_name == input$state2,
             year == input$year) %>%
      ggplot(aes_string(fill = input$party, geometry = "geometry")) +
        geom_sf() +
        scale_fill_gradient(high = high_color, low = "white",
                            limits = c(0, 1)) +
        theme_map() +
        theme(legend.position = "right") +
        labs(caption = "Data via MIT Election Data and Science Lab")
    
  })
    
  output$partymap <- renderPlot({
    if(input$party2 == "Democrat"){
      high_color = "dodgerblue3"
    }
    if(input$party2 == "Republican"){
      high_color = "firebrick"
    }
    
    # Again, used if(){} format to adjust by political party
    
    maps_data %>%
      filter(STATE == input$state3) %>%
      ggplot(aes_string(fill = input$party2, geometry = "geometry")) +
      
      # Used aes_string() instead of standard aes() because the fill was a
      # dynamic input instead of a static value
      # Also put geometry into quotation marks because of aes_string()
      
      geom_sf() +
      scale_fill_gradient(high = high_color, low = "white") +
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
    if(input$compare == "no_hs"){
      NAME = "No Secondary Education"
    }
    if(input$compare == "higheredu"){
      NAME = "Bachelor's and Beyond"
    }
    
    # Again used if(){} format, this time to change the title labels for each of
    # the gradient legends
    # Also (again) created new object, 'NAME', to be inputted later
    
    maps_data %>%
      filter(STATE == input$state3) %>%
      ggplot(aes_string(fill = input$compare, geometry = "geometry")) +
      geom_sf() +
      scale_fill_gradient(high = "#242440", low = "white",
                          name = NAME) +
      theme_map() +
      theme(legend.position = "right")

  })
  
  output$statmodel <- render_gt({
    model_1_table
    
    # Stored all data for the model_1 table as an RDS file to make app run more
    # quickly and efficiently
    
  })
    
}

shinyApp(ui = ui, server = server)
