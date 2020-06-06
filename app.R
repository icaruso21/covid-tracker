#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readr)
library(tidyverse)
library(Hmisc)
library(forecast)
library(shinyWidgets)
#library(scales)

covidDataCountiesNYT <- read_csv(url("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")) 
covidDataStatesNYT <- read_csv(url("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")) 

# ggplot(covidDataNYT, aes(date, cases)) +
#     geom_point() +
#     stat_summary(fun.data= mean_cl_normal) + 
#     geom_smooth() + 
#     geom_forecast(showgap = FALSE) +
#     scale_x_date(date_minor_breaks = "1 day", date_breaks = "14 day", date_labels = "%m/%d")
# scale_y_continuous(limits = c(0,100), breaks=seq(0,100,10)) + 
# scale_x_date(date_breaks = "months" , date_labels = "%b-%y")

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Covid Tracker"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
        #     selectInput("selected_county",
        #                 "Choose a county:",
        #                 unique(covidDataCountiesNYT$county),
        #                 selectize = FALSE)
        # ),
        switchInput(
            inputId = "switch",
            label = "County or state?",
            value = TRUE,
            onLabel = "County",
            offLabel = "State"
        ),
        multiInput(
            inputId = "selected", 
            label = "Search for a county to view :", 
            choices = unique(covidDataCountiesNYT$county),
            selected = covidDataCountiesNYT$county[1]
        )),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("covidCountyPlot"),
            position = "left"
            # tabPanel("State", plotOutput("covidStatePlot")))
        ))
    )


# Define server logic required to draw a histogram
server <- function(input, output, session) {
    observeEvent(input$switch, {
        if(input$switch){
            updateMultiInput(
                session = session,
                inputId = "selected",
                label = "Search for a county to view :", 
                choices = unique(covidDataCountiesNYT$county),
                selected = covidDataCountiesNYT$county[1]
            )}
        else{updateMultiInput(
                    session = session,
                    inputId = "selected",
                    label = "Search for a state to view :", 
                    choices = unique(covidDataStatesNYT$state),
                    selected = covidDataStatesNYT$state[1]
                )}
    })
    
    covidCounty <- reactive({
        isCounty <- input$switch
        if(isCounty){
            x <- covidDataCountiesNYT %>% 
            filter(county %in% input$selected[1])}
        else{x <- covidDataStatesNYT %>% 
            filter(state %in% input$selected[1])}
    })
    # covidState <- reactive({
    #     x <- covidDataStatesNYT %>% 
    #         filter(state %in% input$selected_state) 
    # })
    
    output$covidCountyPlot <- renderPlot({
        ggplot(covidCounty(), aes(date, cases)) +
            geom_line(aes(y = deaths), color = "darkred") + 
            labs(title = str_c(input$selected, " covid cases over time")) +
            xlab("Date (2020)") + 
            ylab("Cases") +
            geom_point() +
            stat_summary(fun.data= mean_cl_normal) + 
            geom_smooth() + 
            geom_forecast(showgap = FALSE) +
            theme_minimal() +
            scale_x_date(date_minor_breaks = "1 day", date_breaks = "14 day", date_labels = "%m/%d") +
            scale_y_continuous(limits = c(0, 1.5 * max(covidCounty()$cases)))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
