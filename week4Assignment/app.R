

library(shiny)
library(tidyverse)
library(DT)
library(plotly)

covidData <- read_csv("time-series-19-covid-combined.csv") %>% 
    group_by(`Country/Region`, Date) %>% 
    summarize(Confirmed = sum(Confirmed, na.rm = TRUE),
              Deaths = sum(Deaths, na.rm = TRUE)) %>% 
    mutate(NewCases = Confirmed - lag(Confirmed),
           NewCases = if_else(is.na(NewCases), Confirmed, NewCases),
           NewDeaths = Deaths - lag(Deaths),
           NewDeaths = if_else(is.na(NewDeaths), Deaths, NewDeaths)) %>% 
    ungroup()

covidGlobalData <- covidData %>% 
    group_by(Date) %>% 
    summarise(totalDailyCases = sum(NewCases),
              totalDailyDeaths = sum(Deaths)) %>% 
    mutate(totalWorldCases = cumsum(totalDailyCases),
           totalWorldDeaths = cumsum(totalDailyDeaths)) %>%
    select(Date, totalDailyCases, totalWorldCases)

# Define UI for application that draws a histogram
ui <- navbarPage("MSDS650: Week 4 Assignment",
                 tabPanel("Covid Data",
                     mainPanel(
                         h2("Covid Dataset Downloaded"),
                         p("Covid dataset was downloaded from John Hopkins Covid Github on 30Mar at 18:41EST.  This data set contains time series data for a number of countries that detail the number of confirmed COVID cases, deaths, and number of recovered"),
                         br(),
                         p("We will be using two visualizations to view this data.  The first is a time series line plot to view total number of cases in the world.  I will use an identical plot to just view the cases a few countries.  The second plot will be a column plot to take a look at the number of new cases by day in the world.  The complete data set can be downloaded by using the table at the bottom of the page."),
                         br(),
                         br(),
                         plotlyOutput("plot1"),
                         br(),
                         plotlyOutput('plot2'),
                         br(),
                         plotlyOutput('plot3'),
                         br(),
                         h3("Covid Data by Country"),
                         DT::dataTableOutput('table1'),
                         br(),
                         h3("Covid World Data"),
                         DT::dataTableOutput('table2')
                     )
                 ),
                 tabPanel("Baseball Data",
                     mainPanel(
                         
                     )
                 )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
        output$plot1 <- renderPlotly(
            ggplotly(
            ggplot(covidGlobalData, aes(x = Date, y = totalWorldCases)) +
                geom_point() +
                geom_line() +
                theme_classic() +
                labs(
                    title = "Total Cases of COVID in the World by Date"
                )
            )
        )
        
        output$plot2 <- renderPlotly(
            ggplotly(
                ggplot(filter(covidData, `Country/Region` %in% c("China", "US", "Italy")), aes(x = Date, y = Confirmed, color =`Country/Region`)) +
                    geom_point() +
                    geom_line() +
                    theme_classic() +
                    labs(
                        title = "Total Cases of Covid in the US by Date"
                    )
            )
            
        )
        
        output$plot3 <- renderPlotly(
            ggplotly(
                ggplot(covidGlobalData, aes(x = Date, y = totalDailyCases)) +
                    geom_col() +
                    theme_classic() +
                    labs(
                        title = "Total New Cases in the World by Date"
                    )
            )
        )
        
        output$table1 <- renderDT({
            datatable(covidData,
                      extensions = 'Buttons', options = list(
                          dom = 'Bfrtip',
                          buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                      )
            )
        }
        )
        
        output$table2 <- renderDT({
            datatable(covidGlobalData,
                      extensions = 'Buttons', options = list(
                          dom = 'Bfrtip',
                          buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                      )
            )        })


}

# Run the application 
shinyApp(ui = ui, server = server)
