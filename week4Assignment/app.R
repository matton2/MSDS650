

library(shiny)
library(tidyverse)
library(DT)
library(plotly)
library(ggrepel)

officeData <- read_csv("office_ratings.csv") 

officeDataMutate <- officeData %>% 
    mutate(season = as.factor(season),
           episode = as.factor(episode),
           season_episode = paste0(season, "_", episode))

# Define UI for application that draws a histogram
ui <- navbarPage("MSDS650: Week 4 Assignment",
                 tabPanel("Office Data",
                     mainPanel(
                         h2("The Office Ratings Data"),
                         p("Tidy Tuesday is a popular weekly project from the R4DS community.  Each week they publish an open source dataset which links back to an article or figure.  On the 17Mar, they published a dataset based on The Office tv show "),
                         br(),
                         p("This published data set contains 6 variables, season, episode, title, imdb_rating, total_votes, and air_date, with a 188 observations.  The dataset will available for download at the bottom of this page.  I will be creating a few new variables as well, the expended dataset will also be available for download at the bottom of the page.  I will create a few different visualizations to take a look at ratings, total votes, and air date of the years."),
                         br(),
                         br(),
                         h3("Visualization 1"),
                         p("This first realization will just take a lot at the distubtion via a column chart of ratings based upon season.  We needed to create a new variable here where we joined the season and episode columns"),
                         plotlyOutput("plot1"),
                         br(),
                         h3("Visualization 2"),
                         p("So visualization 1 probably isn't great.  That's create a similar distrubtion plot using a density curve and see if that looks any better."),
                         plotlyOutput('plot2'),
                         br(),
                         h3("Visualization 3"),
                         p("That was better.  Another way to visualize the spread of data based upon season would be to use a boxplot.  I have added some points that allow you to hover over and see which episode recieved the given rating"),
                         plotlyOutput('plot3'),
                         br(),
                         h4("Visualization 4 and 5"),
                         p("The final 2 visualizations will be scatter plots with a smooth line(s) placed on top of the data.  The first plot (visualization 4) will investigation to see if a higher rating correlates with more total votes.  This may answer the question if the episode is better, are more people likely to vote.  I have added both a linear model and a loess model both based on the formula y ~x.  The seond plot (visualization 5) will attempt to determine if the ratings for a season got better with later episodes.  This may answer the question if the first couple of episodes were set ups for a bigger pay off later in the series."),
                         plotlyOutput('plot4'),
                         br(),
                         plotlyOutput('plot5'),
                         br(),
                         h3("Raw Office Data"),
                         DT::dataTableOutput('table1'),
                         br(),
                         h3("Mutated Office Data"),
                         DT::dataTableOutput('table2')
                     )
                 ),
                 tabPanel("Perhaps Some Other Assignment",
                     mainPanel(
                         h2("Place holder for a second assignment in the future?")
                     )
                 )
    
)


server <- function(input, output) {
    
        output$plot1 <- renderPlotly({
            ggplotly(
                ggplot(officeDataMutate, aes(x = season_episode, y = imdb_rating, fill = season)) +
                    geom_col() +
                    theme_classic() +
                    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                    labs(
                        title = "Column Plot of IMDB Ratings Based upon Episode and Season"
                    )
                
                
            )
        })
        
        output$plot2 <- renderPlotly(
            ggplotly(
                ggplot(officeDataMutate, aes(x = imdb_rating, fill = season)) +
                    geom_density(alpha = 0.4) +
                    theme_classic() +
                    labs(
                        title = "Density Plot based upon IMDB Ratings and Season"
                    )
            )
            
        )
        
        output$plot3 <- renderPlotly({
            ggplotly(
                ggplot(officeDataMutate, aes(x = season, y = imdb_rating, fill = season, 
                                             text = paste("Episode:", episode))) +
                    geom_jitter() +
                    geom_boxplot(alpha = 0.3) +
                    theme_classic() +
                    theme(legend.position = 'none') +
                    labs(
                        title = "Box plot of IMDB Ratings per Season"
                    )
            )
        })
        
        output$plot4 <- renderPlotly({
            ggplotly(
                ggplot(officeDataMutate, aes(x = total_votes, y = imdb_rating)) +
                    geom_point(aes(text = paste0("Season: ", season,"\n",
                                                "Episode: ", episode))) +
                    geom_smooth(method = 'lm', se = FALSE) +
                    geom_smooth(se = FALSE, color = 'red') +
                    theme_classic() +
                    labs(
                        title = "Scatter Plot and Smoothers based on Total Votes and Rating"
                    )
            )
        })
        
        output$plot5 <- renderPlotly({
            ggplotly(
                ggplot(officeDataMutate, aes(x = as.numeric(episode), y = imdb_rating, color = season)) +
                    geom_jitter() +
                    geom_smooth(se = FALSE) +
                    theme_classic() +
                    labs(
                        title = "Scatter Plot and Smoothers based on Episode Number and Rating",
                        x = "Episode (in a given season)"
                    )
            )
        })
        
        output$table1 <- renderDT({
            datatable(officeData,
                      extensions = 'Buttons', options = list(
                          dom = 'Bfrtip',
                          buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                      )
            )
        }
        )
        
        output$table2 <- renderDT({
            datatable(officeDataMutate,
                      extensions = 'Buttons', options = list(
                          dom = 'Bfrtip',
                          buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                      )
            )        })


}

# Run the application 
shinyApp(ui = ui, server = server)
