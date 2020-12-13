## ui.R
library(shiny)
library(shinydashboard)
library(recommenderlab)
library(data.table)
library(ShinyRatingInput)
library(shinyjs)


source('functions/helpers.R')

genre_list = c("Action", "Adventure", "Animation", 
               "Children's", "Comedy", "Crime",
               "Documentary", "Drama", "Fantasy",
               "Film-Noir", "Horror", "Musical", 
               "Mystery", "Romance", "Sci-Fi", 
               "Thriller", "War", "Western")


sidebar <- dashboardSidebar(
  sidebarMenu(
     menuItem("Popular Movies", tabName = "system1_pop"),
    menuItem("Highly Rated Movies", tabName = "system2_hr"),
    menuItem("Collaborative Recommendation", tabName = "system2")
  )
)



shinyUI(
  dashboardPage(
    skin = "red",
    dashboardHeader(title = "Movie Recommender By User Input"),
    
    sidebar,
    
    dashboardBody(
      
      tabItems(
        
      tabItem(tabName = "system2",
             
      includeCSS("css/movies.css"),
                  fluidRow(
                    box(width = 12, title = "Step 1: Rate as many movies as possible", status = "info", solidHeader = TRUE, collapsible = TRUE,
                        div(class = "rateitems",
                            uiOutput('ratings')
                        )
                    )
                  ),
                  fluidRow(
                    useShinyjs(),
                    box(
                      width = 12, status = "info", solidHeader = TRUE,
                      title = "Step 2: Discover movies you might like",
                      br(),
                      withBusyIndicatorUI(
                        actionButton("btn", "Click here to get your recommendations below", class = "btn-warning")
                      ),
                      br(),
                      tableOutput("results")
                    )
                  )
    ),
    
    tabItem(tabName = "system1_pop",
            titlePanel("Genre Recommendation- Most Popular Movies"),
            sidebarPanel(
              selectInput("genre", "Genre:",
                          choices=genre_list),
              hr(),
              helpText("Choose a Genre")
            ),

            # Output
            mainPanel(
              tableOutput("result_sys1_popular")
            )

    ),
    tabItem(tabName = "system2_hr",
            titlePanel("Genre Recommendation- Highly Rated Movies"),
            sidebarPanel(
              selectInput("genre_2", "Genre:", 
                          choices=genre_list),
              hr(),
              helpText("Choose a Genre")
            ),
            
            # Output
            mainPanel(
              tableOutput("result_sys1_highlyrated") 
            )
            
    )
    
    
    )
    )
  )
)