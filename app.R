library(shiny)
library(dplyr)
library(DT)
crunchyroll <- read.csv("crunchyroll.csv", stringsAsFactors = FALSE)
ui <- fluidPage(
  
  # Feature 4: making the app more visually appealing by giving it a theme
  # this feature was previously included in assignment B3, but will now be counted towards assignment B4 as it was not marked as a feature in assignment B3
  theme = bslib::bs_theme(bootswatch = "quartz"), 

  # Feature 1: putting the crunchyroll logo in the corner to show what website the anime can be found/watched at
  titlePanel(
    tags$div(
      tags$img(src = "Crunchyroll_Logo.png", height = 110, width = 100),
      "Anime I Have Seen :)"
    )
  ),
 
  sidebarLayout(
    sidebarPanel("Genre(s) of interest", 
                 
                 # Feature 5: adding a drop down menu to choose how to order the list of returned anime, either by rating or number of episodes
                 selectInput("order_by", "Order By", choices = c("None", "rate", "episodes")),  
                 
                 # Feature 2: filtering the csv by genre so that you can find anime that appeals to your taste
                 # multiple genres can be included using the check box - for a more specific search
                 checkboxGroupInput("genres", "Select Genres", choices = colnames(crunchyroll)[5:ncol(crunchyroll)]) 
                 ),
    mainPanel("Animes", 
              dataTableOutput("selected_animes"),
              textOutput("result_count")
              )
  )
)

# Feature 6: rendering the data table so that it is interactive
# user can switch pages and use search bar to go through the selected animes
server <- function(input, output) {
  output$selected_animes <- renderDataTable({
    req(input$genres)
    selected_data <- crunchyroll %>%
      filter(rowSums(select(., input$genres)) == length(input$genres))
    
    if (input$order_by == "rate") {
      selected_data <- selected_data %>% arrange(desc(rate))
    } else if (input$order_by == "episodes") {
      selected_data <- selected_data %>% arrange(episodes)
    }
    datatable(selected_data, options = list(pageLength = 10))
  })

  # Feature 3: showing the number of results found from your search
  output$result_count <- renderText({
    req(input$genres)
    count <- nrow(crunchyroll %>%
           filter(rowSums(select(., input$genres)) == length(input$genres)))
    paste("We have found", count, "animes that may interest you!")
  })
}
shinyApp(ui = ui, server = server)
