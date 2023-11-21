library(shiny)
library(dplyr)
crunchyroll <- read.csv("crunchyroll.csv", stringsAsFactors = FALSE)
ui <- fluidPage(
  
# making the app more visually appealing by giving it a theme
  theme = bslib::bs_theme(bootswatch = "quartz"),
# Feature 1: putting the crunchyroll logo in the corner to show what website the anime can be found/watched at
  titlePanel(
    tags$div(
      tags$img(src = "Crunchyroll_Logo.png", height = 110, width = 100),
      "Anime I Have Seen :)"
    )
  ),
# Feature 2: filtering the csv by genre so that you can find anime that appeals to your taste
# multiple genres can be included using the check box - for a more specific search 
  sidebarLayout(
    sidebarPanel("Genre(s) of interest", 
                 checkboxGroupInput("genres", "Select Genres", choices = colnames(crunchyroll)[5:ncol(crunchyroll)])
                 ), 
    mainPanel("Animes", 
              tableOutput("selected_animes"),
              textOutput("result_count")
              )
  )
)
server <- function(input, output) {
  output$selected_animes <- renderTable({
    req(input$genres)
    selected_data <- crunchyroll %>%
      filter(rowSums(select(., input$genres)) == length(input$genres))
  })

# Feature 3: showing the number of results found from your search
  output$result_count <- renderText({
    req(input$genres)
    count <- nrow(crunchyroll %>%
           filter(rowSums(select(., input$genres)) == length(input$genres)))
    paste("We have found", count, "results for you")
  })
}
shinyApp(ui = ui, server = server)
