pacman::p_load(ggplot2, dplyr, purrr, shiny.semantic, semantic.dashboard, shiny, htmltools, highcharter)
source("mods/search_games_mod.R")
# library(dplyr)
# library(purrr)
# library(shiny.semantic)
# library(shiny)
# library(htmltools)

ui <- dashboard_page(
  dashboard_header(),
  dashboard_sidebar(
    inverted = F, 
    size = "thin",
    sidebarMenu(
      menuItem(
        tabName = "search", 
        text = "Tournaments"
        #icon = img(class="ui right floated logo icon image", src="images/bitcoin_logo.png", style = "width: 10%")
      )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "search", 
        search_games_UI("search")
      )
    )
  ), theme = "cyborg"
)

server <- function(input, output, session) {

  observe({
    callModule(search_games, "search")
  })
  
}

shiny::shinyApp(ui, server)