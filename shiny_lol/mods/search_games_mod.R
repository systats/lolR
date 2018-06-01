league_logos_web <- list(
  "all" = "/assets/img/structure/lol-compact-logo.png",
  "challenge-france" = "https://am-a.akamaihd.net/image/?f=https://lolstatic-a.akamaihd.net/esports-assets/production/league/challenge-france-5e8ap77a.png&resize=280:280",
  "eu-lcs" = "https://eu.lolesports.com/darkroom/280/280/181fa74d9cf6d25d0db4ee5c8433196d:a5c75bf238d4f1493840dd8849611ffe",
  "na-lcs" = "https://am-a.akamaihd.net/image/?f=https://lolstatic-a.akamaihd.net/esports-assets/production/league/na-lcs-g63ljv52.png&resize=280:280",
  "lck" = "https://eu.lolesports.com/darkroom/280/280/97671bfadc32d8949ff282ce1e799abc:726b28378e9f6cc4db6fc512d6fa3e7e", 
  "lpl-china" = "https://am-a.akamaihd.net/image/?f=https://lolstatic-a.akamaihd.net/esports-assets/production/league/lpl-china-6ygsd4c8.png&resize=280:280",
  "eu-cs" = "https://eu.lolesports.com/darkroom/280/280/a2c28b87c24928c0c6647457e79f2321:d6229a2c25568016f81228cc19db0e5d",
  "na-cs" = "https://eu.lolesports.com/darkroom/280/280/fc7befff8cac76dfc3d5132542cb79c4:82a76009fd7337d2fef2b96b79b28d04", 
  "worlds" = "https://eu.lolesports.com/darkroom/280/280/46f2586c04c13d14c586b32dbfff0da7:8e1b9d4ef940dedec61e81b12efc18fc",
  "msi" = "https://eu.lolesports.com/darkroom/280/280/eb93da7c4f5603fb3fb717c0e4e6d80c:a4097a2d4908b82ad10bd709673d345e",
  "all-star" = "https://eu.lolesports.com/darkroom/280/280/e580ff37697c569e55e7d2b322b01dd2:33c879451d6dcd218f49f89933d4829f",
  "rift-rivals" = "https://eu.lolesports.com/darkroom/280/280/03fdee3ae177326c3494dc4b580ec756:d27cd4db3f8698c33f3164310f80940f"
)

league_logos <- list(
  "all" = c("all.png", ""),
  "challenge-france" = c("challenge-france.png", ""),
  "eu-lcs" = c("eu-lcs.png", "The LCS is the preeminent League of Legends esports league in Europe. It is comprised of 10 teams who will face off against every other team."),
  "na-lcs" = c("na-lcs.png", "The North America League of Legends Championship Series (NA LCS) is the preeminent League of Legends esports league in North America."),
  "lck" = c("lck.jpeg", "League of Legends Champions Korea (LCK) is the ultimate League of Legends esports league in Korea. The LCK features ten teams facing every other team twice over the course of the split."), 
  "lpl-china" = c("lpl-china.png", "The Tencent League of Legends Pro League (LPL) is the premier League of Legends esports competition in China. The LPL features 12 teams split into two divisions of 6 teams each."),
  "eu-cs" = c("eu-cs.png", ""),
  "na-cs" = c("na-cs.png", ""), 
  "worlds" = c("worlds.png", "Autumn marks the climax of every League of Legends Season: Worlds. In addition to international pride and glory, teams are competing for the chance to be the 2017 World Champion."),
  "msi" = c("msi.png", "Who will own the Rift when fourteen regional champions clash at our international showdown in Berlin and Paris? Welcome to the 2018 League of Legends Mid-Season Invitational (MSI)."),
  "all-star" = c("all-star.png", "Every year the All-Star Event serves as a celebration of competition at the highest levels on the Rift."),
  "rift-rivals" = c("rift-rivals.png", "Rift Rivals will be held across the world during July 3-9. The event will pit regional rivals against each other in grudge matches for glory and bragging rights.")
)

search_games_UI <- function(id){
  ns <- NS(id)
  tagList(
    #div(class = "ui grid",
        div(class = "four wide column", 
            div(class = "ui basic padded segment", 
                uiOutput(ns("leagues"))  
            )    
        ),
        div(class = "eight wide column", 
            uiOutput(ns("matches"))
        ),
        div(class = "four wide column", 
            div(class = "ui basic padded segment", 
                uiOutput(ns("league"))
            )
        )
  #  )
  )
}


search_games <- function(input, output, session){
  
  output$leagues <- renderUI({
    
    ns <- session$ns
    
    tagList(
      div(class="ui animated selection list",
          league_logos %>% 
            names %>% 
            as.list() %>% 
            purrr::map2(.x = ., .y = league_logos, ~{
              shiny::tags$div(class="item",
                              div(class="ui mini image",
                                  img(src= paste0("league_logo/", .y))
                              ),
                              div(class="middle aligned content",
                                  h3(class="header", actionLink(ns(.x[1]), stringr::str_to_upper(.x)))
                              )
              )
            })
      )
    )
  })
  
  
  league_links <- reactive({
    
    league_logos %>% 
      names %>% 
      #str_replace_
      as.list %>% 
      purrr::map(~{
        input[[.x[1]]]
      })
    
  })
  
  # output$dev <- renderPrint({
  #   league_links() %>% unlist()
  # })
  
  p <- reactiveValues(
    before = as.list(rep(0, 12)) # TODO....
  )
  
  # observeEvent(input$nprofiles, {
  #   p$before <- rep(0, length(league_logos)) %>% 
  #     as.list
  # })
  
  signal <- eventReactive(league_links(), {
    
    if(length(league_links()) != length(p$before)) return()
    
    signal <- p$before %>%
      purrr::map2(., league_links(), ~{
        .y > .x
      })
    
    p$before <- league_links()
    
    out <- league_logos %>%
      names() %>% 
      .[unlist(signal)]
    
    return(out)
  })
  
  matches <- reactive({
    
    if(is.null(signal()) | length(signal()) == 0) return()
    
    
    league <- get(load("data/match_esports.Rdata")) %>% 
      filter(event == signal()) %>% 
      mutate(year = as.numeric(year))
    
    return(league)
  })
  
  output$matches <- renderUI({
    
    if(is.null(matches()) | length(matches()) == 0) return()
    
    if(is.null(input$years)){
      year <- max(as.numeric(matches()$year))
    } else {
      year <- input$years
    }
    
    # final <- matches() %>% 
    #   filter(year == year) %>% 
    #   slice(1:10)
    
    final <- matches()[matches()$year == year, ] 
      #slice(1:10)
    
    
    tagList(
      div(class="ui statistics",
        div(class="statistic",
          div(class="value",
            nrow(final)
          ),
          div(class="label",
            "Games played"
          )
        ),
        div(class="statistic",
            div(class="value",
                sum(final$rounds)
            ),
            div(class="label",
                "Rounds played"
            )
        )
      ),
      final %>% 
        split(.$date) %>% 
        map(~{
          div(class = "ui segment",
              div(class="ui top left attached label", .x$date[1]),
              div(class="ui divided items",
                .x %>% 
                  split(.$game_link) %>% 
                  map(~{
                    div(class="item",
                      div(class="left aligned content",
                          p(.x$time)
                      ),
                      div(class="center aligned content",
                          div(class = "ui euqal width center aligned grid",
                              div(class= "row",
                                  # div(class = "column",
                                  #   img(class = "ui tiny image", src = .x$blue_team_logo)
                                  # ),
                                  # div(class = "column",
                                  #     ""
                                  # ),
                                  div(class = "column",
                                      h3(.x$blue_team)
                                  ),
                                  div(class = "column",
                                      "  "
                                  ),
                                  div(class = "column",
                                      h3("VS") 
                                  ),
                                  div(class = "column",
                                      "  "
                                  ),
                                  div(class = "column",
                                      h3(.x$red_team)
                                  )
                                  # div(class = "column",
                                  #     ""
                                  # ),
                                  # div(class = "column",
                                  #     img(class = "ui tiny image", src = .x$red_team_logo)
                                  # )
                              )
                          )
                      ),
                      div(class="right aligned content",
                        div(class="ui right floated button",
                            a("Watch Game", href=paste0("https://eu.lolesports.com", .x$game_link), target="_blank")  
                        )
                      )
                    )
                  })
              )
          )
        })
    )
  })
  
  
  # output$highmap <- renderHighchart({
  #   
  #   # wmap <- download_map_data("custom/world-palestine-lowres")
  #   # #save(wmap, file = "data/wmap.Rdata")
  #   # load("data/wmap.Rdata")
  # 
  #   #hc_title(text = paste0("Global Distribution of ", input$var, " (",input$yr, ")")) %>%
  #   
  #   
  #   
  # })
  # 
  # getContent <- function(url) {
  #   library(httr)
  #   content(GET(url))
  # }
  # 
  # world <- getContent("https://raw.githubusercontent.com/johan/world.geo.json/master/countries.geo.json")
  # # is text
  # world <- jsonlite::fromJSON(world, simplifyVector = FALSE)
  #save(world, file = "data/world.Rdata")
  #load("data/world.Rdata")
  # world <- jsonlite::fromJSON("data/world-continents.geo.json", simplifyVector = FALSE)
  # highcharter::highchartOutput(ns("highmap"))
  # highchart(type = "map") %>%
  #   #hc_chart(backgroundColor = "#161C20") %>% 
  #   hc_add_series(mapData = world, showInLegend = FALSE, nullColor = "#424242",
  #                 borderWidth = 0) %>%
  #   hc_add_theme(hc_theme_null()) %>%
  #   hc_mapNavigation(enabled = F)
  
  output$league <- renderUI({
    
    if(is.null(signal()) | length(signal()) == 0) return("")
    
    ns <- session$ns
    
    years <- matches() %>% 
      filter(event == signal()) %>% 
      .$year %>% 
      unique
    
    tagList(
      img(class = "ui centered small image", src = paste0("league_logo/", league_logos[[signal()]][1])),
      h2(class = "ui center aligned header", signal()),
      dropdown(ns("years"), choices = years, choices_value = years, value = max(years)),
      br(),
      img(class = "ui big image", src = "world.png"),
      br(),
      league_logos[[signal()]][2]
    )
  })
}