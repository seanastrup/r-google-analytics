



library(shiny)
library(shinydashboard)
library(googleAnalyticsR)
library(googleAuthR)
library(highcharter)
library(DT)
library(tidyverse)
source('ga-auth.R')






ui <- 
  shiny::fluidPage(
    tags$head(
      #tags$script(src='https://code.jquery.com/jquery-2.2.4.min.js'),  # jquery
      tags$link(rel = 'stylesheet', href = 'https://fonts.googleapis.com/icon?family=Material+Icons'), # google fonts
      tags$script(src = 'https://cdnjs.cloudflare.com/ajax/libs/materialize/1.0.0-rc.2/js/materialize.min.js'),  # material js
      tags$script(src = 'my.js'),  # my js
      tags$link(rel = 'stylesheet', href = 'https://cdnjs.cloudflare.com/ajax/libs/materialize/1.0.0-rc.2/css/materialize.min.css'),  # material css
      tags$link(rel = 'stylesheet', href = '/style.css')  # my css
    ),  # head
    tags$nav(style = 'padding:0px; margin:0%',
      tags$a(href = '#', 'data-target' = 'slide-out', class = 'sidenav-trigger hide-on-large-only',
        tags$i(class = 'material-icons', 'menu')
      ),  # navbar collapse
      tags$a(href = '#', class = 'brand-logo',
        'Google Analytics - R'
      )
    ),   # navbar
    tags$br(),
    tags$ul(id = 'slide-out', class = 'sidenav sidenav-fixed how-on-large-only', 
      tags$li(
        tags$div(class = 'user-view', 
          tags$div(class = 'background'
            # insert packground
          ), 
          googleAuthUI('login'), 
          tags$div(class = 'divider')
        )
      ),  # background li 
      tags$li(class = 'blue', 
        tags$ul(class = 'collapsible collapsible-accordion', 
          tags$li(
            tags$a(class = 'collapsible-header white-text waves-effect waves-blue ', 
              tags$i(class = 'material-icons white-text ', 'fingerprint'), 
              tags$div(class = 'nav-words', 'Login')
            ),  # a
            tags$div(class = 'collapsible-body z-depth-3', 
              tags$ul(
                tags$li(tags$a(class="waves-effect waves-blue", href = '#test1', 'First')),
                tags$li(tags$a(class="waves-effect waves-blue", href = '#test2', 'Second')),
                tags$li(tags$a(class="waves-effect waves-blue", href = '#test3', 'Third'))
              )  # ul       
            )  # collapsible body
          )  # li
        )  # collapsible accordion
      )  # blue li
    ),  # side nav
    tags$div(class = 'main',
      authDropdownUI('auth_menu',inColumns = TRUE),
      shiny::fluidRow(
        shiny::column(
          width = 6,
          tags$div(class = 'card z-debth-5', 
            tags$div(class = 'card-content',
              highcharter::highchartOutput('DailyDeviceSessions', height = 300)       
            )  # card content
          )  # card
        ),  # column
        shiny::column(
          width = 6, 
          tags$div(class = 'card z-debth-5', 
            tags$div(class = 'card-content',
              highcharter::highchartOutput('SessionsByHour', height = 300)       
            )  # card content
          )  # card
        )
      )  # row
    )  # div
  )  # ui



server <- function(input, output) {

  token <- callModule(googleAuth, 'login')
  
  ga_accounts <- reactive({
    
    req(token())
    
    with_shiny(ga_account_list, shiny_access_token = token())
    
  })
  
  selected_id <- callModule(authDropdown, 'auth_menu', ga.table = ga_accounts)
  
  ###################################################
  ######## DEFINE CALLS TO GOOGLE ANALYTICS #########
  ###################################################
  get_device_sessions <- reactive({
    
    req(selected_id())
    gaid <- selected_id()
    with_shiny(google_analytics,
               viewId = gaid,
               date_range = c(lubridate::today() - 90, lubridate::today()),
               metrics = c('sessions'),
               dimensions = c('date', 'deviceCategory'),
               shiny_access_token = token())
    
  })
  
  get_sessions_by_hour <- reactive({
    
    req(selected_id())
    gaid <- selected_id()
    
    with_shiny(google_analytics,
               viewId = gaid,
               date_range = c(lubridate::today() - 90, lubridate::today()),
               metrics = c('sessions', 'Users', 'newUsers'),
               dimensions = c('dateHour', 'deviceCategory', 'dayOfWeekName'),
               shiny_access_token = token())
  })
  
  
  ###################################################
  ############## CREATE OUTPUTS #####################
  ###################################################
  output$DailyDeviceSessions <- renderHighchart({
    
    # only trigger once authenticated
    req(get_device_sessions())
    
    gadata <- get_device_sessions()
    
    hchart(gadata, 'spline' , hcaes(x = date, y = sessions, 
                                                 group = deviceCategory)) %>%
      hc_xAxis(
        title = list( 
          text = ''
        )
      ) %>% 
      hc_title(text = 'Device Sessions') 
      
    
  })
  
  output$SessionsByHour <- renderHighchart({
    
    req(get_sessions_by_hour())
    
    gadata <- get_sessions_by_hour()
    
    gadata$dateHour <- 
      lubridate::hour(lubridate::force_tz(lubridate::ymd_h(gadata$dateHour), tzone = "America/Los_Angeles"))  
    
    gadata <- 
      gadata %>% 
        group_by(dateHour, dayOfWeekName) %>% 
        summarize(sessions = sum(sessions, na.rm = TRUE))
    
    hchart(gadata, type = 'heatmap', hcaes(x = dateHour, y = dayOfWeekName, 
                                           value = sessions)) %>% 
      hc_xAxis(
        title = list( 
          text = ''
        )
      ) %>% 
      hc_yAxis(
        title = list( 
          text = ''
        )
      )
  })
  
}



shinyApp(ui = ui, server = server)