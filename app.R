



library(shiny)
library(shinydashboard)
library(googleAnalyticsR)
library(googleAuthR)
library(highcharter)
source('ga-auth.R')






ui <- 
  shiny::fluidPage(
    tags$head(
      # tags$script(src = 'jquery-3.3.1.min.js'),  # jquery
      tags$link(rel = 'stylesheet', href = 'https://fonts.googleapis.com/icon?family=Material+Icons'), # google fonts
      tags$link(rel = 'stylesheet', href = 'https://cdnjs.cloudflare.com/ajax/libs/materialize/1.0.0-rc.2/css/materialize.min.css'),  # material css
      tags$script(src = 'https://cdnjs.cloudflare.com/ajax/libs/materialize/1.0.0-rc.2/js/materialize.min.js'),  # material js
      tags$style(HTML('
                      body {
                      background-color:#fafafa;
                      }
                      .selectize-input{
                        height:50%;
                      }'
          )
      ),
      tags$script(HTML("
        $(document).ready(function(){
                       $('.sidenav').sidenav();
                       });"
        )
      )
    ),  # head
    tags$nav(class = 'blue darken-3',
      tags$div(class = 'nav-wrapper',
        tags$a(href = '#', class = 'brand-logo',
          'Google Analytics - R'
        )
      )  # nav wrapper
    ),   # navbar
    tags$br(),
    tags$ul(id = 'slide-out', class = 'sidenav', 
      tags$li(
        tags$div(class = 'user-view', 
          tags$div(class = 'background'
            # insert packground
          ), 
          tags$br()
        )
      ),  # background li  
      tags$div(class = 'card z-debth-5',
      tags$div(class = 'card-content',
               'TEST'
          # googleAuthUI('login'),
          # authDropdownUI('auth_menu')
        )  # card content
      )  # card
    ),  # side nav
    shiny::fluidRow(
      shiny::column(
        width = 2,
        tags$div(class = 'card-z-debth-5', 
          tags$div(class = 'card-content',
            googleAuthUI('login'),
            authDropdownUI('auth_menu')
          )  # card content
        )   # card
      ),  # column
      shiny::column(
        width = 6,
        tags$div(class = 'card z-debth-5', 
          tags$div(class = 'card-content',
            highcharter::highchartOutput('DailyDeviceSessions')       
          )  # card content
        )  # card
      )  # column
    )  # row
  )  # ui



server <- function(input, output) {

  token <- callModule(googleAuth, 'login')
  
  ga_accounts <- reactive({
    
    req(token())
    
    with_shiny(ga_account_list, shiny_access_token = token())
    
  })
  
  selected_id <- callModule(authDropdown, 'auth_menu', ga.table = ga_accounts)
  
  gadata <- reactive({
    
    req(selected_id())
    gaid <- selected_id()
    with_shiny(google_analytics,
               viewId = gaid,
               date_range = c(lubridate::today() - 60, lubridate::today()),
               metrics = c('sessions'),
               dimensions = c('date'),
               shiny_access_token = token())
    
  })
  
  output$DailyDeviceSessions <- renderHighchart({
    
    # only trigger once authenticated
    req(gadata())
    
    gadata <- gadata()
    
    highcharter::hchart(gadata, 'spline' , hcaes(x = date, y = sessions 
                                                 )) %>%
      hc_title(text = 'Device Sessions')
      
    
  })
}



shinyApp(ui = ui, server = server)