



library(shiny)
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
      tags$link(rel = 'stylesheet', href = 'style.css')  # my css
    ),  # head
    tags$nav(style = 'padding:0px; margin:0%',
      tags$a(href = '#', 'data-target' = 'slide-out', 
             class = 'sidenav-trigger hide-on-large-only',
        tags$i(class = 'material-icons', 'menu')
      ),  # navbar collapse
      tags$a(href = '#', class = 'brand-logo center hide-on-small-only',
        'Google Analytics - R'
      ),  # non-mobile brand logo
      tags$a(href = '#', class = 'h2 center hide-on-med-and-up', 
        'Google Analytics - R'
      ),  # mobile brand logo
      tags$ul(class = 'right', 
        tags$li(
          tags$a(href = 'http://www.github.com/seanastrup/r-google-analytics',
                 target = '_blank',
            tags$img(src = 'mark-github.svg', height = 35, widht = 35, 
                     id = 'github-icon')  # github mark
          )  # link
        )  # github link
      )  # ul
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
            tags$a(class = 'collapsible-header white-text waves-effect waves-blue', 
              tags$i(class = 'material-icons white-text ', 'fingerprint'), 
              tags$div(class = 'nav-words', 'Choose View')
            ),  # a
            tags$div(class = 'collapsible-body z-depth-3', 
              tags$ul(
                selectizeInput(inputId = 'Accounts', label = 'Accounts', 
                               choices = NULL), 
                selectizeInput(inputId = 'Properties', label = 'Properties', 
                               choices = NULL),
                selectizeInput(inputId = 'Views', label = 'Views', 
                               choices = NULL)
              )  # ul       
            )  # collapsible body
          )  # li
        )  # collapsible accordion
      )  # blue li
    ),  # side nav
    tags$div(class = 'main',
      tags$p(style = 'display: inline; vertical-align: top;', textOutput('GaAccount'), 
             tags$i(class = 'material-icons', 'chevron_right'), textOutput('GaProperty'), 
             tags$i(class = 'material-icons', 'chevron_right'), textOutput('GaView')),
      # tags$div(id = 'selected-information', style = 'display: inline',
        # tags$p(style = 'display: inline;', textOutput('GaAccount'), 
        #        textOutput('GaProperty'), 
        #        textOutput('GaView')),
        # tags$p(style = 'display: inline;', textOutput('GaProperty')), 
        # tags$p(style = 'display: inline;', textOutput('GaView'))
      # ),  # selected information
      shiny::textOutput('TestAccount'),
      shiny::textOutput('TestProperty'),
      shiny::textOutput('TestView'),
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
        )  # column
      )   # row
    )  # div
  )  # ui



server <- function(session, input, output) {

  token <- callModule(googleAuth, 'login')
  
  
  ###################################################
  ########## VIEW ID SELECTION CONTROLS #############
  ###################################################
  ga_accounts <- reactive({
    
    req(token())
    
    with_shiny(ga_account_list, shiny_access_token = token())
    
  })
  
  # selected_id <- callModule(authDropdown, 'auth_menu', ga.table = ga_accounts)
  
  AccountTable <- reactive({
    
    ga_accounts()
    
  })
  
  pList <- reactive({
    
    ga.table <- ga_accounts()
    
    ga.table[,c('accountName','webPropertyId','websiteUrl','viewName', 'viewId')]
    
  })
  
  ##############################
  ### UPDATE SELECTION VALUES ##
  ##############################
  observe({
    
    validate(
      need(pList(), "Need profiles")
    )
    
    pList  <- pList()
    
    AccountChoices <- unique(pList$accountName)
    
    updateSelectizeInput(session, 
                      "Accounts",
                      label = "Accounts",
                      choices = AccountChoices, 
                      selected = AccountChoices[1],
                      server = TRUE)
    
  })
  
  observe({
      
      validate(
        need(input$Accounts, "Need accounts")
      )
      pList  <- pList()
      
      pList <- pList[input$Accounts == pList$accountName,]
      
      PropertyChoices <- pList$websiteUrl
      
      updateSelectizeInput(session, 
                        "Properties", label="Properties",
                        choices = PropertyChoices, 
                        selected = PropertyChoices[1],
                        server = TRUE)
    
    })
  
  observe({
    
    validate(
      need(input$Properties, "Need web")
    )
    
    pList <- pList()
    
    pList <- pList[input$Properties == pList$websiteUrl,]
    
    ViewChoices <- pList$viewId 
    
    names(ViewChoices) <- paste(pList$viewName, pList$viewId)
    
    updateSelectizeInput(session, 
                         "Views", label = "Views",
                         choices = ViewChoices, 
                         selected = ViewChoices[1],
                         server = TRUE)
  })
  
  GaAccount <- reactive({
    
    validate(
      need(pList(), "Need profiles")
    )
    
    pList  <- pList()
    
    out <- unique(pList$accountName)
    
    return(out[1])
    
  })
  GaProperty <- reactive({
    
    validate(
      need(input$Properties, "Please login")
    )
    
    pList <- pList()
    out <- pList[input$Accounts == pList$accountName,]
    
    return(out$websiteUrl)
    
  })
  GaView <- reactive({
    
    validate(
      need(input$Views, "Please login")
    )
    pList <- pList()
    
    out <- pList[input$Views == pList$viewId,]
    
    return(out$viewId)
    
  })
    
  choice <- 'Please login'
  ###################################################
  ######## DEFINE CALLS TO GOOGLE ANALYTICS #########
  ###################################################
  get_device_sessions <- reactive({
    
    req(GaView())
    gaid <- GaView()
    with_shiny(google_analytics,
               viewId = gaid,
               date_range = c(lubridate::today() - 90, lubridate::today()),
               metrics = c('sessions'),
               dimensions = c('date', 'deviceCategory'),
               shiny_access_token = token())
    
  })
  
  get_sessions_by_hour <- reactive({
    
    req(GaView())
    gaid <- GaView()
    
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
      hc_yAxis(
        title = list( 
          text = ''
        )
      ) %>% 
      hc_title(text = 'Device Sessions By Date') 
      
    
  })
  output$SessionsByHour <- renderHighchart({
    
    req(get_sessions_by_hour())
    
    gadata <- get_sessions_by_hour()
    
    gadata$dateHour <- 
      lubridate::hour(lubridate::force_tz(lubridate::ymd_h(gadata$dateHour), 
                                          tzone = "America/Los_Angeles"))  
    
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
  
  output$TestAccount <- renderText({GaAccount()})
  output$TestProperty <- renderText({GaProperty()})
  output$TestView <- renderText({GaView()})
}



shinyApp(ui = ui, server = server, options = list(port = '5761'))