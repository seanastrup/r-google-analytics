



library(shiny)
library(shinydashboard)
library(googleAnalyticsR)
library(googleAuthR)
library(highcharter)
source('ga-auth.R')


ui <- 
  dashboardPage(
    dashboardHeader(
      title = 'Google Analytics R'
    ),  # dashboard header
    dashboardSidebar(
      width = 350,
      sidebarMenu(
        menuItem(
          googleAuthUI("login")
        )  # menu item
      ),  # sidebar menu
      authDropdownUI("auth_menu")
    ),  #dashboard sidebar
    dashboardBody(
      highchartOutput("something")
    )
  )  # dashboard page



server <- function(input, output) {

  token <- callModule(googleAuth, "login")
  
  ga_accounts <- reactive({
    req(token())
    
    with_shiny(ga_account_list, shiny_access_token = token())
    
  })
  
  selected_id <- callModule(authDropdown, "auth_menu", ga.table = ga_accounts)
  
  gadata <- reactive({
    
    req(selected_id())
    gaid <- selected_id()
    with_shiny(google_analytics,
               viewId = gaid,
               date_range = c("2018-06-01", "2018-07-02"),
               metrics = c("sessions"),
               dimensions = c("date"),
               shiny_access_token = token())
  })
  
  output$something <- renderHighchart({
    
    ## only trigger once authenticated
    req(gadata())
    
    gadata <- gadata()
    
    ## creates a line chart using highcharts
    hchart(gadata, "line" , hcaes(x = date, y = sessions))
    
  })
}



shinyApp(ui = ui, server = server)