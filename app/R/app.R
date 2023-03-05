library(shiny)
library(magrittr)
library(googleAuthR)

# options(shiny.port = 1221)

options("spinner.color" = itvPalette::itv_palette()$blue)
auth <- jsonlite::read_json("auth.json")
Sys.setenv(BARB_API_USERNAME=auth$username)
Sys.setenv(BARB_API_PASSWORD=auth$password)

barbBrowser <- function(...) {
  # MRE for testing
  # library(shiny)
  # ui <- fluidPage(
  #   "Hello, world!"
  # )
  # server <- function(input, output, session) {
  # }
  # shinyApp(ui, server)
  
  # Define UI for application that draws a histogram
  ui <- bs4Dash::dashboardPage(
    header = bs4Dash::dashboardHeader(
      title = bs4Dash::dashboardBrand(
        title = "BARB Spot Browser",
        color = "white",
        href = "https://www.itvmedia.co.uk/itv-adlabs/product",
        image = "https://storage.googleapis.com/itv-logos/adlabs.jpg"
      )
    ),
    sidebar = bs4Dash::dashboardSidebar(disable = TRUE),
    controlbar = bs4Dash::dashboardControlbar(
      collapsed = FALSE,
      column(
        8,
        shiny::uiOutput("advertiser_select"),
        shiny::dateRangeInput(
          "uiDateRange",
          "Select Date Range",
          "2022-12-26",
          "2022-12-28"
        ),
        shiny::actionButton("uiGetSpots", "Get Spots")
      )
    ),
    dark = NULL,
    body = bs4Dash::dashboardBody(
      fresh::use_theme(fresh::create_theme(itvPalette::itv_bs4dash())),
      # CSS
      tags$head(tags$style(
        HTML(
          "
      .img-circle {
        border-radius: 0 !important;
      }
      .brand-link .brand-image{
        float: none !important;
      }
      .elevation-3{
        box-shadow: 0 0 0 0 !important;
      }"
        )
      )),
      fluidRow(column(3, 
        bs4Dash::valueBoxOutput("advertiser_info", width = 12)
      ),
      column(3,
        bs4Dash::valueBoxOutput("date_info",
                                width = 12)
      ),
      column(3,
        bs4Dash::valueBoxOutput("spot_count_info",
                                width = 12)
      ),
      column(3,
        bs4Dash::valueBoxOutput("impacts_info",
                                       width = 12)
      )),
      fluidRow(
        bs4Dash::box(
          width = 12,
          headerBorder = FALSE,
          shinycssloaders::withSpinner(plotly::plotlyOutput("daily_impacts_chart"))
        )
  )),
    title = "BARB Browser"
  )

  # Define server logic required to draw a histogram
  server <- function(input, output, session) {
    
    # gar_shiny_auth(session)
    
    gar_set_client(
      web_json = "client_secret.json",
      scopes = "https://www.googleapis.com/auth/userinfo.email")
    
    
    advertisers <- baRb::barb_get_advertisers()

    output$advertiser_select <- shiny::renderUI({
      shiny::selectInput("uiSelectAdvertiser", "Select Advertiser", advertisers)
    })

    advertiser_spots <- reactive({
      input$uiGetSpots

      isolate({
        req(input$uiSelectAdvertiser)

        spots <-
          baRb::barb_get_spots(
            min_transmission_date = input$uiDateRange[1],
            max_transmission_date = input$uiDateRange[2],
            advertiser_name = input$uiSelectAdvertiser
          )
      })
    })
    
    spots_daily <- reactive({
      req(nrow(advertiser_spots()) > 0)
      
      advertiser_spots() %>%
        dplyr::mutate(date = lubridate::as_date(standard_datetime)) %>%
        dplyr::group_by(date) %>%
        dplyr::summarise(impacts = sum(`All Adults`, na.rm = TRUE))
    })

    output$daily_impacts_chart <- plotly::renderPlotly({
      spots_daily() %>%
        plotly::plot_ly() %>%
        plotly::add_bars(
          x = ~ date,
          y = ~ impacts,
          marker = list(color = itvPalette::itv_palette()$blue)
        )
    })


    output$advertiser_info <- bs4Dash::renderValueBox({
      bs4Dash::valueBox(
        subtitle = "Advertiser",
        value = input$uiSelectAdvertiser,
        icon = shiny::icon("briefcase"),
        color = "gray"
      )
    })
    
    output$date_info <- bs4Dash::renderValueBox({
      bs4Dash::valueBox(
        subtitle = "Date Range",
        value = glue::glue('{input$uiDateRange[1]} to {input$uiDateRange[2]}'),
        icon = shiny::icon("calendar"),
        color = "gray"
      )
    })
    
    output$spot_count_info <- bs4Dash::renderValueBox({
      bs4Dash::valueBox(
        subtitle = "Spot Count",
        value = nrow(advertiser_spots()),
        icon = shiny::icon("calculator"),
        color = "gray"
      )
    })

    output$impacts_info <- bs4Dash::renderValueBox({
      bs4Dash::valueBox(
        subtitle = "Adult Impacts",
        value = sum(advertiser_spots()$`All Adults`, na.rm = TRUE),
        icon = shiny::icon("chart-simple"),
        color = "gray"
      )
    })


  }
  
  # shinyApp(gar_shiny_ui(ui, login_ui = gar_shiny_login_ui), server)
  shinyApp(ui, server)
}
