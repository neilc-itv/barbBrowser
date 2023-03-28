library(shiny)
library(magrittr)
library(googleAuthR)

options(googleAuthR.redirect = "https://barb-browser-v2mof5dnmq-nw.a.run.app")

gar_set_client(
  web_json = "client_secret.json",
  scopes = "https://www.googleapis.com/auth/userinfo.email",
  activate = "web")

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
          "Date Range",
          "2023-01-01",
          "2023-01-31"
        ),
        shiny::actionButton("uiGetSpots", "Get Spots"),
        hr(),
        shiny::textInput("uiTrendsTerm", "Google Trends Search Term"),
        shiny::actionButton("uiGetTrends", "Get Trends"),
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
        bs4Dash::bs4TabCard(
          width = 12,
          tabPanel(
              'Daily Impacts',
                width = 12,
                headerBorder = FALSE,
                shinycssloaders::withSpinner(plotly::plotlyOutput("daily_impacts_chart"))
          ),
          tabPanel(
            'Sales Houses',
            width = 12,
            headerBorder = FALSE,
            shinycssloaders::withSpinner(plotly::plotlyOutput("sales_house_chart"))
          ),
          tabPanel(
            'Stations',
            width = 12,
            headerBorder = FALSE,
            shinycssloaders::withSpinner(plotly::plotlyOutput("stations_chart"))
          )
        )
  )),
    title = "BARB Browser"
  )

  # Define server logic required to draw a histogram
  server <- function(input, output, session) {
    
    gar_shiny_auth(session)
    
    advertisers <- baRb::barb_get_advertisers()

    output$advertiser_select <- shiny::renderUI({
      shiny::selectInput("uiSelectAdvertiser", "Advertiser", advertisers)
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
      
      test <- advertiser_spots() |>
        dplyr::mutate(date = lubridate::as_date(standard_datetime)) |> 
        dplyr::group_by(date) |> 
        dplyr::summarise(impacts = sum(`all_adults`, na.rm = TRUE))
    })

    output$daily_impacts_chart <- plotly::renderPlotly({
    
      req(spots_daily())
    
      plot <- spots_daily() |> 
        plotly::plot_ly() |> 
        plotly::add_bars(
          x = ~ date,
          y = ~ impacts,
          name = "Adult Impacts",
          marker = list(color = itvPalette::itv_palette()$blue)
        )
      
      if(!is.null(google_trends())){
        
        trends <- google_trends()
        
        plot <- plot |> 
          plotly::add_lines(data = trends,
                            x = ~date,
                            y = ~hits,
                            yaxis = "y2",
                            name = "Google Trends",
                            line = list(color = "#a90061"))  |> 
          plotly::layout(yaxis2 = list(overlaying = "y", side = "right"))
      }
      
      plot
      
    })

    output$sales_house_chart <- plotly::renderPlotly({
      
      req(advertiser_spots())
      
      plot <- advertiser_spots() |>
        dplyr::group_by(sales_house_name) |>
        dplyr::summarise(all_adults = sum(all_adults)) |> 
        dplyr::arrange(all_adults) |>
        dplyr::mutate(sales_house_name = forcats::fct_inorder(sales_house_name)) |> 
        plotly::plot_ly() |> 
          plotly::add_bars(
            x = ~ all_adults,
            y = ~ sales_house_name,
            name = "Adult Impacts",
            marker = list(color = itvPalette::itv_palette()$blue)
          ) |> 
        plotly::layout(
          yaxis = list(title = "")
        )
      
      plot
      
    })
    
    output$stations_chart <- plotly::renderPlotly({
      
      req(advertiser_spots())
      
      plot <- advertiser_spots() |>
        dplyr::group_by(station_name) |>
        dplyr::summarise(all_adults = sum(all_adults)) |> 
        dplyr::arrange(all_adults) |>
        dplyr::mutate(station_name = forcats::fct_inorder(station_name)) |> 
        plotly::plot_ly() |> 
        plotly::add_bars(
          x = ~ all_adults,
          y = ~ station_name,
          name = "Adult Impacts",
          marker = list(color = itvPalette::itv_palette()$blue)
        ) |> 
        plotly::layout(
          yaxis = list(title = "")
        )
      
      plot
      
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
        value = sum(advertiser_spots()$`all_adults`, na.rm = TRUE),
        icon = shiny::icon("chart-simple"),
        color = "gray"
      )
    })
    
    google_trends <- reactive({
      input$uiGetTrends
      
      search_term <- isolate(input$uiTrendsTerm)
      
      if(search_term=="") return(NULL)
      
      trends <- gtrendsR::gtrends(search_term,
                                  geo = "GB",
                                  glue::glue("{isolate(input$uiDateRange[1])} {isolate(input$uiDateRange[2])}"))
      
      trends$interest_over_time
    })


  }
  
  # shinyApp(gar_shiny_ui(ui, login_ui = gar_shiny_login_ui), server)
  shinyApp(ui, server)
}
