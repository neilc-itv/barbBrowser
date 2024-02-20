library(shiny)
library(magrittr)
library(googleAuthR)
library(geoxR)

options(googleAuthR.redirect = "https://barb-browser-v2mof5dnmq-nw.a.run.app")
# options(googleAuthR.redirect = "http://localhost:1221")

gar_set_client(
  web_json = "client_secret.json",
  scopes = c("https://www.googleapis.com/auth/userinfo.email",
             "https://www.googleapis.com/auth/analytics.edit",
             "https://www.googleapis.com/auth/analytics.readonly"),
  activate = "web")

options(shiny.port = 1221)

options("spinner.color" = itvPalette::itv_palette()$blue)
auth <- jsonlite::read_json("auth.json")
Sys.setenv(BARB_API_USERNAME=auth$username)
Sys.setenv(BARB_API_PASSWORD=auth$password)

thematic::thematic_shiny(font = "auto")

region_list <- geoxR::city_macro_lookup |> 
  dplyr::filter(!is.na(macro)) |> 
  dplyr::group_by(macro) |> 
  dplyr::summarise() |> 
  dplyr::pull(macro)


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
  ui <- bslib::page_fluid(
    
    title = "GeoX Self-Service",
    
        # href = "https://www.itvmedia.co.uk/itv-adlabs/product",
        # image = "https://storage.googleapis.com/itv-logos/adlabs.jpg"

    theme = bslib::bs_theme(
      version = 5,
      fg = "rgb(99, 99, 105)",
      primary = "#0568A6",
      secondary = "#D7D7D9",
      success = "#52BD6F",
      info = "#0568A6",
      warning = "#F2B705",
      danger = "#D92344",
      base_font = bslib::font_google("Nunito Sans"),
      heading_font = bslib::font_google("Nunito Sans"),
      font_scale = 0.8,
      `enable-rounded` = TRUE,
      preset = "cosmo",
      bg = "#fff"
    ),
    
    
    # controlbar = bs4Dash::dashboardControlbar(
    #   collapsed = FALSE,
    #   column(
    #     8,
    #     shiny::uiOutput("advertiser_select"),
    #     shiny::dateRangeInput(
    #       "uiDateRange",
    #       "Date Range",
    #       lubridate::today()-24,
    #       lubridate::today()-10
    #     ),
    #     shiny::actionButton("uiGetSpots", "Get Spots"),
    #     hr(),
    #     shiny::textInput("uiTrendsTerm", "Google Trends Search Term"),
    #     shiny::actionButton("uiGetTrends", "Get Trends"),
    #     hr(),
    #     shiny::radioButtons("uiRollup", "Granularity", c("Daily" = "day", "Weekly" = "week"))
    #   )
    # ),
    
    bslib::page_sidebar(
      title = div(img(
        src = "https://storage.googleapis.com/itv-logos/itv_background_crop.jpg",
        height = 20,
        width = 20,
        style = "margin:1px 1px"
      ), "Self-Serve GeoX"),
      
      sidebar = bslib::sidebar(
        title = "Connect Google Analytics",
        dateRangeInput("uiDateRange", "Date range including pre-test period", start = lubridate::today() - 40, end = lubridate::today() - 10),
        googleAnalyticsR::accountPickerUI("auth_menu", inColumns = TRUE),
        actionButton("getGA4", "Get Traffic")
      ),
      
      bslib::layout_columns(
        col_widths = c(8,4),
        bslib::navset_card_tab(
          title = "GeoX Modelling",
          bslib::nav_panel(
            "Traffic Plot",
            plotly::plotlyOutput("traffic_plot")

          ),
          bslib::nav_panel(
            "Test Setup",
            dateInput("uiTestStartDate", "Test start date", value = lubridate::today() - 20),
            shiny::uiOutput("test_regions_select"),
            shiny::uiOutput("excluded_regions_select"),
            actionButton("uiCalculateGeolift", "Calculate results")
          ),
          bslib::nav_panel(
            "Test vs. Control",
            plotly::plotlyOutput("geolift_test_control")
          ),
          bslib::nav_panel(
            "Incremental Uplift",
            plotly::plotlyOutput("geolift_att")
          )),
        bslib::layout_columns(
          col_widths = 12,
          uiOutput("att_value"),
          uiOutput("percent_uplift_value"),
          uiOutput("confidence_value")
        )
      )
      
    ),
    
    bslib::card(
      # -----------------
      
      # fluidRow(column(3, 
      #   bs4Dash::valueBoxOutput("advertiser_info", width = 12)
      # ),
      # column(3,
      #   bs4Dash::valueBoxOutput("date_info",
      #                           width = 12)
      # ),
      # column(3,
      #   bs4Dash::valueBoxOutput("spot_count_info",
      #                           width = 12)
      # ),
      # column(3,
      #   bs4Dash::valueBoxOutput("impacts_info",
      #                                  width = 12)
      # )),
      # fluidRow(
      #   bs4Dash::bs4TabCard(
      #     width = 12,
      #     tabPanel(
      #         'Daily Impacts',
      #           width = 12,
      #           headerBorder = FALSE,
      #           shinycssloaders::withSpinner(plotly::plotlyOutput("daily_impacts_chart"))
      #     ),
      #     tabPanel(
      #       'Sales Houses',
      #       width = 12,
      #       headerBorder = FALSE,
      #       shinycssloaders::withSpinner(plotly::plotlyOutput("sales_house_chart"))
      #     ),
      #     tabPanel(
      #       'Regions',
      #       width = 12,
      #       headerBorder = FALSE,
      #       shinycssloaders::withSpinner(plotly::plotlyOutput("region_chart"))
      #     ))
  ))

  # Define server logic required to draw a histogram
  server <- function(input, output, session) {
    
    # gar_shiny_auth(session)
    
    # GA Auth -------------------
    
    token <- gar_shiny_auth(session)

    accs <- reactive({

      req(token)
      googleAnalyticsR::ga_account_list("ga4")
    })

    # module for authentication
    property_id <- googleAnalyticsR::accountPicker("auth_menu", ga_table = accs, id_only = TRUE)
    
    ga_id <- reactive({
      accs() |> 
        dplyr::filter(account_name==input$`auth_menu-account_name`,
                      property_name==input$`auth_menu-property_name`) |> 
        dplyr::pull(propertyId)
    })
    
    ga4_traffic <- shiny::eventReactive(input$getGA4, {
      
      withProgress(message = 'Getting web traffic', value = 0.5, {
        all_traffic <- ga4_query(ga_id(), input$uiDateRange[1], input$uiDateRange[2])
        setProgress(0.9)
      })
      
      all_traffic
    })
    
    # END GA Auth -------------------
    
    # Model Controls ----------------
    
    output$test_regions_select <- shiny::renderUI({
      
      shiny::selectInput("uiTestRegionsSelect", "Test Regions", region_list[!region_list %in% input$uiExcludedRegionsSelect], multi = TRUE)
      
    })
    
    output$excluded_regions_select <- shiny::renderUI({
      
      shiny::selectInput("uiExcludedRegionsSelect", "Excluded Regions", region_list, multi = TRUE)
      
    })
    
    
    output$traffic_plot <- plotly::renderPlotly({
      
      ga4_traffic() |> 
        dplyr::group_by(date) |> 
        dplyr::mutate(sessions = sum(sessions)) |> 
        dplyr::ungroup() |> 
        plotly::plot_ly() |> 
          plotly::add_lines(x = ~date, y = ~sessions)
        
    })
    
  
    # Uplift Calculation ------------
    
    geolift <- eventReactive(input$uiCalculateGeolift, {
      req(ga4_traffic())
      
      withProgress(message = 'Calculating Geolifts', value = 0.5, {
        geolifts <- calculate_geoLifts(
          list("all_traffic" = ga4_traffic()),
          test_regions = input$uiTestRegionsSelect,
          exclude_regions = input$uiExcludedRegionsSelect,
          test_start = input$uiTestStartDate[1],
          test_end = input$uiDateRange[2],
          benchmark_advertiser = input$`auth_menu-account_name`,
          benchmark_comment = "Shiny self-serve run",
          benchmark_schedule = NA
        )
        
        setProgress(0.9)
      })
      
      geolifts
    })
    
    output$geolift_test_control <- plotly::renderPlotly({
      geoLift_plotly_test_control(geolift()$all_traffic, input$uiDateRange[1], input$uiTestStartDate[1], "All Traffic")
    })
    
    output$geolift_att <- plotly::renderPlotly({
      geoLift_plotly_att(geolift()$all_traffic, input$uiDateRange[1], input$uiTestStartDate[1], "All Traffic")
    })
    
  output$percent_uplift_value <- renderUI({
    bslib::value_box(
      title = "Percentage Uplift",
      value = geolift()$all_traffic$inference$Perc.Lift,
      showcase = bsicons::bs_icon("percent")
    )
    
  })
  
  output$confidence_value <- renderUI({
    
    bslib::value_box(
      title = "Confidence in Measurement",
      value = round((1-geolift()$all_traffic$summary$average_att$p_val)*100),
      showcase = bsicons::bs_icon("percent")
    )
    })
    
  output$att_value <- renderUI({
    bslib::value_box(
      title = "Total Uplift",
      value = round(geolift()$all_traffic$incremental),
      showcase = bsicons::bs_icon("arrow-up")
    )
  })
  
    advertisers <- baRb::barb_get_advertisers()

    output$advertiser_select <- shiny::renderUI({
      shiny::selectizeInput("uiSelectAdvertiser", label = "Advertiser", choices = advertisers, multiple = FALSE)
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
        
        shiny::validate(
          need(nrow(spots) > 0, "No spots returned for the requested time range and advertiser")
        )
        
        spots
      })
    })
    
    spots_rollup <- reactive({
      req(nrow(advertiser_spots()) > 0)
      
      test <- advertiser_spots() |>
        dplyr::mutate(date = lubridate::as_date(standard_datetime)) |> 
        dplyr::mutate(date = lubridate::floor_date(date, input$uiRollup)) |> 
        dplyr::group_by(date) |> 
        dplyr::summarise(impacts = sum(`all_adults`, na.rm = TRUE))
    })

    output$daily_impacts_chart <- plotly::renderPlotly({
    
      req(spots_rollup())
    
      plot <- spots_rollup() |> 
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
        dplyr::summarise(all_adults = sum(all_adults, na.rm = TRUE)) |> 
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
    
    output$region_chart <- plotly::renderPlotly({
      
      req(advertiser_spots())
      
      plot <- advertiser_spots() |>
        dplyr::group_by(panel_region) |>
        dplyr::summarise(all_adults = sum(all_adults, na.rm = TRUE)) |> 
        dplyr::arrange(all_adults) |>
        dplyr::mutate(panel_region = forcats::fct_inorder(panel_region)) |> 
        plotly::plot_ly() |> 
        plotly::add_bars(
          x = ~ all_adults,
          y = ~ panel_region,
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
      
      trends$interest_over_time |> 
        dplyr::mutate(date = lubridate::floor_date(date, input$uiRollup)) |> 
        dplyr::group_by(date) |> 
        dplyr::summarise(hits = mean(hits))
    })


  }
  
  shinyApp(gar_shiny_ui(ui, login_ui = login_screen), server)
  # shinyApp(ui, server)
}
