library(shiny)
library(magrittr)
library(googleAuthR)
library(geoxR)
library(waiter)

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

  # Define UI for application that draws a histogram
  ui <- bslib::page_fluid(
    
    use_waiter(),
    waiter_on_busy(),
    
    title = "Interactive GeoX",
    
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
    
    div(img(
      src = "https://storage.googleapis.com/itv-logos/itv_background_crop.jpg",
      width = "100%",
      height = "100px",
      style = "margin:1px 1px; object-fit:cover; object-position:top",
    )),
    
    bslib::page_sidebar(
      title = "Interactive GeoX",
      
      header = imageOutput(
        "https://storage.googleapis.com/itv-logos/itv_background_crop.jpg"
        ),
      
      sidebar = bslib::sidebar(
        title = "Data Connections",
        width = 400,
        dateRangeInput("uiDateRange", "Date range including pre-test period", start = lubridate::today() - 40, end = lubridate::today() - 10),
        
        bslib::navset_card_tab(
          bslib::nav_panel(
            title = "Google Analytics",
            uiOutput("traffic_filter"),
            googleAnalyticsR::accountPickerUI("auth_menu", inColumns = TRUE),
            actionButton("getGA4", "Get Traffic"),
            span(textOutput("confirm_ga"), style="color:green"),
            hr()
          ),
          bslib::nav_panel(
            title = "BARB",
            shiny::uiOutput("advertiser_select"),
            actionButton("getSpots", "Get Spots"),
            br(),br(),
            span(textOutput("confirm_spots"), style="color:green"),
            hr()
          )
        )
      ),
      
      bslib::navset_card_pill(
        title = "GeoX Modelling",
        bslib::nav_panel(
          title = "Data Investigation",
        bslib::navset_card_tab(
          title = NULL,
          bslib::nav_panel(
            "Traffic Source Summary",
            bslib::layout_columns(
              col_widths= c(8,4),
            plotly::plotlyOutput("traffic_summary_plot"),
            div(
              p("Use traffic sources to understand which filtered sources may be worth running in addition to analysing all traffic."),
               p("Organic Search and Direct traffic sources tend to be particularly responsive to TV and can be useful when uplifts are more difficult to detect.")
            )
            )
            
          ),
          bslib::nav_panel(
            "Traffic Plot",
            bslib::layout_columns(
              col_widths= c(8,4),
              plotly::plotlyOutput("traffic_plot"),
              p("Check that daily traffic looks as you expect (you've connected to the right account!) and doesn't contain very large short-term spikes or missing periods.")
              p("Missing data or sudden very low periods of traffic can be a sign that there have been issues with tagging. Large spikes may be email drops or PR events, which may cause issues if they have different impacts in different regions.")
            )),
          bslib::nav_panel(
            title = "Impacts Time Series",
            bslib::layout_columns(
              col_widths= c(8,4),
              plotly::plotlyOutput("daily_impacts_chart"),
              p("Verify that campaign start and end dates and daily Adult impacts look as expected.")
              p("In particular, check the there is not additional unexpected activity running alongside the test that you are trying to measure and that there are not campaigns just before the test that you were not aware of.")
            )),
          bslib::nav_panel(
            title = "Impacts Sales Houses and Regionality",
            bslib::layout_columns(
              col_widths= c(4,4,4),
              plotly::plotlyOutput("region_chart"),
              plotly::plotlyOutput("sales_house_chart"),
              p("Verify that active regions are as briefed and that there is not additional activity running in addition to regionalised ITV1")
            )),
          bslib::nav_panel(
            title = "Largest Spots",
            bslib::layout_columns(
              col_widths= c(8,4),
              DT::dataTableOutput("spots_summary_table"),
              div(
                p("Visualising minute-by-minute traffic around large spots is useful to check that the advert is working to prompt web visits in the very short term."),
                p("Lack of minute-by-minute response can be a sign that the advert has not communicated an online destination and may explain low response in the GeoX test itself if strong uplifts are not measured."),
                p("Click on a spot to see minute-by-minute traffic on that day."),
                actionButton("ui_show_minute_chart", "Show Traffic on Selected Day")
              )
            )))),
          bslib::nav_panel(
            title = "Uplift Calculation",
            bslib::navset_card_tab(
              title = NULL,
            
          bslib::nav_panel(
            "Test Setup",
            dateInput("uiTestStartDate", "Test start date", value = lubridate::today() - 20),
            shiny::uiOutput("test_regions_select"),
            shiny::uiOutput("excluded_regions_select"),
            actionButton("uiCalculateGeolift", "Calculate results")
          ),
          bslib::nav_panel(
            "Test vs. Control",
            bslib::layout_columns(
              col_widths= c(8,4),
              plotly::plotlyOutput("geolift_test_control"),
              p()
          )),
          bslib::nav_panel(
            "Incremental Uplift",
            bslib::layout_columns(
              col_widths= c(8,4),
              plotly::plotlyOutput("geolift_att"),
              p()
          ))),
        bslib::layout_columns(
          col_widths = c(4,4,4),
          uiOutput("att_value"),
          uiOutput("percent_uplift_value"),
          uiOutput("confidence_value")
        )
    )))
    
)

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
    
    output$traffic_filter <- renderUI({
      selectInput("uiTrafficFilter", "Traffic Filter", c("NO FILTER", "Paid Search", "Organic Search", "Paid Social", "Direct"))
    })
    
    ga4_traffic <- shiny::eventReactive(input$getGA4, {
      
      if(input$uiTrafficFilter != "NO FILTER"){
        filter_clause <- ga_filter_channel_group(input$uiTrafficFilter)
      } else {
        filter_clause <- NULL
      }
      
      all_traffic <- ga4_query(ga_id(), input$uiDateRange[1], input$uiDateRange[2], filter_clause = filter_clause)

      all_traffic
    })
    
    ga4_traffic_source_summary <- shiny::eventReactive(input$getGA4, {
      
        traffic_sources <- ga4_summarise_sources(ga_id(), input$uiDateRange[1], input$uiDateRange[2])

      traffic_sources
    })
    
    
    # END GA Auth -------------------
    
    # Model Controls ----------------
    
    output$test_regions_select <- shiny::renderUI({
      
      shiny::selectInput("uiTestRegionsSelect", "Test Regions", region_list[!region_list %in% input$uiExcludedRegionsSelect], multi = TRUE)
      
    })
    
    output$excluded_regions_select <- shiny::renderUI({
      
      shiny::selectInput("uiExcludedRegionsSelect", "Excluded Regions", region_list, multi = TRUE)
      
    })
    
    # --------------- GA Traffic summaries -------------------------------------
    
    
    output$traffic_plot <- plotly::renderPlotly({
      
      ga4_traffic() |> 
        dplyr::group_by(date) |> 
        dplyr::mutate(sessions = sum(sessions)) |> 
        dplyr::ungroup() |> 
        plotly::plot_ly() |> 
          plotly::add_lines(x = ~date, y = ~sessions)
        
    })
    
    output$traffic_summary_plot <- plotly::renderPlotly({
      ga4_traffic_source_summary() |> 
        dplyr::mutate(sessionDefaultChannelGroup = forcats::fct_inorder(sessionDefaultChannelGroup)) |> 
        plotly::plot_ly() |> 
          plotly::add_bars(orientation = 'h', y = ~sessionDefaultChannelGroup, x = ~sessions)
      
    })
    
    output$confirm_ga <- renderText({
      validate(need(
        ga4_traffic(), "Select a Google Analytics account and click Get Traffic"
      ))
      "Google Analytics loaded"
    })
    
  
    # --------------- Geolift --------------------------------------------------  
  
    # Uplift Calculation ------------
    
    geolift <- eventReactive(input$uiCalculateGeolift, {
      
      req(ga4_traffic())
      
      validate(need(input$uiTestRegionsSelect, "Select at least one test region before running."))
      
      geolifts <- NULL
      
      try({
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
      })
      
      shiny::validate(
        need(!is.null(geolifts), "Geolift did not calculate successfully. Are you sure the test dates are correctly specified and your test regions have consistent data series?")
      )
      
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
  
  # --------------- BARB Spots -------------------------------------------------
  
  advertisers <- baRb::barb_get_advertisers()

  output$advertiser_select <- shiny::renderUI({
    shiny::selectInput("uiSelectAdvertiser", label = "Advertiser", choices = advertisers, multiple = FALSE, selectize = TRUE)
  })

  advertiser_spots <- eventReactive(input$getSpots, {
    
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
    
  output$confirm_spots <- renderText({
    validate(need(
      advertiser_spots(), "Select an advertiser and click Get Spots"
    ))
    "BARB Spots loaded"
  })
      
    
  spots_rollup <- reactive({
    req(nrow(advertiser_spots()) > 0)
    
    advertiser_spots() |>
        dplyr::mutate(date = lubridate::as_date(standard_datetime)) |> 
        dplyr::mutate(date = lubridate::floor_date(date, "day")) |> 
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
  
  spots_summary <- reactive({
    advertiser_spots() |> 
      dplyr::group_by(sales_house_name,
        standard_datetime,
        preceding_programme_name) |> 
      dplyr::summarise(all_adults = sum(all_adults)) |> 
      dplyr::arrange(-all_adults) |> 
      dplyr::ungroup()
  })
  
  output$spots_summary_table <- DT::renderDataTable({
    spots_summary() |> 
      dplyr::rename(`Sales House` = sales_house_name,
             `Time` = standard_datetime,
             `Preceding Programme` = preceding_programme_name,
             Adults = all_adults) |> 
      dplyr::mutate(Adults = Adults * 100) |> 
      DT::datatable(
        selection = 'single'
      )
  })
  
  # Minute level chart popup ---------------------------------------------------
  output$minute_level_chart <- plotly::renderPlotly({
    
    req(input$spots_summary_table_rows_selected)
    
    spot <- spots_summary()[input$spots_summary_table_rows_selected,]
    
    # Get traffic for the selected day
    ga_minute <- googleAnalyticsR::ga_data(ga_id(),
                                           date_range = c(as.Date(spot$standard_datetime), as.Date(spot$standard_datetime)),
                                           dimensions = c('date', 'hour', 'minute'),
                                           metrics = 'sessions',
                                           limit = -1) |> 
      dplyr::mutate(hour_minute = glue::glue("{hour}_{minute}"))
    
    ga_minute |>
      dplyr::arrange(as.numeric(hour), as.numeric(minute)) |> 
      dplyr::mutate(hour_minute = forcats::fct_inorder(as.character(hour_minute))) |> 
      plotly::plot_ly() |> 
      plotly::add_bars(x = ~hour_minute, y =~ sessions)
    
     
  })
  
  observeEvent(input$ui_show_minute_chart, {
    showModal(
      modalDialog(plotly::plotlyOutput("minute_level_chart"),
                  size = "xl",
                  easyClose = TRUE)
    )
  })


  }
  
  shinyApp(gar_shiny_ui(ui, login_ui = login_screen), server)
  # shinyApp(ui, server)
}
