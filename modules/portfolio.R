portfolioUI <- function(id, manualdata, historicaldata, portfoliodata) {
  ns <- NS(id)
  
  bslib::page_fillable(
    bslib::layout_sidebar(
      fillable = TRUE,
      sidebar = bslib::sidebar(
        width = 320,
        bslib::card(
          bslib::card_header("Filter"),
          dateRangeInput(
            ns("daterange"),
            "Zeitraum",
            start = as.Date("2021-01-01"),
            end   = Sys.Date()
          ),
          checkboxGroupInput(
            ns("forms"),
            "Asset-Typ",
            choices = c("ETF", "AKTIE", "CRYPTO", "SPARBRIEF"),
            selected = c("ETF", "AKTIE")
          ),
          checkboxInput(ns("exclude_crypto"), "Crypto ausblenden (Quick Toggle)", value = TRUE),
          hr(),
          radioButtons(
            ns("kpi_mode"),
            "KPIs beziehen auf",
            choices = c("Letzter Stand" = "last", "Zeitraum (Start→Ende)" = "range"),
            selected = "last",
            inline = TRUE
          )
        )
      ),
      
      bslib::layout_column_wrap(
        width = 1/3,
        bslib::value_box(
          title = "Depotwert",
          value = uiOutput(ns("kpi_value")),
          showcase = bsicons::bs_icon("wallet2")
        ),
        bslib::value_box(
          title = "Rendite (€)",
          value = uiOutput(ns("kpi_return_abs")),
          showcase = bsicons::bs_icon("graph-up-arrow")
        ),
        bslib::value_box(
          title = "Rendite (%)",
          value = uiOutput(ns("kpi_return_pct")),
          showcase = bsicons::bs_icon("percent")
        )
      ),
      
      bslib::navset_card_underline(
        height = "100%",
        full_screen = TRUE,
        
        bslib::nav_panel(
          "Überblick",
          icon = bsicons::bs_icon("speedometer2"),
          bslib::layout_column_wrap(
            width = 1/2,
            bslib::card(
              bslib::card_header("Depotwert & eingesetztes Kapital"),
              highchartOutput(ns("hc_equity_curve"), height = "360px")
            ),
            bslib::card(
              bslib::card_header("Monatliche Cashflows (Käufe/Einzahlungen)"),
              highchartOutput(ns("hc_cashflows"), height = "360px")
            ),
            bslib::card(
              bslib::card_header("Drawdown"),
              highchartOutput(ns("hc_drawdown"), height = "320px")
            ),
            bslib::card(
              bslib::card_header("Rolling 12M Return"),
              highchartOutput(ns("hc_rolling_12m"), height = "320px")
            )
          )
        ),
        
        bslib::nav_panel(
          "Allokation",
          icon = bsicons::bs_icon("pie-chart"),
          bslib::layout_column_wrap(
            width = 1/2,
            bslib::card(
              bslib::card_header("Allokation (Letzter Stand)"),
              highchartOutput(ns("hc_allocation"), height = "360px")
            ),
            bslib::card(
              bslib::card_header("Allokation im Zeitverlauf"),
              highchartOutput(ns("hc_allocation_ts"), height = "360px")
            )
          )
        ),
        
        bslib::nav_panel(
          "Beiträge",
          icon = bsicons::bs_icon("bar-chart-line"),
          bslib::layout_column_wrap(
            width = 1,
            bslib::card(
              bslib::card_header("Monatliche Veränderung der Rendite (Δ €)"),
              highchartOutput(ns("hc_return_delta"), height = "380px")
            )
          )
        )
      )
    )
  )
}

portfolioServer <- function(id, manualdata, historicaldata, portfoliodata) {
  moduleServer(id, function(input, output, session) {
    
    # ------------------------------------------------------------
    # 1) Basis-Zeitreihe aus portfoliodata
    # ------------------------------------------------------------
    ts_data <- reactive({
      df <- portfoliodata %>%
        purrr::map_dfr(~ .x$data) %>%
        dplyr::mutate(datum = as.Date(datum))
      
      # Filter: forms
      selected_forms <- input$forms
      if (isTRUE(input$exclude_crypto)) {
        selected_forms <- setdiff(selected_forms, "CRYPTO")
      }
      
      df <- df %>%
        dplyr::filter(form %in% selected_forms) %>%
        dplyr::filter(datum >= input$daterange[1], datum <= input$daterange[2])
      
      # Aggregation je Tag (gesamt)
      total <- df %>%
        dplyr::group_by(datum) %>%
        dplyr::summarise(
          eingesetztes_kapital = sum(saldo_raw, na.rm = TRUE),
          gesamtwert = sum(saldo_rendite, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::arrange(datum) %>%
        dplyr::mutate(
          rendite_abs = gesamtwert - eingesetztes_kapital,
          rendite_pct = dplyr::if_else(eingesetztes_kapital > 0,
                                       (gesamtwert / eingesetztes_kapital - 1) * 100,
                                       NA_real_)
        )
      
      list(total = total, by_form = df)
    })
    
    # ------------------------------------------------------------
    # 2) KPIs (Letzter Stand oder Zeitraum)
    # ------------------------------------------------------------
    kpis <- reactive({
      total <- ts_data()$total
      if (nrow(total) == 0) return(list(value = NA, ra = NA, rp = NA))
      
      if (input$kpi_mode == "range") {
        first <- total[1, ]
        last  <- total[nrow(total), ]
        value <- last$gesamtwert
        ra    <- last$gesamtwert - first$gesamtwert
        rp    <- if (!is.na(first$gesamtwert) && first$gesamtwert > 0) (last$gesamtwert / first$gesamtwert - 1) * 100 else NA
      } else {
        last <- total[nrow(total), ]
        value <- last$gesamtwert
        ra    <- last$rendite_abs
        rp    <- last$rendite_pct
      }
      
      list(value = value, ra = ra, rp = rp)
    })
    
    output$kpi_value <- renderUI({
      x <- kpis()$value
      if (is.na(x)) return(HTML("&ndash;"))
      scales::dollar(x, prefix = "", suffix = " €", big.mark = ".", decimal.mark = ",")
    })
    output$kpi_return_abs <- renderUI({
      x <- kpis()$ra
      if (is.na(x)) return(HTML("&ndash;"))
      scales::dollar(x, prefix = "", suffix = " €", big.mark = ".", decimal.mark = ",")
    })
    output$kpi_return_pct <- renderUI({
      x <- kpis()$rp
      if (is.na(x)) return(HTML("&ndash;"))
      sprintf("%.2f %%", x)
    })
    
    # ------------------------------------------------------------
    # 3) Equity Curve: Wert + Kapital (Area/Line) + Rendite im Tooltip
    # ------------------------------------------------------------
    output$hc_equity_curve <- renderHighchart({
      total <- ts_data()$total
      if (nrow(total) == 0) return(highchart())
      
      highchart() %>%
        hc_xAxis(type = "datetime") %>%
        hc_yAxis(title = list(text = "€")) %>%
        hc_add_series(total, "area", hcaes(x = datum, y = eingesetztes_kapital),
                      name = "Eingesetztes Kapital", opacity = 0.35) %>%
        hc_add_series(total, "line", hcaes(x = datum, y = gesamtwert),
                      name = "Depotwert") %>%
        hc_tooltip(
          shared = TRUE,
          useHTML = TRUE,
          headerFormat = "<b>{point.key}</b><br/>",
          pointFormat = "<span style='color:{series.color}'>●</span> {series.name}: <b>{point.y:,.2f} €</b><br/>",
          footerFormat = "<br/><b>Rendite:</b> {point.rendite_abs:,.2f} € ({point.rendite_pct:,.2f} %)"
        ) %>%
        hc_title(text = NULL)
    })
    
    # ------------------------------------------------------------
    # 4) Drawdown (aus gesamtwert)
    # ------------------------------------------------------------
    output$hc_drawdown <- renderHighchart({
      total <- ts_data()$total
      if (nrow(total) == 0) return(highchart())
      
      dd <- total %>%
        dplyr::mutate(
          peak = cummax(gesamtwert),
          drawdown = dplyr::if_else(peak > 0, (gesamtwert / peak - 1) * 100, NA_real_)
        )
      
      highchart() %>%
        hc_add_series(dd, "area", hcaes(x = datum, y = drawdown), name = "Drawdown (%)", opacity = 0.35) %>%
        hc_xAxis(type = "datetime") %>%
        hc_yAxis(title = list(text = "%"), max = 0) %>%
        hc_tooltip(pointFormat = "<b>{point.y:,.2f} %</b>") %>%
        hc_title(text = NULL)
    })
    
    # ------------------------------------------------------------
    # 5) Rolling 12M Return (aus gesamtwert)
    # ------------------------------------------------------------
    output$hc_rolling_12m <- renderHighchart({
      total <- ts_data()$total
      if (nrow(total) < 260) return(highchart())  # grob ~ 1 Jahr Handelstage
      
      roll <- total %>%
        dplyr::arrange(datum) %>%
        dplyr::mutate(
          value_lag = dplyr::lag(gesamtwert, 365),
          roll_12m = dplyr::if_else(!is.na(value_lag) & value_lag > 0,
                                    (gesamtwert / value_lag - 1) * 100, NA_real_)
        ) %>%
        dplyr::filter(!is.na(roll_12m))
      
      highchart() %>%
        hc_add_series(roll, "line", hcaes(x = datum, y = roll_12m), name = "Rolling 12M (%)") %>%
        hc_xAxis(type = "datetime") %>%
        hc_yAxis(title = list(text = "%")) %>%
        hc_tooltip(pointFormat = "<b>{point.y:,.2f} %</b>") %>%
        hc_title(text = NULL)
    })
    
    # ------------------------------------------------------------
    # 6) Allokation (Letzter Stand): Donut nach form
    # ------------------------------------------------------------
    output$hc_allocation <- renderHighchart({
      df <- ts_data()$by_form
      if (nrow(df) == 0) return(highchart())
      
      last_date <- max(df$datum, na.rm = TRUE)
      alloc <- df %>%
        dplyr::filter(datum == last_date) %>%
        dplyr::group_by(form) %>%
        dplyr::summarise(value = sum(saldo_rendite, na.rm = TRUE), .groups = "drop") %>%
        dplyr::mutate(name = form)
      
      highchart() %>%
        hc_add_series(alloc, "pie", hcaes(name = name, y = value),
                      innerSize = "55%", name = "Allokation") %>%
        hc_tooltip(pointFormat = "<b>{point.y:,.2f} €</b> ({point.percentage:.1f}%)") %>%
        hc_title(text = NULL)
    })
    
    # ------------------------------------------------------------
    # 7) Allokation im Zeitverlauf: Stacked Area (Anteile)
    # ------------------------------------------------------------
    output$hc_allocation_ts <- renderHighchart({
      df <- ts_data()$by_form
      if (nrow(df) == 0) return(highchart())
      
      ts_alloc <- df %>%
        dplyr::group_by(datum, form) %>%
        dplyr::summarise(value = sum(saldo_rendite, na.rm = TRUE), .groups = "drop") %>%
        dplyr::group_by(datum) %>%
        dplyr::mutate(total = sum(value, na.rm = TRUE),
                      pct = dplyr::if_else(total > 0, value / total * 100, NA_real_)) %>%
        dplyr::ungroup()
      
      highchart() %>%
        hc_chart(type = "area") %>%
        hc_plotOptions(area = list(stacking = "normal")) %>%
        hc_xAxis(type = "datetime") %>%
        hc_yAxis(title = list(text = "%"), max = 100) %>%
        hc_add_series(ts_alloc, "area", hcaes(x = datum, y = pct, group = form),
                      name = "Anteil") %>%
        hc_tooltip(shared = TRUE, pointFormat = "<b>{point.y:,.2f}%</b><br/>") %>%
        hc_title(text = NULL)
    })
    
    # ------------------------------------------------------------
    # 8) Monatliche Cashflows: aus manualdata (wie bei dir: Käufe)
    #    -> hier nur grobe Variante auf Basis deiner bisherigen data()-Logik
    # ------------------------------------------------------------
    cashflow_monthly <- reactive({
      # Minimal: nutze deinen bestehenden Ansatz (Käufe aus manualdata)
      df <- data.frame(dplyr::bind_rows(manualdata)$data) %>%
        dplyr::mutate(datum = as.Date(datum)) %>%
        dplyr::select(datum, verwendungszweck, betrag, betrag_edited) %>%
        dplyr::filter(stringr::str_detect(verwendungszweck, "ETFHANDEL|AKTIENHANDEL|CRYPTOHANDEL|SPARBRIEFHANDEL")) %>%
        tidyr::separate_wider_delim(verwendungszweck, delim = "|", names = c("form", "id", "anzahl")) %>%
        dplyr::mutate(form = gsub("HANDEL", "", form)) %>%
        dplyr::filter(betrag == betrag_edited) %>%
        dplyr::mutate(flow = -betrag) %>%  # Käufe als positiver Cashflow (investiert)
        dplyr::mutate(monat = lubridate::floor_date(datum, "month")) %>%
        dplyr::filter(monat >= lubridate::floor_date(input$daterange[1], "month"),
                      monat <= lubridate::floor_date(input$daterange[2], "month")) %>%
        dplyr::group_by(monat) %>%
        dplyr::summarise(flow = sum(flow, na.rm = TRUE), .groups = "drop") %>%
        dplyr::arrange(monat)
      
      df
    })
    
    output$hc_cashflows <- renderHighchart({
      cf <- cashflow_monthly()
      if (nrow(cf) == 0) return(highchart())
      
      highchart() %>%
        hc_chart(type = "column") %>%
        hc_xAxis(categories = format(cf$monat, "%b %Y")) %>%
        hc_yAxis(title = list(text = "€")) %>%
        hc_add_series(name = "Investiert (Monat)", data = round(cf$flow, 2)) %>%
        hc_tooltip(pointFormat = "<b>{point.y:,.2f} €</b>") %>%
        hc_title(text = NULL)
    })
    
    # ------------------------------------------------------------
    # 9) Rendite-Delta (MoM): Δ Rendite in €
    # ------------------------------------------------------------
    output$hc_return_delta <- renderHighchart({
      total <- ts_data()$total
      if (nrow(total) == 0) return(highchart())
      
      mm <- total %>%
        dplyr::mutate(monat = lubridate::floor_date(datum, "month")) %>%
        dplyr::group_by(monat) %>%
        dplyr::summarise(rendite_abs = dplyr::last(rendite_abs), .groups = "drop") %>%
        dplyr::arrange(monat) %>%
        dplyr::mutate(delta = rendite_abs - dplyr::lag(rendite_abs))
      
      highchart() %>%
        hc_chart(type = "column") %>%
        hc_xAxis(categories = format(mm$monat, "%b %Y")) %>%
        hc_yAxis(title = list(text = "Δ Rendite (€)")) %>%
        hc_add_series(name = "Δ Rendite", data = round(mm$delta, 2), colorByPoint = TRUE) %>%
        hc_tooltip(pointFormat = "<b>{point.y:,.2f} €</b>") %>%
        hc_title(text = NULL)
    })
    
  })
}
