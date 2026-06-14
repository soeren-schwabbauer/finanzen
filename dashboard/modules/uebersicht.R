# verwendet wird das kontosaldo, bzw. der gegenwärtige depotwert.
if(FALSE) {
  input <- list(
    grouping_var = "konto", #bank-insgesammt-display_name,
    display_values = "absolut", #prozent
    subset_var = c("DEPOT", "GIRO", "EXTRAKONTO", "WALLET"),
    filter_daterange = c("2024-11-01", as.character(Sys.Date())),
    incl_inflation = FALSE
  )
}

uebersichtUI <- function(id, finanzkonto, bank_konto) {
  ns <- NS(id)
  
  tagList(
    
    navset_card_underline(
      full_screen = TRUE,
      
      nav_panel(
        title = "Übersicht",
        card_body(
          highchartOutput(ns("uebersicht_plot"))
        )
      ),
      
      # Vermögen (Level)
      nav_panel(
        title = "Vermögen",
        card_body(
          highchartOutput(ns("vermoegen_plot"))
        )
      ),
      
      # NEU: % Veränderung als Zeitreihe (1/6/12 Monate)
      nav_panel(
        title = "% Veränderung",
        card_body(
          highchartOutput(ns("delta_plot"))
        )
      ),
      
      # NEU: Absolute Veränderung als Zeitreihe (1/6/12 Monate)
      nav_panel(
        title = "Absolute Veränderung",
        card_body(
          highchartOutput(ns("delta_abs_plot"))
        )
      ),
      
      # NEU: Drawdown als Zeitreihe (in %)
      nav_panel(
        title = "Drawdown",
        card_body(
          highchartOutput(ns("drawdown_plot"))
        )
      ),
      
      nav_panel(
        title = "",
        icon = bsicons::bs_icon("gear"),
        
        card_body(
          fluidRow(
            column(
              3,
              dateRangeInput(
                ns("filter_daterange"),
                "Zeitraum auswählen",
                start = "2023-01-01",
                end = Sys.Date(),
                min = "2020-01-01",
                max = Sys.Date()
              )
            ),
            column(
              2,
              radioButtons(
                ns("grouping_var"),
                "Gruppieren nach",
                choices = c(
                  "Insgesammt" = "insgesammt",
                  "Kontoart" = "konto",
                  "Bank" = "bank",
                  "Konto" = "display_name"
                ),
                selected = "konto",
                inline = FALSE
              )
            ),
            column(
              4,
              selectInput(
                ns("subset_var"),
                label = NULL,
                choices = NULL,
                multiple = TRUE
              )
            ),
            column(
              2,
              radioButtons(
                ns("display_values"),
                "Werte in",
                choices = c(
                  "Absolut (€)" = "absolut",
                  "Prozent (%)" = "prozent"
                ),
                selected = "absolut",
                inline = FALSE
              )
            ),
            column(
              1,
              actionButton(ns("refresh_data"), label = "", icon = icon("repeat"))
            )
          )
        )
      )
    )
  )
}

uebersichtServer <- function(id, MANUALDATA, PORTFOLIODATA, INFLATIONDATA, bank_konto) {
  moduleServer(id, function(input, output, session) {
    
    # -------------------------------------------------------------------------
    # Helpers
    # -------------------------------------------------------------------------
    calc_drawdown_pct <- function(x) {
      peak <- cummax(x)
      dd <- (x / peak - 1) * 100
      dd[is.infinite(dd)] <- NA_real_
      dd
    }
    
    # % Veränderung ggü. (datum %m-% months(k)) mit "letzter <= target_date" Logik
    pct_change_series_vs_months <- function(dates, values, months_back) {
      target_dates <- dates %m-% months(months_back)
      idx <- findInterval(target_dates, dates) # Index des letzten dates <= target_date
      out <- rep(NA_real_, length(values))
      ok <- idx > 0 & !is.na(values) & !is.na(values[idx]) & values[idx] != 0
      out[ok] <- (values[ok] / values[idx[ok]] - 1) * 100
      out
    }
    
    # Absolute Veränderung ggü. (datum %m-% months(k)) mit "letzter <= target_date" Logik
    abs_change_series_vs_months <- function(dates, values, months_back) {
      target_dates <- dates %m-% months(months_back)
      idx <- findInterval(target_dates, dates) # Index des letzten dates <= target_date
      out <- rep(NA_real_, length(values))
      ok <- idx > 0 & !is.na(values) & !is.na(values[idx])
      out[ok] <- values[ok] - values[idx[ok]]
      out
    }
    
    # -------------------------------------------------------------------------
    # Übersicht timeseries (gruppiert)
    # -------------------------------------------------------------------------
    data_fullts <- reactive({
      
      # MANUALDATA to GRID ===================================================
      manualdata_unlist <- bind_rows(MANUALDATA)
      
      basekonten <- data.frame(
        display_name = manualdata_unlist$display_name,
        manualdata_unlist$data
      )
      
      data_basekonten <- basekonten %>%
        select(datum, display_name, saldo) %>%
        mutate(datum = as.Date(datum)) %>%
        # doppelte salden vermeiden
        group_by(datum, display_name) %>%
        slice(1) %>% #-> erstes saldo behalten (das letzte eingetragene saldo des tages)
        ungroup() %>%
        arrange(display_name, datum)
      
      grid_basekonten <-
        expand.grid(
          display_name = unique(data_basekonten$display_name),
          datum = seq.Date(from = as.Date("2020-01-01"), to = Sys.Date(), by = "day")
        ) %>%
        arrange(display_name, datum) %>%
        left_join(data_basekonten, by = c("display_name", "datum")) %>%
        group_by(display_name) %>%
        fill(saldo, .direction = "down") %>%
        ungroup()
      
      # PORTFOLIOS to GRID ===================================================
      portfolio_unlist <- bind_rows(PORTFOLIODATA)
      
      portfolio <- data.frame(
        display_name = portfolio_unlist$display_name,
        portfolio_unlist$data
      )
      
      # Rendite-Option entfernt -> für die Übersicht immer "mit Rendite" (saldo_rendite)
      data_portfolio <- portfolio %>%
        mutate(saldo = saldo_rendite)
      
      grid_portfolio <-
        expand.grid(
          display_name = unique(data_portfolio$display_name),
          datum = seq.Date(from = as.Date("2020-01-01"), to = Sys.Date(), by = "day")
        ) %>%
        arrange(display_name, datum) %>%
        left_join(data_portfolio, by = c("display_name", "datum"))
      
      # alle konten verbinden ==================================================
      grid_full <- bind_rows(grid_basekonten, grid_portfolio) %>%
        mutate(saldo = ifelse(is.na(saldo), 0, saldo))
      
      # datum filtern
      full <- grid_full %>%
        filter(
          datum >= input$filter_daterange[1],
          datum <= input$filter_daterange[2]
        )
      
      # kontosaldo nach groupingvariable aufsummieren ------------------------
      if (input$grouping_var == "insgesammt") {
        full <- full %>%
          group_by(datum) %>%
          summarise(saldo = sum(saldo), .groups = "drop") %>%
          mutate(grouped_var = "Total")
      } else {
        full <- full %>%
          mutate(
            konto = gsub(".* - ", "", display_name),
            bank  = gsub(" - .*", "", display_name)
          ) %>%
          group_by(datum, grouped_var = !!sym(input$grouping_var)) %>%
          summarise(saldo = sum(saldo), .groups = "drop")
      }
      
      # inflation bereinigen ===================================================
      if (isTRUE(input$incl_inflation)) {
        
        full <- full %>% mutate(monat = floor_date(datum, unit = "month"))
        
        # 2. VPI-Basis definieren (z.B. erster Monat im gefilterten Zeitraum)
        basis_datum <- min(full$monat)
        vpi_basis <- INFLATIONDATA %>% filter(datum == basis_datum) %>% pull(preis)
        
        # 3. Mergen mit monatlichen VPI-Daten
        full <- full %>%
          left_join(INFLATIONDATA, by = c("monat" = "datum")) %>%
          tidyr::fill(preis, .direction = "down")
        
        # 4. Realsaldo berechnen
        full <- full %>% mutate(saldo = saldo * (vpi_basis / preis))
      }
      
      full
    })
    
    # -------------------------------------------------------------------------
    # Gesamtes Vermögen mit und ohne Rendite (immer "insgesammt")
    # -------------------------------------------------------------------------
    data_vermoegen <- reactive({
      
      manualdata_unlist <- bind_rows(MANUALDATA)
      
      basekonten <- data.frame(
        display_name = manualdata_unlist$display_name,
        manualdata_unlist$data
      )
      
      data_basekonten <- basekonten %>%
        select(datum, display_name, saldo) %>%
        mutate(datum = as.Date(datum)) %>%
        group_by(datum, display_name) %>%
        slice(1) %>%
        ungroup() %>%
        arrange(display_name, datum)
      
      grid_basekonten <-
        expand.grid(
          display_name = unique(data_basekonten$display_name),
          datum = seq.Date(from = as.Date("2020-01-01"), to = Sys.Date(), by = "day")
        ) %>%
        arrange(display_name, datum) %>%
        left_join(data_basekonten, by = c("display_name", "datum")) %>%
        group_by(display_name) %>%
        fill(saldo, .direction = "down") %>%
        ungroup() %>%
        mutate(saldo = ifelse(is.na(saldo), 0, saldo))
      
      manual_total <- grid_basekonten %>%
        group_by(datum) %>%
        summarise(saldo_manual = sum(saldo), .groups = "drop")
      
      
      portfolio_unlist <- bind_rows(PORTFOLIODATA)
      
      portfolio <- data.frame(
        display_name = portfolio_unlist$display_name,
        portfolio_unlist$data
      )
      
      data_portfolio_both <- portfolio %>%
        mutate(datum = as.Date(datum))
      
      grid_portfolio <-
        expand.grid(
          display_name = unique(data_portfolio_both$display_name),
          datum = seq.Date(from = as.Date("2020-01-01"), to = Sys.Date(), by = "day")
        ) %>%
        arrange(display_name, datum) %>%
        left_join(data_portfolio_both, by = c("display_name", "datum")) %>%
        mutate(
          saldo_raw = ifelse(is.na(saldo_raw), 0, saldo_raw),
          saldo_rendite = ifelse(is.na(saldo_rendite), 0, saldo_rendite)
        )
      
      portfolio_total <- grid_portfolio %>%
        group_by(datum) %>%
        summarise(
          depot_raw = sum(saldo_raw),
          depot_rendite = sum(saldo_rendite),
          .groups = "drop"
        )
      
      total <- manual_total %>%
        full_join(portfolio_total, by = "datum") %>%
        mutate(
          saldo_manual = ifelse(is.na(saldo_manual), 0, saldo_manual),
          depot_raw = ifelse(is.na(depot_raw), 0, depot_raw),
          depot_rendite = ifelse(is.na(depot_rendite), 0, depot_rendite),
          
          ohne_rendite_ohne_inflation = saldo_manual + depot_raw,
          mit_rendite_ohne_inflation  = saldo_manual + depot_rendite
        ) %>%
        filter(
          datum >= input$filter_daterange[1],
          datum <= input$filter_daterange[2]
        )
      
      total <- total %>%
        mutate(monat = floor_date(datum, unit = "month"))
      
      basis_datum <- min(total$monat)
      vpi_basis <- INFLATIONDATA %>%
        filter(datum == basis_datum) %>%
        pull(preis)
      
      total <- total %>%
        left_join(INFLATIONDATA, by = c("monat" = "datum")) %>%
        tidyr::fill(preis, .direction = "down") %>%
        mutate(
          ohne_rendite_mit_inflation = ohne_rendite_ohne_inflation * (vpi_basis / preis),
          mit_rendite_mit_inflation  = mit_rendite_ohne_inflation  * (vpi_basis / preis)
        ) %>%
        select(
          datum,
          mit_rendite_mit_inflation,
          mit_rendite_ohne_inflation,
          ohne_rendite_mit_inflation,
          ohne_rendite_ohne_inflation
        )
      
      total
    })
    
    # -------------------------------------------------------------------------
    # NEU: % Veränderung (1/6/12 Monate) als Zeitreihe
    # Basis: "vermoegen_mit_rendite" (du kannst hier leicht auf "ohne" wechseln)
    # -------------------------------------------------------------------------
    data_vermoegen_delta <- reactive({
      df <- data_vermoegen() %>%
        arrange(datum)
      
      if (nrow(df) == 0) return(df %>% mutate(delta_1m = NA_real_, delta_6m = NA_real_, delta_12m = NA_real_))
      
      dates <- df$datum
      vals <- df$mit_rendite_ohne_inflation
      
      df %>%
        mutate(
          delta_1m  = pct_change_series_vs_months(dates, vals, 1),
          delta_6m  = pct_change_series_vs_months(dates, vals, 6),
          delta_12m = pct_change_series_vs_months(dates, vals, 12)
        )
    })
    
    # -------------------------------------------------------------------------
    # Observing changes in data_fullts(), but preserving user selection
    # -------------------------------------------------------------------------
    observe({
      new_options <- data_fullts() %>% pull(grouped_var) %>% unique()
      
      old_selection <- isolate(input$subset_var)
      valid_selection <- intersect(old_selection, new_options)
      
      if (length(valid_selection) == 0) {
        valid_selection <- new_options
      }
      
      updateSelectInput(
        session = session,
        inputId = "subset_var",
        label = paste0(input$grouping_var, " filtern"),
        choices = new_options,
        selected = valid_selection
      )
    })
    
    # -------------------------------------------------------------------------
    # NEU: Absolute Veränderung (1/6/12 Monate) als Zeitreihe
    # Basis: "vermoegen_mit_rendite"
    # -------------------------------------------------------------------------
    data_vermoegen_delta_abs <- reactive({
      df <- data_vermoegen() %>%
        arrange(datum)
      
      if (nrow(df) == 0) {
        return(df %>%
                 mutate(
                   delta_abs_1m = NA_real_,
                   delta_abs_6m = NA_real_,
                   delta_abs_12m = NA_real_
                 ))
      }
      
      dates <- df$datum
      vals <- df$mit_rendite_ohne_inflation
      
      df %>%
        mutate(
          delta_abs_1m  = abs_change_series_vs_months(dates, vals, 1),
          delta_abs_6m  = abs_change_series_vs_months(dates, vals, 6),
          delta_abs_12m = abs_change_series_vs_months(dates, vals, 12)
        )
    })
    
    # -------------------------------------------------------------------------
    # Übersicht Plot
    # -------------------------------------------------------------------------
    output$uebersicht_plot <- renderHighchart({
      
      full <- data_fullts() %>%
        filter(grouped_var %in% input$subset_var) %>%
        mutate(
          datum_TS = datetime_to_timestamp(as.POSIXct(datum))
        )
      
      # Prozent pro Tag berechnen
      if (input$display_values == "prozent") {
        full <- full %>%
          group_by(datum_TS) %>%
          mutate(total = sum(saldo)) %>%
          ungroup() %>%
          mutate(saldo = round(saldo / total, 4) * 100)
        sign_tooltip <- "%"
      } else {
        sign_tooltip <- "€"
      }
      
      # Total sum für schwarze Linie
      sum_data <- full %>%
        group_by(datum_TS) %>%
        summarize(saldo_sum = sum(saldo), .groups = "drop")
      
      highchart() %>%
        hc_chart(type = "area") %>%
        hc_xAxis(type = "datetime", title = list(text = "Datum")) %>%
        hc_add_series_list(
          full %>%
            group_by(grouped_var) %>%
            summarize(
              data = list(map2(datum_TS, saldo, ~ list(x = .x, y = .y))),
              .groups = "drop"
            ) %>%
            mutate(series = map2(
              grouped_var, data,
              ~ list(
                name = .x,
                data = .y,
                type = "area"
              )
            )) %>%
            pull(series)
        ) %>%
        hc_add_series(
          name = "Total Sum",
          data = map2(sum_data$datum_TS, sum_data$saldo_sum, ~ list(x = .x, y = .y)),
          type = "line",
          color = "black"
        ) %>%
        hc_plotOptions(area = list(stacking = "normal")) %>%
        hc_tooltip(
          shared = TRUE,
          formatter = JS(
            paste0(
              "function() {
                 var points = this.points || [];
                 var totalSum = points.find(p => p.series.name === 'Total Sum')?.y || 0;
                 var tooltip = '<b>' + Highcharts.dateFormat('%b %e, %Y', this.x) +
                   ': ' + Highcharts.numberFormat(totalSum, 0, ',', '.') + '",
              sign_tooltip,
              "</b><br>';
                 points = points.sort(function(a, b) { return b.y - a.y; });
                 points.forEach(function(point) {
                   if (point.series.name !== 'Total Sum') {
                     tooltip += '<span style=\"color:' + point.color + '\">●</span> ' +
                                point.series.name + ': ' +
                                Highcharts.numberFormat(point.y, 0, ',', '.') + '",
              sign_tooltip,
              "<br>';
                   }
                 });
                 return tooltip;
               }"
            )
          )
        ) %>%
        hc_title(text = "") %>%
        hc_yAxis(
          title = list(
            text = if (input$display_values == "prozent") "Anteil (%)" else "Saldo (€)"
          )
        )
    })
    
    # -------------------------------------------------------------------------
    # Vermögen Plot (mit/ohne Rendite)
    # -------------------------------------------------------------------------
    output$vermoegen_plot <- renderHighchart({
      
      df <- data_vermoegen() %>%
        mutate(datum_TS = datetime_to_timestamp(as.POSIXct(datum)))
      
      s1 <- map2(df$datum_TS, df$mit_rendite_mit_inflation, ~ list(x = .x, y = .y))
      s2 <- map2(df$datum_TS, df$mit_rendite_ohne_inflation, ~ list(x = .x, y = .y))
      s3 <- map2(df$datum_TS, df$ohne_rendite_mit_inflation, ~ list(x = .x, y = .y))
      s4 <- map2(df$datum_TS, df$ohne_rendite_ohne_inflation, ~ list(x = .x, y = .y))
      
      highchart() %>%
        hc_chart(type = "line") %>%
        hc_xAxis(type = "datetime", title = list(text = "Datum")) %>%
        hc_add_series(name = "Mit Rendite – mit Inflation", data = s1, type = "line") %>%
        hc_add_series(name = "Mit Rendite – ohne Inflation", data = s2, type = "line") %>%
        hc_add_series(name = "Ohne Rendite – mit Inflation", data = s3, type = "line") %>%
        hc_add_series(name = "Ohne Rendite – ohne Inflation", data = s4, type = "line") %>%
        hc_tooltip(
          shared = TRUE,
          formatter = JS(
            "function() {
           var points = this.points || [];
           var tooltip = '<b>' + Highcharts.dateFormat('%b %e, %Y', this.x) + '</b><br>';
           points = points.sort(function(a, b) { return b.y - a.y; });
           points.forEach(function(point) {
             tooltip += '<span style=\"color:' + point.color + '\">●</span> ' +
                        point.series.name + ': ' +
                        Highcharts.numberFormat(point.y, 0, ',', '.') + ' €<br>';
           });
           return tooltip;
         }"
          )
        ) %>%
        hc_title(text = "") %>%
        hc_yAxis(title = list(text = "Vermögen (€)"), min = 0)
    })
    
    # -------------------------------------------------------------------------
    # NEU: % Veränderung Plot (1/6/12 Monate) als Zeitreihe
    # -------------------------------------------------------------------------
    output$delta_plot <- renderHighchart({
      
      df <- data_vermoegen_delta() %>%
        mutate(datum_TS = datetime_to_timestamp(as.POSIXct(datum)))
      
      s1  <- map2(df$datum_TS, df$delta_1m,  ~ list(x = .x, y = .y))
      s6  <- map2(df$datum_TS, df$delta_6m,  ~ list(x = .x, y = .y))
      s12 <- map2(df$datum_TS, df$delta_12m, ~ list(x = .x, y = .y))
      
      highchart() %>%
        hc_chart(type = "line") %>%
        hc_xAxis(type = "datetime", title = list(text = "Datum")) %>%
        hc_add_series(name = "Δ vs. 1 Monat",  data = s1,  type = "line") %>%
        hc_add_series(name = "Δ vs. 6 Monate", data = s6,  type = "line") %>%
        hc_add_series(name = "Δ vs. 12 Monate",data = s12, type = "line") %>%
        hc_tooltip(
          shared = TRUE,
          formatter = JS(
            "function() {
               var points = this.points || [];
               var tooltip = '<b>' + Highcharts.dateFormat('%b %e, %Y', this.x) + '</b><br>';
               points = points.sort(function(a, b) { return b.y - a.y; });
               points.forEach(function(point) {
                 if (point.y !== null && point.y !== undefined) {
                   tooltip += '<span style=\"color:' + point.color + '\">●</span> ' +
                              point.series.name + ': ' +
                              Highcharts.numberFormat(point.y, 2, ',', '.') + ' %<br>';
                 }
               });
               return tooltip;
             }"
          )
        ) %>%
        hc_title(text = "") %>%
        hc_yAxis(title = list(text = "Veränderung (%)"))
    })
    
    # -------------------------------------------------------------------------
    # NEU: Absolute Veränderung Plot (1/6/12 Monate) als Zeitreihe
    # -------------------------------------------------------------------------
    output$delta_abs_plot <- renderHighchart({
      
      df <- data_vermoegen_delta_abs() %>%
        mutate(datum_TS = datetime_to_timestamp(as.POSIXct(datum)))
      
      s1  <- map2(df$datum_TS, df$delta_abs_1m,  ~ list(x = .x, y = .y))
      s6  <- map2(df$datum_TS, df$delta_abs_6m,  ~ list(x = .x, y = .y))
      s12 <- map2(df$datum_TS, df$delta_abs_12m, ~ list(x = .x, y = .y))
      
      highchart() %>%
        hc_chart(type = "line") %>%
        hc_xAxis(type = "datetime", title = list(text = "Datum")) %>%
        hc_add_series(name = "Δ absolut vs. 1 Monat",  data = s1,  type = "line") %>%
        hc_add_series(name = "Δ absolut vs. 6 Monate", data = s6,  type = "line") %>%
        hc_add_series(name = "Δ absolut vs. 12 Monate", data = s12, type = "line") %>%
        hc_tooltip(
          shared = TRUE,
          formatter = JS(
            "function() {
               var points = this.points || [];
               var tooltip = '<b>' + Highcharts.dateFormat('%b %e, %Y', this.x) + '</b><br>';
               points = points.sort(function(a, b) { return b.y - a.y; });
               points.forEach(function(point) {
                 if (point.y !== null && point.y !== undefined) {
                   tooltip += '<span style=\"color:' + point.color + '\">●</span> ' +
                              point.series.name + ': ' +
                              Highcharts.numberFormat(point.y, 0, ',', '.') + ' €<br>';
                 }
               });
               return tooltip;
             }"
          )
        ) %>%
        hc_title(text = "") %>%
        hc_yAxis(title = list(text = "Veränderung (€)"))
    })
    
    # -------------------------------------------------------------------------
    # NEU: Drawdown Plot (in %) für mit/ohne Rendite
    # -------------------------------------------------------------------------
    output$drawdown_plot <- renderHighchart({
      
      df <- data_vermoegen() %>%
        arrange(datum) %>%
        mutate(
          dd_mit  = calc_drawdown_pct(mit_rendite_mit_inflation),
          dd_ohne = calc_drawdown_pct(ohne_rendite_mit_inflation),
          datum_TS = datetime_to_timestamp(as.POSIXct(datum))
        )
      
      series_dd_mit  <- map2(df$datum_TS, df$dd_mit,  ~ list(x = .x, y = .y))
      series_dd_ohne <- map2(df$datum_TS, df$dd_ohne, ~ list(x = .x, y = .y))
      
      highchart() %>%
        hc_chart(type = "area") %>%
        hc_xAxis(type = "datetime", title = list(text = "Datum")) %>%
        hc_yAxis(
          title = list(text = "Drawdown (%)"),
          max = 0
        ) %>%
        hc_add_series(name = "Drawdown (Mit Rendite)",  data = series_dd_mit,  type = "area") %>%
        hc_add_series(name = "Drawdown (Ohne Rendite)", data = series_dd_ohne, type = "area") %>%
        hc_tooltip(
          shared = TRUE,
          formatter = JS(
            "function() {
               var points = this.points || [];
               var tooltip = '<b>' + Highcharts.dateFormat('%b %e, %Y', this.x) + '</b><br>';
               points = points.sort(function(a, b) { return b.y - a.y; });
               points.forEach(function(point) {
                 if (point.y !== null && point.y !== undefined) {
                   tooltip += '<span style=\"color:' + point.color + '\">●</span> ' +
                              point.series.name + ': ' +
                              Highcharts.numberFormat(point.y, 2, ',', '.') + ' %<br>';
                 }
               });
               return tooltip;
             }"
          )
        ) %>%
        hc_title(text = "")
    })
    
  })
}
