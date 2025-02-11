##### CAN-REP Server

server <- function(input, output) {
  
  # Date Inputs
  date1 <- reactive({
    ymd(input$daterange[1])
  })
  date2 <- reactive({
    ymd(input$daterange[2])
  })
  
  # Calendar Min and Max cutoffs
  calendarmin <- reactive({
    req(date1())
    ymd(floor_date(date2(), "month") - months(11))
    })
  
  calendarmax <- reactive({
    req(date2())
    ymd(ceiling_date(date2(), "month") - days(1))
    })
  
  # Calculating number of breaks
  nMonths <- reactive({
    req(date1(), date2())
    nBreaks(date1(), date2())
  })
  
  # Base diagnoses data, filtering by HA
  diagnoses <- reactive({
    req(input$authority)
    diagraw %>%
      filter(HA %in% c(input$authority))
  })
  
  # Base reportability data, filtering by HA
  reportability <- reactive({
    req(input$authority)
    reportraw %>%
      filter(HA %in% c(input$authority))
  })
  
  # Sum of predicted labels grouped by month 
  diag0 <- reactive({
    req(diagnoses())
    
    diagnoses() %>%
      group_by(month = floor_date(fulldate, unit = "month"), final_label) %>%
      filter(month >= date1() & month <= date2()) %>%
      summarise(volume = n()) %>%
      ungroup()
  }) 
  
  # Sum of predicted labels grouped by day
  diag1 <- reactive({
    req(diagnoses())
    
    diagnoses() %>%
      group_by(fulldate, final_label) %>%
      summarise(volume = n()) %>%
      ungroup() %>%
      complete(fulldate = seq(min(fulldate), max(fulldate), by = "day"),
               final_label, fill = list(volume = 0)) %>%
      arrange(fulldate) %>%
      ungroup()
  }) 
  
  # Sum of *one* predicted label grouped by day, 
  diag1filter <- reactive({
    req(diagnoses(), input$series2a)
    
    diag1() %>%
      filter(final_label %in% input$series2a) 
  })
  
  # Sum of predicted labels grouped by day and HA
  diag1HA <- reactive({
    req(diagnoses(), input$series2b)
    
    diagnoses() %>%
      filter(final_label == input$series2b) %>%
      group_by(fulldate, HA) %>%
      summarise(volume = n()) %>%
      ungroup() %>%
      complete(fulldate = seq(min(fulldate), max(fulldate), by = "day"),
               HA, fill = list(volume = 0)) %>%
      arrange(fulldate) %>%
      ungroup()
  }) 
  
  # Sum of non/reportable grouped by month
  report0 <- reactive({
    req(reportability())
    
    reportability() %>%
      mutate(final_label = factor(combined_label, levels = c(0, 1), labels = c("Non-Reportable", "Reportable"))) %>%
      group_by(month = floor_date(fulldate, unit = "month"), final_label) %>% 
      summarise(volume = sum(n())) %>%
      ungroup()
  })
  
  # Sum of non/reportable by day plus totals, as columns
  report1 <- reactive({
    req(reportability())
    
    reportability() %>%
      group_by(fulldate, combined_label) %>%
      summarise(volume = n()) %>%
      ungroup() %>%
      complete(fulldate = seq(min(fulldate), max(fulldate), by = "day"),
               combined_label, fill = list(volume = 0)) %>%
      pivot_wider(names_from = combined_label, values_from = volume) %>%
      rename("Reportable" = 3, "NonReportable" = 2) %>%
      group_by(fulldate) %>%
      mutate(Total = sum(NonReportable, Reportable))
  })
  
  # Sum of non/reportable grouped by day
  report2 <- reactive({
    req(reportability())
    
    reportability() %>%
      mutate(final_label = factor(combined_label, levels = c(0, 1), labels = c("Non-Reportable", "Reportable"))) %>%
      group_by(fulldate, final_label) %>%
      summarise(volume = sum(n()))
  })
  
  # Summarizing the counts for health authorities
  totalHA <- reactive({
    reportraw %>%
      filter(fulldate >= date1() & fulldate <= date2()) %>%
      group_by(HA) %>%
      summarise(total_volume = sum(n()),
                "Percentage of Total" = percent(total_volume/nrow(reportraw), accuracy = 0.1)
                ) %>%
      arrange(desc(total_volume)) %>%
      ungroup()
  })
  
  # Sum of non/reportable grouped by day and HA
  report2HA <- reactive({
    req(reportability(), input$series1a)
    
    reportability() %>%
      mutate(final_label = factor(combined_label, levels = c(0, 1), labels = c("Non-Reportable", "Reportable"))) %>%
      filter(final_label == input$series1a) %>%
      group_by(fulldate, HA, final_label) %>%
      summarise(volume = sum(n())) %>%
      ungroup() %>%
      complete(fulldate = seq(min(fulldate), max(fulldate), by = "day"),
               HA, fill = list(volume = 0)) %>%
      ungroup()
  })
  
  # Sum of predicted labels grouped by day, as columns
  diag5 <- reactive({
    req(diag1())
    
    diag1() %>%
      pivot_wider(names_from = final_label, values_from = volume) %>%
      mutate(wday = str_sub(weekdays(fulldate), 1, 3),
             month_day = day(fulldate),
             month = month(fulldate),
             week_increment = ifelse(month_day == 1 | wday == "Sun", 1, 0)) %>%
      group_by(month) %>% 
      mutate(week = cumsum(week_increment)) %>% 
      ungroup()
  })
  
  # Combined sum columns for both predicted labels and non/reportability
  main <- reactive({
    req(report1())
    
    report1() %>%
      left_join(diag5(), by = "fulldate") %>%
      rename("Non-Reportable" = NonReportable)
  })
  
  # Reformatting calendar data to fit the getStats summary function
  main5 <- reactive({
    req(main(), input$var1)
  
    calendarSummary <- main() %>%
      filter(fulldate >= calendarmin() & fulldate <= calendarmax()) %>%
      mutate(text_month = format(fulldate, "%B %Y"),
             month1 = floor_date(fulldate, unit = "month")) %>%
      select(fulldate, text_month, input$var1) %>%
      rename(final_label = text_month,
             volume = input$var1) 
    
    getSummary <- getStats(calendarmin(), calendarmax(), calendarSummary, "final_label") 
    
    getSummary <- getSummary %>%
      rename(Month = 1) %>%
      mutate(fullmonth = as.Date(paste("01", Month), format = "%d %B %Y")) %>%
      arrange(desc(fullmonth)) %>%
      select(1:5, 8) 
  })
  
  # Regression Dataset, Reportable:NonReportable Ratios grouped by Month
  regress0 <- reactive({
    req(date1(), date2())
    report0() %>%
      filter(month >= date1() & month <= date2()) %>%
      pivot_wider(names_from = final_label, values_from = volume) %>%
      mutate("Ratio" = (.[[3]] / .[[2]])) %>%
      rename(NonReportable = 2)
  })
  
  # Tier 1 Input
  output$sliderInputPanel <- renderUI({
    req(input$tabs)
    if (input$tabs == "Reportability Volumes") {
      radioGroupButtons(
        inputId = "series1",
        label = "Select series:",
        choices = c("Reportability", "Health Authority"),
        selected = "Reportability")
    } else if (input$tabs == "Diagnostic Classifier Volumes") {
      radioGroupButtons(
        inputId = "series2",
        label = "Select series:",
        choices = c("Predicted Label", "Health Authority"),
        selected = "Predicted Label")
    } else if (input$tabs == "Daily Volumes") {
      varSelectInput("var1",
                     "Select the volume indicator:",
                     data = main()[2:23], 
                     selected = "Total"
      )
    } else if (input$tabs == "Predictive Analysis") {
      req(regress0())
      varSelectInput("predictvar",
                     "Select variable: ",
                     data = regress0()[2:4],
                     selected = "Ratio"
      )
    } else {
      return(NULL)
    }
  })
  
  # Tier 2 Input for Reportability (Non/Reportable, HA)
  output$varSelect1 <- renderUI({
    req(input$series1)
    if (input$tabs == "Reportability Volumes" & input$series1 == "Health Authority") {
      radioGroupButtons(
        inputId = "series1a",
        label = "Select volume:",
        choices = c("Non-Reportable", "Reportable")
      )
    } else {
      return(NULL)
    }
  })
  
  # Tier 2 Input for Diagnostic Labels (Label, HA)
  output$varSelect2 <- renderUI({
    req(input$series2)
    if (input$tabs == "Diagnostic Classifier Volumes" & input$series2 == "Predicted Label") {
      pickerInput("series2a",
                  "Select the predicted label(s):",
                  choices = unique(diag1()$final_label),
                  options = list('actions-box' = TRUE),
                  multiple = TRUE,
                  selected = c("BR", "CR", "LU", "PR", "GU"))
    } else if (input$tabs == "Diagnostic Classifier Volumes" & input$series2 == "Health Authority") {
      pickerInput("series2b",
                  "Select the predicted label:",
                  choices = unique(diag1()$final_label),
                  options = list('actions-box' = TRUE),
                  selected = c("BR"))
    } else {
      return(NULL)
    }
  })  
  
  # Tier 2 Input for Predictive tab, how many months to predict
  output$predictToggle <- renderUI({
  req(input$modelSwitch, input$tabs)
    if (input$tabs == "Predictive Analysis" & (input$modelSwitch == "Poisson" || input$modelSwitch == "Ordinary")) {
      sliderTextInput("predictSwitch",
                      "Select number of months to predict:",
                      choices = 0:12,
                      grid = TRUE
      )
    } else {
      NULL
    }
  })
  
  # Tier 2 Input for Predictive tab, switch between Linear Regression
  output$modelToggle <- renderUI({
    if (input$tabs == "Predictive Analysis") {
      radioGroupButtons(
        inputId = "modelSwitch",
        label = "Select regression model:",
        choices = c("None", "Ordinary", "Poisson"),
        selected = "None"
      )
    } else {
      NULL
    }
  })
  
  # Tier 2 Input for whether to show confidence intervals, for regression plot
  output$seToggle <- renderUI({
    if (input$tabs == "Predictive Analysis") {
      materialSwitch("showSE", "Show confidence intervals?", value = FALSE)
    } else {
      NULL
    }
  })
  
  # Flowchart overall summary plot
  output$plot1 <- renderPlot({
    req(totalHA())
    createFlowchart(date1(), date2(), report1(), diag1(), input$authority)
  })
  
  
  # Non/reportability plots
  output$plot2 <- renderPlotly({
    req(input$series1, nMonths())
    if (input$series1 == "Reportability") {
      plotly2 <- report0() %>%
        filter(month >= date1() & month <= date2()) %>%
        group_by(month) %>%
        mutate(month_total = sum(volume),
               percentage = percent((volume/month_total), accuracy = .1)) %>%
        ggplot(mapping = aes(x = month, y = volume, color = final_label, label = percentage)) +
        geom_line() +
        scale_x_date(date_breaks = nMonths(), date_labels = "%b %Y") +
        labs(title = "Reportability Volumes", x = "Date", y = "Volume", color = "Reportability") +
        theme_ipsum() +
        theme(axis.text.x=element_text(angle = 45, vjust = 0.5))
      ggplotly(plotly2, height = 650) 
    } else if (input$series1 == "Health Authority") {
      req(input$series1a)
      plotly2a <- reportability() %>%
        mutate(final_label = factor(combined_label, levels = c(0, 1), labels = c("Non-Reportable", "Reportable"))) %>%
        group_by(month = floor_date(fulldate, unit = "month"), HA, final_label) %>% 
        summarise(volume = sum(n())) %>%
        ungroup() %>%
        filter(month >= date1() & month <= date2() & final_label == input$series1a) %>%
        group_by(month, final_label) %>%
        mutate(month_total = sum(volume),
               percentage = percent((volume/month_total), accuracy = .1)) %>%
        ggplot(mapping = aes(x = month, y = volume, color = HA, label = percentage)) +
        geom_line() +
        scale_x_date(date_breaks = nMonths(), date_labels = "%b %Y") +
        labs(title = str_c(input$series1a, " Volumes"), x = "Date", y = "Volume", color = "Health Authority") +
        theme_ipsum() +
        theme(axis.text.x=element_text(angle = 45, vjust = 0.5))
      ggplotly(plotly2a, height = 650)
    }
  })
  
  # Diagnostic classifier plots
  output$plot3 <- renderPlotly({
    req(input$series2)
    if (input$series2 == "Predicted Label") {
      req(input$series2a)
      plotly3 <- diag0() %>%
        filter(final_label %in% input$series2a) %>%
        group_by(month) %>%
        mutate(month_total = sum(volume),
               percentage = percent((volume/month_total), accuracy = .1)) %>%
        ggplot(mapping = aes(
          x = month, 
          y = volume, 
          color = final_label,
          label = percentage)) +
        geom_line() + 
        labs(title = "Diagnostic Classifier Volumes", x = "Date", y = "Volume", color = "Predicted Label") +
        scale_x_date(date_breaks = nMonths(), date_labels = "%b %Y") +
        theme_ipsum() +
        theme(axis.text.x=element_text(angle = 45, vjust = 0.5))
      ggplotly(plotly3, height = 650)
    } else if (input$series2 == "Health Authority") {
      req(input$series2b)
      plotly3a <- diagnoses() %>%
        group_by(month = floor_date(fulldate, unit = "month"), HA, final_label) %>%
        filter(month >= date1() & month <= date2()) %>%
        summarise(volume = n()) %>%
        ungroup() %>%
        filter(final_label %in% input$series2b) %>%
        group_by(month) %>%
        mutate(month_total = sum(volume),
               percentage = percent((volume/month_total), accuracy = .1)) %>%
        ggplot(mapping = aes(
          x = month, 
          y = volume, 
          color = HA,
          label = percentage)) +
        geom_line() + 
        labs(title = str_c(input$series2b, " Volumes by Health Authority"),
             x = "Date", y = "Volume", color = "Health Authority") +
        scale_x_date(date_breaks = nMonths(), date_labels = "%b %Y") +
        theme_ipsum() +
        theme(axis.text.x=element_text(angle = 45, vjust = 0.5))
      ggplotly(plotly3a, height = 650)
    }
  })
  
  # Calendar app plot
  output$plot4 <- renderPlot({
    
    req(input$tabs, input$var1, diag1())
    
    if (calendarmin() < min(diag1()$fulldate)) {
      calendarmin() <- min(diag1()$fulldate)
    } else {
      NULL
    }
    
    cmaxfull <- format(calendarmax(), "%B %d, %Y")
    
    main1 <- main() %>%
      filter(fulldate <= calendarmax() & fulldate >= calendarmin()) %>%
      mutate(
        wday = factor(wday, levels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")),
        text_month = format(fulldate, "%B %Y"),
        text_month = factor(text_month, levels = sort(unique(text_month)))
      ) %>%
      arrange(text_month) %>%
      group_by(text_month) %>%
      mutate(normalvol = rescale(!!rlang::sym(input$var1))) %>%
      ungroup 
    
    calendar <- ggplot(main1, aes(x = wday, y = week)) + 
      geom_tile(aes(fill = normalvol), color = "black") +
      facet_wrap(~ text_month, scales = "free", strip.position = "top") + 
      scale_y_reverse() + 
      theme_minimal() + 
      scale_x_discrete(position = "top") + 
      ylab("") + xlab("") + labs(fill = "Message Volume \n(by month)") + 
      ggtitle(str_c("Showing daily volumes for 12 months until ", 
                    cmaxfull, 
                    " for indicator '", 
                    toString(input$var1), 
                    "' \n",
                    "Note: Weekly is defined as each cycle from Monday to Sunday")) +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_blank(),
            strip.text.x = element_text(
              size = 16, color = "black", face = "bold"), 
            plot.title = element_text(size = 16),
            strip.background = element_blank(),
            panel.spacing = unit(3, "lines")) +
      geom_text(aes(label = !!input$var1), vjust = 0.5, color = "white", size = 5) +
      scale_fill_gradient(low = "#56B1F7", high = "#132B43", labels = c("Low", "", "Medium", "", "High")) +
      theme(text = element_text(family = "Arial Narrow"))
    calendar
  }, height = 700)
  
  # Regression Plot
  output$plot5 = renderPlotly({
    
    req(regress0(), input$predictvar, input$modelSwitch)
    
    if (input$modelSwitch == "Ordinary") {
      model <- lm(as.formula(paste(input$predictvar, "~ month")), data = regress0())
    } else if (input$modelSwitch == "Poisson") {
      model <- glm(as.formula(paste(input$predictvar, "~ month")),
                   family = poisson(link = "log"),
                   data = regress0())
    } else {
      NULL
    }
    
    if (input$modelSwitch != "None") {
      req(input$predictSwitch)
      future <- data.frame(
        month = seq.Date(from = min(regress0()$month),
                         to = max(regress0()$month) + months(as.numeric(input$predictSwitch)),
                         by = "month"),
        prediction = NA
      )
      future$prediction <- predict(model, newdata = future, type = "response")
    } else {
      NULL
    }

    plotly5 <- regress0() %>%
      ggplot(aes(x = month, y = !!sym(input$predictvar))) +
      geom_point(data = regress0(), aes(x = month, y = !!sym(input$predictvar)), colour = "red") +
      scale_x_date(date_breaks = nMonths(), date_labels = "%b %Y") +
      labs(title = paste(toString(input$predictvar), 
                         "~ Month \n(Ratio = Reportable/Non-Reportable)"),
           x = "Date",
           y = toString(input$predictvar),
           colour = "Series") +
      theme_ipsum() +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5))
    
    if (input$modelSwitch == "None") {
      plotly5 <- plotly5 +
        geom_smooth(se = input$showSE,
                    colour = "red")
    } else if (input$modelSwitch == "Ordinary") {
      plotly5 <- plotly5 +
        geom_smooth(data = future,
         method = "lm",
         aes(y = prediction,
             color = "Predicted"),
         se = input$showSE,
         linetype = "dotted") + 
        geom_point(data = future, aes(x = month, y = prediction), colour = "#40E0D0") +
        geom_line(data = regress0(),
                  aes(color = "Observed"),
                  se = FALSE,
                  linetype = "solid"
        )
    } else if (input$modelSwitch == "Poisson") {
      plotly5 <- plotly5 + 
        geom_smooth(data = future,
                     method = "glm", 
                     method.args = list(family = poisson(link = "log")),
                     aes(y = prediction,
                         color = "Predicted"),
                     se = input$showSE,
                     linetype = "dotted") +
        geom_point(data = future, aes(x = month, y = prediction), colour = "#40E0D0") +
        geom_line(data = regress0(),
                  aes(color = "Observed"),
                  se = FALSE,
                  linetype = "solid"
        )
    } else {
      NULL
    }
    
    ggplotly(plotly5, height = 600)
  })
  
  # Regression line output summary
  output$regressionline <- renderPrint({
    req(input$predictvar, input$modelSwitch)

    poissonmodel <- glm(as.formula(paste(input$predictvar, "~ month")),
                        family = poisson(link = "log"),
                        data = regress0())
    
    linearmodel <- lm(as.formula(paste(input$predictvar, "~ month")), data = regress0())

    if (input$modelSwitch == "None") {
      print("No model selected")
    } else if (input$modelSwitch == "Ordinary") {
      summary(linearmodel)
    } else if (input$modelSwitch == "Poisson") {
      summary(poissonmodel)
    } else {
      NULL
    }
    
  })
  
  # Summary table for total volumes by health authority
  output$table1 = renderDT({
    req(totalHA())
    
    totalHA1 <- totalHA() %>%
      rename("Total Volume" = total_volume) %>%
      rename("Health Authority" = HA)
    
    datatable(
      totalHA1,
      options = list(
        pageLength = 6
      )
    )
  })
  
  # Summary table for reportability
  output$table2 = renderDT({
    req(input$series1)
    if (input$series1 == "Reportability") {
      datatable(
        getStats(date1(), date2(), report2(), "final_label"),
        options = list(
          pageLength = 5
        )
      )
    } else if (input$series1 == "Health Authority") {
      datatable(
        getStats(date1(), date2(), report2HA(), "HA"),
        options = list(
          pageLength = 6
        )
      )
    }
  })
  
  # Summary table for outliers for reportability
  output$table2a = renderDT({
    req(input$series1)
    if (input$series1 == "Reportability") {
      datatable(
        getOutliers(date1(), date2(), report2(), final_label) %>%
          rename("Final Label" = 1),
        options = list(
          pageLength = 10
        )
      )
    } else if (input$series1 == "Health Authority") {
      req(input$series1a)
      datatable(
        getOutliers(date1(), date2(), report2HA(), HA) %>%
          rename("Health Authority" = 1),
        options = list(
          pageLength = 10
        )
      )
    }
  })
  
  # Summary table for diagnostic classifiers
  output$table3 = renderDT({
    req(input$series2)
    if (input$series2 == "Predicted Label") {
      datatable(
        getStats(date1(), date2(), diag1filter(), "final_label"),
        options = list(
          pageLength = 30,
          scrollY = "350px"
        )
      )
    } else if (input$series2 == "Health Authority") {
      req(input$series2b)
      datatable(
        getStats(date1(), date2(), diag1HA(), "HA"),
        options = list(
          pageLength = 6,
          scrollY = "250px"
        )
      )
    }
  })
  
  # Summary table for diagnostic classifiers outliers
  output$table3a = renderDT({
    req(input$series2)
    if (input$series2 == "Predicted Label") {
      datatable(
        getOutliers(date1(), date2(), diag1filter(), final_label) %>%
          rename("Final Label" = 1),
        options = list(
          pageLength = 30,
          scrollY = "350px"
        )
      )
    } else if (input$series2 == "Health Authority") {
      req(input$series2b)
      datatable(
        getOutliers(date1(), date2(), diag1HA(), HA) %>%
          rename("Health Authority" = 1),
        options = list(
          pageLength = 10,
          scrollY = "350px"
        )
      )
    }
  })
  
  # Summary table for calendar visual, divided by months
  output$table4 = renderDT({
    datatable(
      main5(),
      options = list(
        pageLength = 12
      )
    )
  })
  
  # n Outliers for Reportability
  output$numOutliers2 = renderText({
    req(input$series1)
    if (input$series1 == "Reportability") {
      nOutliers <- str_c("Outliers (n = ",
                         nrow(getOutliers(date1(), date2(), report2(), final_label)),
                         ")")
    } else if (input$series1 == "Health Authority") {
      nOutliers <- str_c("Outliers (n = ",
                         nrow(getOutliers(date1(), date2(), report2HA(), HA)),
                         ")")
    }
  })
  
  # n Outliers for Diagnostic Groups
  output$numOutliers3 <- renderText({
    req(input$series2)
    if (input$series2 == "Predicted Label") {
      nOutliers <- str_c("Outliers (n = ",
                         nrow(getOutliers(date1(), date2(), diag1filter(), final_label)),
                         ")")
    } else if (input$series2 == "Health Authority") {
      nOutliers <- str_c("Outliers (n = ",
                         nrow(getOutliers(date1(), date2(), diag1HA(), HA)),
                         ")")
    }
  })
  
  observeEvent(input$readMe, {
    file.show("H:/Data Integrity Analyst/Data Science/dashboard data/volumes/readme.pdf")
  })

}