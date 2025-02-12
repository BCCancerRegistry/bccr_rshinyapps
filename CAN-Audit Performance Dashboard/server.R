#####

server <- function(input, output, session) {

  # Formatted dx accuracy
  dxAccuracyData <- reactive({
    
  req(input$monthView)
  
  accuracyData %>%
    filter(month %in% input$monthView) %>%
    filter(ds_dx_group != "NULL")
  }) 
  
  # overall content
  overallData <- reactive({
    
    req(input$dxNum1, input$metricNum1)
    
    dxMonthData <- accuracyData %>%
      filter(ds_dx_group != "NULL") %>%
      mutate(month = as.Date(paste0(month, "01"), format = "%B %Y%d")
      ) %>%
      select(3, 6, 7)
    
    dxOverall <- dxMonthData %>%
      group_by(month) %>%
      summarise("Overall Tumour Group Accuracy" = confusionMatrix(data = ds_dx_group, reference = tr_dxgroup)$overall[1])
    
    repMonthData <- accuracyData %>%
      select(2, 5, 7) %>%
      mutate(month = as.Date(paste0(month, "01"), format = "%B %Y%d")) %>%
      ungroup()
      
    recallReportable <- repMonthData %>%
      group_by(month) %>%
      summarise("Reportable Recall" = confusionMatrix(data = ds_reportability, reference = tr_reportability)$byClass[1])
    
    recallNonReportable <- repMonthData %>%
      group_by(month) %>%
      summarise("Non-Reportable Recall" = confusionMatrix(data = ds_reportability, reference = tr_reportability)$byClass[2])
    
    dxSummaries <- list()
    
    # custom series 
    for (i in 1:2) {
  
      dxNumInput <- paste0("dxNum", i)
      metricNumInput <- paste0("metricNum", i)
      
      dxIndex <- match(input[[dxNumInput]], dxGroups)
      metricIndex <- match(input[[metricNumInput]], metricGroups)
      
      customSeriesName <- paste("Custom Series", i, "-", input[[dxNumInput]], input[[metricNumInput]])
      
      dxSummaries[[i]] <- dxMonthData %>%
        group_by(month) %>%
        summarise(!!customSeriesName := 
                    confusionMatrix(data = ds_dx_group, reference = tr_dxgroup)$byClass[dxIndex, metricIndex])
    }
    
    combinedData <- dxOverall %>%
      inner_join(recallReportable, by = "month") %>%
      inner_join(recallNonReportable, by = "month")
    
    for (dx in dxSummaries) {
      combinedData <- combinedData %>%
        inner_join(dx, by = "month")
    }
    
    combinedData <- combinedData %>%
      pivot_longer(names_to = "Metric", values_to = "Recall", cols = -month)
  })
  
  output$overallSummary <- renderDT({
    req(overallData())
    
    summaryData <- overallData() %>%
      group_by(Metric) %>%
      summarise("Monthly Mean" = percent(mean(Recall), accuracy = 0.1))
    
    datatable(
      summaryData,
      options = list(
        pageLength = 20
      )
    )
  })
  
  output$overallPlot <- renderPlotly({
    
    req(overallData())
    
    dxMonthPlot <- overallData() %>%
      ggplot(mapping = aes(x = month, y = Recall, color = Metric)) +
      geom_line() +
      geom_point() +
      scale_x_date(date_breaks = "1 month", date_labels = "%B %Y") +
      scale_y_continuous(labels = percent) +
      labs(x = "Month", y = "Percentage") +
      theme_ipsum() +
      theme(axis.title.x = element_text(size = 12),
            axis.text.x = element_text(size = 10, angle = 45),
            axis.title.y = element_text(size = 12)
      )
    
    dxMonthPlotly <- ggplotly(dxMonthPlot, height = 600)
  })
  
  # dx Confusion Matrix
  dxConfusionMatrix <- reactive({
    req(dxAccuracyData())
    confusionMatrix(data = dxAccuracyData()$ds_dx_group,
                    reference = dxAccuracyData()$tr_dxgroup,
                    mode = "prec_recall")
  })
  
  reportAccuracyData <- reactive({
    req(input$monthView)
    
    accuracyData %>%
      filter(month %in% input$monthView) %>%
      select(2, 5)
  })
  
  reportConfusionMatrix <- reactive({
    req(reportAccuracyData())
    confusionMatrix(data = reportAccuracyData()$ds_reportability,
                    reference = reportAccuracyData()$tr_reportability,
                    positive = "Yes")
  })
  
  # reportability confusion matrix, modified slightly to "force" into two-class configuration
  reportCMTwoClass <- reactive({
    req(reportAccuracyData())
    
    reportCMTC <- reportAccuracyData() %>%
      mutate(ds_reportability = factor(ds_reportability, levels = reportFactor),
             tr_reportability = factor(tr_reportability, levels = reportFactor)
             )
    
    confusionMatrix(data = reportCMTC$ds_reportability,
                    reference = reportCMTC$tr_reportability,
                    positive = "Yes")
  })
  
  # dx Confusion  Matrix
  output$ssPlot <- renderPlotly({
    
    req(dxAccuracyData(), dxConfusionMatrix(), reportConfusionMatrix())
    
    if (input$switchSS == FALSE) {
      dxPlotData <- as.data.frame(dxConfusionMatrix()$table)
      dxPlotData <- dxPlotData %>%
        mutate(Reference = factor(dxPlotData$Reference,
                                  levels = rev(levels(dxPlotData$Prediction))),
               Accuracy = ifelse(Prediction == Reference & Freq > 0, "Correct",
                                 ifelse(Prediction != Reference & Freq > 0, "Incorrect", "None"))
        )
      
      cmPlot <- dxPlotData %>%
        ggplot(mapping = aes(y = Reference, x = Prediction, fill = Accuracy)) +
        geom_tile(color = "white", alpha = 0.55) +
        geom_text(aes(label = Freq)) +
        scale_fill_manual(values = c("Correct" = "green", "Incorrect" = "red", "None" = "white")
        ) +
        scale_x_discrete(position = "top") +
        labs(x = "Prediction (DS)", 
             y = "Reference (TR)") +
        theme_ipsum(base_size = 12) +
        theme(panel.grid.major = element_blank(),
              axis.title.x = element_text(size = 12),
              axis.title.y = element_text(size = 12),
              legend.position = "none"
              )
    } else if (input$switchSS == TRUE) {
      reportPlotData <- as.data.frame(reportConfusionMatrix()$table) %>%
        mutate(Accuracy = ifelse(Reference == Prediction, "Correct", "Incorrect"))
      
      reportPlotData$Prediction <- factor(reportPlotData$Prediction,
                                          levels = rev(levels(reportPlotData$Prediction)))
      
      cmPlot <- reportPlotData %>%
        ggplot(mapping = aes(y = Reference, x = Prediction, fill = Accuracy)) +
        geom_tile(color = "white", alpha = 0.55) +
        scale_fill_manual(values = c("Correct" = "green", "Incorrect" = "red")) +
        geom_text(aes(label = Freq), size = 10) + 
        labs(x = "Prediction (DS)", 
             y = "Reference (TR)"
        ) +
        theme_ipsum(base_size = 14) +
        theme(panel.grid.major = element_blank(),
              axis.title.x = element_text(size = 14),
              axis.title.y = element_text(size = 14),
              legend.position = "none"
        )
    }
    
    dxPlotly <- ggplotly(cmPlot, height = 600) %>%
      layout(xaxis = list(side = "top"))
  })
  
  # DX Summary Table
  output$ssTable = renderDT({
    req(dxConfusionMatrix(), reportCMTwoClass())
    
    if (input$switchSS == FALSE) {
      dxTableData <- as.data.frame(dxConfusionMatrix()$byClass)
      ssTable <- dxTableData %>%
        mutate_all(~ round(., 2)) %>%
        rownames_to_column(var = "Class") %>%
        mutate(Class = gsub("Class: ", "", Class)) %>%
        select(1, 6:7, 3, 5, 8:12)
    } else if (input$switchSS == TRUE) {
      ssTable <- as.data.frame(reportCMTwoClass()$byClass) %>%
        filter(!if_any(everything(), ~ . == "Test")) %>%
        mutate_all(~ round(., 2)) %>%
        rownames_to_column(var = "Class") %>%
        mutate(Class = gsub("Class: ", "", Class)) %>%
        select(1, 6:7, 3, 5, 8:12) 
    }
    
    datatable(
      ssTable,
      options = list(
        pageLength = 20
      )
    )
  })

  # Big box DX Stats 
  output$ssAccuracy <- renderText({
    req(dxConfusionMatrix(), reportConfusionMatrix())
    
    if (input$switchSS == FALSE) {
      accuracy <- toString(percent(dxConfusionMatrix()$overall[1], 0.1))
    } else if (input$switchSS == TRUE) {
      accuracy <- toString(percent(reportConfusionMatrix()$overall[1], 0.1))
    }
  })
  
  output$ssConfInt <- renderText({
    req(dxConfusionMatrix(), reportConfusionMatrix())
    
    if (input$switchSS == FALSE) {
      dxConfInt <- paste0("(", 
                          percent(dxConfusionMatrix()$overall[3], 0.1),
                          ", ",
                          percent(dxConfusionMatrix()$overall[4], 0.1),
                          ")"
      )
    } else if (input$switchSS == TRUE) {
      reportConfInt <- paste0("(", 
                              percent(reportConfusionMatrix()$overall[3], 0.1),
                              ", ",
                              percent(reportConfusionMatrix()$overall[4], 0.1),
                              ")"
      )
    }
  })
  
  output$ssDim <- renderText({
    req(dxConfusionMatrix(), reportConfusionMatrix())
    
    if (input$switchSS == FALSE) {
      n <- nrow(dxAccuracyData()) 
    } else if (input$switchSS == TRUE) {
      n <- nrow(reportAccuracyData())
    }
    
  })
  
  #### Surveillance
  
  svlSelectedData <- reactive({
    req(input$dxGroupSelect)
    
    svlData <- get(paste0(input$dxGroupSelect, "svl"))
    
  })
  
  svlCM <- reactive({
    req(input$svlSelect, svlSelectedData())
    
    svlAuditGroup <- paste0(input$svlSelect, "_audit")
    
    svlCM1 <- confusionMatrix(data = svlSelectedData()[[svlAuditGroup]],
                              reference = svlSelectedData()[[input$svlSelect]],
                              mode = "prec_recall")
  })
  
  svlTop <- reactive({
    req(svlSelectedData())
    
    topSVL <- svlSelectedData() %>%
      group_by(!!sym(input$svlSelect)) %>%
      summarise(Freq = n()) %>%
      arrange(desc(Freq))
  })
  
  output$svlCMPlotly <- renderPlotly({
    
    req(svlCM(), svlTop())
    
    svlPlotData <- as.data.frame(svlCM()$table)
    
    #svlPlotData <- svlPlotData %>%
    #  mutate(Reference = factor(svlPlotData$Reference,
    #                            levels = rev(levels(svlPlotData$Reference))),
    #         Accuracy = ifelse((Prediction == Reference & Freq > 0) | (Prediction == 9999 & Freq > 0), "Correct",
    #                           ifelse(Prediction != Reference & Freq > 0, "Incorrect", "None"))
    #    )
    
    svlSeq <- svlTop() 
    
    if (nrow(svlSeq) > 5) {
      svlSeq <- svlTop() %>%
        slice(eval(parse(text = input$svlSlider)))
    }
    
    svlPlotData <- svlPlotData %>%
      mutate(Accuracy = ifelse((Prediction == Reference & Freq > 0) | (Prediction == 9999 & Freq > 0), "Correct",
                               ifelse(Prediction != Reference & Freq > 0, "Incorrect", "None"))
             ) %>%
      filter(!if_any(everything(), ~ . == "Other")) %>%
      filter(Reference %in% svlSeq[[1]]) %>%
      mutate(Prediction = factor(Prediction, levels = levels(Prediction))) %>%
      mutate(Reference = factor(Reference, levels = rev(levels(Prediction))))
    
    svlPlot <- svlPlotData %>%
      ggplot(mapping = aes(y = Reference, x = Prediction, fill = Accuracy)) +
      geom_tile(color = "white", alpha = 0.55) +
      geom_text(aes(label = Freq), size = 8) +
      scale_fill_manual(values = c("Correct" = "green", "Incorrect" = "red", "None" = "white")
      ) +
      scale_x_discrete(position = "top") +
      labs(x = paste(input$dxGroupSelect, input$svlSelect, "Prediction"), 
           y = paste(input$dxGroupSelect, input$svlSelect, "Reference")
           ) +
      theme_ipsum(base_size = 16) +
      theme(panel.grid.major = element_blank(),
            axis.title.x = element_text(size = 16),
            axis.title.y = element_text(size = 16),
            legend.position = "none"
      )
    svlPlotly <- ggplotly(svlPlot, height = 600) %>%
      layout(xaxis = list(side = "top"))
  })
  
  output$svlTable = renderDT({
    req(svlCM(), svlSelectedData())
    
    if (input$dxGroupSelect == "PR" & input$svlSelect == "Histology") {
      
      svlDataPR <- PRsvl %>%
        mutate(Histology = factor(ifelse(Histology == 8140, "8140", "Other")),
               Histology_audit = factor(ifelse(Histology_audit == 8140, "8140", "Other"))
               ) %>%
        mutate(across(c(Histology, Histology_audit), ~ factor(.x, levels = c("8140", "Other", "Test")))
              ) 
        
      PRMatrix <- confusionMatrix(data = svlDataPR$Histology_audit,
                                  reference = svlDataPR$Histology,
                                  mode = "prec_recall",
                                  positive = "8140")
      
      svlTableData <- as.data.frame(PRMatrix$byClass) %>%
        filter(!if_any(everything(), ~ . == "Test"))
      
      svlCMSummary <- PRMatrix
      
    } else {
      svlCMSummary <- svlCM()
      
      svlTableData <- as.data.frame(svlCM()$byClass)
    }
    
    if (ncol(svlTableData) > 2) {
      svlTableData <- svlTableData %>%
        mutate_all(~ round(., 2)) %>%
        rownames_to_column(var = "Class") %>%
        mutate(Class = gsub("Class: ", "", Class)) %>%
        select(1, 6:7, 3, 5, 8:12)
    } else {
      svlTableData <- svlTableData %>%
        rownames_to_column(var = "Metric") %>%
        rename("Value" = 2) %>%
        pivot_wider(names_from = Metric, values_from = Value) %>%
        mutate_all(~ round(., 2)) %>%
        mutate(Class = svlCMSummary[1]) %>%
        select(12, 5:6, 2, 4, 7:11)
    }
     
    datatable(
      svlTableData,
      # svlTop(),
      options = list(
        pageLength = 20
      )
    )
  })
  
  # Big box surveillance Stats
  output$svlAccuracy <- renderText({
    req(svlCM())
    
    accuracy <- toString(percent(svlCM()$overall[1], 0.1))
  })
  
  output$svlConfInt <- renderText({
    req(svlCM())
    svlConfInt <- paste0("(", 
                            percent(svlCM()$overall[3], 0.1),
                            ", ",
                            percent(svlCM()$overall[4], 0.1),
                            ")"
    )
  })
  
  output$svlDim <- renderText({
    req(svlSelectedData())
    
    n <- nrow(svlSelectedData()) 
  })
  
  output$ssOverallTable <- renderDT({
    
    req(input$svlMetricSel)

    metricIndex <- match(input$svlMetricSel, metricGroups)
    
    allSummary <- data.frame()
    
    for (set in SVLSets) {
      
      accSummary <- set %>%
        summarise(
          "Diagnostic Group" = dx_group[1],
          Site = confusionMatrix(data = Site_audit, reference = Site)$overall[metricIndex],
          Behaviour = confusionMatrix(data = Behaviour_audit, reference = Behaviour)$overall[metricIndex],
          Histology = confusionMatrix(data = Histology_audit, reference = Histology)$overall[metricIndex],
          Laterality = confusionMatrix(data = Laterality_audit, reference = Laterality)$overall[metricIndex],
          LVI = confusionMatrix(data = LVI_audit, reference = LVI)$overall[metricIndex]
        ) %>%
        mutate(across(c(Site, Behaviour, Histology, Laterality, LVI), round, digits = 2))
      
      allSummary <- rbind(allSummary, accSummary)
    }
      
    datatable(
      allSummary,
      options = list(
        pageLength = 5
      )
    )
  })
  
  # Turning on row sequencing filter
  output$sequenceOn <- reactive({
    req(svlTop())
    
    nrow(svlTop()) > 5
  })
  outputOptions(output, "sequenceOn", suspendWhenHidden = FALSE)
  
  observe({
    
    req(input$svlMetricSel, input$svlSelect)
    
    N1 <- nrow(svlTop())
    
    if (N1 > 5) {
      updateSelectInput(session, "svlSlider", choices = paste(seq(1, N1, by = 5), seq(5, ceiling(N1 / 5) * 5, by = 5), sep = ":"))
    } else {
      updateSelectInput(session, "svlSlider", choices = NULL)
    }
  })
}