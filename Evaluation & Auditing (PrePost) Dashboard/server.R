##### Surveillance Descriptive Dashboard Server

server <- function(input, output, session) {
  
  # Main Dataset
  selectedCombinedData <- reactive({
    
    req(input$dxSelectComp)
    
    CRPost <- PostCon %>%
      filter(Year %in% input$yearFilter) %>%
      filter(dx_group == input$dxSelectComp) %>%
      select(1, 3:7) %>%
      mutate(type = "Post-Consolidation")
      
    CRPre <- PreCon %>%
      filter(Year %in% input$yearFilter) %>%
      filter(dx_group == input$dxSelectComp) %>%
      select(1, 3:7) %>%
      mutate(type = "Pre-Consolidation") %>%
      mutate(laterality = case_when(
        laterality == "Manual Review" ~ "9",
        TRUE ~ as.character(laterality)
      ))
    
    # further formatting
    if (input$dxSelectComp == "BR") {
      CRPre <- CRPre %>%
        mutate(lvi = recode(lvi,
                            "absent" = "0",
                            "present" = "1",
                            "indeterminate" = "9")
        )
    } else {
      CRPre <- CRPre %>%
        mutate(lvi = as.character(lvi))
    }
    
    freqType <- rbind(CRPre, CRPost)
    
  })
  
# Pre- and Post-Consolidation ---------------------------------------------

  # Note for indicating the table names
  output$prePostText <- renderText({
    text <- "- Pre-consolidation data is from [eMaRCPlus_v60Patch].[dbo].[bccr_surveillance_pipeline]
            (2022-2024) and [eMaRCPlus_v60Patch].[BCCR].[surveillance_pipeline] (2025) <br>
            - Post-consolidation data is from [eMaRCPlus_v60Patch].[dbo].[bccr_consolidation_pipeline] (2022-2024) and
            [eMaRCPLus_v60Patch].[BCCR].[consolidation_pipeline] (2025)"
  })
  
  # Big Box Stats
  output$nrowComp <- renderText({
    
    freqTotal <- selectedCombinedData() %>%
      group_by(type) %>%
      summarise(total = n(), .groups = "drop")
    
    text <- paste0("Pre: ", 
                   toString(freqTotal[2, 2]),
                   ", Post: ",
                   toString(freqTotal[1, 2])
            )
  })
  
  output$SHComp <- renderText({
    
    pre <- selectedCombinedData() %>%
      filter(type == "Pre-Consolidation")

    preSite <- names(which.max(table(pre$site))) 
    
    preHist <- names(which.max(table(pre$histology))) 
    
    post <- selectedCombinedData() %>%
      filter(type == "Post-Consolidation")
    
    postSite <- names(which.max(table(post$site))) 
    
    postHist <- names(which.max(table(post$histology))) 
    
    both <- paste0("Pre: ",
                   preSite,
                   " & ",
                   preHist,
                   ", ",
                   "Post: ",
                   postSite,
                   " & ",
                   postHist)
    
  })
  
  output$percentReduced <- renderText({
    
    freqTotal <- selectedCombinedData() %>%
      group_by(type) %>%
      summarise(total = n(), .groups = "drop")
    
    pre <- as.numeric(freqTotal[2, 2])
    
    post <- as.numeric(freqTotal[1, 2])
    
    difference <- pre - post
    
    percent <- percent((pre - post)/(pre), accuracy = 0.01)
    
    value <- paste0(difference, " (", percent, ")")
    
  })
  
  # Comparative Bar Plots
  
  # SITE
  output$sitePlotComp <- renderPlotly({
    
    req(input$dxSelectComp, selectedCombinedData())
    
    # run function
    plotly <- getPlotly(selectedCombinedData(), "Site", input$chooseView)
    
  })
  
  output$siteText <- renderText({
    if (input$chooseView == "Proportion Consolidated") {
      caption_text <- "Note: 'Proportion Consolidated' refers to the proportion of the original Pre-Consolidation count
      that has been reduced or consolidated into the Post-Consolidation value for each site"
    } else if (input$chooseView == "Proportion") {
      caption_text <- "Note: 'Proportion' refers to the share of each site relative to the total count of either the 
      Pre-Consolidation or Post-Consolidation totals"
    } else {
      caption_text <- ""
    }
    
    caption_text
  })
  
  # HISTOLOGY
  output$histologyPlotComp <- renderPlotly({
    req(input$dxSelectComp, selectedCombinedData())
    
    plotly <- getPlotly(selectedCombinedData(), "Histology", input$chooseView)
    
  })
  
  output$histologyText <- renderText({
    
    if (input$chooseView == "Proportion Consolidated") {
      caption_text <- "Note: 'Proportion Consolidated' refers to the proportion of the original Pre-Consolidation count
      that has been reduced or consolidated into the Post-Consolidation value for each histology site"
    } else if (input$chooseView == "Proportion") {
      caption_text <- "Note: 'Proportion' refers to the share of each histology site relative to the total count of either the 
      Pre-Consolidation or Post-Consolidation totals"
    } else {
      caption_text <- ""
    }
    
    caption_text
  })
  
  # BEHAVIOUR
  output$behaviourPlotComp <- renderPlotly({
    
    req(input$dxSelectComp, selectedCombinedData())
    
    plotly <- getPlotly(selectedCombinedData(), "Behaviour", input$chooseView)
    
  })
  
  output$behaviourText <- renderText({
    
    if (input$chooseView == "Proportion Consolidated") {
      caption_text <- "Note: 'Proportion Consolidated' refers to the proportion of the original Pre-Consolidation count 
      that has been reduced or consolidated into the Post-Consolidation value for each behaviour category"
    } else if (input$chooseView == "Proportion") {
      caption_text <- "Note: 'Proportion' refers to the share of each behaviour category relative to the total count of either the 
      Pre-Consolidation or Post-Consolidation totals"
    } else {
      caption_text <- ""
    }
    
    caption_text
  })
  
  # LATERALITY
  output$lateralityPlotComp <- renderPlotly({
    
    req(input$dxSelectComp, selectedCombinedData())
    
    plotly <- getPlotly(selectedCombinedData(), "Laterality", input$chooseView)
    
  })
  
  output$lateralityText <- renderText({
    
    if (input$chooseView == "Proportion Consolidated") {
      caption_text <- "Note: 'Proportion Consolidated' refers to the proportion of the original Pre-Consolidation count
      that has been reduced or consolidated into the Post-Consolidation value for each laterality category"
    } else if (input$chooseView == "Proportion") {
      caption_text <- "Note: 'Proportion' refers to the share of each laterality category relative to the total count of either the 
      Pre-Consolidation or Post-Consolidation totals"
    } else {
      caption_text <- ""
    }
    
    caption_text
    
  })
  
  # LVI
  output$LVIPlotComp <- renderPlotly({
    
    req(input$dxSelectComp, selectedCombinedData())
    
    plotly <- getPlotly(selectedCombinedData(), "LVI", input$chooseView)
    
  })
  
  output$LVIText <- renderText({
    
    if (input$chooseView == "Proportion Consolidated") {
      caption_text <- "Note: 'Proportion Consolidated' refers to the proportion of the original Pre-Consolidation count
      that has been reduced or consolidated into the Post-Consolidation value for each LVI category"
    } else if (input$chooseView == "Proportion") {
      caption_text <- "Note: 'Proportion' refers to the share of each LVI category relative to the total count of either the 
      Pre-Consolidation or Post-Consolidation totals"
    } else {
      caption_text <- ""
    }
    
    caption_text
    
  })

# Post-Onco Changes ----------------------------------------------
  
  # Core Post-Consolidation Changes Dataset
  evaluationData <- reactive({
    
    req(input$yearFilter, input$dxGroupPCC, input$TFDReviewFlag)
    
    if (length(input$TFDReviewFlag) > 1) {
      review_flag <- c(1, 2)
    } else if (identical(input$TFDReviewFlag, "No")) {
      review_flag <- 2
    } else if (identical(input$TFDReviewFlag, "Yes")) {
      review_flag <- 1
    }

    data <- evalQueryData %>%
      filter(tfa_diagnosis_group_descr == input$dxGroupPCC) %>%
      filter(tfd_review_flag %in% review_flag) %>%
      filter(diagnosis_year %in% input$yearFilter)

  })
  
  output$PCCText <- renderText({

    text <- "Post-Onco Data is from
             H:/Data Integrity Analyst/Data Science/Operations/Evaluation/input/eval_qry_results.csv"

  })
  
  output$proportionTogglePCC <- renderUI({
    
    req(input$viewFormatPCC)
    
    if (input$viewFormatPCC == "Total Bar Graph") {
      selectizeInput(
        "chooseViewPCC",
        label = "Metric to view:",
        choices = c("Count", "Proportion")
      )
    }
    
  })
  
  # SITE
  output$sitePlotPCC <- renderPlotly({
    
    req(evaluationData())
    
    plotly <- summarisePCC(evaluationData(), "Site", input$chooseViewPCC)
      
})
  
  output$siteTablePCC <- renderDT({
    
    req(evaluationData())
    
    plotPCCmatrix("site_code_at_record_load", "final_site_code", "Site", evaluationData())
    
  })
  
  output$sitePCC <- renderUI({
    
    req(input$viewFormatPCC)
    
    if (input$viewFormatPCC == "Total Bar Graph") {
      plotly::plotlyOutput("sitePlotPCC")
    } else if (input$viewFormatPCC == "Table") {
      DT::DTOutput("siteTablePCC")
    }
    
  })
  
  # HISTOLOGY
  output$histologyPlotPCC <- renderPlotly({
    
    req(evaluationData())
    
    plotly <- summarisePCC(evaluationData(), "Histology", input$chooseViewPCC)
    
  })
  
  output$histologyTablePCC <- renderDT({
    
    req(evaluationData())
    
    plotPCCmatrix("hist_code_at_record_load", "final_hist_code", "Histology", evaluationData())
    
  })
  
  output$histologyPCC <- renderUI({
    
    req(input$viewFormatPCC)
    
    if (input$viewFormatPCC == "Total Bar Graph") {
      plotly::plotlyOutput("histologyPlotPCC")
    } else if (input$viewFormatPCC == "Table") {
      DT::DTOutput("histologyTablePCC")
    }
    
  })
  
  # LATERALITY
  output$lateralityPlotPCC <- renderPlotly({
    
    req(evaluationData())
    
    plotly <- summarisePCC(evaluationData(), "Laterality", input$chooseViewPCC)
    
  })
  
  output$lateralityTablePCC <- renderDT({
    
    req(evaluationData())
    
    plotPCCmatrix("lat_at_record_load", "final_laterality", "Laterality", evaluationData())
    
  })
  
  output$lateralityPCC <- renderUI({
    
    req(input$viewFormatPCC)
    
    if (input$viewFormatPCC == "Total Bar Graph") {
      plotly::plotlyOutput("lateralityPlotPCC")
    } else if (input$viewFormatPCC == "Table") {
      DT::DTOutput("lateralityTablePCC")
    }
    
  })
  
  # LVI
  output$LVIPlotPCC <- renderPlotly({
    
    req(evaluationData())

    plotly <- summarisePCC(evaluationData(), "LVI", input$chooseViewPCC)
    
  })
  
  output$LVITablePCC <- renderDT({
    
    req(evaluationData())
    
    plotPCCmatrix("lvi_at_record_load", "final_lvi", "LVI", evaluationData())
    
  })
  
  output$LVIPCC <- renderUI({
    
    req(input$viewFormatPCC)
    
    if (input$viewFormatPCC == "Total Bar Graph") {
      plotly::plotlyOutput("LVIPlotPCC")
    } else if (input$viewFormatPCC == "Table") {
      DT::DTOutput("LVITablePCC")
    }
    
  })

# Review Reasons ----------------------------------------------------------
  
  # For review reasons graphs/calculations
  postConsolidation <- reactive({
    
    req(input$yearFilter, input$excludeOutofDate)

    data <- PostCon %>%
      filter(Year %in% input$yearFilter) 
    
    if (input$excludeOutofDate == "Yes") {
      data <- data %>%
        mutate(review_flag_1 = case_when(review_reason == "Out of date" ~ "N",
                                         TRUE ~ review_flag_1),
               review_reason = case_when(review_reason == "Out of date" ~ NA,
                                         TRUE ~ review_reason)
        )
    }
    
    return(data)
    
  })
  
  # Note for indicating the table names
  output$reviewReasonsText <- renderText({
    text <- "Review flags and review reasons data is from [eMaRCPlus_v60Patch].[dbo].[bccr_consolidation_pipeline]
             (2022-2024) and [eMaRCPLus_v60Patch].[BCCR].[consolidation_pipeline] (2025)"
  })
  
  # Text boxes & text outputs
  output$totalReports <- renderText({
    
    req(postConsolidation())
    
    all <- nrow(postConsolidation())
    
  })
  
  # Number of Total Review Flags
  output$nReview <- renderText({
    
    req(postConsolidation())
    
    positive <- sum(postConsolidation()$review_flag_1 == "Y")
    
  })
  
  # Proportion of Review Flags (of total)
  output$propReview <- renderText({
    
    req(postConsolidation())
    
    numberPositive <-  sum(postConsolidation()$review_flag_1 == "Y")
    
    numberTotal <- nrow(postConsolidation())
    
    percentagePositive <- percent(numberPositive/numberTotal, accuracy = 0.01)
    
    return(percentagePositive)
  
  })
  
  # Review Flag Plot
  output$reviewByDX <- renderPlotly({
    
    req(postConsolidation())
    
    postReview <- postConsolidation() %>%
      group_by(dx_group, review_flag_1) %>%
      summarise(Count = n(), .groups = "drop") %>%
      group_by(dx_group) %>%
      mutate(Proportion = round(Count/sum(Count), digits = 4)) %>%
      filter(review_flag_1 == "Y") %>%
      select(1, 3, 4)

    reviewPlot <- postReview %>%
      ggplot(mapping = aes(x = dx_group, y = !!sym(input$percentToggle))) +
      geom_bar(stat = "identity", position = position_dodge(), fill = "#605ca8", color = "black") +
      theme_ipsum() +
      theme(axis.text.x = element_text(size = 9)
      ) +
      labs(x = "DX Group",
           y = input$percentToggle,
           fill = "Type")
    
    reviewPlotly <- ggplotly(reviewPlot) %>%
      layout(
        margin = list(t = 50, b = 30)
      )
    
  })
  
  # Review Reasons Plot
  output$reviewReasons <- renderPlotly({
    
    req(postConsolidation(), input$reasonDX)
    
    # Standard Grouping for Review Reasons
    reasonSummaryAll <- postConsolidation() %>%
      filter(review_flag_1 == "Y") %>%
      filter(dx_group %in% input$reasonDX) %>%
      group_by(review_reason) %>%
      summarise(Count = n(), .groups = "drop") %>%
      mutate(Proportion = round(Count/sum(Count), digits = 4))
  
    # Grouping Review Reasons, statify by DX Group
    reasonSummary <- postConsolidation() %>%
      filter(review_flag_1 == "Y") %>%
      filter(dx_group %in% input$reasonDX) %>%
      group_by(dx_group, review_reason) %>%
      summarise(Count = n(), .groups = "drop") %>%
      group_by(dx_group) %>%
      mutate(Proportion = round(Count/sum(Count), digits = 4))
    
    # Conditional
    if (input$dxCompareToggle == FALSE) {
      reasonPlot <- reasonSummaryAll %>%
        group_by(review_reason) %>%
        ggplot(mapping = aes(x = review_reason, y = !!sym(input$percentToggle))) +
        geom_bar(stat = "identity", position = position_dodge(), fill = "#8F8EA3", color = "black") +
        theme_ipsum() +
        labs(x = "Reason for Review",
             y = input$percentToggle
        ) +
        theme(axis.text.x = element_text(size = 8, angle = 45))
    } else if (input$dxCompareToggle == TRUE) {
      reasonPlot <- reasonSummary %>%
        ggplot(mapping = aes(x = review_reason, y = !!sym(input$percentToggle), fill = dx_group)) +
        geom_bar(stat = "identity", position = position_dodge(), color = "black") +
        theme_ipsum() +
        labs(x = "Reason for Review",
             y = input$percentToggle,
             fill = "DX Group") +
        theme(axis.text.x = element_text(size = 8, angle = 45))
    }
    
    # Final Reason Plotly
    reasonPlotly <- ggplotly(reasonPlot) %>%
      layout(
        margin = list(t = 50, b = 30)
      )
  })
  
# Audits ------------------------------------------------------------------
  
  output$auditText <- renderText({
    text <- "- Diagnostic group and reportability audit data is from [eMaRCPlus_V90_Audit].[dbo].[bccr_ss_audit_temp]
               (sddbsmarc001) <br>
             - Surveillance data (BR, CR, PR) is read from static .csv files (H:/Data Integrity Analyst)"
  })
  
  # Confusion Matrix (DX Group, SVL, or Reportability)
  matrixMatrix <- reactive({
    
    req(input$matrixView, input$svlTimeFilter)
    
    matrixData <- auditData %>%
      filter(Month %in% input$svlTimeFilter)
    
    if (input$matrixView == "Diagnostic Group") {
      
      matrixFull <- confusionMatrix(data = matrixData$ds_dx_group,
                                    reference = matrixData$tr_dxgroup,
                                    mode = "prec_recall")
      
    } else if (input$matrixView == "Reportability") {
      
      matrixFull <- confusionMatrix(data = matrixData$ds_reportability,
                                    reference = matrixData$tr_reportability,
                                    positive = "Yes")
      
    } else if (input$matrixView == "Surveillance") {
      
      req(input$svlMetric)
      
      surveillanceData <- surveillanceData %>%
        filter(dx_group == input$svlDXGroup) %>%
        mutate(across(c(2:11), ~ levelFactor(.x, cur_column(), "Test")))
      

      # Mapping the reference/prediction variables to the input
      reference <- paste0(str_to_lower(input$svlMetric), "_code_actual")
      
      prediction <- paste0(str_to_lower(input$svlMetric), "_code")
      
      # Manual variable mapping for lat and LVI (due to names not conforming to pattern)
      if (input$svlMetric == "Laterality") {
        reference <- "laterality_actual"
        prediction <- "laterality"
      } else if (input$svlMetric == "LVI") {
        reference <- "lymph_vascular_invasion_actual"
        prediction <- "lymph_vascular_invasion"
      }
      
      # Generating surveillance matrix
      matrixFull <- confusionMatrix(data = surveillanceData[[prediction]],
                                    reference = surveillanceData[[reference]],
                                    mode = "prec_recall")
    }
    
    matrixFull

  })
  
  # Date Filters for Confusion Matrix (when available)
  output$auditTimeFilter <- renderUI({
    
    if (input$matrixView %in% c("Diagnostic Group", "Reportability")) {
      pickerInput(
        "svlTimeFilter",
        label = "Select month(s) to include:",
        choices = unique(auditData$Month),
        multiple = TRUE,
        selected = unique(auditData$Month)
      )
    }
    
  })
  
  # Accuracy Stat
  output$ssAccuracy <- renderText({
    accuracy <- toString(percent(matrixMatrix()$overall[1], 0.1))
  })
  
  # Accuracy Confidence Interval Stat
  output$ssConfInt <- renderText({
    
    reportConfInt <- paste0("(", 
                            percent(matrixMatrix()$overall[3], 0.1),
                            ", ",
                            percent(matrixMatrix()$overall[4], 0.1),
                            ")"
    )
    
  })
  
  # Confusion Number of Obs
  output$ssDim <- renderText({
    
    matrixAsTable <- as.data.frame(matrixMatrix()$table)
    
    sum <- sum(matrixAsTable$Freq)
    
  })
  
  # MAIN - Matrix Plot [geom_tile]
  output$matrixPlot <- renderPlotly({
    
    req(matrixMatrix())
    
    matrixAsTable <- as.data.frame(matrixMatrix()$table) %>%
      mutate(Reference = factor(Reference,  levels = rev(levels(factor(Reference)))),
             Prediction = factor(Prediction),
             Accuracy = case_when(Prediction == Reference & Freq > 0 ~ "Correct",
                                  Prediction == "9999" & Freq > 0 ~ "Correct",
                                  Prediction != Reference & Freq > 0 ~ "Incorrect",
                                  TRUE ~ "None"
             )
      ) %>%
      filter(!if_any(everything(), ~ . %in% c("NA", "NAN"))) 
    
    if (length(unique(matrixAsTable$Prediction)) <= 6) {
      textSize = 18
    } else {
      textSize = 12
    }
    
    plot <- matrixAsTable %>%
      ggplot(mapping = aes(y = Reference, x = Prediction, fill = Accuracy)) +
      geom_tile(color = "white", alpha = 0.55) +
      geom_text(aes(label = Freq), size = (textSize - 7)) +
      scale_fill_manual(values = c("Correct" = "green", "Incorrect" = "red", "None" = "white")
      ) +
      scale_x_discrete(position = "top") +
      labs(x = "Prediction (DS)", 
           y = "Reference (TR)",
           title = paste0(input$matrixView, " Confusion Matrix")) +
      theme_ipsum(base_size = textSize) +
      theme(panel.grid.major = element_blank(),
            axis.title.x = element_text(size = textSize),
            axis.title.y = element_text(size = textSize),
            legend.position = "none"
      )
    
    plotly <- ggplotly(plot) %>%
      layout(margin = list(t = 80, b = 30),
             xaxis = list(side = "top"),
             autosize = TRUE)
    
  })
  
  # Confusion Matrix Summary Table
  output$matrixSummary <- renderDT({
    
    # Standard formatting - matrix output to table output
    summary <- as.data.frame(matrixMatrix()$byClass) 
      # filter(!if_any(everything(), ~ . == "Other"))
      
    # Special formatting for PR Histology - dichotomization
    if (input$matrixView == "Surveillance") {
      if (input$svlDXGroup == "PR" & input$svlMetric == "Histology") {

        svlDataPR <- surveillanceData %>%
          mutate(histology_code_actual = factor(ifelse(histology_code_actual == 8140, "8140", "Other")),
                 histology_code = factor(ifelse(histology_code == 8140, "8140", "Other"))
          ) %>%
          mutate(across(c(histology_code, histology_code_actual), ~ factor(.x, levels = c("8140", "Other", "Test")))
          )

        PRMatrix <- confusionMatrix(data = svlDataPR$histology_code,
                                    reference = svlDataPR$histology_code_actual,
                                    mode = "prec_recall",
                                    positive = "8140")

        summary <- as.data.frame(PRMatrix$byClass) %>%
          filter(!if_any(everything(), ~ . == "Test"))

      }
    }
  
    # Formatting Table
    svlTableData <- summary %>%
      mutate_all(~ round(., 2)) %>%
      rownames_to_column(var = "Class") %>%
      mutate(Class = gsub("Class: ", "", Class)) %>%
      filter(!if_any(everything(), ~ . %in% c("NA", "NAN"))) %>% # Filtering out dummy values
      select(1, 6:7, 3, 5, 8:12)
    
    # Generating DT Output
    datatable(
      svlTableData,
      options = list(
        pageLength = 20
      )
    )
  })
  
  # Surveillance Metric UI Switch
  output$surveillanceMetric <- renderUI({
    
    req(input$matrixView)
    
    if (input$matrixView == "Surveillance") {
      tagList(
        pickerInput(
          "svlDXGroup",
          label = "Select Diagnostic Group:",
          choices = c("BR", "CR", "PR"),
          multiple = FALSE,
          selected = "PR"
        ),
        pickerInput(
          "svlMetric",
          label = "Select Surveillance Metric:",
          choices = c("Site", "Histology", "Behaviour", "Laterality", "LVI"),
          multiple = FALSE,
          selected = "Site"
        )
      )
      
    }
  
  })
  
}