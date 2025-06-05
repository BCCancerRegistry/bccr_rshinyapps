# Imaging Dashboard Server ------------------------------------------------

server <- function(input, output, session) {

  # Global items ------------------------------------------------------------
  # Combined result + patient data
  mainData <- reactive({
    
    req(input$XdateMetric)
    
    req(input$dateRange[2] >= input$dateRange[1])
    
    col_map <- c(
      "Observation Date" = 2,
      "Results Report Date" = 3,
      "Message Date" = 11
    )
    
    set <- resultData %>%
      full_join(messageData, by = "MessageId") %>%
      full_join(patientData, by = "MessageId")
    
    set1 <- set %>%
      rename(date = !!colnames(set)[col_map[[input$XdateMetric]]])
    
    set2 <- set1 %>%
      mutate(Week = floor_date(date, unit = "week", week_start = 1)) %>%
      mutate(Month = floor_date(date, unit = "month")) %>%
      filter(between(date, input$dateRange[1], input$dateRange[2])) %>%
      filter(ChannelName %in% input$authorityFilt)
      # filter(ExamTypeStd %in% c("CT", "MG", "MR", "NM", "PET", "US"))
    
  })
  
  # Global Date Switch 
  output$globalDateSwitch <- renderUI({
    
    req(input$XdateMetric)
    
    startDate <- ifelse(input$XdateMetric == "Message Date",
                        ymd("2024-10-01"),
                        ymd("2020-01-01")
                        )
    
    dateRangeInput(
      "dateRange",
      label = "Filter by Date:",
      min = startDate,
      max = today(),
      start = ymd("2025-04-01"),
      end = today()
    )
    
  })
  
  # Tab 1 - Time Series -----------------------------------------------------

  # Time series grouping switch - dynamic, by day if date range within one month, and by week/month if more than that
  output$daySwitch <- renderUI({
    
    req(input$dateRange)
    
    if (input$tabs == "Time Series") {
      
      diff_in_days <- as.numeric(difftime(input$dateRange[2], input$dateRange[1], units = "days"))
      
      if (diff_in_days <= 31) {
        groupChoices <- c("Day", "Week") 
      } else if (between(diff_in_days, 32, 365)) {
        groupChoices <- c("Week", "Month")
      } else {
        groupChoices <- "Month"
      }
      
      if (floor_date(input$dateRange[2], unit = "month") == floor_date(input$dateRange[1], unit = "month")) {
        selectedMetric <- "Day"
      } else if (((diff_in_days / 365) * 12) <= 2.5) {
        selectedMetric <- "Week" 
      } else {
        selectedMetric <- "Month"
      }
      
      selectInput("XbreakFreq",
                  label = "Select Date Grouping:",
                  choices = groupChoices,
                  multiple = FALSE,
                  width = "200px",
                  selected = selectedMetric
      )
    }
    
  })
  
  # Time series title
  output$timeSeriesTitle <- renderText({
    
    req(input$XbreakFreq, input$XtypeTime, input$timeSeriesSeries, input$XdateMetric)
    
    time <- case_when((input$XbreakFreq == "Day") ~ "Daily ",
                      TRUE ~ paste0(input$XbreakFreq, "ly "))
    
    text <- paste0(time,
                   input$XtypeTime,
                   " of Reports by ",
                   input$timeSeriesSeries,
                   " (based on ",
                   input$XdateMetric,
                   ")")
  })
  
  # Time series plot
  output$examTrends <- renderPlotly({ 
    
    req(mainData(), input$XtypeTime, input$dateRange, input$XbreakFreq, input$timeSeriesSeries)
    
    req(length(input$authorityFilt) > 0, length(input$timeSeriesExam) > 0)
    
    diffDays <- as.numeric(difftime(input$dateRange[2], input$dateRange[1], units = "days"))
    
    diffMonths <- as.numeric(difftime(input$dateRange[2], input$dateRange[1], units = "days") / 365) * 12
    
    if (input$XbreakFreq == "Day") {
      breakFreq <- "1 day"
      metric <- sym("date")
      date_label <- "%b %d %Y"
    } else if (input$XbreakFreq == "Week") {
      breakFreq <- "1 week"
      metric <- sym("Week")
      date_label <- "%b %d %Y"
    } else {
      breakFreq <- paste0(ceiling(diffMonths / 24), " month")
      metric <- sym("Month")
      date_label <- "%b %Y"
    }
    
    if (input$timeSeriesSeries == "Exam Type") {
      colorSeries <- sym("ExamTypeStd")
    } else if (input$timeSeriesSeries == "Health Authority") {
      colorSeries <- sym("ChannelName")
    }
    
    plotData <- mainData() %>%
      filter(ExamTypeStd %in% input$timeSeriesExam) %>%
      group_by(!!metric, !!colorSeries) %>%
      summarise(Volume = n()) %>%
      ungroup() %>%
      mutate(total = sum(Volume)) %>%
      group_by(!!metric, !!colorSeries) %>%
      mutate(Proportion = round(Volume/total, digits = 4)) %>%
      filter(!!colorSeries != "") %>%
      ungroup()
    
    examTimeSeries <- plotData %>%
      ggplot(mapping = aes(x = !!metric, y = !!sym(input$XtypeTime), color = !!colorSeries)) +
      geom_line() +
      theme_ipsum() +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
      labs(title = paste0("n = ", sum(plotData$Volume))) +
      scale_x_date(date_breaks = breakFreq,
                   date_labels = date_label) 
    
    examTimeSeriesPlotly <- ggplotly(examTimeSeries) %>%
      layout(margin = list(t = 50, b = 30))
      
  })
  
  # Time series summary data 
  timeSeriesSummaryData <- reactive({
    
    req(mainData(), input$XbreakFreq, input$dateRange)
    
    colorVar <- switch(input$timeSeriesSeries,
                       "Exam Type" = "ExamTypeStd",
                       "Health Authority" = "ChannelName")
    
    colorSym <- sym(colorVar)
    
    theData <- mainData() %>%
      filter(ExamTypeStd %in% input$timeSeriesExam) %>%
      filter(!!colorSym != "") %>%
      group_by(date, !!colorSym) %>%
      summarise(Volume = sum(n()), .groups = "drop")
    
    table <- getStats(input$dateRange[1], input$dateRange[2], theData, colorVar)
    
    if (input$XbreakFreq == "Day") {
      table <- table %>%
        select(1:5, 14)
    } else if (input$XbreakFreq == "Week") {
      table <- table %>%
        select(1, 6:9, 14)
    } else {
      table <- table %>%
        select(1, 10:14)
    }
    
  })
  
  # Time series summary data formatted as table
  output$timeSeriesSummary <- renderDT({
    
    req(input$dateRange, timeSeriesSummaryData(), mainData(), input$XbreakFreq)
    
    datatable(timeSeriesSummaryData())
    
  })
  
  # Outlier data 
  outlierData <- reactive({
    
    req(input$timeSeriesSeries, input$XbreakFreq)

    dateType <- switch(input$XbreakFreq,
                       "Day" = "date",
                       "Week" = "Week",
                       "Month" = "Month")
    
    colorVar <- switch(input$timeSeriesSeries,
                       "Exam Type" = "ExamTypeStd",
                       "Health Authority" = "ChannelName")

    dateType <- sym(dateType)

    colorSym <- sym(colorVar)

    outlierData <- mainData() %>%
      filter(ExamTypeStd %in% input$timeSeriesExam) %>%
      filter(!!colorSym != "") %>%
      mutate(Month = format(Month, "%Y-%m")) %>%
      group_by(!!colorSym, !!dateType) %>%
      summarise(Volume = sum(n()), .groups = "drop") %>%
      ungroup() %>%
      group_by(!!colorSym) %>%
      mutate(low = ceiling(quantile(Volume, 0.10)),
             high = ceiling(quantile(Volume, 0.75) + (1.5 * IQR(Volume))),
             "Median" = ceiling(quantile(Volume, 0.5)),
             Type = ifelse((Volume > high), "High", 
                           ifelse((Volume < low), "Low", "Normal")),
             "Outlier Bounds" = str_c("(", low, ", ", high, ")")
      ) %>%
      ungroup() %>%
      filter(Type %in% c("High", "Low")) %>%
      mutate("Proportion Above/Below Threshold" = case_when(Type == "High" ~ round((Volume - high)/high, 3),
                                                            Type == "Low" ~ round((Volume - low)/low, 3),
                                                            TRUE ~ NA
                                                            )
      ) %>%
      select(1:2, 6, 8, 7, 3, 9) %>%
      arrange(desc(!!dateType))
      
  })
  
  # Outlier data - title for n of outliers 
  output$nOutliers <- renderText({
    
    req(outlierData())
    
    text <- paste0("Outliers (n = ", nrow(outlierData()), ")")
    
  })
  
  # Summary of detected outliers
  output$timeSeriesOutliers <- renderDT({
    
    req(input$XtypeTime, outlierData())
    
    datatable(outlierData())
  })
  
  # Time series, health authority filter
  output$timeSeriesHA <- renderUI({
    
    pickerInput("authorityFilt",
                label = paste0("Filter by Health Authority:"),
                choices = unique(messageData$ChannelName),
                selected = unique(messageData$ChannelName),
                multiple = TRUE,
                options = list(
                  'actions-box' = TRUE 
                )
    )
    
  })
  

  # Tab 2 - Exam Types ------------------------------------------------------
  
  # Total sum of Exam Types (Std)
  output$typeReports <- renderPlotly({
    
    req(mainData(), input$examType)
    
    examType <- mainData() %>%
      filter(ExamTypeStd != "") %>%
      group_by(ExamTypeStd) %>%
      summarise(Volume = n()) %>%
      ungroup() %>%
      mutate(total = sum(Volume)) %>%
      group_by(ExamTypeStd) %>%
      mutate(Proportion = round(Volume/total, digits = 3)) %>%
      select(1, 2, 4) %>%
      arrange(desc(Volume)) 
    
    examPlot <- examType %>%
      ggplot(mapping = aes(x = ExamTypeStd, y = !!sym(input$XtypeReports))) +
      geom_bar(stat = "identity", fill = "#dd4b39", color = "black") +
      theme_ipsum() +
      labs(title = paste0("n = ", nrow(mainData())))
    
    ggplotly(examPlot) %>%
      layout(margin = list(t = 50, b = 10))
    
  })
  
  # Mapping Exam Descriptions to a consistent format
  examDescription <- reactive({
    
    req(input$examType, input$examViewSwitch)
    
    grouped <- mainData() %>%
      filter(ExamTypeStd != "") %>%
      filter(ExamTypeStd == input$examType)
    
    # Category consolidation
    if (input$examType == "CT") {
      grouped1 <- grouped %>%
        group_by(ExamDescription) %>%
        summarise(count = n()) %>%
        left_join(codes, by = "ExamDescription") %>%
        mutate(GroupName = ifelse(is.na(GroupName), ExamDescription, GroupName),
               GroupName = str_extract(GroupName, "^\\S+(?:\\s+\\S+)?"),
               GroupName = gsub(",", "", as.character(GroupName)),
               GroupName = case_when(
                 GroupName == "CT Sinuses" ~ "CT Sinus",
                 GroupName == "CT Lower" ~ "CT Lower Extremity",
                 GroupName == "CT Upper" ~ "CT Upper Extremity",
                 GroupName == "CT Soft" ~ "CT Soft Tissue",
                 TRUE ~ GroupName
               )
        )
    } else if (input$examType == "MR") {
      grouped1 <- grouped %>%
        group_by(ExamDescription) %>%
        summarise(count = n()) %>%
        left_join(codes, by = "ExamDescription") %>%
        mutate(GroupName = ifelse(is.na(GroupName), ExamDescription, GroupName)) %>%
        mutate(GroupName = str_extract(GroupName, "^\\S+(?:\\s+\\S+)?"))
    } else if (input$examType == "MG") {
      grouped1 <- grouped %>%
        # mutate(ExamDescription = case_when(
        #   str_detect(ExamDescription, "^[A-Z]{4}.*") ~ paste0("MP", ExamDescription),
        #   TRUE ~ ExamDescription
        # )) %>%
        group_by(ExamDescription) %>%
        summarise(count = n(), .groups = "drop") %>%
        left_join(codes, by = "ExamDescription") %>%
        mutate(GroupName = ifelse(is.na(GroupName), ExamDescription, GroupName),
               GroupName = case_when(
                 str_detect(GroupName, "Screening") ~ "MG Screening",
                 str_detect(GroupName, "Diagnostic") ~ "MG Diagnostic",
                 str_detect(GroupName, "Tomo") ~ "MG Tomosynthesis",
                 str_detect(GroupName, "Specimen") ~ "MG Breast Specimen",
                 str_detect(GroupName, "Stereo") ~ "MG Stereotactic/Stereo Core Biopsy",
                 str_detect(GroupName, "Consult") ~ "MG Consultation",
                 str_detect(GroupName, "XR BREAST SPECIMEN") ~ "MG Breast Specimen",
                 str_detect(GroupName, "MAM-CON") ~ "MG Consultation",
                 TRUE ~ GroupName
               )
        )
    } else if (input$examType == "NM") {
      grouped1 <- grouped %>%
        group_by(ExamDescription) %>%
        summarise(count = n()) %>%
        left_join(codes, by = "ExamDescription") %>%
        mutate(GroupName = ifelse(is.na(GroupName), ExamDescription, GroupName)) %>%
        mutate(GroupName = str_extract(GroupName, "^\\S+(?:\\s+\\S+)?")) 
    } else if (input$examType == "US") {
      grouped1 <- grouped %>%
        # mutate(ExamDescription = case_when(
        #   str_detect(ExamDescription, "^[A-Z]+$") ~ paste0("US", ExamDescription),
        #   TRUE ~ ExamDescription
        # )) %>%
        group_by(ExamDescription) %>%
        summarise(count = n()) %>%
        left_join(codes, by = "ExamDescription") %>%
        mutate(GroupName = ifelse(is.na(GroupName), ExamDescription, GroupName),
               GroupName = str_extract(GroupName, "^\\S+(?:\\s+\\S+)?"),
               GroupName = case_when(
                 GroupName == "US Extremity" ~ "US Extremities",
                 GroupName == "US Lower" ~ "US Extremities",
                 GroupName == "US Upper" ~ "US Extremities",
                 GroupName == "US OB" ~ "US Obstetrical",
                 GroupName == "US Soft" ~ "US Soft Tissue",
                 GroupName == "US Mass" ~ "US Soft Tissue",
                 GroupName == "US-CON" ~ "US Consultation",
                 TRUE ~ GroupName
               )
        )
    } else if (input$examType == "PET") {
      grouped1 <- grouped %>%
        group_by(ExamDescription) %>%
        summarise(count = n()) %>%
        left_join(codes, by = "ExamDescription") %>%
        mutate(GroupName = ifelse(is.na(GroupName), ExamDescription, GroupName))
    }
    
    grouped2 <- grouped1 %>%
      group_by(GroupName) %>%
      summarise(Volume = sum(count)) %>%
      ungroup() %>%
      mutate(Proportion = round(Volume/sum(Volume), digits = 4),
             GroupName = replace(GroupName, is.na(GroupName), "NA")
      )
    
  })
  
  # Exam Description Plot
  output$examDescriptionPlot <- renderPlotly({
    
    req(input$examType, input$examViewSwitch, examDescription(), input$XtypeExaminations)
    
    if (input$examType %in% c("US", "MR", "CT")) {
      grouped <- examDescription() %>%
        slice_max(!!sym(input$XtypeExaminations), n = 20) 
    } else if (input$examType %in% c("PET", "NM", "MG")) {
      grouped <- examDescription() %>%
        slice_max(!!sym(input$XtypeExaminations), n = 10) 
    }
    
    ggplot <- grouped %>%
      ggplot(mapping = aes(x = GroupName, y = !!sym(input$XtypeExaminations))) +
      geom_bar(stat = "identity", fill = "#dd4b39", color = "black") +
      theme_ipsum() + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_x_discrete(limits = grouped$GroupName) + 
      labs(title = paste0("n = ", sum(examDescription()$Volume)))
    
    plot <- ggplotly(ggplot) %>%
      layout(margin = list(t = 50, b = 30))
    
  })
  
  # Full Exam Description Data, formatted in table
  output$examDescriptionTable <- renderDT({
    
    req(examDescription())
    
    grouped <- examDescription() %>%
      arrange(desc(Volume))
    
    datatable(grouped,
              options = list(
                pageLength = 20
              )
    )
    
  })
  
  # Switch for viewing - selecting between the table or the plot
  output$examDescriptions <- renderUI({
    
    if (input$examViewSwitch == "Plot") {
      plotly::plotlyOutput("examDescriptionPlot", height = 500)
    } else if (input$examViewSwitch == "Table") {
      DT::DTOutput("examDescriptionTable")
    }
    
  })
  
  # Switch for viewing - switching between volume/proportions, for the Exam Description plot
  output$examDPlotSwitch <- renderUI({
    
    req(input$examViewSwitch)
    
    if (input$examViewSwitch == "Plot") {
      selectInput("XtypeExaminations",
                  label = "Select Metric:",
                  choices = c("Volume", "Proportion"),
                  selected = "Volume",
                  multiple = FALSE,
                  width = "200px"
      )
    } 
    
  })
  

  # Tab 3 - Message Data ----------------------------------------------------

  output$channelPlot <- renderPlotly({
    
    req(input$XtypeAuthority, input$dateRange)
    
    channels <- messageData %>%
      filter(between(MessageDateTime, input$dateRange[1], input$dateRange[2])) %>%
      count(ChannelName, name = "Volume") %>%
      mutate(
        total = sum(Volume),
        Proportion = round(Volume/total, digits = 3)
      )
    
    plot <- channels %>%
      ggplot(mapping = aes(x = ChannelName, y = !!sym(input$XtypeAuthority))) +
      geom_bar(stat = "identity", fill = "#dd4b39", color = "black") +
      theme_ipsum() +
      labs(title = paste0("n = ", sum(channels$Volume)))
    
    channelsPlot <- ggplotly(plot) %>%
      layout(margin = list(t = 50, b = 30))
    
  })
  
  output$facilityTable <- renderDT({
    
    req(length(input$facilityHA) > 0)
    
    sendingFacilities <- messageData %>%
      filter(between(MessageDateTime, input$dateRange[1], input$dateRange[2])) %>%
      mutate(SendingFacility = if_else(
        str_detect(SendingFacility, "_"),
        str_sub(SendingFacility, 1, 3),
        SendingFacility
      )) %>%
      filter(ChannelName %in% input$facilityHA) 
    
    total <- nrow(sendingFacilities)
    
    summary <- sendingFacilities %>%
      group_by(ChannelName, SendingFacility) %>%
      summarise(Volume = n(), Proportion = round(Volume/total, digits = 2), groups = "drop") %>%
      arrange(desc(Volume)) %>%
      select(2, 1, 3, 4) %>%
      rename("Sending Facility" = 1, "Health Authority" = 2)
    
    datatable(summary)
    
  })
  
  output$dataComplete <- renderDT({
    
    req(mainData())
    
    mainData <- mainData() %>%
      rename("Observation Date" = 2,
             "Results Report Date" = 3,
             "Message Date" = 11
      ) %>%
      mutate(across(where(is.character), ~ if_else(. == "", NA_character_, .)))
    
    table <- mainData %>%
      summarise(across(everything(),
                       list(
                         "N Incomplete" = ~ sum(is.na(.)),
                         "Proportion Incomplete" = ~ round(mean(is.na(.)), digits = 3)
                       )
      )
      ) %>%
      pivot_longer(cols = everything(), names_to = c("Variable", "Metric"), values_to = "Value", names_sep = "_") %>%
      pivot_wider(names_from = "Metric", values_from = Value) %>%
      arrange(desc(!!sym("N Incomplete"))) %>%
      ungroup()
    
    datatable(table,
              options = list(
                pageLength = 10
              )
    )
    
  })  

  # Tab 4 - Demographic Plots -----------------------------------------------
  
  # Distribution of number of reports per patient 
  output$reportsPerPatient <- renderPlotly({
    
    req(input$demoMetric)
    
    numberReports <- mainData() %>%
      group_by(Identifier) %>%
      summarise(reports_per_patient = n()) %>%
      arrange(desc(reports_per_patient)) %>%
      ungroup() %>%
      mutate(reports_per_patient = if_else(reports_per_patient >= 10, "10+", as.character(reports_per_patient)),
             reports_per_patient = factor(reports_per_patient, levels = c(1:9, "10+"))) %>%
      group_by(reports_per_patient) %>%
      summarise(Volume = n()) %>%
      mutate(Proportion = round(Volume/sum(Volume), digits = 3)) %>%
      rename("Reports per Patient" = 1)
    
    plot <- numberReports %>%
      ggplot(mapping = aes(x = !!sym("Reports per Patient"), y = !!sym(input$demoMetric))) +
      geom_bar(stat = "identity", fill = "#dd4b39", color = "black") +
      theme_ipsum() +
      labs(title = paste0("n = ", sum(numberReports$Volume)))
    
    numberReportsPlot <- ggplotly(plot) %>%
      layout(margin = list(t = 50, b = 30))
    
  })
  
  # Age of patients (distinct) - 10 year age brackets
  output$patientAgePlot <- renderPlotly({
    
    req(input$demoMetric, length(input$authorityFilt) > 0)
    
    mainData <- mainData() %>%
      distinct(Identifier, .keep_all = TRUE) %>%
      filter(!is.na(BirthDate))
    
    ageBrackets <- mainData %>%
      mutate(
        ageBracket = cut(
          Age,
          breaks = seq(0, 130, by = 10),
          right = FALSE,
          labels = paste0(seq(0, 120, by = 10), "-", seq(9, 119, by = 10), " years old")
        )
      ) %>%
      group_by(ageBracket) %>%
      summarise(Volume = n(), .groups = "drop") %>%
      mutate(Proportion = round(Volume/sum(Volume), 4))
    
    plot <- ageBrackets %>%
      ggplot(mapping = aes(x = ageBracket, y = !!sym(input$demoMetric))) +
      geom_bar(stat = "identity", fill = "#dd4b39", color = "black") +
      theme_ipsum() + 
      theme(axis.text.x = element_text(angle = 45)) +
      labs(title = paste0("n = ", nrow(mainData),
                          ", Mean Age = ", ceiling(mean(mainData$Age, na.rm = TRUE)),
                          ", Median Age = ", ceiling(median(mainData$Age, na.rm = TRUE))
                          ),
           x = "Age Bracket",
           y = "Volume")
    
    numberReportsPlot <- ggplotly(plot) %>%
      layout(margin = list(t = 50, b = 10))
    
  })
  
  # Overall sex distribution of patients 
  output$sexPlot <- renderPlotly({
    
    req(input$demoMetric)
    
    mainData <- mainData() %>%
      distinct(Identifier, .keep_all = TRUE)
    
    sexDistribution <- mainData %>%
      filter(Sex %in% c("M", "F")) %>%
      group_by(Sex) %>%
      summarise(Volume = n()) %>%
      ungroup() %>%
      mutate(Proportion = round(Volume/sum(Volume), digits = 3))
    
    plot <- sexDistribution %>%
      ggplot(mapping = aes(x = Sex, y = !!sym(input$demoMetric))) +
      geom_bar(stat = "identity", fill = "#dd4b39", color = "black") +
      theme_ipsum() +
      labs(title = paste0("n = ", sum(sexDistribution$Volume)))
    
    sexPlot <- ggplotly(plot, height = 400) %>%
      layout(margin = list(t = 50, b = 30))
    
  })
  
  # Plot of reports - from the 10 most frequent patient cities, based on address
  output$cityPlot <- renderPlotly({
    
    req(input$demoMetric)
    
    mainData <- mainData() %>%
      distinct(Identifier, .keep_all = TRUE)
    
    cityDistribution <- mainData %>%
      group_by(City) %>%
      summarise(Volume = n()) %>%
      ungroup() %>%
      mutate(Proportion = round(Volume/sum(Volume), digits = 3)) %>%
      slice_max(order_by = !!sym(input$demoMetric), n = 10)
    
    plot <- cityDistribution %>%
      ggplot(mapping = aes(x = reorder(City, -!!sym(input$demoMetric)), y = !!sym(input$demoMetric))) +
      geom_bar(stat = "identity", fill = "#dd4b39", color = "black") +
      theme_ipsum() +
      theme(axis.text.x = element_text(angle = 45),
            size = 8) +
      labs(x = "City",
           y = input$demoMetric,
           title = paste0("n = ", sum(cityDistribution$Volume)))
    
    cityPlot <- ggplotly(plot, height = 400) %>%
      layout(margin = list(t = 50, b = 30))
    
  })
  
}