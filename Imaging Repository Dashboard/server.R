# Imaging Dashboard Server ------------------------------------------------

server <- function(input, output, session) {
  
  # Global items ------------------------------------------------------------
  # Connection
  con <- dbConnect(odbc::odbc(),
                   Driver = "SQL Server",
                   Server = "spdbsdima001",
                   Database = "BCCR_Imaging_Results",
                   Trusted_Connection = "Yes")
  # Combined result + patient data
  mainData <- reactive({
    
    req(input$XdateMetric, input$authorityFilt)
    
    req(input$dateRange[2] >= input$dateRange[1])
    
    dateMap <- c(
      "Observation Date" = "ObservationDateTime",
      "Results Report Date" = "ResultsReportDateTime",
      "Message Date" = "MessageDateTime"
    )
    
    imagingQuery <- paste0(
                      "SELECT 
                      p.[MessageId],
                      p.[Identifier],
                      p.[BirthDate],
                      p.[Sex],
                      p.[City],
                      m.[SendingFacility],
                      m.[MessageDateTime],
                      m.[BaseMessageId],
                      r.[ExamDescription],
                      r.[ScheduledDateTime],
                      r.[ObservationDateTime],
                      r.[ResultsReportDateTime],
                      r.[ExamType],
                      r.[ExamTypeStd],
                      r.[ResultStatus],
                      r.[HealthAuthority]
                      FROM [BCCR_Imaging_Results].[dbo].[Patient] p 
                      JOIN [BCCR_Imaging_Results].[dbo].[Message] m 
                      ON p.[MessageId] = m.[MessageId]
                      JOIN [BCCR_Imaging_Results].[dbo].[Result] r
                      ON p.[MessageId] = r.[MessageId] 
                      WHERE [",
                      dateMap[input$XdateMetric],
                      "] BETWEEN '",
                      ymd(input$dateRange[1]),
                      "' AND '",
                      ymd(input$dateRange[2]),
                      "'"
                    )
    
    mainData <- dbGetQuery(con, imagingQuery) %>%
      filter(HealthAuthority %in% input$authorityFilt) %>%
      mutate(across(c(ObservationDateTime, ResultsReportDateTime, MessageDateTime), # To ensure that datetimes are displayed PST/PDT
                    ~ ymd(as.Date(with_tz(ymd_hms(.x), tzone = "America/Los_Angeles"))) # I.e., in the case that date filter min set to
                    ) # '2025-04-01' but some observations are displayed as '2025-03-31' due to timezone reporting difference 
             ) %>%
      mutate(Month = floor_date(!!sym(dateMap[input$XdateMetric]), unit = "month"),
             Week = floor_date(!!sym(dateMap[input$XdateMetric]), unit = "week", week_start = 1),
             ) %>%
      mutate(BirthDate = ymd(substr(BirthDate, 1, 10)),
             BirthDate = if_else(BirthDate <= ymd("1901-01-01"), as.Date(NA), BirthDate),
             Identifier = if_else(Identifier == "", as.character(NA), Identifier),
             Age = if_else(is.na(BirthDate),
                           as.numeric(NA),
                           floor(as.numeric(interval(BirthDate, today())/ years(1)))
                           ),
             City = str_to_title(City)
            ) %>%
      left_join(codes, by = "ExamDescription") %>%
      mutate(
        GroupName = ifelse(is.na(GroupName), ExamDescription, GroupName),
        GroupName = ifelse(!ExamTypeStd %in% c("PET", "MG"), str_extract(GroupName, "^\\S+(?:\\s+\\S+)?"), GroupName),
        GroupName = gsub(",", "", as.character(GroupName)),
        GroupName = case_when(
          ExamTypeStd == "CT" & GroupName == "CT Sinuses" ~ "CT Sinus",
          ExamTypeStd == "CT" & GroupName == "CT Lower" ~ "CT Lower Extremity",
          ExamTypeStd == "CT" & GroupName == "CT Upper" ~ "CT Upper Extremity",
          ExamTypeStd == "CT" & GroupName == "CT Soft" ~ "CT Soft Tissue",
          ExamTypeStd == "MG" & str_detect(GroupName, "Screening") ~ "MG Screening",
          ExamTypeStd == "MG" & str_detect(GroupName, "Diagnostic") ~ "MG Diagnostic",
          ExamTypeStd == "MG" & str_detect(GroupName, "Tomo") ~ "MG Tomosynthesis",
          ExamTypeStd == "MG" & str_detect(GroupName, "Specimen|XR BREAST SPECIMEN") ~ "MG Breast Specimen",
          ExamTypeStd == "MG" & str_detect(GroupName, "Stereo") ~ "MG Stereotactic/Stereo Core Biopsy",
          ExamTypeStd == "MG" & str_detect(GroupName, "Consult|MAM-CON") ~ "MG Consultation",
          ExamTypeStd == "MG" & str_detect(GroupName, "Breast Clip") ~ "MG Breast Clip Placement",
          ExamTypeStd == "MG" & str_detect(GroupName, "Contrast") ~ "MG Mammogram w/ Contrast",
          ExamTypeStd == "US" & GroupName %in% c("US Extremity", "US Lower", "US Upper") ~ "US Extremities",
          ExamTypeStd == "US" & GroupName %in% c("US OB") ~ "US Obstetrical",
          ExamTypeStd == "US" & GroupName %in% c("US Soft", "US Mass") ~ "US Soft Tissue",
          ExamTypeStd == "US" & GroupName == "US-CON" ~ "US Consultation",
          ExamTypeStd == "MR" & str_detect(GroupName, "MR Spine|MRI Spine") ~ "MR/MRI Spine",
          ExamTypeStd == "MR" & str_detect(GroupName, "MR Head|MRI Head") ~ "MR/MRI Head",
          TRUE ~ GroupName
        ),
        GroupName = replace(GroupName, is.na(GroupName), "NA")
      )

  })
  
  # Global Date Switch 
  output$globalDateSwitch <- renderUI({
    
    # req(input$XdateMetric)
    
    startDate <- ifelse(input$XdateMetric == "Message Date",
                        ymd("2024-10-01"),
                        ymd("2020-01-01")
                        )
    
    dateRangeInput(
      "dateRange",
      label = "Filter by Date:",
      min = startDate,
      max = today(),
      start = ymd("2025-06-01"),
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
      
      selectedMetric <- "Month"
      if (floor_date(input$dateRange[2], unit = "month") == floor_date(input$dateRange[1], unit = "month")) {
        selectedMetric <- "Day"
      } else if (((diff_in_days / 365) * 12) <= 2.5) {
        selectedMetric <- "Week" 
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
    
    dateMap <- c(
      "Observation Date" = "ObservationDateTime",
      "Results Report Date" = "ResultsReportDateTime",
      "Message Date" = "MessageDateTime"
    )
    
    diffDays <- as.numeric(difftime(input$dateRange[2], input$dateRange[1], units = "days"))
    
    diffMonths <- as.numeric(difftime(input$dateRange[2], input$dateRange[1], units = "days") / 365) * 12
    
    if (input$XbreakFreq == "Day") {
      breakFreq <- "1 day"
      metric <- sym(dateMap[input$XdateMetric])
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
      colorSeries <- sym("HealthAuthority")
    }
    
    plotData <- mainData() %>%
      filter(ExamTypeStd %in% input$timeSeriesExam) %>%
      group_by(!!metric, !!colorSeries) %>%
      summarise(Volume = n(), .groups = "drop") %>%
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
    
    dateMetric <- switch(input$XdateMetric,
                         "Observation Date" = "ObservationDateTime",
                         "Results Report Date" = "ResultsReportDateTime",
                         "Message Date" = "MessageDateTime")
    
    viewVariable <- switch(input$timeSeriesSeries,
                           "Exam Type" = "ExamTypeStd",
                           "Health Authority" = "HealthAuthority")
  
    if (input$XbreakFreq == "Day") {
      summaryTable <- getTimeSummary(mainData(), dateMetric, viewVariable, "Daily")
    } else if (input$XbreakFreq == "Week") {
      summaryTable <- getTimeSummary(mainData(), "Week", viewVariable, "Weekly")
    } else {
      summaryTable <- getTimeSummary(mainData(), "Month", viewVariable, "Monthly")
    }
    
    return(summaryTable)
    
  })
  
  # Time series summary data formatted as table
  output$timeSeriesSummary <- renderDT({
    
    req(input$dateRange, timeSeriesSummaryData(), mainData(), input$XbreakFreq)
    
    datatable(timeSeriesSummaryData())
    
  })
  
  # Outlier data 
  outlierData <- reactive({
    
    req(mainData(), input$timeSeriesSeries, input$XbreakFreq, input$XdateMetric)

    dateType <- switch(input$XbreakFreq,
                       "Day" = switch(input$XdateMetric,
                                      "Observation Date" = "ObservationDateTime",
                                      "Results Report Date" = "ResultsReportDateTime",
                                      "Message Date" = "MessageDateTime"),
                       "Week" = "Week",
                       "Month" = "Month")
    
    colorVar <- switch(input$timeSeriesSeries,
                       "Exam Type" = "ExamTypeStd",
                       "Health Authority" = "HealthAuthority")

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
                choices = c("FHA", "IHA", "NHA", "PHSA", "VCH-PHC", "VCHA", "VIHA"),
                selected = c("FHA", "IHA", "NHA", "PHSA", "VCH-PHC", "VCHA", "VIHA"),
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
    
    plotly <- getBarPlotly(mainData(), "ExamTypeStd", input$XtypeReports, "", "")
    
  })
  
  # Exam Description Plot
  output$examDescriptionPlot <- renderPlotly({
    
    req(input$examType, input$examViewSwitch, input$XtypeExaminations)
    
    plotly <- getBarPlotly(mainData(), "GroupName", input$XtypeExaminations, "ExamTypeStd", input$examType)
    
  })
  
  # Full Exam Description Data, formatted in table
  output$examDescriptionTable <- renderDT({
    
    req(mainData())
    
    grouped <- mainData() %>%
      filter(ExamTypeStd != "") %>%
      filter(ExamTypeStd == input$examType) %>%
      group_by(GroupName) %>%
      summarise(Volume = sum(n())) %>%
      ungroup() %>%
      mutate(Proportion = round(Volume/sum(Volume), digits = 3)
      ) %>%
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

  # Health Authority Bar Plot
  output$channelPlot <- renderPlotly({
    
    req(input$XtypeAuthority, input$dateRange)
    
    plotly <- getBarPlotly(mainData(), "HealthAuthority", input$XtypeAuthority, "", "")
    
  })
  
  # Table of Facilities
  output$facilityTable <- renderDT({
    
    req(length(input$facilityHA) > 0)
    
    sendingFacilities <- mainData() %>%
      # filter(between(MessageDateTime, input$dateRange[1], input$dateRange[2])) %>%
      mutate(SendingFacility = if_else(
        str_detect(SendingFacility, "_"),
        str_sub(SendingFacility, 1, 3),
        SendingFacility
      )) %>%
      filter(HealthAuthority %in% input$facilityHA) 
    
    total <- nrow(sendingFacilities)
    
    summary <- sendingFacilities %>%
      group_by(HealthAuthority, SendingFacility) %>%
      summarise(Volume = n(), Proportion = round(Volume/total, digits = 2), groups = "drop") %>%
      arrange(desc(Volume)) %>%
      select(2, 1, 3, 4) %>%
      rename("Sending Facility" = 1, "Health Authority" = 2)
    
    datatable(summary)
    
  })
  
  # Data Completeness Table - to show "incompletes" (where obs is "NA")
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
    
    req(input$demoMetric, input$demoExamType)
    
    numberReports <- mainData() %>%
      filter(ExamTypeStd %in% input$demoExamType) %>%
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
    
    req(input$demoMetric, length(input$authorityFilt) > 0, input$demoExamType)
    
    mainData <- mainData() %>%
      distinct(Identifier, .keep_all = TRUE) %>%
      filter(ExamTypeStd %in% input$demoExamType) %>%
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
      layout(margin = list(t = 50, b = 10, l = 10, r = 10))
    
  })
  
  # Overall sex distribution of patients 
  output$sexPlot <- renderPlotly({
    
    req(input$demoMetric, input$demoExamType)
    
    mainData <- mainData() %>%
      filter(ExamTypeStd %in% input$demoExamType) %>%
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
    
    sexPlot <- ggplotly(plot) %>%
      layout(margin = list(t = 50, b = 10, l = 10, r = 10))
    
  })
  
  # Plot of reports - from the 10 most frequent patient cities, based on address
  output$cityPlot <- renderPlotly({
    
    req(input$demoMetric, input$demoExamType)
    
    mainData <- mainData() %>%
      filter(ExamTypeStd %in% input$demoExamType) %>%
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
    
    cityPlot <- ggplotly(plot) %>%
      layout(margin = list(t = 50, b = 10, l = 10, r = 10))
    
  })
  
}
