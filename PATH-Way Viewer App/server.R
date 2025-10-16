# Imaging-Path Viewer Dashboard - Server ---------------------------------------

server <- function(input, output) {
  
  # Connection (Pathology Server)
  pathCon <- dbConnect(odbc::odbc(),
                       Driver = "SQL Server",
                       Server = "spdbsmarc001",
                       Database = "eMaRCPlus_v60Patch",
                       Trusted_Connection = "Yes")
  
  # Connection (Imaging Snapshot Server)
  imagingCon <- dbConnect(odbc::odbc(),
                          Driver = "SQL Server",
                          Server = "spdbsdima002",
                          Database = "bccr_img_path_DS",
                          Trusted_Connection = "Yes")
  
# Information Boxes -------------------------------------------------------
  
  # Reminder box
  observe({
    showModal(
      modalDialog(
        title = "Note",
        paste0("Dashboard requires approximately 30 seconds to load data/visualizations at start-up"),
        easyClose = TRUE,
        size = "m"
      )
    )
  })
  
  # Selected Patient ID - Text Output
  output$selectedPatient <- renderText({
    
    req(input$IdPHN)
    
    ID <- input$IdPHN
    
  })
  
  # Number of Pathology/Imaging Reports, per patient - Text Output
  output$reportNumbers <- renderTable({
    
    req(input$IdPHN, patientReports(), input$DXGroup)
    
    stats <- data.frame(
      Imaging = sum(patientReports()$'Report Type' == "Imaging"),
      Pathology = sum(patientReports()$'Report Type' == "Pathology"),
      Total = nrow(patientReports())
    )
    
  })
  
  # Patient Characteristics - Table Output
  output$patientProfile <- renderTable({
    
    req(patientIndex(), rawPathology(), input$IdPHN)
    
    index <- patientIndex() %>%
      filter(Identifier == input$IdPHN) %>%
      select(DXGroup, Age, Sex, HealthAuthority) %>%
      mutate(Age = as.character(Age),
             DXGroup = as.character(DXGroup)) %>%
      rename("DX Group" = DXGroup,
             "Health Authority" = HealthAuthority)
    
  })
  
  # Patient Selection Modal
  updateModal <- function() {
    showModal(
      modalDialog(
        title = paste0("Select Patient to View"),
        br(),
        infoBox(value = textOutput("selectedPatient"),
                "Patient to View",
                color = "blue",
                icon = icon("user-md"),
                width = 12
        ),
        box(
          title = "Patient Index",
          DTOutput("patientSelectionTable"),
          width = 12,
        ),
        footer = tagList(
          br()
        ),
        easyClose = TRUE,
        size = "l"
      )
    )
    
  }

# Datasets ----------------------------------------------------------------

  # Large load - all pathology reports
  # One load/format at initialization - less computing down the road
  rawPathology <- reactive({
   
    # Main Pathology Report Query
    pathQuery <- "SELECT [msgid]
                        ,[msg_dt]
                        ,[ha]
                        ,[final_label]
                  FROM [eMaRCPLus_v60Patch].[dbo].[bccr_vw_can_rep]
                  WHERE [reportability_filter] = 1 
                        AND [msg_dt] >= '2023-01-01'"
    
    # Path Identifier Query
    pid_query <- "SELECT [MSGID], [PHN], [SEX], [BIRTHDATE]
                  FROM [eMaRCPLus_v60Patch].[dbo].[bccr_vw_pid]"
    
    # Path Identifier Data
    pid_data <- dbGetQuery(pathCon, pid_query) %>%
      rename(msgid = MSGID,
             Identifier = PHN) %>%
      mutate(Age = BIRTHDATE,
             Sex = SEX) %>%
      mutate(Age = floor(as.numeric(interval(as.Date(as.character(BIRTHDATE), format = "%Y%m%d"), today())/years(1))))
    
    # Pulling pathology reports
    pathologyData <- dbGetQuery(pathCon, pathQuery) %>%
      left_join(pid_data, by = "msgid") %>%
      mutate(msg_dt = ymd(msg_dt)) %>%
      distinct(msgid, .keep_all = TRUE) %>%
      rename(HealthAuthority = ha)
    
  })
  
  # Index of unique patients associated with pathology
  allPathologyIDs <- reactive({
    
    req(rawPathology())
    
    uniquePathologyPatients <- rawPathology() %>%
      group_by(Identifier) %>%
      summarise(DXGroup = list(sort(unique(final_label))),
                PathMessageIDs = paste(sort(unique(msgid)), collapse = ", "),
                Age,
                Sex,
                HealthAuthority,
                EarliestPath = min(msg_dt),
                LatestPath = max(msg_dt),
                .groups = "drop") %>%
      distinct(Identifier, .keep_all = TRUE)
    
  })
  
  # Filtered pathology index - Selecting pathology IDs
  pathologyIDs <- reactive({
    
    req(allPathologyIDs())
    
    req(input$dateRange1, input$dateRange2,
        input$DXGroup,
        input$HAFilter,
        input$ageRange,
        input$sex)
    
    path_index <- allPathologyIDs() %>%
      filter(EarliestPath >= input$dateRange1 & LatestPath <= input$dateRange2) %>%
      filter(map_lgl(DXGroup, ~ any(.x %in% input$DXGroup))) %>%
      filter(HealthAuthority %in% input$HAFilter) %>%
      filter(between(Age, input$ageRange[1], input$ageRange[2])) %>%
      filter(Sex %in% input$sex)
    
  })
  
  # Large load - all imaging reports
  # One load/format at initialization - less computing down the road
  imagingData <- reactive({
  
    imagingQuery <- paste0("SELECT m.MessageId,
                                   p.Identifier,
                                   m.MessageDateTime,
                                   r.ExamTypeStd
                           FROM
                                  [bccr_img_path_DS].[img].[Message] m
                           JOIN
                                  [bccr_img_path_DS].[img].[Result] r
                                  ON m.MessageId = r.MessageId
                           JOIN
                                  [bccr_img_path_DS].[img].[Patient] p
                                  ON m.MessageId = p.MessageId")
    
    imagingReports <- dbGetQuery(imagingCon, imagingQuery) %>%
      mutate(MessageDateTime = ymd(as.Date(with_tz(ymd_hms(MessageDateTime), tzone = "America/Los_Angeles"))),
             ReportType = "Imaging") %>%
      rename(Label = ExamTypeStd) 
    
  })
  
  # Selecting all imaging IDs
  imagingIDs <- reactive({
    
    req(imagingData())
    
    # All Imaging Patients
    imagingIDs <- imagingData() %>%
      select(Identifier) %>%
      distinct(Identifier)
    
  })
  
  # Linked dataset of all imaging/pathology reports for patients associated with both datasets
  linkageSet <- reactive({
    
    req(rawPathology(), imagingData())
    
    # Loading in Pathology Reports - Filtering
    pathologyReports <- rawPathology() %>%
      filter(between(msg_dt, input$dateRange1, input$dateRange2)) %>%
      filter(map_lgl(final_label, ~ any(.x %in% input$DXGroup))) %>%
      filter(HealthAuthority %in% input$HAFilter) %>%
      filter(between(Age, input$ageRange[1], input$ageRange[2])) %>%
      filter(Sex %in% input$sex) %>%
      select(-HealthAuthority, -SEX, -BIRTHDATE, -Age, -Sex) %>%
      rename(MessageId = msgid,
             MessageDateTime = msg_dt,
             Label = final_label) %>%
      mutate(ReportType = "Pathology")
    
    # Loading in Imaging Data (from large load)
    imagingReports <- imagingData()
    
    # Building the Linked Set
    linkedSet <- bind_rows(imagingReports, pathologyReports) %>%
      filter(Identifier %in% intersect(imagingReports$Identifier, pathologyReports$Identifier)) %>%
      arrange(MessageDateTime, Identifier) 
    
    return(linkedSet)
    
  })
  
  # Index of unique patients associated with both imaging and pathology
  patientIndex <- reactive({
    
    req(pathologyIDs())
    
    patients <- pathologyIDs() %>%
      inner_join(imagingIDs(), by = "Identifier")
    
  })
  
  # Linked dataset - display output
  output$DTlinkageSet <- renderDT({
    
    datatable(linkageSet(), 
              filter = 'top', 
              options = list(
                pageLength = 10 
              )
    )
    
  })
  
  # Summary statistics for linkage set
  linkedSummarySet <- reactive({
    
    req(linkageSet(), imagingData())
    
    imagingData <- imagingData() 
      # filter(between(MessageDateTime, input$dateRange1, input$dateRange2))
    
    # Summarization Query
    summarise_query <- paste0("SELECT combined_label,
                                COUNT(*) AS Total
                              FROM
                                [eMaRCPlus_v60Patch].[dbo].[bccr_reportability_filter]
                              WHERE msg_dt BETWEEN '", 
                              input$dateRange1, "' AND '",
                              input$dateRange2, "'
                              GROUP BY combined_label")
    
    # Pathology Reports Summary
    pathologySummary <- dbGetQuery(pathCon, summarise_query) %>%
      mutate(combined_label = factor(combined_label,
                                     levels = c(0, 1),
                                     labels = c("Non-Reportable Pathology", "Reportable Pathology"))
      ) %>%
      rename(Metric = 1) %>%
      mutate(Percentage = percent(Total/sum(Total), accuracy = 0.01)) %>%
      bind_rows(summarise(., Metric = "All Pathology", Total = sum(Total), Percentage = "100.00%"))
    
    # Combining statistics
    newRows <- data.frame(
      Metric = c("All Imaging", "Linked Dataset"),
      Total = c(nrow(imagingData), nrow(linkageSet())),
      Percentage = c("100.00%", "100.00%")
    )
    
    linkedSetSummary <- linkageSet() %>%
      group_by(ReportType) %>%
      summarise(Total = n(), .groups = "drop") %>%
      mutate(Percentage = percent(Total/nrow(linkageSet()), 0.01)) %>%
      rename(Metric = 1)
    
    allSummary <- pathologySummary %>%
      bind_rows(newRows) %>%
      bind_rows(linkedSetSummary) 
    
    # print(allSummary)
    
  })
  
  # All Imaging/Pathology Reports for Selected Patient
  patientReports <- reactive({
    
    req(pathologyIDs()$Identifier %in% input$IdPHN)
    
    messages <- pathologyIDs() %>%
      filter(Identifier == input$IdPHN)
    
    # Pathology Query
    pathMessageQuery <- paste0("SELECT [msgid]
                                      ,[msg_dt]
                                      ,[section_type]
                                      ,[final_text]
                                      ,[message]
                                FROM [eMaRCPLus_v60Patch].[dbo].[bccr_reportability_filter]
                                WHERE [msgid] IN (",
                               messages[1, 3],
                               ")")
    
    # Imaging Query
    imagingMessageQuery <- paste0("SELECT 
                                      m.[MessageId],
                                      m.[ObservationDateTime],
                                      m.[ExamDescription],
                                      m.[ReasonForStudy],
                                      CAST(SUBSTRING(m.[ResultText], 1, 4000) AS VARCHAR(4000)) AS MsgText1,
                                      CAST(SUBSTRING(m.[ResultText], 4001, 4000) AS VARCHAR(4000)) AS MsgText2,
                                      CAST(SUBSTRING(m.[ResultText], 8001, 4000) AS VARCHAR(4000)) AS MsgText3,
                                      CAST(SUBSTRING(m.[ResultText], 12001, 4000) AS VARCHAR(4000)) AS MsgText4
                                  FROM 
                                      [bccr_img_path_DS].[img].[Result] m
                                  JOIN 
                                      [bccr_img_path_DS].[img].[Patient] p
                                      ON m.[MessageId] = p.[MessageId]
                                  WHERE 
                                      p.[Identifier] = '", input$IdPHN, "'")
    
    # Pull + Format - Pathology
    selectedPath <- dbGetQuery(pathCon, pathMessageQuery) %>%
      left_join(dxData, by = "msgid") %>%
      mutate("Report Type" = "Pathology",
             msgid = as.character(msgid),
             section_type = paste0(str_to_title(section_type), ": ", final_label)) %>%
      rename("Message ID" = 1,
             "Message Date" = 2,
             "Procedure" = 3, 
             "Brief Description" = 4,
             "Full Message" = 5) %>%
      select(1, 7, 2:5)
    
    # Pull + Format - Imaging
    selectedImaging <- dbGetQuery(imagingCon, imagingMessageQuery) %>%
      mutate("Full Message" = paste0(MsgText1, MsgText2, MsgText3, MsgText4),
             "Report Type" = "Imaging",
             ObservationDateTime = str_sub(ObservationDateTime, 1, 10),
             MessageId = as.character(MessageId)
      ) %>%
      left_join(codes, by = "ExamDescription") %>%
      mutate(ExamDescription = ifelse(is.na(GroupName), ExamDescription, GroupName)) %>%
      mutate(ExamDescription = case_when(str_detect(ExamDescription, "Consult|MAM-CON") ~ "MG Consultation",
                                         ExamDescription == "US-CON" ~ "US Consultation",
                                         ExamDescription == "CT-CON" ~ "CT Consultation",
                                         TRUE ~ ExamDescription)
      ) %>%
      rename("Message ID" = 1,
             "Message Date" = 2,
             "Procedure" = 3, 
             "Brief Description" = 4) %>%
      select(1, 10, 2:4, 9)
    
    # Combine rows
    selectedReports <- selectedPath %>%
      bind_rows(selectedImaging) %>%
      arrange(desc(!!sym("Message Date"))) 
    
  })
  
# Visualizations ----------------------------------------------------------
  
  # Linkage Data Flow Plot 
  output$linkageDiagram <- renderGrViz({
    
    summaryData <- linkedSummarySet()
    
    parseData <- function(category) {
      data <- summaryData[summaryData$Metric == category, ]
      label <- paste0(data[1], "\\n", data[2], " (", data[3], ")")
      return(label)
    }
    
    # print(summaryData)
    
    graphCode <- paste0("digraph {
                      
                      graph [layout = dot, rankdir = LR]
                      
                      # Define node styles
                      node [shape = rectangle, style = filled, fillcolor = SkyBlue]
                      
                      # Nodes
                      all_path     [label = '", parseData("All Pathology"), "']
                      report_path  [label = '", parseData("Reportable Pathology"), "']
                      non_path     [label = '", parseData("Non-Reportable Pathology"), "']
                      
                      all_img      [label = '", parseData("All Imaging"), "']
                      report_img   [label = 'Reportable Imaging\\n", 
                        summaryData[summaryData$Metric == "All Imaging", 2], " (100.00%)']
                      non_img      [label = 'Non-Reportable Imaging\\n NA']
                      
                      linked_set   [label = '", parseData("Linked Dataset"), "']
                      
                      linked_path  [label = '", parseData("Pathology"), "']
                      linked_img   [label = '", parseData("Imaging"), "']
                      
                      # Edges
                      all_path -> report_path
                      all_path -> non_path
                      all_img  -> report_img
                      all_img  -> non_img
                      
                      report_path -> linked_set
                      report_img  -> linked_set
                      
                      linked_set -> linked_path
                      linked_set -> linked_img
                      
                      # Rank constraints 
                      {rank = same; report_path; non_path}
                      {rank = same; report_img; non_img}
                      
                      # Invisible edges 
                      non_path -> report_path [style = invis]
                      report_img -> non_img [style = invis]
                      
                    }")
    
    grViz(graphCode)
    
  })
  
  # DT Output for Patient Selection
  output$patientSelectionTable <- renderDT({
    
    req(nrow(patientIndex()) >= 1)
    
    table <- patientIndex()
    
    datatable(table, selection = list(mode = "single", target = "row"))
    
  })
  
  # View a specific patient - search widget
  output$identifierSearch <- renderUI({
    
    req(patientIndex())
    
    pickerInput("IdPHN",
                choices = sort(unique(patientIndex()$Identifier)),
                options = pickerOptions(container = "body", 
                                        liveSearch = TRUE, 
                                        size = 15),
                multiple = FALSE,
                selected = patientIndex()[input$patientSelectionTable_rows_selected, "Identifier"],
                width = "110px"
    )
    
  })
  
  # Show All Patients Modal Dialog
  observeEvent(input$patientSelectAction, {
    
    req(input$patientSelectAction)
    
    updateModal()
    
  })
  
  # Show Report Message (from Plot Table)
  observeEvent(input$plotTable_rows_selected, {
    
    req(patientReports())
    
    type <- patientReports()[input$plotTable_rows_selected, "Report Type"]
    ID <- patientReports()[input$plotTable_rows_selected, "Message ID"]
    text <- patientReports()[input$plotTable_rows_selected, "Full Message"]
    
    output$formatted_message <- message_text_output(type, text)
    
    showModal(
      modalDialog(
        title = paste0("Showing Message #", ID, " (", type, ")"),
        infoBox(
          title = "Message Date",
          value = patientReports()[input$plotTable_rows_selected, "Message Date"],
          width = 6,
          icon = icon("calendar"),
          color = "blue"
        ),
        infoBox(
          title = "Label",
          value = patientReports()[input$plotTable_rows_selected, "Procedure"],
          width = 6,
          icon = icon("file-lines"),
          color = "blue"
        ),
        box(
          div(
            style = "max-height: 600px; overflow-y: auto;",
            uiOutput("formatted_message")
          ),
          solidHeader = TRUE,
          width = 12,
          title = paste0("View ", type, " Report"),
          collapsible = TRUE,
          collapsed = TRUE
        ),
        easyClose = TRUE,
        size = "l"
      )
    )
    
  })
  
  # Show Report Message (from TimeVis Plot)
  observeEvent(input$timelinePlot_selected, {
    
    req(patientReports())
    
    text <- patientReports()[patientReports()$'Message ID' == input$timelinePlot_selected, "Full Message"]
    
    type <- patientReports()[patientReports()$'Message ID' == input$timelinePlot_selected, "Report Type"]
    
    output$formatted_message <- message_text_output(type, text)

    showModal(
      modalDialog(
        title = paste0("Showing Message #", input$timelinePlot_selected, " (", type, ")"),
        infoBox(
          title = "Message Date",
          value = patientReports()[patientReports()$'Message ID' == input$timelinePlot_selected, "Message Date"],
          width = 6,
          icon = icon("calendar"),
          color = "blue"
        ),
        infoBox(
          title = "Label",
          value = patientReports()[patientReports()$'Message ID' == input$timelinePlot_selected, "Procedure"],
          width = 6,
          icon = icon("file-lines"),
          color = "blue"
        ),
        box(
          div(
            style = "max-height: 600px; overflow-y: auto;",
            uiOutput("formatted_message")
          ),
          solidHeader = TRUE,
          width = 12,
          title = paste0("View ", type, " Report"),
          collapsible = TRUE,
          collapsed = TRUE
        ),
        easyClose = TRUE,
        size = "l"
      )
    )
    
  })
  
  # Imaging/Pathology Reports for Selected Patient
  output$plotTable <- renderDT({
    
    req(pathologyIDs(), patientReports())
    
    data <- patientReports() %>%
      select(-6) %>%
      mutate(`Brief Description` = substr(`Brief Description`, 1, 100))
      
    datatable(data, selection = list(mode = "single", target = "row"))
    
  })
  
  # Timeline (Patient Journey) Visualization - All Reports
  output$timelinePlot <- renderTimevis({
    
    req(pathologyIDs()$Identifier %in% input$IdPHN)
    
    req(patientReports())
    
    data <- patientReports() %>%
      rename(id = 1,
             content = 4,
             start = 3,
             group = 2)
    
    groups <- data.frame(
      id = c("Pathology", "Imaging"),
      content = c("Pathology", "Imaging"),
      style = c("color: red;", "color: blue;")
    )
    
    timevis(data = data,
            groups = groups,
            height = 500,
            showZoom = TRUE)
    
  })
  
}
