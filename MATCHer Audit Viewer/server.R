##### MATCHer Server

server <- function(input, output, session) {
  
  # Production Server Connection
  productionCon <- dbConnect(odbc::odbc(),
                             Driver = "SQL Server",
                             Server = "spdbsmarc001",
                             Database = "eMaRCPlus_V90_Audit",
                             Trusted_Connection = "Yes")
  
  # Backup RDS Properties
  output$backupProperty <- renderDT({
    
    if (file.exists("insert backup file directory here")) {
      data <- as.data.frame(file.info("insert backup file directory here")) %>%
        select(4, 8) %>%
        mutate(
          mtime = mtime %>%
            ymd_hms(tz = "UTC") %>%
            format("%Y-%m-%d %H:%M:%S")
        ) %>%
        rename("Last Modified" = 1,
               "User" = 2)
      
      datatable(
        data,
        options = list(
          dom = 't' 
        )
      )
    }
    
  })
  
  # Modal Dialog - For selecting whether to restore previous state
  observe({
    if (file.exists("insert backup file directory here")) {
      showModal(
        modalDialog(
          title = "Load Previous State?",
          box(title = "Previous State Properties",
              DTOutput("backupProperty"),
              width = 12,
              solidHeader = TRUE,
              status = "warning",
              br(),
              fluidRow(
                column(6, align = "center",
                       actionBttn("restore",
                                  label = "Restore State",
                                  color = "warning",
                                  style = "jelly")
                ),
                column(6, align = "center",
                       actionBttn("clean",
                                  label = "Use Empty State",
                                  color = "warning")
                )
              )
          ),
          easyClose = FALSE,
          size = "l"
        )
      )
    }
  })
  
  # Loading pulling specific message
  output$selectedMessage <- renderText({
    
    req(mainData())
    
    messageQuery <- paste0("SELECT [message] FROM [eMaRCPLus_v60Patch].[dbo].[bccr_reportability_filter] WHERE msgid = ",
                           mainData()$prod_msgid[rv$theRow])
    
    messageText <- dbGetQuery(productionCon, messageQuery)
    
    return(messageText[1, 1])
    
  })
  
  # Baseline dataset
  baseData <- reactiveVal(audit)
  
  # Dataset for filtering and viewing
  mainData <- reactiveVal()
  
  # Observe - Restore Previous State
  observeEvent(input$restore, {
    # Merge inputs from backup to baseData
    baseData(backup)
    removeModal()
  })
  
  # Observe - Do not restore previous state (clean slate)
  observeEvent(input$clean, {
    removeModal()
  })
  
  # Applying filters - for viewing
  observe({
    
    req(input$filterSelect, 
        input$TR_R_Select,
        input$DS_R_Select,
        input$TR_DX_Select,
        input$DS_DX_Select,
        input$dateFilter,
        input$ID_Range,
        input$ccs_Blanks)
    
    filterAudit <- baseData() %>%
      filter(tr_reportability %in% input$TR_R_Select) %>%
      filter(ds_reportability %in% input$DS_R_Select) %>%
      filter(tr_dxgroup %in% input$TR_DX_Select) %>%
      filter(ds_dx_group %in% input$DS_DX_Select) %>%
      filter(import_dt_time %in% input$dateFilter) %>%
      filter(audit_msgid >= input$ID_Range[1] & audit_msgid <= input$ID_Range[2]) %>%
      filter(ccs_edited %in% input$ccs_Blanks)
      
    if (input$searchID == TRUE) {
      filterAudit <- filterAudit %>%
        filter(audit_msgid %in% input$ID_Range_2)
    } else if (input$searchID == FALSE) {
      filterAudit <- filterAudit %>%
        filter(audit_msgid >= input$ID_Range[1] & audit_msgid <= input$ID_Range[2])
    }
    
    filterAudit <- switch(input$filterSelect,
                          "All Full Matches" = filterAudit %>%
                            filter(match_reportability_ds_tr == 1 & match_dxclass_ds_tr == 1),
                          "All Mismatches" = filterAudit %>%
                            filter(match_reportability_ds_tr == 0 | match_dxclass_ds_tr == 0),
                          "Mismatched Reportability" = filterAudit %>%
                            filter(match_reportability_ds_tr == 0),
                          "Mismatched DX Groups" = filterAudit %>%
                            filter(match_dxclass_ds_tr == 0),
                          filterAudit
    )
    
    # Sort by ascending audit_msgid
    filterAudit <- filterAudit %>%
      arrange(audit_msgid)
      
    # Merge
    mainData(filterAudit)
    
  })
  
  # Main Table for Editing
  output$mainTable <- renderDT({
    
    req(input$filterSelect, mainData())
    
    page <- isolate(input$current_page)
    page <- if (is.null(page)) 1 else page
    
    render <- mainData() %>%
      select(1:7, 10:13)
    
    datatable(
      render,
      editable = "cell",
      selection = "single",
      options = list(
        pageLength = 20,
        ordering = FALSE,
        searching = FALSE,
        drawCallback = JS(
          "function(settings) {",
          "  var page = settings._iDisplayStart / settings._iDisplayLength + 1;",
          "  Shiny.setInputValue('current_page', page, {priority: 'event'});",
          "}"
        ),
        initComplete = JS(
          sprintf(
            "function(settings, json) { this.api().page(%s - 1).draw(false); }",
            page
          )
        )
      ),
      rownames = FALSE
    )
    
  }, server = TRUE)
  
  # Mini-table for reportability comparison within the modal dialog
  output$reportTable = renderTable({
    req(mainData(), rv$theRow)
    
    row <- rv$theRow
    
    mainData()[row, c(4:5)]
  }, width = "100%", striped = TRUE, bordered = TRUE, hover = TRUE)
  
  # Mini-table for DX group comparison within the modal dialog
  output$dxTable = renderTable({
    req(mainData(), rv$theRow)
    
    row <- rv$theRow
    
    mainData()[row, c(6:7)]
  }, width = "100%", striped = TRUE, bordered = TRUE, hover = TRUE)
  
  # Refreshing the modal dialog
  updateModal <- function() {
    
    req(mainData())
    
    if (length(rv$theRow) > 0) {
      showModal(
        modalDialog(
          title = paste0("Showing Audit #", mainData()$audit_msgid[rv$theRow],
                         " (Message #", mainData()$prod_msgid[rv$theRow], ")"),
          box(
            div(
              style = "max-height: 300px; overflow-y: auto;",
              htmlOutput("selectedMessage")
            ), 
            solidHeader = TRUE,
            width = 12,
            title = "View Full Message",
            collapsible = TRUE,
            collapsed = TRUE
          ),
          box(
            title = "Reportability",
            tableOutput("reportTable"),
            br(),
            pickerInput(
              "ccs_reportability_new",
              label = "CCS Reportability:",
              choices = c("Yes", "No", NA),
              selected = mainData()$tr_reportability[rv$theRow]
            ),
            status = "warning",
            solidHeader = TRUE
          ),
          box(
            title = "DX Group",
            tableOutput("dxTable"),
            br(),
            pickerInput(
              "ccs_dx_class_new",
              label = "CCS DX Group:",
              choices = c(dxGroups, NA),
              selected = mainData()$tr_dxgroup[rv$theRow]
            ),
            status = "warning",
            solidHeader = TRUE
          ),
          textInput(
            "comment",
            "Add comment:",
            placeholder = "Insert text here...",
            width = 1000
          ),
          footer = tagList(
            actionBttn("submit", 
                       "Submit Changes",
                       color = "warning",
                       block = TRUE),
            br(),
            actionButton("prevOne", "Previous"),
            actionButton("nextOne", "Next"),
            modalButton("Close")
          ),
          easyClose = TRUE,
          size = "l"
        )
      )
    }
  }
  
  # Row index
  rv <- reactiveValues(theRow = NULL)
  
  # Modal Dialog
  observeEvent(input$mainTable_rows_selected, {
    
    req(mainData(), input$mainTable_rows_selected)
    
    rv$theRow <- input$mainTable_rows_selected
    
    table <- c(mainData()$audit_msgid[rv$theRow])
    
    updateModal()
    
  })
  
  # Modal Dialog Previous Row
  observeEvent(input$prevOne, {
    
    req(mainData())
    
    if (rv$theRow > 1) {
      rv$theRow <- rv$theRow - 1
    }
    
    updateModal()
    
  })
  
  # Modal Dialog Next Row
  observeEvent(input$nextOne, {
    
    req(mainData())
    
    if (rv$theRow <= nrow(mainData())) {
      rv$theRow <- rv$theRow + 1 
    }
    
    updateModal()
    
  })
  
  # Update local datasets
  observeEvent(input$submit, {
    
    req(mainData())
    
    auditIndex <- mainData()$audit_msgid[rv$theRow]
    
    # Updating dataset for viewing
    newData <- mainData()
   
    newData$ccs_reportability[newData$audit_msgid == auditIndex] <- input$ccs_reportability_new
    newData$ccs_dx_class[newData$audit_msgid == auditIndex] <- input$ccs_dx_class_new
    newData$ccs_dx_class_comment[newData$audit_msgid == auditIndex] <- input$comment
    newData$ccs_edited[newData$audit_msgid == auditIndex] <- "Yes"
    
    mainData(newData)
    
    # Updating baseline dataset for editing and writing
    baseData <- baseData()
    
    baseData$ccs_reportability[baseData$audit_msgid == auditIndex] <- input$ccs_reportability_new
    baseData$ccs_dx_class[baseData$audit_msgid == auditIndex] <- input$ccs_dx_class_new
    baseData$ccs_dx_class_comment[baseData$audit_msgid == auditIndex] <- input$comment
    baseData$ccs_edited[baseData$audit_msgid == auditIndex] <- "Yes"
    
    baseData(baseData)

    # Next row
    if (rv$theRow < nrow(mainData())) {
      rv$theRow <- rv$theRow + 1
      updateModal()
    }
    
    # Save backup
    saveRDS(baseData(), file = "audit_input_backup.rds")
    
    showNotification(paste0("Fields updated successfully!"), type = "message")
    
  })
  
  # Submit function, to write changes to the server
  observeEvent(input$submitChanges, {
    
    edited <- baseData() %>%
      filter(ccs_edited == "Yes") %>%
      select(audit_msgid,
             ccs_reportability,
             ccs_dx_class,
             ccs_dx_class_comment)
    
    for (i in 1:nrow(edited)) {
      
      auditIndex <- edited$audit_msgid[i]
      reportability <- ifelse(edited$ccs_reportability == "Yes", 1, 0)
      dx_group <- edited$ccs_dx_class[i]
      dx_comment <- edited$ccs_dx_class_comment[i]
      
      auditIndex <- edited$audit_msgid[i]
      
      writeQuery <- paste0("UPDATE [eMaRCPlus_V90_Audit].[ dbo].[bccr_ss_audit_temp] SET ",
                           "ccs_reportability = ", reportability, ", ",
                           "ccs_dx_class = '", dx_group, "', ",
                           "ccs_dx_class_comment = '", dx_comment,
                           "' WHERE audit_msgid = ", auditIndex, ";")
    }
    
    dbExecute(con, writeQuery)
    
    })
  
  observeEvent(input$submitChanges, {
    showModal(
      modalDialog(
        title = "Success",
        h4("Changes submitted to [bccr_ss_audit_temp]!")
      )
    )
  })
}
