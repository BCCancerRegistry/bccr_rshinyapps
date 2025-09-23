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
    
    if (file.exists("C:/Users/howard.zhang/Downloads/MATCHer/audit_input_backup.rds")) {
      data <- as.data.frame(file.info("C:/Users/howard.zhang/Downloads/MATCHer/audit_input_backup.rds")) %>%
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
  observeEvent(input$restoreBackup, {
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
                                label = "Dismiss",
                                color = "warning")
              )
            )
        ),
        easyClose = FALSE,
        size = "l"
      )
    )
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
    
    req(input$TR_R_Select,
        input$DS_R_Select,
        input$TR_DX_Select,
        input$DS_DX_Select,
        input$batchSelect,
        input$ccs_Blanks)
    
    filterAudit <- baseData() %>%
      filter(Batch %in% input$batchSelect) %>%
      filter(tr_reportability %in% input$TR_R_Select) %>%
      filter(ds_reportability %in% input$DS_R_Select) %>%
      filter(tr_dxgroup %in% input$TR_DX_Select) %>%
      filter(ds_dx_group %in% input$DS_DX_Select) %>%
      filter(ccs_edited %in% input$ccs_Blanks)
      
    # if (input$searchID == TRUE) {
    #   filterAudit <- filterAudit %>%
    #     filter(prod_msgid %in% input$ID_Range_2)
    # } else if (input$searchID == FALSE) {
    #   filterAudit <- filterAudit %>%
    #     filter(prod_msgid >= input$ID_Range[1] & prod_msgid <= input$ID_Range[2])
    # }
    
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
    
    # Sort by ascending prod_msgid
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
      select(1:10)
    
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
    
    mainData()[row, c("tr_reportability", "ds_reportability")]
  }, width = "100%", striped = TRUE, bordered = TRUE, hover = TRUE)
  
  # Mini-table for DX group comparison within the modal dialog
  output$dxTable = renderTable({
    req(mainData(), rv$theRow)
    
    row <- rv$theRow
    
    mainData()[row, c("tr_dxgroup", "ds_dx_group")]
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
              choices = c("Yes", "No", "N/A", ""),
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
              choices = c(dxGroups, ""),
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
    
    table <- c(mainData()$prod_msgid[rv$theRow])
    
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
    
    auditIndex <- mainData()$prod_msgid[rv$theRow]
    
    # Updating dataset for viewing
    newData <- mainData()
   
    newData$ccs_reportability[newData$prod_msgid == auditIndex] <- input$ccs_reportability_new
    newData$ccs_dx_class[newData$prod_msgid == auditIndex] <- input$ccs_dx_class_new
    newData$ccs_dx_class_comment[newData$prod_msgid == auditIndex] <- input$comment
    
    if (input$ccs_reportability_new == "" & input$ccs_dx_class_new == "") {
      newData$ccs_edited[newData$prod_msgid == auditIndex] <- "No"
    } else {
      newData$ccs_edited[newData$prod_msgid == auditIndex] <- "Yes"
    }
    
    mainData(newData)
    
    # Updating baseline dataset for editing and writing
    baseData <- baseData()
    
    baseData$ccs_reportability[baseData$prod_msgid == auditIndex] <- input$ccs_reportability_new
    baseData$ccs_dx_class[baseData$prod_msgid == auditIndex] <- input$ccs_dx_class_new
    baseData$ccs_dx_class_comment[baseData$prod_msgid == auditIndex] <- input$comment
    
    if (input$ccs_reportability_new == "" & input$ccs_dx_class_new == "") {
      baseData$ccs_edited[baseData$prod_msgid == auditIndex] <- "No"
    } else {
      baseData$ccs_edited[baseData$prod_msgid == auditIndex] <- "Yes"
    }
    
    baseData(baseData)
    
    write.csv(baseData, "audit_data.csv", row.names = FALSE)

    # Next row
    if (rv$theRow < nrow(mainData())) {
      rv$theRow <- rv$theRow + 1
      updateModal()
    }
    
    # Save backup
    saveRDS(baseData(), file = "audit_input_backup.rds")
    
    showNotification(paste0("Fields updated successfully to H:/Data Integrity Analyst/Data Science/dashboard data/MATCHer/.csv version/audit_data.csv"),
                     type = "message")
    
  })
  
  # Submit function, to write changes to the server
  # observeEvent(input$submitChanges, {
  #   
  #   edited <- baseData() %>%
  #     filter(ccs_edited == "Yes") %>%
  #     select(prod_msgid,
  #            ccs_reportability,
  #            ccs_dx_class,
  #            ccs_dx_class_comment)
  #   
  #   for (i in 1:nrow(edited)) {
  #     
  #     auditIndex <- edited$prod_msgid[i]
  #     reportability <- ifelse(edited$ccs_reportability == "Yes", 1, 0)
  #     dx_group <- edited$ccs_dx_class[i]
  #     dx_comment <- edited$ccs_dx_class_comment[i]
  #     
  #     auditIndex <- edited$prod_msgid[i]
  #     
  #     writeQuery <- paste0("UPDATE [eMaRCPlus_V90_Audit].[ dbo].[bccr_ss_audit_temp] SET ",
  #                          "ccs_reportability = ", reportability, ", ",
  #                          "ccs_dx_class = '", dx_group, "', ",
  #                          "ccs_dx_class_comment = '", dx_comment,
  #                          "' WHERE prod_msgid = ", auditIndex, ";")
  #   }
  #   
  #   dbExecute(con, writeQuery)
  #   
  #   })
  
  # observeEvent(input$submitChanges, {
  #   showModal(
  #     modalDialog(
  #       title = "Success",
  #       h4("Changes submitted to [bccr_ss_audit_temp]!")
  #     )
  #   )
  # })
}





# dbExecute(con, query, params = list(audit$ccs_dx_class[i], 
#                                     audit$ccs_reportability[i], 
#                                     audit$ccs_dx_class_comment[i], 
#                                     audit$prod_msgid[i]))
  

# audit$ccs_dx_class <- mainData()$ccs_dx_class[match(audit$prod_msgid, mainData()$prod_msgid)]
# 
# audit$ccs_reportability <- mainData()$ccs_reportability[match(audit$prod_msgid, mainData()$prod_msgid)]
# 
# audit$ccs_dx_class_comment <- mainData()$ccs_dx_class_comment[match(audit$prod_msgid, mainData()$prod_msgid)]
   