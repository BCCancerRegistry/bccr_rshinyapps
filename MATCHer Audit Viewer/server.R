#####

server <- function(input, output, session) {
  
  # Base server dataset
  mainData <- reactiveVal()
  
  # Applying filters
  observe({
    req(input$filterSelect, 
        input$TR_R_Select,
        input$DS_R_Select,
        input$TR_DX_Select,
        input$DS_DX_Select,
        input$dateFilter,
        input$ID_Range)
    
    filterAudit <- audit %>%
      filter(tr_reportability %in% input$TR_R_Select) %>%
      filter(ds_reportability %in% input$DS_R_Select) %>%
      filter(tr_dxgroup %in% input$TR_DX_Select) %>%
      filter(ds_dx_group %in% input$DS_DX_Select) %>%
      filter(import_dt_time %in% input$dateFilter) %>%
      filter(audit_msgid >= input$ID_Range[1] & audit_msgid <= input$ID_Range[2])
    
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
    
  
    if ("Missing SD Reportability" %in% input$css_Blanks) {
      filterAudit <- filterAudit %>%
        filter(is.na(filterAudit$sd_reportability))
    } 
    
    if ("Missing SD DX Group" %in% input$css_Blanks) {
      filterAudit <- filterAudit %>%
        filter(is.na(filterAudit$sd_dx_group))
    } 
    
    if ("Missing SD Comment" %in% input$css_Blanks) {
      filterAudit <- filterAudit %>%
        filter(filterAudit$sd_comment == "")
    } 
      
    mainData(filterAudit)
    
  })
  
  # Mini-table for DX group comparison within the modal dialog
  output$mainTable = renderDT({
    
    req(input$filterSelect, mainData())
    
    dataRender <- mainData() %>%
      select(-7, -8)
    
    datatable(
      dataRender,
      editable = "cell",
      selection = "single",
      options = list(
        pageLength = 20,
        ordering = FALSE,
        searching = FALSE
      )
    )
  })
  
  # Mini-table for reportability comparison within the modal dialog
  output$reportTable = renderTable({
    req(mainData(), rv$theRow)
    
    row <- rv$theRow
    
    mainData()[row, c(3:4)]
  }, width = "100%", striped = TRUE, bordered = TRUE, hover = TRUE)
  
  # Mini-table for DX group comparison within the modal dialog
  output$dxTable = renderTable({
    req(mainData(), rv$theRow)
    
    row <- rv$theRow
    
    mainData()[row, c(5:6)]
  }, width = "100%", striped = TRUE, bordered = TRUE, hover = TRUE)
  
  # Refreshing the modal dialog
  updateModal <- function() {
    req(mainData())
    
    if (length(rv$theRow) > 0) {
      showModal(
        modalDialog(
          title = paste0("Showing Audit #", mainData()$audit_msgid[rv$theRow]),
          # h1(message),
          box(
            title = "Reportability",
            tableOutput("reportTable"),
            br(),
            pickerInput(
              "sd_reportability_new",
              label = "SD Reportability:",
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
              "sd_dx_group_new",
              label = "SD DX Group:",
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
    
    message <- ""
    
    if (mainData()$match_reportability_ds_tr[rv$theRow] == 0) {
      message <- "Reportability mismatched"
    }
    
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
  
  # Modal Dialog Submit, to internal data
  observeEvent(input$submit, {
    
    req(mainData())
    
    # Updating the server environment
    newData <- mainData()
    
    newData$sd_reportability[rv$theRow] <- input$sd_reportability_new
    
    newData$sd_dx_group[rv$theRow] <- input$sd_dx_group_new
    
    newData$sd_comment[rv$theRow] <- input$comment
    
    mainData(newData)
    
    # Updating the global environment, data there
    audit$sd_reportability[rv$theRow] <<- input$sd_reportability_new
    
    audit$sd_dx_group[rv$theRow] <<- input$sd_dx_group_new
    
    audit$sd_comment[rv$theRow] <<- input$comment

    # Next row
    if (rv$theRow < nrow(mainData())) {
      rv$theRow <- rv$theRow + 1
      updateModal()
    }
    
    showNotification("Fields updated successfully!", type = "message")
    
  })
  
  # Submit function, to write changes to the server
  observeEvent(input$submitChanges, {
    
    # for (i in audit$audit_msgid) {
    #   if (audit$sd_comment != mainData()$sd_comment[i]) {
    #     audit$sd_comment<- mainData()$sd_comment[i]
    #   }
    # }
    
    audit$sd_dx_group <- mainData()$sd_dx_group[match(audit$audit_msgid, mainData()$audit_msgid)]
    
    audit$sd_reportability <- mainData()$sd_reportability[match(audit$audit_msgid, mainData()$audit_msgid)]
    
    audit$sd_comment <- mainData()$sd_comment[match(audit$audit_msgid, mainData()$audit_msgid)]
    
    query <- "
              UPDATE bccr_ss_audit_temp
              SET 
                  bccr_ss_audit_temp.ccs_dx_class = ?,
                  bccr_ss_audit_temp.ccs_reportability = ?,
                  bccr_ss_audit_temp.ccs_dx_class_comment = ?
              WHERE 
                  bccr_ss_audit_temp.audit_msgid = ?
            "
    
    for (i in 1:nrow(audit)) {
      dbExecute(con, query, params = list(audit$sd_dx_group[i], 
                                          audit$sd_reportability[i], 
                                          audit$sd_comment[i], 
                                          audit$audit_msgid[i]))
    }
    
    })
}
  
   