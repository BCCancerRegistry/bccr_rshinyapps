# MATCHer UI ------------------------------------------------------

ui <- dashboardPage(skin = "yellow",
                    dashboardHeader(
                      title = tags$div(
                        tags$span(
                          class = "custom-title",
                          tagList(icon("dashboard")),
                          "MATCHer"
                        ),
                        tags$span(style = "font-size: 14px;", "BCCR")
                      )
                    ),
                    dashboardSidebar(
                      tags$head(
                        tags$style(
                          HTML(".scroll-cont {
                              position: fixed;
                              width: 225px;
                              top: -100px;
                              z-index: 1000;
                              }"
                          )
                        )
                      ),
                      br(),
                      actionBttn(
                        "restoreBackup",
                        label = "Restore backup?",
                        icon = icon("plus-circle"),
                        size = "sm",
                        color = "warning"
                      ),
                      br(),
                      # numericRangeInput(
                      #   "ID_Range",
                      #   label = "Filter Audit ID:",
                      #   min = min(audit$prod_msgid),
                      #   max = max(audit$prod_msgid),
                      #   value = c(min(audit$prod_msgid), max(audit$prod_msgid))
                      # ),
                      # conditionalPanel(
                      #   'input.searchID == true',
                      #   virtualSelectInput("ID_Range_2",
                      #                      label = "Filter Audit ID:",
                      #                      choices = unique(audit$audit_msgid),
                      #                      selected = NULL,
                      #                      multiple = TRUE,
                      #                      search = TRUE
                      #   )
                      br(),
                      pickerInput(
                        "batchSelect",
                        label = "Filter by audit batch:",
                        choices = unique(audit$Batch),
                        multiple = TRUE,
                        selected = unique(audit$Batch),
                        options = list(
                          "actions-box" = TRUE
                        )
                      ),
                      selectInput(
                        "filterSelect",
                        label = "Filter by match type:",
                        choices = filterChoices
                      ),
                      pickerInput(
                        "TR_R_Select",
                        label = "Filter TR Reportability:",
                        choices = c("Yes", "No", "N/A"),
                        multiple = TRUE,
                        selected = c("Yes", "No", "N/A"),
                        options = list(
                          "actions-box" = TRUE
                        )
                      ),
                      pickerInput(
                        "DS_R_Select",
                        label = "Filter DS Reportability:",
                        choices = c("Yes", "No", "N/A"),
                        multiple = TRUE,
                        selected = c("Yes", "No", "N/A"),
                        options = list(
                          "actions-box" = TRUE
                        )
                      ),
                      pickerInput(
                        "TR_DX_Select",
                        label = "Filter TR DX Groups:",
                        choices = dxGroups,
                        multiple = TRUE,
                        selected = dxGroups,
                        options = list(
                          "actions-box" = TRUE
                        )
                      ),
                      pickerInput(
                        "DS_DX_Select",
                        label = "Filter DS DX Groups:",
                        choices = dxGroups,
                        multiple = TRUE,
                        selected = dxGroups,
                        options = list(
                          "actions-box" = TRUE
                        )
                      ),
                      pickerInput(
                        "ccs_Blanks",
                        label = "Filter CCS Edited Audits:",
                        choices = c("Yes", "No"),
                        multiple = TRUE,
                        selected = c("Yes", "No"),
                        options = list(
                          "actions-box" = TRUE
                        )
                      )
                    ),
                    dashboardBody(
                      tags$head(
                        tags$script(HTML("
                          window.onbeforeunload = function(e) {
                            e.preventDefault();  // For some browsers
                            e.returnValue = '';  // Standard compliant
                          };
                        ")),
                        tags$style(HTML("
                            body, h1, h2, h3, h4, h5, h6 {
                              font-family: 'Arial Narrow', sans-serif;
                            }
                            .skin-red .main-header .logo {
                              font-family: 'Arial Narrow', sans-serif;
                            }
                            .skin-red .main-header .navbar {
                              font-family: 'Arial Narrow', sans-serif;
                            }
                            .skin-red .main-sidebar {
                              font-family: 'Arial Narrow', sans-serif;
                            }
                            .content-wrapper, .right-side {
                              font-family: 'Arial Narrow', sans-serif;
                            }
                              .tabbable > .nav > li > a {color:black; width: 230PX; font-size: 15px;
                            }
                              .btn-group .btn {
                                    font-size: 12px;
                            } .white-box {
                              background-color: white;
                              padding: 15px;
                            } .custom-title {
                              font-size: 24px;
                              font-weight: bold;
                            } .scroll-cont {
                              position: fixed;
                              top: 50px;
                              z-index: 1000;
                            };
                        "))
                      ),
                      fluidRow(
                        column(12,
                               box(
                                 DTOutput("mainTable"),
                                 status = "warning",
                                 width = 12,
                                 title = "Audit Data",
                                 solidHeader = TRUE
                               )
                        )
                      )
                    )
)