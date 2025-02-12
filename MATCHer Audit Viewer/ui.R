
ui <- dashboardPage(skin = "yellow",
                    dashboardHeader(
                      title = tags$div(
                        tags$span(
                          class = "custom-title",
                          tagList(icon("dashboard")),
                          "MATCHer",
                        ),
                        tags$span(style = "font-size: 14px;",
                                 "BCCR"),
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
                        "submitChanges",
                        label = "Save changes to server",
                        icon = icon("plus-circle"),
                        size = "sm",
                        color = "warning",
                        style = "jelly"
                      ),
                      br(),
                      conditionalPanel(
                        'input.searchID == false',
                        numericRangeInput(
                          "ID_Range",
                          label = "Filter Audit ID:",
                          min = min(audit$audit_msgid),
                          max = max(audit$audit_msgid), 
                          value = c(min(audit$audit_msgid), max(audit$audit_msgid))
                        )
                      ),
                      conditionalPanel(
                        'input.searchID == true',
                        virtualSelectInput("ID_Range_2",
                                           label = "Filter Audit ID:",
                                           choices = unique(audit$audit_msgid),
                                           selected = NULL,
                                           multiple = TRUE,
                                           search = TRUE
                        )
                      ),
                      materialSwitch(
                        "searchID",
                        label = "Filter audits by individual ID?",
                        value = FALSE
                      ),
                      br(),
                      selectInput(
                        "filterSelect",
                        label = "Filter by match type:",
                        choices = filterChoices
                      ),
                      pickerInput(
                        "dateFilter",
                        label = "Filter Month:",
                        choices = unique(audit$import_dt_time),
                        multiple = TRUE,
                        selected = unique(audit$import_dt_time)
                      ),
                      pickerInput(
                        "TR_R_Select",
                        label = "Filter TR Reportability:",
                        choices = unique(audit$tr_reportability),
                        multiple = TRUE,
                        selected = unique(audit$tr_reportability)
                      ),
                      pickerInput(
                        "DS_R_Select",
                        label = "Filter DS Reportability:",
                        choices = unique(audit$ds_reportability),
                        multiple = TRUE,
                        selected = unique(audit$ds_reportability)
                      ),
                      pickerInput(
                        "TR_DX_Select",
                        label = "Filter TR DX Groups:",
                        choices = dxGroups,
                        multiple = TRUE,
                        selected = dxGroups
                      ),
                      pickerInput(
                        "DS_DX_Select",
                        label = "Filter DS DX Groups:",
                        choices = dxGroups,
                        multiple = TRUE,
                        selected = dxGroups
                      ),
                      pickerInput(
                        "css_Blanks",
                        label = "Filter unresolved audits:",
                        choices = c("Missing SD Reportability",
                                    "Missing SD DX Group", 
                                    "Missing SD Comment"),
                        multiple = TRUE,
                        selected = NULL
                      )
                    ),
                    dashboardBody(
                      tags$head(
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
                            }
                        "))
                      ),
                      fluidRow(
                        column(12,
                               box(
                                 DTOutput("mainTable"),
                                 status = "warning",
                                 width = 1000,
                                 title = "Audit Data",
                                 solidHeader = TRUE
                               )
                        )
                      )
                    )
)