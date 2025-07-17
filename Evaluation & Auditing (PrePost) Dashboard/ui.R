# Evaluation/Auto-Coding Dashboard UI -------------------------------------

ui <- dashboardPage(skin = "purple",
                    dashboardHeader(
                      title = tags$div(
                        tags$span(
                          class = "custom-title",
                          tagList(icon("dashboard")),
                          "PrePost",
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
                      conditionalPanel(
                        'input.tabs == "Review Flags" || input.tabs == "Pre- & Post-Consolidation" ||
                        input.tabs == "Post-Onco Changes"',
                        pickerInput(
                          "yearFilter",
                          label = "Select year(s) to include:",
                          choices = c(2022, 2023, 2024, 2025),
                          multiple = TRUE,
                          selected = c(2022, 2023, 2024, 2025)
                        )
                      ),
                      conditionalPanel(
                        'input.tabs == "Pre- & Post-Consolidation"',
                        selectizeInput(
                          "dxSelectComp",
                          label = "Select DX Group:",
                          choices = c("BR", "CR", "LK", "LU", "PR")
                        ),
                        selectizeInput(
                          "chooseView",
                          label = "Metric to view:",
                          choices = c("Count", "Proportion", "N Consolidated", "Proportion Consolidated")
                        )
                      ),
                      conditionalPanel(
                        'input.tabs == "Post-Onco Changes"',
                        pickerInput(
                          "TFDReviewFlag",
                          label = "Select TFD Review Flag:",
                          choices = c("Yes", "No"),
                          selected = c("Yes", "No"),
                          multiple = TRUE
                        ),
                        selectizeInput(
                          "dxGroupPCC",
                          label = "Select DX Group:",
                          choices = c("BR", "CR", "LK", "LU", "PR")
                        ),
                        selectizeInput(
                          "viewFormatPCC",
                          label = "Change view format:",
                          choices = c("Total Bar Graph", "Table")
                        ),
                        uiOutput("proportionTogglePCC")
                      ),
                      conditionalPanel(
                        'input.tabs == "Review Flags"',
                        pickerInput(
                          "excludeOutofDate",
                          label = "Exclude cases where Review Reason is 'Out of Date'?",
                          choices = c("Yes", "No"),
                          multiple = FALSE
                        ),
                        pickerInput(
                          "percentToggle",
                          label = "Metric to view:",
                          choices = c("Count", "Proportion"),
                          selected = "Count"
                        )
                      ),
                      conditionalPanel(
                        'input.tabs == "Audit Data"',
                        pickerInput(
                          "matrixView",
                          label = "Select Audit Data:",
                          choices = c("Diagnostic Group", "Reportability", "Surveillance")
                        ),
                        uiOutput("auditTimeFilter"),
                        uiOutput("surveillanceMetric"),
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
                              padding: 10px;
                              border-radius: 5px;
                              width: 100%;
                              display: flex;
                              flex-wrap: wrap;
                              border-radius: 0px; 
                              justify-content: space-between;
                            } .custom-title {
                              font-size: 24px;
                              font-weight: bold;
                            } 
                        "))
                      ),
                      tabsetPanel(
                        tabPanel("Pre- & Post-Consolidation",
                                 fluidRow(
                                   column(12,
                                          div(class = 'white-box',
                                              box(
                                                title = "Note",
                                                htmlOutput("prePostText"),
                                                solidHeader = TRUE,
                                                width = 12
                                              )
                                          )
                                   )
                                 ),
                                 fluidRow(
                                   column(12,
                                          div(class = "white-box",
                                              infoBox(value = textOutput("nrowComp"),
                                                      "Total Pathology Reports",
                                                      color = "purple",
                                                      icon = icon("server")
                                              ),
                                              infoBox(value = textOutput("SHComp"),
                                                      "Most Frequently Predicted Site and Histology",
                                                      color = "purple",
                                                      icon = icon("clipboard-list")
                                              ),
                                              infoBox(value = textOutput("percentReduced"),
                                                      "Total Reports Consolidated",
                                                      color = "purple",
                                                      icon = icon("chart-line")
                                              )
                                          )
                                   )
                                 ),
                                 fluidRow(
                                   column(12,
                                          div(class = "white-box",
                                              box(
                                                title = "Site",
                                                plotlyOutput("sitePlotComp"),
                                                textOutput("siteText"),
                                                width = 6
                                              ),
                                              box(
                                                title = "Histology",
                                                plotlyOutput("histologyPlotComp"),
                                                textOutput("histologyText"),
                                                width = 6
                                              )
                                          )
                                   )
                                 ),
                                 fluidRow(
                                   column(12,
                                          div(class = "white-box",
                                              box(
                                                title = "Behaviour",
                                                plotlyOutput("behaviourPlotComp"),
                                                textOutput("behaviourText"),
                                                width = 4
                                              ),
                                              box(
                                                title = "Laterality",
                                                plotlyOutput("lateralityPlotComp"),
                                                textOutput("lateralityText"),
                                                width = 4
                                              ),
                                              box(
                                                title = "Lymph Vascular Invasion",
                                                plotlyOutput("LVIPlotComp"),
                                                textOutput("LVIText"),
                                                width = 4
                                              )
                                          )
                                   )
                                 )
                              ),
                        tabPanel("Review Flags",
                                 fluidRow(
                                   column(12,
                                          div(class = 'white-box',
                                              box(
                                                title = "Note",
                                                htmlOutput("reviewReasonsText"),
                                                solidHeader = TRUE,
                                                width = 12
                                              )
                                          )
                                   )
                                 ),
                                 fluidRow(
                                   column(12,
                                          div(class = "white-box",
                                              infoBox(value = textOutput("totalReports"),
                                                      "N Total Reports",
                                                      color = "purple",
                                                      icon = icon("flag")
                                              ),
                                              infoBox(value = textOutput("nReview"),
                                                      "N Review Flag",
                                                      color = "purple",
                                                      icon = icon("table")
                                              ),
                                              infoBox(value = textOutput("propReview"),
                                                      "Review Flag Percentage",
                                                      color = "purple",
                                                      icon = icon("file-export")
                                              )
                                          )
                                   )
                                 ),
                                 fluidRow(
                                   column(12,
                                          div(class = 'white-box',
                                              box(
                                                title = "Positive Review Flag by DX Group",
                                                plotlyOutput("reviewByDX", height = "643px"),
                                                width = 4
                                              ),
                                              box(
                                                title = "Reasons for Review",
                                                fluidRow(
                                                  column(6,
                                                         pickerInput("reasonDX",
                                                                     label = "Select DX Group(s):",
                                                                     choices = c("BR", "CR", "LK", "LU", "PR"),
                                                                     multiple = TRUE,
                                                                     selected = c("BR", "CR", "LK", "LU", "PR")
                                                         ),
                                                         materialSwitch(
                                                           "dxCompareToggle",
                                                           label = "Compare across DX groups?",
                                                           value = FALSE
                                                         )
                                                  )
                                                ),
                                                plotlyOutput("reviewReasons", height = "530px"),
                                                width = 8
                                              )
                                          )
                                   )
                                 )
                              ),
                        tabPanel("Post-Onco Changes",
                                 fluidRow(
                                   column(12,
                                          div(class = 'white-box',
                                              box(
                                                title = "Note",
                                                htmlOutput("PCCText"),
                                                solidHeader = TRUE,
                                                width = 12
                                              )
                                          )
                                   )
                                 ),
                                 fluidRow(
                                   column(12,
                                          div(class = "white-box",
                                              box(
                                                title = "Site",
                                                uiOutput("sitePCC"),
                                                width = 6
                                              ),
                                              box(
                                                title = "Histology",
                                                uiOutput("histologyPCC"),
                                                width = 6
                                              )
                                          )
                                   )
                                 ),
                                 fluidRow(
                                   column(12,
                                          div(class = "white-box",
                                              box(
                                                title = "Laterality",
                                                uiOutput("lateralityPCC"),
                                                width = 6
                                              ),
                                              box(
                                                title = "Lymph Vascular Invasion",
                                                uiOutput("LVIPCC"),
                                                width = 6
                                              )
                                          )
                                   )
                                 )
                          ),
                        tabPanel("Audit Data",
                                 fluidRow(
                                   column(12,
                                          div(class = 'white-box',
                                              box(
                                                title = "Note",
                                                htmlOutput("auditText"),
                                                solidHeader = TRUE,
                                                width = 12
                                              )
                                          )
                                   )
                                 ),
                                 fluidRow(
                                   column(12,
                                          div(class = "white-box",
                                              infoBox(value = textOutput("ssAccuracy"),
                                                      "Overall Accuracy",
                                                      icon = icon("check"), 
                                                      color = "purple"
                                              ),
                                              infoBox(value = textOutput("ssConfInt"),
                                                      "Accuracy 95% Confidence Interval",
                                                      icon = icon("chart-line"),
                                                      color = "purple"
                                              ),
                                              infoBox(value = textOutput("ssDim"),
                                                      "Number of Observations",
                                                      icon = icon("filter"),
                                                      color = "purple"
                                              ),
                                              style = "height:140px"
                                          )
                                   )
                                 ),
                                 fluidRow(
                                   column(12,
                                          div(class = "white-box",
                                              plotlyOutput("matrixPlot", height = "600px"),
                                          )
                                   )
                                 ),
                                 fluidRow(
                                   column(12,
                                          div(class = "white-box",
                                              box(DTOutput("matrixSummary"),
                                                  paste("Precision: Proportion of true positives out of all predicted positives
                                                        ([TP]/[TP + FP])"),
                                                  br(),
                                                  paste("Recall: Proportion of true positives out of actual positives
                                                        ([TP]/[TP + FN])"),
                                                  width = 12, 
                                                  title = "Summary Table",
                                                  solidHeader = FALSE
                                              )
                                          )
                                   )
                                 )
                        ),
                        id = "tabs"
                      )
                    )
)