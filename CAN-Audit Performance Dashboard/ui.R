
ui <- dashboardPage(skin = "green",
                    dashboardHeader(
                      title = tags$div(
                        tags$span(
                          class = "custom-title",
                          tagList(icon("dashboard")),
                          "CanAudit",
                        ),
                        tags$span(style = "font-size: 14px;",
                                 "BCCR"),
                        )
                    ),
                    dashboardSidebar(
                      useShinyjs(),
                      conditionalPanel(
                        'input.tabs == "Time Series"',
                        sidebarMenu(
                          menuItem("Series 1", 
                                   icon = icon("sliders"),
                                   selectInput(
                                     "dxNum1",
                                     label = "Select DX group:",
                                     choices = dxGroups,
                                     selected = "BR"
                                   ),
                                   selectInput(
                                     "metricNum1",
                                     label = "Select metric group:",
                                     choices = metricGroups,
                                     selected = "Recall"
                                   )
                          )
                        ),
                        sidebarMenu(
                          menuItem("Series 2", 
                                   icon = icon("sliders"),
                                   selectInput(
                                     "dxNum2",
                                     label = "Select DX group:",
                                     choices = dxGroups,
                                     selected = "LK"
                                   ),
                                   selectInput(
                                     "metricNum2",
                                     label = "Select metric group:",
                                     choices = metricGroups,
                                     selected = "Recall"
                                   )
                          )
                        )
                      ),
                      conditionalPanel(
                        'input.tabs == "Diagnostic & Reportability Audits"',
                        switchInput(
                          "switchSS",
                          label = "Series",
                          onStatus = FALSE,
                          onLabel = "Reportability",
                          offLabel = "Diagnostic Groups"
                        ),
                        selectInput(
                          "monthView",
                          label = "Select audit months:",
                          choices = unique(accuracyData$month),
                          selected = accuracyData$month,
                          multiple = TRUE
                          )
                        ),
                      conditionalPanel(
                        'input.tabs == "Surveillance Audits"',
                        selectInput(
                          "dxGroupSelect",
                          label = "Select diagnostic group:",
                          choices = c("BR", "PR"),
                          selected = "PR"
                        ),
                        selectInput(
                          "svlSelect",
                          label = "Select surveillance metric:",
                          choices = svlGroups
                        ),
                        conditionalPanel(
                          'output.sequenceOn == true',
                          selectInput(
                            "svlSlider",
                            label = "View rows by descending frequency:",
                            choices = seq(from = 0, to = 10, by = 5),
                            selected = 5
                          )
                        )
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
                            } 
                        "))
                      ),
                      tabsetPanel(
                        tabPanel("Time Series",
                                 fluidRow(
                                   column(12,
                                          div(class = "white-box",
                                              box(plotlyOutput("overallPlot", height = "600px"),
                                                  width = 800, 
                                                  status = "success",
                                                  title = "Overall Metrics Time-Series",
                                                  solidHeader = TRUE
                                              ),
                                              style = "height:700px"
                                          )
                                   )
                                 ),
                                 fluidRow(
                                   column(12,
                                          div(class = "white-box",
                                              box(DTOutput("overallSummary"),
                                                  width = 800, 
                                                  status = "success",
                                                  title = "Summary",
                                                  solidHeader = TRUE
                                              ),
                                              style = "height:450px"
                                          )
                                   )
                                 )
                        ),
                        tabPanel("Diagnostic & Reportability Audits",
                                 fluidRow(
                                   column(12,
                                          div(class = "white-box",
                                              valueBox(textOutput("ssAccuracy"), 
                                                       "Overall Accuracy", 
                                                       icon = icon("check"),
                                                       color = "light-blue"
                                                       ),
                                              valueBox(textOutput("ssConfInt"),
                                                       "Accuracy 95% Confidence Interval",
                                                       icon = icon("chart-line"),
                                                       color = "orange"
                                                       ),
                                              valueBox(textOutput("ssDim"),
                                                       "Number of Observations",
                                                       icon = icon("filter"),
                                                       color = "purple"),
                                              style = "height:140px"
                                            )
                                          )
                                   ),
                                 fluidRow(
                                   column(12,
                                          div(class = "white-box",
                                              box(plotlyOutput("ssPlot", height = "600px"),
                                                  width = 800, 
                                                  status = "success",
                                                  title = "Confusion Matrix",
                                                  solidHeader = TRUE
                                                  )
                                              )
                                   )
                                 ),
                                 fluidRow(
                                   column(12,
                                          div(class = "white-box",
                                              box(DTOutput("ssTable"),
                                                  width = 800,
                                                  status = "success",
                                                  title = "Summary Table",
                                                  solidHeader = TRUE
                                                  )
                                          )
                                   )
                                 )
                        ),
                        tabPanel("Surveillance Audits",
                                 fluidRow(
                                   column(12,
                                          div(class = "white-box",
                                              valueBox(textOutput("svlAccuracy"), 
                                                       "Overall Accuracy", 
                                                       icon = icon("check"),
                                                       color = "light-blue"
                                              ),
                                              valueBox(textOutput("svlConfInt"),
                                                       "Accuracy 95% Confidence Interval",
                                                       icon = icon("chart-line"),
                                                       color = "orange"
                                              ),
                                              valueBox(textOutput("svlDim"),
                                                       "Number of Observations",
                                                       icon = icon("filter"),
                                                       color = "purple"),
                                              style = "height:140px"
                                          )
                                   )
                                 ),
                                 fluidRow(
                                   column(12,
                                          div(class = "white-box",
                                              box(plotlyOutput("svlCMPlotly", height = "600px"),
                                                  width = 800, 
                                                  status = "success",
                                                  title = "Surveillance Confusion Matrix",
                                                  solidHeader = TRUE
                                              )
                                          )
                                   )
                                 ),
                                 fluidRow(
                                   column(12,
                                          div(class = "white-box",
                                              box(DTOutput("svlTable"),
                                                  width = 800,
                                                  status = "success",
                                                  title = "Summary Table",
                                                  solidHeader = TRUE
                                              )
                                          )
                                   )
                                 ),
                                 fluidRow(
                                   column(12,
                                          div(class = "white-box",
                                              box(
                                                selectInput(
                                                  "svlMetricSel",
                                                  label = "Select metric to summarise:",
                                                  choices = unique(metricGroups),
                                                  multiple = FALSE,
                                                  selected = "Recall"
                                                  ),
                                                DTOutput("ssOverallTable"),
                                                width = 800, 
                                                status = "success",
                                                title = "Cross-Group Summary",
                                                solidHeader = TRUE
                                              ),
                                              style = "height:360px"
                                          )
                                   )
                                 )
                        ),
                        id = "tabs"
                      )
                    )
)