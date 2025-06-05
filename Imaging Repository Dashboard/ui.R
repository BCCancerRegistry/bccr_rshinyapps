# Imaging Dashboard UI ----------------------------------------------------

ui <- dashboardPage(skin = "red",
                    dashboardHeader(
                      title = tags$div(
                        tags$span(
                          class = "custom-title",
                          tagList(icon("dashboard")),
                          "IMAGING",
                        ),
                        tags$span(style = "font-size: 14px;",
                                  "BCCR"),
                      )
                    ),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Date Filters",
                                 tabName = "dateFilters",
                                 icon = icon("calendar"),
                                 startExpanded = TRUE,
                                 selectInput("XdateMetric",
                                             label = "Select Date Metric:",
                                             choices = c("Observation Date", "Results Report Date", "Message Date"),
                                             multiple = FALSE,
                                             width = "200px",
                                             selected = "Message Date"
                                 ),
                                 uiOutput("globalDateSwitch"),
                                 uiOutput("daySwitch"),
                                 br()

                        )
                      ),
                      conditionalPanel("!(['Message Data', 'Time Series'].includes(input.tabs))",
                        pickerInput("authorityFilt",
                                    label = "Select Health Authorities:",
                                    choices = unique(messageData$ChannelName),
                                    selected = unique(messageData$ChannelName),
                                    multiple = TRUE,
                                    options = list(
                                      'actions-box' = TRUE 
                                    )
                        )
                      ),
                      conditionalPanel("input.tabs == 'Demographic Analysis'",
                                       pickerInput("demoMetric",
                                                   label = "Select Metric:",
                                                   choices = c("Volume", "Proportion"),
                                                   selected = "Volume",
                                                   multiple = FALSE
                                       )
                      ),
                      conditionalPanel("input.tabs == 'Time Series'",
                                       sidebarMenu(
                                         menuItem(
                                           "Time Series Filters",
                                           tabName = "Time Series Filters",
                                           icon = icon("dashboard"),
                                           startExpanded = TRUE,
                                           pickerInput("timeSeriesSeries",
                                                       label = "Select Series:",
                                                       choices = c("Exam Type", "Health Authority"),
                                                       selected = "Exam Type",
                                                       multiple = FALSE
                                           ),
                                           pickerInput("timeSeriesExam",
                                                       label = paste0("Filter by Exam Type Standardized:"),
                                                       choices = c("CT", "MG", "MR", "NM", "PET", "US", "RF"),
                                                       selected = c("CT", "MG", "MR", "NM", "PET", "US", "RF"),
                                                       multiple = TRUE,
                                                       options = list(
                                                         'actions-box' = TRUE 
                                                       )
                                           ),
                                           uiOutput("timeSeriesHA"),
                                           br()
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
                        tabPanel("Time Series",
                                 fluidRow(
                                   column(12,
                                          div(class = "white-box",
                                              box(
                                                title = textOutput("timeSeriesTitle"),
                                                fluidRow(
                                                  column(2,
                                                         selectInput("XtypeTime",
                                                                     label = "Select Metric:",
                                                                     choices = c("Volume", "Proportion"),
                                                                     selected = "Volume",
                                                                     multiple = FALSE,
                                                                     width = "200px"
                                                         )
                                                  )
                                                ),
                                                plotlyOutput("examTrends", height = 450),
                                                paste("Note: Grouping by day available when date range <= 31 days \n
                                                      and by week when date range within 1 year"),
                                                width = 12
                                              )
                                          )
                                   ),
                                   column(12,
                                          div(class = "white-box",
                                              box(
                                                title = "Time Series Summary",
                                                DTOutput("timeSeriesSummary"),
                                                width = 12
                                              )   
                                          )
                                   ),
                                   column(12,
                                          div(class = "white-box",
                                              box(
                                                title = textOutput("nOutliers"),
                                                DTOutput("timeSeriesOutliers"),
                                                paste("Note: Outlier bounds are 10th Percentile and Q3 + 1.5(IQR) for a given series and time grouping"),
                                                width = 12
                                              )   
                                          )
                                   )
                                 )
                        ),
                        tabPanel("Exam Type",
                                 fluidRow(
                                   column(12,
                                          div(class = "white-box",
                                              box(title = "Exam Type Plot",
                                                  fluidRow(
                                                    column(2,
                                                           selectInput("XtypeReports",
                                                                         label = "Select Metric:",
                                                                         choices = c("Volume", "Proportion"),
                                                                         selected = "Volume",
                                                                         multiple = FALSE,
                                                                         width = "200px"
                                                                       )
                                                           )
                                                  ),
                                                  plotlyOutput("typeReports", height = "500px"),
                                                  width = 12
                                              )
                                          )   
                                   )
                                 ),
                                 fluidRow(
                                   column(12,
                                          div(class = "white-box",
                                              box(
                                                title = "Examination Sites",
                                                fluidRow(
                                                  column(2,
                                                         selectInput("examType",
                                                                     label = "Select Exam Type:",
                                                                     choices = c("CT", "MG", "MR", "NM", "PET", "US"),
                                                                     multiple = FALSE,
                                                                     width = "200px"
                                                         )
                                                  ),
                                                  column(2,
                                                         selectInput("examViewSwitch",
                                                                     label = "Select View:",
                                                                     choices = c("Plot", "Table"),
                                                                     selected = "Plot",
                                                                     multiple = FALSE,
                                                                     width = "200px"
                                                         )
                                                  ),
                                                  column(2,
                                                         uiOutput("examDPlotSwitch")
                                                         )
                                                ),
                                                uiOutput("examDescriptions", height = 500),
                                                width = 12
                                                )
                                              )
                                   )
                                 )
                        ),
                        tabPanel("Message Data",
                                 fluidRow(
                                   column(12,
                                          div(class = "white-box",
                                              box(
                                                title = "Messages by Health Authority",
                                                fluidRow(
                                                  column(2,
                                                         selectInput("XtypeAuthority",
                                                                     label = "Select Metric:",
                                                                     choices = c("Volume", "Proportion"),
                                                                     selected = "Volume",
                                                                     multiple = FALSE,
                                                                     width = "200px"
                                                         )
                                                  )
                                                ),
                                                plotlyOutput("channelPlot"),
                                                width = 12
                                              )
                                            )
                                          )
                                 ),
                                 fluidRow(
                                   column(12,
                                          div(class = "white-box",
                                              box(
                                                title = "Examination Sites",
                                                fluidRow(
                                                  column(4,
                                                         pickerInput("facilityHA",
                                                                     label = "Select Health Authorities:",
                                                                     choices = unique(messageData$ChannelName),
                                                                     selected = unique(messageData$ChannelName),
                                                                     multiple = TRUE,
                                                                     width = "300px",
                                                                     options = list(
                                                                       'actions-box' = TRUE 
                                                                     )
                                                         )
                                                  )
                                                ),
                                                DTOutput("facilityTable"),
                                                width = 12
                                              )
                                            )
                                        )
                                 ),
                                 fluidRow(
                                   column(12,
                                          div(class = "white-box",
                                              box(
                                                title = "Data Completeness",
                                                DTOutput("dataComplete"),
                                                width = 12
                                              )
                                          )
                                   )
                                 )
                              ),
                        tabPanel("Demographic Analysis",
                                 fluidRow(
                                   column(12,
                                          div(class = "white-box",
                                              box(
                                                title = "Patient Age Distribution",
                                                plotlyOutput("patientAgePlot"),
                                                width = 12
                                              )
                                          )
                                   ),
                                   column(12,
                                          div(class = "white-box",
                                              box(
                                                title = "Number of Reports per Patient",
                                                plotlyOutput("reportsPerPatient"),
                                                width = 4
                                              ),
                                              box(
                                                title = "Sex Distribution",
                                                plotlyOutput("sexPlot"),
                                                width = 4
                                              ),
                                              box(
                                                title = "Top Ten Patient Cities (based on Address)",
                                                plotlyOutput("cityPlot"),
                                                width = 4
                                              )
                                          )
                                   )
                                 )
                              ),
                        id = "tabs"
                      )
                    )
)
