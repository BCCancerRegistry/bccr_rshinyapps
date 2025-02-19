##### CAN-REP Interface
ui <- dashboardPage(skin = "black",
                    dashboardHeader(
                      title = tags$div(
                        tags$span(
                          class = "custom-title",
                          tagList(icon("chart-bar")),
                          "CAN-REP",
                        ),
                        tags$span(style = "font-size: 14px;",
                                 "BCCR"),
                        )
                    ),
                    dashboardSidebar(
                      actionButton(
                        inputId = "readMe",
                        label = "ReadMe File",
                        icon = icon("th")
                        ),
                      dateRangeInput("daterange", "Select date range:",
                                     start = min(diagraw$fulldate),
                                     end = max(diagraw$fulldate),
                                     min = min(diagraw$fulldate),
                                     max = max(diagraw$fulldate),
                                     format = "yyyy-mm-dd",
                                     separator = " to "
                      ),
                      pickerInput("authority",
                                  "Select the health authorities:",
                                  choices = unique(diagraw$HA),
                                  options = list('actions-box' = TRUE),
                                  multiple = TRUE,
                                  selected = unique(diagraw$HA)
                      ),
                      uiOutput("sliderInputPanel"),
                      uiOutput("varSelect1"),
                      uiOutput("varSelect2"),
                      uiOutput("modelToggle"),
                      uiOutput("seToggle"),
                      uiOutput("predictToggle")
                    ),
                    dashboardBody(
                      tags$head(
                        tags$style(HTML("
                            body, h1, h2, h3, h4, h5, h6 {
                              font-family: 'Arial Narrow', sans-serif;
                            }
                            .skin-blue .main-header .logo {
                              font-family: 'Arial Narrow', sans-serif;
                            }
                            .skin-blue .main-header .navbar {
                              font-family: 'Arial Narrow', sans-serif; 
                            }
                            .skin-blue .main-sidebar {
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
                        tabPanel("Overall Summary",
                                 fluidRow(
                                   column(12,
                                          div(class = "white-box",
                                            h3(tags$b("Welcome to the BCCR Cancer Reportability and Diagnostic Classifier (CAN-REP) Dashboard!")),
                                            plotOutput("plot1", width = "1150px", height = "650px"),
                                            style = "height:700px",
                                          )
                                   ),
                                   column(12,
                                          div(class = "white-box",
                                              h3(tags$b("Health Authority Summary")),
                                              DTOutput("table1")
                                              )
                                          )
                                 )
                        ),
                        tabPanel("Reportability Volumes",
                                 fluidRow(
                                   column(12,
                                          div(class = "white-box",
                                              plotlyOutput("plot2"),
                                              style = "height:650px")
                                   )
                                 ),
                                 fluidRow(
                                   column(12,
                                          div(class = "white-box",
                                              h3(tags$b("Summary Table")),
                                              DTOutput("table2")
                                              )
                                   )
                                 ),
                                 fluidRow(
                                   column(12,
                                          div(class = "white-box",
                                              h3(tags$b(textOutput("numOutliers2"))),
                                              DTOutput("table2a")
                                          )
                                   )
                                 )
                        ),
                        tabPanel("Diagnostic Classifier Volumes",
                                 fluidRow(
                                   column(12,
                                          div(class = "white-box",
                                              plotlyOutput("plot3"),
                                              style = "height:650px")
                                   )
                                 ),
                                 fluidRow(
                                   column(12,
                                          div(class = "white-box",
                                              h3(tags$b("Summary Table")),
                                              DTOutput("table3"),
                                              style = "height:550px")
                                   )
                                 ),
                                 fluidRow(
                                   column(12,
                                          div(class = "white-box",
                                              h3(tags$b(textOutput("numOutliers3"))),
                                              DTOutput("table3a")
                                          )
                                   )
                                 )
                        ),
                        tabPanel("Daily Volumes",
                                 fluidRow(
                                   column(12,
                                          plotOutput("plot4"),
                                          style = "height:700px"
                                   )
                                 ),
                                 fluidRow(
                                   column(12,
                                          div(class = "white-box",
                                              h3(tags$b("Summary Table")),
                                              DTOutput("table4")
                                              )
                                          ),
                                 )
                        ),
                        tabPanel("Predictive Analysis",
                                 fluidRow(
                                   column(12,
                                          div(class = "white-box",
                                              plotlyOutput("plot5"),
                                              style = "height:650px")
                                   )
                                 ),
                                 fluidRow(
                                   column(12,
                                          div(class = "white-box",
                                              h3(tags$b("Model Summary Output")),
                                              verbatimTextOutput("regressionline"))
                                   )
                                 )
                        ),
                        id = "tabs"
                      )
                    )
)