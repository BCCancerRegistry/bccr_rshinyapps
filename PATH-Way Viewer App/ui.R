# Imaging-Pathology Viewer UI ---------------------------------------------

ui <- dashboardPage(skin = "blue",
                    dashboardHeader(
                      title = tags$div(
                        tags$span(
                          class = "custom-title",
                          tagList(icon("dashboard")),
                          "PATH-Way",
                        ),
                        tags$span(style = "font-size: 14px;",
                                  "BCCR"),
                      )
                    ),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Patient Selection Filters",
                                 airDatepickerInput(
                                   inputId = "dateRange1",
                                   label = "Filter by Earliest Pathology Date:", 
                                   minDate = ymd("2024-01-01"),
                                   maxDate = ymd(maxDate),
                                   value = ymd("2024-10-01")
                                 ),
                                 airDatepickerInput(
                                   inputId = "dateRange2",
                                   label = "Filter by Latest Pathology Date:", 
                                   minDate = ymd("2024-01-01"),
                                   maxDate = ymd(maxDate),
                                   value = ymd(maxDate)
                                 ),
                                 numericRangeInput("ageRange",
                                                   label = "Filter by Age:",
                                                   min = 0,
                                                   max = 125,
                                                   value = c(25, 55)
                                 ),
                                 virtualSelectInput("sex",
                                             label = "Filter by Sex:",
                                             choices = c("F", "M"),
                                             selected = c("F", "M"),
                                             multiple = TRUE,
                                             optionsCount = 5
                                 ),
                                 virtualSelectInput("DXGroup",
                                             label = "Filter by DX Groups:",
                                             choices = c("BR", "CR", "CX", "GI", "GU", "GY", "HN", "LK", "LU", "LY",
                                                         "ME", "MM", "NE", "OO", "PR", "PU", "SA", "SK", "TH"),
                                             selected = c("BR", "CR", "CX", "GI", "GU", "GY", "HN", "LK", "LU", "LY",
                                                          "ME", "MM", "NE", "OO", "PR", "PU", "SA", "SK", "TH"),
                                             multiple = TRUE,
                                             optionsCount = 5
                                 ),
                                 virtualSelectInput("HAFilter",
                                             label = "Filter by Health Authority:",
                                             choices = c("FHA", "IHA", "NHA", "PHSA", "VIHA", "VCHA"),
                                             selected = c("FHA", "IHA", "NHA", "PHSA", "VIHA", "VCHA"),
                                             multiple = TRUE,
                                             optionsCount = 5
                                 ),
                                 br(),
                                 startExpanded = TRUE
                        )
                      )
                    ),
                    dashboardBody(
                      tags$head(
                        tags$style(HTML("
                            body, h1, h2, h3, h4, h5, h6 {
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
                            } .custom-font {
                              font-size: 12px;
                            } .scroll-cont {
                              position: fixed;
                              top: 50px;
                              z-index: 1000;
                            } .info-box .info-box-content {
                            padding-top: 0px;
                            max-height: 85px;
                            }
                        "))
                      ),
                      tabsetPanel(
                        tabPanel("Patient Viewer",
                                 fluidRow(
                                   column(12,
                                          div(class = "white-box",
                                              infoBox(
                                                fluidRow(
                                                  column(4,
                                                         uiOutput("identifierSearch")
                                                  ),
                                                  column(4,
                                                         actionButton("patientSelectAction",
                                                                      label = "View Full Patient Index")
                                                  )
                                                ),
                                                title = "View Patient (by Identifier):",
                                                color = "blue",
                                                icon = icon("user")
                                              ),
                                              infoBox(title = "Number of Reports",
                                                      value = NULL, 
                                                      tableOutput("reportNumbers"),
                                                      color = "blue",
                                                      icon = icon("server")
                                              ),
                                              infoBox(tableOutput("patientProfile"),
                                                      value = NULL,
                                                      title = "Patient Profile",
                                                      color = "blue",
                                                      icon = icon("notes-medical")
                                              )
                                          )
                                   )
                                 ),
                                 fluidRow(
                                   column(12,
                                          div(class = "white-box",
                                              box(
                                                timevisOutput("timelinePlot"),
                                                width = 12,
                                                title = "Patient Report Timeline",
                                                solidHeader = TRUE
                                              )
                                          )
                                   )
                                 ),
                                 fluidRow(
                                   column(12,
                                          div(class = "white-box",
                                              box(
                                                DTOutput("plotTable"),
                                                width = 12,
                                                title = "Table",
                                                solidHeader = TRUE
                                              )
                                          )
                                   )
                                 )
                        ),
                        tabPanel("Linkage Statistics",
                                 fluidRow(
                                   column(12,
                                          div(class = "white-box",
                                              grVizOutput("linkageDiagram")
                                          )
                                   )
                                 ),
                                 fluidRow(
                                   column(12,
                                          div(class = "white-box",
                                              box(
                                                DTOutput("DTlinkageSet"),
                                                width = 12,
                                                title = "Linked Dataset", 
                                                solidHeader = TRUE
                                              )
                                              
                                          )
                                   )
                                 )
                        )
                      )
                    ),
                    tags$head(
                      tags$title("PATH-Way Dashboard")
                    )
                      
)
