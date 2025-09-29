# Test App UI ------------------------------------------------------

ui <- dashboardPage(skin = "black",
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
                      numericRangeInput(
                          "mpg_range",
                          label = "Filter by MPG range:",
                          min = min(mtcars$mpg),
                          max = max(mtcars$mpg),
                          value = c(min(mtcars$mpg), max(mtcars$mpg))
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
                            };
                        "))
                      ),
                      fluidRow(
                        column(12,
                               box(
                                 plotlyOutput("plot"),
                                 width = 12,
                                 title = "mpg ~ wt plot",
                                 solidHeader = FALSE,
                                 height = 700
                               ),
                               box(
                                 DTOutput("table_summary"),
                                 width = 12,
                                 title = "Summary",
                                 solidHeader = FALSE
                               )
                        )
                      )
                    )
)