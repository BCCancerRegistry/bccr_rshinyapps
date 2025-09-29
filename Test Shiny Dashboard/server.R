##### Test App Server

server <- function(input, output, session) {
  
  output$plot <- renderPlotly({
    
    plot_analysis <- mtcars %>%
      filter(between(mpg, input$mpg_range[1], input$mpg_range[2])) %>%
      ggplot(mapping = aes(x = wt, y = mpg)) + 
      geom_point() + 
      theme_ipsum() +
      geom_smooth()
    
    ggplotly(plot_analysis, height = 600)
    
  })
  
  output$table_summary <- renderDT({
    
    summary_analysis <- mtcars %>%
      group_by(cyl) %>%
      summarise(mean_mpg = round(mean(mpg), 3), .groups = "drop")
    
    datatable(summary_analysis)
  
  })
}
   