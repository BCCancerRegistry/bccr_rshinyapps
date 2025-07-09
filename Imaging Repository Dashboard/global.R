# Imaging Dashboard Global File -------------------------------------------

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(scales)
library(shinydashboard)
library(hrbrthemes)
library(DT)
library(plotly)
library(shinyWidgets)
library(DBI)
library(odbc)
library(stringr)
library(stringi)

options(scipen = 999)

# Global Files ------------------------------------------------------------

# Getting unique OBS values
con <- dbConnect(odbc::odbc(),
                 Driver = "SQL Server",
                 Server = "spdbsdima001",
                 Database = "BCCR_Imaging_Results",
                 Trusted_Connection = "Yes")

examTypeStdQuery <- "SELECT DISTINCT [ExamTypeStd] FROM [BCCR_Imaging_Results].[dbo].[Result]"
authorityQuery <- "SELECT DISTINCT [HealthAuthority] FROM [BCCR_Imaging_Results].[dbo].[Result]"

# ExamTypeStd
distinctExamType <- dbGetQuery(con, examTypeStdQuery) %>%
  filter(ExamTypeStd != "")

# HealthAuthority
distinctAuthority <- dbGetQuery(con, authorityQuery) %>%
  filter(HealthAuthority != "")

# Exam description codes -> to Cerner Master Procedure Name
codes <- read.csv("H:/Data Integrity Analyst/Data Science/dashboard data/imaging exam description codes/examDescriptionCodes.csv") %>%
  select(2, 3)

# Functions ---------------------------------------------------------------
# Time Series Summary Table Function
getTimeSummary <- function(dataSet, dateMetric, viewVariable, nameConvention) {
  
  allSummary <- dataSet %>%
    filter(!!sym(viewVariable) != "") %>%
    group_by(!!sym(dateMetric), !!sym(viewVariable)) %>%
    summarise(Volume = n(), .groups = 'drop')
  
  Total <- sum(allSummary$Volume) 
  
  summary <- allSummary %>%
    group_by(!!sym(viewVariable)) %>%
    summarise("Mean" = ceiling(mean(Volume)),
              "Median" = ceiling(quantile(Volume, 0.5)),
              "Range" = str_c("(", min(Volume), ", ", max(Volume), ")"),
              "Outlier Bounds" = paste0("(",
                                        ceiling(quantile(Volume, 0.1)),
                                        ", ",
                                        ceiling(quantile(Volume, 0.75) + (1.5 * IQR(Volume))),
                                        ")"
              ),
              "Total Volume" = paste0(sum(Volume), " (", percent(sum(Volume)/Total, accuracy = 0.01), ")"),
              .groups = 'drop') %>%
    rename_with(.cols = 2:5, .fn = ~ paste0(nameConvention, " ", .x)) 
  
  return(summary)
  
}

# Bar Plot Generation Function
getBarPlotly <- function(dataset, mainVariable, viewMetric, filterVariable, filterBy) {
  
  # Option to add in a secondary filter
  if (filterVariable != "") {
    plotDataInitial <- dataset %>%
      filter(!!sym(mainVariable) != "") %>%
      filter(!!sym(filterVariable) %in% filterBy)  
  } else {
    plotDataInitial <- dataset %>%
      filter(!!sym(mainVariable) != "")
  }
  
  # Standardized pipeline - to generate top counts - counts + proportions
  if (!"Volume" %in% colnames(dataset)) {
    plotData <- plotDataInitial %>%
      group_by(!!sym(mainVariable)) %>%
      summarise(Volume = n(), .groups = "drop") %>%
      ungroup() %>%
      mutate(Total = sum(Volume)) %>%
      slice_max(Volume, n = 10) %>%
      group_by(!!sym(mainVariable)) %>%
      mutate(Proportion = round(Volume/Total, digits = 3))
  } else {
    plotData <- dataset
  }
  
  
  # x-lab angle
  if (nrow(plotData) > 7) {
    size <- 7
  } else {
    size <- 12
  }
  
  # Converting to gg bar plot - geom_plot
  plot <- plotData %>%
    ggplot(mapping = aes(x = reorder(!!sym(mainVariable), -!!sym(viewMetric)), y = !!sym(viewMetric))) +
    geom_bar(stat = "identity", fill = "#dd4b39", color = "black") +
    theme_ipsum() +
    labs() +
    theme(axis.text.x = element_text(size = size, hjust = 1)) +
    labs(x = mainVariable,
         y = viewMetric,
         title = paste0("n = ", plotData$Total[1])
         )
  
  # Converting to plotly
  plotly <- ggplotly(plot) %>%
    layout(margin = list(t = 40, b = 15, l = 15, r = 15))
  
  return(plotly)
  
}
