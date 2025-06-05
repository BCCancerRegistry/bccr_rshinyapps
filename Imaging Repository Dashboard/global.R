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


# Connections -------------------------------------------------------------

con <- dbConnect(odbc::odbc(),
                 Driver = "SQL Server",
                 Server = "spdbsdima001",
                 Database = "BCCR_Results",
                 Trusted_Connection = "Yes")

patientQuery <- "SELECT [MessageId]
                ,[Identifier]
                ,[LastName]
                ,[FirstName]
                ,[MiddleName]
                ,[BirthDate]
                ,[Sex]
                ,[StreetAddress]
                ,[AddressOtherDesignation]
                ,[City]
                ,[StateOrProvince]
                ,[ZipOrPostalCode]
                ,[Country]
              FROM [BCCR_Results].[dbo].[Patient]"

messageQuery <- "SELECT [MessageId]
                ,[ChannelName]
                ,[SendingApplication]
                ,[SendingFacility]
                ,[MessageDateTime]
                ,[MessageControlId]
                ,[T1StatusCode]
                ,[T2StatusCode]
                ,[ErrorCodes]
            FROM [BCCR_Results].[dbo].[Message]"

resultQuery <- "SELECT [MessageId]
          ,[ObservationDateTime]
          ,[ResultsReportDateTime]
          ,[ExamDescription]
          ,[ExamType]
          ,[ExamTypeStd]
          ,[ReasonForStudy]
          FROM [BCCR_Results].[dbo].[Result]"

# Exam description codes -> to Cerner Master Procedure Name
codes <- read.csv("H:/Data Integrity Analyst/Data Science/dashboard data/imaging exam description codes/examDescriptionCodes.csv") %>%
  select(2, 3)

# Loading datasets, formatting

patientData <- dbGetQuery(con, patientQuery) %>%
  mutate(BirthDate = ymd(substr(BirthDate, 1, 10)),
         BirthDate = if_else(BirthDate <= ymd("1901-01-01"), as.Date(NA), BirthDate),
         Identifier = if_else(Identifier == "", as.character(NA), Identifier),
         Age = if_else(is.na(BirthDate),
                       as.numeric(NA),
                       floor(as.numeric(interval(BirthDate, today())/ years(1)
                                        )
                             )
         ),
         City = str_to_title(City)
  )

messageData <- dbGetQuery(con, messageQuery) %>%
  mutate(MessageDateTime = as.Date(substr(MessageDateTime, 1, 10)),
         MessageMonth = floor_date(MessageDateTime, unit = "month")
  )

resultData <- dbGetQuery(con, resultQuery) %>%
  rename(obsDate = 2) %>%
  mutate(obsDate = as.Date(substr(obsDate, 1, 10))) %>%
  rename(rrDate = 3) %>%
  mutate(rrDate = as.Date(substr(rrDate, 1, 10)))

# summary table function
getStats <- function(x, y, z, group_col) {
  
  filt <- z %>%
    filter(between(date, x, y))
  
  ntotal <- sum(filt$Volume) 
    
  dailyval <- filt %>%
    group_by(across(all_of(group_col)), date) %>%
    summarise(Volume = sum(Volume), .groups = 'drop') %>%
    group_by(across(all_of(group_col))) %>%
    summarise("Daily Mean" = ceiling(mean(Volume)),
              "Daily Median" = ceiling(quantile(Volume, 0.5)),
              "Daily Range" = str_c("(", min(Volume), ", ", max(Volume), ")"),
              "Daily Outlier Bounds" = paste0("(",
                                               ceiling(quantile(Volume, 0.1)),
                                               ", ",
                                               ceiling(quantile(Volume, 0.75) + (1.5 * IQR(Volume))),
                                               ")"
              ),
              .groups = 'drop')
  
  weeklyval <- filt %>%
    mutate(week = floor_date(date, unit = "week", week_start = 1)) %>%
    group_by(across(all_of(group_col)), week) %>%
    summarise(Volume = sum(Volume), .groups = 'drop') %>%
    group_by(across(all_of(group_col))) %>%
    summarise("Weekly Mean" = ceiling(mean(Volume)),
              "Weekly Median" = ceiling(quantile(Volume, 0.5)),
              "Weekly Range" = str_c("(", min(Volume), ", ", max(Volume), ")"),
              "Weekly Outlier Bounds" = paste0("(",
                                                ceiling(quantile(Volume, 0.1)),
                                                ", ",
                                                ceiling(quantile(Volume, 0.75) + (1.5 * IQR(Volume))),
                                                ")"
              ),
              .groups = 'drop')
  
  monthlyval <- filt %>%
    group_by(across(all_of(group_col)), year(date), month(date)) %>%
    summarise(Volume = sum(Volume), .groups = 'drop') %>%
    group_by(across(all_of(group_col))) %>%
    summarise("Monthly Mean" = ceiling(mean(Volume)),
              "Monthly Median" = ceiling(quantile(Volume, 0.5)),
              "Monthly Range" = str_c("(", min(Volume), ", ", max(Volume), ")"),
              "Monthly Outlier Bounds" = paste0("(",
                                                ceiling(quantile(Volume, 0.1)),
                                                ", ",
                                                ceiling(quantile(Volume, 0.75) + (1.5 * IQR(Volume))),
                                                ")"
              ),
              "Total Volume" = paste0(sum(Volume), " (", percent(sum(Volume)/ntotal, accuracy = 0.01), ")"),
              .groups = 'drop')
  
  
  
  summarytable <- dailyval %>%
    inner_join(weeklyval, by = group_col) %>%
    inner_join(monthlyval, by = group_col)
  
  return(summarytable)
}


