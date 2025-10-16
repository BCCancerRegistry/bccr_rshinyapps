# Imaging/Pathology Viewer Dashboard Global --------------------------------------

# Libraries
library(tidyverse)
library(scales)
library(shinydashboard)
library(DT)
library(plotly)
library(shinyWidgets)
library(DBI)
library(odbc)
library(stringr)
library(stringi)
library(timevis)
library(DiagrammeR)
library(remotes)

# Non-CRAN Packages
missing_non_cran <- setdiff(c("hl7r", "hrbrthemes"), rownames(installed.packages()))

if (length(missing_non_cran)) {
  remotes::install_github("bransonf/hl7r")
  remotes::install_github("hrbrmstr/hrbrthemes")
}

library(hrbrthemes)
library(hl7r)

options(scipen = 999)

# Connections -------------------------------------------------------------------------

pathCon <- dbConnect(odbc::odbc(),
                     Driver = "SQL Server",
                     Server = "spdbsmarc001",
                     Database = "eMaRCPlus_v60Patch",
                     Trusted_Connection = "Yes")

# Pathology -----------------------------------------------------------------

# DX Groups
dxGroupQuery <- "SELECT [msg_id], [dx_group]
                FROM [eMaRCPLus_v60Patch].[BCCR].[dx_group_pipeline]"
dxData <- dbGetQuery(pathCon, dxGroupQuery) %>%
  rename(msgid = msg_id,
         final_label = dx_group) %>%
  distinct(msgid, .keep_all = TRUE)

# Pathology Message Query
codes <- read.csv("examDescriptionCodes.csv")

# Latest Pathology Date 
maxDateQuery <- "SELECT MAX(msg_dt) AS max_date
                 FROM [eMaRCPlus_v60Patch].[dbo].[bccr_reportability_filter]"
maxDate <- dbGetQuery(pathCon, maxDateQuery)

# Message Text Processing Function
message_text_output <- function(report_type, message_text_raw) {
  
  renderUI({
    
    if (report_type == "Pathology") {
      # Initial Format
      message_text_raw <- message_text_raw %>%
        gsub("\\\\X0D\\\\", "\r", .) %>%
        gsub("\\\\X0A\\\\", "\n", .)
      
      # Convert raw message text into HL7 vector
      hl7_vector <- strsplit(message_text_raw, "\n")[[1]]
      
      # Parse HL7 message - to identify the main component
      parsed_message <- parsehl7(hl7_vector)
      # Pushing into data frame (Native R)
      hl7_dataframe <- hl7df(parsed_message)
      # Coercing into long format - selecting for only the pathology description
      df_long <- stack(hl7_dataframe) %>%
        filter(str_detect(ind, "OBX")) %>%
        filter(nchar(values) >= 300)
      # Extracted formatted text
      formatted_text <- df_long[1, 1] %>%
        gsub("\r|\n", "<br>", .)
      
    } else if (report_type == "Imaging") {
      formatted_text <- message_text_raw %>%
        gsub("~", "<br>", .)
      
    } else if (report_type == "Invalid!") {
      formatted_text <- "Check dependencies"
    }
    
    formatted_text <- paste0("<p style='font-size: 18px;'>", formatted_text, "</p>")
    
    HTML(formatted_text)
    
  })
  
}

# combinedData <- bind_rows(imagingData, pathData) %>%
#   filter(between(date, ymd("2024-11-01"), today())) %>%
#   filter(Identifier %in% intersect(imagingData$Identifier, pathData$Identifier)) %>%
#   # distinct(msgid, .keep_all = TRUE) %>%
#   arrange(Identifier, date) %>%
#   # filter(date >= ymd("2024-01-01")) %>%
#   mutate(Hierarchy = ifelse(Type == "Imaging", -0.5, 0.5)) %>% # imaging is negative, pathology is positive
#   # group_by(Identifier, Type) %>%
#   # arrange(date) %>%
#   # mutate(
#   #   date_diff = as.numeric(difftime(date, lag(date), units = "days")),
#   #   group_id = accumulate(
#   #     .x = date_diff,
#   #     .f = function(Hierarchy, diff) {
#   #       if (Type == "Imaging" & diff >= 15) Hierarchy - 1 else Hierarchy
#   #     },
#   #     .init = 0
#   #   )[-1]
#   # ) %>%
#   ungroup()

# Separating, assigning "tiers" for observations that are close together in date
#### assign a value - i.e., 1 for imaging, -1 for pathology
#### where day is the same, further stratify (1, 2, 3, 4)

# stratifyHierarchy <- function(dataset, reference) {
#   
#   if (nrow(dataset) > 1) {
#     
#     for (i in 2:nrow(dataset)) {
#       
#       dayElapsed <- as.numeric(difftime(dataset$date[i], dataset$date[i - 1], units = "days"))
#       
#       if (!is.na(dayElapsed) && dayElapsed <= 14) {
#         dataset$Hierarchy[i] <- dataset$Hierarchy[i - 1] + reference
#       } else {
#         NA
#       }
#     }
#   
#   } 
#   
#   return(dataset)
#   
# }

# stratifyHierarchy <- function(dataset, reference) {
#   
#   if (nrow(dataset) > 1) {
#     
#     for (i in 2:nrow(dataset)) {
#       
#       dayElapsed <- as.numeric(difftime(dataset$date[i], dataset$date[i - 1], units = "days"))
#       
#       if (dataset$Identifier[i] == dataset$Identifier[i - 1]) {
#         if (!is.na(dayElapsed) && dayElapsed <= 14) {
#           dataset$Hierarchy[i] <- dataset$Hierarchy[i - 1] + reference
#         } else {
#           return(dataset)
#         }
#       } else {
#         return(dataset)
#       }
#     }
#     
#     return(dataset)
#     
#   } else {
#     
#     return(dataset)
#     
#   }


  




