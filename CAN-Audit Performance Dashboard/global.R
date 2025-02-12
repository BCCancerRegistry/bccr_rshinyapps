# Raw File

# Libraries 
library(tidyverse)
library(scales)
library(shinydashboard)
library(hrbrthemes)
library(DT)
library(plotly)
library(shinyWidgets)
library(rintrojs)
library(caret)
library(shinyjs)
library(readxl)
library(DBI)
library(odbc)

# Surveillance Data

BRsvl <- read_excel("~/Performance Dashboard/surveillancetest.xlsx")

PRsvl <- read.csv("~/Performance Dashboard/prostate_surveillance_audit_data.csv")

# Reading the folder that holds the SS audit .csv files
sourceFiles <- list.files(path = "//srvnetapp02.phsabc.ehcnet.ca/bcca/docs/Data Integrity Analyst/Data Science/dashboard data/performance",
                          pattern = "^small_scale_audits_.*\\.csv$",
                          full.names = TRUE)

# Reading DX/Rep audits from server

con <- dbConnect(odbc::odbc(),
                 Driver = "SQL Server",
                 Server = "sddbsmarc001",
                 Database = "eMaRCPlus_V90_Audit",
                 Trusted_Connection = "Yes")
query <- "SELECT prod_msgid, ds_reportability, ds_dx_group, audit_msgid, tr_reportability, tr_dxgroup, import_dt_time FROM [eMaRCPlus_V90_Audit].[dbo].[bccr_ss_audit_temp];"
data <- dbGetQuery(con, query)

# Joining the SS audit .csv files together, into a combined one
fileConsolidation <- function(fileNames) {
  
  allFile <- data.frame()
  
  for (file in fileNames) {
    
    monthName <- str_extract(basename(file), "[A-Za-z]+[0-9]{4}")
    
    file1 <- read.csv(file)
    
    file1 <- file1 %>%
      select(1:7) %>%
      mutate(month = as.Date(paste0(monthName, "01"), format = "%B%Y%d"))
    
    allFile <- rbind(allFile, file1)
  }
  
  allFile <- allFile %>%
    arrange(month) %>%
    mutate(month = format(month, "%B %Y"))
    
  return(allFile)
}

# Re-level function for generating the confusion matrices
levelFactor <- function(col, col_name) {
  if (endsWith(col_name, "_audit")) {
    return(factor(col, levels = unique(sub("_audit$", "", col))))
  } 
  
  if (length(unique(col)) < 2) {
    return(factor(col, levels = c(unique(col), "Other")))
  }
  
  return(factor(col, levels = unique(col)))
}

dxGroups <- c("BR", "CR", "CX", "GI", "GU", "GY", "HN", "LK", "LU", "LY", "MM", "ME", "NE", "OO", "PR", "PU", "SA", "SK", "TH", "UNK")

metricGroups <- c("Sensitivity", "Specificity", "Pos Pred Value", "Neg Pred Value", "Precision", "Recall", "F1", "Prevalence", "Detection Rate",
                  "Detection Prevalence", "Balanced Accuracy")

svlGroups <- c("Site", "Histology", "Behaviour", "Laterality", "LVI")

reportFactor <- c("Yes", "No", "Test")

# accuracyData <- data %>%
#   mutate(ds_reportability = factor(ds_reportability, levels = c(0, 1), labels = c("No", "Yes")),
#          ds_dx_group = factor(ds_dx_group),
#          tr_reportability = factor(tr_reportability, levels = c(0, 1), labels = c("No", "Yes")),
#          tr_dxgroup = factor(trimws(toupper(tr_dxgroup))),
#   ) %>%
#   mutate(tr_dxgroup = factor(tr_dxgroup, levels = dxGroups),
#          ds_dx_group = factor(ds_dx_group, levels = dxGroups)
#   ) %>%
#   mutate(month = format(ymd(substring(import_dt_time, 1, 10)), "%B %Y")) %>%
#   select(1:6, 8)

# Formatted accuracy data
accuracyData <- fileConsolidation(sourceFiles) %>%
  mutate(ds_reportability = factor(ds_reportability, levels = c(0, 1), labels = c("No", "Yes")),
         ds_dx_group = factor(ds_dx_group),
         tr_reportability = factor(recode(tr_reportability, y = "Yes", n = "No")),
         tr_dxgroup = factor(trimws(toupper(tr_dxgroup))),
         ) %>%
  mutate(tr_dxgroup = factor(tr_dxgroup, levels = dxGroups),
         ds_dx_group = factor(ds_dx_group, levels = dxGroups)
         ) %>%
  select(2:8)


# Breast surveillance
BRsvl <- BRsvl %>%
  mutate(across(c(Site, Site_audit, LVI, LVI_audit, Behaviour, 
                  Behaviour_audit, Laterality, Laterality_audit), 
                ~ levelFactor(.x, cur_column()))
  ) %>%
  mutate(across(c(Histology, Histology_audit), ~ factor(.x, levels = c(sort(unique(Histology)), 9999))))
  
# site_code_actual	histology_code_actual	behaviour_code_actual	laterality_actual	lymph_vascular_invasion_actual

# Prostate surveillance
PRsvl <- PRsvl %>%
  rename(Site = site_code_actual,
         Site_audit = site_code,
         Histology = histology_code_actual,
         Histology_audit = histology_code,
         Behaviour = behaviour_code_actual,
         Behaviour_audit = behaviour_code,
         LVI = lymph_vascular_invasion_actual,
         LVI_audit = lymph_vascular_invasion,
         Laterality = laterality_actual,
         Laterality_audit = laterality
  ) %>%
  mutate(across(c(Site, Site_audit, LVI, LVI_audit, Behaviour, 
                  Behaviour_audit, Laterality, Laterality_audit), 
                ~ levelFactor(.x, cur_column()))
  ) %>%
  mutate(across(c(Histology, Histology_audit), ~ factor(.x, levels = c(sort(unique(Histology)), 9999))),
         across(c(LVI, LVI_audit), ~ factor(.x, levels = c("Y", "N", "U")))
  ) %>%
  mutate(dx_group = "PR")

SVLSets <- list(PRsvl, BRsvl)