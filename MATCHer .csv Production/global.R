# MATCHer Audit App Global ------------------------------------------------

# Libraries
library(tidyverse)
library(scales)
library(shinydashboard)
library(DT)
library(shinyWidgets)
library(rintrojs)
library(shinyjs)
library(readxl)
library(DBI)
library(odbc)
library(editData)

# DX Factors
dxGroups <- c("BR", "CR", "CX", "GI", "GU", "GY", "HN", "LK", "LU", "LY", "MM", "ME", "NE", "OO", "PR", "PU", "SA", "SK", "TH", "UNK", "N/A")

# Other Factor
filterChoices <- c("All", "All Full Matches", "All Mismatches", "Mismatched DX Groups", "Mismatched Reportability")

# Server connection
con <- dbConnect(odbc::odbc(),
                 Driver = "SQL Server",
                 Server = "sddbsmarc001",
                 Database = "eMaRCPlus_V90_Audit",
                 Trusted_Connection = "Yes")

# Audit data query
query <- "SELECT [audit_msgid],
                 [prod_msgid],
                 [import_dt_time], 
                 [tr_reportability], 
                 [ds_reportability],
                 [tr_dxgroup],
                 [ds_dx_group], 
                 [match_reportability_ds_tr], 
                 [match_dxclass_ds_tr],
                 [ccs_reportability],
                 [ccs_dx_class],
                 [ccs_dx_class_comment]
                 FROM [eMaRCPlus_V90_Audit].[dbo].[bccr_ss_audit_temp];"

# Baseline Dataset 
audit <- read.csv("H:/Data Integrity Analyst/Data Science/dashboard data/MATCHer/.csv version/audit_data.csv") %>%
  mutate(ds_reportability = factor(ds_reportability, levels = c("Yes", "No", "N/A")),
         tr_reportability = factor(tr_reportability, levels = c("Yes", "No", "N/A")),
         ccs_reportability = factor(ccs_reportability, levels = c("Yes", "No", "N/A"))
  ) %>%
  mutate(ds_dx_group = factor(ds_dx_group, levels = dxGroups),
         ccs_dx_class = factor(ccs_dx_class, levels = dxGroups),
         tr_dxgroup = factor(tr_dxgroup, levels = dxGroups)
  ) %>%
  mutate(ccs_edited = if_else(if_any(c(ccs_reportability, ccs_dx_class, ccs_dx_class_comment), is.na), "No", "Yes"))

# audit2 <- read.csv("H:/Data Integrity Analyst/Data Science/dashboard data/audit_data.csv")

# Backup Dataset
# Updates everything CCS submits a change locally
if (file.exists("H:/Data Integrity Analyst/Data Science/dashboard data/MATCHer/.csv version/audit_input_backup.rds")) {
  backup <- readRDS("H:/Data Integrity Analyst/Data Science/dashboard data/MATCHer/.csv version/audit_input_backup.rds")
}
