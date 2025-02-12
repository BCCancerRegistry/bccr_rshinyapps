# global

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
library(editData)

dxGroups <- c("BR", "CR", "CX", "GI", "GU", "GY", "HN", "LK", "LU", "LY", "MM", "ME", "NE", "OO", "PR", "PU", "SA", "SK", "TH", "UNK", "<NA>")

filterChoices <- c("All", "All Full Matches", "All Mismatches", "Mismatched DX Groups", "Mismatched Reportability")

# Server connection
con <- dbConnect(odbc::odbc(),
                 Driver = "SQL Server",
                 Server = "sddbsmarc001",
                 Database = "eMaRCPlus_V90_Audit",
                 Trusted_Connection = "Yes")

query <- "SELECT audit_msgid, import_dt_time, tr_reportability, ds_reportability, tr_dxgroup, ds_dx_group, match_reportability_ds_tr, match_dxclass_ds_tr FROM [eMaRCPlus_V90_Audit].[dbo].[bccr_ss_audit_temp];"

# Formatting
audit <- dbGetQuery(con, query) %>%
  mutate(ds_dx_group = ifelse(is.na(ds_dx_group), "<NA>", ds_dx_group)) %>%
  mutate(ds_reportability = factor(ds_reportability, levels = c(0, 1), labels = c("No", "Yes")),
         ds_dx_group = factor(ds_dx_group),
         tr_reportability = factor(tr_reportability, levels = c(0, 1), labels = c("No", "Yes")),
         tr_dxgroup = factor(trimws(toupper(tr_dxgroup))),
  ) %>%
  mutate(tr_dxgroup = factor(tr_dxgroup, levels = dxGroups),
         ds_dx_group = factor(ds_dx_group, levels = dxGroups) 
  ) %>%
  mutate(import_dt_time = format(ymd(substring(import_dt_time, 1, 10)), "%Y-%m")) %>%
  mutate(sd_reportability = factor("", levels = c("Yes", "No")),
         sd_dx_group = factor("", levels = dxGroups) 
  ) %>%
  mutate(sd_comment = "")
