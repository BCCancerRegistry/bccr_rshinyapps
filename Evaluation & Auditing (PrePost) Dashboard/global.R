# ------------ Surveillance Descriptive Dashboard Global

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(scales)
library(shinydashboard)
library(hrbrthemes)
library(DT)
library(plotly)
library(shinyWidgets)
library(caret)
library(readxl)
library(DBI)
library(odbc)

# Connections --------------------------------------------------------------

# Development Server
con <- dbConnect(odbc::odbc(),
                 Driver = "SQL Server",
                 Server = "sddbsmarc001",
                 Database = "eMaRCPlus_v60Patch",
                 Trusted_Connection = "Yes")

# Production Server
con1 <- dbConnect(odbc::odbc(),
                 Driver = "SQL Server",
                 Server = "spdbsmarc001",
                 Database = "eMaRCPlus_v60Patch",
                 Trusted_Connection = "Yes")


# Queries ------------------------------------------------------------------

# Consolidation Pipeline
query4 <- "SELECT [msgid]
          ,[dx_group]
          ,[site_final]
          ,[hist_final]
          ,[behaviour_final]
          ,[laterality_final]
          ,[lvi_final]
          ,[date_final]
          ,[t24_final]
          ,[t25_final]
          ,[Neoadjuvant_final]
          ,[review_flag_1]
          ,[review_reason]
          ,[onco_msgid]
          ,[onco_site_code]
          ,[onco_hist_behav]
          ,[onco_laterality]
          ,[onco_dx_date]
          ,[onco_lvi]
          ,[onco_diagnostic_confirm]
          ,[onco_tfa_dx_group]
          ,[run_type]
          FROM [eMaRCPlus_v60Patch].[dbo].[bccr_consolidation]"

query4new <- "SELECT [msg_ids]
                    ,[dx_group]
                    ,[site_code]
                    ,[histology_code]
                    ,[behaviour_code]
                    ,[laterality]
                    ,[lymph_vascular_invasion]
                    ,[date_initial_diagnosis]
                    ,[review_flag_1]
                    ,[review_reason]
                    ,[invasive_surgery_flag]
                    ,[addendum_section_flag]
                    ,[neoadjuvant_therapy_flag]
                    ,[multifocal_flag]
                    ,[method_used_est_dx_date]
                    ,[diagnostic_confirm]
                    ,[grade_clin]
                    ,[grade_path]
                    ,[t_class_path]
                    ,[n_class_path]
                    ,[m_class_path]
                    ,[gleason_patterns_clin]
                    ,[gleason_score_clin]
                    ,[gleason_patterns_path]
                    ,[gleason_score_path]
                    ,[run_type]
                FROM [eMaRCPLus_v60Patch].[BCCR].[consolidation_pipeline]"

# Surveillance Pipeline
query5 <-  "SELECT 
             [msgid]
            ,[dx_group]
            ,[site_code]
            ,[histology_code]
            ,[behaviour_code]
            ,[laterality]
            ,[lymph_vascular_invasion]
            ,[invasive_surgery_flag]
            ,[addendum_section_flag]
            ,[diagnostic_procedure_flag]
            ,[neoadjuvant_therapy_flag]
            ,[run_type]
        FROM [eMaRCPlus_v60Patch].[dbo].[bccr_surveillance_pipeline]
"

# (new?) Surveillance pipeline 
query5new <- "SELECT [msg_id]
                    ,[site_code]
                    ,[histology_code]
                    ,[behaviour_code]
                    ,[laterality]
                    ,[lymph_vascular_invasion]
                    ,[invasive_surgery_flag]
                    ,[addendum_section_flag]
                    ,[neoadjuvant_therapy_flag]
                    ,[run_version]
                FROM [eMaRCPlus_v60Patch].[BCCR].[surveillance_pipeline]"

msgQuery <- "SELECT [msgid]
                   ,[msg_dt]
             FROM [eMaRCPLus_v60Patch].[dbo].[bccr_reportability_filter]"

auditQuery <- "SELECT prod_msgid,
                 ds_reportability, 
                 ds_dx_group, 
                 audit_msgid, 
                 tr_reportability, 
                 tr_dxgroup, 
                 import_dt_time 
                 FROM [eMaRCPlus_V90_Audit].[dbo].[bccr_ss_audit_temp]"

# Pre-Consolidation
# BR <- dbGetQuery(con, query1)
# CR <- dbGetQuery(con, query2)
# PR <- dbGetQuery(con, query3)


# DX Group, Message Date Lookup ---------------------------------------------------------

# Queries for DX Group and Message Date Data
dxGroupQuery <- "SELECT [msgid]
                    ,[final_label]
                FROM [eMaRCPLus_v60Patch].[BCCR].[bccr_dx_pipeline_new]"

dxGroupQuery2 <- "SELECT [msg_id]
                    ,[dx_group]
                  FROM [eMaRCPLus_v60Patch].[BCCR].[dx_group_pipeline]"

dxData1 <- dbGetQuery(con1, dxGroupQuery)
dxData2 <- dbGetQuery(con1, dxGroupQuery2) %>%
  rename(msgid = msg_id,
         final_label = dx_group)
dxData <- rbind(dxData1, dxData2)

messageDates <- dbGetQuery(con1, msgQuery)

# Pre-Consolidation Data -------------------------------------------------------

# FROM [eMaRCPLus_v60Patch].[BCCR].[surveillance_pipeline]
# Pre-Consolidation Pipeline (batch two)
SurveillancePipelineNew <- dbGetQuery(con1, query5new) %>%
  rename(msgid = 1) %>%
  left_join(messageDates, by = "msgid") %>%
  left_join(dxData, by = "msgid") %>%
  rename(dx_group = final_label) %>%
  mutate(Year = year(ymd(msg_dt)),
         ) %>%
  select(1, dx_group, 2:6, Year) %>%
  mutate(across(3:7, as.character))

# FROM [eMaRCPlus_v60Patch].[dbo].[bccr_surveillance_pipeline]
# Pre-Consolidation Pipeline (batch one)
SurveillancePipeline <- dbGetQuery(con1, query5) %>%
  filter(run_type == "P") %>%
  # filter(!(run_type == "T" & dx_group == "CR")) %>%
  left_join(messageDates, by = "msgid") %>%
  distinct(msgid, .keep_all = TRUE) %>%
  mutate(Year = year(ymd(msg_dt)),
         histology_code = case_when(histology_code == "Other" ~ "9999", TRUE ~ histology_code)
         ) %>%
  select(1:7, Year) %>%
  mutate(across(3:7, as.character))

# Pre-Consolidation Table - All
PreCon <- bind_rows(SurveillancePipeline, SurveillancePipelineNew) %>%
  rename(site = 3,
         histology = 4,
         behaviour = 5,
         laterality = 6,
         lvi = 7
  ) %>%
  mutate(lvi = case_when(lvi == "absent" ~ "0",
                         lvi == "present" ~ "1",
                         lvi == "indeterminate" ~ "9",
                         TRUE ~ lvi),
         laterality = case_when(laterality == "Manual Review" ~ "9",
                                TRUE ~ laterality
                                ),
         lvi = str_sub(lvi, 1, 1),
         histology = case_when(histology == "other" ~ "9999", TRUE ~ histology),
         laterality = str_sub(laterality, 1, 1)
         ) 

# Merging Post-Consolidation Data -----------------------------------------
# Post-Consolidation Pipeline (old)
# FROM [eMaRCPlus_v60Patch].[dbo].[bccr_consolidation]
ConsolidationPipeline <- dbGetQuery(con1, query4) %>%
  filter(run_type == "P") %>%
  rename(site = 3,
         histology = 4,
         behaviour = 5,
         laterality = 6,
         lvi = 7) %>%
  distinct(msgid, .keep_all = TRUE) %>%
  left_join(messageDates, by = "msgid") %>%
  mutate(Year = year(ymd(msg_dt))) %>%
  select(1:7, Year, review_flag_1, review_reason) %>%
  mutate(across(1:7, as.character)) %>%
  mutate(review_reason = case_when(str_count(review_reason, "\\/") == 1 ~ "2 Reasons",
                                   str_count(review_reason, "\\|") == 1 ~ "2 Reasons",
                                   str_count(review_reason, "\\/") == 2 ~ "3 Reasons",
                                   str_count(review_reason, "\\/") >= 3 ~ "4+ Reasons",
                                   TRUE ~ review_reason)
  ) %>%
  mutate(review_reason = recode(review_reason, 
                                "0: Unknown histology" = "Unknown histology",
                                "Unknown Histology" = "Unknown histology",
                                "1: Neoadjuvant found" = "Neoadjuvant found",
                                "Neoadjuvant Found" = "Neoadjuvant found",
                                "2: Invalid PHN" = "Invalid PHN",
                                "3: Invalid laterality" = "Invalid laterality",
                                "3: Unknown laterality" = "Invalid laterality",
                                "Invalid site" = "Invalid site",
                                "8: Multi focal site" = "Multi-focal site",
                                "out of date" = "Out of date",
                                "7: Invalid site" = "Invalid site")
  ) 

# Post-Consolidation Pipeline (new)
# FROM [eMaRCPlus_v60Patch].[dbo].[BCCR_consolidation]
ConsolidationPipelineNew <- dbGetQuery(con1, query4new) %>%
  rename(msgid = msg_ids) %>%
  distinct(msgid, .keep_all = TRUE) %>%
  mutate(Year = year(ymd(date_initial_diagnosis))) %>%
  select(1:7, Year, review_flag_1, review_reason) %>% 
  rename(site = 3,
         histology = 4,
         behaviour = 5,
         laterality = 6,
         lvi = 7
  ) %>%
  mutate(across(3:7, as.character)) %>%
  mutate(review_reason = case_when(str_count(review_reason, "\\|") == 1 ~ "2 Reasons",
                                   str_count(review_reason, "\\|") == 2 ~ "3 Reasons",
                                   str_count(review_reason, "\\|") >= 3 ~ "4+ Reasons",
                                   TRUE ~ review_reason)
  ) %>%
  mutate(review_reason = case_when(review_reason == "lymph_vascular_invasion" ~ "LVI",
                                   review_reason == "out of date" ~ "Out of date",
                                   str_detect(review_reason, "gleason") ~ "Gleason Pattern",
                                   review_reason == "neoadjuvant" ~ "Neoadjuvant found",
                                   review_reason == "phn" ~ "Invalid PHN",
                                   str_detect(review_reason, "histology") ~ "Unknown histology",
                                   str_detect(review_reason, "laterality") ~ "Invalid laterality",
                                   str_detect(review_reason, "site") ~ "Invalid site",
                                   TRUE ~ review_reason))

PostCon <- bind_rows(ConsolidationPipeline, ConsolidationPipelineNew)
  
# Evaluation Query Results ------------------------------------------------

evalQueryData <- read.csv("H:/Data Integrity Analyst/Data Science/Operations/Evaluation/input/eval_qry_results.csv") %>%
  filter(surv_load == 1)

# Global Functions -------------------------------------------------------------------

# Function: Evaluation Query, Summary Bar Plot (position_dodge()) Function 
summarisePCC <- function(dataSet, svlValue, valueType) {
  
  # "Manually" converting the input svlValue to the corresponding column in the dataset
  # Because function can't properly process "site_match" or "hist_match" 
  if (svlValue == "Site") {
    match <- "site_match"
  } else if (svlValue == "Histology") {
    match <- "hist_match"
  } else if (svlValue == "LVI") {
    match <- "lvi_match"
  } else if (svlValue == "Laterality") {
    match <- "lat_match"
  }
  
  # Generating the summary, Count/Proportion "Change" Indicator post-consolidation
  summary <- dataSet %>%
    group_by(tfa_diagnosis_group_descr, diagnosis_year, !!sym(match)) %>%
    summarise(Count = n(),
              .groups = "drop") %>%
    group_by(tfa_diagnosis_group_descr, diagnosis_year) %>%
    mutate(Proportion = round(Count/sum(Count), 3)) %>%
    mutate(diagnosis_year = as.character(diagnosis_year)) %>%
    rename("DX Group" = 1,
           "Diagnosis Year" = 2
    )
  
  summaryPlot <- summary %>%
    mutate(changed = ifelse(!!sym(match) == 0, "Yes", "No")) %>%
    ggplot(mapping = aes(x = !!sym("Diagnosis Year"), y = !!sym(valueType), fill = changed)) +
    geom_bar(stat = "identity", position = position_dodge(), color = "black") +
    theme_ipsum() +
    theme(axis.text.x = element_text(angle = 45)
    ) +
    labs(x = "Year",
         y = valueType,
         fill = paste0("Changed ", svlValue)
    ) +
    scale_fill_manual(values = c("No" = "#8F8EA3", "Yes" = "#605ca8"))
  
  summaryPlotly <- ggplotly(summaryPlot) %>%
    layout(margin = list(t = 50, b = 30))
  
  return(summaryPlotly)
  
}

# Function: Refactoring prediction and reference columns to share the same factors for each metric (i.e., site)
levelFactor <- function(col, col_name, dx) {
  
  # Mapping to correpsonding column
  if (!endsWith(col_name, "_actual")) {
    otherCol <- get(paste0(col_name, "_actual"), envir = parent.frame())
  } else if (endsWith(col_name, "_actual")) {
    otherCol <- get(gsub("_actual", "", col_name), envir = parent.frame())
    
  }
  
  # Coercing single-factor columns to fit summary table format - dummy factors
  if (length(unique(c(col, otherCol))) <= 2) {
    return(factor(col, levels = c(sort(unique(c(col, otherCol))), "NA", "NAN")))
  } else {
    return(factor(col, levels = sort(unique(c(col, otherCol)))))
  }
  
}

# Function: Evaluation Summary Table Generation Function 
plotPCCmatrix <- function(loadValue, finalValue, metricName, dataSet) {
  
  matrixAsTable <- table(Initial = dataSet[[loadValue]], 
                         Final = dataSet[[finalValue]], 
                         Year = as.character(dataSet$diagnosis_year))
  
  matrixAsTable <- as.data.frame(matrixAsTable) %>%
    mutate(Initial = as.character(Initial),
           Final = as.character(Final)
           ) %>%
    mutate(Proportion = round(Freq/sum(Freq), 3)) %>%
    mutate(Class = case_when(Initial == Final & Freq > 0 ~ "No Change",
                             Initial != Final & Freq > 0 ~ "Changed",
                             TRUE ~ "None")
    ) %>%
    arrange(desc(Freq))
  
  datatableOutput <- datatable(matrixAsTable,
                               filter = 'top',
                               rownames = FALSE,
                               options = list(
                                 searchCols = list(
                                   NULL,
                                   NULL, 
                                   NULL,
                                   NULL,
                                   NULL,
                                   list(search = "Changed")
                                   ),
                                 pageLength = 10
                                 )
                               )
  
  return(datatableOutput)
  
}

# Function: Plotly Graph Generation Function 
getPlotly <- function(dataSet, svlValue, chooseView) {
  
  if (svlValue == "Site") {
    metric <- "site"
  } else if (svlValue == "Histology") {
    metric <- "histology"
  } else if (svlValue == "LVI") {
    metric <- "lvi"
  } else if (svlValue == "Laterality") {
    metric <- "laterality"
  } else if (svlValue == "Behaviour") {
    metric <- "behaviour"
  }
  
  # Sum of Pre- and Post- Consolidation Counts (all)
  freqTotal <- dataSet %>%
    group_by(type) %>%
    summarise(total = n(), .groups = "drop")
  
  # Sum of Pre- and Post- Consolidation Counts (all)
  freq <- dataSet %>%
    group_by(type, !!sym(metric)) %>%
    summarise(Count = n(),.groups = "drop") %>%
    left_join(freqTotal, by = "type") %>%
    group_by(type, !!sym(metric)) %>%
    summarise(Count, 
              Proportion = round(Count/total, digits = 4),
              .groups = "drop") %>%
    mutate(type = factor(type, levels = c("Pre-Consolidation", "Post-Consolidation")))
  
  # Generating consolidated counts
  totalsPre <- dataSet %>%
    filter(type == "Pre-Consolidation") %>%
    group_by(!!sym(metric)) %>%
    summarise(pre_total = n(), .groups = "drop")
  
  # Counts by pre- and post-consolidation
  consolidated <- dataSet %>%
    group_by(!!sym(metric), type) %>%
    summarise(total = n(), .groups = "drop") %>%
    ungroup() %>%
    group_by(!!sym(metric)) %>%
    summarise(n_consolidated = abs(diff(total)), .groups = "drop") %>%
    left_join(totalsPre, by = setNames(metric, metric)) %>%
    mutate(prop_consolidated = round(n_consolidated/pre_total, digits = 4)) %>%
    select(1, 2, 4) %>%
    rename("N Consolidated" = 2,
           "Proportion Consolidated" = 3)
  
  # Setting angle of x-axis labels by the number of levels of the metric
  if (length(unique(freq[[2]])) <= 8) {
    angleDegree <- 0
  } else {
    angleDegree <- 45
  }
  
  if (chooseView == "Count" || chooseView == "Proportion") {
    plot <- freq %>%
      ggplot(mapping = aes(x = !!sym(metric), y = !!sym(chooseView), fill = type)) +
      geom_bar(stat = "identity", position = position_dodge(), color = "black") +
      theme_ipsum() +
      theme(axis.text.x = element_text(angle = angleDegree)
      ) +
      labs(x = str_c(svlValue, " Code"),
           y = chooseView,
           fill = "Type") +
      scale_fill_manual(values = c("Pre-Consolidation" = "#8F8EA3", "Post-Consolidation" = "#605ca8")) 
  } else if (chooseView == "N Consolidated" || chooseView == "Proportion Consolidated") {
    plot <- consolidated %>%
      ggplot(mapping = aes(x = !!sym(metric), y = !!sym(chooseView))) +
      geom_bar(stat = "identity", fill = "#605ca8", color = "black") +
      theme_ipsum() +
      theme(axis.text.x = element_text(angle = angleDegree)
      ) +
      labs(x = str_c(svlValue, " Code"),
           y = chooseView
      )
  }
  
  plotly <- ggplotly(plot) %>%
    layout(
      annotations = list(
        list(
          x = 0,
          y = -0.5,
          showarrow = FALSE,
          xref = "paper",
          yref = "paper",
          xanchor = "left",
          yanchor = "top",
          font = list(size = 15, color = "gray")
        )
      ),
      margin = list(t = 50, b = 30)
    )
  
  return(plotly)
  
}

# Pulling DX and Reportability Audit Data - Re-coding & Applying Factors ------------------------------------------------------

dxGroups <- c("BR", "CR", "CX", "GI", "GU", "GY", "HN", "LK", "LU", "LY", "MM", "ME", "NE", "OO", "PR", "PU", "SA", "SK", "TH", "UNK")

metricGroups <- c("Sensitivity", "Specificity", "Pos Pred Value", "Neg Pred Value", "Precision", "Recall", "F1", "Prevalence", "Detection Rate",
                  "Detection Prevalence", "Balanced Accuracy")

svlGroups <- c("Site", "Histology", "Behaviour", "Laterality", "LVI")

reportFactor <- c("Yes", "No")

auditData <- dbGetQuery(con, auditQuery) %>%
  mutate(Month = format(ymd(str_sub(import_dt_time, 1, 10)), "%B %Y"),
         ds_reportability = factor(ds_reportability, levels = c(1, 0, 2), labels = c("Yes", "No", "NA")),
         ds_dx_group = factor(ds_dx_group),
         tr_reportability = factor(tr_reportability, levels = c(1, 0, 2), labels = c("Yes", "No", "NA")),
         tr_dxgroup = factor(trimws(toupper(tr_dxgroup))),
  ) %>%
  mutate(tr_dxgroup = factor(tr_dxgroup, levels = dxGroups),
         ds_dx_group = factor(ds_dx_group, levels = dxGroups)
  )


# Pulling Surveillance Audit Data - Formatting -----------------------------------------
# Re-level function for generating the confusion matrices


# Combining Surveillance Audit Data ---------------------------------------

PRsurveillanceData <- read.csv("H:/Data Integrity Analyst/Data Science/dashboard data/all dashboard files/surveillance_prostate_audit_data.csv") %>%
  select(1:7, 9, 11, 13, 15) %>%
  mutate(dx_group = "PR") %>%
  mutate(lymph_vascular_invasion = recode(lymph_vascular_invasion, "Y" = 1, "N" = 0, "U" = 9),
         lymph_vascular_invasion_actual = recode(lymph_vascular_invasion_actual, "Y" = 1, "N" = 0, "U" = 9)
  )
  # mutate(across(c(2:11), ~ levelFactor(.x, cur_column(), "PR")))

BRsurveillanceData <- read.csv("H:/Data Integrity Analyst/Data Science/dashboard data/all dashboard files/surveillance_breast_audit_data.csv") %>%
  select(1:6, 10, 12, 14, 16, 18) %>%
  mutate(dx_group = "BR")
  # mutate(across(c(2:11), ~ levelFactor(.x, cur_column(), "BR")))

CRsurveillanceData <- read.csv("H:/Data Integrity Analyst/Data Science/dashboard data/all dashboard files/surveillance_colorectal_audit_data.csv") %>%
  select(1:5, 9:10, 12, 14, 16, 18) %>%
  mutate(dx_group = "CR") %>%
  mutate(across(c(2:11), ~ levelFactor(.x, cur_column(), "CR")))

surveillanceData <- BRsurveillanceData %>%
  rbind(PRsurveillanceData) %>%
  rbind(CRsurveillanceData)
