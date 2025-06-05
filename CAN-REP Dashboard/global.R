##### CAN-REP Global File

# Libraries & Data Wrangling
#####
library(tidyverse)
library(ggflowchart)
library(scales)
library(shinydashboard)
library(hrbrthemes)
library(DT)
library(plotly)
library(shinyWidgets)
library(ggpubr)
library(DBI)
library(odbc)


# Connections to the server -----------------------------------------------

# for the overall view
con <- dbConnect(odbc::odbc(),
                 Driver = "SQL Server",
                 Server = "spdbsmarc001",
                 Database = "eMaRCPlus_v60Patch",
                 Trusted_Connection = "Yes")

query <- "SELECT [msgid]
              ,[msg_dt]
              ,[ha]
              ,[reportability_filter]
              ,[final_label]
          FROM [eMaRCPLus_v60Patch].[dbo].[bccr_vw_can_rep]"

# CAN-REP View
BCCRVW <- dbGetQuery(con, query)


# For filling in DX Groups - where final_label is missing (after mid-2024)
dxGroupQuery <- "SELECT [msgid]
                    ,[final_label]
                FROM [eMaRCPLus_v60Patch].[BCCR].[bccr_dx_pipeline_new]"

dxGroupQuery2 <- "SELECT [msg_id]
                    ,[dx_group]
                  FROM [eMaRCPLus_v60Patch].[BCCR].[dx_group_pipeline]"

dxData1 <- dbGetQuery(con, dxGroupQuery) %>%
  rename(finalLabel = final_label)
dxData2 <- dbGetQuery(con, dxGroupQuery2) %>%
  rename(msgid = msg_id,
         finalLabel = dx_group)
dxData <- rbind(dxData1, dxData2)

# Formatting datasets for analysis ----------------------------------------

BCCRVW <- BCCRVW %>%
  left_join(dxData, by = "msgid") %>%
  mutate(final_label = ifelse(is.na(final_label), finalLabel, final_label)) %>%
  mutate(msg_dt = ymd(msg_dt),
         ha = case_when(ha == "FH" ~ "FHA", TRUE ~ ha)
         ) %>%
  rename(fulldate = msg_dt,
         HA = ha,
         combined_label = reportability_filter)
  # select(-6)

# Separating reportability data and diagnostic data

# reportability data
reportraw <- BCCRVW %>%
  select(1:2, 4, 3)  

# final_label / dx group data (reportables only)
diagraw <- BCCRVW %>%
  filter(combined_label == 1) %>%
  select(1:2, 5, 3)
  

#####################

##### Functions #####
#####################

createFlowchart <- function(x, y, a, b, c) {
  
  totals <- a %>%
    filter(fulldate >= x & fulldate <= y) %>%
    rename("Non-Reportable" = NonReportable)
  totals <- totals[2:4]
  
  diag3 <- b %>%
    filter(fulldate >= x & fulldate <= y) %>%
    group_by(final_label) %>%
    rename("Predicted Label" = final_label) %>%
    summarise(Volume = sum(volume), 
              "% Total Reportable" = percent((sum(volume)/sum(totals[2])), accuracy = 0.1)) %>%
    ungroup() %>%
    arrange(desc(Volume))
  
  totallist <- vector()
  for (i in seq_along(totals)) {
    totallabels <- colnames(totals)
    grandtotal <- sum(totals[3])
    totallist <- append(
      totallist, 
      str_c(totallabels[i], ": \n", sum(totals[i]), " (",
            percent((sum(totals[i])/grandtotal), accuracy = 0.1), ")"))
  }
  
  text_table <- ggtexttable(
    diag3,
    rows = NULL,
    theme = ttheme(
      colnames.style = colnames_style(
        fill = "white",
        linecolor = "black",
        size = 10),
      tbody.style = tbody_style(
        fill = "white",
        linecolor = "black",
        size = 10
      ),
    ) 
  ) 
  
  flow1 <- tibble::tibble(
    from = c(totallist[3], totallist[2], totallist[3]),
    to = c(totallist[2], "", totallist[1])
  )
  
  flowchart <- ggflowchart(
    flow1, 
    horizontal = TRUE, 
    x_nudge = 0.15, 
    y_nudge = 0.2, 
    fill = "white", 
    color = "black",
    arrow_size = 0.5,
    family = "Arial Narrow",
    text_size = 5
  ) +
    ggtitle(label = str_c("Showing volumes from ", x, " to ", y, " (", as.numeric(difftime(y, x, units = "days")), " days) - ",
                          "for health authorities ", toString(c))) +
    scale_x_reverse() +
    theme(text = element_text(size = 15, family = "Arial Narrow"))
  
  combined <- flowchart + annotation_custom(ggplotGrob(text_table),
                                            xmin = -0.28,
                                            ymin = -0.18)
  return(combined)
}

# Summary table generation function
getStats <- function(x, y, z, group_col) {
  
  filt <- z %>%
    filter(fulldate >= x & fulldate <= y)
  
  dailyval <- filt %>%
    group_by(across(all_of(group_col)), fulldate) %>%
    summarise(volume = sum(volume), .groups = 'drop') %>%
    group_by(across(all_of(group_col))) %>%
    summarise("Daily Average" = ceiling(mean(volume)),
              "Daily Range" = str_c("(", min(volume), ", ", max(volume), ")"),
              .groups = 'drop')
  
  weeklyval <- filt %>%
    group_by(across(all_of(group_col)), year(fulldate), week(fulldate)) %>%
    summarise(volume = sum(volume), .groups = 'drop') %>%
    group_by(across(all_of(group_col))) %>%
    summarise("Weekly Average" = ceiling(mean(volume)),
              "Weekly Range" = str_c("(", min(volume), ", ", max(volume), ")"), .groups = 'drop')
  
  monthlyval <- filt %>%
    group_by(across(all_of(group_col)), year(fulldate), month(fulldate)) %>%
    summarise(volume = sum(volume), .groups = 'drop') %>%
    group_by(across(all_of(group_col))) %>%
    summarise("Monthly Average" = ceiling(mean(volume)),
              "Monthly Range" = str_c("(", min(volume), ", ", max(volume), ")"),
              "Total" = sum(volume), 
              .groups = 'drop')
  
  summarytable <- dailyval %>%
    inner_join(weeklyval, by = group_col) %>%
    inner_join(monthlyval, by = group_col)
  
  if (group_col == "HA") {
    summarytable <- summarytable %>%
      rename("Health Authority" = 1)
  } else if (group_col == "final_label") {
    summarytable <- summarytable %>%
      rename("Final Label" = 1)
  }
  
  return(summarytable)
}

# Function for calculating number of breaks
nBreaks <- function(date1, date2) {
  diffmonths <- as.numeric(difftime(date2, date1, units = "days") / 365) * 12
  if (diffmonths <= 24) {
    breakFreq <- "1 month"
  } else {
    breakFreq <- "2 month"
  }
  return(breakFreq)
}

# Function for generating outliers
getOutliers <- function(date1, date2, data, group_col) {
  outliers <- data %>%
    filter(fulldate >= date1 & fulldate <= date2) %>%
    group_by({{group_col}}, month = floor_date(fulldate, unit = "month")) %>%
    summarise(volume = sum(volume)) %>%
    ungroup() %>%
    group_by({{group_col}}) %>%
    mutate(low = ceiling(quantile(volume, 0.10)),
           high = ceiling(quantile(volume, 0.75) + (1.5 * IQR(volume))),
           "Median" = quantile(volume, 0.5),
           outlier = case_when(volume > high ~ "High",
                               volume < low ~ "Low",
                               TRUE ~ "No"),
           "Outlier Bounds" = str_c("(", low, ", ", high, ")"),
           "Month" = format(month, "%B %Y")
    ) %>%
    ungroup() %>%
    filter(outlier != "No") %>%
    mutate("Proportion Above/Below Threshold" = case_when(outlier == "High" ~ round((volume - high)/high, 3),
                                                          outlier == "Low" ~ round((volume - low)/low, 3),
                                                          TRUE ~ NA)
    ) %>%
    arrange(desc(month))
  outliers <- outliers %>%
    rename("Volume" = 3) %>%
    select(1, 9, 8, 3, 10)
  return(outliers)
}
