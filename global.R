##### CAN-REP Global File

BCCRVW <- read.csv("H:/Data Integrity Analyst/Data Science/dashboard data/volumes/bccr_vw_can_rep.csv", header=FALSE)

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

###### Data Wrangling 
#####################

BCCRVW <- BCCRVW %>%
  select(1:6)
colnames(BCCRVW) <- c("msgid", "fulldate", "HA", "XHA", "combined_label", "final_label")
BCCRVW$fulldate <- ymd(BCCRVW$fulldate)

reportraw <- BCCRVW %>%
  select(1:2, 5, 3)  

diagraw <- BCCRVW %>%
  select(1:2, 6, 3) %>%
  filter(final_label != "" & !is.na(final_label))

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
    mutate(low = ceiling(quantile(volume, 0.25) - (1.5 * IQR(volume))),
           high = ceiling(quantile(volume, 0.75) + (1.5 * IQR(volume))),
           "Median" = quantile(volume, 0.5),
           outlier = ifelse((volume < low | volume > high), "Yes", "No"),
           "Outlier Bounds" = str_c("(", low, ", ", high, ")"),
           "Month" = format(month, "%B %Y")
    ) %>%
    ungroup() %>%
    filter(outlier == "Yes") %>%
    arrange(desc(month))
  outliers <- outliers %>%
    rename("Volume" = 3) %>%
    select(1, 9, 8, 3)
  return(outliers)
}