library(dplyr)
library(redcapAPI)
library(stringr)
library(ggplot2)

# source fucntions
source("functions.R")

# set plotting parameters
theme_set(theme_classic(base_size = 15))
# main:background ----------
# pull REDCap enrollment data, save to history
# this will be a "refresh data" button
rcon <- build_rcon("rc_proper")
rc_df <- download_rc_dataframe(rcon, c(sl_values,"is2_date"), NULL)



current_enrollment <- suppressWarnings(get_current_enrollment(rc_df))
append_enrollment(current_enrollment) # no return, saves to .tsv file


# load time-series enrollment
enrollment_history <- load_enrollment_history()

# for app display --------
summarize_enrollment(enrollment_history) # gives tibble


#plots
plot_enrollment(enrollment_history)



ivr <- load_ivr()

ivr_stats <- function(df){
  # summarize interesting IVR statistics
  df <- df %>% 
    summarise(study_packs = sum(studycigs, na.rm = TRUE)/20,
              ub_packs = sum(nonstudycigs, na.rm = TRUE)/20,
              study_pods = sum(nstudypods, na.rm = TRUE),
              ub_pods = sum(nnonstudypods, na.rm = TRUE))
  return(df)
}


ivr_summary <- function(ivr){
  df <- ivr %>% 
    group_by(subjectid, project, site) %>%
    summarise(studycigs = mean(studycigs, na.rm = TRUE),
              nonstudycigs = mean(nonstudycigs, na.rm = TRUE)) %>%
    mutate(adherence = studycigs / sum(studycigs, nonstudycigs, na.rm = TRUE)) %>%
    mutate(nonadherent = adherence < .5)
  return(df)
}

ivr_summary <- ivr_summary(ivr)

plot_cig_adherence <- function(df){
  # scatter plot of avg study and nonstudy cpd
  xmax <- max(ivr_summary$studycigs, na.rm = TRUE) + 10
  ggplot(ivr_summary, aes(x = studycigs, y = nonstudycigs)) +
    geom_point(aes(color = project, shape = project), size = 3) +
    geom_label(data = df %>% filter(nonadherent == TRUE),
              aes(studycigs, nonstudycigs, label = subjectid)) +
    xlim(0, xmax) + ylim(0, xmax) + 
    coord_fixed() +
    geom_abline(slope = 1, linetype = "dashed") +
    labs(x = "Avg. Study CPD",
         y = "Avg. UB CPD") +
    theme(legend.position = "top")
  
}

plot_cig_adherence(ivr_summary)

ip_list <- in_progress_list(rc_df)

current_enrollment$date <- Sys.Date()
write.table(current_enrollment, file = "enrollment_history.tsv", sep = "\t", row.names = FALSE)

enrollment_history <- enrollment_history %>% filter(project == "Project 1")
plot_randomization_goal(enrollment_history, "2020-01-01", "2023-06-01",
                        0, 212)


enrollment_history <- load_enrollment_history()
if (max(enrollment_history$date) != Sys.Date()){
  rcon <- build_rcon("rc_proper")
  rc_df <- download_rc_dataframe(rcon, c(sl_values, "is2_date"), NULL)
  current_enrollment <- suppressWarnings(get_current_enrollment(rc_df))
  ip_list <- in_progress_list(rc_df) # status == "In Progress" current pull
  append_enrollment(current_enrollment)
  enrollment_history <- load_enrollment_history()
} else {
  current_enrollment <- enrollment_history %>%
    filter(date == max(date))
  ip_list <- read.csv("in_progress_list.csv", stringsAsFactors = FALSE)
}


df <- data.frame(date = Sys.Date())
write.table(df, file = "logfile.tsv", sep = "\t", row.names = FALSE, col.names = TRUE)

randomization_needed <- function(current_enrollment, max_date, 
                                 goal_p1_p3, goal_p2){
  w <- difftime(ymd(max_date), Sys.Date(), units = "weeks")
  df <- current_enrollment %>%
    select(project, randomized) %>%
    mutate(difference = ifelse(project == "Project 1" | project == "Project 3",
                               goal_p1_p3 - randomized, goal_p2 - randomized)) %>%
    mutate(per_week = difference / as.integer(ceiling(w)))
  
  return(df)
  
}



