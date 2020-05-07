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
rc_df <- download_rc_dataframe(rcon, sl_values, NULL)
current_enrollment <- suppressWarnings(get_current_enrollment(df))
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
              nonstudycigs = mean(nonstudycigs, na.rm = TRUE))
  return(df)
}

ivr_summary <- ivr_summary(ivr)

plot_cig_adherence <- function(df){
  # scatter plot of avg study and nonstudy cpd
  xmax <- max(ivr_summary$studycigs, na.rm = TRUE) + 10
  ggplot(ivr_summary, aes(x = studycigs, y = nonstudycigs)) +
    geom_point(aes(color = project, shape = project)) +
    xlim(0, xmax) + ylim(0, xmax) +
    geom_abline(slope = 1, linetype = "dashed") +
    labs(x = "Avg. Study CPD",
         y = "Avg. UB CPD")
  
}

plot_cig_adherence(ivr_summary)

ip_list <- in_progress_list(rc_df)
