# Script to query current data source. Should be controlled by 
# crontab.
library(dplyr)
library(redcapAPI)
library(stringr)
library(lubridate)
# source fucntions
source("functions.R")
# source("diagrammer_flowchart.R")

logfile <- load_logfile()
logfile$date <- ymd_hms(logfile$date)

# TCORS STUDY 3 ---------------
min_date <- ymd("2020-01-01")
# enrollment

# REDCap connections
rcon <- build_rcon("rc_proper")

# prescreen

ps_df <- pull_prescreen_data()


write.csv(ps_df, "s3/df_ps.csv", row.names = FALSE)


rc_df <- download_rc_dataframe(rcon, c(sl_values, session_dates, 
                                       "baseline_2_interviewer_complete",
                                       co_values), NULL)

current_enrollment <- suppressWarnings(get_current_enrollment(rc_df))


ps <- summarize_prescreen(ps_df, min_date)
ps_history <- ps[[1]]
ps_summary <- ps[[2]]
write.csv(ps_history, "s3/prescreen_history.csv", row.names = FALSE)
write.csv(ps_summary, "s3/prescreen_summary.csv", row.names = FALSE)

# current_ps <- summarize_prescreen(ps_df)[[2]]
ip_list <- in_progress_list(rc_df, "s3/") # status == "In Progress" current pull
append_enrollment(current_enrollment, "s3/") 


# append_prescreen(current_ps, "s3/")
# ps_history <- load_prescreen_history("s3/")

enrollment_history <- load_enrollment_history("s3/")
write.table(Sys.time(), file = "logfile.tsv", append = TRUE,
            sep = "\t", col.names = FALSE, row.names = FALSE) # append logfile
df_ps_inel <- ps_ineligible_recode(ps_df, "s3/")
ps_inel_generic(df_ps_inel, "s3/")
ps_inel_specific(df_ps_inel, "s3/")
ps_inel_one(df_ps_inel, "s3/")
screening_inel(rc_df, "s3/") #PILOT
# ps_df_sum <- summarize_prescreen(ps_df, min_date)


# make an estimate of UVM screenings scheduled
est_scrn_sched(build_rcon("ps_proper_uvm"))

ivr <- load_ivr("s3/")



# scratch

# TCORS STUDY 2 PROJECT 4 --------------

# REDCap connections

preg_ps_uvm <- ps_df %>% filter(pregnant == 1)

rcon <- build_rcon("rc_p4")
p4_df <- download_p4_df(rcon, c(all_of(sl_p4), "baseline_2_interviewer_complete"), NULL)



# rcon_ps_p4 <- build_rcon("ps_p4_uvm")
# ps_p4 <- download_ps_p4(rcon_ps_p4, NULL, NULL, "uvm")

# rcon <- build_rcon("rc_p4")
# p4_df <- download_p4_df(rcon, c(all_of(sl_p4), "baseline_2_interviewer_complete"), NULL)
# # #
# current_enrollment <- get_current_enrollment(p4_df)
# ip_list <- in_progress_list(p4_df, "p4/") # status == "In Progress" current pull
# append_enrollment(current_enrollment, "p4/")


