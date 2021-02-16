# Script to query current data source. Should be controlled by 
# crontab.
library(dplyr)
library(redcapAPI)
library(stringr)
library(lubridate)
# source fucntions
source("functions.R")

logfile <- load_logfile()
logfile$date <- ymd_hms(logfile$date)

# TCORS STUDY 3 ---------------

# enrollment

# REDCap connections
rcon <- build_rcon("rc_proper")
rcon_ps_uvm <- build_rcon("ps_proper_uvm")
rcon_ps_brown <- build_rcon("ps_proper_brown")
rcon_ps_jhu <- build_rcon("ps_proper_jhu")



# prescreen
ps_df_uvm <- download_ps(rcon_ps_uvm, NULL, NULL, "uvm", all_of(vars_to_keep))
ps_df_brown <- download_ps(rcon_ps_brown, NULL, NULL, "brown", all_of(vars_to_keep))
#ps_df_jhu <- download_ps(rcon_ps_jhu, ps_values, NULL)
ps_df <- rbind(ps_df_uvm, ps_df_brown)

# recruitment source
assemble_rct_source(ps_df, source_vars, "s3/")


rc_df <- download_rc_dataframe(rcon, c(sl_values, "baseline_2_interviewer_complete"), NULL)
current_enrollment <- suppressWarnings(get_current_enrollment(rc_df))

current_ps <- summarize_prescreen(ps_df)
ip_list <- in_progress_list(rc_df, "s3/") # status == "In Progress" current pull
append_enrollment(current_enrollment, "s3/")
append_prescreen(current_ps, "s3/")
ps_history <- load_prescreen_history("s3/")
enrollment_history <- load_enrollment_history("s3/")
write.table(Sys.time(), file = "logfile.tsv", append = TRUE,
            sep = "\t", col.names = FALSE, row.names = FALSE) # append logfile
df_ps_inel <- ps_ineligible_recode(ps_df, "s3/")
ps_inel_generic(df_ps_inel, "s3/")
ps_inel_specific(df_ps_inel, "s3/")
ps_inel_one(df_ps_inel, "s3/")
screening_inel(rc_df, "s3/") #PILOT
ps_df_sum <- summarize_prescreen(ps_df)

# ivr
ivr <- load_ivr("s3/")




# TCORS STUDY 2 PROJECT 4 --------------
# rcon <- build_rcon("rc_p4")
# p4_df <- download_p4_df(rcon, c(all_of(sl_p4), "baseline_2_interviewer_complete"), NULL)
# #
# current_enrollment <- get_current_enrollment(p4_df)
# ip_list <- in_progress_list(p4_df, "p4/") # status == "In Progress" current pull
# append_enrollment(current_enrollment, "p4/")
