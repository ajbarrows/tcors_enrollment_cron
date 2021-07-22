# TCORS functions

build_rcon <- function(rc){
  # Wrapper around the redcapAPI::redcapConnection() using
  # a .csv token sheet in the working directory.
  url <- 'https://redcap.med.uvm.edu/api/'
  # import passwords document
  pw <- read.csv("password.csv", stringsAsFactors = FALSE)
  token <- NA
  if(rc == "rc_pilot"){
    token <- pw$password[2]
  } else if (rc == "rc_proper"){
    token <- pw$password[4]
  } else if (rc == "ps_pilot_uvm") {
    token <- pw$password[3]
  } else if (rc == "ps_proper_uvm") {
    token <- pw$password[4]
  } else if (rc == "rc_p4") {
    token <- pw$password[6]
  }
  rcon <- redcapConnection(url = url, token = token)

  return(rcon)
}
# build_rcon <- function(rc){
#   url <- 'https://redcap.med.uvm.edu/api/'
#   # import passwords document
#   pw <- read.csv("password.csv", stringsAsFactors = FALSE)
#   token <- NA
#   if(rc == "rc_pilot"){
#     token <- pw$password[2]
#   } else if (rc == "rc_proper"){
#     token <- pw$password[4]
#   } else if (rc == "ps_pilot_uvm") {
#     token <- pw$password[3]
#   } else if (rc == "ps_proper_uvm") {
#     token <- pw$password[4]
#   } else if (rc == "rc_p4") {
#     token <- pw$password[6]
#   }
#   #rcon <- redcapConnection(url = url, token = token)
#   rcon <- data.frame(url = url, token = token)
#   return(rcon)
# }

# download_rc_dataframe <- function(rcon){
#   # Download dataframe -----------------
#   result <- postForm(
#     uri=rcon$url[1],
#     token=rcon$token[1],
#     content='record',
#     format='json',
#     type='flat',
#     csvDelimiter='',
#     'fields[0]'='screen_id',
#     'fields[1]'='sl_status',
#     'fields[2]'= "sl_condition",
#     'fields[3]'='sl_ecig_condition',
#     'fields[4]'='sl_juul_vuse',
#     'fields[5]'='screen_project',
#     'fields[6]'='screen_date',
#     'fields[7]'='rescreen_date',
#     'fields[8]'='is1_date',
#     'fields[9]'='is2_date',
#     'fields[10]'='iodd_date_session',
#     'fields[11]'='ievn_date',
#     'fields[12]'='iab_date',
#     rawOrLabel='label',
#     rawOrLabelHeaders='raw',
#     exportCheckboxLabel='false',
#     exportSurveyFields='false',
#     exportDataAccessGroups='true',
#     returnFormat='json'
#   )
#   
#   df <- fromJSON(result)
#   
#   df <- df %>% filter(!grepl("-2", df$screen_id))
#   df <- df %>% filter(grepl("X-", df$screen_id) |
#                         grepl("Y-", df$screen_id) |
#                         grepl("Z-", df$screen_id))
#   return(df)
# }

#rc_df_compare <- download_rc_dataframe(rcon) 
download_rc_dataframe <- function(rcon, fields, events){
  # Wrapper around redcapAPI::exportRecords(). Returns dataframe
  # with clean subject IDs.
  
  df <- exportRecords(rcon, fields = c("screen_id", fields), labels = FALSE, survey = FALSE,
                      dag = TRUE, events = events, form_complete_auto = FALSE,
                      dates = FALSE)
  df <- df %>% filter(!grepl("-2", df$screen_id))
  df <- df %>% filter(grepl("X-", df$screen_id) |
                        grepl("Y-", df$screen_id) |
                        grepl("Z-", df$screen_id))
  return(df)
}

clean_redcap_id <- function(df) {
  df$redcap_id <- toupper(df$redcap_id)
  df <- df %>%
    filter(grepl("S", df$redcap_id))
  df <- df %>% filter(!grepl("-", df$redcap_id))
  #filter(str_length(df$redcap_id) == 7)
  
  return(df)
}

pjt_ste <- function(df){
  # impose project and site values on a data frame
  pjt <- substr(df$subjectid, 1, 1)
  ste <- substr(df$subjectid, 3, 3)
  
  df$project <- NA
  df$project[pjt == "X"] <- "Project 1"
  df$project[pjt == "Y"] <- "Project 2"
  df$project[pjt == "Z"] <- "Project 3"
  
  df$site <- NA
  df$site[ste == "A"] <- "uvm"
  df$site[ste == "B"] <- "brown"
  df$site[ste == "C"] <- "jhu"
  df$site <- factor(df$site, levels = c("uvm", "brown", "jhu"))
  
  return(df)
}


load_ivr <- function(){
  # load and clean IVR data externally
  user <- "tcorsstudy3"
  pw <- "ArdentFoamBlur47"
  url <- paste('https://', user, ':', pw,
               '@rd.telesage.com/tcorsstudy3/surveys.tsv', sep = '')
  df <- read.csv(url, header = TRUE, sep = "\t", na.strings = "")
  
  # clean
  colnames(df) <- tolower(colnames(df))
  df$subjectid <- toupper(df$subjectid)
  df$subjectid <- gsub("-", "", df$subjectid)
  df$subjectid <- gsub("A", "-A", df$subjectid)
  df$subjectid <- gsub("B", "-B", df$subjectid)
  df$subjectid <- gsub("C", "-C", df$subjectid)
  
  df <- pjt_ste(df)
  
  # remove incomplete calls
  df <- df %>% filter(!is.na(complete))
  
  # write record
  write.table(df, "ivr.tsv", sep = "\t",
              row.names = FALSE)
  return(df)
}

ivr_timeseries <- function(ivr, sid) {

  df <- ivr %>%
    filter(subjectid == sid) %>%
    select(daynumber, daynumberbl2, cigs, studycigs, nonstudycigs,
           ecig, studyecig, nonstudyecig, nstudypods,
           nnonstudypods) 
  
  colnames(df) <- str_replace_all(colnames(df), "study", "study_")
  colnames(df) <- str_replace_all(colnames(df), "nstudy_pods", "study_pods")
  colnames(df) <- str_replace_all(colnames(df), "nnonstudy_pods", "nonstudy_pods")
  
  df <- df %>%
    select(daynumber, daynumberbl2, study_cigs, nonstudy_cigs,
           study_ecig, nonstudy_ecig) %>%
    tidyr::pivot_longer(
      -c(daynumber, daynumberbl2),
      names_to = c("type", ".value"),
      names_sep = "_"
    )

  return(df)
}

# define "library" of REDCap fields ------------

sl_values <- c("sl_condition", "sl_ecig_condition", "sl_juul_vuse", 
               "screen_project", "sl_status","sl_baseline1_med", 
               "sl_baseline1_dose")
session_dates <- c("screen_date", "rescreen_date", "is1_date", 
                   "is2_date", "iodd_date_session", "ievn_date","iab_date")
co_values <- c("screen_id", "sl_condition", "sl_ecig_condition", "sl_juul_vuse", 
               "is1_co", "is2_co", "iodd_co", "ievn_co")
time_values <- c("is1_lastcig_time", "is1_co_time", "is2_lastcig_time", 
                 "is2_co_time", "iodd_lastcig_time","iodd_co1_time", 
                 "ievn_lastcig_time", "ievn_co_time")

  sl_p4 <- c("screen_project", "sl_baseline1_dose", "sl_status")
# prescreen metrics
source_vars <- "recruit_1__0"
for (i in 1:12){
  lbl <- paste("recruit_1__", i, sep="")
  source_vars <- c(source_vars, lbl)
}

# session names
odd_weeks <- c("week_1_arm_1", "week_3_arm_1", "week_5_arm_1", "week_7_arm_1",
               "week_9_arm_1", "week_11_arm_1", "week_13_arm_1", "week_15_arm_1")
even_weeks <- c("week_2_arm_1", "week_4_arm_1", "week_6_arm_1", "week_8_arm_1",
                "week_10_arm_1", "week_12_arm_1", "week_14_arm_1", "week_16_arm_1")


# app functions --------------------
get_current_enrollment <- function(df) {
  # take dataframe with values from Study 3 
  # session log, return tibble with
  # site-project summary information
  df <- df %>%
    rename("project" = screen_project,
           "site" = redcap_data_access_group,
           "status" = sl_status)
  
  # if they're not explictly something else, they're pending
  # screening approval......let's hope.....
  df$status[df$status == ""] <- NA
  df$status <- forcats::fct_explicit_na(df$status,
                                        na_level = "Pending Approval") 
  
  
  df$randomized <- ifelse(!is.na(df$is2_date), 1, 0)
  # enrollment <- df %>%
  #   select(project, site, status) %>%
  #   group_by(project, site) %>%
  #   tidyr::drop_na() %>%
  #   count(status) %>%
  #   tidyr::pivot_wider(id_cols = c("project", "site"),
  #                    names_from = status,
  #                   values_from = n)
  # 
  
  enrollment <- df %>%
    select(project, site, status) %>%
    group_by(project, site) %>%
    count(status)
  
  enrollment$site <- tolower(enrollment$site)
  enrollment$project[enrollment$project == ""] <- NA
  enrollment <- enrollment %>% filter(!is.na(project))
  
  enrollment <- dcast(enrollment, site + project ~ status, fun.aggregate = length)
  # ### FIXES ERROR BY ADDING BLANK COLUMNS
  enrollment$withdrawn__preproduct <- 0
  enrollment$withdrawn__postproduct <- 0
  enrollment$complete <- 0
  ###



  randomized <- df %>%
    select(project, site, randomized) %>%
    group_by(project, site) %>%
    summarise(randomized = sum(randomized, na.rm = TRUE))

  enrollment <- merge(enrollment, randomized, all.x = TRUE)
  names(enrollment) <- tolower(names(enrollment))
  names(enrollment) <- str_replace_all(names(enrollment), "-", "")
  names(enrollment) <- str_replace_all(names(enrollment), " ", "_")
  
  enrollment$randomized[is.na(enrollment$randomized)] <- 0
  # calculate total screenings
  enrollment <- enrollment %>%
    mutate(total_screenings = screening_ineligible +
             in_progress + withdrawn__preproduct +
             withdrawn__postproduct + complete +
             pending_approval) %>%
    select(project, site, total_screenings, pending_approval,
           screening_ineligible, in_progress,  withdrawn__preproduct,
           withdrawn__postproduct, randomized, complete)

  return(enrollment)
}

append_enrollment <- function(current_enrollment_df) {
  # add today's date to current enrollment, append
  # enrollment history
  current_enrollment_df$date <- Sys.Date()
  write.table(current_enrollment_df,
              file = "enrollment_history.tsv",
              append = TRUE,
              row.names = FALSE, col.names = FALSE,
              sep = "\t")
}

load_enrollment_history <- function() {
  # load report from enrollment_history.tsv
  # return full df
  df <- read.table("enrollment_history.tsv", 
                   sep = "\t",
                   stringsAsFactors = FALSE,
                   header = TRUE)
  df$date <- ymd(df$date)
  return(df)
}

in_progress_list <- function(rc_df){
  df <- rc_df %>%
    select(screen_id, sl_status) %>%
    rename("subjectid" = screen_id) %>%
    filter(sl_status == "In Progress")
  
  write.csv(df, "in_progress_list.csv", row.names = FALSE)
  return(df)
}

summarize_enrollment <- function(enrollment_history) {
  # summarizes current enrollment, returns
  # summzrized df
  df <-  enrollment_history %>% 
    distinct() %>%
    filter(date == max(date)) %>%
    select(-date)
  
  colnames(df) <- c("project", "site",
                    "total\nscreenings",
                    "ineligible\nscreenings",
                    "pending\napproval",
                    "in\nprogress",
                    "withdrawn\npre-product",
                    "withdrawn\npost-product",
                    "randomized", "complete")
  return(df)
}

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


load_logfile <- function() {
  df <- read.table("logfile.tsv", header = TRUE, sep = "\t",
                   stringsAsFactors = FALSE)
  return(df)
}

randomization_needed <- function(current_enrollment, start_date, max_date, 
                                 goal_p1_p3, goal_p2){
  
  days_per_month <- 30
  w <- difftime(ymd(max_date), start_date, units = "weeks")
  d <- difftime(ymd(max_date), start_date, units = "days")
  #m <- length(seq(from=ymd(Sys.Date()), to=ymd(max_date), by='month')) - 1 
  m <- d/days_per_month
  df <- current_enrollment %>%
    select(project, randomized) %>%
    mutate(still_needed = ifelse(project == "Project 1" | project == "Project 3",
                               as.integer(goal_p1_p3 - randomized), 
                               as.integer(goal_p2 - randomized))) %>%
    mutate(need_per_week = still_needed / as.numeric(w)) %>%
    mutate(need_per_month = still_needed / as.numeric(m)) %>%
    distinct() %>%
    select(project, still_needed, need_per_week, need_per_month)
  
  return(df)
  
}



# plotting functions -------------

plot_enrollment <- function(enrollment_history) {
  # might end up with a minor logic error because there could be 
  # more than one value per project per day
  
  # pivot longer for plotting
  df <- enrollment_history %>%
    tidyr::pivot_longer(cols = -c(project, site, date),
                        names_to = "variable",
                        values_to = "value")
  p <- ggplot(df, aes(x = date, y = value)) +
    geom_point(aes(color = project)) + 
    geom_line(aes(color = project)) +
    facet_wrap(~variable, scales = "free")
  
  return(p)
}

plot_randomization_goal <- function(enrollment_history, min_date, max_date,
                                    min_enroll, max_enroll) {
  
  # filter by project
  # df <- enrollment_history %>%
  #   filter(project == filter_project)
  
  max_date <- ymd(max_date)
  min_date <- ymd(min_date)
  enroll_difference <- max_enroll - min_enroll
  
  # create sequence of dates and enrollment per day based on 
  # goal and date inputs
  x_seq <- seq(min_date, max_date, by = "day")
  y_seq <- seq(min_enroll, max_enroll, length.out = length(x_seq))
  enrollment_goal <- data.frame(date = x_seq, target = y_seq)
  
  # plot current enrollment against goal
  p <- ggplot(enrollment_goal, aes(date, target)) +
    geom_line(linetype = "dashed") +
    geom_point(data = enrollment_history,
               aes(x = ymd(date), y = randomized,
                   color = project)) +
    geom_line(data = enrollment_history, 
              aes(x = ymd(date), y = randomized,
                  color = project)) +
    geom_hline(aes(yintercept = max_enroll),
               linetype = "dotted") +
    labs(y = "Randomized",
         color = "") +
    theme(legend.position = "top")
  
  return(p)
  
}


plot_cig_adherence <- function(df){
  # scatter plot of avg study and nonstudy cpd
  xmax <- max(df$studycigs, na.rm = TRUE) + 10
  ggplot(df, aes(x = studycigs, y = nonstudycigs)) +
    geom_point(aes(color = project, shape = project), size = 3) +
    geom_label(data = df %>% filter(nonadherent == TRUE),
               aes(studycigs, nonstudycigs, label = subjectid)) +
    xlim(0, xmax) + ylim(0, xmax) +
    geom_abline(slope = 1, linetype = "dashed") +
    labs(x = "Avg. Study CPD",
         y = "Avg. UB CPD") +
    theme(legend.position = "top")
  
}


plot_ivr_timeseries <- function(df_long, ivr, sid) {
  
  ivr_filter <- ivr %>% filter(subjectid == sid)
  bl_cpd <- mean(ivr_filter$cigs, na.rm = TRUE)
  
  df_long <- df_long %>%
    filter(daynumber >= daynumberbl2)
  
  p <- ggplot(df_long, aes(daynumber)) +
    geom_smooth(aes(y = cigs, color = type), method = "loess",
                formula = 'y ~ x', na.rm = TRUE, se = FALSE) +
    geom_hline(aes(yintercept = bl_cpd), 
               linetype = "dashed") +
    geom_point(data = df_long %>% filter(ecig == 1),
               aes(y = ecig - 1, color = type)) +
    geom_point(data = df_long %>% filter(ecig == 0),
               aes(y = ecig, color = type), alpha = 0.1) +
    labs(y = "cpd",
         x = "day (experimental phase)",
         color = "")

  return(ggplotly(p))
}



