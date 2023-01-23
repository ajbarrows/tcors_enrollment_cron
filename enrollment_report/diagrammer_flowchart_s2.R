library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)
library(magrittr)
library(dplyr)
library(lubridate)


pi_prop <- function(df) {
  df %>%
    mutate(
      pi_prop = ifelse(
        substr(subjectid, 4, 4) == 9, "pilot","proper"
      )
    )
}

clean_prescreen_df <- function(ps_df, min_date) {
  filter_str <- "-|_|copy|incomplete|empty"
  ps_df %>% 
    filter(!stringr::str_detect(redcap_id, filter_str)) %>%
    rowwise() %>%
    filter(
      !is.na(sum(elig_project_1, elig_project_2, elig_project_3))
    ) %>%
    ungroup() %>%
    filter(
      recruit_date > min_date & recruit_date <= Sys.Date()
    )
}

pjt_ste <- function(df) {
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
  
  df
}

count_rct_flow <- function(ps_df, full_list, sites, min_date) {
  # filter for valid prescreens
  # RECOMMEND HARD-CODING SITES
  
  ps_df <- ps_df %>% 
    filter(site == "uvm")
  ps <- ps_df
  
  # filter unique
  
  ps_unique <- clean_prescreen_df(ps_df, min_date) %>%
    arrange(recruit_date) %>%
    mutate(sequence = dense_rank(recruit_date))
  # extract prescreen summary information
  rct_summary <- ps_unique %>%
    count(recruit_int_summ)
  rct_summary$recruit_int_summ <- factor(
    rct_summary$recruit_int_summ, levels = c(4, 3, 1, 2, 5, 6)
  )
  rct_summary <- rct_summary[order(rct_summary$recruit_int_summ),]
  
  # extract screening summary
  scrn_summary <- ps_unique %>%
    count(screen_status)
  scrn_summary$screen_status <- factor(
    scrn_summary$screen_status, levels = c(1, 3, 2)
  )
  scrn_summary <- scrn_summary[order(scrn_summary$screen_status),]
  
  # extract post-screening status
  full_list <- pjt_ste(full_list)
  full_list <- pi_prop(full_list)
  enroll_status <- full_list %>%
    filter(site %in% sites) %>%
    filter(pi_prop == "proper") %>%
    count(sl_status)
  
  if (!"Screening 1 Complete" %in% enroll_status$sl_status) {
    df <- data.frame(sl_status = "Screening 1 Complete", n = 0)
    enroll_status <- rbind(enroll_status, df)
  } 
  if (!"Under LMP Review" %in% enroll_status$sl_status) {
    df <- data.frame(sl_status = "Under LMP Review", n = 0)
    enroll_status <- rbind(enroll_status, df)
  }
  enroll_status$sl_status <- factor(
    enroll_status$sl_status,
    levels = c(
      "Screening Ineligible", "Under LMP Review", "Screening 1 Complete","Complete",
      "In Progress", "Withdrawn - Pre-Product", "Withdrawn - Post-Product"
    )
  )
  
  enroll_status <- enroll_status[order(enroll_status$sl_status),]
  
  
  list(nrow(ps), nrow(ps_unique), rct_summary, scrn_summary, enroll_status)
}

# end functions -----------


s2_ps_df <- read.csv("s2_ps_df.csv", stringsAsFactors = FALSE)
s2_ps_df$recruit_date <- as.Date(s2_ps_df$recruit_date)
# 
# s2_ps_df <- s2 %>%
#   select(
#     REDCapID, PrescreenDate, PSEligible, PreScreen.Status, 
#     Screening.Complete.,ParticipationStatus, Lost.Contact., 
#     Baseline.2.Complete.Date.) %>%
#   mutate(
#     recruit_date = lubridate::mdy(PrescreenDate),
#     duplicate = ifelse(PreScreen.Status == "Repeated Contact", "yes", "no"),
#     ps_status = ifelse(PSEligible == 0, "Ineligible", NA),
#     ps_status = ifelse(PreScreen.Status %in% c("Lost Contact", "Unknown", "Other"), "Unreachable", ps_status),
#     ps_status = ifelse(Screening.Complete. == "Complete", "Screening Scheduled", ps_status),
#     ps_status = ifelse(duplicate != "yes" & is.na(ps_status), "Screening Scheduled", ps_status),
#     scrn_status = ifelse(Screening.Complete. == "Complete", "Signed Consent", NA),
#     scrn_status = ifelse(Screening.Complete. == "Participation Declined", "Declined Consent", scrn_status),
#     scrn_status = ifelse(Screening.Complete. %in% c("Lost Contact", ""), "No Show", scrn_status)
#   ) %>%
#   select(recruit_date, duplicate, ps_status, scrn_status)

# filter as of current trial progress. let's argue that was 2020-01-01



s2_out <- function(s2_ps_df, seq_day) {
  s2_ps_df <- s2_ps_df %>%
    filter(
      recruit_date >= as.Date("2016-10-01")  & 
        recruit_date <= as.Date("2019-08-01")
    ) %>%
    arrange(recruit_date) %>%
    mutate(sequence = dense_rank(recruit_date)) %>%
    filter(sequence <= seq_day)
  
  s2_ps_unique <- s2_ps_df %>% filter(duplicate == "no")

  
  ps_status <- s2_ps_unique %>% count(ps_status)
  
  scrn_status <- s2_ps_unique %>% 
    filter(ps_status == "Screening Scheduled") %>%
    count(scrn_status)
  
  rand <- s2_ps_unique %>%
    mutate(randomized = ifelse(!is.na(randomized_date), "randomized", "not_randomized")) %>%
    count(randomized)
  
  list(nrow(s2_ps_df), nrow(s2_ps_unique), ps_status, scrn_status, rand)
}

seq_day <- 283

s2 <- s2_out(s2_ps_df, seq_day)

s2_ps_count <- s2[[1]]
s2_ps_unique_count <- s2[[2]]
s2_unique_ps_pct <- round((s2_ps_unique_count/s2_ps_count) * 100)

s2_rct_interview_sum <- s2[[3]] %>%
  mutate(pct = round(n/sum(n) * 100))

s2_scrn_status <- s2[[4]] %>%
  tidyr::drop_na() %>%
  mutate(pct = round(n/sum(n) * 100))

s2_rand <- s2[[5]] %>%
  mutate(pct = round(n/sum(n) * 100))


# Study 3 -----------


# source("functions.R")
min_date <- ymd("2020-08-01")
# data
full_subject_list <- read.csv("s3/full_list.csv")
ps_df <- read.csv("s3/df_ps.csv")
flow <- count_rct_flow(ps_df, full_subject_list, sites = "uvm", min_date)


# prescreen counts
ps <- flow[[1]]
unique_ps <- flow[[2]]
unique_ps_pct <- round((unique_ps/ps) * 100)

# prescreen summary
rct_interview_sum <- flow[[3]]$n
rct_interview_pct <- round((rct_interview_sum/unique_ps) * 100)
rct_int_sum_names <- c(
  "Ineligble", "Screening Declined", "Screening Scheduled",
  "Attempting to Schedule Screening", "Unreachable", "Not Yet Coded" 
)
n_scrn_scheduled <- rct_interview_sum[3]
#4, 3, 1, 2, 5


# screening summary
scrn_status_sum <- flow[[4]]$n
scrn_status_pct <- round((scrn_status_sum/n_scrn_scheduled) * 100)
# scrn_status_pct <- round((scrn_status_sum/unique_ps) * 100)
scrn_status_names <- c(
  "Declined Consent", "Signed Consent", "No Show"
)
n_signed_consent <- scrn_status_sum[2]

# enroll_status <- flow[[5]] %>%
  

# 
# enroll_status$pct <- round((enroll_status$n/n_signed_consent) * 100)

randomized <- full_list %>%
  filter(site == "uvm", pi_prop == "proper") %>%
  count(is2_date) %>%
  mutate(pct = round(n/sum(n) * 100))
randomized[is.na(randomized)] <- "not_randomized"

  




# combine --------
ps_count <- list(s2_ps_count, ps)
unique_ps_count <- list(s2_ps_unique_count, unique_ps)
unique_pct <- list(s2_unique_ps_pct, unique_ps_pct)


uvm_flowchart_graph_compare <- grViz("
digraph uvm_flowchart {

  # node definitions with substituted label text
  node [fontname = Helvetica, shape = none, fontcolor = red]
 
  a [label = '@@1-1', fontsize = 50]  # study
  b [label = '@@1-2', fontsize = 50]

  node [fontname = Helvetica, shape = oval, fontcolor = black, fontsize = 20]
  c [label = '@@2-1']  # prescreens
  d [label = '@@2-2']
  
  e [label = '@@3-1']  # unique prescreens
  f [label = '@@3-2'] 
  

  
  # node [shape = rectangle, color = red]
  g [label = '@@4-1', color = red] # ps ineligible s3
  h [label = '@@4-2', color = red] # screening declined s3
  i [label = '@@4-3', color = green] # screening scheduled s3
  j [label = '@@4-4', color = orange] # attempting to schedule s3
  k [label = '@@4-5', color = red] # unreachable s3
  l [label = '@@4-6', color = orange] # not yet coded s3 

  
  m [label = '@@4-7', color = red] # ps ineligible s2
  n [label = '@@4-8', color = green] # screening scheduled s2
  o [label = '@@4-9', color = red] # unreachable s2
  

  
  p [label = '@@5-1', color = red] # declined consent s3
  q [label = '@@5-2', color = green] # signed consent s3
  r [label = '@@5-3', color = red] # no show s3
  
  t [label = '@@5-4', color = red] # declined consent s2
  u [label = '@@5-5', color = red] # no show s2
  v [label = '@@5-6', color = green] # signed consent s2
  
  w [label = '@@6-1', color = red] # not randomized s2
  x [label = '@@6-2', color = green] # randomized s2
  
  y [label = '@@6-3', color = green] # randomized s3
  z [label = '@@6-4', color = red] # randomized s3
  
  # zz [label = '@@7, shape = rectangle]
  

  
  edge [arrowhead = none]
    a -> c -> e
    b -> d -> f
    f -> {g, h, i, j, k, l}
    e -> {m, n, o}
    i -> {p, q, r}
    n -> {t, u, v}
    v -> {w, x}
    q -> {y, z}
    
    
}
  
  [1]: c('Study 2', 'Study 3')
  [2]: paste('Prescreens \\n ', ps_count)
  [3]: paste('Unique Prescreens \\n ', unique_ps_count, ' (', unique_pct, '%)', sep = '')
  [4]: c(paste(rct_int_sum_names[1:6], '\\n  ', rct_interview_sum[1:6], ' (', rct_interview_pct[1:6], '%)', sep = ''), paste(s2_rct_interview_sum$ps_status, '\\n', s2_rct_interview_sum$n, '(', s2_rct_interview_sum$pct, '%)'), sep='')
  [5]: c(paste(scrn_status_names[1:3], '\\n', scrn_status_sum[1:3], ' (', scrn_status_pct[1:3], '%)', sep = ''), paste(s2_scrn_status$scrn_status, '\\n', s2_scrn_status$n, ' (', s2_scrn_status$pct, '%)', sep=''))
  [6]: c(paste(s2_rand$randomized, '\\n', s2_rand$n, ' (', s2_rand$pct, '%)', sep = ''), paste(randomized$is2_date, '\\n', randomized$n, ' (', randomized$pct, '%)', sep = ''))
  # [7]: paste('These numbers (estimates) assume Study 2 began', '\\n', 'recruiting 10/7/2016. They ignore the first part of Study 3,', '\\n', 'setting its start date to 8/1/2020. Therefore, the numbers', '\\n', 'represent 283 trial-days.')
")
uvm_flowchart_graph_compare %>%
  export_svg %>% charToRaw %>% rsvg_pdf("s2_s3_flowchart_randomized.pdf")

# save(uvm_flowchart_graph, file = "s3/flowchart.RData")




# 1, 3, 2

# # enrolled summary
# enroll_status <- flow[[5]]
# enroll_status$pct <- round((enroll_status$n/n_signed_consent) * 100)
# enroll_status$pct <- round((enroll_status$n/unique_ps) * 100)


# uvm_flowchart_graph <- grViz("
# digraph uvm_flowchart {
# 
#   # node definitions with substituted label text
#   node [fontname = Helvetica]
#   a [label = '@@1']
#   b [label = '@@2-1']
#   
#   node [color = red, shape = rectangle]
#   c [label = '@@3-1']
#   d [label = '@@3-2']
#   
#   node [color = green, shape = oval]
#   e [label = '@@3-3']
#   
#   node [color = orange]
#   f [label = '@@3-4']
#   
#   node [color = red, shape = rectangle]
#   g [label = '@@3-5']
#   i [label = '@@4-1']
#   
#   node [color = green, shape = oval]
#   j [label = '@@4-2']
#   
#   node [color = red, shape = rectangle]
#   k [label = '@@4-3']
#   # l [label = '@@5-1']
#   # 
#   # node [color = green, shape = oval]
#   # m [label = '@@5-2']
#   # n [label = '@@5-3']
#   # o [label = '@@5-4']
#   # p [label = '@@5-5']
#   # 
#   # node [color = red, shape = rectangle]
#   # q [label = '@@5-6']
#   # r [label = '@@5-7']
#   
#   # edge definitions with the node IDs
#   edge [arrowhead = none]
#     a -> b
#     b -> {c, d, e, f, g}
#     e -> {i, j, k}
#     # j -> {l, m, n, o, p, q, r}
#   }
#   
#   [1]: paste('Prescreens \\n ', ps)
#   [2]: paste('Unique Prescreens \\n ', unique_ps, ' (', unique_ps_pct, '%)', sep = '')
#   [3]: paste(rct_int_sum_names[1:5], '\\n  ', rct_interview_sum[1:5], ' (', rct_interview_pct[1:5], '%)', sep = '')
#   [4]: paste(scrn_status_names[1:3], '\\n', scrn_status_sum[1:3], ' (', scrn_status_pct[1:3], '%)', sep = '')
#   # [5]: paste(enroll_status$sl_status[1:7], '\\n', enroll_status$n[1:7], ' (', enroll_status$pct[1:7], '%)', sep = '')
# ")
# uvm_flowchart_graph
# # save(uvm_flowchart_graph, file = "s3/flowchart.RData")

  
  