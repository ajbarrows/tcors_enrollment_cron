library(DiagrammeR)

# data
flow <- count_rct_flow(ps_df, full_list, sites = "uvm")

# prescreen counts
ps <- flow[[1]]
unique_ps <- flow[[2]]
unique_ps_pct <- round((unique_ps/ps) * 100)

# prescreen summary
rct_interview_sum <- flow[[3]]$n
rct_interview_pct <- round((rct_interview_sum/unique_ps) * 100)
rct_int_sum_names <- c(
  "Ineligble", "Screening Declined", "Screening Scheduled",
  "Attempting to Schedule Screening", "Unreachable" 
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

# 1, 3, 2

# enrolled summary
enroll_status <- flow[[5]]
enroll_status$pct <- round((enroll_status$n/n_signed_consent) * 100)
# enroll_status$pct <- round((enroll_status$n/unique_ps) * 100)


grViz("
digraph a_nice_graph {

  # node definitions with substituted label text
  node [fontname = Helvetica]
  a [label = '@@1']
  b [label = '@@2-1']
  
  node [color = red, shape = rectangle]
  c [label = '@@3-1']
  d [label = '@@3-2']
  
  node [color = green, shape = oval]
  e [label = '@@3-3']
  
  node [color = orange]
  f [label = '@@3-4']
  
  node [color = red, shape = rectangle]
  g [label = '@@3-5']
  i [label = '@@4-1']
  
  node [color = green, shape = oval]
  j [label = '@@4-2']
  
  node [color = red, shape = rectangle]
  k [label = '@@4-3']
  l [label = '@@5-1']
  
  node [color = green, shape = oval]
  m [label = '@@5-2']
  n [label = '@@5-3']
  o [label = '@@5-4']
  p [label = '@@5-5']
  
  node [color = red, shape = rectangle]
  q [label = '@@5-6']
  r [label = '@@5-7']
  
  # edge definitions with the node IDs
  edge [arrowhead = none]
    a -> b
    b -> {c, d, e, f, g}
    e -> {i, j, k}
    j -> {l, m, n, o, p, q, r}
  }
  
  [1]: paste('Prescreens \\n ', ps)
  [2]: paste('Unique Prescreens \\n ', unique_ps, ' (', unique_ps_pct, '%)', sep = '')
  [3]: paste(rct_int_sum_names[1:5], '\\n  ', rct_interview_sum[1:5], ' (', rct_interview_pct[1:5], '%)', sep = '')
  [4]: paste(scrn_status_names[1:3], '\\n', scrn_status_sum[1:3], ' (', scrn_status_pct[1:3], '%)', sep = '')
  [5]: paste(enroll_status$sl_status[1:7], '\\n', enroll_status$n[1:7], ' (', enroll_status$pct[1:7], '%)', sep = '')
")