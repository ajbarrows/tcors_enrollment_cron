library(DiagrammeR)

# data
flow <- count_rct_flow(ps_df)

# prescreen counts
ps <- flow[[1]]
unique_ps <- flow[[2]]

# prescreen summary
rct_interview_sum <- flow[[3]]$n
rct_int_sum_names <- c(
  "Ineligble", "Screening Declined", "Screening Scheduled",
  "Will Schedule Screening", "Unreachable" 
)

#4, 3, 1, 2, 5


# screening summary
scrn_status_sum <- flow[[4]]$n
scrn_status_names <- c(
  "Declined Consent", "Signed Consent", "No Show"
)

# 1, 3, 2

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
  
  # edge definitions with the node IDs
  edge [arrowhead = none]
    a -> b
    b -> {c, d, e, f, g}
    e -> {i, j, k}
  }
  
  [1]: paste('Prescreens \\n ', ps)
  [2]: paste('Unique Prescreens \\n ', unique_ps)
  [3]: paste(rct_int_sum_names[1:5], '\\n  ', rct_interview_sum[1:5])
  [4]: paste(scrn_status_names[1:3], '\\n', scrn_status_sum[1:3])
")