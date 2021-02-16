library(RCurl)
library(jsonlite)
library(dplyr)

build_rcon <- function(rc){
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
  #rcon <- redcapConnection(url = url, token = token)
  rcon <- data.frame(url = url, token = token)
  return(rcon)
}

rcon <- build_rcon("rc_proper")


download_rc_dataframe <- function(rcon){
  # Download dataframe -----------------
  result <- postForm(
    uri=rcon$url[1],
    token=rcon$token[1],
    content='record',
    format='json',
    type='flat',
    csvDelimiter='',
    'events[0]' = 'screening_arm_1',
    'fields[0]'='screen_id',
    'fields[1]'='sl_status',
    rawOrLabel='label',
    rawOrLabelHeaders='raw',
    exportCheckboxLabel='false',
    exportSurveyFields='false',
    exportDataAccessGroups='false',
    returnFormat='json'
  )
  
  df <- fromJSON(result)
  
  df <- df %>% filter(!grepl("-2", df$screen_id))
  df <- df %>% filter(grepl("X-", df$screen_id) |
                        grepl("Y-", df$screen_id) |
                        grepl("Z-", df$screen_id))
  return(df)
}

df <- download_rc_dataframe(rcon)
