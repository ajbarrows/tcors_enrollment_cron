library(dplyr)
library(ggplot2)

# functions -------

load_ivr <- function(direc){
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
  df$pi_prop <-ifelse(substr(df$subjectid, 4, 4) == 9, "pilot", "proper")
  
  # remove incomplete calls
  df <- df %>% filter(!is.na(complete))
  
  # write record
  write.table(df, 
              paste(direc, "ivr.tsv", sep=""), 
              sep = "\t",
              row.names = FALSE)
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

# load static IVR file
# df <- load_ivr("../misc/2020-02-02_")

# examine some plots
# pilot_ivr <- read.table("../misc/2020-02-02_ivr.tsv", 
#                         sep = "\t",
#                         header = TRUE) %>%
#   filter(pi_prop == "pilot") %>%
#   mutate(calldate = lubridate::mdy(calldate),
#          project = as.factor(project),
#          site = as.factor(site))
# 
# bl <- pilot_ivr %>%
#   filter(daynumber %in% 2:8) %>%
#   group_by(subjectid) %>%
#   summarize(bl_cpd = mean(cigs, na.rm = TRUE))
# 
# pilot_ivr <- merge(pilot_ivr, bl, by = "subjectid")
# 
# # fill in missing gaps
# pilot_ivr <- pilot_ivr %>% group_by(subjectid) %>%
#   mutate(max_daynum = max(daynumber))
# 
# pilot_ivr <- pilot_ivr %>%
#   tidyr::complete(subjectid, daynumber) %>%
#   filter(daynumber != 0) %>%
#   filter(daynumber <= max_daynum) %>%
#   tidyr::fill(daynumberbl2, .direction = c("downup")) %>%
#   mutate(daynumberEX = daynumber - daynumberbl2)
# 
# write.csv(pilot_ivr, "../misc/pilot_ivr.csv", row.names = FALSE)


pilot_ivr <- read.csv("../misc/pilot_ivr.csv")

pilot_sub <- pilot_ivr %>%
  select(subjectid, daynumber, daynumberbl2, daynumberEX, studycigs, 
         nonstudycigs, studyecig, nstudypods, nonstudyecig, 
         nnonstudypods, project, site, bl_cpd) %>%
  group_by(subjectid) %>%
  filter(daynumber >= daynumberbl2) %>%
  ungroup()

pilot_sub <- impose_week(pilot_sub)


cig_avg <- pilot_sub %>%
  group_by(week) %>%
  filter(!is.na(week), week != "week0") %>%
  summarize(avg_study = mean(studycigs, na.rm = TRUE),
            sd_study = sd(studycigs, na.rm = TRUE),
            avg_nonstudy = mean(nonstudycigs, na.rm = TRUE),
            sd_nonstudy = sd(nonstudycigs, na.rm = TRUE),
            n = n(),
            sem_study = sd_study / n,
            sem_nonstudy = sd_nonstudy / n)


ggplot(cig_avg, aes(x = week)) + 
  geom_errorbar(aes(y = avg_study, 
                      ymin = avg_study - sem_study,
                      ymax = avg_study + sem_study),
                  color = "cornflowerblue") +
  geom_line(aes(y = avg_study, group = 1), color = "cornflowerblue") +
  geom_errorbar(aes(y = avg_nonstudy, 
                      ymin = avg_nonstudy - sem_study,
                      ymax = avg_nonstudy + sem_study),
                  color = "deeppink4") +
  geom_line(aes(y = avg_nonstudy, group = 1), color = "deeppink4") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(y = "Average CPD")
  



