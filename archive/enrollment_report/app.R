library(shiny)
library(dplyr)
library(redcapAPI)
library(stringr)
library(lubridate)
library(ggplot2)
library(plotly)
library(reshape2)
library(RCurl)
library(jsonlite)

# source fucntions
source("functions.R")


# set plotting parameters
theme_set(theme_classic(base_size = 15))

start <- 0
goal_p1_p3 <- 212
goal_p2 <- 300
min_date <- ymd("2020-01-01")
max_date <- ymd("2023-06-01")

# main:background ----------

logfile <- load_logfile()
logfile$date <- ymd(logfile$date)

# pull redcap enrollment data once per day

if (max(logfile$date) != Sys.Date()){
    rcon <- build_rcon("rc_proper")
    rc_df <- download_rc_dataframe(rcon, c(sl_values, "is2_date"), NULL)
    # rc_df <- download_rc_dataframe(rcon)
    current_enrollment <- suppressWarnings(get_current_enrollment(rc_df))
    ip_list <- in_progress_list(rc_df) # status == "In Progress" current pull
    append_enrollment(current_enrollment)
    enrollment_history <- load_enrollment_history()
    write.table(Sys.Date(), file = "logfile.tsv", append = TRUE,
                sep = "\t", col.names = FALSE, row.names = FALSE) # append logfile
        
} else {
    enrollment_history <- load_enrollment_history()
    current_enrollment <- enrollment_history %>%
        filter(date == max(date))
    ip_list <- read.csv("in_progress_list.csv", stringsAsFactors = FALSE)
}

# load ivr once per day
if (max(logfile$date) != Sys.Date()) {
    ivr <- load_ivr()
} else {
    ivr <- read.table("ivr.tsv", header = TRUE, sep = "\t",
                      stringsAsFactors = FALSE)
}

ivr_sum <- ivr_summary(ivr)
# for app display --------


# Define UI 
ui <- fluidPage(
    # script from StackOverflow to keep plot aspect ratio 1:1
    tags$head(tags$script('$(document).on("shiny:connected", function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            $(window).resize(function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            ')),
    # App title -----
    fluidRow(
        column(6,
               titlePanel("TCORS Study 3")
        ),
        column(4,
               titlePanel("TCORS Study 3", title=div(img(src="vcbh_logo.png", width = 200)))
        ),
    ),
    
    # titlePanel("TCORS Study 3", title=div(img(src="vcbh_logo.png", width = 300))),
    # titlePanel("TCORS Study 3"),
    

    
    mainPanel(
        tabsetPanel(type = "pills",
                    tabPanel("Study 3",
                             fluidRow(
                                 # select project(s)
                                 column(3,
                                        checkboxGroupInput("checkProject",
                                                           h3("Project"),
                                                           choices = list("Project 1" = "Project 1",
                                                                          "Project 2" = "Project 2",
                                                                          "Project 3" = "Project 3"),
                                                           selected = c("Project 1", 
                                                                        "Project 2",
                                                                        "Project 3"))),
                                 # select site(s)
                                 column(3,
                                        checkboxGroupInput("checkSite",
                                                           h3("Site"),
                                                           choices = list("UVM" = "uvm",
                                                                          "Brown" = "brown",
                                                                          "JHU" = "jhu"),
                                                           selected = "uvm"))
                             ),
                             
                             tabsetPanel(type = "tabs",
                                         tabPanel("Enrollment", 
                                                  tableOutput("enroll_tab"),
                                                  hr(),
                                                  br(),
                                                  h3("Progress"),
                                                  fluidRow(
                                                      column(6,
                                                             
                                                             conditionalPanel(
                                                                 condition = "input.checkProject.includes('Project 1' || 'Project 3')",
                                                                 h4("P1/P3"),
                                                                 plotOutput("p1_p3_randomization")
                                                             ),
                                                             
                                                      ),
                                                      
                                                      column(6,
                                                             conditionalPanel(
                                                                 condition = "input.checkProject.includes('Project 2')",
                                                                 h4("P2"),
                                                                 plotOutput("p2_randomization")
                                                             )
                                                      )
                                                      
                                                  ),
                                                  br(),
                                                  fluidRow(
                                                      column(12, offset = 1,
                                                             tableOutput("randomization_need"))
                                                  ),
                                                  sliderInput("start_date",
                                                              label = "Start Date:",
                                                              min = min_date,
                                                              max = max_date,
                                                              value = min_date,
                                                              timeFormat = "%Y-%m-%d")
                                                  
                                                  
                                                  
                                                  # 
                                                  #                                  conditionalPanel(
                                                  #                                      condition = "input.checkProject.includes('Project 1' || 'Project 3')",
                                                  #                                             h4("P1/P3 Progress"),
                                                  #                                             plotOutput("p1_p3_randomization")
                                                  #                                      ),
                                                  #                                  conditionalPanel(
                                                  #                                     condition = "input.checkProject.includes('Project 2')",
                                                  #                                     h4("P2 Progress"),
                                                  #                                     plotOutput("p2_randomization")
                                                  #                                     )
                                                  
                                         ),
                                         
                                         tabPanel("Adherence",
                                                  plotOutput("cig_adherence"),
                                                  hr(),
                                                  #textOutput("interesting_title"),
                                                  #tableOutput("interesting_ivr")
                                         ),
                                         
                                         tabPanel("IVR History",
                                                  br(),
                                                  selectInput("selected_subjectid",
                                                              label = "Select Participant",
                                                              choices = ip_list$subjectid,
                                                              selected = "Z-B001"),
                                                  
                                                  plotlyOutput("ivr_history")
                                         )
                             )
                             
                     ),
                    tabPanel("Study 2")
                    
        )
        
    )

)

# Define server logic 
server <- function(input, output) {
    # enrollment tab --------
    output$enroll_tab <- renderTable({
        summarize_enrollment(enrollment_history) %>%
            filter(project %in% input$checkProject,
                   site %in% input$checkSite)
   },
        hover = TRUE,
        spacing = 's',
        align = 'c',
    )
    
    output$p1_p3_randomization <- renderPlot({
        enrollment_history <- enrollment_history %>%
            filter(project %in% input$checkProject &
                       project != "Project 2")
        
        plot_randomization_goal(enrollment_history, input$start_date, max_date,
                                start, goal_p1_p3)
    })
    
    output$p2_randomization <- renderPlot({
        enrollment_history <- enrollment_history %>%
            filter(project == "Project 2")
        
        plot_randomization_goal(enrollment_history, input$start_date, max_date,
                                start, goal_p2)
    })
    
    output$randomization_need <- renderTable({
        randomization_needed(current_enrollment, input$start_date, max_date,
                             goal_p1_p3, goal_p2) %>%
            filter(project %in% input$checkProject)
    })
    
    # output$enroll_plot <- renderPlot({
    #     plot_enrollment(
    #         enrollment_history %>%
    #             filter(project %in% input$checkProject,
    #                    site %in% input$checkSite)
    #     )
    #     }
    # )
    # 
    
    
    # adherence tab -------------
    output$cig_adherence <- renderPlot({
        # filter reactive project, site, in progress
        df <- ivr_sum %>%
            filter(project %in% input$checkProject,
                   site %in% input$checkSite,
                   subjectid %in% ip_list$subjectid)
        plot_cig_adherence(df)
        
    },
    height=reactive(ifelse(!is.null(input$innerWidth),input$innerWidth*3/5,0)) # keep aspect ratio the same
    )
    
    # output$interesting_title <- renderText({
    #     "Study 3 IVR Statistics"      
    # })
    # 
    # output$interesting_ivr <- renderTable({
    #     df <- ivr %>%
    #         filter(project %in% input$checkProject,
    #                site %in% input$checkSite)
    #     ivr_stats(df)
    # })
    
    # ivr history tab ---------
    
    output$ivr_history <- renderPlotly({
        sid <- input$selected_subjectid
        df_long <- ivr_timeseries(ivr, sid)
        plot_ivr_timeseries(df_long, ivr, sid)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
