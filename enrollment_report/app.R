library(shiny)
library(shinyWidgets)
library(dplyr)
library(redcapAPI)
library(stringr)
library(lubridate)
library(DT)
library(ggplot2)
library(plotly)
# library(DiagrammeR)
library(ggalluvial)


# source fucntions
source("functions.R")

# set plotting parameters
theme_set(theme_classic(base_size = 15))
# theme_set(theme_classic())

start <- 0
goal_p1_p3 <- 212
goal_p2 <- 255
min_date <- ymd("2020-01-01")
max_date <- ymd("2023-06-01")
start_date <- ymd("2020-11-05")
inc_comp_date <- ymd("2021-05-17")

# main:background ----------

logfile <- load_logfile()
logfile$date <- ymd_hms(logfile$date)

# Study 3 -----------------
# load from static: see api_call_script.R
enrollment_history <- load_enrollment_history("s3/")
current_enrollment <- enrollment_history %>%
  filter(date == max(date)) %>%
  select(-date)
ip_list <- read.csv("s3/in_progress_list.csv", stringsAsFactors = FALSE)
full_subject_list <- read.csv("s3/full_list.csv", stringsAsFactors = FALSE)
ivr <- read.table("s3/ivr.tsv", header = TRUE, sep = "\t", stringsAsFactors = FALSE) %>%
  mutate(studycigs = as.numeric(studycigs))
# rc_df <- read.csv("s3/rc_df.csv", stringsAsFactors = FALSE)
load("s3/rc_df.RData")
session_dates <- get_session_dates(rc_df) %>%
  group_by(subjectid) %>%
  filter(date == max(date, na.rm = TRUE))
co_df <- get_session_dates(rc_df) %>%
  select(subjectid, date, session, pi_prop, project, site, co)
s2_bl2_dates <- read.csv("study2_dates.csv") %>% filter(session == "baseline_2")
ps_df <- read.csv("s3/df_ps.csv", stringsAsFactors = FALSE)

# apply summary functions
ivr_sum <- ivr_summary(ivr)
df_ps_inel <- read.csv("s3/df_ps_inel.csv", stringsAsFactors = FALSE)
df_ps_gen <- read.csv("s3/df_ps_gen.csv", stringsAsFactors = FALSE)
df_ps_spec <- read.csv("s3/df_ps_spec.csv", stringsAsFactors = FALSE)
df_ps_inel_one <- read.csv("s3/df_ps_inel_one.csv", stringsAsFactors = FALSE)
inel_df <- read.csv("s3/screening_inel.csv", stringsAsFactors = FALSE) # PILOT
# ps_history <- load_prescreen_history("s3/")
# ps_history <- read.csv("s3/prescreen_history.csv", stringsAsFactors = FALSE)
ps_history <- load_prescreen_history("s3/")
ps_df_sum <- read.csv("s3/prescreen_summary.csv", stringsAsFactors = FALSE)
# ps_df_sum <- ps_history %>%
#   filter(recruit_date == max(recruit_date)) %>%
#   select(-recruit_date)

rct_source <- assemble_rct_source(ps_df, c(source_vars, uvm_only), min_date)

# prescreen flowchart
load("s3/flowchart.RData")

# make an estimate of UVM screenings scheduled
scrns_sched <- read.csv("s3/scrn_sched.csv", stringsAsFactors = FALSE)

# ps_df_sum <- summarize_prescreen(ps_df)

# S3 Pilot Examination
# pilot_ivr <- read.csv("./misc/pilot_ivr.csv")

# S2 P4 -----------------
enrollment_history_p4 <- load_enrollment_history("p4/")
current_enrollment_p4 <- enrollment_history_p4 %>%
  filter(date == max(date))
ip_list_p4 <- read.csv("p4/in_progress_list.csv", stringsAsFactors = FALSE)
# ivr_p4 <- read.table("p4/ivr.tsv", header = TRUE, sep = "\t",
#                   stringsAsFactors = FALSE)


last_updated <- max(enrollment_history$date)
# for app display --------

# Define UI
ui <- fluidPage(
  
  # Despite my best efforts, some fixate on the least useful metrics as gospel.
  tags$style("#est_scrn_sched {font-size:15px;
             font-weight:bold;
             width: 100%;
             display:block;}"),
  
  # keep plot aspect ratio 1:1
  tags$head(tags$script('$(document).on("shiny:connected", function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            $(window).resize(function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            ')),
  # App title -----
  fluidRow(
    column(
      6,
      titlePanel("TCORS Enrollment Report"),
      textOutput("last_updated")
    ),
    column(
      4,
      titlePanel("TCORS Study 3", title = div(img(src = "vcbh_logo.png", width = 200)))
    ),
  ),


  mainPanel(tabsetPanel(
    tabPanel(
      "Study 3",
      fluidRow(
        # select project(s)
        column(
          3,
          checkboxGroupInput("checkProject",
            h3("Project"),
            choices = list(
              "Project 1" = "Project 1",
              "Project 2" = "Project 2",
              "Project 3" = "Project 3"
            ),
            selected = c(
              "Project 1",
              "Project 2",
              "Project 3"
            )
          )
        ),
        # select site(s)
        column(
          3,
          checkboxGroupInput("checkSite",
            h3("Site"),
            choices = list(
              "UVM" = "uvm",
              "Brown" = "brown",
              "JHU" = "jhu"
            ),
            selected = c("uvm", "brown", "jhu")
          )
        ),
        column(
          3,
          checkboxGroupInput("checkStudy",
            h3("Study"),
            choices = list(
              "Pilot" = "pilot",
              "Proper" = "proper"
            ),
            selected = "proper"
          )
          )
        )
      ),

      tabsetPanel(
        type = "tabs",
        selected = "Enrollment",
        tabPanel(
          "Recruitment",
          h3("Prescreen"),
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Cumulative",
              tableOutput("prescreen_tab"),
              plotlyOutput("ps_plot"),
              # plotOutput("ps_plot"),
              checkboxInput(
                "checkCompDate", "Show Comp. Change Date (5/17/2021)", value = TRUE
              ),
              sliderInput(
                "ps_range",
                label = "Date Range:",
                min = min_date,
                max = Sys.Date(),
                value = c(min_date, Sys.Date()),
                timeFormat = "%Y-%m-%d"
              ),
            ),
            tabPanel(
              "Recruitment Source",
              br(),
              materialSwitch(
                inputId = "filter_screened",
                label = "Show only in-person screenings",
                value = FALSE),
              br(),
              plotlyOutput("rct_source_ts", height = "600px"),
              br(),
              plotOutput(outputId = "rct_alluvial", height = "800px", width = "1000px"),
              # plotOutput("rct_source_plot"),
              checkboxInput(
                inputId = "show_rct_table",
                label = "Show Recruitment Source Table",
                value = FALSE
              ),
              br(),
              tableOutput("rct_source_tab")
            ),
            tabPanel(
              "Ineligibility",
              br(),
              h4("Ineligiblity"),
              h5("General"),
              plotOutput("gen_ps_plot"),
              h5("Only One Reason"),
              plotOutput("ps_one_plot"),
              h5("Specific"),
              plotOutput("gen_ps_spec"),
            ),
            tabPanel(
              "Locations and Cost",
              br(),
              br(),
              tags$a(
                href="https://tcors.w3.uvm.edu/rct-location/", 
                "Recruitment Location App",
                target = "_blank"
                )
            )
          ),
        ),
        tabPanel(
          "Enrollment",
          br(),
          h4("Current Enrollment Snapshot"),
          materialSwitch(
            inputId = "exec_by_site",
            label = "Show by site",
            value = FALSE),
          tableOutput("executive_sum"),
          div(style="text-align:center;
              box-shadow:10px 10px 5px #888888;
              position:relative;",
              textOutput("est_scrn_sched")),
          # textOutput("est_scrn_sched"),
          br(),
          checkboxInput("show_detail", "Show Detail", value = FALSE),
          br(),
          tableOutput("enroll_tab"),
          br(),
          # fluidRow(column(3,
          #                 ),
          #          column(3,
          #                 selectInput(
          #                   "selectPlot",
          #                   "Select Quantity",
          #                   choices = c(
          #                     "Received Product" = "received_product",
          #                     "Total Screenings" = "total_screenings",
          #                     "Ineligible Screenings" = "screening_ineligible"
          #                   ),
          #                   selected = "total_screenings"
          #                 ))),
                          selectInput(
                            "selectTimeframe",
                            "Select Time Frame",
                            choices = c(
                              "1 month",
                              "3 months",
                              "1 year",
                              "All"
                            ),
                            selected = "3 months"
                          ),
          switchInput(
            "switchEnrollRate",
            onLabel = "Avg. Rate Per Day",
            offLabel = "Cumulative"
          ),
          fluidRow(
            column(
              12,
              plotlyOutput("enrollment_ts")
            )
          ),
          
          h3("Progress"),
          h4("Target Completion Date: June 1, 2023"),
          fluidRow(
            column(
              6,
              conditionalPanel(
                condition = "input.checkProject.includes('Project 1' || 'Project 3')",
                h4("P1/P3"),
                plotOutput("p1_p3_randomization")
              ),
            ),
            
            column(
              6,
              conditionalPanel(
                condition = "input.checkProject.includes('Project 2')",
                h4("P2"),
                plotOutput("p2_randomization")
              )
            )
          ),
          br(),
          sliderInput("start_date",
                      label = "Start Date:",
                      min = min_date,
                      max = Sys.Date(),
                      value = start_date,
                      timeFormat = "%Y-%m-%d"
          ),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          fluidRow(
            column(6, offset = 0,
                   h5("Randomization Progress")),
            column(3, offset = 0,
                   h5("Average Rand. Per Week")),
            column(3, offset = 0,
                   h5("Study 2 Reference"))
          ),
          fluidRow(
            column(6,
                   offset = 0,
                   tableOutput("randomization_need")
            ),
            column(3,
                   offset = 0,
                   tableOutput("avg_rand_table")
            ),
            column(3,
                   offset = 0,
                   tableOutput("study2_avg_rand")
            )
          ),
          br()
        ),
        tabPanel(
          "Eligibility",
          br(),
          plotOutput("inel_plot"),
          br()
        ),
        tabPanel(
            "Distribution",
            br(),
            fluidRow(
              column(8,plotOutput("session_distribution")),
              column(4,plotOutput("plot_complete"))
            )
          ),
        tabPanel(
          "Subjects",
          dataTableOutput("subjects")
          
        ),
        tabPanel(
          "Adherence",
          plotOutput("cig_adherence"),
          br(),
          br(),
          br(),
        ),
        tabPanel(
          "Product Use",
          br(),
          br(),
          selectInput("selected_subjectid",
                      label = "Select Participant",
                      choices = ivr %>% distinct(subjectid)
          ),
          fluidRow(
            column(4,
                   checkboxInput("show_ivr_loess", "Trendline - LOESS",
                                 value = TRUE)
            ),
            column(4,
                   checkboxInput("show_threshold", "Show CPD Increase Thresholds",
                                 value = FALSE)
            )
          ),
          plotOutput("ivr_history"),
          hr(),
          checkboxInput("show_co_loess", "Trendline - LOESS",
                        value = TRUE),
          plotOutput("co_values")
        ),
        tabPanel(
          "Miscellaneous",
          br(),
          h4("Attrition"),
          materialSwitch(
            inputId = "att_by_site",
            label = "Show by site",
            value = FALSE),
          tableOutput("attrition"),
          hr(),
          h4("Sex Assigned at Birth"),
          h5("Randomized Participants"),
          materialSwitch(
            inputId = "sex_by_site",
            label = "Show by site",
            value = FALSE
          ),
          tableOutput("sex_at_birth"),
          br(),
          h4("P1 Population Represented in All Projects"),
          h5("Age < 45, Education < Assoc. Degree"),
          tableOutput("p1_like"),
          br(),
        )
      )
    )

    # tabPanel(
    #   "Study 2 Project 4",
    #   fluidRow(
    #     column(
    #       3,
    #       checkboxGroupInput("checkSite_p4",
    #         h3("Site"),
    #         choices = list(
    #           "UVM" = "uvm",
    #           "JHU" = "jhu"
    #         ),
    #         selected = "uvm"
    #       )
    #     ),
    #     column(
    #       5,
    #       checkboxGroupInput("checkProject_p4",
    #         h3("Project"),
    #         choices = list(
    #           "Opioid-Maintained",
    #           "Not Opioid-Maintained"
    #         ),
    #         selected = c(
    #           "Opioid-Maintained",
    #           "Not Opioid-Maintained"
    #         )
    #       )
    #     ),
    #   ),
    #   tabsetPanel(
    #     tabPanel(
    #       "Enrollment",
    #       tableOutput("p4_enroll_tab"),
    #       br(),
    #       plotlyOutput("enrollment_ts_p4",
    #         width = "150%"
    #       )
    #     )
    #   )
    # )
  ))


# Define server logic
server <- function(input, output, session) {
  # last updated _________
  output$last_updated <- renderText({
    paste("Last Updated", as.character(last_updated))
  })
  
  # assemble filtering values
  site_list <- reactive(req(input$checkSite))
  project_list <- reactive(req(input$checkProject))
  pi_prop_list <- reactive(req(input$checkStudy))
  
  # recruitment ----------


  output$prescreen_tab <- renderTable(
    {
      ps_df_sum %>%
        filter(site %in% site_list())
    },
    spacing = "s",
    align = "c"
  )
  output$ps_plot <- renderPlotly({
    df <- ps_history %>%
      filter(site %in% input$checkSite)
    plot_prescreen(df, inc_comp_date, input$checkCompDate, input$ps_range)
  })
  
  output$rct_source_ts <- renderPlotly({
    df <- rct_source %>%
      filter(site %in% site_list())
    if (input$filter_screened) {
      df <- df %>%
        filter(screen_status == TRUE)
    }
    plot_rct_source_ts(df, inc_comp_date, input$checkCompDate, input$ps_range)
  })
  
  output$rct_source_plot <- renderPlot({
    df <- rct_source %>%
      filter(site %in% site_list())
    if (input$filter_screened) {
      df <- df %>%
        filter(screen_status == TRUE)
    }
    plot_rct_source(df)
  })
  
  output$rct_source_tab <- renderTable({
    if (input$show_rct_table) {
      rct_source %>%
        filter(site %in% site_list()) %>%
        rct_source_table()
    }
  })
  
  output$rct_alluvial <- renderPlot({
    ps_df <- ps_df %>%
      filter(site %in% site_list())
    
    rct_source_alluvial(rc_df, ps_df, input$filter_screened)
  })

  output$gen_ps_tab <- renderTable(
    {
      df_ps_gen %>%
        filter(site %in% site_list())
    },
    spacing = "s",
    align = "c"
  )

  output$gen_ps_plot <- renderPlot({
    df <- df_ps_gen %>%
      filter(site %in% site_list())
    plot_ps_ineligibility(df)
  })

  output$ps_one_plot <- renderPlot({
    df <- df_ps_inel_one %>%
      filter(site %in% site_list())
    plot_ps_ineligibility(df)
  })

  output$specific_ps_tab <- renderTable(
    {
      df_ps_spec %>%
        filter(site %in% site_list())
    },
    spacing = "s",
    align = "c"
  )

  output$gen_ps_spec <- renderPlot({
    df <- df_ps_spec %>%
      filter(site %in% site_list())
    plot_ps_ineligibility(df)
  })
  
  # enrollment tab --------
  
  output$executive_sum <- renderTable(
    {
      c_enrl <- current_enrollment %>%
        filter(
          project %in% project_list(),
          site %in% site_list(),
          pi_prop %in% pi_prop_list()
        )
        
      t <- exec_summary(ip_list, c_enrl, input$exec_by_site) %>%
        select(-pi_prop)
      # t <- exec_summary(ip_list, c_enrl, TRUE)
      #TODO: make this a function
      # sums
      t_sum <- t %>%
        summarise(across(where(is.numeric), sum)) %>%
        mutate(across(.cols = everything(), as.character))
      
      t_sum$project <- "sum"
      # t_sum$pi_prop <- ""
      
      if (input$exec_by_site) {
        t_sum$site <- ""
      }
    
      # make bold
      t_sum[] <- lapply(t_sum, function(x) paste("<strong>", x, "</strong>", sep = ""))
      
      # blank row
      t <- t %>% mutate(across(.fns = as.character))
      t[nrow(t) + 1, ] <- ""
      
      rbind(t, t_sum) 
    },
    sanitize.text.function = function(x) {
      x
    },
    hover = TRUE,
    spacing = "s",
    align = "c",
  )
  
  output$attrition <- renderTable(
    {
      t <- current_enrollment %>%
        filter(
          project %in% project_list(),
          site %in% site_list(),
          pi_prop %in% pi_prop_list()
        ) %>%
        attrition(input$att_by_site)

      t
    },
    sanitize.text.function = function(x) {
      x
    },
    hover = TRUE,
    spacing = "s",
    align = "c"
  )
  
  sex_tab <- reactive({
      build_sex_table(rc_df, full_subject_list, input$sex_by_site, site_list())
  })
  
  output$sex_at_birth <- renderTable({
    t <- sex_tab()[[1]]
    # if (input$sex_by_site) {
    #   t <- t %>% filter(site %in% site_list())
    # }
    t %>% filter(
      project %in% project_list(),
      pi_prop %in% pi_prop_list()
    )
  }
  )
  
  output$p1_like <- renderTable(
    {
      p1_age(sex_tab()[[2]]) %>%
        filter(
          project %in% project_list(),
          pi_prop %in% pi_prop_list()
        )
    }
  )
  
  
  output$enroll_tab <- renderTable(
    if (req(input$show_detail)) {
      t <- current_enrollment %>%
        filter(
          project %in% project_list(),
          site %in% site_list(),
          pi_prop %in% pi_prop_list()
        )
      colnames(t) <- c(
        "project", "site",
        "total\nscreenings",
        "screening_1\ncomplete",
        "pending\napproval",
        "ineligible\nscreenings",
        "in\nprogress",
        "withdrawn\npre-product",
        "withdrawn\npost-product",
        "randomized\n(received product)", "complete", "pi_prop"
      )

      # sums
      t_sum <- t %>%
        summarise(across(-c(project, site, pi_prop), sum))

      t_sum$project <- "sum"
      t_sum$site <- ""
      t_sum$pi_prop <- ""

      # make bold
      t_sum[] <- lapply(t_sum, function(x) paste("<strong>", x, "</strong>", sep = ""))

      # blank row
      t[nrow(t) + 1, ] <- ""

      rbind(t, t_sum)
    },
    sanitize.text.function = function(x) {
      x
    },
    hover = TRUE,
    spacing = "s",
    align = "c",
  )

  output$est_scrn_sched <- renderText({
    if(req(length(site_list()) == 1 & "uvm" %in% site_list())) {
      paste("Number of UVM screenings upcoming (estimated):", nrow(scrns_sched))
    }
  })

  output$enrollment_ts <- renderPlotly({
    df <- enrollment_history %>%
      filter(
        project %in% project_list(),
        site %in% site_list(),
        pi_prop %in% pi_prop_list()
      )

    plot_enrollment(df, input$selectTimeframe, input$switchEnrollRate)
  })

  output$session_distribution <- renderPlot({
    df <- session_dates %>%
      filter(
        project %in% project_list(),
        site %in% site_list(),
        pi_prop %in% pi_prop_list()
      )
    plot_session_distribution(df)
  })
  
  output$plot_complete <- renderPlot({
    df <- session_dates %>%
      filter(
        project %in% project_list(),
        site %in% site_list(),
        pi_prop %in% pi_prop_list()
      )
    plot_complete(df)
  })
  
  output$inel_plot <- renderPlot({
     inel_df %>%
        filter(
          site %in% site_list(),
          project %in% project_list(),
          pi_prop %in% pi_prop_list()
        ) %>%
      plot_screening_ineligibility()
  })

  output$p1_p3_randomization <- renderPlot({
    if ("proper" %in% pi_prop_list()) {
      enrollment_history <- enrollment_history %>%
        filter(project %in% project_list() &
          project != "Project 2" &
          site %in% sites)

      plot_randomization_goal(
        enrollment_history, input$start_date, max_date,
        start, goal_p1_p3
      )
    }
  })

  output$p2_randomization <- renderPlot({
    if ("proper" %in% pi_prop_list()) {
      enrollment_history <- enrollment_history %>%
        filter(project == "Project 2" &
          site %in% sites)

      plot_randomization_goal(
        enrollment_history, input$start_date, max_date,
        start, goal_p2
      )
    }
  })

  output$randomization_need <- renderTable({
    if ("proper" %in% pi_prop_list()) {
        randomization_needed(
          current_enrollment, input$start_date, max_date,
          goal_p1_p3, goal_p2, start
        ) %>%
          filter(project %in% project_list())
    }
  })
  
  output$avg_rand_table <- renderTable({
    if ("proper" %in% pi_prop_list()) {
      rand_rate(rc_df, input$start_date)[[1]]
    }
  })
  
  output$study2_avg_rand <- renderTable({
    if("proper" %in% pi_prop_list()) {
      s2_rand_rate(s2_bl2_dates)
    }
  })
  
  # subjects tab ------------
  output$subjects <- renderDataTable({
    df <- session_dates %>%
      filter(
        project %in% project_list(),
        site %in% site_list(),
        # subjectid %in% ip_list$subjectid,
        pi_prop %in% pi_prop_list()
      ) %>%
      filter(
        !sl_status %in% c(
          "Screening Ineligible", "Withdrawn - Pre-Product",
          "Withdrawn - Post-Product", "Complete"
        )
      )
    session_dates_list(df) %>%
      datatable(options = list(
        columnDefs = list(list(targets = c(8,11), visible = FALSE)),
        pageLength = 100
      )) %>%
      formatStyle(
        'date', 'problem',
        target = 'row',
        backgroundColor = styleEqual(c(TRUE, FALSE), c('yellow', 'white'))
      )
      
  })
  # adherence tab -------------
  output$cig_adherence <- renderPlot(
    {
      # filter reactive project, site, in progress
      df <- ivr_sum %>%
        filter(
          project %in% project_list(),
          site %in% site_list(),
          subjectid %in% ip_list$subjectid,
          pi_prop %in% pi_prop_list()
        )
      plot_cig_adherence(df)
    },
    height = reactive(ifelse(!is.null(input$innerWidth), input$innerWidth * 3 / 5, 0)) # keep aspect ratio the same
  )
  # product use tab  ---------

  use_history <- reactive ({
    sid <- input$selected_subjectid
    
    df_long <- ivr_timeseries(ivr, sid) %>%
      filter(!is.na(cigs))
    
    date_min <- min(mdy(df_long$calldate))
    date_max <- max(mdy(df_long$calldate))
    
    co <- co_df %>% 
      filter(subjectid == sid,
             ymd(date) >= date_min,
             ymd(date) <= date_max)
    
    list(df_long, co, date_min, date_max)
  })
  
  
  output$ivr_history <- renderPlot({
    use_history <- use_history()
    plot_ivr_timeseries(
      use_history[[1]], 
      ivr, input$selected_subjectid,
      input$show_ivr_loess,
      input$show_threshold)
  },
  height = 450, width = 650)
  
  output$co_values <- renderPlot({
    use_history <- use_history()
    # sid <- input$selected_subjectid
    # df_long <- ivr_timeseries(ivr, sid)
    sid <- input$selected_subjectid
    plot_co(sid, use_history[[2]],
            use_history[[3]], use_history[[4]],
            input$show_co_loess)
  },
  height = 450, width = 600)
  
  # Project 4 -------------
  # enrollment
  output$p4_enroll_tab <- renderTable(
    {
      summarize_enrollment(enrollment_history_p4) %>%
        filter(
          site %in% checkSite_p4,
          project %in% input$checkProject_p4
        )

      # #sums
      # t_sum <- t %>%
      #   summarise(across(-c(project, site), sum))
      #
      # t_sum$project <- "sum"
      # t_sum$site <- ""
      # t_sum$pi_prop <- ""
      #
      # # make bold
      # t_sum[] <- lapply(t_sum, function(x) paste("<strong>", x, "</strong>", sep=""))
      #
      # # blank row
      # t[nrow(t)+1,] <- ""
      #
      # rbind(t, t_sum)
    },
    sanitize.text.function = function(x) {
      x
    }, # apply HTML tags
    hover = TRUE,
    spacing = "s",
    align = "c",
  )

  output$enrollment_ts_p4 <- renderPlotly({
    df <- enrollment_history_p4 %>%
      filter(
        site %in% input$checkSite_p4,
        project %in% input$checkProject_p4
      )
    # filter(project %in% input$checkProject,
    #        site %in% input$checkSite,
    #        pi_prop %in% pi_prop_list())

    plot_enrollment(df)
  })
}

# Run the application
shinyApp(ui = ui, server = server)

