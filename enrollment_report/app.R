library(shiny)
library(dplyr)
library(redcapAPI)
library(stringr)
library(ggplot2)

# source fucntions
source("functions.R")

# set plotting parameters
theme_set(theme_classic(base_size = 15))

# main:background ----------
# pull REDCap enrollment data, save to history
# this will be a "refresh data" button

# rcon <- build_rcon("rc_proper")
rc_df <- download_rc_dataframe(rcon, sl_values, NULL)
# current_enrollment <- suppressWarnings(get_current_enrollment(df))
# append_enrollment(current_enrollment) # no return, saves to .tsv file
ip_list <- in_progress_list(rc_df)

# load time-series enrollment
enrollment_history <- load_enrollment_history()

# load ivr
ivr <- load_ivr()
ivr_summary <- ivr_summary(ivr)
# for app display --------



#plots


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # App title -----
    titlePanel("TCORS Study 3"),
    
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
    
    mainPanel(
        
        tabsetPanel(type = "tabs",
                    tabPanel("Enrollment", 
                             tableOutput("enroll_tab"),
                             plotOutput("enroll_plot", width = "150%"),
                             textOutput("test")),
                    tabPanel("Adherence",
                             plotOutput("cig_adherence", width = "150%"),
                             tableOutput("interesting_ivr"))
        )
    )

)

# Define server logic 
server <- function(input, output) {
    # reactivity

    # enrollment tab --------
    output$enroll_tab <- renderTable({
        summarize_enrollment(enrollment_history) %>%
            filter(project %in% input$checkProject,
                   site %in% input$checkSite)
    },
        hover = TRUE,
        spacing = 'l',
        align = 'c'
    )
    
    output$enroll_plot <- renderPlot({
        plot_enrollment(
            enrollment_history %>%
                filter(project %in% input$checkProject,
                       site %in% input$checkSite)
        )
        }
    )
    
    # adherence tab -------------
    output$cig_adherence <- renderPlot({
        df <- ivr_summary %>%
            filter(project %in% input$checkProject,
                   site %in% input$checkSite)
        plot_cig_adherence(df)
        
    })
    
    output$interesting_ivr <- renderTable({
        df <- ivr %>%
            filter(project %in% input$checkProject,
                   site %in% input$checkSite)
        ivr_stats(df)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
