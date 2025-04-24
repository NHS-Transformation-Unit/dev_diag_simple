library(shiny)
library(DT)

shinyUI(navbarPage("Diagnostic Pathway Simulator",
                   tags$link(rel = "stylesheet", type = "text/css", href = "config/app_theme.css"),
                   tabPanel("Introduction",
                            fluidPage(
                              tags$img(src = "images/TU_logo_large.png", height = "80px", style = "margin-bottom:20px"),
                              h2("Welcome to the Diagnostic Pathway Simulation App"),
                              hr(),
                              p("We have created this simulation app of a simple diagnostic pathway as an example 
                                of how the ", strong("Transformation Unit"), "can support demand and capacity modelling within diagnostics. 
                                Please explore the app to understand how changes in demand and ring-fencing capacity can impact on the size 
                                of waiting times. For the purposes of this demonstration, the diagnostic pathway is fairly simple. However, it includes 
                                real world constraints that would need to be considered when modelling any pathway."),
                              br(),
                              h3("What does the pathway look like?"),
                              hr(),
                              p("THe diagnostic pathway involves:"),
                              tags$ul(
                                tags$li("Patients are referred into the service for a planned investigation"),
                                tags$li("A minimum waiting time is applied to each patient to account for the time required to contact them"),
                                tags$li("Patients are seen in order, i.e. the longest waiter is seen next"),
                                tags$li("Alongside planned patients there are also emergency inpatients who need to be seen as a priority"),
                                tags$li("Emergency patients are treated in capacity ring-fenced for emergencies only"),
                                tags$li("If there is not enough emergency capacity then planned capacity will be utilised to accommodate 
                                        these patients.")
                              ),
                              br(),
                              h3("How to run the model"),
                              hr(),
                              p("Running the model has two stages. The first involves setting the current waiting list 
                                for the start of the simulation. To do this:"),
                              tags$ul(
                                tags$li("Navigate to the ", strong("Starting Waiting List"), " tab."),
                                tags$li("Use the inputs to set the size of the waiting list at the start and to set the 
                                        distribution of the waiting times for those patients."),
                                tags$li("This distribution can be set by applying the mean and standard deviation of the 
                                        current waiting times."),
                                tags$li("These then simulate the waiting times for the starting population using the ", 
                                        strong("log-normal distribution."))
                              ),
                              p("The next stage is to then set the daily demand and capacity for both planned and 
                                emergency referrals. This is done by:"),
                              tags$ul(
                                tags$li("Navigate to the ", strong("Run Model"), " tab."),
                                tags$li("Set the number of days that you want to run the simulation for"),
                                tags$li("Set the inputs for the average number of planned and emergency patients. 
                                         This represents your demand."),
                                tags$li("Set the daily capacity for both the planned and emergency patients."),
                                tags$li("Set the minimum waiting time for planned patients to accommodate getting them 
                                        booked onto a slot.")
                              ),
                              p("In the main panel you will see charts showing the waiting list size at the end of each 
                                day in the simulation and a summary of how long those patients have been waiting. These 
                                charts will automatically update as you change the model parameters. You can also see 
                                the detail across each day of the simulation by selecting the ", strong("Daily Table"), 
                                " tab. This shows how the waiting list changes each day based on the demand and capacity 
                                available. The data can be downloaded using the ", strong("Download Table"), " button."),
                              br(),
                              h3("Want to know more?"),
                              hr(),
                              p("If you would like to know more about how demand and capacity modelling can support the re-design 
                                of diagnostic pathways please contact ", a("Andy Wilson", href = "mailto:andy.wilson8@nhs.net"))
                            )
                   ),
                   
                   tabPanel("Starting Waiting List",
                            sidebarLayout(
                              sidebarPanel(
                                h4("Set Waiting List Parameters"),
                                p("Use the parameters below to simulate the starting waiting list:"),
                                br(),
                                numericInput("init_pop", "Starting waiting list size:", 100, min = 0),
                                numericInput("init_mean", "Mean of initial waiting times:", 7, min = 0.1),
                                numericInput("init_sd", "SD of initial waiting times:", 3, min = 0.1)
                              ),
                              mainPanel(
                                plotOutput("start_hist")
                              )
                            )
                   ),
                   
                   tabPanel("Run Model",
                            sidebarLayout(
                              sidebarPanel(
                                h3("Set Demand and Capacity Parameters:"),
                                p("Use the parameters below to set the demand and capacity parameters for the 
                                  diagnostic pathway"),
                                br(),
                                numericInput("seed", "Random seed (for reproducibility):", value = 1410, min = 1),
                                numericInput("n_days", "Number of days to simulate:", 30, min = 1),
                                numericInput("lambda", "Mean planned referrals per day:", 10, min = 0.1),
                                numericInput("emergency_lambda", "Mean emergency referrals per day:", 2, min = 0),
                                numericInput("planned_capacity", "Planned capacity per day:", 8, min = 0),
                                numericInput("emergency_capacity", "Emergency capacity per day:", 2, min = 0),
                                numericInput("min_wait", "Minimum days to contact planned patient:", 2, min = 0)
                              ),
                              mainPanel(
                                tabsetPanel(
                                  tabPanel("Summary Plots",
                                           h3("Model Summary Outputs"),
                                           hr(),
                                           p("The chart below shows the number of patients who are on the waiting list 
                                             at the end of each day of the simulation."),
                                           plotOutput("waitlist_plot"),
                                           br(),
                                           p("The chart below shows a summary of how long patients still on the waiting 
                                             list at the end of each day have waited. The central line represents the ",
                                             strong("median"), " waiting times and the shaded area represents the ",
                                             strong("Interquartile Range.")),
                                           plotOutput("wait_summary_plot")
                                  ),
                                  tabPanel("Daily Table",
                                           h3("Waiting List Position Across Each Day"),
                                           hr(),
                                           p("The table below provides a summary of what is happening across each day of 
                                              the model simulation such as the number of patients waiting at the start of 
                                              the day, new referrals and the number of planned patients seen."),
                                           br(),
                                           DT::dataTableOutput("daily_table"),
                                           downloadButton("download_table", "Download Table")
                                  )
                                )
                              )
                            )
                   )
))
