
library(shiny)
library(dplyr)
library(prophet)
library(shinythemes)


# UI ------------------------------
ui <- fluidPage(

        # Application title
        titlePanel("Prophet Explore"),
        
        # Sidebar -------------------------------------
        fluidPage(theme = shinytheme("flatly"),
                sidebarPanel(width=3,
                             
                             tabsetPanel(tabPanel(HTML("prophet <br> Parameters"),
                                                  
                                                  # paramter: growth
                                                  radioButtons("growth","growth",
                                                               c('linear','logistic'), inline = TRUE),
                                                  
                                                  # date input
                                                  dateInput("ch_date", "Add changepoints",value=NULL),
                                                  
                                                  # paramter: changepoints
                                                  textInput("changepoints","changepoints",NULL),
                                                  
                                                  uiOutput("ch_points"),
                                                  
                                                  # parameter: n.changepoints
                                                  numericInput("n.changepoints","n.changepoints", value = 25),
                                                  
                                                  # parameter: yearly.seasonality
                                                  checkboxInput("yearly","yearly.seasonality", value = TRUE),
                                                  
                                                  # parameter: weekly.seasonality 
                                                  checkboxInput("monthly","weekly.seasonality", value = TRUE),
                                                  
                                                  # parameter: holidays
                                                  
                                                  # parameter: seasonality.prior.scale
                                                  numericInput("seasonality_scale","seasonality.prior.scale", value = 10),
                                                  
                                                  # parameter: changepoint.prior.scale
                                                  numericInput("changepoint_scale","changepoint.prior.scale", value = 0.05, step = 0.01),
                                                  
                                                  # parameter: holidays.prior.scale
                                                  numericInput("holidays_scale","holidays.prior.scale", value = 10),
                                                  
                                                  # parameter: mcmc.samples
                                                  numericInput("mcmc.samples", "mcmc.samples", value = 0),
                                                  
                                                  # parameter: interval.width
                                                  numericInput("interval.width", "interval.width", value= 0.8, step = 0.1),
                                                  
                                                  # parameter: uncertainty.samples
                                                  numericInput("uncertainty.samples","uncertainty.samples", value = 1000),
                                                  
                                                  # parameter: fit
                                                  checkboxInput("fit", "fit", value = TRUE)),
                                         
                                         tabPanel(HTML("predict <br> Parameters"),
                                                  
                                                  # paramater: periods
                                                  numericInput("periods","periods",value=365),
                                                  
                                                  # parameter: freq
                                                  selectInput("freq","freq",
                                                              choices = c('day', 'week', 'month', 'quarter','year')),
                                                  # parameter: include_history
                                                  checkboxInput("include_history","include_history", value = TRUE)
                                                  ))
                             
                             
                             # h3("prophet Parameters"),
                             
                             
                             
                             
                             
                ),
                
                # Main panel -------------------------
                mainPanel(
                        
                        fluidRow(
                                ## upload file -----------------
                                column(width = 6,
                                        fileInput("ts_file","Choose CSV File",
                                                  accept = c(
                                                          "text/csv",
                                                          "text/comma-separated-values,text/plain",
                                                          ".csv"))),
                                ## plot buttion -----------------
                                 column(width = 3,
                                        actionButton("plot_btn2", "Plot",
                                                     style = "width:100%; margin-top: 25px;"))),
                        
                        ## plot tabs --------------------------------
                        fluidRow(column(width=12,
                                        tabsetPanel(tabPanel("Forecast Plot",
                                                             plotOutput("ts_plot")),
                                                    tabPanel("Prophet Plot Components",
                                                             plotOutput("prophet_comp_plot"))))
                                 ),
                        
                        ## test output --------
                        verbatimTextOutput("test")
                        
                        
                )
        )
)

# Server -----------------------
server <- function(input, output, session) {
        
        ## read csv file data----------
        dat <- reactive({
                req(input$ts_file)
                
                file_in <- input$ts_file
                
                df <- read.csv(file_in$datapath, header = T) %>% 
                        mutate(y = log(y))
                
                return(df)
        })
        
        changepoints_vector <- reactive({
                req(input$ch_points_param)
                ch <- input$ch_points_param %>% 
                        strsplit(",") %>% 
                        unlist 
                
                if(length(ch)==0) NULL
                else ch
        })
        
        ## propher forecast -----------
        prophet_model <- reactive({
                req(dat(),
                    input$n.changepoints,
                    input$seasonality_scale, input$changepoint_scale,
                    input$holidays_scale, input$mcmc.samples,
                    input$mcmc.samples, input$interval.width,
                    input$uncertainty.samples)
                
                prophet(dat(),
                        growth = input$growth,
                        changepoints = NULL,
                        n.changepoints = input$n.changepoints,
                        yearly.seasonality = input$yearly,
                        weekly.seasonality = input$monthly,
                        holidays = NULL,
                        seasonality.prior.scale = input$seasonality_scale,
                        changepoint.prior.scale = input$changepoint_scale,
                        holidays.prior.scale = input$holidays_scale,
                        mcmc.samples = input$mcmc.samples,
                        interval.width = input$interval.width,
                        uncertainty.samples = input$uncertainty.samples,
                        fit = input$fit)
        })
        
        ## Make dataframe with future dates for forecasting -------------
        future <- reactive({
                req(prophet_model(),input$periods, input$freq)
                make_future_dataframe(prophet_model(),
                                      periods = input$periods,
                                      freq = input$freq,
                                      include_history = input$include_history)
        })
        
        ## predict future values -----------------------
        forecast <- reactive({
                req(prophet_model(),future())
                predict(prophet_model(), future())
        })
        
        ## plot forecast -------------
        output$ts_plot <- renderPlot({
                req(prophet_model(), forecast())
                g <- plot(prophet_model(), forecast())
                g+theme_classic()
        })
        
        ## plot prophet components --------------
        output$prophet_comp_plot <- renderPlot({
                req(prophet_model(), forecast())
                prophet_plot_components(prophet_model(), forecast())
        })
        
        
        ## test op -------------------
        output$test <- renderPrint(input$yearly
                # changepoints_vector()
                                   # prophet_model()[["changepoints"]]
        )
        
        ## parameters ----------------
        output$ch_points <- renderUI({
                textAreaInput("ch_points_param","Selected Changepoints",value=NULL)
        })
        
        ## update textArea
        observeEvent(input$ch_date,{
                updateTextAreaInput(session,"ch_points_param",
                                    value=c(input$ch_points_param,
                                            as.character(as.Date(as.numeric(input$ch_date),
                                                                 origin="1970-01-01"))))
        })
        
        
}

# Run the application ---------------------------
shinyApp(ui = ui, server = server)

