
library(shiny)
library(dplyr)
library(prophet)
library(ggplot2)
library(DT)
library(shinythemes)
library(shinyjs)


# UI ------------------------------
ui <- fluidPage(
        shinyjs::useShinyjs(),
        # tags$head(tags$style(HTML(mycss))),
        tags$head(tags$style(includeCSS("./www/mycss.css"))),
        
        # Application title
        titlePanel("Prophet Explore"),
        helpText(tags$a(href="https://github.com/OmaymaS/Prophet_Explore","Prophet Explore "),
                 HTML("is a Shiny App that offers an interactive interface to explore the main functions of the "),
                 tags$a(href='https://facebookincubator.github.io/prophet/',"[prophet Package]"),
                 HTML("; an open source software released by Facebook's Core Data Science team."),
                 tags$br(),
                 HTML("To explore: upload your data in the right format, tune the parameters, then press 'Fit Prophet Model and Plot'.")),
        tags$br(),
        
        # Sidebar -------------------------------------
        fluidPage(theme = shinytheme("flatly"),
                  sidebarPanel(width=3,
                               tabsetPanel(
                                       tabPanel(HTML("prophet <br> Parameters"),
                                                
                                                ## prophet() parameters ------------------------------
                                                ### paramter: growth
                                                h5(tags$b("growth")),
                                                
                                                helpText("If growth is logistic, the input dataframe must have a column cap that specifies the capacity at each ds.",
                                                         style = "margin-bottom: 0px;"),
                                                
                                                radioButtons("growth","",
                                                             c('linear','logistic'), inline = TRUE),
                                                
                                                
                                                
                                                # ### date input
                                                # dateInput("ch_date", "Add changepoints",value=NULL),
                                                # 
                                                # ### paramter: changepoints
                                                # # textInput("changepoints","changepoints",NULL),
                                                # 
                                                # uiOutput("ch_points"),
                                                
                                                
                                                ### parameter: fit
                                                checkboxInput("fit", "fit", value = TRUE),
                                                
                                                ### parameter: yearly.seasonality
                                                checkboxInput("yearly","yearly.seasonality", value = TRUE),
                                                
                                                ### parameter: weekly.seasonality 
                                                checkboxInput("monthly","weekly.seasonality", value = TRUE),
                                                ### parameter: n.changepoints
                                                numericInput("n.changepoints","n.changepoints", value = 25),
                                                
                                                ### parameter: seasonality.prior.scale
                                                numericInput("seasonality_scale","seasonality.prior.scale", value = 10),
                                                
                                                ### parameter: changepoint.prior.scale
                                                numericInput("changepoint_scale","changepoint.prior.scale", value = 0.05, step = 0.01),
                                                
                                                ### parameter: holidays.prior.scale
                                                numericInput("holidays_scale","holidays.prior.scale", value = 10),
                                                
                                                ### parameter: mcmc.samples
                                                numericInput("mcmc.samples", "mcmc.samples", value = 0),
                                                
                                                ### parameter: interval.width
                                                numericInput("interval.width", "interval.width", value= 0.8, step = 0.1),
                                                
                                                ### parameter: uncertainty.samples
                                                numericInput("uncertainty.samples","uncertainty.samples", value = 1000),
                                                
                                                
                                                ### parameter: holidays
                                                h5(tags$b("holidays (optional)")),
                                                
                                                helpText("Upload a data frame with columns holiday (character) and ds (date type) and optionally columns lower_window and upper_window which specify a range of days around the date to be included as holidays."),
                                                
                                                fileInput("holidays_file","",
                                                          accept = c(
                                                                  "text/csv",
                                                                  "text/comma-separated-values,text/plain",
                                                                  ".csv"))
                                                
                                       ),
                                       
                                       tabPanel(HTML("predict <br> Parameters"),
                                                
                                                ## make_future_dataframe() parameters ------------------
                                                ### paramater: periods
                                                numericInput("periods","periods",value=365),
                                                
                                                ### parameter: freq
                                                selectInput("freq","freq",
                                                            choices = c('day', 'week', 'month', 'quarter','year')),
                                                ### parameter: include_history
                                                checkboxInput("include_history","include_history", value = TRUE)
                                       ))
                               
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
                                  ## plot button -----------------
                                  column(width = 6,
                                         shinyjs::disabled(actionButton("plot_btn2", "Fit Prophet Model & Plot",
                                                                        style = "width:80%; margin-top: 25px;"))
                                         
                                         )
                                  ),
                          
                          fluidRow(column(width = 6,
                                          conditionalPanel("input.ch_points_param",
                                                           dateInput("ch_date", "Add changepoints", value = NULL))
                                          
                          ),
                          
                          column(width = 6,uiOutput("ch_points"))
                          ),
                          
                          ## plot/results tabs --------------------------------
                          fluidRow(column(width=12,
                                          tabsetPanel(
                                                  tabPanel("Forecast Plot",
                                                           conditionalPanel("input.plot_btn2",
                                                                            div(id = "output-container",
                                                                                tags$img(src = "spinner.gif",
                                                                                         id = "loading-spinner"),
                                                                                plotOutput("ts_plot")))
                                                  ),
                                                  tabPanel("Prophet Plot Components",
                                                           conditionalPanel("input.plot_btn2",
                                                                            div(id = "output-container",
                                                                                tags$img(src = "spinner.gif",
                                                                                         id = "loading-spinner"),
                                                                                plotOutput("prophet_comp_plot")))
                                                  ),
                                                  tabPanel("Forecast Results",
                                                           uiOutput("dw_button"),
                                                           conditionalPanel("input.plot_btn2",
                                                                            div(id = "output-container",
                                                                                tags$img(src = "spinner.gif",
                                                                                         id = "loading-spinner"),
                                                                                dataTableOutput("data")))
                                                  )
                                          )
                          )
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
                
                read.csv(file_in$datapath, header = T) %>% 
                        mutate(y = log(y))
        })
        
        observeEvent(input$ts_file,{
                shinyjs::enable("plot_btn2")
        })
        
        # observeEvent(!(is.null(input$ts_file)),{
        #         shinyjs::enable("plot_btn2")
        # })
        
        # output$go_button <- renderUI({
        #         if(is.null(input$ts_file)){
        #                 
        #         }else{
        #                 
        #         }
        # })
        
        
        ## get holidays -------------
        holidays_upload <- reactive({
                if(is.null(input$holidays_file)) h <- NULL
                else h <- read.csv(input$holidays_file$datapath, header = T) 
                return(h)
        })
        
        ## create prophet model -----------
        prophet_model <- eventReactive(input$plot_btn2,{
                req(dat(),
                    input$n.changepoints,
                    input$seasonality_scale, input$changepoint_scale,
                    input$holidays_scale, input$mcmc.samples,
                    input$mcmc.samples, input$interval.width,
                    input$uncertainty.samples)
                
                
                
                
                kk <- prophet(dat(),
                              growth = input$growth,
                              changepoints = NULL,
                              n.changepoints = input$n.changepoints,
                              yearly.seasonality = input$yearly,
                              weekly.seasonality = input$monthly,
                              holidays = holidays_upload(),
                              seasonality.prior.scale = input$seasonality_scale,
                              changepoint.prior.scale = input$changepoint_scale,
                              holidays.prior.scale = input$holidays_scale,
                              mcmc.samples = input$mcmc.samples,
                              interval.width = input$interval.width,
                              uncertainty.samples = input$uncertainty.samples,
                              fit = input$fit)
                
        })
        
        ## Make dataframe with future dates for forecasting -------------
        future <- eventReactive(input$plot_btn2,{
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
                # input$plot_btn2
                req(prophet_model(), forecast())
                
                g <- plot(prophet_model(), forecast())
                g+theme_classic()
        })
        
        ## plot prophet components --------------
        output$prophet_comp_plot <- renderPlot({
                # input$plot_btn2
                req(prophet_model(), forecast())
                
                prophet_plot_components(isolate(prophet_model()), isolate(forecast()))
        })
        
        ## create datatabke from forecast dataframe --------------------
        output$data <- renderDataTable({
                
                # input$plot_btn2
                
                # dat <- data.frame(x = numeric(0), y = numeric(0))
                # 
                # withProgress(message = 'Generating data', detail = "part 0", value = 0, {
                #         for (i in 1:10) {
                #                 # Each time through the loop, add another row of data. This a stand-in
                #                 # for a long-running computation.
                #                 dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
                #                 
                #                 # Increment the progress bar, and update the detail text.
                #                 incProgress(0.1, detail = paste("part", i))
                #                 
                #                 # Pause for 0.1 seconds to simulate a long computation.
                #                 Sys.sleep(2)
                #         }
                # })
                
                datatable(forecast()) %>% 
                        formatRound(columns=2:17,digits=4)
        })
        
        ## download button ----------------
        output$dw_button <- renderUI({
                req(forecast())
                downloadButton('downloadData', 'Download Data',
                               style = "width:20%;
                               margin-bottom: 25px;
                               margin-top: 25px;")
        })
        
        output$downloadData <- downloadHandler(
                # filename = function() { paste(input$dataset, '.csv', sep='') },
                filename = "forecast_data.csv",
                content = function(file) {
                        write.csv(forecast(), file)
                }
        )
        
        ## test op -------------------
        output$test <- renderPrint({
                is.null(input$ts_file)
        })
        
        ## selected Changepoints ----------------
        # output$ch_points <- renderUI({
        #         req(prophet_model())
        #         textAreaInput("ch_points_param","Selected Changepoints",
        #                       value=paste(prophet_model()$changepoints,collapse=", "))
        # })
        
        ## changepoints_vector -------------------------
        # changepoints_vector <- reactive({
        #         req(input$ch_points_param)
        #         ch <- input$ch_points_param %>% 
        #                 strsplit(",") %>% 
        #                 unlist 
        #         
        #         if(length(ch)==0) NULL
        #         else ch
        # })
        
        # update textArea ------------------------
        # observeEvent(input$ch_date,{
        #         updateTextAreaInput(session,"ch_points_param",
        #                             value=c(input$ch_points_param,
        #                                     as.character(as.Date(as.numeric(input$ch_date),
        #                                                          origin="1970-01-01"))))
        # })
        
        
}

# Run the application ---------------------------
shinyApp(ui = ui, server = server)

