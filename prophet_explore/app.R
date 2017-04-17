
library(shiny)
library(dplyr)
library(prophet)
library(ggplot2)
library(DT)
library(shinythemes)
library(shinyjs)


# UI ------------------------------
ui <- fluidPage(
        ## shiny theme to use ------------------
        theme = shinytheme("flatly"),
        ## use shinyjs -------------------------
        shinyjs::useShinyjs(),
        ## include css file --------------------
        tags$head(tags$style(includeCSS("./www/mycss.css"))),
        
        ## Application title -------------------
        titlePanel("Prophet Explore"),
        ## Help text paragraph ----------------
        helpText(tags$a(href="https://github.com/OmaymaS/Prophet_Explore","Prophet Explore "),
                 HTML("is a Shiny App that offers an interactive interface to explore the main functions of the "),
                 tags$a(href='https://facebookincubator.github.io/prophet/',"[prophet Package]"),
                 HTML("; an open source software released by Facebook's Core Data Science team."),
                 tags$br(),
                 HTML("To explore: upload your data in the right format, tune the parameters, then press 'Fit Prophet Model and Plot'.")),
        tags$br(),
        
        # Sidebar -------------------------------------
        fluidPage(sidebarPanel(width=3,
                               tabsetPanel(
                                       ## Tab prophet parameters ----------------------------
                                       tabPanel(HTML("prophet <br> Parameters"),
                                                
                                                ### paramter: growth
                                                h5(tags$b("growth")),
                                                
                                                helpText("If growth is logistic, the input dataframe must have a column cap that specifies the capacity at each ds.",
                                                         style = "margin-bottom: 0px;"),
                                                
                                                radioButtons("growth","",
                                                             c('linear','logistic'), inline = TRUE),
                                                
                                                ### parameter: fit
                                                # checkboxInput("fit", "fit", value = TRUE),
                                                
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
                                       
                                       ## Tab predict parameters -------------------------
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
                                                              style = "width:80%; margin-top: 25px;")))
                ),
                
                fluidRow(column(width = 12,
                                uiOutput("msg"))),
                
                fluidRow(column(width = 12,
                                uiOutput("msg2"))),
                
                fluidRow(
                        column(width = 12
                               # uiOutput("ch_points", style = "width:100")
                               # conditionalPanel("input.ch_points_param",
                               #                  dateInput("ch_date", "Add changepoints", value = NULL))
                        )
                ),
                
                ## plot/results tabs --------------------------------
                fluidRow(column(width=12,
                                tabsetPanel(
                                        tabPanel("Forecast Plot",
                                                 conditionalPanel("input.plot_btn2",
                                                                  div(id = "output-container1",
                                                                      tags$img(src = "spinner.gif",
                                                                               id = "loading-spinner"),
                                                                      plotOutput("ts_plot")
                                                                      )
                                                                  )
                                        ),
                                        tabPanel("Prophet Plot Components",
                                                 # output.logistic_check=='no_error'
                                                 conditionalPanel("input.plot_btn2",
                                                                  div(id = "output-container2",
                                                                      tags$img(src = "spinner.gif",
                                                                               id = "loading-spinner"))),
                                                                      plotOutput("prophet_comp_plot")
                                                                  # )
                                                 # )
                                        ),
                                        tabPanel("Forecast Results",
                                                 conditionalPanel("output.data",
                                                                  uiOutput("dw_button")
                                                 ),
                                                 
                                                 # uiOutput("dw_button"),
                                                 conditionalPanel("input.plot_btn2",
                                                                  div(id = "output-container3",
                                                                      tags$img(src = "spinner.gif",
                                                                               id = "loading-spinner"))),
                                                                      dataTableOutput("data")
                                                                  #     )
                                                                  # )
                                        )
                                )
                )
                ),
                
                ## test output --------
                verbatimTextOutput("test")
                # uiOutput("logistic_check")
                
        )
        )
)

# Server -----------------------
server <- function(input, output, session) {
        
        ## function: duplicatedRecative values -----------------------------
        duplicatedRecative <- function(signal){
                values <- reactiveValues(val="")
                
                observe({
                        values$val <- signal()
                })
                
                reactive(values$val)
        }
        
        ## read csv file data----------
        dat <- reactive({
                req(input$ts_file)
                
                file_in <- input$ts_file
                
                # read csv
                df <- read.csv(file_in$datapath, header = T) 
                
                # validate input df names 
                validate(
                        need( try("ds" %in% names(df) & "y" %in% names(df)),
                              "Error: Input dataframe should have at least two columns named (ds & y)")
                        )
                # return df
                df %>% 
                        mutate(y = log(y))
        })
        
        ## enable calculate button when file is uploaded ---------------------
        observeEvent(input$ts_file,{
                shinyjs::enable("plot_btn2")
        })
        
        ## get holidays -------------
        holidays_upload <- reactive({
                if(is.null(input$holidays_file)) h <- NULL
                else h <- read.csv(input$holidays_file$datapath, header = T) 
                return(h)
        })

        ## logistic_check -------------------
       logistic_check <- eventReactive(input$plot_btn2, {
                # req(dat())
                if( (input$growth == "logistic") & !("cap" %in% names(dat())) )
                {
                        return("error")
                }
                else 
                        return("no_error")
        })
        
        ## create prophet model -----------
        prophet_model <- eventReactive(input$plot_btn2,{
                req(dat(),
                    input$n.changepoints,
                    input$seasonality_scale, input$changepoint_scale,
                    input$holidays_scale, input$mcmc.samples,
                    input$mcmc.samples, input$interval.width,
                    input$uncertainty.samples)
        
                
                
                if(input$growth == "logistic"){
                        validate(
                                need(try("cap" %in% names(dat())),
                                     "Error: for logistic 'growth', the input dataframe must have a column 'cap' that specifies the capacity at each 'ds'."))
    
                }

                # if(!identical(rv$dat_last[[1]],rv$dat_last[[2]]))
                #         
                # {
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
                              fit = T)
                # print(kk$changepoints)
                
                return(kk)
                
                # } else
                #         return(p_model())
                
        })
        
        ## dup reactive --------------
        p_model <- duplicatedRecative(prophet_model)
        
        ## Make dataframe with future dates for forecasting -------------
        future <- eventReactive(input$plot_btn2,{
                req(p_model(),input$periods, input$freq)
                make_future_dataframe(p_model(),
                                      periods = input$periods,
                                      freq = input$freq,
                                      include_history = input$include_history)
        })
        
        ## dup reactive --------------
        p_future <- duplicatedRecative(future)
        
        ## predict future values -----------------------
        forecast <- reactive({
                req(prophet_model(),p_future())
                predict(prophet_model(),p_future())
        })
        
        ## dup reactive --------------
        p_forecast <- duplicatedRecative(forecast)
        
        ## plot forecast -------------
        output$ts_plot <- renderPlot({
                req(logistic_check()!="error")
                g <- plot(p_model(), forecast())
                g+theme_classic()
        })
        
        ## plot prophet components --------------
        output$prophet_comp_plot <- renderPlot({
                req(logistic_check()!="error")
                prophet_plot_components(p_model(),forecast())
        })
        
        ## create datatable from forecast dataframe --------------------
        output$data <- renderDataTable({
                req(logistic_check()!="error")
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
                filename = "forecast_data.csv",
                content = function(file) {
                        write.csv(forecast(), file)
                }
        )
        
        ## error msg ------------------------
        output$msg <- renderUI({
                req(dat())
                textOutput("")
        })
        # 
        ## error msg2 ------------------------
        output$msg2 <- renderUI({
                req(prophet_model())
                textOutput("")
        })
        ## output tes --------------
        # output$test <- renderPrint({
        #         # logistic_check()
        #         # paste0("msg:",prophet_model())
        #         # forecast() %>% length()
        # })
        
}

# Run the application ---------------------------
shinyApp(ui = ui, server = server)

