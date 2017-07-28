library(DT)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(dplyr)
library(prophet)
library(ggplot2)


ui <- dashboardPage(
  dashboardHeader(title = "Prophet Explorer"),
  
  
  dashboardSidebar(
    # sidebarMenuOutput("menu")
    
    # radioButtons("growth","growth",
    #              c('linear','logistic'), inline = TRUE),
    # 
    # ### parameter: yearly.seasonality
    # checkboxInput("yearly","yearly.seasonality", value = TRUE),
    # 
    # ### parameter: weekly.seasonality 
    # checkboxInput("monthly","weekly.seasonality", value = TRUE),
    # ### parameter: n.changepoints
    # numericInput("n.changepoints","n.changepoints", value = 25),
    # 
    # ### parameter: seasonality.prior.scale
    # numericInput("seasonality_scale","seasonality.prior.scale", value = 10),
    # 
    # ### parameter: changepoint.prior.scale
    # numericInput("changepoint_scale","changepoint.prior.scale", value = 0.05, step = 0.01),
    # 
    # ### parameter: holidays.prior.scale
    # numericInput("holidays_scale","holidays.prior.scale", value = 10),
    # 
    # ### parameter: mcmc.samples
    # numericInput("mcmc.samples", "mcmc.samples", value = 0),
    # 
    # ### parameter: interval.width
    # numericInput("interval.width", "interval.width", value= 0.8, step = 0.1),
    # ### parameter: uncertainty.samples
    # numericInput("uncertainty.samples","uncertainty.samples", value = 1000)
  ),
  
  dashboardBody(
    ## include css file --------------------
    tags$head(tags$style(includeCSS("./www/mycss.css"))),
    ## use shinyjs -----------------------
    useShinyjs(),

    fluidRow(
      column(width = 12,
             tabsetPanel(id = "inTabset",
                         ## TAB 1 -------------
                         tabPanel(title = "Upload Data", value = "panel1",
                                  fluidRow(br()),
                                  fluidRow(
                                    ## upload main dataset -----------------
                                    box(width = 6,
                                        column(width = 12,
                                               tags$h4("Main Dataset"),
                                               fileInput("ts_file","Upload CSV File",
                                                         accept = c(
                                                           "text/csv",
                                                           "text/comma-separated-values,text/plain",
                                                           ".csv"))),
                                        column(width = 12,
                                               tableOutput("uploaded_data")),
                                        column(width = 4, offset = 4)
                                    ),
                                    ## upload holidays -----------------
                                    box(width = 6,
                                        column(width = 12,
                                               tags$h4("Holidays (Optional)"),
                                               fileInput("holidays_file","Upload CSV File",
                                                         accept = c(
                                                           "text/csv",
                                                           "text/comma-separated-values,text/plain",
                                                           ".csv"))))
                                    
                                  ),
                                  ## Next 1 ---------------
                                  fluidRow(
                                    box(width = 12,
                                        column(width = 2, offset = 6,
                                               shinyjs::disabled(actionButton("next1", "Next",
                                                                              style = "width:100%; margin-top: 25px; font-size:200%"))))
                                  )
                         ),
                         ## TAB 2 -------------
                         tabPanel(title = "Set Parameters", value = "panel2",
                                  fluidRow(
                                    box(width = 12,
                                        ## prophet parameters --------------------
                                        box(width = 8,
                                            column(width = 8, offset = 2,
                                                   tags$h3("Prophet Parameters")),
                                            column(width = 6,
                                                   
                                                   radioButtons("growth","growth",
                                                                c('linear','logistic'), inline = TRUE),
                                                   
                                                   ### parameter: yearly.seasonality
                                                   checkboxInput("yearly","yearly.seasonality", value = TRUE),
                                                   
                                                   ### parameter: weekly.seasonality 
                                                   checkboxInput("monthly","weekly.seasonality", value = TRUE),
                                                   ### parameter: n.changepoints
                                                   numericInput("n.changepoints","n.changepoints", value = 25),
                                                   
                                                   ### parameter: seasonality.prior.scale
                                                   numericInput("seasonality_scale","seasonality.prior.scale", value = 10),
                                                   
                                                   ### parameter: changepoint.prior.scale
                                                   numericInput("changepoint_scale","changepoint.prior.scale", value = 0.05, step = 0.01)),
                                            column(width = 6,
                                                   
                                                   ### parameter: holidays.prior.scale
                                                   numericInput("holidays_scale","holidays.prior.scale", value = 10),
                                                   
                                                   ### parameter: mcmc.samples
                                                   numericInput("mcmc.samples", "mcmc.samples", value = 0),
                                                   
                                                   ### parameter: interval.width
                                                   numericInput("interval.width", "interval.width", value= 0.8, step = 0.1),
                                                   ### parameter: uncertainty.samples
                                                   numericInput("uncertainty.samples","uncertainty.samples", value = 1000))
                                            
                                        ),
                                        ## predict parameters --------------------
                                        box(width = 4,
                                            column(width = 12,
                                                   tags$h3("Predict Parameters")),
                                            column(width = 12,
                                                   ### paramater: periods
                                                   numericInput("periods","periods",value=365),
                                                   
                                                   ### parameter: freq
                                                   selectInput("freq","freq",
                                                               choices = c('day', 'week', 'month', 'quarter','year')),
                                                   
                                                   ### parameter: include_history
                                                   checkboxInput("include_history","include_history", value = TRUE))),
                                        
                                        ## Back/Next 2 --------------------------
                                        box(width = 12,
                                            column(width = 2, offset = 4,
                                                   actionButton("back2", "Back",
                                                                style = "width:100%; margin-top: 25px; font-size:200%")),
                                            column(width = 2,
                                                   actionButton("next2", "Next",
                                                                style = "width:100%; margin-top: 25px; font-size:200%")))
                                        
                                        
                                    ))),
                         ## TAB 3 -------------
                         tabPanel(title = "Fit Model", value = "panel3", 
                                  fluidRow(
                                    box(width = 12,
                                        column(width = 12,
                                               shinyjs::disabled(actionButton("plot_btn2", "Fit Prophet Model",
                                                                              style = "width:20%; margin-top: 25px;")
                                               )
                                        ),
                                        column(width = 12, HTML("<br><br>")),
                                        column(width = 12,
                                               conditionalPanel("input.plot_btn2",
                                                                div(id = "output-container3",
                                                                    tags$img(src = "spinner.gif",
                                                                             id = "loading-spinner"),
                                                                    DT::dataTableOutput("data"))))
                                    ),
                                    
                                    box(width = 12,
                                        column(width = 2, offset = 4,
                                               actionButton("back3", "Back",
                                                            style = "width:100%; margin-top: 25px; font-size:200%")),
                                        column(width = 2,
                                               actionButton("next3", "Next",
                                                            style = "width:100%; margin-top: 25px; font-size:200%")))
                                  )
                                  
                         ),
                         tabPanel(title = "Plot Results", value = "panel4",
                                  fluidRow(
                                    box(width =12, 
                                        
                                        column(width = 12,
                                               shinyjs::disabled(actionButton("plot_btn3", "Plot Results",
                                                                              style = "width:20%; margin-top: 25px;")
                                               )
                                        ),
                                        column(width = 12, HTML("<br><br>")),
                                        column(width=12,
                                               tabsetPanel(
                                                 tabPanel("Forecast Plot",
                                                          conditionalPanel("input.plot_btn3",
                                                                           div(id = "output-container",
                                                                               tags$img(src = "spinner.gif",
                                                                                        id = "loading-spinner")
                                                                           ),
                                                                           div(plotOutput("ts_plot"))
                                                          )
                                                 ),
                                                 tabPanel("Prophet Plot Components",
                                                          # output.logistic_check=='no_error'
                                                          conditionalPanel("input.plot_btn3",
                                                                           div(id = "output-container2",
                                                                               tags$img(src = "spinner.gif",
                                                                                        id = "loading-spinner"),
                                                                               plotOutput("prophet_comp_plot"))
                                                          )
                                                 )
                                               )
                                        )
                                    ),
                                    
                                    box(width = 12,
                                        column(width = 2, offset = 4,
                                               actionButton("back4", "Back",
                                                            style = "width:100%; margin-top: 25px; font-size:200%"))
                                    )
                                  ))
             )
      )
      
    )
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$next1, {
    updateTabsetPanel(session, "inTabset",
                      selected = "panel2")
  })
  
  observeEvent(input$next2, {
    updateTabsetPanel(session, "inTabset",
                      selected = "panel3")
  })
  
  observeEvent(input$back2, {
    updateTabsetPanel(session, "inTabset",
                      selected = "panel1")
  })
  
  observeEvent(input$back3, {
    updateTabsetPanel(session, "inTabset",
                      selected = "panel2")
  })
  
  observeEvent(input$back4, {
    updateTabsetPanel(session, "inTabset",
                      selected = "panel3")
  })
  
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
    read.csv(file_in$datapath, header = T)     # read csv
  })
  
  ## get holidays -------------
  holidays_upload <- reactive({
    if(is.null(input$holidays_file)) h <- NULL
    else h <- read.csv(input$holidays_file$datapath, header = T) 
    return(h)
  })
  
  ## Toggle submit button state according to data ---------------
  observe({
    if(!(c("ds","y") %in% names(dat()) %>% mean ==1))
      shinyjs::disable("next1")
    else if(c("ds","y") %in% names(dat()) %>% mean ==1)
      shinyjs::enable("next1")
  })
  
  ##
  output$uploaded_data <- renderTable({
    req(dat)
    head(dat())
  })
  
  ## Toggle submit button state according to data ---------------
  observe({
    if(!(c("ds","y") %in% names(dat()) %>% mean ==1))
      shinyjs::disable("plot_btn2")
    else if(c("ds","y") %in% names(dat()) %>% mean ==1)
      shinyjs::enable("plot_btn2")
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
        # ("ds" %in% dat()), "y" %in% names(dat()),
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
    
    datx <- dat() %>% 
      mutate(y = log(y))
    
    kk <- prophet(datx,
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
    
    return(kk)
    # print(head(kk))
    
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
  output$uploaded_data2 <- renderPrint({
    req(prophet_model())
    # prophet_model()
  })
  
  ## create datatable from forecast dataframe --------------------
  output$data <- renderDataTable({
    # req(logistic_check()!="error")
    DT::datatable(forecast(), 
                  options = list(scrollX = TRUE, pageLength = 5)) %>% 
      formatRound(columns=2:17,digits=4)
  })
  
  ## Toggle submit button state according to data ---------------
  observe({
    # if(nrow(forecast() == 0))
    #   shinyjs::disable("plot_btn3")
    # else 
    if(nrow(forecast() > 0))
      shinyjs::enable("plot_btn3")
  })
  
  ## plot forecast -------------
  output$ts_plot <- renderPlot({
    # req(logistic_check()!="error")
    g <- plot(p_model(), forecast())
    g+theme_classic()
  })
  
  ## plot prophet components --------------
  output$prophet_comp_plot <- renderPlot({
    # req(logistic_check()!="error")
    prophet_plot_components(p_model(),forecast())
  })
  
}

shinyApp(ui, server)
