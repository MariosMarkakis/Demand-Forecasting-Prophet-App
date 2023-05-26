packages <- c("shiny","dagR","data.table","reshape","plyr","dplyr","pdc","chron","TSA"
              ,"changepoint","sqldf","odbc","stringi","xts","shinyWidgets"
              ,"shinydashboard","shinycssloaders","xlsx","prophet","DT","highcharter")

new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
invisible(lapply(packages,library,character.only=TRUE))

#input countries and cities for filters
filter_data <- read.table(
  text = "Country,City
Colombia,Bogota
Colombia,Cali
Colombia,Medellin
Colombia,Bucaramanga
Colombia,Pereira
Colombia,Ibagué
Colombia,Barranquilla
Chile,Santiago
Chile,La Serena
Chile,V Región
Chile,Concepción
Peru,Lima
Mexico,Ciudad de México
Mexico,Monterrey
Mexico,Guadalajara
Mexico,Saltillo
Mexico,Villahermosa
Mexico,Aguascalientes
Mexico,León
Mexico,Durango
Mexico,La Laguna
Mexico,Querétaro
Mexico,Toluca
Argentina,Buenos Aires"
  ,
  header = TRUE,sep="," ,
  stringsAsFactors = TRUE)

#function for rounding numbers to k or m
form_num <- function(tx) { 
  div <- findInterval(as.numeric(gsub("\\,", "", tx)), 
                      c(0, 1e3, 1e6, 1e9, 1e12) )
  paste(round( as.numeric(gsub("\\,","",tx))/10^(3*(div-1)), 2), 
        c("","K","M","B","T")[div] )}

ui <- dashboardPage(
  dashboardHeader(title = "AIG Forecasting Tool",titleWidth = 300),
  dashboardSidebar(width = 300,
                   sidebarMenu(
                     menuItem("Descriptives", tabName = "Descriptives", icon = icon("dashboard")),
                     menuItem("Forecasting Results", tabName = "Forecasting_Results", icon = icon("bar-chart-o")),
                     br(),
                     menuItem("Manual file imports",startExpanded = FALSE,
                        br(),
                        fileInput("data22",label = tags$div(HTML('<i class="fa fa-cloud-upload fa-1x" aria-hidden="true" style = "color:#23d2aa;"></i> Import data',accept = ".xlsx"))),
                        fileInput("holidays",label = tags$div(HTML('<i class="fa fa-upload fa-1x" aria-hidden="true" style = "color:#23d2aa;"></i> Import holidays (if applicable)'))),
                        fileInput("regressors",label = tags$div(HTML('<i class="fa fa-upload fa-1x" aria-hidden="true" margin-bottom = 0px style = "color:#23d2aa;"></i> Import regressors (if applicable)')))
                     ),
                     menuItem("Fetch data from DB",startExpanded = FALSE,
                        br(),
                        uiOutput("Country2"),
                        uiOutput("City2"),
                        dateRangeInput("date_range",label = tags$div(HTML('<i class="fa fa-calendar fa-1x" style = "color:#23d2aa;"></i> Date Range')),start=Sys.Date(),end=Sys.Date()),
                        selectInput("var","Variabe to forecast",c("Rides","Raw_Requests","Sessionized_Requests","Raw_Eyeballs","Sessionized_Eyeballs","NA","PR","ETRC")),
                        radioButtons("period", "Time granularity", c("daily","weekly","hourly"), inline=TRUE)
                     ),
                     actionButton("fetch_data","Fetch Data",icon = icon("refresh"),width = 270, style="color: #333333; background-color: #23d2aa; border-color: #007bb8;font-weight: bold;"),
                     br(),
                     menuItem("Forecasting Advanced Options",startExpanded = FALSE,
                        h3("Seasonalities"),
                        radioButtons("seasonality_mode", "Seasonality Mode", c("multiplicative","additive"), inline=TRUE),
                        splitLayout(cellWidths = c("50%","50%"),
                                    numericInput("daily_prior", "Day Prior Scale", 0.5, min = 0, max = 20, step = 0.1),
                                    numericInput("daily_fourier", "Day Fourier", 3, min = 1, max = 20, step = 1)
                        ),
                        splitLayout(cellWidths = c("50%","50%"),
                                    numericInput("weekly_prior", "Week Prior Scale", 0.5, min = 0, max = 20, step = 0.1),
                                    numericInput("weekly_fourier", "Week Fourier", 5, min = 1, max = 20, step = 1)
                        ),
                        splitLayout(cellWidths = c("50%","50%"),
                                    numericInput("monthly_prior", "Month Prior Scale", 0.5, min = 0, max = 20, step = 0.1),
                                    numericInput("monthly_fourier", "Month Fourier", 5, min = 1, max = 20, step = 1)
                        ),
                        splitLayout(cellWidths = c("50%","50%"),
                                    numericInput("yearly_prior", "Year Prior Scale", 0.5, min = 0, max = 20, step = 0.1),
                                    numericInput("yearly_fourier", "Year Fourier", 10, min = 1, max = 20, step = 1)
                        ),
                        h3("Changepoints"),
                        sliderInput("changepoint_range", "ChangePoint Range",0, 1, 0.8, step = 0.1),
                        numericInput("changepoint_number", "Number of Changepoints",  25, min = 0, max = 1000, step = 1),
                        numericInput("changepoint_prior", "Changepoint Prior Scale", 0.5, min = 0, max = 10, step = 0.1)
                     ),
                     br(),
                     sliderInput("Points_of_Forecast", "Points of Forecast",1, 100, 10, step = 1),
                     actionButton("Go","Run Forecasting",icon = icon("play-circle"),width = 270, style="color: #333333; background-color: #23d2aa; border-color: #007bb8;font-weight: bold;")
                   )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "Descriptives",
              fluidRow(
                valueBoxOutput("num_periods"),
                valueBoxOutput("sums"),
                valueBoxOutput("avg_period")
              ),
              fluidRow(
                highchartOutput("line") %>% withSpinner(type=6,color="#23d2aa"),
                br(),
                downloadButton('downloadData','Download Data'),
                br(),
                DTOutput('table',width = "98%")
              )
      ),
      tabItem(tabName = "Forecasting_Results",
              fluidPage(
                tabsetPanel(
                  tabPanel("Forecast Decomposition", fluidPage(
                    highchartOutput('line2', height = 600,width="100%") %>% withSpinner(type=6,color="#23d2aa"),
                    br(),
                    downloadButton('downloadResults','Download Results'),
                    DTOutput('table2',width = "100%"))),
                    tabPanel("Model Diagnostics", fluidRow(
                      box(title="MAPE",solidHeader = TRUE ,width=6,status = "primary",plotOutput("line3")  %>% withSpinner(type=6,color="#23d2aa")),
                      box(title="MSE",solidHeader = TRUE,width=6, status = "primary",plotOutput("line4")  %>% withSpinner(type=6,color="#23d2aa"))
                    ),
                    fluidRow(
                      box(title="RMSE",solidHeader = TRUE ,width=6,status = "primary",plotOutput("line5")%>% withSpinner(type=6,color="#23d2aa")),
                      box(title="MAE",solidHeader = TRUE ,width=6,status = "primary",plotOutput("line6")%>% withSpinner(type=6,color="#23d2aa"))
                    ),
                    br(),
                    downloadButton('downloadvalid','Download Validation'),
                    splitLayout(cellWidths = c("49%","2%","50%"),
                                DTOutput('table3'),
                                br(),
                                DTOutput('table4')
                    )
                  )
                )
              )
      )
    ),
    
    tags$head(tags$style(HTML("
        section.sidebar .shiny-input-container {
        padding: 1px 15px 0px 15px;
        white-space: normal;
        }
        
        .tabbable > .nav > li > a[data-value='Forecast Decomposition'] {background-color: #333333;   color:white}
        .tabbable > .nav > li > a[data-value='Model Diagnostics'] {background-color: #333333;  color:white}
        .tabbable > .nav > li.active > a[data-value='Forecast Decomposition'] {background-color: #23d2aa;  color:black}
        .tabbable > .nav > li.active > a[data-value='Model Diagnostics'] {background-color: #23d2aa;  color:black}
        
        .small-box.bg-yellow { background-color: #ff6600 !important; color: #fff !important; }
        .small-box.bg-aqua { background-color: #6f6987 !important; color: #fff !important; }
        .small-box.bg-blue { background-color: #9e9bae !important; color: #fff !important; }
        
        .box.box-solid.box-primary>.box-header {
        color:#fff;
        background:#333333
        }
        
                    .box.box-solid.box-primary{
                    border-bottom-color:#333333;
                    border-left-color:#333333;
                    border-right-color:#333333;
                    border-top-color:#333333
                    }

        div.box-header {
          text-align: center;
        }
        
        /* logo */
        .skin-blue .main-header .logo {
                              background-color: #333333;
                              color:#ffffff;
                              }

        /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
                              background-color: #333333;
                              }        

        /* main sidebar */
        .skin-blue .main-sidebar {
                              background-color: #333333;
                              }

        /* active selected tab in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #23d2aa;
                              color: #333333;
                              }

        /* other links in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                              background-color: #333333;
                              color: #ffffff;
                              }

        /* other links in the sidebarmenu when hovered */
         .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-color: #23d2aa;
                              color: #ffffff;
                              }
        ")))
  )
)

server = function(input, output,session) {
  
  #create the dynamic filters for country
  output$Country2 <-renderUI({
    choice_var1 <- reactive({
      filter_data %>%
        pull(unique(Country)) %>%
        as.character()
    })
    selectizeInput('Country', label = tags$div(HTML('<i class="fa fa-globe fa-1x" aria-hidden="true" style = "color:#23d2aa;"></i> Country')) , choices = c("Country"="",choice_var1()))
  })
  
  #create the dynamic filters for city
  output$City2 <- renderUI({
    choice_var2 <- reactive({
      filter_data %>%
        filter(Country == input$Country) %>%
        pull(unique(City)) %>%
        as.character()
    })
    selectizeInput('City', label = tags$div(HTML('<i class="fa fa-flag fa-1x" aria-hidden="true" style = "color:#23d2aa;"></i> City')), choices = c("City" = "", choice_var2())) # <- put the reactive element here
  })
  
  time_level<- eventReactive(input$fetch_data,{
    if (input$period=='daily') {time_level<-'day'} else if (input$period=='weekly') {time_level<-'week'} else {time_level<-'hour'}  
    return(time_level)
  })
  
  #fetching data from db
  data<-eventReactive(input$fetch_data,{
    
    time_level<-time_level()
    
    if(is.null(input$data22))
    {
      
      if (input$Country=="Peru") {tmz <- 'America/Lima'
      cntr<-'PE'} else if (input$Country=="Chile") {tmz <- 'America/Santiago'
      cntr<-'CL'} else if (input$Country=="Colombia") {tmz <- 'America/Bogota'
      cntr<-'CO'} else if (input$Country=="Mexico") {tmz <- 'America/Mexico_City'
      cntr<-'MX'} else if (input$Country=="Argentina") {tmz <- 'America/Buenos_Aires'
      cntr<-'AR'} else if (input$Country=="Greece") {tmz <- 'Europe/Athens'
      cntr<-'GR'}
      
      host1 <- paste0("presto-tbl-",cntr,".taxibeat.com")
      con <-
        dbConnect(
          RPresto::Presto(),
          catalog = "hive",
          schema = "default",
          user = "admin",
          host = host1,
          port = 8080,
          session.timezone = "UTC"
        )
      if (input$var=='Rides') 
      {
        query<-sprintf("select date_trunc('%s',created_local) as date_of, count(distinct id_ride) as Rides 
                          from bi_ride a inner join ot_city cit on a.id_city=cit.id_city
                          where 1=1
                          and date(created_local)>=date('%s')
                          and date(created_local)<=date('%s')
                          and revenue>0
                          and name= '%s' group by 1 order by 1",time_level,format(input$date_range[1]),format(input$date_range[2]),input$City)
      } else if (input$var=='Sessionized_Requests')
      {
        query<-sprintf("select date_trunc('%s',last_timestamp_local) as date_of, count(distinct id_request) as Sessionized_Requests 
                          from request_per_session a inner join ot_city cit on a.city_id=cit.id_city
                          where 1=1
                          and date(last_timestamp_local)>=date('%s')
                          and date(last_timestamp_local)<=date('%s')
                          and name= '%s' group by 1 order by 1",time_level,format(input$date_range[1]),format(input$date_range[2]),input$City)
      } else if (input$var=='Raw_Eyeballs')
      {
        query<-sprintf("select date_trunc('%s',created_at at time zone '%s') as date_of, count(distinct id_batch) as Raw_Eyeballs
                          from ot_fare_snapshot a inner join ot_city cit on a.id_city=cit.id_city
                          where 1=1
                          and date(created_at at time zone '%s')>=date('%s')
                          and date(created_at at time zone '%s')<=date('%s')
                          and name= '%s' group by 1 order by 1",time_level,tmz,tmz,format(input$date_range[1]),tmz,format(input$date_range[2]),input$City)
      } else if (input$var=='Raw_Requests')
      {
        query<-sprintf("select date_trunc('%s',created_at at time zone '%s') as date_of, count(distinct id) as Raw_Requests 
                          from ot_ride_request a inner join ot_city cit on a.id_city=cit.id_city
                          where 1=1
                          and date(created_at at time zone '%s')>=date('%s')
                          and date(created_at at time zone '%s')<=date('%s')
                          and name= '%s' group by 1 order by 1",time_level,tmz,tmz,format(input$date_range[1]),tmz,format(input$date_range[2]),input$City)
      } else if (input$var=='Sessionized_Eyeballs')
      {
        query<-sprintf("select date_trunc('%s',created_at at time zone '%s') as date_of, count(distinct id_batch) as Sessionized_Eyeballs 
                          from eyeball_per_session a inner join ot_city cit on a.id_city=cit.id_city
                          where 1=1
                          and date(created_at at time zone '%s')>=date('%s')
                          and date(created_at at time zone '%s')<=date('%s')
                          and name= '%s' group by 1 order by 1",time_level,tmz,tmz,format(input$date_range[1]),tmz,format(input$date_range[2]),input$City)
      }
      data <- dbSendQuery(con, query)
      data <- as.data.table(dbFetch(data, Inf))
      data<-as.data.table(data)
      if (time_level != 'hour') {data$date_of=as.Date(data$date_of)}
      return(data)
    } else {data <- read.xlsx(input$data22$datapath,1) #FETCHING DATA from file input if imported by the user
    data<-as.data.table(data)
    if (time_level != 'hour') {data$date_of=as.Date(data$date_of)}
    return(data)
    }
  })
  
  #fetching holidays if imported by user
  holidays <- eventReactive(input$fetch_data,{
    if (is.null(input$holidays) == FALSE) {
      holidays <- read.xlsx(input$holidays$datapath,1)
      return(holidays)
    }
  })
  
  #building the modeling data
  data_original_prophet<-eventReactive(input$Go ,{
    data<-data()
    if (is.null(input$holidays) == FALSE) {holidays<-holidays()}
    
    data_original_prophet = data
    data_original_prophet <- dplyr::rename(data_original_prophet, "ds"="date_of", "y"=input$var)
    
    #outliers detection
    #initiate the empty outliers data table and fill it by running outlier detection per month in our data
    data_original_prophet$month_year<-paste0(format(data_original_prophet$ds,"%m"),'_',format(data_original_prophet$ds,"%y"))
    outliers_data<-data.table(V1=numeric())
    for (i in unique(data_original_prophet$month_year)) {
      sd1 <- sd(data_original_prophet$y[data_original_prophet$month_year==i])
      mean1<- mean(data_original_prophet$y[data_original_prophet$month_year==i])
      outliers = as.data.table((abs(data_original_prophet$y[data_original_prophet$month_year==i] - mean1))/sd1)
      outliers_data <- rbind(outliers_data,outliers)
    }
    
    data_original_prophet<-cbind(data_original_prophet,outliers_data)
    data_original_prophet <- reshape::rename(data_original_prophet, c("V1"="outlier_ratio"))
    # null where outlier ratio > 3 std and no holidays available in this specific date (if holidays exist)
    if (is.null(input$holidays) == FALSE) {
      holidays2<-as.data.table(unique(holidays$ds))
      holidays2<- reshape::rename(holidays2, c("V1"="ds"))
      holidays2$help_col <- 1
      data_original_prophet<-merge(data_original_prophet,holidays2,by="ds",all.x=TRUE)
      data_original_prophet$y<-ifelse(data_original_prophet$outlier_ratio > 3.00 & data_original_prophet$help_col != 1 , NA, data_original_prophet$y)
      data_original_prophet$is_outlier<-ifelse(is.na(data_original_prophet$y)==TRUE , 1 , 0)
    } else {
      data_original_prophet$y<-ifelse(data_original_prophet$outlier_ratio > 3.00 , NA, data_original_prophet$y)
      data_original_prophet$is_outlier<-ifelse(is.na(data_original_prophet$y)==TRUE , 1 , 0)
    }
    
    return(data_original_prophet)  
  })
  
  #here we create the model and tune the model parameters
  model<-eventReactive(input$Go ,{
    #building the model
    data=data()
    data_original_prophet=data_original_prophet()
    
    #setting holidays to null if no holiday file is imported
    if (is.null(input$holidays)) {holidays <- data.table(holiday=character(), ds=character())
    holidays$ds = as.Date(holidays$ds)
    } else {holidays=holidays()}
    
    model <-  prophet(daily.seasonality = F
                      ,weekly.seasonality = F
                      ,yearly.seasonality = F
                      #,growth = 'logistic'
                      ,holidays = holidays
                      ,changepoint.range=input$changepoint_range
                      ,changepoint.prior.scale = input$changepoint_prior
                      ,n.changepoints = input$changepoint_number
    )
    
  #adding daily seasonality if data are at hour level and have more than 48 hours
  if (input$period=='hourly' & nrow(data)>=24*2) {
    model <- add_seasonality(model, name='daily', period=24, fourier.order=input$daily_fourier, prior.scale = input$daily_prior,mode=input$seasonality_mode)} 
  
  #adding weekly seasonality if data have more than 14 days, 14*24 hours
  if (input$period=='daily' & nrow(data)>=7*2) {
    model <- add_seasonality(model, name='weekly', period=7, fourier.order=input$weekly_fourier, prior.scale = input$weekly_prior ,mode=input$seasonality_mode)
  } else if (input$period=='hourly' & nrow(data)>=7*2*24) {
    model <- add_seasonality(model, name='weekly', period=7*24, fourier.order=input$weekly_fourier, prior.scale = input$weekly_prior ,mode=input$seasonality_mode)
  }
  
  #adding monthly seasonality if data have more than 61 days, 61*24 hours, 8 weeks
  if (input$period=='daily' & nrow(data)>=30.5*2) {
    model <- add_seasonality(model, name='monthly', period=30.5, fourier.order=input$monthly_fourier, prior.scale = input$monthly_prior, mode=input$seasonality_mode)
  } else if (input$period=='weekly' & nrow(data)>=8) {
    model <- add_seasonality(model, name='monthly', period=4, fourier.order=input$monthly_fourier, prior.scale = input$monthly_prior, mode=input$seasonality_mode)
  } else if (input$period=='hourly' & nrow(data)>=61*24) {
    model <- add_seasonality(model, name='monthly', period=30.5*24, fourier.order=input$monthly_fourier, prior.scale = input$monthly_prior, mode=input$seasonality_mode)
  }
  
  #adding yearly seasonality if data has more than 2 years of days or 104 weeks or 365,5*24 hours
  if (input$period=='daily' & nrow(data)>=365.5*2) {
    model <- add_seasonality(model, name='yearly', period=365.5, fourier.order=input$yearly_fourier, prior.scale = input$yearly_prior, mode=input$seasonality_mode)
  } else if (input$period=='weekly' & nrow(data)>=104) {
    model <- add_seasonality(model, name='yearly', period=52, fourier.order=input$yearly_fourier, prior.scale = input$yearly_prior,mode=input$seasonality_mode)
  } else if (input$period=='hourly' & nrow(data)>365.5*2*24) {
    model <- add_seasonality(model, name='yearly', period=365.5*24, fourier.order=input$yearly_fourier, prior.scale = input$yearly_prior,mode=input$seasonality_mode)
  }
    
  # model<-add_regressor(model,"incentives")
    
    #fitting the model
    model<-fit.prophet(model,data_original_prophet)
    
    return(model)
  })
  
  cross_valid <- eventReactive(input$Go ,{
    model=model()
    data=data()
    initial_thr<-round(0.7*nrow(data),0)
    if(input$period=='daily') { period_thr=7
    horizon_thr=30
    units_thr='days'
    } else if (input$period=='weekly') {period_thr=1
    horizon_thr=5
    units_thr='weeks'
    } else {input$period_thr=2
    horizon_thr=48
    units_thr='hours'
    }
    
    #cross-validation: e.g. initial=400 (days for training data) , horizon =30 (days to forcast), period=30 (every 30 days), 
    cross_valid <- cross_validation(model, initial = initial_thr, period = period_thr , horizon = horizon_thr, units = units_thr)
    
    #plot the diagnostics
    # plot_cross_validation_metric(cross_valid, metric = 'mape')
    return(cross_valid)
  })
  
  validation_diagnostics <- eventReactive(cross_valid(),{
    cross_valid=cross_valid()
    validation_diagnostics <- performance_metrics(cross_valid,rolling_window = 0)
    return(validation_diagnostics)
  })
  
  results <- eventReactive(input$Go ,{
    model=model()
    data=data()
    data_original_prophet=data_original_prophet()
    future_data <- as.data.table(make_future_dataframe(model, periods = input$Points_of_Forecast))
    future_data$ds <- as.Date(future_data$ds)
    #future_data$cap=500000
    forecast <- as.data.table(predict(model, future_data))
    forecast$ds<-as.Date(forecast$ds)
    
    #combine results in one table
    data33 <- dplyr::rename(data, "ds"="date_of", "y"=input$var)
    results<- as.data.table(sqldf::sqldf('select a.ds,d.y,c.yhat
                                    ,d.y-c.yhat as residuals
                                    ,(d.y/c.yhat -1)*100 as prcnt_error
                                    ,abs((d.y/c.yhat -1) * 100) as abs_prcnt_error
                                    ,b.outlier_ratio,b.is_outlier,c.*
                                    from future_data a
                                    left join data_original_prophet b on a.ds=b.ds
                                    left join forecast c on a.ds=c.ds
                                    left join data33 d on a.ds=d.ds
                                     '))
    return(results)
  })
  
  #value box with number of periods in dataset
  output$num_periods <- renderValueBox({
    data33<-data()
    time_level<-time_level()
    valueBox(paste0("Period: ",form_num(nrow(data33))), subtitle = paste0(time_level,"s"), 
             icon = icon("calendar-alt"),color='yellow')
  })
  
  #value box with sum of total period target VARIABLE
  output$sums <- renderValueBox({
    data33<-data()
    data33<-as.data.frame(data33)
    vv<-input$var
    sum_var<- sum(data33[vv])
    valueBox(paste0("Total: ",form_num(sum_var)), subtitle = input$var, 
             icon = icon("car"),color='aqua')
  })
  
  #value box with avg VARIABLE per period selected
  output$avg_period <- renderValueBox({
    data33<-data()
    data33<-as.data.frame(data33)
    vv<-input$var
    avg_var<- round(sum(data33[vv])/nrow(data33),2)
    valueBox(paste0("Average ",input$period,": ",form_num(avg_var)), subtitle = input$var, 
             icon = icon("rocket"),color='blue')
  })
  
  #graph with original data
  output$line<-renderHighchart({
    data2 <- as.xts.data.table(data())
    highchart(type = "stock") %>%
      hc_chart(zoomType = "xy") %>%
      hc_add_series(data2[,eval(input$var)],name= input$var, type = "line",color = "#23d2aa") %>%
      hc_title(text=paste0(input$period," ",input$var," - ",input$City),align="center")
      #hc_add_theme(hc_theme_chalk())
  })
  
  #table of original data
  output$table <-DT::renderDataTable({
    data3<-data()
    if (is.null(input$holidays) == FALSE) {
      holidays3<-holidays()
      if (input$period != 'weekly') {
        data3<- sqldf::sqldf("select a.*,b.holiday from data3 a left join holidays3 b on date(a.date_of)=date(b.ds)")
      } else {data3<- sqldf::sqldf("select a.*,b.holiday from data3 a left join holidays3 b on date(a.date_of)=date_trunc('week',b.ds)")}
    }
    return(data3)
  }, options=list(scrollX = TRUE,columnDefs = list(list(className = 'dt-center',targets="_all"))), rownames = FALSE
  , filter = list(position = 'top', clear = FALSE))
  
  #forecasting decomposition graph
  output$line2<-renderHighchart({
    results<-results()
    data2<-results[,c("ds","y","yhat","trend","residuals")]
    data2 <-  as.xts.data.table(data2)
    highchart(type = "stock") %>%
      hc_chart(zoomType = "xy") %>%
      #hc_plotOptions(dataGrouping="false") %>%
      hc_add_series(round(data2$yhat,0),name='Prediction', type = "line",color = "#23d2aa") %>%
      hc_add_series(data2$y, name='Actual', type = "line",color = "#6f6987") %>%
      hc_add_series(round(data2$residuals,0), name='Error', type = "column",color = "red") %>%
      hc_add_series(round(data2$trend,0),name='Trend', type = "area", color = "#9e9bae", fillOpacity = 0.3 ) %>%
      hc_title(text="Forecast Decomposition",align="center") #%>%
    #hc_add_theme(hc_theme_elementary())
  })
  
  ##table output of forecasting results
  output$table2 <- DT::renderDataTable({
    return(results())
  },  options=list(scrollX = TRUE,columnDefs = list(list(className = 'dt-center',targets="_all"))), rownames = FALSE
  , filter = list(position = 'top', clear = FALSE)
  )
  
  output$downloadData <- downloadHandler(
    filename = function() {paste0(input$City,"_Raw_",input$period,"_",input$date_range[1],"_",input$date_range[2],"_",input$var,".xlsx")},
    content = function(file) {
      write.xlsx(data()[input[["table_rows_all"]], ],file,row.names=FALSE)
    }
  )
  
  output$downloadResults <- downloadHandler(
    filename = function() {paste0(input$City,"_Results_",input$period,"_",input$date_range[1],"_",input$date_range[2],"_",input$var,".xlsx")},
    content = function(file) {
      # write.xlsx(results(),file,row.names = FALSE)
      write.xlsx(results()[input[["table2_rows_all"]], ],file,row.names=FALSE)
    }
  )
  
  #download validation results
  output$downloadvalid <- downloadHandler(
    filename = function() {paste0(input$City,"_Validation_",input$period,"_",input$date_range[1],"_",input$date_range[2],"_",input$var,".xlsx")},
    content = function(file) {
      write.xlsx(cross_valid(),file,row.names = FALSE)
    }
  )
  
  #plotting mape
  output$line3 <- renderPlot({
    cross_valid = cross_valid()
    plot_cross_validation_metric(cross_valid, metric = 'mape')
  })
  
  #plotting mse
  output$line4 <- renderPlot({
    cross_valid = cross_valid()
    plot_cross_validation_metric(cross_valid, metric = 'mse')
  })
  
  #plotting rmse
  output$line5 <- renderPlot({
    cross_valid = cross_valid()
    plot_cross_validation_metric(cross_valid, metric = 'rmse')
  })
  
  #plotting MAE
  output$line6 <- renderPlot({
    cross_valid = cross_valid()
    plot_cross_validation_metric(cross_valid, metric = 'mae')
  })
  
  ##table output of validation forecasts
  output$table3 <- DT::renderDataTable({
    return(cross_valid())
  },  options=list(columnDefs = list(list(className = 'dt-center',targets="_all"))), rownames = FALSE
  , filter = list(position = 'top', clear = FALSE)
  )
  
  ##table output of validation metrics
  output$table4 <- DT::renderDataTable({
    return(validation_diagnostics())
  },  options=list(columnDefs = list(list(className = 'dt-center',targets="_all"))), rownames = FALSE
  , filter = list(position = 'top', clear = FALSE)
  )
  
}

shinyApp(ui, server)
