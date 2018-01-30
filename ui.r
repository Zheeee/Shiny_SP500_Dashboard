shinyUI(dashboardPage (
  dashboardHeader(title = 'S&P 500 stocks'),
  dashboardSidebar(
    sidebarUserPanel('ZheYang'),
    sidebarMenu(
      menuItem('Data', tabName = 'dt', icon = icon('database')),
      menuItem('Treemap', tabName = 'tm', icon = icon('map')),
      menuItem('TimeSeries', tabName = 'ts', icon = icon('line-chart'))
    ),
    selectizeInput('sector', h3('Sectors'), 
                   choices = na.omit(unique(stocks_w_sec$Sector)),
                   selected = 'XLI'),
    selectizeInput('price1', h3('Price1'), 
                   choices = unique(stocks_w_spy$Name),
                   selected = 'MMM'),
    textInput('price2', h3('Price2'),
                   value = 'SPY'),
    selectizeInput('indict', h3('Indicator'), 
                   choices = colnames(indict_w_sec)[3:10],
                   selected = 'Price.Earnings')),
  dashboardBody(
    tabItems(
      tabItem(tabName = 'dt',
              fluidRow(box(DT::dataTableOutput('table'),
                           'Time series data of S&P 500 companies',br(),
                           'Other data including:',br(),
                           'Indicator of S&P500 companies',br(),
                           'Time series data pf Sector ETF price',
                           width = 12, solidHeader = T,
                           status = 'warning'))
              ),
      tabItem(tabName = 'tm',
              fluidRow(
                box(title = 'TreeMap',status = 'info',solidHeader = T,
                    h4('Treemap to visulization the return and volume of Sector'),
                    htmlOutput('tree')),
                tabBox(title = tagList(shiny::icon("bar-chart"), "Indicators"), width = 4,
                    tabPanel('Indicators',
                             plotlyOutput('bar')),
                    tabPanel('Correlation',
                             plotlyOutput('corr'))),
                box(title = 'Stock Price Input',status = 'danger',solidHeader = T,
                    width = 2,
                    'Moving bar to adjust SMA or EMA',
                    sliderInput('sma', h3('SMA'), min = 10,
                                max = 200, value = 30),
                    sliderInput('ema', h3('EMA'), min = 10,
                                max = 200, value = 100)),
                box(title = 'Stock Price Movement',status = 'success',solidHeader = T,
                    width = 6,
                    dygraphOutput('price')),
                box(title = 'S&P500 Price Movement',status = 'success',solidHeader = T,
                    width = 6,
                    dygraphOutput('spy'))
                )),
      tabItem(tabName = 'ts',
             fluidRow( 
               box(title = 'ARIMA(p,d,q)', width = 3, 
                   solidHeader = T, status = 'warning',
                   sliderInput('ar', 'Autoregression',
                               min = 0, max = 3, value = 0),
                   sliderInput('diff', 'Differencing',
                               min = 1 , max = 3, value = 1),
                   sliderInput('ma', 'Moving-average',
                               min = 0 , max = 3, value = 0)),
               tabBox(title = tagList(shiny::icon("line-chart"), "Time Series"), width = 9,
                      tabPanel('Price',
                               plotOutput('fit_stock')),
                      tabPanel('Model',
                               fluidRow(
                                 box('ACF', solidHeader = T, status = 'success',
                                     plotOutput('acf')),
                                 box('PACF', solidHeader = T, status = 'success',
                                     plotOutput('pacf')),
                                 box('DIFF', width = 9,
                                     plotOutput('diff'))
                               )),
                      tabPanel('Result',
                               fluidRow(
                                 box('Residuals',
                                     plotOutput('resid')),
                                 box('Residuals Distribution',
                                     plotOutput('resid_dis')),
                                 box('Ljung-Box test',
                                     textOutput('lbt')),
                                 box('Hist Bins input:',
                                     sliderInput('resid_hist', 'Bins',
                                                 min = 20, max = 60, value = 30))
                               )),
                      tabPanel('Forecast',
                               fluidRow(
                                 box('Forcast',
                                     plotOutput('fore')),
                                 box('Fitted Vs Data',
                                     plotOutput('f_v_d')),
                                 box('Accuracy',
                                     tableOutput('accu'))
                               ))) 
      )
    ))
)))
