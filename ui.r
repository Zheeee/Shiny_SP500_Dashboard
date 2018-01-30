shinyUI(dashboardPage (
  dashboardHeader(title = 'S&P 500 Dashboard'),
  dashboardSidebar(
    sidebarUserPanel('ZheYang',
                     image = 'https://avatars3.githubusercontent.com/u/32778947?s=40&v=4'),
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
                    h4(tags$b('Treemap to visulization the return and volume of Sector')),
                    'In this plot: red color means lost, green color means profit.',
                    htmlOutput('tree')),
                tabBox(title = tagList(shiny::icon("bar-chart"), "Indicators"), width = 4,
                    tabPanel('Indicators', 'Compare different indicators in the same sector.',
                             plotlyOutput('bar')),
                    tabPanel('Correlation', 'Correlation in the same sector',
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
                               min = 0 , max = 3, value = 0),
                   sliderInput('ma', 'Moving-average',
                               min = 0 , max = 3, value = 0)),
               tabBox(title = tagList(shiny::icon("line-chart"), "Time Series"), width = 9,
                      tabPanel('Price',
                               plotOutput('fit_stock')),
                      tabPanel('Model',
                               fluidRow(
                                 box(h4(tags$b('ACF')), solidHeader = T, status = 'success',
                                     'Auto correlation between time series and its own lag',
                                     plotOutput('acf')),
                                 box(h4(tags$b('PACF')), solidHeader = T, status = 'success',
                                     'Conditional auto correlation between time seires and its own lag',
                                     plotOutput('pacf'))
                               )),
                      tabPanel('Result',
                               fluidRow(
                                 box('Residuals',
                                     plotOutput('resid')),
                                 box('Residuals Distribution',
                                     plotOutput('resid_dis')),
                                 tabBox(title = tagList(shiny::icon("info"), "Test Result"),
                                   tabPanel('Ljung-Box Test',
                                            textOutput('lbt')),
                                   tabPanel('Shapiro Test',
                                            textOutput('spt'))
                                     ),
                                 box('Hist Bins input:',
                                     sliderInput('resid_hist', 'Bins',
                                                 min = 20, max = 60, value = 30))
                               )),
                      tabPanel('Forecast',
                               fluidRow(
                                 box('Forecast',
                                     plotOutput('fore')),
                                 box('Plot fitted data with price',
                                     plotOutput('f_v_d')),
                                 box('Accuracy',
                                     tableOutput('accu'))
                               ))) 
      )
    ))
)))
