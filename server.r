shinyServer(function(input, output, session){
  output$table = DT::renderDataTable({
    datatable(stocks_w_sec, rownames = F)
  })
  
  observe({
    st = na.omit(unique(stocks_w_spy$Name[stocks_w_sec$Sector == input$sector]))
    updateSelectInput(
      session, 'price1',
      choices = st,
      selected = st[1])
  })
  
  indict_selected = reactive({
    indict_w_sec %>%
      filter(., Sector == input$sector) %>% 
      select(., -Sector, -Price.Book, -Price.Sales)
  })
  
  cor_selected = reactive({
    stocks_w_sec %>% 
      select(., Date, Name, Close, Sector) %>%
      filter(., Sector == input$sector) %>% 
      select(., -Sector) %>% 
      spread(., key = Name, value = Close) %>% 
      mutate(., SPY = spy$Close) %>% 
      select(., -Date) %>% 
      cor(.) %>% 
      round(., 3)
  })
  
  stock_selected1 = reactive({
    stocks_w_spy %>% 
      filter(., Name == input$price1) %>% 
      select(., Date, Open, High, Low, Close) %>%
      na.omit(.) %>% 
      mutate(., SMA = SMA(Close, n = input$sma)) %>% 
      mutate(., EMA = EMA(Close, n = input$ema)) %>% 
      column_to_rownames(.)
  })
  
  stock_selected2 = reactive({
    stocks_w_spy %>% 
      filter(., Name == input$price2) %>% 
      select(., Date, Open, High, Low, Close) %>%
      na.omit(.) %>% 
      mutate(., SMA = SMA(Close, n = input$sma)) %>% 
      mutate(., EMA = EMA(Close, n = input$ema)) %>% 
      column_to_rownames(.)
  })
  
  fit_data = reactive({
    stocks_w_spy %>% 
      filter(., Name == input$price1) %>% 
      select(., Date, Close) %>%
      na.omit(.) %>% 
      column_to_rownames(.)
  })
  
  fit_model = reactive({
    stocks_w_spy %>% 
      filter(., Name == input$price1) %>% 
      select(., Date, Close) %>%
      slice(., 1:200) %>% 
      na.omit(.) %>% 
      column_to_rownames(.) %>% 
      as.ts(.) %>% 
      Arima(., order = c(input$ar, input$diff, input$ma))
  })
  
  output$tree = renderGvis({
    gvisTreeMap(ret_day,
                idvar = 'Name', parentvar = 'Parent', 
                sizevar = 'Volume', colorvar = 'day_ret',
                options = list(
                  showScale = T,
                  highlightOnMouseOver = T,
                  height = 350,
                  maxDepth = 1,
                  maxPostDepth = 2,
                  minColor = 'red',
                  maxColor = 'green'
                ))
  })
  
  output$price = renderDygraph({
    dygraph(stock_selected1(), main = input$price1, group = 'my_stocks') %>%
      dyCandlestick() %>%
      dyLegend(show = 'follow', hideOnMouseOut = T) %>% 
      dyRangeSelector(retainDateWindow = T) 
  })
  
  output$spy = renderDygraph({
    dygraph(stock_selected2(), main = input$price2, group = 'my_stocks') %>% 
      dyCandlestick() %>% 
      dyLegend(show = 'always', hideOnMouseOut = T) %>% 
      dyRangeSelector(retainDateWindow = T)
  })
  
  output$bar = renderPlotly({
      plot_ly(x = indict_selected()[,'Symbol'], 
              y = indict_selected()[, input$indict], type = 'bar' ) %>% 
      layout(title = "",
             xaxis = list(title = ""),
             yaxis = list(title = input$indict))
  })
  
  output$corr = renderPlotly({
      plot_ly(x = rownames(cor_selected()), y = colnames(cor_selected()),
              z = cor_selected(), type = 'heatmap')
  })
  
  output$fit_stock = renderPlot({
    ggplot(data = fit_data(), aes(x = 1:nrow(fit_data()), y = Close)) +
      geom_line() + xlab('Time Periods') + ggtitle(input$price1)
  })
  
  output$diff = renderPlot({
    ggplot() + geom_line(aes(x = 1:length(diff(ts(fit_data()), differences = input$diff)),
                             y = diff(ts(fit_data()), differences = input$diff))) +
      xlab('periods') + ylab('After Diff')
  })
  
  output$acf = renderPlot({
    Acf(diff(ts(fit_data()), differences = input$diff), main = '')
  })
  
  output$pacf = renderPlot({
    Pacf(diff(ts(fit_data()), differences = input$diff), main = '')
  })
  
  output$resid = renderPlot({
    ggplot() + geom_line(aes(x = 1:200, y = residuals(fit_model()))) +
      ylab('Residuals') + xlab('periods')
  })
  
  output$resid_dis = renderPlot({
    ggplot(data = data.frame(res = residuals(fit_model())), aes(x = res)) + 
      geom_histogram(aes(y = ..density..), 
                     bins = input$resid_hist, col = 'red', alpha = 0.5) + 
      geom_density(aes(y = ..density..), col = 'yellow')
  })
  
  output$lbt = renderPrint({
    checkresiduals(fit_model(), plot = F)
  })
  
  output$fore = renderPlot({
    plot(forecast(fit_model(), h = 50))
    lines(fit_data()[1:250,])

  })
  
  output$accu = renderTable({
      accuracy(forecast(fit_model(), h = 50), fit_data()[201:250,])
  })
  
  output$f_v_d = renderPlot({
    ggplot() +
      geom_line(aes(x = 1:200, y = fit_data()[1:200,])) +
      geom_line(aes(x = 1:length(fitted(fit_model())),
                    y = fitted(fit_model())),
                col = 'red',
                inherit.aes = F)
  })
})