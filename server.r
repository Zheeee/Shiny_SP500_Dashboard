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
  
  d = reactive({
    url = input$url
    webpage = read_html(url)
    boxx = html_node(webpage, '#articleText')
    boxx_text = html_text(boxx)
    test = trimws(boxx_text)
    myCorpus = Corpus(VectorSource(test))
    myCorpus = tm_map(myCorpus, content_transformer(tolower))
    myCorpus = tm_map(myCorpus, removePunctuation)
    myCorpus = tm_map(myCorpus, removeNumbers)
    myCorpus = tm_map(myCorpus, removeWords, c(stopwords("english"), 'free', 'report', 'stock'))
    dtm <- TermDocumentMatrix(myCorpus)
    m <- as.matrix(dtm)
    v <- sort(rowSums(m),decreasing=TRUE)
    d <- data.frame(word = names(v),freq=v)
  })
  
  tree_sec = reactive({
    sector_data %>% 
      mutate(., day_ret = (close - open) / open * 100) %>% 
      select(., Name = name, Date = timestamp, Volume = volume, day_ret) %>% 
      filter(., is.null(day_ret) == F) %>% 
      mutate(., Sector = 'S&P500') %>% 
      filter(., as.character(Date) == as.character(input$date)) %>% 
      mutate(., Parent_f = factor(Sector), Name_f = factor(Name)) %>% 
      select(., Name_f, Parent_f, Volume, day_ret, -c(Name, Sector)) %>% 
      rename(., Name = Name_f, Parent = Parent_f)
  })
  
  tree_stock = reactive({
     stocks_w_sec %>% 
       arrange(., desc(Date)) %>% 
       mutate(., day_ret = (Close - Open) / Open * 100) %>% 
       select(., Name, Sector, Volume, day_ret, Date) %>% 
       filter(., is.null(day_ret) == F) %>% 
       na.omit(.) %>% 
       filter(., as.character(Date) == as.character(input$date)) %>% 
       select(., -Date) %>% 
       rbind(., df_add) %>% 
       rename(., Parent = Sector) %>% 
       arrange(., Parent) %>% 
       mutate(., Parent_f = factor(Parent), Name_f = factor(Name)) %>% 
       select(., Name_f, Parent_f, Volume, day_ret, -c(Name, Parent)) %>% 
       rename(., Name = Name_f, Parent = Parent_f)
  })
  
  ret_day = reactive({
    rbind(tree_sec(), tree_stock()) %>% 
      arrange(., Parent)
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
      mutate(., SMA1 = SMA(Close, n = input$sma1)) %>% 
      mutate(., SMA2 = SMA(Close, n = input$sma2)) %>% 
      column_to_rownames(.)
  })
  
  stock_selected2 = reactive({
    if (input$price2 != '') {
      stocks_w_spy %>% 
        filter(., Name == input$price2) %>% 
        select(., Date, Open, High, Low, Close) %>%
        na.omit(.) %>% 
        mutate(., SMA1 = SMA(Close, n = input$sma1)) %>% 
        mutate(., SMA2 = SMA(Close, n = input$sma2)) %>% 
        column_to_rownames(.)
    } else {
      stocks_w_spy %>% 
        filter(., Name == "SPY") %>% 
        select(., Date, Open, High, Low, Close) %>%
        na.omit(.) %>% 
        mutate(., SMA1 = SMA(Close, n = input$sma1)) %>% 
        mutate(., SMA2 = SMA(Close, n = input$sma2)) %>% 
        column_to_rownames(.)
    }
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
  
  output$word = renderPlot({
    wordcloud(words = d()$word, freq = d()$freq, scale = c(4, 0.5), random.order = F,
              min.freq = 1, max.words=300, colors=brewer.pal(8, "Dark2"),
              rot.per = 0.5)
  })
  
  output$tree = renderGvis({
    gvisTreeMap(ret_day(),
                idvar = 'Name', parentvar = 'Parent', 
                sizevar = 'Volume', colorvar = 'day_ret',
                options = list(
                  showScale = T,
                  highlightOnMouseOver = T,
                  height = 300,
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
    if (input$price2 != '') {
      dygraph(stock_selected2(), main = input$price2, group = 'my_stocks') %>% 
        dyCandlestick() %>% 
        dyLegend(show = 'always', hideOnMouseOut = T) %>% 
        dyRangeSelector(retainDateWindow = T)
    } else {
      dygraph(stock_selected2(), main = 'SPY', group = 'my_stocks') %>% 
        dyCandlestick() %>% 
        dyLegend(show = 'always', hideOnMouseOut = T) %>% 
        dyRangeSelector(retainDateWindow = T)
    }
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
    if (input$diff == 0) {
      ggplot(data = fit_data(), aes(x = 1:nrow(fit_data()), y = Close)) +
        geom_line() + xlab('Time Period') + 
        ggtitle(input$price1) + theme_bw()
    } else {
      ggplot() + geom_line(aes(x = 1:length(diff(ts(fit_data()), differences = input$diff)),
                               y = diff(ts(fit_data()), differences = input$diff))) +
        xlab('Time Period') + ylab('After Diff') + 
        ggtitle(input$price1) + theme_bw()
    }
  })
  
  output$acf = renderPlot({
    if (input$diff == 0) {
      Acf(fit_data()[1:200,], main = '')}
    else {
      Acf(diff(ts(fit_data()), differences = input$diff), main = '')
    }
  })
  
  output$pacf = renderPlot({
    if (input$diff == 0) {
      pacf(fit_data()[1:200,], main = '')}
    else {
      pacf(diff(ts(fit_data()), differences = input$diff), main = '')
    }
  })
  
  output$resid = renderPlot({
    ggplot() + geom_point(aes(x = 1:200, 
                              y = residuals(fit_model())), alpha = 0.7) +
      geom_smooth(aes(x = 1:200, y = residuals(fit_model()))) +
      ylab('Residuals') + xlab('period')
  })
  
  output$resid_dis = renderPlot({
    ggplot(data = data.frame(res = residuals(fit_model())), aes(x = res)) + 
      geom_histogram(aes(y = ..density..), 
                     bins = input$resid_hist, 
                     col = 'red', alpha = 0.5, fill = 'red') + 
      geom_density(aes(y = ..density..), col = 'blue')
  })
  
  output$lbt = renderPrint({
    checkresiduals(fit_model(), plot = F)
  })
  
  output$spt = renderPrint({
    shapiro.test(residuals(fit_model()))
  })
  
  output$fore = renderPlot({
    plot(forecast(fit_model(), h = 50))
    lines(fit_data()[1:250,])
    abline(h = forecast(fit_model(), h = 50)[[6]][50,1], lty = 'dashed', col = 'red') + 
      text(x = 200, 
           y = forecast(fit_model(), h = 50)[[6]][50,1] - 1, 
           labels = round(forecast(fit_model(), h = 50)[[6]][50,1], 2), cex = 0.8) + 
      text(x = 175, 
           y = forecast(fit_model(), h = 50)[[6]][50,1] - 1, 
           labels = '80%:', cex = 0.8)
    abline(h = forecast(fit_model(), h = 50)[[6]][50,2], lty = 'dotdash', col = 'blue') +
      text(x = 200,
           y = forecast(fit_model(), h = 50)[[6]][50,2] - 1,
           labels = round(forecast(fit_model(), h = 50)[[6]][50,2], 2), cex = 0.8) +
      text(x = 175,
           y = forecast(fit_model(), h = 50)[[6]][50,2] - 1,
           labels = '95%:', cex = 0.8)
    abline(h = forecast(fit_model(), h = 50)[[5]][50,1], lty = 'dashed', col = 'red') + 
      text(x = 200, 
           y = forecast(fit_model(), h = 50)[[5]][50,1] + 1, 
           labels = round(forecast(fit_model(), h = 50)[[5]][50,1], 2), cex = 0.8) + 
      text(x = 175, 
           y = forecast(fit_model(), h = 50)[[5]][50,1] + 1, 
           labels = '80%:', cex = 0.8)
    abline(h = forecast(fit_model(), h = 50)[[5]][50,2], lty = 'dotdash', col = 'blue') +
      text(x = 200,
           y = forecast(fit_model(), h = 50)[[5]][50,2] + 1,
           labels = round(forecast(fit_model(), h = 50)[[5]][50,2], 2), cex = 0.8) +
      text(x = 175,
           y = forecast(fit_model(), h = 50)[[5]][50,2] + 1,
           labels = '95%:', cex = 0.8)

  })
  
  output$accu = renderTable({
      accuracy(forecast(fit_model(), h = 50), fit_data()[201:250,])
  })
  
  output$f_v_d = renderPlot({
    ggplot() +
      geom_line(aes(x = 1:200, y = fit_data()[1:200,])) +
      geom_line(aes(x = 1:length(fitted(fit_model())),
                    y = fitted(fit_model())), col = 'red',
                inherit.aes = F) +
      ggtitle(input$price1) + xlab('Time Period') + ylab('Price') +
      theme_bw()
  })
})