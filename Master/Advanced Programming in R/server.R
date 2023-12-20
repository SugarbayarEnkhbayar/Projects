server = function(input, output,session) {
  rv <- reactiveValues(data = NULL) # create reactiveValue of input value. If we change something on our dataset, it will be saved. Then all chart, table, selectinputs will be changed.
  observeEvent(input$fileInput, { # create observeEvent
    file <- input$fileInput # File input to file
    ext <- tools::file_ext(file$datapath)
    req(file)
    rv$data <- read.csv(file$datapath, dec=',') # read data from input file
  })
  output$FirstFile<-renderDT({
    datatable(rv$data,options = list(autowidth = TRUE,scrollX = TRUE))
  })
  output$ROW<-renderInfoBox({
    infoBox('ROW',nrow(rv$data)) # Number of observations
  })
  output$FEATURES<-renderInfoBox({
    infoBox('FEATURES',length(rv$data)) # Number of features or columns
  })
  output$SecondFile<-renderDT({
    datatable(rv$data,options = list(autowidth = TRUE,scrollX = TRUE))
  })
  output$ChanTypFile<-renderDT({
    df=data.frame(sapply(rv$data,class)) # define type of columns and save as df
    colnames(df)<-c('Types') 
    datatable(df) # Column names and their data type
  })
  
  observeEvent(input$click4,{ # When we click on button(button id is click4), below codes will work. If we don't click, below codes will not work.
    df=rv$data %>% filter(is.na(!!!input$MissVal)) %>% as.data.frame() # filter data using selectinput
    output$MissRes<-renderDT({
      datatable(df,options = list(autowidth=T,scrollX=T)) # datatable output. We can change some little part of datatable using options. 
    })
  })
  output$down <- downloadHandler( # Download button
    filename = function() {
      paste0('cleaned_data', ".csv") # File name will be changed
    },
    content = function(file) {
      write.csv(rv$data, file) # File will be download
    }
  )
  observeEvent(rv$data,{ # observeEvent-When we change something on dataframe, varselectInput will be updated. VarselectInput ID is rem_un.
    updateVarSelectInput(session, inputId = "rem_un",data = rv$data,selected = "")
  })
  observeEvent(rv$data,{
    updateVarSelectInput(session, inputId = "change_typ",data = rv$data,selected = "") # observeEvent-When we change something on dataframe, varselectInput will be updated.
  })
  observeEvent(rv$data,{
    updateVarSelectInput(session, inputId = "changeColNam",data = rv$data,selected = "") # observeEvent-When we change something on dataframe, varselectInput will be updated.
  })
  observeEvent(rv$data,{
    updateVarSelectInput(session, inputId = "MissVal",data = rv$data,selected = "") # observeEvent-When we change something on dataframe, varselectInput will be updated.
  })
  observeEvent(rv$data,{
    updateVarSelectInput(session, inputId = "SpecVal",data = rv$data,selected = "") # observeEvent-When we change something on dataframe, varselectInput will be updated.
  })
  observeEvent(rv$data,{
    updateVarSelectInput(session, inputId = "SpecVal_GR",data = rv$data,selected = "") # observeEvent-When we change something on dataframe, varselectInput will be updated.
  })
  observeEvent(rv$data,{
    updateVarSelectInput(session, inputId = "SelColOut",data = rv$data,selected = "") # observeEvent-When we change something on dataframe, varselectInput will be updated.
  })
  observeEvent(rv$data,{
    updateVarSelectInput(session, inputId = "SelColFilt",data = rv$data,selected = "") # observeEvent-When we change something on dataframe, varselectInput will be updated.
  })
  observeEvent(rv$data,{
    updateVarSelectInput(session, inputId = "SelColtt1",data = rv$data,selected = "") # observeEvent-When we change something on dataframe, varselectInput will be updated.
  })
  observeEvent(rv$data,{
    updateVarSelectInput(session, inputId = "SelColtt2",data = rv$data,selected = "") # observeEvent-When we change something on dataframe, varselectInput will be updated.
  })
  observeEvent(rv$data,{
    updateVarSelectInput(session, inputId = "SelColchi1",data = rv$data,selected = "") # observeEvent-When we change something on dataframe, varselectInput will be updated.
  })
  observeEvent(rv$data,{
    updateVarSelectInput(session, inputId = "SelColchi2",data = rv$data,selected = "") # observeEvent-When we change something on dataframe, varselectInput will be updated.
  })
  observeEvent(rv$data,{
    updateVarSelectInput(session, inputId = "SelColMath",data = rv$data,selected = "") # observeEvent-When we change something on dataframe, varselectInput will be updated.
  })
  observeEvent(rv$data,{
    updateVarSelectInput(session, inputId = "SelColCor1",data = rv$data,selected = "") # observeEvent-When we change something on dataframe, varselectInput will be updated.
  })
  observeEvent(rv$data,{
    updateVarSelectInput(session, inputId = "SelColCor2",data = rv$data,selected = "") # observeEvent-When we change something on dataframe, varselectInput will be updated.
  })
  observeEvent(rv$data,{
    updateVarSelectInput(session, inputId = "VarX",data = rv$data,selected = "") # observeEvent-When we change something on dataframe, varselectInput will be updated.
  })
  observeEvent(rv$data,{
    updateVarSelectInput(session, inputId = "VarY",data = rv$data,selected = "") # observeEvent-When we change something on dataframe, varselectInput will be updated.
  })
  observeEvent(rv$data,{
    updateVarSelectInput(session, inputId = "SelOneVarPlot",data = rv$data,selected = "") # observeEvent-When we change something on dataframe, varselectInput will be updated.
  })
  observeEvent(rv$data,{
    updateVarSelectInput(session, inputId = "SelTwoVarPlot",data = rv$data,selected = "") # observeEvent-When we change something on dataframe, varselectInput will be updated.
  })
  observeEvent(rv$data,{
    updateVarSelectInput(session, inputId = "SelThrVarPlot",data = rv$data,selected = "") # observeEvent-When we change something on dataframe, varselectInput will be updated.
  })
  observeEvent(rv$data,{
    updateVarSelectInput(session, inputId = "IntSelVar",data = rv$data,selected = "") # observeEvent-When we change something on dataframe, varselectInput will be updated.
  })
  
  observeEvent(input$click1,{ # when we click on button(ID is click1), following codes will work
    remover <- new("ColumnRemoval", data = rv$data, columns_to_remove = as.character(input$rem_un)) # We used removeColumns function from our created package
    rv$data <- datacleaningtoolbox::removeColumns(remover) # removeColumns function from our created package
  })
  observeEvent(input$click2,{
    rv$data=ChangeType(rv$data,input$change_typ,input$selectInpTyp)
  })
  observeEvent(input$click3,{ # when we click on button(ID is click3), following codes will work
    colnames(rv$data)[which(names(rv$data) == input$changeColNam)] <- input$NewColNam 
  })
  observeEvent(input$click5,{ # filling missing value. We can fill missing value as mean, median, or most frequency values. Or we can also remove it.
    if(input$MissSelect=='Remove'){
      rv$data<-rv$data %>% filter(!is.na(!!!input$MissVal))
    } else if(input$MissSelect=='Mean'){
      val=rv$data %>% filter(!is.na(!!!input$MissVal)) %>% summarize(x=mean(!!!input$MissVal))
      val=as.integer(val$x)
      rv$data <- rv$data %>% mutate(across(!!input$MissVal, ~replace_na(., val)))
    } else if(input$MissSelect=='Median'){
      val=rv$data %>% filter(!is.na(!!!input$MissVal)) %>% summarize(x=median(!!!input$MissVal))
      val=as.integer(val$x)
      rv$data <- rv$data %>% mutate(across(!!input$MissVal, ~replace_na(., val)))
    } else if(input$MissSelect=='Most_Fre'){
      val<-names(which.max(table(rv$data %>% select(!!!input$MissVal))))
      rv$data<-rv$data %>% mutate(across(!!input$MissVal,~replace_na(.,val)))
    }
  })
  observeEvent(input$click6,{
    rv$data=RemDup(rv$data,input$DupliSelect) # remove duplicated rows
  })
  observeEvent(input$click7,{ # calculate mean, median, count, sum of chosen column and group by chosen column
    if(input$SelMM=='Mean'){
      dfGr<-rv$data %>% group_by(!!!input$SpecVal) %>% summarize(Mean=mean(!!!input$SpecVal_GR))
    } else if(input$SelMM=='Median'){
      dfGr<-rv$data %>% group_by(!!!input$SpecVal) %>% summarize(Median=median(!!!input$SpecVal_GR))
    }else if(input$SelMM=='Count'){
      dfGr<-rv$data %>% group_by(!!!input$SpecVal) %>% summarize(Count=n())
    }else if(input$SelMM=='Sum'){
      dfGr<-rv$data %>% group_by(!!!input$SpecVal) %>% summarize(Sum=sum(!!!input$SpecVal_GR))
    }
    output$GroupTable<-renderDT({
      datatable(dfGr,options = list(autowidth=T,scrollX=T))
    })
  })
  observeEvent(input$click8,{ # Remove outliers
    if(input$SelMetOut=='IQR'){ # removing outliers using IQR
      Q1=rv$data %>% summarize(val=quantile(!!!input$SelColOut,0.25)) # calculating quartile 0.25
      Q1=Q1$val
      Q3=rv$data %>% summarize(val=quantile(!!!input$SelColOut,0.75)) # calculating quartile 0.75
      Q3=Q3$val
      IQR=rv$data %>% summarize(val=IQR(!!!input$SelColOut)) # calculating IQR
      IQR=IQR$val
      rv$data<-rv$data %>% filter(!!input$SelColOut>(Q1-1.5*IQR) & !!input$SelColOut<(Q3+1.5*IQR)) # filtering not outlier rows
    } else if(input$SelMetOut=='Z score'){ # removing outliers using Z score
      rv$data<-rv$data %>% mutate(zscore=as.numeric(scale(!!input$SelColOut,center=T,scale=T)))
      rv$data<-rv$data %>% filter(zscore<3 & zscore>-3)
    }
    output$OutTable<-renderDT({
      datatable(rv$data,options=list(autowidth=T,scrollX=T))
    })
  })
  observeEvent(input$click81,{
    mydata <- new("DataCleaning", data = rv$data) # correlation function from our created package
    corUI<-datacleaningtoolbox::correlation(mydata, as.character(input$SelColCor1), as.character(input$SelColCor2))
    output$rescor<-renderUI({
      print(paste0('Cor.Coef:  ',corUI))
    })
  })
  observeEvent(input$click82,{
    df_filter <- new("DataCleaning", data = rv$data) # filterData function from our created package
    df_filter <- filterData(df_filter, as.character(input$SelColFilt), as.integer(input$FiltVal))
    df_filter<-data.frame(df_filter@data)
    df_filter<-df_filter %>% filter(!is.na(!!!input$SelColFilt))
    output$resFilt<-renderDT({
      datatable(df_filter,options=list(autowidth=T,scrollX=T))
    })
  })
  observeEvent(input$click83,{
    mydata <- new("DataCleaning", data = rv$data) # ttest function from our created package
    t=ttest(mydata, as.character(input$SelColtt1), as.character(input$SelColtt2))
    output$resTtest<-renderPrint({
      t
    })
  })
  observeEvent(input$click84,{
    mydata <- new("DataCleaning", data = rv$data) # chisquared function from our created package
    chi=chisquared(mydata, as.character(input$SelColchi1), as.character(input$SelColchi2))
    output$resChitest<-renderPrint({
      chi
    })
  })
  observeEvent(input$click85,{ # we used Cplus code here. Cplus code are in global.R file
    if(input$SelActMath=='Max'){
      df=rv$data %>% filter(!is.na(!!input$SelColMath)) %>% select(!!input$SelColMath)
      x=CplusMax(df[,1])
    } else if(input$SelActMath=='Min'){
      df=rv$data %>% filter(!is.na(!!input$SelColMath)) %>% select(!!input$SelColMath)
      x=CplusMin(df[,1])
    } else if(input$SelActMath=='Mean'){
      df=rv$data %>% filter(!is.na(!!input$SelColMath)) %>% select(!!input$SelColMath)
      x=CplusMean(df[,1])
    }
    output$resstat<-renderUI({
      print(paste0("Result: ",x))
    })
  })
  observeEvent(input$magic,{ # When we click magic button, following codes will work
    if(input$SelModel=='Linear Regression'){ # select linear regression model
      y=rv$data %>% select(!!input$VarY) # select chosen Y variables and save as y
      x <- rv$data[ , which(names(rv$data) %in% input$VarX)] # select chosen X variables and save as X
      df=cbind(y,x) # combine x and y variables
      df <- na.omit(df) # remove omit value
      colnames(df)[1]<-'y' # change column name
      desc1=summary(df) # descriptive statistic
      nums=dplyr::select_if(df, is.numeric) # chose only numeric columns
      desc2=cor(nums) # calculating correaltion coefficients of only numeric columns
      model<-lm(y~.,data=df) # train linear model
      desc4=summary(model) # result of linear model
    } else if(input$SelModel=='Classification'){ # chose classification model
      y=rv$data %>% select(!!input$VarY) # select Y variables and save as y
      x <- rv$data[ , which(names(rv$data) %in% input$VarX)] # select X variables and save as x
      x<-x %>% mutate(across(where(is.numeric), scale)) 
      df=cbind(y,x) # combine x and y
      df <- na.omit(df) # remove na rows
      colnames(df)[1]<-'y'
      desc1=summary(df) # descriptive statistics
      nums=dplyr::select_if(df, is.numeric) # select only numeric columns
      desc2=cor(nums) # calculate correlation between only numeric columns
      parts = createDataPartition(df$y, p = .8, list = F) # split data into train and test
      train = df[parts, ] # split train
      test = df[-parts, ] # split test
      train_control = trainControl(method = "cv", number = 5) # Kfold
      model = train(as.factor(y)~., data = df, method = "svmLinear", trControl = train_control) # train model
      desc4=print(model) # print result of model
    } else if(input$SelModel=='Cluster'){ # chose cluster model
      df <- rv$data[ , which(names(rv$data) %in% input$VarX)] # select columns use for cluster
      df<-df %>% mutate(across(where(is.numeric), scale)) # select numeric columns
      df <- na.omit(df) # remove na rows
      desc1=summary(df) # descriptive statistics
      nums=dplyr::select_if(df, is.numeric) # select only numeric columns
      desc2=cor(nums) # calculate correlation coefficients between only numeric columns
      model <- kmeans(df[!is.na(df)], centers = 2, nstart = 20) # train kmean model
      desc4=print(model) # print result of kmean model
    }
    output$ui1<-renderPrint({
      print('summary of dataset')
      desc1
    })
    output$ui2<-renderPrint({
      print('correlation of dataset')
      desc2
    })
    output$ui3<-renderPrint({
      print('Summary of model')
      desc4
    })
  })
ONEPLT<-function(df,typeplt){ # functions will show us chosen type of graph
  if(typeplt=='Line'){ # line chart
    res=plot_ly(data = df,y = ~df[,1],type = "scatter",mode = "lines")
  } else if(typeplt=='Bar'){ # bar chart
    dfpl=df %>% group_by(df[,1]) %>% summarize(count=n())
    colnames(dfpl)<-c('type','count')
    res=plot_ly(x = dfpl$count, y = dfpl$type, type = 'bar', orientation = 'h')
  } else if(typeplt=='Pie'){ # pie chart
    dfpl=df %>% group_by(df[,1]) %>% summarize(count=n())
    colnames(dfpl)<-c('type','count')
    res=plot_ly(labels = dfpl$type, values = dfpl$count, type = 'pie')
  }
  return(res)
}
  observeEvent(input$click9,{
    df<-rv$data %>% select(!!input$SelOneVarPlot)
    df<-na.omit(df)
    res=ONEPLT(df,input$OneVarTypePlot)
    output$OnePLT<-renderPlotly({
      res
    })
  })
  observeEvent(input$click10,{
    df <- rv$data[ , which(names(rv$data) %in% input$SelTwoVarPlot)]
    df<-na.omit(df)
    if(input$TwoVarTypePlot=='Scatter'){
      res=plot_ly(data=df,x=~df[,1],y=~df[,2]) # scatter plot between 2 chosen columns
    }
    output$TwoPLT<-renderPlotly({
      res
    })
  })
  observeEvent(input$click11,{
    output$ThrPLT<-renderRglwidget({
      df<-rv$data[,which(names(rv$data) %in% input$SelThrVarPlot)]
      df<-na.omit(df)
      rgl.open(useNULL=T)
      scatter3D(df[,1],df[,2],df[,3],bty='g',pch=20,cex=2,ticktype='detailed') # show 3d graph. But u can see it from only out window
      rglwidget()
    })
  })
  observeEvent(input$click12,{
    df<-rv$data %>% select(!!input$IntSelVar)
    df<-na.omit(df)
    output$IntPLT<-renderPlotly({
      dfpl=df %>% group_by(df[,1]) %>% summarize(count=n())
      colnames(dfpl)<-c('type','count')
      res=plot_ly(x = dfpl$count, y = dfpl$type, type = 'bar', orientation = 'h',source = 'subset')
    })
    output$IntDT<-renderDT({
      event.data=event_data('plotly_click',source = 'subset') # when we click on bar chart, it will show us chosen data
      df=rv$data %>% filter(!!input$IntSelVar==event.data$y)
      df
    })
  })
 }




