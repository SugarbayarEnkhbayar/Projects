### Packages
library(shiny)
library(shinydashboard)
library(shinythemes)
library(DT)
library(dplyr)
library(shinyWidgets)
library(tidyverse)
library(shinycssloaders)
library(caret)
library(e1071)
library(plotly)
library(plot3D)
library(rgl)
library(kernlab)
library(Rcpp)

### Function - this function can change data type fo chosen column
ChangeType=function(df,var,typecol){
  if(typecol=='Date'){
    df=df %>% mutate(across(var,as.Date))
  } else if(typecol=='Numeric'){
    df=df %>% mutate(across(var,as.numeric))
  }else if(typecol=='Integer'){
    df=df %>% mutate(across(var,as.integer))
  }else if(typecol=='Factor'){
    df=df %>% mutate(across(var,as.factor))
  }else if(typecol=='Character'){
    df=df %>% mutate(across(var,as.character))
  }
  return(df)
}

### Function - this function remove duplicate rows of chosen column
RemDup<-function(df,allow){
  if(allow=='Yes'){
    df<-unique(df)
  } else if(allow=='No'){
    df<-df
  }
  return(df)
}

### Function - this function show us different type of charts
ONEPLT<-function(df,typeplt){
  if(typeplt=='Line'){
    res=plot_ly(data = df,y = ~df[,1],type = "scatter",mode = "lines")
  } else if(typeplt=='Bar'){
    dfpl=df %>% group_by(df[,1]) %>% summarize(count=n())
    colnames(dfpl)<-c('type','count')
    res=plot_ly(x = dfpl$count, y = dfpl$type, type = 'bar', orientation = 'h')
  } else if(typeplt=='Pie'){
    dfpl=df %>% group_by(df[,1]) %>% summarize(count=n())
    colnames(dfpl)<-c('type','count')
    res=plot_ly(labels = dfpl$type, values = dfpl$count, type = 'pie')
  }
  return(res)
}


### Install packages. We used some function from our created package.
### Below code will install our created package from Marwan folder
#install.packages("C:/Users/se456296/Desktop/AutoDataCleanDashboard/Marwan/DataCleaningPackage/datacleaningtoolbox_0.1.0.tar.gz",repos = NULL)
# We used following functions from our created package-ttest, chisquared, correlation, filterdata, removecolumn
library(datacleaningtoolbox)

### We used Cplus for 3 cases. Calculating mean, max, and min value of chosen column
cppFunction('double CplusMean(NumericVector x) {
                return mean(x);
             }')
cppFunction('double CplusMax(NumericVector x) {
                return max(x);
             }')
cppFunction('double CplusMin(NumericVector x) {
                return min(x);
             }')