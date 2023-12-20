library(shiny)
library(tidyverse)
library(factoextra)
library(flexclust) 
library(FeatureImpCluster) 
df_all=readRDS("df_all.RDS")
df_all=df_all[-2]
df_all<-data.frame(t(df_all))
colnames(df_all)=df_all[1,]
df_all<-df_all[-1,]
df_all <- df_all[, !duplicated(colnames(df_all))]
df_all<-unique(df_all)
df_all<-df_all %>% select(-`Address:`,-`Gorod:`,-Oblast,-`Strana:`)
for (i in 1:length(df_all)) {
  df_all[,i]=gsub("[^[:digit:]., ]", "", df_all[,i])
  df_all[,i]=gsub("[][!#$%()*,:;<=>@^_`|~{}]", "", df_all[,i])
  df_all[,i]=gsub(" ", "", df_all[,i])
}
df_all$Bathrooms<-as.numeric(df_all$Bathrooms)
df_all$Bedrooms<-as.numeric(df_all$Bedrooms)
df_all$`Floor:`<-as.numeric(df_all$`Floor:`)
df_all$Number<-as.numeric(df_all$Number)
df_all$`Rooms:`<-as.numeric(df_all$`Rooms:`)
df_all$Total<-as.numeric(df_all$Total)
df_all$price<-as.numeric(df_all$price)
df_all$Posted<-as.Date(df_all$Posted,'%d.%m.%Y')
df_all$Updated<-as.Date(df_all$Updated,'%d.%m.%Y')
df_all$Posted_day=Sys.Date()-df_all$Posted
df_all$Updated_day=Sys.Date()-df_all$Updated
df_all=df_all[complete.cases(df_all),]
df_all=df_all %>% select(-Posted,-Updated) # we don't need posted, updated variables anymore. Because we have posted_day and updated_day variables.
df_all$Posted_day<-as.numeric(df_all$Posted_day)
df_all$Updated_day<-as.numeric(df_all$Updated_day)
for (i in 1:9) {
  q1=quantile(df_all[,i], .25)
  q3=quantile(df_all[,i], .75)
  IQR=IQR(df_all[,i])
  count_out<-subset(df_all, df_all[,i] > (q1 - 1.5*IQR) & df_all[,i] < (q3 + 1.5*IQR))
  print(paste0(colnames(df_all)[i]," variable count of outlier - ",count(df_all)-count(count_out)))
}
outliers=df_all[0,]
for (i in 2:9) {
  q1=quantile(df_all[,i], .25)
  q3=quantile(df_all[,i], .75)
  IQR=IQR(df_all[,i])
  count_out<-subset(df_all, df_all[,i] > (q1 - 1.5*IQR) & df_all[,i] < (q3 + 1.5*IQR))
  add_out<-setdiff(df_all,count_out)
  outliers<-rbind(outliers,add_out)
  outliers<-unique(outliers)
}
df_all<-setdiff(df_all,outliers)
df=df_all[c('Bathrooms','Bedrooms','Floor:','Number','price','Rooms:','Total','Posted_day','Updated_day')]
### Data normalized
df$Bathrooms<-scale(df$Bathrooms)
df$Bedrooms<-scale(df$Bedrooms)
df$`Floor:`<-scale(df$`Floor:`)
df$Number<-scale(df$Number)
df$price<-scale(df$price)
df$`Rooms:`<-scale(df$`Rooms:`)
df$Total<-scale(df$Total)
df$Posted_day<-scale(df$Posted_day)
df$Updated_day<-scale(df$Updated_day)
ui <- fluidPage(

    titlePanel("Interactive KMeans model"),
    sidebarLayout(
        sidebarPanel(
          selectInput("kcount_adj", label = "Choose number of cluster:", 
                        choices = c(2,3,4,5,6,7,8,9,10),selected = 2), 
            selectInput('select_dif','choose the method',choices = c('euclidean','manhattan','minkowski','canberra'),selected = 'euclidean')
        ),

        # Show a plot of the generated distribution
        mainPanel(
                column(9,
                       shinycssloaders::withSpinner(plotOutput("plot1")),
                       shinycssloaders::withSpinner(plotOutput("plot2")),
                       shinycssloaders::withSpinner(plotOutput("plot3")),
                       shinycssloaders::withSpinner(plotOutput("plot4")),
                       shinycssloaders::withSpinner(tableOutput('table1')),
                       shinycssloaders::withSpinner(tableOutput('table2')),
                       shinycssloaders::withSpinner(tableOutput('table3'))
        )
    )
))

server <- function(input, output) {
  output$plot1<-renderPlot({
    km1=eclust(df,FUNcluster = 'kmeans',k=input$kcount_adj,graph = F,hc_metric = input$select_dif) 
    fviz_cluster(km1,ellipse.type = 'norm') 
  }) 
  output$plot2<-renderPlot({ 
  km1=eclust(df,FUNcluster = 'kmeans',k=input$kcount_adj,graph = F,hc_metric = input$select_dif) 
  fviz_silhouette(km1)
  })
  output$plot3<-renderPlot({ 
      km=kcca(df, k=input$kcount_adj) 
      FeatureImp_km<-FeatureImpCluster(km, as.data.table(df)) 
      plot(FeatureImp_km) 
    })
  output$plot4<-renderPlot({ 
      d1<-cclust(df,k=input$kcount_adj,dist=input$select_dif) 
      shadow(d1) 
      plot(shadow(d1)) 
    })
    output$table1<-  renderTable({ 
      km1=eclust(df,FUNcluster = 'kmeans',k=input$kcount_adj,graph = F,hc_metric = input$select_dif) 
      km1[2] 
    })
    output$table2<-  renderTable({ 
      km1=eclust(df,FUNcluster = 'kmeans',k=input$kcount_adj,graph = F,hc_metric = input$select_dif) 
      km1[7] 

    })
    output$table3<-  renderTable({ 
      km1=eclust(df,FUNcluster = 'kmeans',k=input$kcount_adj,graph = F,hc_metric = input$select_dif) 
      c1=data.frame(clusts=km1[1]) 
      c1=cbind(c1,df) 
      c1 %>% group_by(cluster) %>% summarize(mean_bath=mean(Bathrooms), 
                                             mean_bed=mean(Bedrooms), 
                                             mean_floor=mean(`Floor:`), 
                                             mean_num=mean(Number), 
                                             mean_price=mean(price), 
                                             mean_room=mean(`Rooms:`), 
                                             mean_total=mean(Total), 
                                             mean_posted=mean(Posted_day), 
                                             mean_updated=mean(Updated_day)) 
    })
}


shinyApp(ui = ui, server = server)
