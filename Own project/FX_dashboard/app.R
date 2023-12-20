library(shiny)
library(shinydashboard)
library(readxl)
library(tidyverse)
library(dplyr)
library(DT)
library(dygraphs)
library(xts)
library(shinythemes)
library(rsconnect)
library(rhandsontable)

gain<-data.frame(start=200, end=250)
posi<-'https://www.babypips.com/tools/position-size-calculator'
pip_value<-'https://www.babypips.com/tools/pip-value-calculator'
FX_journal<-readRDS("FX_journal.RDS")
FX_journal$Date<-as.Date(FX_journal$Date)
FX_journal$Open_time<-substring(FX_journal$Open_time,12,19)
FX_journal$Close_time<-substring(FX_journal$Close_time,12,19)

ui<-navbarPage(
  theme=shinytheme("sandstone"),'',
  tabPanel('Analysis',
           fluidPage(
             tabBox(width = 12,
               title = '',
               tabPanel('USDJPY',
                        fluidPage(
                          fluidRow(
                            fluidPage(
                              column(width = 1,
                                     box(title = tags$h6(tags$b('Хугацаа')),uiOutput('dateout_USDJPY'))),
                              column(width = 11,
                                     box(title = tags$h6(tags$b('Fundamental sites')), rHandsontableOutput('textout1_USDJPY',width = '100%')))
                            )
                          ),
                          fluidRow(
                            fluidPage(
                              column(3,
                                     box(title = tags$h6(tags$b('Week chart')), uiOutput('textout2_USDJPY'))),
                              column(3,
                                     box(title = tags$h6(tags$b('Day chart')), uiOutput('textout3_USDJPY'))),
                              column(3,
                                     box(title = tags$h6(tags$b('H4 chart')), uiOutput('textout4_USDJPY'))),
                              column(3,
                                     box(title = tags$h6(tags$b('H1 chart')), uiOutput('textout5_USDJPY')))
                            )
                          ),
                          fluidRow(
                            fluidPage(
                              column(2,
                                     box(title = tags$h6(tags$b('Pattern')), uiOutput('textout6_USDJPY'))),
                              column(2,
                                     box(title = tags$h6(tags$b('Indicator')), uiOutput('textout7_USDJPY'))),
                              column(2,
                                     box(title = tags$h6(tags$b('Тэмдэглэгээ')), uiOutput('textout8_USDJPY'))),
                              column(2,
                                     box(title = tags$h6(tags$b('Market maker')), uiOutput('textout9_USDJPY'))),
                              column(4,
                                     box(title = tags$h6(tags$b('Хойшхи бүх цаас')), uiOutput('textout10_USDJPY')))
                            )
                          ),
                          fluidRow(
                            fluidPage(
                              selectInput('type_USDJPY','',choices = c('date','fundamental','week','day','h4','h1','pattern','indicator','note','mm','all_paper')),
                              textInput('input_USDJPY','Текст бич',value = ''),
                              textInput('pass_USDJPY','Нууц үг',value = ''),
                              uiOutput('ui_USDJPY')
                            )
                          ),
                          fluidRow(
                            actionButton('button_USDJPY','Хадгалах')
                          )
                        )),
               tabPanel( 'EURJPY',
                         fluidPage(
                           fluidRow(
                             fluidPage(
                               column(1,
                                      box(title = tags$h6(tags$b('Хугацаа')),uiOutput('dateout_EURJPY'))),
                               column(11,
                                      box(title = tags$h6(tags$b('Fundamental sites')), rHandsontableOutput('textout1_EURJPY',width = '100%')))
                             )
                           ),
                           fluidRow(
                             fluidPage(
                               column(3,
                                      box(title = tags$h6(tags$b('Week chart')), uiOutput('textout2_EURJPY'))),
                               column(3,
                                      box(title = tags$h6(tags$b('Day chart')), uiOutput('textout3_EURJPY'))),
                               column(3,
                                      box(title = tags$h6(tags$b('H4 chart')), uiOutput('textout4_EURJPY'))),
                               column(3,
                                      box(title = tags$h6(tags$b('H1 chart')), uiOutput('textout5_EURJPY')))
                             )
                           ),
                           fluidRow(
                             fluidPage(
                               column(2,
                                      box(title = tags$h6(tags$b('Pattern')), uiOutput('textout6_EURJPY'))),
                               column(2,
                                      box(title = tags$h6(tags$b('Indicator')), uiOutput('textout7_EURJPY'))),
                               column(2,
                                      box(title = tags$h6(tags$b('Тэмдэглэгээ')), uiOutput('textout8_EURJPY'))),
                               column(2,
                                      box(title = tags$h6(tags$b('Market maker')), uiOutput('textout9_EURJPY'))),
                               column(4,
                                      box(title = tags$h6(tags$b('Хойшхи бүх цаас')), uiOutput('textout10_EURJPY')))
                             )
                           ),
                           fluidRow(
                             fluidPage(
                               selectInput('type_EURJPY','',choices = c('date','fundamental','week','day','h4','h1','pattern','indicator','note','mm','all_paper')),
                               textInput('input_EURJPY','Текст бич',value = ''),
                               textInput('pass_EURJPY','Нууц үг',value = ''),
                               uiOutput('ui_EURJPY')
                             )
                           ),
                           fluidRow(
                             actionButton('button_EURJPY','Хадгалах')
                           )
                         )),
               tabPanel( 'GBPJPY',
                         fluidPage(
                           fluidRow(
                             fluidPage(
                               column(1,
                                      box(title = tags$h6(tags$b('Хугацаа')),uiOutput('dateout_GBPJPY'))),
                               column(11,
                                      box(title = tags$h6(tags$b('Fundamental sites')), rHandsontableOutput('textout1_GBPJPY',width = '100%')))
                             )
                           ),
                           fluidRow(
                             fluidPage(
                               column(3,
                                      box(title = tags$h6(tags$b('Week chart')), uiOutput('textout2_GBPJPY'))),
                               column(3,
                                      box(title = tags$h6(tags$b('Day chart')), uiOutput('textout3_GBPJPY'))),
                               column(3,
                                      box(title = tags$h6(tags$b('H4 chart')), uiOutput('textout4_GBPJPY'))),
                               column(3,
                                      box(title = tags$h6(tags$b('H1 chart')), uiOutput('textout5_GBPJPY')))
                             )
                           ),
                           fluidRow(
                             fluidPage(
                               column(2,
                                      box(title = tags$h6(tags$b('Pattern')), uiOutput('textout6_GBPJPY'))),
                               column(2,
                                      box(title = tags$h6(tags$b('Indicator')), uiOutput('textout7_GBPJPY'))),
                               column(2,
                                      box(title = tags$h6(tags$b('Тэмдэглэгээ')), uiOutput('textout8_GBPJPY'))),
                               column(2,
                                      box(title = tags$h6(tags$b('Market maker')), uiOutput('textout9_GBPJPY'))),
                               column(4,
                                      box(title = tags$h6(tags$b('Хойшхи бүх цаас')), uiOutput('textout10_GBPJPY')))
                             )
                           ),
                           fluidRow(
                             fluidPage(
                               selectInput('type_GBPJPY','',choices = c('date','fundamental','week','day','h4','h1','pattern','indicator','note','mm','all_paper')),
                               textInput('input_GBPJPY','Текст бич',value = ''),
                               textInput('pass_GBPJPY','Нууц үг',value = ''),
                               uiOutput('ui_GBPJPY')
                             )
                           ),
                           fluidRow(
                             actionButton('button_GBPJPY','Хадгалах')
                           )
                         )),
               tabPanel( 'AUDJPY',
                         fluidPage(
                           fluidRow(
                             fluidPage(
                               column(1,
                                      box(title = tags$h6(tags$b('Хугацаа')),uiOutput('dateout_AUDJPY'))),
                               column(11,
                                      box(title = tags$h6(tags$b('Fundamental sites')), rHandsontableOutput('textout1_AUDJPY',width = '100%')))
                             )
                           ),
                           fluidRow(
                             fluidPage(
                               column(3,
                                      box(title = tags$h6(tags$b('Week chart')), uiOutput('textout2_AUDJPY'))),
                               column(3,
                                      box(title = tags$h6(tags$b('Day chart')), uiOutput('textout3_AUDJPY'))),
                               column(3,
                                      box(title = tags$h6(tags$b('H4 chart')), uiOutput('textout4_AUDJPY'))),
                               column(3,
                                      box(title = tags$h6(tags$b('H1 chart')), uiOutput('textout5_AUDJPY')))
                             )
                           ),
                           fluidRow(
                             fluidPage(
                               column(2,
                                      box(title = tags$h6(tags$b('Pattern')), uiOutput('textout6_AUDJPY'))),
                               column(2,
                                      box(title = tags$h6(tags$b('Indicator')), uiOutput('textout7_AUDJPY'))),
                               column(2,
                                      box(title = tags$h6(tags$b('Тэмдэглэгээ')), uiOutput('textout8_AUDJPY'))),
                               column(2,
                                      box(title = tags$h6(tags$b('Market maker')), uiOutput('textout9_AUDJPY'))),
                               column(4,
                                      box(title = tags$h6(tags$b('Хойшхи бүх цаас')), uiOutput('textout10_AUDJPY')))
                             )
                           ),
                           fluidRow(
                             fluidPage(
                               selectInput('type_AUDJPY','',choices = c('date','fundamental','week','day','h4','h1','pattern','indicator','note','mm','all_paper')),
                               textInput('input_AUDJPY','Текст бич',value = ''),
                               textInput('pass_AUDJPY','Нууц үг',value = ''),
                               uiOutput('ui_AUDJPY')
                             )
                           ),
                           fluidRow(
                             actionButton('button_AUDJPY','Хадгалах')
                           )
                         )),
               tabPanel( 'NZDJPY',
                         fluidPage(
                           fluidRow(
                             fluidPage(
                               column(1,
                                      box(title = tags$h6(tags$b('Хугацаа')),uiOutput('dateout_NZDJPY'))),
                               column(11,
                                      box(title = tags$h6(tags$b('Fundamental sites')), rHandsontableOutput('textout1_NZDJPY',width = '100%')))
                             )
                           ),
                           fluidRow(
                             fluidPage(
                               column(3,
                                      box(title = tags$h6(tags$b('Week chart')), uiOutput('textout2_NZDJPY'))),
                               column(3,
                                      box(title = tags$h6(tags$b('Day chart')), uiOutput('textout3_NZDJPY'))),
                               column(3,
                                      box(title = tags$h6(tags$b('H4 chart')), uiOutput('textout4_NZDJPY'))),
                               column(3,
                                      box(title = tags$h6(tags$b('H1 chart')), uiOutput('textout5_NZDJPY')))
                             )
                           ),
                           fluidRow(
                             fluidPage(
                               column(2,
                                      box(title = tags$h6(tags$b('Pattern')), uiOutput('textout6_NZDJPY'))),
                               column(2,
                                      box(title = tags$h6(tags$b('Indicator')), uiOutput('textout7_NZDJPY'))),
                               column(2,
                                      box(title = tags$h6(tags$b('Тэмдэглэгээ')), uiOutput('textout8_NZDJPY'))),
                               column(2,
                                      box(title = tags$h6(tags$b('Market maker')), uiOutput('textout9_NZDJPY'))),
                               column(4,
                                      box(title = tags$h6(tags$b('Хойшхи бүх цаас')), uiOutput('textout10_NZDJPY')))
                             )
                           ),
                           fluidRow(
                             fluidPage(
                               selectInput('type_NZDJPY','',choices = c('date','fundamental','week','day','h4','h1','pattern','indicator','note','mm','all_paper')),
                               textInput('input_NZDJPY','Текст бич',value = ''),
                               textInput('pass_NZDJPY','Нууц үг',value = ''),
                               uiOutput('ui_NZDJPY')
                           ),
                           fluidRow(
                             actionButton('button_NZDJPY','Хадгалах')
                           )
                         ))),
               tabPanel( 'GOLD',
                         fluidPage(
                           fluidRow(
                             fluidPage(
                               column(1,
                                      box(title = tags$h6(tags$b('Хугацаа')),uiOutput('dateout_GOLD'))),
                               column(11,
                                      box(title = tags$h6(tags$b('Fundamental sites')), rHandsontableOutput('textout1_GOLD',width = '100%')))
                             )
                           ),
                           fluidRow(
                             fluidPage(
                               column(3,
                                      box(title = tags$h6(tags$b('Week chart')), uiOutput('textout2_GOLD'))),
                               column(3,
                                      box(title = tags$h6(tags$b('Day chart')), uiOutput('textout3_GOLD'))),
                               column(3,
                                      box(title = tags$h6(tags$b('H4 chart')), uiOutput('textout4_GOLD'))),
                               column(3,
                                      box(title = tags$h6(tags$b('H1 chart')), uiOutput('textout5_GOLD')))
                             )
                           ),
                           fluidRow(
                             fluidPage(
                               column(2,
                                      box(title = tags$h6(tags$b('Pattern')), uiOutput('textout6_GOLD'))),
                               column(2,
                                      box(title = tags$h6(tags$b('Indicator')), uiOutput('textout7_GOLD'))),
                               column(2,
                                      box(title = tags$h6(tags$b('Тэмдэглэгээ')), uiOutput('textout8_GOLD'))),
                               column(2,
                                      box(title = tags$h6(tags$b('Market maker')), uiOutput('textout9_GOLD'))),
                               column(4,
                                      box(title = tags$h6(tags$b('Хойшхи бүх цаас')), uiOutput('textout10_GOLD')))
                             )
                           ),
                           fluidRow(
                             fluidPage(
                               selectInput('type_GOLD','',choices = c('date','fundamental','week','day','h4','h1','pattern','indicator','note','mm','all_paper')),
                               textInput('input_GOLD','Текст бич',value = ''),
                               textInput('pass_GOLD','Нууц үг',value = ''),
                               uiOutput('ui_GOLD')
                             )
                           ),
                           fluidRow(
                             actionButton('button_GOLD','Хадгалах')
                           )
                         )),
               tabPanel( 'OIL',
                         fluidPage(
                           fluidRow(
                             fluidPage(
                               column(1,
                                      box(title = tags$h6(tags$b('Хугацаа')),uiOutput('dateout_OIL'))),
                               column(11,
                                      box(title = tags$h6(tags$b('Fundamental sites')), rHandsontableOutput('textout1_OIL',width = '100%')))
                             )
                           ),
                           fluidRow(
                             fluidPage(
                               column(3,
                                      box(title = tags$h6(tags$b('Week chart')), uiOutput('textout2_OIL'))),
                               column(3,
                                      box(title = tags$h6(tags$b('Day chart')), uiOutput('textout3_OIL'))),
                               column(3,
                                      box(title = tags$h6(tags$b('H4 chart')), uiOutput('textout4_OIL'))),
                               column(3,
                                      box(title = tags$h6(tags$b('H1 chart')), uiOutput('textout5_OIL')))
                             )
                           ),
                           fluidRow(
                             fluidPage(
                               column(2,
                                      box(title = tags$h6(tags$b('Pattern')), uiOutput('textout6_OIL'))),
                               column(2,
                                      box(title = tags$h6(tags$b('Indicator')), uiOutput('textout7_OIL'))),
                               column(2,
                                      box(title = tags$h6(tags$b('Тэмдэглэгээ')), uiOutput('textout8_OIL'))),
                               column(2,
                                      box(title = tags$h6(tags$b('Market maker')), uiOutput('textout9_OIL'))),
                               column(4,
                                      box(title = tags$h6(tags$b('Хойшхи бүх цаас')), uiOutput('textout10_OIL')))
                             )
                           ),
                           fluidRow(
                             fluidPage(
                               selectInput('type_OIL','',choices = c('date','fundamental','week','day','h4','h1','pattern','indicator','note','mm','all_paper')),
                               textInput('input_OIL','Текст бич',value = ''),
                               textInput('pass_OIL','Нууц үг',value = ''),
                               uiOutput('ui_OIL')
                             )
                           ),
                           fluidRow(
                             actionButton('button_OIL','Хадгалах')
                           )
                         ))
             )
           )),
  tabPanel('Journal',
           fluidPage(
             fluidRow(
               column(12,
                      DTOutput('DT8'))
             ),
             fluidRow(
               column(1,
                      dateInput('text1','Date',value = '', format = 'yyyy-mm-dd')),
               column(1,
                      selectInput('text2','Session',choices = c('London','Newyork','Tokyo','Sydney'),selected = NULL)),
               column(1,
                      selectInput('text3','Weekdays',choices = c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'),selected = NULL)),
               column(1,
                      numericInput('text4','Opentime',value = NULL)),
               column(1,
                      numericInput('text5','Closetime',value = NULL)),
               column(1,
                      textInput('text6','Pairs')),
               column(1,
                      numericInput('text7','Openprice',value = NULL)),
               column(1,
                      numericInput('text8','Closeprice',value = NULL)),
               column(1,
                      numericInput('text9','Spread pip',value = NULL)),
               column(1,
                      selectInput('text10','Type',choices = c('Buy','Sell'),selected = NULL)),
               column(1,
                      numericInput('text11','SL',value = NULL)),
               column(1,
                      numericInput('text12','TP',value = NULL))
             ),
             fluidRow(
               column(1,
                      numericInput('text13','Unit',value = NULL)),
               column(1,
                      numericInput('text14',"Profit pip",value = NULL)),
               column(1,
                      textInput('pass','Нууц үг',value = NULL)),
               column(1,
                      actionButton('button','Хадгалах'))
             ),
             fluidRow(
               uiOutput('ui1')
             )
           ))
)
server <- function(input, output){
  values<-reactiveValues()

  output$DT8<-renderDataTable({
    datatable(FX_journal, options = list(dom='t'),rownames = F)
  })
  observeEvent(input$button,{
    if(input$pass=='Sugaraa970528*'){
      Data_add<-data.frame(Date=input$text1, Session=input$text2, Weekday=input$text3,Open_time=input$text4, Close_time=input$text5, Pairs=input$text6, Open_price=input$text7, Close_price=input$text8, Spread_pip=input$text9, Type=input$text10, SL=input$text11, TP=input$text12, Unit=input$text13, Profit_pip=input$text14)
      FX_journal<-rbind(FX_journal, Data_add)
      saveRDS(FX_journal, "FX_journal.RDS")
    } else {
      output$ui1<-renderUI({
        print('Код буруу байна')
      })
    }
  })
  observeEvent(input$button_USDJPY,{
    if(input$pass_USDJPY=='Sugaraa970528*'){
      if(input$type_USDJPY=='fundamental'){
        values$data<-hot_to_r(input$textout1_USDJPY)
        Data<-data.frame(values$data)
        saveRDS(Data,"fun_USDJPY.RDS")
        #Data<-input$input_USDJPY
        #saveRDS(Data, "fun_USDJPY.RDS")
      } else if(input$type_USDJPY=='date'){
        Data<-input$input_USDJPY
        saveRDS(Data,'date_USDJPY.RDS')
      } else if(input$type_USDJPY=='week'){
        Data<-input$input_USDJPY
        saveRDS(Data,"week_USDJPY.RDS")
      } else if(input$type_USDJPY=='day'){
        Data<-input$input_USDJPY
        saveRDS(Data,"day_USDJPY.RDS")
      } else if(input$type_USDJPY=='h4'){
        Data<-input$input_USDJPY
        saveRDS(Data,"h4_USDJPY.RDS")
      } else if(input$type_USDJPY=='h1'){
        Data<-input$input_USDJPY
        saveRDS(Data,"h1_USDJPY.RDS")
      } else if(input$type_USDJPY=='pattern'){
        Data<-input$input_USDJPY
        saveRDS(Data,"pattern_USDJPY.RDS")
      } else if(input$type_USDJPY=='indicator'){
        Data<-input$input_USDJPY
        saveRDS(Data,"indicator_USDJPY.RDS")
      } else if(input$type_USDJPY=='note'){
        Data<-input$input_USDJPY
        saveRDS(Data,"note_USDJPY.RDS")
      } else if(input$type_USDJPY=='mm'){
        Data<-input$input_USDJPY
        saveRDS(Data,"mm_USDJPY.RDS")
      } else if(input$type_USDJPY=='all_paper'){
        Data<-input$input_USDJPY
        saveRDS(Data,"all_paper_USDJPY.RDS")
      }
    } else {
      output$ui_USDJPY<-renderUI({
        print('Код буруу байна!')
      })
    }
  })
  output$textout1_USDJPY<-renderRHandsontable({
    DF<-readRDS('fun_USDJPY.RDS')
    DF$Day<-as.character(DF$Day)
    DF$Time<-as.character(DF$Time)
    DF$Crncy<-as.character(DF$Crncy)
    DF$Rank<-as.character(DF$Rank)
    DF$Name<-as.character(DF$Name)
    DF$Expect<-as.character(DF$Expect)
    DF$Desc<-as.character(DF$Desc)
    rhandsontable(DF, rowHeaders = NULL)
  })
  output$textout2_USDJPY<-renderUI({
    Data<-readRDS("week_USDJPY.RDS")
    Data
  })
  output$textout3_USDJPY<-renderUI({
    Data<-readRDS("day_USDJPY.RDS")
    Data
  })
  output$textout4_USDJPY<-renderUI({
    Data<-readRDS("h4_USDJPY.RDS")
    Data
  })
  output$textout5_USDJPY<-renderUI({
    Data<-readRDS("h1_USDJPY.RDS")
    Data
  })
  output$textout6_USDJPY<-renderUI({
    Data<-readRDS("pattern_USDJPY.RDS")
    Data
  })
  output$textout7_USDJPY<-renderUI({
    Data<-readRDS("indicator_USDJPY.RDS")
    Data
  })
  output$textout8_USDJPY<-renderUI({
    Data<-readRDS("note_USDJPY.RDS")
    Data
  })
  output$textout9_USDJPY<-renderUI({
    Data<-readRDS("mm_USDJPY.RDS")
    Data
  })
  output$textout10_USDJPY<-renderUI({
    Data<-readRDS("all_paper_USDJPY.RDS")
    Data
  })
  output$dateout_USDJPY<-renderUI({
    Data<-readRDS('date_USDJPY.RDS')
    Data
  })
  observeEvent(input$button_EURJPY,{
    if(input$pass_EURJPY=='Sugaraa970528*'){
      if(input$type_EURJPY=='fundamental'){
        values$data<-hot_to_r(input$textout1_EURJPY)
        Data<-data.frame(values$data)
        saveRDS(Data,"fun_EURJPY.RDS")
      } else if(input$type_EURJPY=='date'){
        Data<-input$input_EURJPY
        saveRDS(Data,'date_EURJPY.RDS')
      } else if(input$type_EURJPY=='week'){
        Data<-input$input_EURJPY
        saveRDS(Data,"week_EURJPY.RDS")
      } else if(input$type_EURJPY=='day'){
        Data<-input$input_EURJPY
        saveRDS(Data,"day_EURJPY.RDS")
      } else if(input$type_EURJPY=='h4'){
        Data<-input$input_EURJPY
        saveRDS(Data,"h4_EURJPY.RDS")
      } else if(input$type_EURJPY=='h1'){
        Data<-input$input_EURJPY
        saveRDS(Data,"h1_EURJPY.RDS")
      } else if(input$type_EURJPY=='pattern'){
        Data<-input$input_EURJPY
        saveRDS(Data,"pattern_EURJPY.RDS")
      } else if(input$type_EURJPY=='indicator'){
        Data<-input$input_EURJPY
        saveRDS(Data,"indicator_EURJPY.RDS")
      } else if(input$type_EURJPY=='note'){
        Data<-input$input_EURJPY
        saveRDS(Data,"note_EURJPY.RDS")
      } else if(input$type_EURJPY=='mm'){
        Data<-input$input_EURJPY
        saveRDS(Data,"mm_EURJPY.RDS")
      } else if(input$type_EURJPY=='all_paper'){
        Data<-input$input_EURJPY
        saveRDS(Data,"all_paper_EURJPY.RDS")
      }
    } else {
      output$ui_EURJPY<-renderUI({
        print('Код буруу байна')
      })
    }
  })
  output$textout1_EURJPY<-renderRHandsontable({
    DF<-readRDS('fun_EURJPY.RDS')
    DF$Day<-as.character(DF$Day)
    DF$Time<-as.character(DF$Time)
    DF$Crncy<-as.character(DF$Crncy)
    DF$Rank<-as.character(DF$Rank)
    DF$Name<-as.character(DF$Name)
    DF$Expect<-as.character(DF$Expect)
    DF$Desc<-as.character(DF$Desc)
    rhandsontable(DF, rowHeaders = NULL)
  })
  output$textout2_EURJPY<-renderUI({
    Data<-readRDS("week_EURJPY.RDS")
    Data
  })
  output$textout3_EURJPY<-renderUI({
    Data<-readRDS("day_EURJPY.RDS")
    Data
  })
  output$textout4_EURJPY<-renderUI({
    Data<-readRDS("h4_EURJPY.RDS")
    Data
  })
  output$textout5_EURJPY<-renderUI({
    Data<-readRDS("h1_EURJPY.RDS")
    Data
  })
  output$textout6_EURJPY<-renderUI({
    Data<-readRDS("pattern_EURJPY.RDS")
    Data
  })
  output$textout7_EURJPY<-renderUI({
    Data<-readRDS("indicator_EURJPY.RDS")
    Data
  })
  output$textout8_EURJPY<-renderUI({
    Data<-readRDS("note_EURJPY.RDS")
    Data
  })
  output$textout9_EURJPY<-renderUI({
    Data<-readRDS("mm_EURJPY.RDS")
    Data
  })
  output$textout10_EURJPY<-renderUI({
    Data<-readRDS("all_paper_EURJPY.RDS")
    Data
  })
  output$dateout_EURJPY<-renderUI({
    Data<-readRDS('date_EURJPY.RDS')
    Data
  }) 
  observeEvent(input$button_GBPJPY,{
    if(input$pass_GBPJPY=='SUgaraa970528*'){
      if(input$type_GBPJPY=='fundamental'){
        values$data<-hot_to_r(input$textout1_GBPJPY)
        Data<-data.frame(values$data)
        saveRDS(Data,"fun_GBPJPY.RDS")
      } else if(input$type_GBPJPY=='date'){
        Data<-input$input_GBPJPY
        saveRDS(Data,'date_GBPJPY.RDS')
      } else if(input$type_GBPJPY=='week'){
        Data<-input$input_GBPJPY
        saveRDS(Data,"week_GBPJPY.RDS")
      } else if(input$type_GBPJPY=='day'){
        Data<-input$input_GBPJPY
        saveRDS(Data,"day_GBPJPY.RDS")
      } else if(input$type_GBPJPY=='h4'){
        Data<-input$input_GBPJPY
        saveRDS(Data,"h4_GBPJPY.RDS")
      } else if(input$type_GBPJPY=='h1'){
        Data<-input$input_GBPJPY
        saveRDS(Data,"h1_GBPJPY.RDS")
      } else if(input$type_GBPJPY=='pattern'){
        Data<-input$input_GBPJPY
        saveRDS(Data,"pattern_GBPJPY.RDS")
      } else if(input$type_GBPJPY=='indicator'){
        Data<-input$input_GBPJPY
        saveRDS(Data,"indicator_GBPJPY.RDS")
      } else if(input$type_GBPJPY=='note'){
        Data<-input$input_GBPJPY
        saveRDS(Data,"note_GBPJPY.RDS")
      } else if(input$type_GBPJPY=='mm'){
        Data<-input$input_GBPJPY
        saveRDS(Data,"mm_GBPJPY.RDS")
      } else if(input$type_GBPJPY=='all_paper'){
        Data<-input$input_GBPJPY
        saveRDS(Data,"all_paper_GBPJPY.RDS")
      }
    } else {
      output$ui_GBPJPY<-renderUI({
        print('Код буруу байна')
      })
    }
  })
  output$textout1_GBPJPY<-renderRHandsontable({
    DF<-readRDS('fun_GBPJPY.RDS')
    DF$Day<-as.character(DF$Day)
    DF$Time<-as.character(DF$Time)
    DF$Crncy<-as.character(DF$Crncy)
    DF$Rank<-as.character(DF$Rank)
    DF$Name<-as.character(DF$Name)
    DF$Expect<-as.character(DF$Expect)
    DF$Desc<-as.character(DF$Desc)
    rhandsontable(DF, rowHeaders = NULL)
  })
  output$textout2_GBPJPY<-renderUI({
    Data<-readRDS("week_GBPJPY.RDS")
    Data
  })
  output$textout3_GBPJPY<-renderUI({
    Data<-readRDS("day_GBPJPY.RDS")
    Data
  })
  output$textout4_GBPJPY<-renderUI({
    Data<-readRDS("h4_GBPJPY.RDS")
    Data
  })
  output$textout5_GBPJPY<-renderUI({
    Data<-readRDS("h1_GBPJPY.RDS")
    Data
  })
  output$textout6_GBPJPY<-renderUI({
    Data<-readRDS("pattern_GBPJPY.RDS")
    Data
  })
  output$textout7_GBPJPY<-renderUI({
    Data<-readRDS("indicator_GBPJPY.RDS")
    Data
  })
  output$textout8_GBPJPY<-renderUI({
    Data<-readRDS("note_GBPJPY.RDS")
    Data
  })
  output$textout9_GBPJPY<-renderUI({
    Data<-readRDS("mm_GBPJPY.RDS")
    Data
  })
  output$textout10_GBPJPY<-renderUI({
    Data<-readRDS("all_paper_GBPJPY.RDS")
    Data
  })
  output$dateout_GBPJPY<-renderUI({
    Data<-readRDS('date_GBPJPY.RDS')
    Data
  })
  observeEvent(input$button_AUDJPY,{
    if(input$pass_AUDJPY=='Sugaraa970528*'){
      if(input$type_AUDJPY=='fundamental'){
        values$data<-hot_to_r(input$textout1_AUDJPY)
        Data<-data.frame(values$data)
        saveRDS(Data,"fun_AUDJPY.RDS")
      } else if(input$type_AUDJPY=='date'){
        Data<-input$input_AUDJPY
        saveRDS(Data,'date_AUDJPY.RDS')
      } else if(input$type_AUDJPY=='week'){
        Data<-input$input_AUDJPY
        saveRDS(Data,"week_AUDJPY.RDS")
      } else if(input$type_AUDJPY=='day'){
        Data<-input$input_AUDJPY
        saveRDS(Data,"day_AUDJPY.RDS")
      } else if(input$type_AUDJPY=='h4'){
        Data<-input$input_AUDJPY
        saveRDS(Data,"h4_AUDJPY.RDS")
      } else if(input$type_AUDJPY=='h1'){
        Data<-input$input_AUDJPY
        saveRDS(Data,"h1_AUDJPY.RDS")
      } else if(input$type_AUDJPY=='pattern'){
        Data<-input$input_AUDJPY
        saveRDS(Data,"pattern_AUDJPY.RDS")
      } else if(input$type_AUDJPY=='indicator'){
        Data<-input$input_AUDJPY
        saveRDS(Data,"indicator_AUDJPY.RDS")
      } else if(input$type_AUDJPY=='note'){
        Data<-input$input_AUDJPY
        saveRDS(Data,"note_AUDJPY.RDS")
      } else if(input$type_AUDJPY=='mm'){
        Data<-input$input_AUDJPY
        saveRDS(Data,"mm_AUDJPY.RDS")
      } else if(input$type_AUDJPY=='all_paper'){
        Data<-input$input_AUDJPY
        saveRDS(Data,"all_paper_AUDJPY.RDS")
      }
    } else {
      output$ui_AUDJPY<-renderUI({
        print('Код буруу байна')
      })
    }
  })
  output$textout1_AUDJPY<-renderRHandsontable({
    DF<-readRDS('fun_AUDJPY.RDS')
    DF$Day<-as.character(DF$Day)
    DF$Time<-as.character(DF$Time)
    DF$Crncy<-as.character(DF$Crncy)
    DF$Rank<-as.character(DF$Rank)
    DF$Name<-as.character(DF$Name)
    DF$Expect<-as.character(DF$Expect)
    DF$Desc<-as.character(DF$Desc)
    rhandsontable(DF, rowHeaders = NULL)
  })
  output$textout2_AUDJPY<-renderUI({
    Data<-readRDS("week_AUDJPY.RDS")
    Data
  })
  output$textout3_AUDJPY<-renderUI({
    Data<-readRDS("day_AUDJPY.RDS")
    Data
  })
  output$textout4_AUDJPY<-renderUI({
    Data<-readRDS("h4_AUDJPY.RDS")
    Data
  })
  output$textout5_AUDJPY<-renderUI({
    Data<-readRDS("h1_AUDJPY.RDS")
    Data
  })
  output$textout6_AUDJPY<-renderUI({
    Data<-readRDS("pattern_AUDJPY.RDS")
    Data
  })
  output$textout7_AUDJPY<-renderUI({
    Data<-readRDS("indicator_AUDJPY.RDS")
    Data
  })
  output$textout8_AUDJPY<-renderUI({
    Data<-readRDS("note_AUDJPY.RDS")
    Data
  })
  output$textout9_AUDJPY<-renderUI({
    Data<-readRDS("mm_AUDJPY.RDS")
    Data
  })
  output$textout10_AUDJPY<-renderUI({
    Data<-readRDS("all_paper_AUDJPY.RDS")
    Data
  })
  output$dateout_AUDJPY<-renderUI({
    Data<-readRDS('date_AUDJPY.RDS')
    Data
  })
  observeEvent(input$button_NZDJPY,{
    if(input$pass_NZDJPY=='Sugaraa970528*'){
      if(input$type_NZDJPY=='fundamental'){
        values$data<-hot_to_r(input$textout1_NZDJPY)
        Data<-data.frame(values$data)
        saveRDS(Data,"fun_NZDJPY.RDS")
      } else if(input$type_NZDJPY=='date'){
        Data<-input$input_NZDJPY
        saveRDS(Data,'date_NZDJPY.RDS')
      } else if(input$type_NZDJPY=='week'){
        Data<-input$input_NZDJPY
        saveRDS(Data,"week_NZDJPY.RDS")
      } else if(input$type_NZDJPY=='day'){
        Data<-input$input_NZDJPY
        saveRDS(Data,"day_NZDJPY.RDS")
      } else if(input$type_NZDJPY=='h4'){
        Data<-input$input_NZDJPY
        saveRDS(Data,"h4_NZDJPY.RDS")
      } else if(input$type_NZDJPY=='h1'){
        Data<-input$input_NZDJPY
        saveRDS(Data,"h1_NZDJPY.RDS")
      } else if(input$type_NZDJPY=='pattern'){
        Data<-input$input_NZDJPY
        saveRDS(Data,"pattern_NZDJPY.RDS")
      } else if(input$type_NZDJPY=='indicator'){
        Data<-input$input_NZDJPY
        saveRDS(Data,"indicator_NZDJPY.RDS")
      } else if(input$type_NZDJPY=='note'){
        Data<-input$input_NZDJPY
        saveRDS(Data,"note_NZDJPY.RDS")
      } else if(input$type_NZDJPY=='mm'){
        Data<-input$input_NZDJPY
        saveRDS(Data,"mm_NZDJPY.RDS")
      } else if(input$type_NZDJPY=='all_paper'){
        Data<-input$input_NZDJPY
        saveRDS(Data,"all_paper_NZDJPY.RDS")
      }
    } else {
      output$ui_NZDJPY<-renderUI({
        print('Код буруу байна')
      })
    }
  })
  output$textout1_NZDJPY<-renderRHandsontable({
    DF<-readRDS('fun_NZDJPY.RDS')
    DF$Day<-as.character(DF$Day)
    DF$Time<-as.character(DF$Time)
    DF$Crncy<-as.character(DF$Crncy)
    DF$Rank<-as.character(DF$Rank)
    DF$Name<-as.character(DF$Name)
    DF$Expect<-as.character(DF$Expect)
    DF$Desc<-as.character(DF$Desc)
    rhandsontable(DF, rowHeaders = NULL)
  })
  output$textout2_NZDJPY<-renderUI({
    Data<-readRDS("week_NZDJPY.RDS")
    Data
  })
  output$textout3_NZDJPY<-renderUI({
    Data<-readRDS("day_NZDJPY.RDS")
    Data
  })
  output$textout4_NZDJPY<-renderUI({
    Data<-readRDS("h4_NZDJPY.RDS")
    Data
  })
  output$textout5_NZDJPY<-renderUI({
    Data<-readRDS("h1_NZDJPY.RDS")
    Data
  })
  output$textout6_NZDJPY<-renderUI({
    Data<-readRDS("pattern_NZDJPY.RDS")
    Data
  })
  output$textout7_NZDJPY<-renderUI({
    Data<-readRDS("indicator_NZDJPY.RDS")
    Data
  })
  output$textout8_NZDJPY<-renderUI({
    Data<-readRDS("note_NZDJPY.RDS")
    Data
  })
  output$textout9_NZDJPY<-renderUI({
    Data<-readRDS("mm_NZDJPY.RDS")
    Data
  })
  output$textout10_NZDJPY<-renderUI({
    Data<-readRDS("all_paper_NZDJPY.RDS")
    Data
  })
  output$dateout_NZDJPY<-renderUI({
    Data<-readRDS('date_NZDJPY.RDS')
    Data
  })
  observeEvent(input$button_GOLD,{
    if(input$pass_GOLD=='Sugaraa970528*'){
      if(input$type_GOLD=='fundamental'){
        values$data<-hot_to_r(input$textout1_GOLD)
        Data<-data.frame(values$data)
        saveRDS(Data,"fun_GOLD.RDS")
      } else if(input$type_GOLD=='date'){
        Data<-input$input_GOLD
        saveRDS(Data,'date_GOLD.RDS')
      } else if(input$type_GOLD=='week'){
        Data<-input$input_GOLD
        saveRDS(Data,"week_GOLD.RDS")
      } else if(input$type_GOLD=='day'){
        Data<-input$input_GOLD
        saveRDS(Data,"day_GOLD.RDS")
      } else if(input$type_GOLD=='h4'){
        Data<-input$input_GOLD
        saveRDS(Data,"h4_GOLD.RDS")
      } else if(input$type_GOLD=='h1'){
        Data<-input$input_GOLD
        saveRDS(Data,"h1_GOLD.RDS")
      } else if(input$type_GOLD=='pattern'){
        Data<-input$input_GOLD
        saveRDS(Data,"pattern_GOLD.RDS")
      } else if(input$type_GOLD=='indicator'){
        Data<-input$input_GOLD
        saveRDS(Data,"indicator_GOLD.RDS")
      } else if(input$type_GOLD=='note'){
        Data<-input$input_GOLD
        saveRDS(Data,"note_GOLD.RDS")
      } else if(input$type_GOLD=='mm'){
        Data<-input$input_GOLD
        saveRDS(Data,"mm_GOLD.RDS")
      } else if(input$type_GOLD=='all_paper'){
        Data<-input$input_GOLD
        saveRDS(Data,"all_paper_GOLD.RDS")
      }
    } else {
      output$ui_GOLD<-renderUI({
        print('Код буруу байна')
      })
    }
  })
  output$textout1_GOLD<-renderRHandsontable({
    DF<-readRDS('fun_GOLD.RDS')
    DF$Day<-as.character(DF$Day)
    DF$Time<-as.character(DF$Time)
    DF$Crncy<-as.character(DF$Crncy)
    DF$Rank<-as.character(DF$Rank)
    DF$Name<-as.character(DF$Name)
    DF$Expect<-as.character(DF$Expect)
    DF$Desc<-as.character(DF$Desc)
    rhandsontable(DF, rowHeaders = NULL)
  })
  output$textout2_GOLD<-renderUI({
    Data<-readRDS("week_GOLD.RDS")
    Data
  })
  output$textout3_GOLD<-renderUI({
    Data<-readRDS("day_GOLD.RDS")
    Data
  })
  output$textout4_GOLD<-renderUI({
    Data<-readRDS("h4_GOLD.RDS")
    Data
  })
  output$textout5_GOLD<-renderUI({
    Data<-readRDS("h1_GOLD.RDS")
    Data
  })
  output$textout6_GOLD<-renderUI({
    Data<-readRDS("pattern_GOLD.RDS")
    Data
  })
  output$textout7_GOLD<-renderUI({
    Data<-readRDS("indicator_GOLD.RDS")
    Data
  })
  output$textout8_GOLD<-renderUI({
    Data<-readRDS("note_GOLD.RDS")
    Data
  })
  output$textout9_GOLD<-renderUI({
    Data<-readRDS("mm_GOLD.RDS")
    Data
  })
  output$textout10_GOLD<-renderUI({
    Data<-readRDS("all_paper_GOLD.RDS")
    Data
  })
  output$dateout_GOLD<-renderUI({
    Data<-readRDS('date_GOLD.RDS')
    Data
  })
  observeEvent(input$button_OIL,{
    if(input$pass_OIL=='Sugaraa970528*'){
      if(input$type_OIL=='fundamental'){
        values$data<-hot_to_r(input$textout1_OIL)
        Data<-data.frame(values$data)
        saveRDS(Data,"fun_OIL.RDS")
      } else if(input$type_OIL=='date'){
        Data<-input$input_OIL
        saveRDS(Data,'date_OIL.RDS')
      } else if(input$type_OIL=='week'){
        Data<-input$input_OIL
        saveRDS(Data,"week_OIL.RDS")
      } else if(input$type_OIL=='day'){
        Data<-input$input_OIL
        saveRDS(Data,"day_OIL.RDS")
      } else if(input$type_OIL=='h4'){
        Data<-input$input_OIL
        saveRDS(Data,"h4_OIL.RDS")
      } else if(input$type_OIL=='h1'){
        Data<-input$input_OIL
        saveRDS(Data,"h1_OIL.RDS")
      } else if(input$type_OIL=='pattern'){
        Data<-input$input_OIL
        saveRDS(Data,"pattern_OIL.RDS")
      } else if(input$type_OIL=='indicator'){
        Data<-input$input_OIL
        saveRDS(Data,"indicator_OIL.RDS")
      } else if(input$type_OIL=='note'){
        Data<-input$input_OIL
        saveRDS(Data,"note_OIL.RDS")
      } else if(input$type_OIL=='mm'){
        Data<-input$input_OIL
        saveRDS(Data,"mm_OIL.RDS")
      } else if(input$type_OIL=='all_paper'){
        Data<-input$input_OIL
        saveRDS(Data,"all_paper_OIL.RDS")
      }
    } else {
      output$ui_OIL<-renderUI({
        print('Код буруу байна')
      })
    }
  })
  output$textout1_OIL<-renderRHandsontable({
    DF<-readRDS('fun_OIL.RDS')
    DF$Day<-as.character(DF$Day)
    DF$Time<-as.character(DF$Time)
    DF$Crncy<-as.character(DF$Crncy)
    DF$Rank<-as.character(DF$Rank)
    DF$Name<-as.character(DF$Name)
    DF$Expect<-as.character(DF$Expect)
    DF$Desc<-as.character(DF$Desc)
    rhandsontable(DF, rowHeaders = NULL)
  })
  output$textout2_OIL<-renderUI({
    Data<-readRDS("week_OIL.RDS")
    Data
  })
  output$textout3_OIL<-renderUI({
    Data<-readRDS("day_OIL.RDS")
    Data
  })
  output$textout4_OIL<-renderUI({
    Data<-readRDS("h4_OIL.RDS")
    Data
  })
  output$textout5_OIL<-renderUI({
    Data<-readRDS("h1_OIL.RDS")
    Data
  })
  output$textout6_OIL<-renderUI({
    Data<-readRDS("pattern_OIL.RDS")
    Data
  })
  output$textout7_OIL<-renderUI({
    Data<-readRDS("indicator_OIL.RDS")
    Data
  })
  output$textout8_OIL<-renderUI({
    Data<-readRDS("note_OIL.RDS")
    Data
  })
  output$textout9_OIL<-renderUI({
    Data<-readRDS("mm_OIL.RDS")
    Data
  })
  output$textout10_OIL<-renderUI({
    Data<-readRDS("all_paper_OIL.RDS")
    Data
  })
  output$dateout_OIL<-renderUI({
    Data<-readRDS('date_OIL.RDS')
    Data
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

