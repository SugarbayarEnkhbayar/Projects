navbarPage("Magic",theme = shinytheme('sandstone'),
           header = tagList(
             useShinydashboard()
           ),
           tabPanel("Data Input",
                    fluidRow(
                      column(6,shinycssloaders::withSpinner(infoBoxOutput('ROW',width = 12))),
                      column(6,shinycssloaders::withSpinner(infoBoxOutput('FEATURES',width = 12)))
                      ),
                    fluidRow(fluidPage(fileInput('fileInput','Upload file here'))),
                    fluidRow(box(width=12,title = "Data table", status = "primary", solidHeader = TRUE,
                                 shinycssloaders::withSpinner(DTOutput('FirstFile'))))
                    ),
           navbarMenu("Process",
                      tabPanel("Data cleaning",
                               fluidRow(box(width = 12,title='Data',status = 'primary',solidHeader = T,
                                            shinycssloaders::withSpinner(DTOutput('SecondFile')))),
                               fluidRow(
                                 column(4,box(width = 12,title='Unnecessary columns',status = 'primary',solidHeader = F,
                                              varSelectInput('rem_un','Choose columns',data = '',multiple = T),
                                              actionButton('click1','Remove'))),
                                 column(8,box(width = 12,title='Change data types',status='primary',solidHeader = T,
                                              column(9,shinycssloaders::withSpinner(DTOutput('ChanTypFile'))),
                                              column(3,varSelectInput('change_typ','Choose column',data='',multiple = F),
                                                     selectInput('selectInpTyp','Choose type',choices = c('Date','Numeric','Character','Factor','Integer'),selected = NULL),
                                                     actionButton('click2','Change'))))
                                      ),
                               fluidRow(
                                 column(4,box(width = 12,title='Change column name',status = 'primary',solidHeader = T,
                                              varSelectInput('changeColNam','Choose columns',data='',multiple = F),
                                              textInput('NewColNam','Write new column name'),
                                              actionButton('click3','Change'))),
                                 column(4,box(width=12,title='Missing data',status='primary',solidHeader=T,
                                              varSelectInput('MissVal','Choose columns',data='',multiple = F),
                                              actionButton('click4','Show'),
                                              selectInput('MissSelect','Choose Method',choices = c('Remove','Mean','Median','Most_Fre'),selected=NULL,multiple = F),
                                              actionButton('click5','Remove/Fill'),
                                              DTOutput('MissRes'))),
                                 column(4,box(width = 12,title = 'Remove duplicates',status = 'primary',solidHeader = T,
                                              selectInput('DupliSelect','Choose',choices=c('Yes','No'),selected=NULL,multiple=F),
                                              actionButton("click6",'Remove')))
                               ),
                               fluidRow(
                                 column(4,box(width = 12,title='Group by-(educ,rel_len)',status='primary',solidHeader = T,
                                              varSelectInput('SpecVal','Choose columns',data='',multiple = T),
                                              varSelectInput('SpecVal_GR','Choose columns',data='',multiple = F),
                                              selectInput('SelMM','Choose method',choices=c('Mean','Median','Sum','Count'),selected=NULL,multiple=F),
                                              actionButton('click7','Do'),
                                              DTOutput('GroupTable'))),
                                 column(4,box(width=12,title='Outlier',status='primary',solidHeader=T,
                                              varSelectInput('SelColOut','Choose columns',data='',multiple=F),
                                              selectInput('SelMetOut','Choose method',choices=c('IQR','Z score'),selected=NULL,multiple=F),
                                              actionButton('click8','Remove'),
                                              DTOutput('OutTable'))),
                                 column(4,box(width=12,title='Correlation',status='primary',solidHeader=T,
                                              varSelectInput('SelColCor1','Choose columns',data='',multiple=F),
                                              varSelectInput('SelColCor2','Choose columns',data='',multiple=F),
                                              actionButton('click81','Change'),
                                              uiOutput('rescor')))
                               ),
                               fluidRow(
                                 column(6,box(width = 12,title='Filter',status='primary',solidHeader = T,
                                              varSelectInput('SelColFilt','Choose columns',data='',multiple = F),
                                              textInput('FiltVal','Value:',value = ""),
                                              actionButton('click82','Filt'),
                                              DTOutput('resFilt')
                                 )),
                                 column(3,box(width = 12,title='TTest',status='primary',solidHeader = T,
                                              varSelectInput('SelColtt1','Choose columns',data='',multiple = F),
                                              varSelectInput('SelColtt2','Choose columns',data='',multiple = F),
                                              actionButton('click83','do'),
                                              verbatimTextOutput('resTtest')
                                 )),
                                 column(3,box(width = 12,title='ChiSquareTest',status='primary',solidHeader = T,
                                              varSelectInput('SelColchi1','Choose columns',data='',multiple = F),
                                              varSelectInput('SelColchi2','Choose columns',data='',multiple = F),
                                              actionButton('click84','do'),
                                              verbatimTextOutput('resChitest')
                                 ))
                               ),
                               fluidRow(
                                 column(4,box(width = 12,title='Statistic',status='primary',solidHeader = T,
                                              varSelectInput('SelColMath','Choose columns',data='',multiple = F),
                                              selectInput('SelActMath','Choose method',choices=c('Max','Min','Mean'),selected=NULL,multiple=F),
                                              actionButton('click85','Filt'),
                                              uiOutput('resstat'))),
                                 column(4)
                               )
                               ),
                      tabPanel("Analytical",
                               fluidPage(
                                 selectInput('SelModel','Choose method',choices=c('Linear Regression','Classification','Cluster'),selected=NULL,multiple=F),
                                 varSelectInput('VarY','Choose Independent variable',data='',multiple=F),
                                 varSelectInput('VarX','Choose Dependent variable',data='',multiple=T),
                                 actionButton('magic','Abracadabra'),
                                 shinycssloaders::withSpinner(verbatimTextOutput('ui1')),
                                 shinycssloaders::withSpinner(verbatimTextOutput('ui2')),
                                 shinycssloaders::withSpinner(verbatimTextOutput('ui3')),
                                 shinycssloaders::withSpinner(verbatimTextOutput('ui4'))
                               ))
                      ),
           tabPanel('Visualization',
                    fluidPage(
                      fluidRow(
                        column(4,box(width=12,title='One variable',status='primary',solidHeader = T,
                                     varSelectInput('SelOneVarPlot','Choose column',data='',multiple=F),
                                     selectInput('OneVarTypePlot','Choose type of chart',choices = c('Line','Bar','Pie')),
                                     actionButton('click9','Show'),
                                     plotlyOutput('OnePLT'))),
                        column(4,box(width=12,title='Two variable',status='primary',solidHeader = T,
                                     varSelectInput('SelTwoVarPlot','Choose column',data='',multiple=T),
                                     selectInput('TwoVarTypePlot','Choose type of chart',choices = c('Scatter')),
                                     actionButton('click10','Show'),
                                     plotlyOutput('TwoPLT'))),
                        column(4,box(width=12,title='Three variable',status='primary',solidHeader = T,
                                     varSelectInput('SelThrVarPlot','Choose column',data='',multiple=T),
                                     selectInput('ThrVarTypePlot','Choose type of chart',choices = c('3D')),
                                     actionButton('click11','Show'),
                                     rglwidgetOutput('ThrPLT')))
                      ),
                      fluidRow(
                        fluidPage(
                          varSelectInput('IntSelVar','Choose column',data='',multiple = F),
                          actionButton('click12','Show'),
                          plotlyOutput('IntPLT'),
                          DTOutput('IntDT')
                        )
                      )
                    )),
           tabPanel('Download',
                    downloadButton(
                      'down',
                      label = "Download",
                      class = NULL,
                      icon = shiny::icon("download")
                    ))
           )