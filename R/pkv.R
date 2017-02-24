#if(!require(devtools)) {install.packages("devtools"); require(devtools)}


#detach("package:roxygen2", unload=TRUE)
#install.packages("Rcpp", dependencies=TRUE, INSTALL_opts = c('--no-lock'))
#if(!require(roxygen2)) {install.packages("roxygen2"); require(roxygen2)}
if(!require(devtools)) {install.packages("devtools"); require(devtools)}
if(!require(Rcpp)) {    install.packages("Rcpp");     require(Rcpp)}
if(!require(roxygen2)) {install.packages("roxygen2"); require(roxygen2)}
if(!require(shiny)) {   install.packages("shiny");    require(shiny)}
if(!require(haven)) {   install.packages("haven");    require(haven)}
if(!require(foreign)) { install.packages("foreign");  require(foreign)}
if(!require(DT)) {      install.packages("DT");       require(DT)}
if(!require(ggplot2)) { install.packages("ggplot2");  require(ggplot2)}

library(Rcpp)
library(devtools)
library(roxygen2)
library(shiny)
library(haven)
library(foreign)
library(DT)
library(ggplot2)

shiny_pkv <- function(options = c("sas2csv","nm_dataviz", "none")){

  if (options=="none") {return(NULL)}

  else if(options=="sas2csv") {

    server <- function(input, output) {
      #Input the uploaded file to a reactive function#
      con<-reactive({
        inFile <- input$file1
        if (is.null(inFile))
          return(NULL)
        substrRight <- function(inFile, n){
          substr(inFile, nchar(inFile)-n+1, nchar(inFile))
        }

        type= substrRight(input$file1, 4)
        if(type==".xpt")
        {read.xport(inFile$datapath)}
        else {read_sas(inFile$datapath, catalog_file = NULL, encoding = NULL)}
      })

      #Input the contents as a table to visualize#
      df <- reactive({data <-con()})

      output$contents <- DT::renderDataTable( con(),filter = 'top')

      #Download data#
      output$downloadData <- downloadHandler(
        filename = function() { paste("data", '.csv', sep='') },
        content = function(file) {
          write.csv(df(), file)
        })
    }

    #########################
    #          ui.R         #
    #########################

    ui <- fluidPage(
      titlePanel("Convert .sas/.xpt to .csv file"),
      sidebarLayout(
        sidebarPanel(
          fileInput('file1', 'Choose .sas or .xpt File',
                    accept=c( '.sas7bdat','.sas','.xpt')),

          tags$hr(),
          downloadButton('downloadData', 'Download.csv')

        ),mainPanel( DT::dataTableOutput("contents"))
      ))

    shinyApp(ui = ui, server = server)
  }
  else if(options=="nm_dataviz"){
    ui <- fluidPage(
      titlePanel("NONMEM dataset visualization"),
      sidebarLayout(
        sidebarPanel(
          fileInput('file1', 'Choose CSV File',
                    accept=c('text/csv','text/comma-separated-values,text/plain', '.csv')),


          tags$hr(),
          checkboxInput('header', 'Header', TRUE),
          radioButtons('sep', 'Separator',c(Comma=',',Tab='\t'),','),
          selectInput( "TYPE", "Output Type", c("Individual","All Subjects")), uiOutput("DOSE"),uiOutput("PERIOD"),
          conditionalPanel(condition = "input.TYPE == 'Individual'",uiOutput("ID"))),
        #tags$hr(),
        mainPanel(
          conditionalPanel(condition= "input.TYPE == 'Individual'", plotOutput("plotind", height=300,click = "plot_click"),h4("Data points"),
                           tableOutput("plot_clickedpoints1")),
          conditionalPanel(condition= "input.TYPE == 'All Subjects'",plotOutput("plotcum", height=300,click = "plot_click"),h4("Data points"),
                           tableOutput("plot_clickedpoints2"))
        ))
    )
    server <-function(input, output, session) {
      library(ggplot2)
      con<-reactive({
        inFile <- input$file1
        if (is.null(inFile))
          return(NULL)
        data_<- read.csv(inFile$datapath)
        #data_<-read.csv(paste(inpath))

        head(data_)
        df<- data.frame(names(data_))
        df$names.data_.<-trimws(df$names.data_.)
        names(df)[names(df) == 'names.data_.'] <- 'varlist'

        #Renaming the input filenames to match standard names used in the code below#

        df$id<-row.names(df)

        head(df)

        for (i in 1:nrow(df)) {
          s <- subset(df , df$id==i)
          s1<-nrow(s)
          ifelse( s1==1,names(data_)[names(data_) == 'subj'] <- 'subj',names(data_)[names(data_) == 'subj'] <- 'subj')
          ifelse( s1==1,names(data_)[names(data_) == 'SUBJID'] <- 'subj',names(data_)[names(data_) == 'subj'] <- 'subj')
          ifelse( s1==1,names(data_)[names(data_) == 'subjid'] <- 'subj',names(data_)[names(data_) == 'subj'] <- 'subj')
          ifelse( s1==1,names(data_)[names(data_) == 'usubjid'] <- 'subj',names(data_)[names(data_) == 'subj'] <- 'subj')
          ifelse( s1==1,names(data_)[names(data_) == 'USUBJID'] <- 'subj',names(data_)[names(data_) == 'subj'] <- 'subj')
          ifelse( s1==1,names(data_)[names(data_) == 'ID'] <- 'subj',names(data_)[names(data_) == 'subj'] <- 'subj')
          ifelse( s1==1,names(data_)[names(data_) == 'id'] <- 'subj',names(data_)[names(data_) == 'subj'] <- 'subj')
          ifelse( s1==1,names(data_)[names(data_) == 'time'] <- 'time',names(data_)[names(data_) == 'time'] <- 'time')
          ifelse( s1==1,names(data_)[names(data_) == 'TIME'] <- 'time',names(data_)[names(data_) == 'time'] <- 'time')
          ifelse( s1==1,names(data_)[names(data_) == 'ELMSTN'] <- 'time',names(data_)[names(data_) == 'time'] <- 'time')
          ifelse( s1==1,names(data_)[names(data_) == 'elmstn'] <- 'time',names(data_)[names(data_) == 'time'] <- 'time')
          ifelse( s1==1,names(data_)[names(data_) == 'RTFD'] <- 'time',names(data_)[names(data_) == 'time'] <- 'time')
          ifelse( s1==1,names(data_)[names(data_) == 'rtfd'] <- 'time',names(data_)[names(data_) == 'time'] <- 'time')
          ifelse( s1==1,names(data_)[names(data_) == 'conc'] <- 'conc',names(data_)[names(data_) == 'conc'] <- 'conc')
          ifelse( s1==1,names(data_)[names(data_) == 'CONC'] <- 'conc',names(data_)[names(data_) == 'conc'] <- 'conc')
          ifelse( s1==1,names(data_)[names(data_) == 'DV'] <- 'conc',names(data_)[names(data_) == 'conc'] <- 'conc')
          ifelse( s1==1,names(data_)[names(data_) == 'dv'] <- 'conc',names(data_)[names(data_) == 'conc'] <- 'conc')
          ifelse( s1==1,names(data_)[names(data_) == 'dose'] <- 'dose',names(data_)[names(data_) == 'dose'] <- 'dose')
          ifelse( s1==1,names(data_)[names(data_) == 'DOSE'] <- 'dose',names(data_)[names(data_) == 'dose'] <- 'dose')
          ifelse( s1==1,names(data_)[names(data_) == 'period'] <- 'period',names(data_)[names(data_) == 'period'] <- 'period')
          ifelse( s1==1,names(data_)[names(data_) == 'PERIOD'] <- 'period',names(data_)[names(data_) == 'period'] <- 'period')
          ifelse( s1==1,names(data_)[names(data_) == 'PER'] <- 'period',names(data_)[names(data_) == 'period'] <- 'period')
          ifelse( s1==1,names(data_)[names(data_) == 'per'] <- 'period',names(data_)[names(data_) == 'period'] <- 'period')
          ifelse( s1==1,names(data_)[names(data_) == 'COHORT'] <- 'period',names(data_)[names(data_) == 'period'] <- 'period')
          ifelse( s1==1,names(data_)[names(data_) == 'cohort'] <- 'period',names(data_)[names(data_) == 'period'] <- 'period')
          ifelse( s1==1,names(data_)[names(data_) == 'REG'] <- 'period',names(data_)[names(data_) == 'period'] <- 'period')
          ifelse( s1==1,names(data_)[names(data_) == 'reg'] <- 'period',names(data_)[names(data_) == 'period'] <- 'period')
          ifelse( s1==1,names(data_)[names(data_) == 'REGIMEN'] <- 'period',names(data_)[names(data_) == 'period'] <- 'period')
          ifelse( s1==1,names(data_)[names(data_) == 'regimen'] <- 'period',names(data_)[names(data_) == 'period'] <- 'period')
        }

        data<-data_
      })

      output$ID <- renderUI({
        df <-con()
        if (is.null(df)) return(NULL)
        subj<-unique(df$subj)
        selectInput("subj", "subj:",subj)})

      #uiOutput("dose")#
      output$DOSE <- renderUI({
        df <-con()
        if (is.null(df)) return(NULL)
        dose<-unique(df$dose)
        selectInput("dose", "dose:",dose)})

      output$PERIOD <- renderUI({
        df <-con()
        if (is.null(df)) return(NULL)
        period<-unique(df$period)
        selectInput("period", "period:",period)})
      #Individual Plots#
      output$plotind <- renderPlot({
        inFile <- input$file1
        if (is.null(inFile)==TRUE) {print(NULL) }
        else if (is.null(inFile)==FALSE) {

          #data<-read.csv(inFile$datapath)
          data_<- read.csv(inFile$datapath)
          #data_<-read.csv(paste(inpath))
          #data_<-read.csv("C:/Users/pk153230/Desktop/Theo_data.csv")
          head(data_)
          df<- data.frame(names(data_))
          df$names.data_.<-trimws(df$names.data_.)
          names(df)[names(df) == 'names.data_.'] <- 'varlist'

          #Renaming the input filenames to match standard names used in the code below#

          df$id<-row.names(df)

          head(df)

          for (i in 1:nrow(df)) {
            s <- subset(df , df$id==i)
            s1<-nrow(s)
            ifelse( s1==1,names(data_)[names(data_) == 'subj'] <- 'subj',names(data_)[names(data_) == 'subj'] <- 'subj')
            ifelse( s1==1,names(data_)[names(data_) == 'SUBJID'] <- 'subj',names(data_)[names(data_) == 'subj'] <- 'subj')
            ifelse( s1==1,names(data_)[names(data_) == 'subjid'] <- 'subj',names(data_)[names(data_) == 'subj'] <- 'subj')
            ifelse( s1==1,names(data_)[names(data_) == 'usubjid'] <- 'subj',names(data_)[names(data_) == 'subj'] <- 'subj')
            ifelse( s1==1,names(data_)[names(data_) == 'USUBJID'] <- 'subj',names(data_)[names(data_) == 'subj'] <- 'subj')
            ifelse( s1==1,names(data_)[names(data_) == 'ID'] <- 'subj',names(data_)[names(data_) == 'subj'] <- 'subj')
            ifelse( s1==1,names(data_)[names(data_) == 'id'] <- 'subj',names(data_)[names(data_) == 'subj'] <- 'subj')
            ifelse( s1==1,names(data_)[names(data_) == 'time'] <- 'time',names(data_)[names(data_) == 'time'] <- 'time')
            ifelse( s1==1,names(data_)[names(data_) == 'TIME'] <- 'time',names(data_)[names(data_) == 'time'] <- 'time')
            ifelse( s1==1,names(data_)[names(data_) == 'ELMSTN'] <- 'time',names(data_)[names(data_) == 'time'] <- 'time')
            ifelse( s1==1,names(data_)[names(data_) == 'elmstn'] <- 'time',names(data_)[names(data_) == 'time'] <- 'time')
            ifelse( s1==1,names(data_)[names(data_) == 'RTFD'] <- 'time',names(data_)[names(data_) == 'time'] <- 'time')
            ifelse( s1==1,names(data_)[names(data_) == 'rtfd'] <- 'time',names(data_)[names(data_) == 'time'] <- 'time')
            ifelse( s1==1,names(data_)[names(data_) == 'conc'] <- 'conc',names(data_)[names(data_) == 'conc'] <- 'conc')
            ifelse( s1==1,names(data_)[names(data_) == 'CONC'] <- 'conc',names(data_)[names(data_) == 'conc'] <- 'conc')
            ifelse( s1==1,names(data_)[names(data_) == 'DV'] <- 'conc',names(data_)[names(data_) == 'conc'] <- 'conc')
            ifelse( s1==1,names(data_)[names(data_) == 'dv'] <- 'conc',names(data_)[names(data_) == 'conc'] <- 'conc')
            ifelse( s1==1,names(data_)[names(data_) == 'dose'] <- 'dose',names(data_)[names(data_) == 'dose'] <- 'dose')
            ifelse( s1==1,names(data_)[names(data_) == 'DOSE'] <- 'dose',names(data_)[names(data_) == 'dose'] <- 'dose')
            ifelse( s1==1,names(data_)[names(data_) == 'period'] <- 'period',names(data_)[names(data_) == 'period'] <- 'period')
            ifelse( s1==1,names(data_)[names(data_) == 'PERIOD'] <- 'period',names(data_)[names(data_) == 'period'] <- 'period')
            ifelse( s1==1,names(data_)[names(data_) == 'PER'] <- 'period',names(data_)[names(data_) == 'period'] <- 'period')
            ifelse( s1==1,names(data_)[names(data_) == 'per'] <- 'period',names(data_)[names(data_) == 'period'] <- 'period')
            ifelse( s1==1,names(data_)[names(data_) == 'COHORT'] <- 'period',names(data_)[names(data_) == 'period'] <- 'period')
            ifelse( s1==1,names(data_)[names(data_) == 'cohort'] <- 'period',names(data_)[names(data_) == 'period'] <- 'period')
            ifelse( s1==1,names(data_)[names(data_) == 'REG'] <- 'period',names(data_)[names(data_) == 'period'] <- 'period')
            ifelse( s1==1,names(data_)[names(data_) == 'reg'] <- 'period',names(data_)[names(data_) == 'period'] <- 'period')
            ifelse( s1==1,names(data_)[names(data_) == 'REGIMEN'] <- 'period',names(data_)[names(data_) == 'period'] <- 'period')
            ifelse( s1==1,names(data_)[names(data_) == 'regimen'] <- 'period',names(data_)[names(data_) == 'period'] <- 'period')
          }

          data<-data_
          #data$DV<-as.numeric(data$DV)& data$MDV !=1
          data1 <- subset(data ,data$subj == input$subj & data$dose==input$dose & data$period==input$period  )
          p1<-ggplot(data1, aes(x=data1$time,y=data1$conc,color=factor(data1$subj))) + geom_point()+
            ggtitle("Individual Concentration Vs Time (Lin-Lin) ") +
            labs(x="Time in hours (RTFD)",y="Concentartion in ng/L (DV)")
          p2 <- p1 + geom_point(color="blue") + geom_line() + theme_bw() +
            theme(axis.line = element_line(colour = "black"),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  legend.position = "none")
          p2
        }})
      output$plotcum <- renderPlot({
        inFile <- input$file1
        if (is.null(inFile)==TRUE) {print(NULL) }
        else if (is.null(inFile)==FALSE) {

          data_<- read.csv(inFile$datapath)
          #data_<-read.csv(paste(inpath))
          head(data_)
          df<- data.frame(names(data_))
          df$names.data_.<-trimws(df$names.data_.)
          names(df)[names(df) == 'names.data_.'] <- 'varlist'

          #Renaming the input filenames to match standard names used in the code below#

          df$id<-row.names(df)

          head(df)

          for (i in 1:nrow(df)) {
            s <- subset(df , df$id==i)
            s1<-nrow(s)
            ifelse( s1==1,names(data_)[names(data_) == 'subj'] <- 'subj',names(data_)[names(data_) == 'subj'] <- 'subj')
            ifelse( s1==1,names(data_)[names(data_) == 'SUBJID'] <- 'subj',names(data_)[names(data_) == 'subj'] <- 'subj')
            ifelse( s1==1,names(data_)[names(data_) == 'subjid'] <- 'subj',names(data_)[names(data_) == 'subj'] <- 'subj')
            ifelse( s1==1,names(data_)[names(data_) == 'usubjid'] <- 'subj',names(data_)[names(data_) == 'subj'] <- 'subj')
            ifelse( s1==1,names(data_)[names(data_) == 'USUBJID'] <- 'subj',names(data_)[names(data_) == 'subj'] <- 'subj')
            ifelse( s1==1,names(data_)[names(data_) == 'ID'] <- 'subj',names(data_)[names(data_) == 'subj'] <- 'subj')
            ifelse( s1==1,names(data_)[names(data_) == 'id'] <- 'subj',names(data_)[names(data_) == 'subj'] <- 'subj')
            ifelse( s1==1,names(data_)[names(data_) == 'time'] <- 'time',names(data_)[names(data_) == 'time'] <- 'time')
            ifelse( s1==1,names(data_)[names(data_) == 'TIME'] <- 'time',names(data_)[names(data_) == 'time'] <- 'time')
            ifelse( s1==1,names(data_)[names(data_) == 'ELMSTN'] <- 'time',names(data_)[names(data_) == 'time'] <- 'time')
            ifelse( s1==1,names(data_)[names(data_) == 'elmstn'] <- 'time',names(data_)[names(data_) == 'time'] <- 'time')
            ifelse( s1==1,names(data_)[names(data_) == 'RTFD'] <- 'time',names(data_)[names(data_) == 'time'] <- 'time')
            ifelse( s1==1,names(data_)[names(data_) == 'rtfd'] <- 'time',names(data_)[names(data_) == 'time'] <- 'time')
            ifelse( s1==1,names(data_)[names(data_) == 'conc'] <- 'conc',names(data_)[names(data_) == 'conc'] <- 'conc')
            ifelse( s1==1,names(data_)[names(data_) == 'CONC'] <- 'conc',names(data_)[names(data_) == 'conc'] <- 'conc')
            ifelse( s1==1,names(data_)[names(data_) == 'DV'] <- 'conc',names(data_)[names(data_) == 'conc'] <- 'conc')
            ifelse( s1==1,names(data_)[names(data_) == 'dv'] <- 'conc',names(data_)[names(data_) == 'conc'] <- 'conc')
            ifelse( s1==1,names(data_)[names(data_) == 'dose'] <- 'dose',names(data_)[names(data_) == 'dose'] <- 'dose')
            ifelse( s1==1,names(data_)[names(data_) == 'DOSE'] <- 'dose',names(data_)[names(data_) == 'dose'] <- 'dose')
            ifelse( s1==1,names(data_)[names(data_) == 'period'] <- 'period',names(data_)[names(data_) == 'period'] <- 'period')
            ifelse( s1==1,names(data_)[names(data_) == 'PERIOD'] <- 'period',names(data_)[names(data_) == 'period'] <- 'period')
            ifelse( s1==1,names(data_)[names(data_) == 'PER'] <- 'period',names(data_)[names(data_) == 'period'] <- 'period')
            ifelse( s1==1,names(data_)[names(data_) == 'per'] <- 'period',names(data_)[names(data_) == 'period'] <- 'period')
            ifelse( s1==1,names(data_)[names(data_) == 'COHORT'] <- 'period',names(data_)[names(data_) == 'period'] <- 'period')
            ifelse( s1==1,names(data_)[names(data_) == 'cohort'] <- 'period',names(data_)[names(data_) == 'period'] <- 'period')
            ifelse( s1==1,names(data_)[names(data_) == 'REG'] <- 'period',names(data_)[names(data_) == 'period'] <- 'period')
            ifelse( s1==1,names(data_)[names(data_) == 'reg'] <- 'period',names(data_)[names(data_) == 'period'] <- 'period')
            ifelse( s1==1,names(data_)[names(data_) == 'REGIMEN'] <- 'period',names(data_)[names(data_) == 'period'] <- 'period')
            ifelse( s1==1,names(data_)[names(data_) == 'regimen'] <- 'period',names(data_)[names(data_) == 'period'] <- 'period')
          }

          data<-data_
          #data<-read.csv("W:/GSK2798745/TR4117387/Output/poppk_204725_19dec2016.csv")& data$MDV !=1& data$MDV !=1
          data1 <- subset(data , data$dose==input$dose & data$period==input$period  )
          # data1 <- subset(data , data$DOSE==2.4 & data$PERIOD==1 & data$MDV !=1 )& data$MDV !=1
          data1$conc<-as.numeric(data1$conc)
          p1<-ggplot(data1, aes(x=data1$time,y=data1$conc,color=factor(data1$subj))) + geom_point()+
            ggtitle("All Subjects Concentration Vs Time (Lin-Lin) ") +
            labs(x="Time in hours (RTFD)",y="Concentartion in ng/L (DV)")
          p2 <- p1 + geom_point(color="blue")+ geom_line() + theme_bw() +
            theme(axis.line = element_line(colour = "black"),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  legend.position = "none")
          p2
        }})
      con1<-reactive({


        data<-con()
        data1 <- subset(data ,data$subj == input$subj & data$dose==input$dose & data$period==input$period )
        # data1$DV<-as.numeric(data1$DV)
        data1
      })
      con2<-reactive({

        data<-con()
        data1 <- subset(data ,  data$dose==input$dose & data$period==input$period )
        #data1$DV<-as.numeric(data1$DV)
        data1
      })
      output$plot_clickedpoints1 <- renderTable({
        inFile <- input$file1
        if (is.null(inFile)==TRUE) {print(NULL) }
        else if (is.null(inFile)==FALSE) {
         res <- nearPoints(con1(), input$plot_click, "time","conc", )
          if (nrow(res) == 0)
            return()
          res
        }})
      output$plot_clickedpoints2 <- renderTable({
        inFile <- input$file1
        if (is.null(inFile)==TRUE) {print(NULL) }
        else if (is.null(inFile)==FALSE) {
         res <- nearPoints(con2(), input$plot_click, "time","conc", )
          if (nrow(res) == 0)
            return()
          res
        }})
    }
    shinyApp(ui = ui, server = server)
  }
}

#devtools::install()
#library(shinypkv)
#options="sas2csv"
#shiny_pkv(options="sas2csv")
#shiny_pkv(options="none")
#shiny_pkv(options="nm_dataviz")
