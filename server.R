library(shiny)
library(plotly)
library(tools)
library(rvest)
library(GregsOUPR6)

# server
shinyServer(function(input,output,session){

# instantiate objects ----
OUP <- OUProcess$new()
A <- OUP$get_Analytical()
FD <- OUP$get_FiniteDifference()
ML <- OUP$get_MaximumLikelihood()
MC <- OUP$get_MonteCarlo()
A$set_plot_info(theme="light",opaque=0.0,labels=FALSE)
# global variables for Maximum Likelihood and Data tabs
ouppath <- system.file(package="GregsOUPR6")
datapath <- paste(sep="",ouppath,"/data/")
htmlpath <- paste(sep="",getwd(),"/www/html/")
tutorialspath <- paste(sep="",getwd(),"/www/shinytutorials/OUP_ShinyTutorials.html")
ribbonpath <- paste(sep="",getwd(),"/www/ribbonhelp/OUP_Help.html")
uploadname <- "MyData"
uploadpath <- paste(sep="",datapath,"MyData.csv")
agrlist <- file_path_sans_ext(list.files(datapath,pattern="Agric_"))
clilist <- file_path_sans_ext(list.files(datapath,pattern="Climate_"))
ecolist <- file_path_sans_ext(list.files(datapath,pattern="Ecosys_"))
finlist <- file_path_sans_ext(list.files(datapath,pattern="Finance_"))
ouplist <- file_path_sans_ext(list.files(datapath,pattern="OUP_"))
filelist <- list(uploadname,`Ornstein-Uhlenbeck Process`=ouplist,Agriculture=agrlist,Climate=clilist,Ecosystems=ecolist,Finance=finlist)
df <- NULL
framenames <- NULL
nrows <- NULL
ncols <- NULL
nfirst <- NULL
nlast <- NULL
beg <- NULL
Ixbeg <- NULL
end <- NULL
Ixend <- NULL
first <- TRUE
initialize <- c(TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE)
bounce <- c(0,0,0)
dname <- c("data","data","data","data","data","data","data")
tname <- c("tau","tau","tau","tau","tau","tau","tau")
sname <- c("z","z","z","z","z","z","z")
lnL_params <- c(0,0,0)
LRT_params <- c(NA,NA,NA)

# events ----
  observeEvent(input$navBar,{
    # navBar ----
    if(input$navBar == "tabROOUP")
    {
      observeEvent(input$navROOUP,{
        # Data ----
        if(input$navROOUP == "RODataOUP")
        {
          # Define set functions ----
          DataInfo <- function()
          {
            output$descrRODataOUP <- renderUI({
              HTML(paste(sep="",
                "<table align='center'>
                  <tr>
                    <th style='text-align: right; padding: 2px; border-bottom: 1px solid grey;'>First</th>
                    <th style='text-align: right; padding: 2px; border-bottom: 1px solid grey;'>Last</th>
                    <th style='text-align: right; padding: 2px;>&emsp;'</th>
                    <th style='text-align: right; padding: 2px; border-bottom: 1px solid grey;'>Rows</th>
                    <th style='text-align: right; padding: 2px; border-bottom: 1px solid grey;'>Cols</th>
                  </tr>
                  <tr>
                    <td style='text-align: right; padding: 8px;'>",nfirst,"</td>
                    <td style='text-align: right; padding: 8px;'>",nlast,"</td>
                    <td style='text-align: right; padding: 8px;'>&emsp;</td>
                    <td style='text-align: right; padding: 8px;'>",nrows,"</td>
                    <td style='text-align: right; padding: 8px;'>",ncols,"</td>
                  </tr>
                </table>"
              ))
            })
          }
          FromR6toUI <- function()
          {
# message("Data FromR6toUI")
            if(first)
            {
# message("first")
              df <<- read.csv(uploadpath,fileEncoding="UTF-8-BOM")
              framenames <<- colnames(df)
              dname[1] <<- uploadname
              tname[1] <<- framenames[1]
              sname[1] <<- framenames[2]
              nrows <<- nrow(df)
              ncols <<- ncol(df)
              nfirst <<- df[1,1]
              nlast <<- df[nrows,1]
              series <- ML$set_timeseries(df=df,taucol=1,zcol=2)
              Ixend <<- nrow(series)
              end <<- series[Ixend,1]
              if(Ixend > 200) { Ixbeg <<- Ixend-200 }
              else { Ixbeg <<- 1 }
              beg <<- series[Ixbeg,1]
              ML$set_timeseries_info(tbeg=beg,tend=end,dataname=dname[1],timename=tname[1],statename=sname[1],NULL)
# df_info <- ML$get_timeseries_info()
# message(df_info$tbeg,df_info$tend,df_info$dataname,df_info$timename,df_info$statename)
              updateSelectInput(session,"filesRODataOUP",choices=filelist,selected=dname[1])
              updateSelectInput(session,"timeRODataOUP",choices=framenames,selected=tname[1])
              updateSelectInput(session,"stateRODataOUP",choices=framenames,selected=sname[1])
              DataInfo()
              first <<- FALSE
              initialize[6] <<- FALSE
              bounce[1] <<- 1
              bounce[2] <<- 1
              bounce[3] <<- 1
            }
            else if(initialize[6])
            {
# message("initialize")
              df_info <- ML$get_timeseries_info()
              dname[1] <<- df_info[[5]]
              tname[1] <<- df_info[[6]]
              sname[1] <<- df_info[[7]]
              updateSelectInput(session,"filesRODataOUP",choices=filelist,selected=dname[1])
              updateSelectInput(session,"timeRODataOUP",choices=framenames,selected=tname[1])
              updateSelectInput(session,"stateRODataOUP",choices=framenames,selected=sname[1])
              DataInfo()
              initialize[6] <<- FALSE
              bounce[1] <<- 1
              bounce[2] <<- 1
              bounce[3] <<- 1
            }
            else
            {
# message("else")
              df_info <- ML$get_timeseries_info()
              dataname <- df_info[[5]]
              timename <- df_info[[6]]
              statename <- df_info[[7]]
              if(dataname != dname[1] | timename != tname[1] | statename != sname[1])
              {
# message(dataname,", ",dname[1],", ",timename,", ",tname[1],", ",statename,", ",sname[1])
                updateSelectInput(session,"filesRODataOUP",choices=filelist,selected=dataname)
                updateSelectInput(session,"timeRODataOUP",choices=framenames,selected=timename)
                updateSelectInput(session,"stateRODataOUP",choices=framenames,selected=statename)
                DataInfo()
                bounce[1] <<- 1
                bounce[2] <<- 1
                bounce[3] <<- 1
                dname[1] <<- dataname
                tname[1] <<- timename
                sname[1] <<- statename
              }
            }
            updateNumericInput(session,"begRODataOUP",value=beg)
            updateNumericInput(session,"endRODataOUP",value=end)
          }
          # initialize ----
          FromR6toUI()
          # select ----
          observe({
# message("Data observe file")
# message(bounce[1])
            if(bounce[1] > 0) { bounce[1] <<- bounce[1]-1 }
            else
            {
              if(dname[1] != input$filesRODataOUP)
              {
# message(dname[1],", ",input$filesRODataOUP)
                dname[1] <<- input$filesRODataOUP
                if(dname[1] == uploadname) { filepath <- uploadpath }
                else { filepath <- paste(sep="",datapath,input$filesRODataOUP,".csv")  }
                df <<- read.csv(filepath,fileEncoding="UTF-8-BOM")
                framenames <<- colnames(df)
                tname[1] <<- framenames[1]
                sname[1] <<- framenames[2]
                nrows <<- nrow(df)
                ncols <<- ncol(df)
                nfirst <<- df[1,1]
                nlast <<- df[nrows,1]
                series <- ML$set_timeseries(df=df,taucol=1,zcol=2)
                Ixend <<- nrow(series)
                end <<- series[Ixend,1]
                if(Ixend > 200) { Ixbeg <<- Ixend-200 }
                else { Ixbeg <<- 1 }
                beg <<- series[Ixbeg,1]
                ML$set_timeseries_info(tbeg=beg,tend=end,dataname=dname[1],timename=tname[1],statename=sname[1],NULL)
# df_info <- ML$get_timeseries_info()
# message(df_info$tbeg,df_info$tend,df_info$dataname,df_info$timename,df_info$statename)
                updateSelectInput(session,"timeRODataOUP",choices=framenames,selected=tname[1])
                updateSelectInput(session,"stateRODataOUP",choices=framenames,selected=sname[1])
                updateNumericInput(session,"begRODataOUP",value=beg)
                updateNumericInput(session,"endRODataOUP",value=end)
                DataInfo()
                lnL_params[1] <<- 0
                lnL_params[2] <<- 0
                lnL_params[3] <<- 0
                bounce[2] <<- bounce[2]+1
                bounce[3] <<- bounce[3]+1
              }
            }
          }) %>% bindEvent(input$filesRODataOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
# message("Data observe time")
# message(bounce[2])
            if(bounce[2] > 0) { bounce[2] <<- bounce[2]-1 }
            else
            {
              if(tname[1] != input$timeRODataOUP)
              {
# message(tname[1],", ",input$timeRODataOUP)
                tname[1] <<- input$timeRODataOUP
                taucol <- match(tname[1],framenames)
                zcol <- match(sname[1],framenames)
                series <- ML$set_timeseries(df=df,taucol=taucol,zcol=zcol)
                Ixend <<- nrow(series)
                end <<- series[Ixend,1]
                if(Ixend > 200) { Ixbeg <<- Ixend-200 }
                else { Ixbeg <<- 1 }
                beg <<- series[Ixbeg,1]
                ML$set_timeseries_info(tbeg=beg,tend=end,dataname=dname[1],timename=tname[1],statename=sname[1],NULL)
# df_info <- ML$get_timeseries_info()
# message(df_info$tbeg,df_info$tend,df_info$dataname,df_info$timename,df_info$statename)
                updateNumericInput(session,"begRODataOUP",value=beg)
                updateNumericInput(session,"endRODataOUP",value=end)
                lnL_params[1] <<- 0
                lnL_params[2] <<- 0
                lnL_params[3] <<- 0
              }
            }
          }) %>% bindEvent(input$timeRODataOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
# message("Data observe state")
# message(bounce[3])
            if(bounce[3] > 0) { bounce[3] <<- bounce[3]-1 }
            else
            {
              if(sname[1] != input$stateRODataOUP)
              {
# message(sname[1],", ",input$stateRODataOUP)
                sname[1] <<- input$stateRODataOUP
                taucol <- match(tname[1],framenames)
                zcol <- match(sname[1],framenames)
                series <- ML$set_timeseries(df=df,taucol=taucol,zcol=zcol)
                Ixend <<- nrow(series)
                end <<- series[Ixend,1]
                if(Ixend > 200) { Ixbeg <<- Ixend-200 }
                else { Ixbeg <<- 1 }
                beg <<- series[Ixbeg,1]
                ML$set_timeseries_info(tbeg=beg,tend=end,dataname=dname[1],timename=tname[1],statename=sname[1],NULL)
# df_info <- ML$get_timeseries_info()
# message(df_info$tbeg,df_info$tend,df_info$dataname,df_info$timename,df_info$statename)
                updateNumericInput(session,"begRODataOUP",value=beg)
                updateNumericInput(session,"endRODataOUP",value=end)
                lnL_params[1] <<- 0
                lnL_params[2] <<- 0
                lnL_params[3] <<- 0
              }
            }
          }) %>% bindEvent(input$stateRODataOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # upload ----
          observe({
            uploadname <<- file_path_sans_ext(input$filesROUploadOUP$name)
            uploadpath <<- input$filesROUploadOUP$datapath
            filelist[1] <<- uploadname
# message(uploadname)
# print(filelist)
            first <<- TRUE
            FromR6toUI()
          }) %>% bindEvent(input$filesROUploadOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # Observe reset, begin and end ----
          resetButton <- reactiveVal(FALSE)
          beginInput <- reactiveVal(FALSE)
          endInput <- reactiveVal(FALSE)
          observe({
            resetButton(TRUE)
          }) %>% bindEvent(input$resetRODataOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            beginInput(TRUE)
          }) %>% bindEvent(input$begRODataOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            endInput(TRUE)
          }) %>% bindEvent(input$endRODataOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # plot ----
          observe({
# message("plot")
            if(resetButton())
            {
              begin <- -Inf
              endin <- Inf
              df_info <- ML$set_timeseries_info(tbeg=begin,tend=endin,NULL,NULL,NULL,NULL)
              updateNumericInput(session,"begRODataOUP",value=df_info$tbeg)
              updateNumericInput(session,"endRODataOUP",value=df_info$tend)
              beg <<- df_info$tbeg
              end <<- df_info$tend
            }
            else
            {
              if(beginInput())
              {
                begin <- input$begRODataOUP
                if(!is.numeric(begin)) { begin <- -Inf }
                df_info <- ML$set_timeseries_info(tbeg=begin,NULL,NULL,NULL,NULL,NULL)
                updateNumericInput(session,"begRODataOUP",value=df_info$tbeg)
                beg <<- df_info$tbeg
              }
              if(endInput())
              {
                endin <- input$endRODataOUP
                if(!is.numeric(endin)) { endin <- Inf }
                df_info <- ML$set_timeseries_info(NULL,tend=endin,NULL,NULL,NULL,NULL)
                updateNumericInput(session,"endRODataOUP",value=df_info$tend)
                end <<- df_info$tend
              }
            }
            resetButton(FALSE)
            beginInput(FALSE)
            endInput(FALSE)
            output$plotlyRODataOUP <- renderPlotly({ ML$PlotTimeSeries() })
          }) %>% bindEvent(input$resetRODataOUP,input$plotRODataOUP)
          # User clicks i ----
          observe({
            htmlname <- paste(sep="",htmlpath,input$filesRODataOUP,".html")
            if(!file.exists(htmlname)) { htmlname <- paste(sep="",htmlpath,"MyData.html") }
            rawtext <- read_html(htmlname)
            halo <- input$filesRODataOUP
            body <- html_element(rawtext,"body")
            gen1 <- html_children(body)
            gen2 <- html_children(gen1)
            m <- length(gen2)-2
            soul <- as.character(gen2[2:m])
            style <- "<style>h2 { font-size: 120% } h3 { font-size: 110% }</style>"
            showModal(modalDialog(
              title=div(img(src="Roar32x32.png"),halo),
              HTML(paste(sep="",style,soul)),
              easyClose = TRUE,
              footer = modalButton("Close"),
              size = "l"
            ))
          }) %>% bindEvent(input$fileinfoRODataOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # User clicks info ----
          observe({
            showModal(modalDialog(
              title=div(img(src="Roar32x32.png"),"Data"),
              HTML("The rate, location and scale parameters of the Ornstein-Uhlenbeck Process can be plucked out of the air, cogitated by experts, deduced from theory or estimated using data.<br><br>
              Data must be a time-series, with observations of times and states of nature.  Within the time-series, each observation has its own initial time and state, and its own terminal time and state.  Typically, the terminal time and state of one observation will be the initial time and state of the next observation.  Therefore, if measurements are taken at <i>m</i>  times, there will be <i>m</i>-1 observations.<br><br>
              Data is read from 'csv' (comma separated value) files.  Typically the files would be organized as in this table.
              <table style='margin-left: 60px;'>
                <tr>
                  <td style='border-top: solid silver; border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'><i>tau</i></td>
                  <td style='padding: 0px 4px 0px 4px;'><i>z</i><sub>1</sub></td>
                  <td style='padding: 0px 4px 0px 4px;'>&hellip;</td>
                  <td style='padding: 0px 4px 0px 4px;'><i>z</i><sub>n</sub></td>
                  <td style='border-top: solid silver; border-right: solid silver'>&nbsp;</td>
                </tr>
                <tr>
                  <td style='border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'>1</td>
                  <td style='padding: 0px 4px 0px 4px;'>16.3</td>
                  <td style='padding: 0px 4px 0px 4px;'>&hellip;</td>
                  <td style='padding: 0px 4px 0px 4px;'>12.7</td>
                  <td style='border-right: solid silver'>&nbsp;</td>
                </tr>
                <tr>
                  <td style='border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'>2</td>
                  <td style='padding: 0px 4px 0px 4px;'>5.1</td>
                  <td style='padding: 0px 4px 0px 4px;'>&hellip;</td>
                  <td style='padding: 0px 4px 0px 4px;'>13.9</td>
                  <td style='border-right: solid silver'>&nbsp;</td>
                </tr>
                <tr>
                  <td style='border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'>&nbsp;&vellip;</td>
                  <td style='padding: 0px 4px 0px 4px;'>&emsp;&vellip;</td>
                  <td style='padding: 0px 4px 0px 4px;'>&dtdot;</td>
                  <td style='padding: 0px 4px 0px 4px;'>&emsp;&vellip;</td>
                  <td style='border-right: solid silver'>&nbsp;</td>
                </tr>
                <tr>
                  <td style='border-bottom: solid silver; border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'><i>m</i></td>
                  <td style='padding: 0px 4px 0px 4px;'>14.3</td>
                  <td style='padding: 0px 4px 0px 4px;'>&hellip;</td>
                  <td style='padding: 0px 4px 0px 4px;'>8.9</td>
                  <td style='border-bottom: solid silver; border-right: solid silver'>&nbsp;</td>
                  <td style='padding-left: 2px;'>;</td>
                </tr>
              </table>
              Names are in the first row.  Numbers start in the second row.  Time is in the first column and states start in the second column.  There can be more than one time column.  There must be <i>m</i>+1 rows in all columns, but there can be blank elements if there is no measurment at that time.  Data is sorted by time and time intervals can be unequal.  Indeed, unequal time intervals seem to improve the estimation.<br><br>
              How the time intervals are measured affects the estimation of parameters <i>rho</i> and <i>sigma</i>.  For example, if measurements are taken once per year and time is reported in years, time interval <i>t-s</i> will be 1 year for a typical observation.  Parameter <i>rho</i> will likely range from 0.1 to 4.0 and <i>sigma</i> will range from 10 to 50.  If measurements are daily but time is reported in years, time interval <i>t-s</i> will be 1/365 years.  Parameter <i>rho</i> will be about 365 times larger and parameter <i>sigma</i> will be about (2<i>rho</i>)<sup>0.5</sup> times larger."),
              easyClose = TRUE,
              footer = modalButton("Close")
            ))
          }) %>% bindEvent(input$infoRODataOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
        # Estimates ----
        if(input$navROOUP == "ROEstimatesOUP")
        {
          # define set function ----
          FromR6toUI <- function()
          {
# message("Estimates FromR6toUI")
            if(first)
            {
# message("first")
              df <<- read.csv(uploadpath,fileEncoding="UTF-8-BOM")
              framenames <<- colnames(df)
              dname[2] <<- uploadname
              tname[2] <<- framenames[1]
              sname[2] <<- framenames[2]
              nrows <<- nrow(df)
              ncols <<- ncol(df)
              nfirst <<- df[1,1]
              nlast <<- df[nrows,1]
              series <- ML$set_timeseries(df=df,taucol=1,zcol=2)
              Ixend <<- nrow(series)
              end <<- series[Ixend,1]
              if(Ixend > 200) { Ixbeg <<- Ixend-200 }
              else { Ixbeg <<- 1 }
              beg <<- series[Ixbeg,1]
              ML$set_timeseries_info(tbeg=beg,tend=end,dataname=dname[2],timename=tname[2],statename=sname[2],NULL)
# df_info <- ML$get_timeseries_info()
# message(df_info$tbeg,df_info$tend,df_info$dataname,df_info$timename,df_info$statename)
              updateSelectInput(session,"filesROEstimatesOUP",choices=filelist,selected=dname[2])
              updateSelectInput(session,"timeROEstimatesOUP",choices=framenames,selected=tname[2])
              updateSelectInput(session,"stateROEstimatesOUP",choices=framenames,selected=sname[2])
              first <<- FALSE
              initialize[7] <<- FALSE
              bounce[1] <<- 1
              bounce[2] <<- 1
              bounce[3] <<- 1
            }
            else if(initialize[7])
            {
# message("initialize")
              df_info <- ML$get_timeseries_info()
              dname[2] <<- df_info[[5]]
              tname[2] <<- df_info[[6]]
              sname[2] <<- df_info[[7]]
              updateSelectInput(session,"filesROEstimatesOUP",choices=filelist,selected=dname[2])
              updateSelectInput(session,"timeROEstimatesOUP",choices=framenames,selected=tname[2])
              updateSelectInput(session,"stateROEstimatesOUP",choices=framenames,selected=sname[2])
              initialize[7] <<- FALSE
              bounce[1] <<- 1
              bounce[2] <<- 1
              bounce[3] <<- 1
            }
            else
            {
# message("else")
              df_info <- ML$get_timeseries_info()
              dataname <- df_info[[5]]
              timename <- df_info[[6]]
              statename <- df_info[[7]]
              if(dataname != dname[2] | timename != tname[2] | statename != sname[2])
              {
# message(dataname,", ",dname[2],", ",timename,", ",tname[2],", ",statename,", ",sname[2])
                updateSelectInput(session,"filesROEstimatesOUP",choices=filelist,selected=dataname)
                updateSelectInput(session,"timeROEstimatesOUP",choices=framenames,selected=timename)
                updateSelectInput(session,"stateROEstimatesOUP",choices=framenames,selected=statename)
                bounce[1] <<- 1
                bounce[2] <<- 1
                bounce[3] <<- 1
                dname[2] <<- dataname
                tname[2] <<- timename
                sname[2] <<- statename
              }
            }
            updateNumericInput(session,"begROEstimatesOUP",value=beg)
            updateNumericInput(session,"endROEstimatesOUP",value=end)
          }
          # initialize ----
          FromR6toUI()
          # select ----
          observe({
# message("Data and Estimates observe file")
# message(bounce[1])
            if(bounce[1] > 0) { bounce[1] <<- bounce[1]-1 }
            else
            {
              if(dname[2] != input$filesROEstimatesOUP)
              {
# message(dname[2],", ",input$filesROEstimatesOUP)
                dname[2] <<- input$filesROEstimatesOUP
                if(dname[2] == uploadname) { filepath <- uploadpath }
                else { filepath <- paste(sep="",datapath,input$filesROEstimatesOUP,".csv")  }
                df <<- read.csv(filepath,fileEncoding="UTF-8-BOM")
                framenames <<- colnames(df)
                tname[2] <<- framenames[1]
                sname[2] <<- framenames[2]
                nrows <<- nrow(df)
                ncols <<- ncol(df)
                nfirst <<- df[1,1]
                nlast <<- df[nrows,1]
                series <- ML$set_timeseries(df=df,taucol=1,zcol=2)
                Ixend <<- nrow(series)
                end <<- series[Ixend,1]
                if(Ixend > 200) { Ixbeg <<- Ixend-200 }
                else { Ixbeg <<- 1 }
                beg <<- series[Ixbeg,1]
                ML$set_timeseries_info(tbeg=beg,tend=end,dataname=dname[2],timename=tname[2],statename=sname[2])
# df_info <- ML$get_timeseries_info()
# message(df_info$tbeg,df_info$tend,df_info$dataname,df_info$timename,df_info$statename)
                updateSelectInput(session,"timeROEstimatesOUP",choices=framenames,selected=tname[2])
                updateSelectInput(session,"stateROEstimatesOUP",choices=framenames,selected=sname[2])
                updateNumericInput(session,"begROEstimatesOUP",value=beg)
                updateNumericInput(session,"endROEstimatesOUP",value=end)
                lnL_params[1] <<- 0
                lnL_params[2] <<- 0
                lnL_params[3] <<- 0
                LRT_params[1] <<- NA
                LRT_params[2] <<- NA
                LRT_params[3] <<- NA
                bounce[2] <<- bounce[2]+1
                bounce[3] <<- bounce[3]+1
              }
            }
          }) %>% bindEvent(input$filesROEstimatesOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
# message("Estimates observe time")
# message(bounce[2])
            if(bounce[2] > 0) { bounce[2] <<- bounce[2]-1 }
            else
            {
              if(tname[2] != input$timeROEstimatesOUP)
              {
# message(tname[2],", ",input$timeROEstimatesOUP)
                tname[2] <<- input$timeROEstimatesOUP
                taucol <- match(tname[2],framenames)
                zcol <- match(sname[2],framenames)
                series <- ML$set_timeseries(df=df,taucol=taucol,zcol=zcol)
                Ixend <<- nrow(series)
                end <<- series[Ixend,1]
                if(Ixend > 200) { Ixbeg <<- Ixend-200 }
                else { Ixbeg <<- 1 }
                beg <<- series[Ixbeg,1]
                ML$set_timeseries_info(tbeg=beg,tend=end,dataname=dname[2],timename=tname[2],statename=sname[2],NULL)
# df_info <- ML$get_timeseries_info()
# message(df_info$tbeg,df_info$tend,df_info$dataname,df_info$timename,df_info$statename)
                updateNumericInput(session,"begROEstimatesOUP",value=beg)
                updateNumericInput(session,"endROEstimatesOUP",value=end)
                lnL_params[1] <<- 0
                lnL_params[2] <<- 0
                lnL_params[3] <<- 0
                LRT_params[1] <<- NA
                LRT_params[2] <<- NA
                LRT_params[3] <<- NA
              }
            }
          }) %>% bindEvent(input$timeROEstimatesOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
# message("Estimates observe state")
# message(bounce[3])
            if(bounce[3] > 0) { bounce[3] <<- bounce[3]-1 }
            else
            {
              if(sname[2] != input$stateROEstimatesOUP)
              {
# message(sname[2],", ",input$stateROEstimatesOUP)
                sname[2] <<- input$stateROEstimatesOUP
                taucol <- match(tname[2],framenames)
                zcol <- match(sname[2],framenames)
                series <- ML$set_timeseries(df=df,taucol=taucol,zcol=zcol)
                Ixend <<- nrow(series)
                end <<- series[Ixend,1]
                if(Ixend > 200) { Ixbeg <<- Ixend-200 }
                else { Ixbeg <<- 1 }
                beg <<- series[Ixbeg,1]
                ML$set_timeseries_info(tbeg=beg,tend=end,dataname=dname[2],timename=tname[2],statename=sname[2],NULL)
# df_info <- ML$get_timeseries_info()
# message(df_info$tbeg,df_info$tend,df_info$dataname,df_info$timename,df_info$statename)
                lnL_params[1] <<- 0
                lnL_params[2] <<- 0
                lnL_params[3] <<- 0
                LRT_params[1] <<- NA
                LRT_params[2] <<- NA
                LRT_params[3] <<- NA
              }
            }
          }) %>% bindEvent(input$stateROEstimatesOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # observe reset, begin and end ----
          resetButton <- reactiveVal(FALSE)
          beginInput <- reactiveVal(FALSE)
          endInput <- reactiveVal(FALSE)
          observe({
            resetButton(TRUE)
          }) %>% bindEvent(input$resetROEstimatesOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            beginInput(TRUE)
          }) %>% bindEvent(input$begROEstimatesOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            endInput(TRUE)
          }) %>% bindEvent(input$endROEstimatesOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # go ----
          observe({
            if(resetButton())
            {
              begin <- -Inf
              endin <- Inf
              timeseries_info <- ML$set_timeseries_info(tbeg=begin,tend=endin,NULL,NULL,NULL,NULL)
              updateNumericInput(session,"begROEstimatesOUP",value=timeseries_info$tbeg)
              updateNumericInput(session,"endROEstimatesOUP",value=timeseries_info$tend)
              beg <<- timeseries_info$tbeg
              end <<- timeseries_info$tend
            }
            else
            {
              if(beginInput())
              {
                begin <- input$begROEstimatesOUP
                if(!is.numeric(begin)) { begin <- -Inf }
                df_info <- ML$set_timeseries_info(tbeg=begin,NULL,NULL,NULL,NULL,NULL)
                updateNumericInput(session,"begROEstimatesOUP",value=df_info$tbeg)
                beg <<- df_info$tbeg
              }
              if(endInput())
              {
                endin <- input$endROEstimatesOUP
                if(!is.numeric(endin)) { endin <- Inf }
                df_info <- ML$set_timeseries_info(NULL,tend=endin,NULL,NULL,NULL,NULL)
                updateNumericInput(session,"endROEstimatesOUP",value=df_info$tend)
                end <<- df_info$tend
              }
            }
            resetButton(FALSE)
            beginInput(FALSE)
            endInput(FALSE)
            est <- ML$Estimates(plotit=FALSE)
            rho <- format(est[[1]],digits=6)
            mu <- format(est[[2]],digits=6)
            sigma <- format(est[[3]],digits=6)
            lnL_params[1] <<- est[[1]]
            lnL_params[2] <<- est[[2]]
            lnL_params[3] <<- est[[3]]
            output$paramROEstimatesOUP <- renderUI({
              HTML(paste(sep="",
                "<table align='center'>
                  <tr style='border-bottom: 1px solid grey;'>
                    <th></th>
                    <th style='text-align: right; padding: 2px 6px 2px 8px;'>rho</th>
                    <th style='text-align: right; padding: 2px 6px 2px 6px;'>mu</th>
                    <th style='text-align: right; padding: 2px 6px 2px 6px;'>sigma</th>
                  </tr>
                  <tr>
                    <td style='text-align: right; padding: 8px 6px 8px 8px;'><b>Parameters:</b></td>
                    <td style='text-align: right; padding: 8px 6px 8px 6px;'>",rho,"</td>
                    <td style='text-align: right; padding: 8px 6px 8px 6px;'>",mu,"</td>
                    <td style='text-align: right; padding: 8px 6px 8px 6px;'>",sigma,"</td>
                  </tr>
                </table>"
              ))
            })
            output$plotlyROEstimatesOUP <- renderPlotly({ ML$PlotEstimates() })
          }) %>% bindEvent(input$resetROEstimatesOUP,input$plotROEstimatesOUP)
          # User clicks i ----
          observe({
            htmlname <- paste(sep="",htmlpath,input$filesROEstimatesOUP,".html")
            if(!file.exists(htmlname)) { htmlname <- paste(sep="",htmlpath,"MyData.html") }
            rawtext <- read_html(htmlname)
            halo <- input$filesROEstimatesOUP
            body <- html_element(rawtext,"body")
            gen1 <- html_children(body)
            gen2 <- html_children(gen1)
            m <- length(gen2)-2
            soul <- as.character(gen2[2:m])
            style <- "<style>h2 { font-size: 120% } h3 { font-size: 110% }</style>"
            showModal(modalDialog(
              title=div(img(src="Roar32x32.png"),halo),
              HTML(paste(sep="",style,soul)),
              easyClose = TRUE,
              footer = modalButton("Close"),
              size = "l"
            ))
          }) %>% bindEvent(input$fileinfoROEstimatesOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # User clicks info ----
          observe({
            showModal(modalDialog(
              title=div(img(src="Roar32x32.png"),"Estimates"),
              HTML("If you know the parameters of the Ornstein-Uhlenbeck Process, you can enter them directly.  If you have data, you can use it to estimate the parameters.  Maximum Likelihood Estimation finds the rate, location and scale parameters of the Ornstein-Uhlenbeck Process which maximize the Likelihood of observing the data as a random sample.<br><br>
              &emsp;&emsp;Arguments:<br>
              &emsp;&emsp;&emsp;<i>tau</i> are times;<br>
              &emsp;&emsp;&emsp;<i>z</i> are states.<br>
              &emsp;&emsp;Returns:<br>
              &emsp;&emsp;&emsp;<i>rho</i> is the rate parameter;<br>
              &emsp;&emsp;&emsp;<i>mu</i> is the location parameter;<br>
              &emsp;&emsp;&emsp;<i>sigma</i> is the scale parameter."),
              easyClose = TRUE,
              footer = tagList(actionButton("moreROEstimatesOUP","More",class="btn-primary",title="Maximum Likelihood Data"),modalButton("Close")),
            ))
          }) %>% bindEvent(input$infoROEstimatesOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            removeModal()
            updateTabsetPanel(session,"navBar",selected="tabMLOUP")
            updateTabsetPanel(session,"navMLOUP",selected="MLEstimatesOUP")
          }) %>% bindEvent(input$moreROEstimatesOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
        # Regime ----
        else if(input$navROOUP == "RORegimeOUP")
        {
          # Define set/get functions ----
          FromR6toUI <- function()
          {
            # Get from OUP ----
            oup_params <- A$get_oup_params()
            x_stoch_args <- A$get_x_stoch_args()
            rho <-oup_params[[1]]
            mu <- oup_params[[2]]
            sigma <- oup_params[[3]]
            x <- x_stoch_args[[2]]
            y <- x_stoch_args[[4]]
            r <- x_stoch_args[[5]]
            phi <- x_stoch_args[[6]]
            b <- x_stoch_args[[7]]
            c <- x_stoch_args[[8]]
            n <- length(x)
            xFrom <- x[1]
            xTo <- x[n]
            if(n > 1) { xBy <- (xTo-xFrom)/(n-1) }
            else  {xBy <- 0 }
            # Set to UI ----
            updateNumericInput(session,"rhoRORegimeOUP",value=rho)
            updateNumericInput(session,"muRORegimeOUP",value=mu)
            updateNumericInput(session,"sigmaRORegimeOUP",value=sigma)
            updateNumericInput(session,"xFromRORegimeOUP",value=xFrom)
            updateNumericInput(session,"xToRORegimeOUP",value=xTo)
            updateNumericInput(session,"xByRORegimeOUP",value=xBy)
            updateNumericInput(session,"yRORegimeOUP",value=y)
            updateNumericInput(session,"rRORegimeOUP",value=r)
            updateNumericInput(session,"phiRORegimeOUP",value=phi)
            updateNumericInput(session,"bRORegimeOUP",value=b)
            updateNumericInput(session,"cRORegimeOUP",value=c)
          }
          FromUItoR6 <- function()
          {
            # Get from UI ----
            rho <- input$rhoRORegimeOUP
            if(!is.numeric(rho)) { rho <- 0 }
            else if(rho < 0) { rho <- 0 }
            mu <- input$muRORegimeOUP
            if(!is.numeric(mu)) { mu <- 0 }
            sigma <- input$sigmaRORegimeOUP
            if(!is.numeric(sigma)) { sigma <- 0 }
            xFrom <- input$xFromRORegimeOUP
            xTo <- input$xToRORegimeOUP
            xBy <- input$xByRORegimeOUP
            xOK <- FALSE
            if(is.numeric(xFrom) & is.numeric(xTo))
            {
              if(xTo > xFrom)
              {
                if(is.numeric(xBy))
                {
                  if(xBy > (xTo-xFrom)/100) { xBy <- (xTo-xFrom)/100 }
                  else if(xBy < (xTo-xFrom)/1000) { xBy <- (xTo-xFrom)/1000 }
                }
                else { xBy <- (xTo-xFrom)/100 }
                xOK <- TRUE
              }
            }
            else if(is.numeric(xFrom) & is.numeric(xBy))
            {
              if(xBy > 0)
              {
                xTo <- xFrom+100*xBy
                xOK <- TRUE
              }
            }
            else if(is.numeric(xTo) & is.numeric(xBy))
            {
              if(xBy > 0)
              {
                xFrom <- xTo-100*xBy
                xOK <- TRUE
              }
            }
            y <- input$yRORegimeOUP
            if(!is.numeric(y)) { y <- 0 }
            r <- input$rRORegimeOUP
            if(!is.numeric(r)) { r <- 0 }
            phi <- input$phiRORegimeOUP
            if(!is.numeric(phi)) { phi <- -1 }
            else if(phi < 0) { phi <- -1 }
            else if(phi > 0) { phi <- 1 }
            b <- input$bRORegimeOUP
            if(!is.numeric(b)) { b <- 0 }
            c <- input$cRORegimeOUP
            if(!is.numeric(c)) { c <- 0 }
            # Set to OUP ----
            A$set_oup_params(rho=rho,mu=mu,sigma=sigma)
            if(xOK)
            {
              x <- seq(from=xFrom,to=xTo,by=xBy)
              A$set_x_stoch_args(x=x,y=y,r=r,phi=phi,b=b,c=c)
            }
            else
            {
              A$axes_x_stoch()
              A$set_x_stoch_args(y=y,r=r,phi=phi,b=b,c=c)
            }
          }
          # Initialize ----
          FromR6toUI()
          # Observe undo, axes, plot (or enter key), left and rght ----
          undoButton <- reactiveVal(FALSE)
          axesButton <- reactiveVal(FALSE)
          plotButton <- reactiveVal(FALSE)
          leftButton <- reactiveVal(FALSE)
          rghtButton <- reactiveVal(FALSE)
          observe({
            undoButton(TRUE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$undoRORegimeOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            axesButton(TRUE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$axesRORegimeOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            axesButton(FALSE)
            plotButton(TRUE)
            leftButton(FALSE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$plotRORegimeOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(TRUE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$leftRORegimeOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(TRUE)
          }) %>% bindEvent(input$rghtRORegimeOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # User clicks save ----
          observe({
            FromUItoR6()
            A$default_save()
          }) %>% bindEvent(input$saveRORegimeOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # User clicks undo,  axes, plot (or enter key), left or rght ----
          output$plotlyRORegimeOUP <- renderPlotly({
            if(undoButton()) { A$default_read() }
            else if(axesButton())
            {
              FromUItoR6()
              A$axes_x_stoch()
            }
            else if(plotButton()) { FromUItoR6()  }
            else if(leftButton())
            {
              FromUItoR6()
              phi <- A$get_x_stoch_args()[[6]]
              phi <- phi-1
              if(phi < -1) { phi <- 1 }
              A$set_x_stoch_args(phi=phi)
            }
            else if(rghtButton())
            {
              FromUItoR6()
              phi <- A$get_x_stoch_args()[[6]]
              phi <- phi+1
              if(phi > 1) { phi <- -1 }
              A$set_x_stoch_args(phi=phi)
            }
            undoButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(FALSE)
            FromR6toUI()
            phi <- A$get_x_stoch_args()[[6]]
            if(phi < 0) { A$PlotOption(title="Exit Option",type=2) }
            else if(phi == 0) { A$PlotObligation(title="Obligation",type=2) }
            else { A$PlotOption(title="Entry Option",type=2) }
          }) %>% bindEvent(input$undoRORegimeOUP,input$axesRORegimeOUP,input$plotRORegimeOUP,input$leftRORegimeOUP,input$rghtRORegimeOUP)
          # User clicks info ----
          observe({
            showModal(modalDialog(
              title=div(img(src="Roar32x32.png"),"Regime"),
              HTML("A Regime is a benefit/cost analysis with options.  The benefit/cost analysis is called an Obligation.  It is how benefits are gained and costs are lost.  An Obligation is linear in the state of nature and, hence, certain.  The options are Exit and Entry Options.   Options value the flexibility to exit from and enter into an Obligation.  Exit and Entry Options are highly convex and, hence, uncertain.<br><br>
              A Regime consists of an Entry Option, an Obligation and an Exit Option.  An Entry Option without an Exit Option is an Obligation.  Exercising an Exit Option eliminates the Obligation.<br><br>
	            Entry and Exit Options are perpetual options.  There is no fixed expiry date.  If the value of flexibility exceeds the benefits to be gained or the costs being lost, decision-makers will keep their options open.  Otherwise, they will exercise one of their options.<br><br>
              &emsp;&emsp;Arguments:<br>
              &emsp;&emsp;&emsp;<i>rho</i> is the rate parameter;<br>
              &emsp;&emsp;&emsp;<i>mu</i> is the location parameter;<br>
              &emsp;&emsp;&emsp;<i>sigma</i> is the scale parameter;<br>
              &emsp;&emsp;&emsp;<i>y</i> is the break-even point;<br>
              &emsp;&emsp;&emsp;<i>r</i> is the discount rate;<br>
              &emsp;&emsp;&emsp;<i>phi</i> is < 0 for an Exit Option, > 0 for an Entry Option;<br>
              &emsp;&emsp;&emsp;<i>b</i> is a benefit or subsidy for an Entry Option;<br>
              &emsp;&emsp;&emsp;<i>c</i> is a cost or tax for an Exit Option.<br><br>
	            Policies may alter the arguments to advance or delay entry and exit decisions.<br>
              &emsp;1)  A contract may require farmers to plant trees and never cut them, eliminating the Exit Option.<br>
              &emsp;2)  Farmers may be prohibited from ever planting poppies, eliminating the Entry Option.<br>
              &emsp;3)  A business is obligated to pay fixed costs, delaying exit.<br>
              &emsp;4)  Subsidising the installation of solar panels advances entry.<br>
              &emsp;5)  Purchasing excess power from solar panels reduces the break-even point, advancing entry and delaying exit.<br>
              &emsp;6)  An input tax reduces the location parameter, delaying entry and advancing exit.<br>
              &emsp;7)  Insurance decreases the scale parameter, advancing both entry and exit.<br>
              &emsp;8)  Subsidising interest rates for farmers in a drought delays exit."),
              easyClose = TRUE,
              footer = tagList(actionButton("moreRORegimeOUP","More",class="btn-primary",title="Analytical Option"),modalButton("Close")),
              size = "l"
            ))
          }) %>% bindEvent(input$infoRORegimeOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            removeModal()
            updateTabsetPanel(session,"navBar",selected="tabAOUP")
            updateTabsetPanel(session,"navAOUP",selected="AOptionOUP")
          }) %>% bindEvent(input$moreRORegimeOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
        # Decision Threshold ----
        else if(input$navROOUP == "RODecisionOUP")
        {
          # Define set/get functions ----
          FromR6toUI <- function()
          {
            # Get from OUP ----
            oup_params <- A$get_oup_params()
            x_stoch_args <- A$get_x_stoch_args()
            rho <-oup_params[[1]]
            mu <- oup_params[[2]]
            sigma <- oup_params[[3]]
            x <- x_stoch_args[[2]]
            y <- x_stoch_args[[4]]
            r <- x_stoch_args[[5]]
            phi <- x_stoch_args[[6]]
            b <- x_stoch_args[[7]]
            c <- x_stoch_args[[8]]
            n <- length(x)
            xFrom <- x[1]
            xTo <- x[n]
            if(n > 1) { xBy <- (xTo-xFrom)/(n-1) }
            else  {xBy <- 0 }
            # Set to UI ----
            updateNumericInput(session,"rhoRODecisionOUP",value=rho)
            updateNumericInput(session,"muRODecisionOUP",value=mu)
            updateNumericInput(session,"sigmaRODecisionOUP",value=sigma)
            updateNumericInput(session,"xFromRODecisionOUP",value=xFrom)
            updateNumericInput(session,"xToRODecisionOUP",value=xTo)
            updateNumericInput(session,"xByRODecisionOUP",value=xBy)
            updateNumericInput(session,"yRODecisionOUP",value=y)
            updateNumericInput(session,"rRODecisionOUP",value=r)
            updateNumericInput(session,"phiRODecisionOUP",value=phi)
            updateNumericInput(session,"bRODecisionOUP",value=b)
            updateNumericInput(session,"cRODecisionOUP",value=c)
          }
          FromUItoR6 <- function()
          {
            # Get from UI ----
            rho <- input$rhoRODecisionOUP
            if(!is.numeric(rho)) { rho <- 0 }
            else if(rho < 0) { rho <- 0 }
            mu <- input$muRODecisionOUP
            if(!is.numeric(mu)) { mu <- 0 }
            sigma <- input$sigmaRODecisionOUP
            if(!is.numeric(sigma)) { sigma <- 0 }
            xFrom <- input$xFromRODecisionOUP
            xTo <- input$xToRODecisionOUP
            xBy <- input$xByRODecisionOUP
            xOK <- FALSE
            if(is.numeric(xFrom) & is.numeric(xTo))
            {
              if(xTo > xFrom)
              {
                if(is.numeric(xBy))
                {
                  if(xBy > (xTo-xFrom)/100) { xBy <- (xTo-xFrom)/100 }
                  else if(xBy < (xTo-xFrom)/1000) { xBy <- (xTo-xFrom)/1000 }
                }
                else { xBy <- (xTo-xFrom)/100 }
                xOK <- TRUE
              }
            }
            else if(is.numeric(xFrom) & is.numeric(xBy))
            {
              if(xBy > 0)
              {
                xTo <- xFrom+100*xBy
                xOK <- TRUE
              }
            }
            else if(is.numeric(xTo) & is.numeric(xBy))
            {
              if(xBy > 0)
              {
                xFrom <- xTo-100*xBy
                xOK <- TRUE
              }
            }
            y <- input$yRODecisionOUP
            if(!is.numeric(y)) { y <- 0 }
            r <- input$rRODecisionOUP
            if(!is.numeric(r)) { r <- 0 }
            phi <- input$phiRODecisionOUP
            if(!is.numeric(phi)) { phi <- -1 }
            else if(phi < 0) { phi <- -1 }
            else if(phi > 0) { phi <- 1 }
            b <- input$bRODecisionOUP
            if(!is.numeric(b)) { b <- 0 }
            c <- input$cRODecisionOUP
            if(!is.numeric(c)) { c <- 0 }
            # Set to OUP ----
            A$set_oup_params(rho=rho,mu=mu,sigma=sigma)
            if(xOK)
            {
              x <- seq(from=xFrom,to=xTo,by=xBy)
              A$set_x_stoch_args(x=x,y=y,r=r,phi=phi,b=b,c=c)
            }
            else
            {
              A$axes_x_stoch()
              A$set_x_stoch_args(y=y,r=r,phi=phi,b=b,c=c)
            }
          }
          # Initialize ----
          FromR6toUI()
          # Observe undo, sync, axes, plot (or enter key), left or rght ----
          undoButton <- reactiveVal(FALSE)
          syncButton <- reactiveVal(FALSE)
          axesButton <- reactiveVal(FALSE)
          plotButton <- reactiveVal(FALSE)
          leftButton <- reactiveVal(FALSE)
          rghtButton <- reactiveVal(FALSE)
          observe({
            undoButton(TRUE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$undoRODecisionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(TRUE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$syncRODecisionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(TRUE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$axesRODecisionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(TRUE)
            leftButton(FALSE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$plotRODecisionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(TRUE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$leftRODecisionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(TRUE)
          }) %>% bindEvent(input$rghtRODecisionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # User clicks save ----
          observe({
            FromUItoR6()
            A$default_save()
          }) %>% bindEvent(input$saveRODecisionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # User clicks undo, sync, axes, plot (or enter key), left or rght ----
          output$plotlyRODecisionOUP <- renderPlotly({
            if(undoButton()) { A$default_read() }
            else if(syncButton())
            {
              FromUItoR6()
              A$sync_zyxt_stoch()
            }
            else if(axesButton())
            {
              FromUItoR6()
              A$axes_x_stoch()
            }
            else if(plotButton()) { FromUItoR6() }
            else if(leftButton())
            {
              FromUItoR6()
              phi <- A$get_x_stoch_args()[[6]]
              t_stoch_args <- A$get_t_stoch_args()
              phi <- phi-1
              if(phi < -1) { phi <- 1 }
              else if(phi > -1) { phi <- -1 }
              k <- t_stoch_args[[2]]
              x <- t_stoch_args[[4]]
              A$set_x_stoch_args(phi=phi)
              A$set_t_stoch_args(k=x,x=k)
            }
            else if(rghtButton())
            {
              FromUItoR6()
              phi <- A$get_x_stoch_args()[[6]]
              t_stoch_args <- A$get_t_stoch_args()
              phi <- phi+1
              if(phi > 1) { phi <- -1 }
              else if(phi < 1) { phi <- 1 }
              k <- t_stoch_args[[2]]
              x <- t_stoch_args[[4]]
              A$set_x_stoch_args(phi=phi)
              A$set_t_stoch_args(k=x,x=k)
            }
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(FALSE)
            FromR6toUI()
            A$PlotDecisionThreshold()
          }) %>% bindEvent(input$undoRODecisionOUP,input$syncRODecisionOUP,input$axesRODecisionOUP,input$plotRODecisionOUP,input$leftRODecisionOUP,input$rghtRODecisionOUP)
          # User clicks info ----
          observe({
            showModal(modalDialog(
              title=div(img(src="Roar32x32.png"),"Decision Threshold"),
              HTML("The Decision Threshold is the state of the system where a decision-maker will be indifferent between holding or exercising an Entry or Exit Option.  The Option value at the threshold is the price of flexibility&mdash;the price of keeping options open.  It is the most a decision-maker will pay in costs rather than exit prematurely, or the most a decision-maker will forego in benefits rather than enter prematurely.<br><br>
              &emsp;&emsp;Arguments:<br>
              &emsp;&emsp;&emsp;<i>y</i> is the break-even point;<br>
              &emsp;&emsp;&emsp;<i>rho</i> is the rate parameter;<br>
              &emsp;&emsp;&emsp;<i>mu</i> is the location parameter;<br>
              &emsp;&emsp;&emsp;<i>sigma</i> is the scale parameter;<br>
              &emsp;&emsp;&emsp;<i>r</i> is the discount rate;<br>
              &emsp;&emsp;&emsp;<i>phi</i> is < 0 for an Exit Option, > 0 for an Entry Option;<br>
              &emsp;&emsp;&emsp;<i>b</i> is a benefit or subsidy for an Entry Option;<br>
              &emsp;&emsp;&emsp;<i>c</i> is a cost or tax for an Exit Option.<br>
              &emsp;&emsp;Returns:<br>
              &emsp;&emsp;&emsp;<i>k</i> is the state at the Decision Threshold;<br>
              &emsp;&emsp;&emsp;\u00D4 is the Option at the Decision Threshold."),
              easyClose = TRUE,
              footer = tagList(actionButton("moreRODecisionOUP","More",class="btn-primary",title="Analytical Decision Threshold"),modalButton("Close")),
            ))
          }) %>% bindEvent(input$infoRODecisionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            removeModal()
            updateTabsetPanel(session,"navBar",selected="tabAOUP")
            updateTabsetPanel(session,"navAOUP",selected="ADecisionOUP")
          }) %>% bindEvent(input$moreRODecisionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
        # Passage Time ----
        else if(input$navROOUP == "ROPassageTimeOUP")
        {
          # Define set/get functions ----
          FromR6toUI <- function()
          {
            # Get from OUP ----
            oup_params <- A$get_oup_params()
            t_stoch_args <- A$get_t_stoch_args()
            rho <- oup_params[[1]]
            mu <- oup_params[[2]]
            sigma <- oup_params[[3]]
            k <- t_stoch_args[[2]]
            s <- t_stoch_args[[3]]
            x <- t_stoch_args[[4]]
            z <- t_stoch_args[[5]]
            omega <- t_stoch_args[[6]]
            Ppct <- t_stoch_args[[7]]
            n <- length(z)
            zFrom <- z[1]
            zTo <- z[n]
            if(n > 1) { zBy <- (zTo-zFrom)/(n-1) }
            else  {zBy <- 0 }
            # Set to UI ----
            updateNumericInput(session,"rhoROPassageTimeOUP",value=rho)
            updateNumericInput(session,"muROPassageTimeOUP",value=mu)
            updateNumericInput(session,"sigmaROPassageTimeOUP",value=sigma)
            updateNumericInput(session,"zFromROPassageTimeOUP",value=zFrom)
            updateNumericInput(session,"zToROPassageTimeOUP",value=zTo)
            updateNumericInput(session,"zByROPassageTimeOUP",value=zBy)
            updateNumericInput(session,"kROPassageTimeOUP",value=k)
            updateNumericInput(session,"sROPassageTimeOUP",value=s)
            updateNumericInput(session,"xROPassageTimeOUP",value=x)
            updateNumericInput(session,"omegaROPassageTimeOUP",value=omega)
            updateNumericInput(session,"PpctROPassageTimeOUP",value=Ppct)
          }
          FromUItoR6 <- function()
          {
            # Get from UI ----
            t_stoch_args <- A$get_t_stoch_args()
            t <- t_stoch_args[[1]]
            rho <- input$rhoROPassageTimeOUP
            if(!is.numeric(rho)) { rho <- 0 }
            else if(rho < 0) { rho <- 0 }
            mu <- input$muROPassageTimeOUP
            if(!is.numeric(mu)) { mu <- 0 }
            sigma <- input$sigmaROPassageTimeOUP
            if(!is.numeric(sigma)) { sigma <- 0 }
            zFrom <- input$zFromROPassageTimeOUP
            if(!is.numeric(zFrom)) { zFrom <- 0 }
            zTo <- input$zToROPassageTimeOUP
            if(!is.numeric(zTo)) { zTo <- 0 }
            zBy <- input$zByROPassageTimeOUP
            if(!is.numeric(zBy)) { zBy <- 0 }
            if(abs(zTo-zFrom) < abs(zBy)) { zBy <- zTo-zFrom }
            if(abs(zTo-zFrom)/100 > abs(zBy)) { zBy <- (zTo-zFrom)/100 }
            if(zTo > zFrom) { z <- seq(from=zFrom,to=zTo,by=abs(zBy)) }
            else if(zTo == zFrom) { z <- zFrom }
            else { z <- seq(from=zTo,to=zFrom,by=abs(zBy)) }
            k <- input$kROPassageTimeOUP
            if(!is.numeric(k)) { k <- 0 }
            s <- input$sROPassageTimeOUP
            if(!is.numeric(s)) { s <- t[1] }
            else if(s != t[1])
            {
              tadd <- s-t[1]
              t <- t+tadd
            }
            x <- input$xROPassageTimeOUP
            if(!is.numeric(x)) { x <- -mu }
            omega <- input$omegaROPassageTimeOUP
            if(!is.numeric(omega)) { omega <- 1 }
            else if(omega < 0) { omega <- 0 }
            else if(omega > 1) { omega <- 1 }
            Ppct <- input$PpctROPassageTimeOUP
            if(!is.numeric(Ppct)) { Ppct <- 0.841345 }
            else if(Ppct < 0.01) { Ppct <- 0.01 }
            else if(Ppct > 0.99) { Ppct <- 0.99 }
            # Set to OUP ----
            A$set_oup_params(rho=rho,mu=mu,sigma=sigma)
            A$set_t_stoch_args(t=t,k=k,s=s,x=x,z=z,omega=omega,Ppct=Ppct)
          }
          # Initialize ----
          FromR6toUI()
          # Observe undo, sync, axes, plot (or enter key), left and rght ----
          undoButton <- reactiveVal(FALSE)
          syncButton <- reactiveVal(FALSE)
          axesButton <- reactiveVal(FALSE)
          plotButton <- reactiveVal(FALSE)
          leftButton <- reactiveVal(FALSE)
          rghtButton <- reactiveVal(FALSE)
          observe({
            undoButton(TRUE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$undoROPassageTimeOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(TRUE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$syncROPassageTimeOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(TRUE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$axesROPassageTimeOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(TRUE)
            leftButton(FALSE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$plotROPassageTimeOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(TRUE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$leftROPassageTimeOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(TRUE)
          }) %>% bindEvent(input$rghtROPassageTimeOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # User clicks save ----
          observe({
            FromUItoR6()
            A$default_save()
          }) %>% bindEvent(input$saveROPassageTimeOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # User clicks undo, axes, plot (or enter key), left or rght ----
          output$plotlyROPassageTimeOUP <- renderPlotly({
            if(undoButton()) { A$default_read() }
            else if(syncButton())
            {
              FromUItoR6()
              A$sync_zyxt_stoch()
            }
            else if(axesButton())
            {
              FromUItoR6()
              A$axes_t_stoch()
            }
            else if(plotButton()) { FromUItoR6() }
            else if(leftButton())
            {
              FromUItoR6()
              phi <- A$get_x_stoch_args()[[6]]
              t_stoch_args <- A$get_t_stoch_args()
              phi <- phi-1
              if(phi < -1) { phi <- 1 }
              else if(phi > -1) { phi <- -1 }
              k <- t_stoch_args[[2]]
              x <- t_stoch_args[[4]]
              A$set_x_stoch_args(phi=phi)
              A$set_t_stoch_args(k=x,x=k)
            }
            else if(rghtButton())
            {
              FromUItoR6()
              phi <- A$get_x_stoch_args()[[6]]
              t_stoch_args <- A$get_t_stoch_args()
              phi <- phi+1
              if(phi > 1) { phi <- -1 }
              else if(phi < 1) { phi <- 1 }
              k <- t_stoch_args[[2]]
              x <- t_stoch_args[[4]]
              A$set_x_stoch_args(phi=phi)
              A$set_t_stoch_args(k=x,x=k)
            }
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(FALSE)
            FromR6toUI()
            A$PlotPassageTimePercentiles(type=3)
          }) %>% bindEvent(input$undoROPassageTimeOUP,input$syncROPassageTimeOUP,input$axesROPassageTimeOUP,input$plotROPassageTimeOUP,input$leftROPassageTimeOUP,input$rghtROPassageTimeOUP)
          # User clicks info ----
          observe({
            showModal(modalDialog(
              title=div(img(src="Roar32x32.png"),"Passage Times"),
              HTML("A Passage Time is the time until a system crosses a threshold.  The longer the passage time, the more resilient the system.  Passage Times will be longer if the state of the system is far from the threshold, is moving slowly and is less stochastic.<br><br>
                The probabilities of the Ornstein-Uhlenbeck Process are symmetric.  The measure of central tendency is the Mean and the measure of dispersion is the Variance.  The probabilities of Passage Times are not symmetric.  There are three measures of central tendency, the Mode, Median and Mean.  However, the Mean and Variance may not exist.<br><br>
                Percentiles are a reliable alternative. The Median is the Passage Time with a 50% chance the threshold has been crossed and a 50% chance it is yet to be crossed.  Higher and lower Percentiles have similar interpretations.  Percentiles of 0.841345 and 0.158655 are equivalent to adding and subtracting the square-root of the Variance to the Mean of the Ornstein-Uhlenbeck Process.<br><br>
                If crossing a threshold is irreversible, Passage Times are First Passage Times.  If crossing a threshold is completely reversible, Passage Times are Visiting Times.  If crossing a threshold may be partially reversible, Passage Times are in between First Passage Times and Visiting Times.<br><br>
              &emsp;&emsp;Arguments:<br>
              &emsp;&emsp;&emsp;<i>k</i> is the threshold;<br>
              &emsp;&emsp;&emsp;<i>s</i> is the fixed initial time;<br>
              &emsp;&emsp;&emsp;<i>x</i> is the fixed initial state;<br>
              &emsp;&emsp;&emsp;<i>z</i> are alternate initial states;<br>
              &emsp;&emsp;&emsp;<i>omega</i> is the degree of irreversibility;<br>
              &emsp;&emsp;&emsp;<i>rho</i> is the rate parameter;<br>
              &emsp;&emsp;&emsp;<i>mu</i> is the location parameter;<br>
              &emsp;&emsp;&emsp;<i>sigma</i> is the scale parameter;<br>
              &emsp;&emsp;&emsp;<i>Ppct</i> is a passage time probability.<br>
              &emsp;&emsp;Returns:<br>
              &emsp;&emsp;&emsp;<i>t</i><sub>0.5</sub> is the Median Passage Time;<br>
              &emsp;&emsp;&emsp;<i>t<sub>Ppct</sub></i> and <i>t</i><sub>1-<i>Ppct</i></sub> are Passage Time Percentiles for <i>Ppct</i> and 1-<i>Ppct</i>."),
              easyClose = TRUE,
              footer = tagList(actionButton("moreROPassageTimeOUP","More",class="btn-primary",title="Analytical Passage Time Percentiles"),modalButton("Close")),
              size = "l"
            ))
          }) %>% bindEvent(input$infoROPassageTimeOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            removeModal()
            updateTabsetPanel(session,"navBar",selected="tabAOUP")
            updateTabsetPanel(session,"navAOUP",selected="APTPercentilesOUP")
          }) %>% bindEvent(input$moreROPassageTimeOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
        # Sequence ----
        #   else if(input$navROOUP == "ROSequenceOUP")
        #  {
        #    message(input$navROOUP)
        #  }
       })
    }
    else if(input$navBar == "tabAOUP")
    {
      observeEvent(input$navAOUP,{
        # Drift ----
        if(input$navAOUP == "ADriftOUP")
        {
          # Define set/get functions ----
          FromR6toUI <- function()
          {
            # Get from OUP ----
            oup_params <- A$get_oup_params()
            z_stoch_args <- A$get_z_stoch_args()
            rho <- oup_params[[1]]
            mu <- oup_params[[2]]
            z <- z_stoch_args[[1]]
            n <- length(z)
            zFrom <- z[1]
            zTo <- z[n]
            if(n > 1) { zBy <- (zTo-zFrom)/(n-1) }
            else  {zBy <- 0 }
            # Set to UI ----
            updateNumericInput(session,"rhoADriftOUP",value=rho)
            updateNumericInput(session,"muADriftOUP",value=mu)
            updateNumericInput(session,"zFromADriftOUP",value=zFrom)
            updateNumericInput(session,"zToADriftOUP",value=zTo)
            updateNumericInput(session,"zByADriftOUP",value=zBy)
          }
          FromUItoR6 <- function()
          {
            # Get from UI ----
            rho <- input$rhoADriftOUP
            if(!is.numeric(rho)) { rho <- 0 }
            else if(rho < 0) { rho <- 0 }
            mu <- input$muADriftOUP
            if(!is.numeric(mu)) { mu <- 0 }
            zFrom <- input$zFromADriftOUP
            if(!is.numeric(zFrom)) { zFrom <- 0 }
            zTo <- input$zToADriftOUP
            if(!is.numeric(zTo)) { zTo <- 0 }
            zBy <- input$zByADriftOUP
            if(!is.numeric(zBy)) { zBy <- 0 }
            if(abs(zTo-zFrom) < abs(zBy)) { zBy <- zTo-zFrom }
            if(abs(zTo-zFrom)/100 > abs(zBy)) { zBy <- (zTo-zFrom)/100 }
            if(zTo > zFrom) { z <- seq(from=zFrom,to=zTo,by=abs(zBy)) }
            else if(zTo == zFrom) { z <- zFrom }
            else { z <- seq(from=zTo,to=zFrom,by=abs(zBy)) }
            # Set to OUP ----
            A$set_oup_params(rho=rho,mu=mu)
            A$set_z_stoch_args(z=z)
          }
          # Initialize ----
          FromR6toUI()
          # Observe undo, sync, axes and plot (or enter key) ----
          undoButton <- reactiveVal(FALSE)
          syncButton <- reactiveVal(FALSE)
          axesButton <- reactiveVal(FALSE)
          plotButton <- reactiveVal(FALSE)
          observe({
            undoButton(TRUE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
          }) %>% bindEvent(input$undoADriftOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(TRUE)
            axesButton(FALSE)
            plotButton(FALSE)
          }) %>% bindEvent(input$syncADriftOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(TRUE)
            plotButton(FALSE)
          }) %>% bindEvent(input$axesADriftOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(TRUE)
          }) %>% bindEvent(input$plotADriftOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # User clicks save ----
          observe({
            FromUItoR6()
            A$default_save()
          }) %>% bindEvent(input$saveADriftOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # User clicks undo, sync, axes or plot (or enter key) ----
          output$plotlyADriftOUP <- renderPlotly({
            if(undoButton()) { A$default_read() }
            else if(syncButton())
            {
              FromUItoR6()
              A$sync_zyxt_stoch()
            }
            else if(axesButton())
            {
              FromUItoR6()
              A$axes_z_stoch()
            }
            else if(plotButton()) { FromUItoR6() }
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            FromR6toUI()
            A$PlotDrift()
          }) %>% bindEvent(input$undoADriftOUP,input$syncADriftOUP,input$axesADriftOUP,input$plotADriftOUP)
          # User clicks info ----
          observe({
            showModal(modalDialog(
              title=div(img(src="Roar32x32.png"),"Drift"),
              HTML("Drift is the expected change in the state of a stochastic process over a brief instant.  It is also called the Instantaneous Mean.  For the Ornstein-Uhlenbeck Process, it depends upon the current state <i>z</i>.<br><br>
              &emsp;&emsp;The R6 method:<br>
              &emsp;&emsp;&emsp;Drift(<i>z,rho,mu</i>)<br>
              &emsp;&emsp;with arguments:<br>
              &emsp;&emsp;&emsp;<i>z</i> are the stochastic states;<br>
              &emsp;&emsp;&emsp;<i>rho</i> is the rate parameter;<br>
              &emsp;&emsp;&emsp;<i>mu</i> is the location parameter;<br>
              &emsp;&emsp;returns:<br>
              <table style='margin-left: 60px;'>
                <tr>
                  <td style='border-top: solid silver; border-bottom: solid silver; border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'><i>g</i>(<i>z</i><sub>1</sub>)</td>
                  <td style='padding: 0px 4px 0px 4px;'>&hellip;</td>
                  <td style='padding: 0px 4px 0px 4px;'><i>g</i>(<i>z</i><sub>n</sub>)</td>
                  <td style='border-top: solid silver; border-bottom: solid silver; border-right: solid silver'>&nbsp;</td>
                  <td style='padding-left: 2px;'>;</td>
                </tr>
              </table>
              &emsp;&emsp;where:<br>
              &emsp;&emsp;&emsp;<i>g</i> is the Drift."),
              easyClose = TRUE,
              footer = modalButton("Close")
            ))
          }) %>% bindEvent(input$infoADriftOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
        # Diffusion ----
        else if(input$navAOUP == "ADiffusionOUP")
        {
          # Define set/get functions ----
          FromR6toUI <- function()
          {
            # Get from OUP ----
            oup_params <- A$get_oup_params()
            z_stoch_args <- A$get_z_stoch_args()
            rho <- oup_params[[1]]
            mu <- oup_params[[2]]
            sigma <- oup_params[[3]]
            z <- z_stoch_args[[1]]
            n <- length(z)
            zFrom <- z[1]
            zTo <- z[n]
            if(n > 1) { zBy <- (zTo-zFrom)/(n-1) }
            else  {zBy <- 0 }
            # Set to UI ----
            updateNumericInput(session,"rhoADiffusionOUP",value=rho)
            updateNumericInput(session,"muADiffusionOUP",value=mu)
            updateNumericInput(session,"sigmaADiffusionOUP",value=sigma)
            updateNumericInput(session,"zFromADiffusionOUP",value=zFrom)
            updateNumericInput(session,"zToADiffusionOUP",value=zTo)
            updateNumericInput(session,"zByADiffusionOUP",value=zBy)
          }
          FromUItoR6 <- function()
          {
            # Get from UI ----
            rho <- input$rhoADiffusionOUP
            if(!is.numeric(rho)) { rho <- 0 }
            else if(rho < 0) { rho <- 0 }
            mu <- input$muADiffusionOUP
            if(!is.numeric(mu)) { mu <- 0 }
            sigma <- input$sigmaADiffusionOUP
            if(!is.numeric(sigma)) { sigma <- 0 }
            zFrom <- input$zFromADiffusionOUP
            if(!is.numeric(zFrom)) { zFrom <- 0 }
            zTo <- input$zToADiffusionOUP
            if(!is.numeric(zTo)) { zTo <- 0 }
            zBy <- input$zByADiffusionOUP
            if(!is.numeric(zBy)) { zBy <- 0 }
            if(abs(zTo-zFrom) < abs(zBy)) { zBy <- zTo-zFrom }
            if(abs(zTo-zFrom)/100 > abs(zBy)) { zBy <- (zTo-zFrom)/100 }
            if(zTo > zFrom) { z <- seq(from=zFrom,to=zTo,by=abs(zBy)) }
            else if(zTo == zFrom) { z <- zFrom }
            else { z <- seq(from=zTo,to=zFrom,by=abs(zBy)) }
            # Set to OUP ----
            A$set_oup_params(rho=rho,mu=mu,sigma=sigma)
            A$set_z_stoch_args(z=z)
          }
          # Initialize ----
          FromR6toUI()
          # Observe undo, sync, axes, plot (or enter key), left and rght ----
          undoButton <- reactiveVal(FALSE)
          syncButton <- reactiveVal(FALSE)
          axesButton <- reactiveVal(FALSE)
          plotButton <- reactiveVal(FALSE)
          leftButton <- reactiveVal(FALSE)
          rghtButton <- reactiveVal(FALSE)
          observe({
            undoButton(TRUE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$undoADiffusionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(TRUE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$syncADiffusionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(TRUE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$axesADiffusionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(TRUE)
            leftButton(FALSE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$plotADiffusionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(TRUE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$leftADiffusionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(TRUE)
          }) %>% bindEvent(input$rghtADiffusionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # User clicks save ----
          observe({
            FromUItoR6()
            A$default_save()
          }) %>% bindEvent(input$saveADiffusionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # User clicks undo, sync, axes, plot (or enter key), left or rght ----
          output$plotlyADiffusionOUP <- renderPlotly({
            if(undoButton()) { A$default_read() }
            else if(syncButton())
            {
              FromUItoR6()
              A$sync_zyxt_stoch()
            }
            else if(axesButton())
            {
              FromUItoR6()
              A$axes_z_stoch()
            }
            else if(plotButton()) { FromUItoR6() }
            else if(leftButton())
            {
              plot_info <- A$get_plot_info()
              type <- plot_info$plottype[[1]]
              if(type > 3) { type <- 3 }
              else if(type > 2) { type <- 2 }
              else { type <- 3 }
              A$set_plot_info(type=type)
              FromUItoR6()
            }
            else if(rghtButton())
            {
              plot_info <- A$get_plot_info()
              type <- plot_info$plottype[[1]]
              if(type < 2) { type <- 2 }
              else if(type < 3) { type <- 3 }
              else { type <- 2 }
              A$set_plot_info(type=type)
              FromUItoR6()
            }
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(FALSE)
            FromR6toUI()
            A$PlotDiffusion()
          }) %>% bindEvent(input$undoADiffusionOUP,input$syncADiffusionOUP,input$axesADiffusionOUP,input$plotADiffusionOUP,input$leftADiffusionOUP,input$rghtADiffusionOUP)
          # User clicks info ----
          observe({
            showModal(modalDialog(
              title=div(img(src="Roar32x32.png"),"Diffusion"),
              HTML("An error is the difference between the actual and expected changes in the state of a stochastic process.  Diffusion is the error squared over a brief instant.  It is also called the Instantaneous Variance.  For the Ornstein-Uhlenbeck Process, it is constant.<br><br>
              &emsp;&emsp;The R6 method:<br>
              &emsp;&emsp;&emsp;Diffusion(<i>sigma</i>)<br>
              &emsp;&emsp;with argument:<br>
              &emsp;&emsp;&emsp;<i>sigma</i> is the scale parameter;<br>
              &emsp;&emsp;returns:<br>
              <table style='margin-left: 60px;'>
                <tr>
                  <td style='border-top: solid silver; border-bottom: solid silver; border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'><i>h</i><sup>2</sup></td>
                  <td style='padding: 0px 4px 0px 4px;'>&hellip;</td>
                  <td style='padding: 0px 4px 0px 4px;'><i>h</i><sup>2</sup></td>
                  <td style='border-top: solid silver; border-bottom: solid silver; border-right: solid silver'>&nbsp;</td>
                  <td style='padding-left: 2px;'>;</td>
                </tr>
              </table>
              &emsp;&emsp;where:<br>
              &emsp;&emsp;&emsp;<i>h</i><sup>2</sup> is the Diffusion."),
              easyClose = TRUE,
              footer = modalButton("Close")
            ))
          }) %>% bindEvent(input$infoADiffusionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
        # Mean ----
        else if(input$navAOUP == "AMeanOUP")
        {
          # Define set/get functions ----
          FromR6toUI <- function()
          {
            # Get from OUP ----
            oup_params <- A$get_oup_params()
            y_stoch_args <- A$get_y_stoch_args()
            plot_info <- A$get_plot_info()
            rho <- oup_params[[1]]
            mu <- oup_params[[2]]
            sigma <- oup_params[[3]]
            t <- y_stoch_args[[1]]
            y <- y_stoch_args[[2]]
            s <- y_stoch_args[[3]]
            x <- y_stoch_args[[4]]
            psi <- y_stoch_args[[5]]
            pmax <- plot_info$plottype[[2]]
            m <- length(t)
            n <- length(y)
            tFrom <- t[1]
            tTo <- t[m]
            if(m > 1) { tBy <- (tTo-tFrom)/(m-1) }
            else  {tBy <- 0 }
            yFrom <- y[1]
            yTo <- y[n]
            if(n > 1) { yBy <- (yTo-yFrom)/(n-1) }
            else  {yBy <- 0 }
            # Set to UI ----
            updateNumericInput(session,"rhoAMeanOUP",value=rho)
            updateNumericInput(session,"muAMeanOUP",value=mu)
            updateNumericInput(session,"sigmaAMeanOUP",value=sigma)
            updateNumericInput(session,"tFromAMeanOUP",value=tFrom)
            updateNumericInput(session,"tToAMeanOUP",value=tTo)
            updateNumericInput(session,"tByAMeanOUP",value=tBy)
            updateNumericInput(session,"yFromAMeanOUP",value=yFrom)
            updateNumericInput(session,"yToAMeanOUP",value=yTo)
            updateNumericInput(session,"yByAMeanOUP",value=yBy)
            updateNumericInput(session,"sAMeanOUP",value=s)
            updateNumericInput(session,"xAMeanOUP",value=x)
            updateNumericInput(session,"psiAMeanOUP",value=psi)
            updateNumericInput(session,"pmaxAMeanOUP",value=pmax)
          }
          FromUItoR6 <- function()
          {
            # Get from UI ----
            rho <- input$rhoAMeanOUP
            if(!is.numeric(rho)) { rho <- 0 }
            else if(rho < 0) { rho <- 0 }
            mu <- input$muAMeanOUP
            if(!is.numeric(mu)) { mu <- 0 }
            sigma <- input$sigmaAMeanOUP
            if(!is.numeric(sigma)) { sigma <- 0 }
            tfrom <- input$tFromAMeanOUP
            if(!is.numeric(tfrom)) { tfrom <- 0 }
            tto <- input$tToAMeanOUP
            if(!is.numeric(tto)) { tto <- 0 }
            tby <- input$tByAMeanOUP
            if(!is.numeric(tby)) { tby <- 0 }
            if(abs(tto-tfrom) < abs(tby)) { tby <- tto-tfrom }
            if(abs(tto-tfrom)/100 > abs(tby)) { tby <- (tto-tfrom)/100 }
            if(tto > tfrom) { t <- seq(from=tfrom,to=tto,by=abs(tby)) }
            else if(tto == tfrom) { t <- tfrom }
            else { t <- seq(from=tto,to=tfrom,by=abs(tby)) }
            yFrom <- input$yFromAMeanOUP
            if(!is.numeric(yFrom)) { yFrom <- 0 }
            yTo <- input$yToAMeanOUP
            if(!is.numeric(yTo)) { yTo <- 0 }
            yBy <- input$yByAMeanOUP
            if(!is.numeric(yBy)) { yBy <- 0 }
            if(abs(yTo-yFrom) < abs(yBy)) { yBy <- yTo-yFrom }
            if(abs(yTo-yFrom)/100 > abs(yBy)) { yBy <- (yTo-yFrom)/100 }
            if(yTo > yFrom) { y <- seq(from=yFrom,to=yTo,by=abs(yBy)) }
            else if(yTo == yFrom) { y <- yFrom }
            else { y <- seq(from=yTo,to=yFrom,by=abs(yBy)) }
            s <- input$sAMeanOUP
            if(!is.numeric(s)) { s <- t[1] }
            else if(s > t[1]) { s <- t[1] }
            x <- input$xAMeanOUP
            if(!is.numeric(x)) { x <- 0 }
            psi <- input$psiAMeanOUP
            if(!is.numeric(psi)) { psi <- -1 }
            else if(psi <= 0) { psi <- -1 }
            else { psi <- 1 }
            pmax <- input$pmaxAMeanOUP
            if(!is.numeric(pmax)) { pmax <- NaN }
            # Set to OUP ----
            A$set_oup_params(rho=rho,mu=mu,sigma=sigma)
            A$set_y_stoch_args(t=t,y=y,s=s,x=x,psi=psi)
            A$set_plot_info(pmax=pmax)
          }
          # Initialize ----
          FromR6toUI()
          # Observe undo, sync, axes, plot (or enter key), left and rght ----
          undoButton <- reactiveVal(FALSE)
          syncButton <- reactiveVal(FALSE)
          axesButton <- reactiveVal(FALSE)
          plotButton <- reactiveVal(FALSE)
          leftButton <- reactiveVal(FALSE)
          rghtButton <- reactiveVal(FALSE)
          observe({
            undoButton(TRUE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$undoAMeanOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(TRUE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$syncAMeanOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(TRUE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$axesAMeanOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(TRUE)
            leftButton(FALSE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$plotAMeanOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(TRUE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$leftAMeanOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(TRUE)
          }) %>% bindEvent(input$rghtAMeanOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # User clicks save ----
          observe({
            FromUItoR6()
            A$default_save()
          }) %>% bindEvent(input$saveAMeanOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # User clicks undo, sync, axes, plot (or enter key), left or rght ----
          output$plotlyAMeanOUP <- renderPlotly({
            if(undoButton()) { A$default_read() }
            else if(syncButton())
            {
              FromUItoR6()
              A$sync_zyxt_stoch()
            }
            else if(axesButton())
            {
              FromUItoR6()
              A$axes_y_stoch()
            }
            else if(plotButton()) { FromUItoR6() }
            else if(leftButton())
            {
              plot_info <- A$get_plot_info()
              type <- plot_info$plottype[[1]]
              if(type > 5) { type <- 5 }
              else if(type > 4) { type <- 4 }
              else if(type > 3) { type <- 3 }
              else { type <- 5 }
              A$set_plot_info(type=type)
              FromUItoR6()
            }
            else if(rghtButton())
            {
              plot_info <- A$get_plot_info()
              type <- plot_info$plottype[[1]]
              if(type < 3) { type <- 3 }
              else if(type < 4) { type <- 4 }
              else if(type < 5) { type <- 5 }
              else { type <- 3 }
              A$set_plot_info(type=type)
              FromUItoR6()
            }
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(FALSE)
            FromR6toUI()
            A$PlotMean()
          }) %>% bindEvent(input$undoAMeanOUP,input$syncAMeanOUP,input$axesAMeanOUP,input$plotAMeanOUP,input$leftAMeanOUP,input$rghtAMeanOUP)
          # User clicks info ----
          observe({
            showModal(modalDialog(
              title=div(img(src="Roar32x32.png"),"Mean"),
              HTML("A Mean of a stochastic process is the expected state <i>y</i> at time <i>t</i> in the future.  For all stochastic processes, including the Ornstein-Uhlenbeck Process, it depends upon the initial time <i>s</i> and the initial state <i>x</i>.<br><br>
              &emsp;&emsp;The R6 method:<br>
              &emsp;&emsp;&emsp;Mean(<i>t,s,x,rho,mu</i>)<br>
              &emsp;&emsp;with arguments:<br>
              &emsp;&emsp;&emsp;<i>t</i> are the variable times;<br>
              &emsp;&emsp;&emsp;<i>s</i> is the fixed initial time;<br>
              &emsp;&emsp;&emsp;<i>x</i> is the fixed initial state;<br>
              &emsp;&emsp;&emsp;<i>rho</i> is the rate parameter;<br>
              &emsp;&emsp;&emsp;<i>mu</i> is the location parameter;<br>
              &emsp;&emsp;returns:<br>
              <table style='margin-left: 60px;'>
                <tr>
                  <td style='border-top: solid silver; border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'><i>G</i>(<i>t</i><sub>1</sub>)</td>
                  <td style='border-top: solid silver; border-right: solid silver'>&nbsp;</td>
                </tr>
                <tr>
                  <td style='border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'>&emsp;&vellip;</td>
                  <td style='border-right: solid silver'>&nbsp;</td>
                </tr>
                <tr>
                  <td style='border-bottom: solid silver; border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'><i>G</i>(<i>t</i><sub>m</sub>)</td>
                  <td style='border-bottom: solid silver; border-right: solid silver'>&nbsp;</td>
                  <td style='padding-left: 2px;'>;</td>
                </tr>
              </table>
              &emsp;&emsp;where:<br>
              &emsp;&emsp;&emsp;<i>G</i> is the Mean."),
              easyClose = TRUE,
              footer = modalButton("Close")
            ))
          }) %>% bindEvent(input$infoAMeanOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
        # Mean convergence----
        else if(input$navAOUP == "AMeanCOUP")
        {
          # Define set/get functions ----
          FromR6toUI <- function()
          {
            # Get from OUP ----
            oup_params <- A$get_oup_params()
            y_stoch_args <- A$get_y_stoch_args()
            rho <- oup_params[[1]]
            t <- y_stoch_args[[1]]
            s <- y_stoch_args[[3]]
            x <- y_stoch_args[[4]]
            eps <- y_stoch_args[[6]]
            m <- length(t)
            tFrom <- t[1]
            tTo <- t[m]
            if(m > 1) { tBy <- (tTo-tFrom)/(m-1) }
            else  {tBy <- 0 }
            # Set to UI ----
            updateNumericInput(session,"rhoAMeanCOUP",value=rho)
            updateNumericInput(session,"tFromAMeanCOUP",value=tFrom)
            updateNumericInput(session,"tToAMeanCOUP",value=tTo)
            updateNumericInput(session,"tByAMeanCOUP",value=tBy)
            updateNumericInput(session,"sAMeanCOUP",value=s)
            updateNumericInput(session,"xAMeanCOUP",value=x)
            updateNumericInput(session,"epsAMeanCOUP",value=eps)
          }
          FromUItoR6 <- function()
          {
            # Get from UI ----
            rho <- input$rhoAMeanCOUP
            if(!is.numeric(rho)) { rho <- 0 }
            else if(rho < 0) { rho <- 0 }
            tfrom <- input$tFromAMeanCOUP
            if(!is.numeric(tfrom)) { tfrom <- 0 }
            tto <- input$tToAMeanCOUP
            if(!is.numeric(tto)) { tto <- 0 }
            tby <- input$tByAMeanCOUP
            if(!is.numeric(tby)) { tby <- 0 }
            if(abs(tto-tfrom) < abs(tby)) { tby <- tto-tfrom }
            if(abs(tto-tfrom)/100 > abs(tby)) { tby <- (tto-tfrom)/100 }
            if(tto > tfrom) { t <- seq(from=tfrom,to=tto,by=abs(tby)) }
            else if(tto == tfrom) { t <- tfrom }
            else { t <- seq(from=tto,to=tfrom,by=abs(tby)) }
            s <- input$sAMeanCOUP
            if(!is.numeric(s)) { s <- t[1] }
            else if(s > t[1]) { s <- t[1] }
            x <- input$xAMeanCOUP
            if(!is.numeric(x)) { x <- 0 }
            eps <- input$epsAMeanCOUP
            if(!is.numeric(eps)) { eps <- 0 }
            else if(eps < 0) { eps <- 0 }
            else if(eps > 1) { eps <- 1 }
            # Set to OUP ----
            A$set_oup_params(rho=rho)
            A$set_y_stoch_args(t=t,s=s,x=x,eps=eps)
          }
          # Initialize ----
          FromR6toUI()
          # Observe undo, sync, axes and plot (or enter key) ----
          undoButton <- reactiveVal(FALSE)
          syncButton <- reactiveVal(FALSE)
          axesButton <- reactiveVal(FALSE)
          plotButton <- reactiveVal(FALSE)
          observe({
            undoButton(TRUE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
          }) %>% bindEvent(input$undoAMeanCOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(TRUE)
            axesButton(FALSE)
            plotButton(FALSE)
          }) %>% bindEvent(input$syncAMeanCOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(TRUE)
            plotButton(FALSE)
          }) %>% bindEvent(input$axesAMeanCOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(TRUE)
          }) %>% bindEvent(input$plotAMeanCOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # User clicks save ----
          observe({
            FromUItoR6()
            A$default_save()
          }) %>% bindEvent(input$saveAMeanCOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # User clicks undo, sync, axes or plot (or enter key) ----
          output$plotlyAMeanCOUP <- renderPlotly({
            if(undoButton()) { A$default_read() }
            else if(syncButton())
            {
              FromUItoR6()
              A$sync_zyxt_stoch()
            }
            else if(axesButton())
            {
              FromUItoR6()
              A$axes_y_stoch()
            }
            else if(plotButton()) { FromUItoR6() }
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            FromR6toUI()
            A$PlotMeanToConverge()
          }) %>% bindEvent(input$undoAMeanCOUP,input$syncAMeanCOUP,input$axesAMeanCOUP,input$plotAMeanCOUP)
          # User clicks info ----
          observe({
            showModal(modalDialog(
              title=div(img(src="Roar32x32.png"),"Mean Convergence"),
              HTML("Statistical methods applied to time series usually assume weak stationarity.  This requires the Mean to have converged to its Asymptotic Mean, while the Variance may still be converging.  The time for the Mean to converge indicates the required time interval between measurements for observations to become approximately stationary.  For the Ornstein-Uhlenbeck Process, the Asymptotic Mean is location <i>mu</i> and the Mean converges at rate <i>rho</i>.<br><br>
              &emsp;&emsp;The R6 method:<br>
              &emsp;&emsp;&emsp;MeanToConverge(<i>s,rho,epsilon</i>)<br>
              &emsp;&emsp;with arguments:<br>
              &emsp;&emsp;&emsp;<i>s</i> is the fixed initial time;<br>
              &emsp;&emsp;&emsp;<i>rho</i> is the rate parameter;<br>
              &emsp;&emsp;&emsp;<i>epsilon</i> is the proportion remaining;<br>
              &emsp;&emsp;returns:<br>
              <table style='margin-left: 60px;'>
                <tr>
                  <td style='border-top: solid silver; border-bottom: solid silver; border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'><i>t</i><sub>epsilon</sub></td>
                  <td style='border-top: solid silver; border-bottom: solid silver; border-right: solid silver'>&nbsp;</td>
                  <td style='padding-left: 2px;'>;</td>
                </tr>
              </table>
              &emsp;&emsp;where:<br>
              &emsp;&emsp;&emsp;<i>t</i><sub>epsilon</sub> is the time to converge by 1-epsilon."),
              easyClose = TRUE,
              footer = modalButton("Close")
            ))
          }) %>% bindEvent(input$infoAMeanCOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
        # Variance ----
        else if(input$navAOUP == "AVarianceOUP")
        {
          # Define set/get functions ----
          FromR6toUI <- function()
          {
            # Get from OUP ----
            oup_params <- A$get_oup_params()
            y_stoch_args <- A$get_y_stoch_args()
            plot_info <- A$get_plot_info()
            rho <- oup_params[[1]]
            mu <- oup_params[[2]]
            sigma <- oup_params[[3]]
            t <- y_stoch_args[[1]]
            y <- y_stoch_args[[2]]
            s <- y_stoch_args[[3]]
            x <- y_stoch_args[[4]]
            psi <- y_stoch_args[[5]]
            pmax <- plot_info$plottype[[2]]
            m <- length(t)
            n <- length(y)
            tFrom <- t[1]
            tTo <- t[m]
            if(m > 1) { tBy <- (tTo-tFrom)/(m-1) }
            else  {tBy <- 0 }
            yFrom <- y[1]
            yTo <- y[n]
            if(n > 1) { yBy <- (yTo-yFrom)/(n-1) }
            else  {yBy <- 0 }
            # Set to UI ----
            updateNumericInput(session,"rhoAVarianceOUP",value=rho)
            updateNumericInput(session,"muAVarianceOUP",value=mu)
            updateNumericInput(session,"sigmaAVarianceOUP",value=sigma)
            updateNumericInput(session,"tFromAVarianceOUP",value=tFrom)
            updateNumericInput(session,"tToAVarianceOUP",value=tTo)
            updateNumericInput(session,"tByAVarianceOUP",value=tBy)
            updateNumericInput(session,"yFromAVarianceOUP",value=yFrom)
            updateNumericInput(session,"yToAVarianceOUP",value=yTo)
            updateNumericInput(session,"yByAVarianceOUP",value=yBy)
            updateNumericInput(session,"sAVarianceOUP",value=s)
            updateNumericInput(session,"xAVarianceOUP",value=x)
            updateNumericInput(session,"psiAVarianceOUP",value=psi)
            updateNumericInput(session,"pmaxAVarianceOUP",value=pmax)
          }
          FromUItoR6 <- function()
          {
            # Get from UI ----
            rho <- input$rhoAVarianceOUP
            if(!is.numeric(rho)) { rho <- 0 }
            else if(rho < 0) { rho <- 0 }
            mu <- input$muAVarianceOUP
            if(!is.numeric(mu)) { mu <- 0 }
            sigma <- input$sigmaAVarianceOUP
            if(!is.numeric(sigma)) { sigma <- 0 }
            tfrom <- input$tFromAVarianceOUP
            if(!is.numeric(tfrom)) { tfrom <- 0 }
            tto <- input$tToAVarianceOUP
            if(!is.numeric(tto)) { tto <- 0 }
            tby <- input$tByAVarianceOUP
            if(!is.numeric(tby)) { tby <- 0 }
            if(abs(tto-tfrom) < abs(tby)) { tby <- tto-tfrom }
            if(abs(tto-tfrom)/100 > abs(tby)) { tby <- (tto-tfrom)/100 }
            if(tto > tfrom) { t <- seq(from=tfrom,to=tto,by=abs(tby)) }
            else if(tto == tfrom) { t <- tfrom }
            else { t <- seq(from=tto,to=tfrom,by=abs(tby)) }
            yFrom <- input$yFromAVarianceOUP
            if(!is.numeric(yFrom)) { yFrom <- 0 }
            yTo <- input$yToAVarianceOUP
            if(!is.numeric(yTo)) { yTo <- 0 }
            yBy <- input$yByAVarianceOUP
            if(!is.numeric(yBy)) { yBy <- 0 }
            if(abs(yTo-yFrom) < abs(yBy)) { yBy <- yTo-yFrom }
            if(abs(yTo-yFrom)/100 > abs(yBy)) { yBy <- (yTo-yFrom)/100 }
            if(yTo > yFrom) { y <- seq(from=yFrom,to=yTo,by=abs(yBy)) }
            else if(yTo == yFrom) { y <- yFrom }
            else { y <- seq(from=yTo,to=yFrom,by=abs(yBy)) }
            s <- input$sAVarianceOUP
            if(!is.numeric(s)) { s <- t[1] }
            else if(s > t[1]) { s <- t[1] }
            x <- input$xAVarianceOUP
            if(!is.numeric(x)) { x <- 0 }
            psi <- input$psiAVarianceOUP
            if(!is.numeric(psi)) { psi <- -1 }
            else if(psi <= 0) { psi <- -1 }
            else { psi <- 1 }
            pmax <- input$pmaxAVarianceOUP
            if(!is.numeric(pmax)) { pmax <- NaN }
            # Set to OUP ----
            A$set_oup_params(rho=rho,mu=mu,sigma=sigma)
            A$set_y_stoch_args(t=t,y=y,s=s,x=x,psi=psi)
            A$set_plot_info(pmax=pmax)
          }
          # Initialize ----
          FromR6toUI()
          # Observe undo, sync, axes, plot (or enter key), left and rght ----
          undoButton <- reactiveVal(FALSE)
          syncButton <- reactiveVal(FALSE)
          axesButton <- reactiveVal(FALSE)
          plotButton <- reactiveVal(FALSE)
          leftButton <- reactiveVal(FALSE)
          rghtButton <- reactiveVal(FALSE)
          observe({
            undoButton(TRUE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$undoAVarianceOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(TRUE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$syncAVarianceOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(TRUE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$axesAVarianceOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(TRUE)
            leftButton(FALSE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$plotAVarianceOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(TRUE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$leftAVarianceOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(TRUE)
          }) %>% bindEvent(input$rghtAVarianceOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # User clicks save ----
          observe({
            FromUItoR6()
            A$default_save()
          }) %>% bindEvent(input$saveAVarianceOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # User clicks undo, sync, axes, plot (or enter key), left or rght ----
          output$plotlyAVarianceOUP <- renderPlotly({
            if(undoButton()) { A$default_read() }
            else if(syncButton())
            {
              FromUItoR6()
              A$sync_zyxt_stoch()
            }
            else if(axesButton())
            {
              FromUItoR6()
              A$axes_y_stoch()
            }
            else if(plotButton()) { FromUItoR6() }
            else if(leftButton())
            {
              plot_info <- A$get_plot_info()
              type <- plot_info$plottype[[1]]
              if(type > 5) { type <- 5 }
              else if(type > 4) { type <- 4 }
              else if(type > 3) { type <- 3 }
              else if(type > 2) { type <- 2 }
              else { type <- 5 }
              A$set_plot_info(type=type)
              FromUItoR6()
            }
            else if(rghtButton())
            {
              plot_info <- A$get_plot_info()
              type <- plot_info$plottype[[1]]
              if(type < 2) { type <- 2 }
              else if(type < 3) { type <- 3 }
              else if(type < 4) { type <- 4 }
              else if(type < 5) { type <- 5 }
              else { type <- 2 }
              A$set_plot_info(type=type)
              FromUItoR6()
            }
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(FALSE)
            FromR6toUI()
            A$PlotVariance()
          }) %>% bindEvent(input$undoAVarianceOUP,input$syncAVarianceOUP,input$axesAVarianceOUP,input$plotAVarianceOUP,input$leftAVarianceOUP,input$rghtAVarianceOUP)
          # User clicks info ----
          observe({
            showModal(modalDialog(
              title=div(img(src="Roar32x32.png"),"Variance"),
              HTML("An error is the difference between the actual and expected state of a stochastic process for time <i>t</i> in the future.  A Variance is the error squared.  For the Ornstein-Uhlenbeck Process, it depends upon the initial time <i>s</i>.<br><br>
              &emsp;&emsp;The R6 method:<br>
              &emsp;&emsp;&emsp;Variance(<i>t,s,rho,sigma</i>)<br>
              &emsp;&emsp;with arguments:<br>
              &emsp;&emsp;&emsp;<i>t</i> are the variable times;<br>
              &emsp;&emsp;&emsp;<i>s</i> is the fixed initial time;<br>
              &emsp;&emsp;&emsp;<i>rho</i> is the rate parameter;<br>
              &emsp;&emsp;&emsp;<i>sigma</i> is the scale parameter;<br>
              &emsp;&emsp;returns:<br>
              <table style='margin-left: 60px;'>
                <tr>
                  <td style='border-top: solid silver; border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'><i>H</i>&hairsp;<sup>2</sup>(<i>t</i><sub>1</sub>)</td>
                  <td style='border-top: solid silver; border-right: solid silver'>&nbsp;</td>
                </tr>
                <tr>
                  <td style='border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'>&emsp;&vellip;</td>
                  <td style='border-right: solid silver'>&nbsp;</td>
                </tr>
                <tr>
                  <td style='border-bottom: solid silver; border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'><i>H</i>&hairsp;<sup>2</sup>(<i>t</i><sub>m</sub>)</td>
                  <td style='border-bottom: solid silver; border-right: solid silver'>&nbsp;</td>
                  <td style='padding-left: 2px;'>;</td>
                </tr>
              </table>
              &emsp;&emsp;where:<br>
              &emsp;&emsp;&emsp;<i>H</i>&hairsp;<sup>2</sup> is the Variance."),
              easyClose = TRUE,
              footer = modalButton("Close")
            ))
          }) %>% bindEvent(input$infoAVarianceOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
        # Variance convergence----
        else if(input$navAOUP == "AVarianceCOUP")
        {
          # Define set/get functions ----
          FromR6toUI <- function()
          {
            # Get from OUP ----
            oup_params <- A$get_oup_params()
            y_stoch_args <- A$get_y_stoch_args()
            rho <- oup_params[[1]]
            sigma <- oup_params[[3]]
            t <- y_stoch_args[[1]]
            s <- y_stoch_args[[3]]
            eps <- y_stoch_args[[6]]
            m <- length(t)
            tFrom <- t[1]
            tTo <- t[m]
            if(m > 1) { tBy <- (tTo-tFrom)/(m-1) }
            else  {tBy <- 0 }
            # Set to UI ----
            updateNumericInput(session,"rhoAVarianceCOUP",value=rho)
            updateNumericInput(session,"sigmaAVarianceCOUP",value=sigma)
            updateNumericInput(session,"tFromAVarianceCOUP",value=tFrom)
            updateNumericInput(session,"tToAVarianceCOUP",value=tTo)
            updateNumericInput(session,"tByAVarianceCOUP",value=tBy)
            updateNumericInput(session,"sAVarianceCOUP",value=s)
            updateNumericInput(session,"epsAVarianceCOUP",value=eps)
          }
          FromUItoR6 <- function()
          {
            # Get from UI ----
            rho <- input$rhoAVarianceCOUP
            if(!is.numeric(rho)) { rho <- 0 }
            else if(rho < 0) { rho <- 0 }
            sigma <- input$sigmaAVarianceCOUP
            if(!is.numeric(sigma)) { sigma <- 0 }
            tfrom <- input$tFromAVarianceCOUP
            if(!is.numeric(tfrom)) { tfrom <- 0 }
            tto <- input$tToAVarianceCOUP
            if(!is.numeric(tto)) { tto <- 0 }
            tby <- input$tByAVarianceCOUP
            if(!is.numeric(tby)) { tby <- 0 }
            if(abs(tto-tfrom) < abs(tby)) { tby <- tto-tfrom }
            if(abs(tto-tfrom)/100 > abs(tby)) { tby <- (tto-tfrom)/100 }
            if(tto > tfrom) { t <- seq(from=tfrom,to=tto,by=abs(tby)) }
            else if(tto == tfrom) { t <- tfrom }
            else { t <- seq(from=tto,to=tfrom,by=abs(tby)) }
            s <- input$sAVarianceCOUP
            if(!is.numeric(s)) { s <- t[1] }
            else if(s > t[1]) { s <- t[1] }
            eps <- input$epsAVarianceCOUP
            if(!is.numeric(eps)) { eps <- 0 }
            else if(eps < 0) { eps <- 0 }
            else if(eps > 1) { eps <- 1 }
            # Set to OUP ----
            A$set_oup_params(rho=rho,sigma=sigma)
            A$set_y_stoch_args(t=t,s=s,eps=eps)
          }
          # Initialize ----
          FromR6toUI()
          # Observe undo, sync, axes and plot (or enter key) ----
          undoButton <- reactiveVal(FALSE)
          syncButton <- reactiveVal(FALSE)
          axesButton <- reactiveVal(FALSE)
          plotButton <- reactiveVal(FALSE)
          observe({
            undoButton(TRUE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
          }) %>% bindEvent(input$undoAVarianceCOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(TRUE)
            axesButton(FALSE)
            plotButton(FALSE)
          }) %>% bindEvent(input$syncAVarianceCOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(TRUE)
            plotButton(FALSE)
          }) %>% bindEvent(input$axesAVarianceCOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(TRUE)
          }) %>% bindEvent(input$plotAVarianceCOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # User clicks save ----
          observe({
            FromUItoR6()
            A$default_save()
          }) %>% bindEvent(input$saveAVarianceCOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # User clicks undo, sync, axes or plot (or enter key) ----
          output$plotlyAVarianceCOUP <- renderPlotly({
            if(undoButton()) { A$default_read() }
            else if(syncButton())
            {
              FromUItoR6()
              A$sync_zyxt_stoch()
            }
            else if(axesButton())
            {
              FromUItoR6()
              A$axes_y_stoch()
            }
            else if(plotButton()) { FromUItoR6() }
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            FromR6toUI()
            A$PlotVarianceToConverge()
          }) %>% bindEvent(input$undoAVarianceCOUP,input$syncAVarianceCOUP,input$axesAVarianceCOUP,input$plotAVarianceCOUP)
          # User clicks info ----
          observe({
            showModal(modalDialog(
              title=div(img(src="Roar32x32.png"),"Variance Convergence"),
              HTML("In statistical methods applied to time series, weak stationarity assumes the Mean has converged to the Asymptotic Mean. Strong stationarity assumes the Variance has also converged to the Asymptotic Variance.  For the Ornstein-Uhlenbeck Process, the Asymptotic Variance is <i>sigma<sup>2</sup>/2rho</i> and the Variance converges at rate <i>2rho</i>.  The Variance converges twice as fast as the Mean.  Therefore, if the Ornstein-Uhlenbeck Process is weaky stationary, it is also strongly stationary.<br><br>
              &emsp;&emsp;The R6 method:<br>
              &emsp;&emsp;&emsp;VarianceToConverge(<i>s,rho,epsilon</i>)<br>
              &emsp;&emsp;with arguments:<br>
              &emsp;&emsp;&emsp;<i>s</i> is the fixed initial time;<br>
              &emsp;&emsp;&emsp;<i>rho</i> is the rate parameter;<br>
              &emsp;&emsp;&emsp;<i>epsilon</i> is the proportion remaining;<br>
              &emsp;&emsp;returns:<br>
              <table style='margin-left: 60px;'>
                <tr>
                  <td style='border-top: solid silver; border-bottom: solid silver; border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'><i>t</i><sub>epsilon</sub></td>
                  <td style='border-top: solid silver; border-bottom: solid silver; border-right: solid silver'>&nbsp;</td>
                  <td style='padding-left: 2px;'>;</td>
                </tr>
              </table>
              &emsp;&emsp;where:<br>
              &emsp;&emsp;&emsp;<i>t</i><sub>epsilon</sub> is the time to converge by 1-epsilon."),
              easyClose = TRUE,
              footer = modalButton("Close")
            ))
          }) %>% bindEvent(input$infoAVarianceCOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
        # Transition Density ----
        else if(input$navAOUP == "ADensityOUP")
        {
          # Define set/get functions ----
          FromR6toUI <- function()
          {
            # Get from OUP ----
            oup_params <- A$get_oup_params()
            y_stoch_args <- A$get_y_stoch_args()
            plot_info <- A$get_plot_info()
            rho <- oup_params[[1]]
            mu <- oup_params[[2]]
            sigma <- oup_params[[3]]
            t <- y_stoch_args[[1]]
            y <- y_stoch_args[[2]]
            s <- y_stoch_args[[3]]
            x <- y_stoch_args[[4]]
            pmax <- plot_info$plottype[[2]]
            m <- length(t)
            n <- length(y)
            tFrom <- t[1]
            tTo <- t[m]
            if(m > 1) { tBy <- (tTo-tFrom)/(m-1) }
            else  {tBy <- 0 }
            yFrom <- y[1]
            yTo <- y[n]
            if(n > 1) { yBy <- (yTo-yFrom)/(n-1) }
            else  {yBy <- 0 }
            # Set to UI ----
            updateNumericInput(session,"rhoADensityOUP",value=rho)
            updateNumericInput(session,"muADensityOUP",value=mu)
            updateNumericInput(session,"sigmaADensityOUP",value=sigma)
            updateNumericInput(session,"tFromADensityOUP",value=tFrom)
            updateNumericInput(session,"tToADensityOUP",value=tTo)
            updateNumericInput(session,"tByADensityOUP",value=tBy)
            updateNumericInput(session,"yFromADensityOUP",value=yFrom)
            updateNumericInput(session,"yToADensityOUP",value=yTo)
            updateNumericInput(session,"yByADensityOUP",value=yBy)
            updateNumericInput(session,"sADensityOUP",value=s)
            updateNumericInput(session,"xADensityOUP",value=x)
            updateNumericInput(session,"pmaxADensityOUP",value=pmax)
          }
          FromUItoR6 <- function()
          {
            # Get from UI ----
            rho <- input$rhoADensityOUP
            if(!is.numeric(rho)) { rho <- 0 }
            else if(rho < 0) { rho <- 0 }
            mu <- input$muADensityOUP
            if(!is.numeric(mu)) { mu <- 0 }
            sigma <- input$sigmaADensityOUP
            if(!is.numeric(sigma)) { sigma <- 0 }
            tfrom <- input$tFromADensityOUP
            if(!is.numeric(tfrom)) { tfrom <- 0 }
            tto <- input$tToADensityOUP
            if(!is.numeric(tto)) { tto <- 0 }
            tby <- input$tByADensityOUP
            if(!is.numeric(tby)) { tby <- 0 }
            if(abs(tto-tfrom) < abs(tby)) { tby <- tto-tfrom }
            if(abs(tto-tfrom)/100 > abs(tby)) { tby <- (tto-tfrom)/100 }
            if(tto > tfrom) { t <- seq(from=tfrom,to=tto,by=abs(tby)) }
            else if(tto == tfrom) { t <- tfrom }
            else { t <- seq(from=tto,to=tfrom,by=abs(tby)) }
            yFrom <- input$yFromADensityOUP
            if(!is.numeric(yFrom)) { yFrom <- 0 }
            yTo <- input$yToADensityOUP
            if(!is.numeric(yTo)) { yTo <- 0 }
            yBy <- input$yByADensityOUP
            if(!is.numeric(yBy)) { yBy <- 0 }
            if(abs(yTo-yFrom) < abs(yBy)) { yBy <- yTo-yFrom }
            if(abs(yTo-yFrom)/100 > abs(yBy)) { yBy <- (yTo-yFrom)/100 }
            if(yTo > yFrom) { y <- seq(from=yFrom,to=yTo,by=abs(yBy)) }
            else if(yTo == yFrom) { y <- yFrom }
            else { y <- seq(from=yTo,to=yFrom,by=abs(yBy)) }
            s <- input$sADensityOUP
            if(!is.numeric(s)) { s <- t[1] }
            else if(s > t[1]) { s <- t[1] }
            x <- input$xADensityOUP
            if(!is.numeric(x)) { x <- 0 }
            pmax <- input$pmaxADensityOUP
            if(!is.numeric(pmax)) { pmax <- NaN }
            # Set to OUP ----
            A$set_oup_params(rho=rho,mu=mu,sigma=sigma)
            A$set_y_stoch_args(t=t,y=y,s=s,x=x)
            A$set_plot_info(pmax=pmax)
          }
          # Initialize ----
          FromR6toUI()
          # Observe undo, sync, axes, plot (or enter key), left and rght ----
          undoButton <- reactiveVal(FALSE)
          syncButton <- reactiveVal(FALSE)
          axesButton <- reactiveVal(FALSE)
          plotButton <- reactiveVal(FALSE)
          leftButton <- reactiveVal(FALSE)
          rghtButton <- reactiveVal(FALSE)
          observe({
            undoButton(TRUE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$undoADensityOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(TRUE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$syncADensityOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(TRUE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$axesADensityOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(TRUE)
            leftButton(FALSE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$plotADensityOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(TRUE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$leftADensityOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(TRUE)
          }) %>% bindEvent(input$rghtADensityOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # User clicks save ----
          observe({
            FromUItoR6()
            A$default_save()
          }) %>% bindEvent(input$saveADensityOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # User clicks undo, sync, axes, plot (or enter key), left or rght ----
          output$plotlyADensityOUP <- renderPlotly({
            if(undoButton()) { A$default_read() }
            else if(syncButton())
            {
              FromUItoR6()
              A$sync_zyxt_stoch()
            }
            else if(axesButton())
            {
              FromUItoR6()
              A$axes_y_stoch()
            }
            else if(plotButton()) { FromUItoR6() }
            else if(leftButton())
            {
              plot_info <- A$get_plot_info()
              type <- plot_info$plottype[[1]]
              if(type > 5) { type <- 5 }
              else if(type > 4) { type <- 4 }
              else if(type > 3) { type <- 3 }
              else if(type > 2) { type <- 2 }
              else { type <- 5 }
              A$set_plot_info(type=type)
              FromUItoR6()
            }
            else if(rghtButton())
            {
              plot_info <- A$get_plot_info()
              type <- plot_info$plottype[[1]]
              if(type < 2) { type <- 2 }
              else if(type < 3) { type <- 3 }
              else if(type < 4) { type <- 4 }
              else if(type < 5) { type <- 5 }
              else { type <- 2 }
              A$set_plot_info(type=type)
              FromUItoR6()
            }
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(FALSE)
            FromR6toUI()
            A$PlotDensity()
          }) %>% bindEvent(input$undoADensityOUP,input$syncADensityOUP,input$axesADensityOUP,input$plotADensityOUP,input$leftADensityOUP,input$rghtADensityOUP)
          # User clicks info ----
          observe({
            showModal(modalDialog(
              title=div(img(src="Roar32x32.png"),"Transition Density"),
              HTML("The Transition Density is the probability of state <i>y</i> being observed at time <i>t</i>.  At initial time <i>t</i> equal to <i>s</i>, the probability of <i>y</i> equal to <i>x</i> is one and the probability of <i>y</i> not equal to <i>x</i> is zero.  The Transition Density is the Dirac or Degenerate Density.  As time passes, the probability of <i>y</i> equal to <i>x</i> decreases, the probability of <i>y</i> not equal to <i>x</i> increases and the Transition Density widens and moves away from <i>x</i>.  In the limit as <i>t</i> goes to infinity, the Transition Density loses its dependence on <i>s</i> and <i>x</i> and converges to its Invariant Density, with Asymptotic Mean <i>mu</i> and Asymptotic Variance <i>sigma</i><sup>2</sup>/2<i>rho</i>.<br><br>
              &emsp;&emsp;The R6 method:<br>
              &emsp;&emsp;&emsp;Density(<i>t,y,s,x,rho,mu,sigma</i>)<br>
              &emsp;&emsp;with arguments:<br>
              &emsp;&emsp;&emsp;<i>t</i> are the variable times;<br>
              &emsp;&emsp;&emsp;<i>y</i> are the stochastic states;<br>
              &emsp;&emsp;&emsp;<i>s</i> is the fixed initial time;<br>
              &emsp;&emsp;&emsp;<i>x</i> is the fixed initial state;<br>
              &emsp;&emsp;&emsp;<i>rho</i> is the rate parameter;<br>
              &emsp;&emsp;&emsp;<i>mu</i> is the location parameter;<br>
              &emsp;&emsp;&emsp;<i>sigma</i> is the scale parameter;<br>
              &emsp;&emsp;returns:<br>
              <table style='margin-left: 60px;'>
                <tr>
                  <td style='border-top: solid silver; border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'><i>p</i>(<i>t</i><sub>1</sub>,<i>y</i><sub>1</sub>)</td>
                  <td style='padding: 0px 4px 0px 4px;'>&hellip;</td>
                  <td style='padding: 0px 4px 0px 4px;'><i>p</i>(<i>t</i><sub>1</sub>,<i>y</i><sub>n</sub>)</td>
                  <td style='border-top: solid silver; border-right: solid silver'>&nbsp;</td>
                </tr>
                <tr>
                  <td style='border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'>&emsp;&vellip;</td>
                  <td style='padding: 0px 4px 0px 4px;'>&dtdot;</td>
                  <td style='padding: 0px 4px 0px 4px;'>&emsp;&vellip;</td>
                  <td style='border-right: solid silver'>&nbsp;</td>
                </tr>
                <tr>
                  <td style='border-bottom: solid silver; border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'><i>p</i>(<i>t</i><sub>m</sub>,<i>y</i><sub>1</sub>)</td>
                  <td style='padding: 0px 4px 0px 4px;'>&hellip;</td>
                  <td style='padding: 0px 4px 0px 4px;'><i>p</i>(<i>t</i><sub>m</sub>,<i>y</i><sub>n</sub>)</td>
                  <td style='border-bottom: solid silver; border-right: solid silver'>&nbsp;</td>
                  <td style='padding-left: 2px;'>;</td>
                </tr>
              </table>
              &emsp;&emsp;where:<br>
              &emsp;&emsp;&emsp;<i>p</i> is the Transition Density."),
              easyClose = TRUE,
              footer = modalButton("Close")
            ))
          }) %>% bindEvent(input$infoADensityOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
        # Transition Probability ----
        else if(input$navAOUP == "AProbabilityOUP")
        {
          # Define set/get functions ----
          FromR6toUI <- function()
          {
            # Get from OUP ----
            oup_params <- A$get_oup_params()
            y_stoch_args <- A$get_y_stoch_args()
            rho <- oup_params[[1]]
            mu <- oup_params[[2]]
            sigma <- oup_params[[3]]
            t <- y_stoch_args[[1]]
            y <- y_stoch_args[[2]]
            s <- y_stoch_args[[3]]
            x <- y_stoch_args[[4]]
            psi <- y_stoch_args[[5]]
            m <- length(t)
            n <- length(y)
            tFrom <- t[1]
            tTo <- t[m]
            if(m > 1) { tBy <- (tTo-tFrom)/(m-1) }
            else  {tBy <- 0 }
            yFrom <- y[1]
            yTo <- y[n]
            if(n > 1) { yBy <- (yTo-yFrom)/(n-1) }
            else  {yBy <- 0 }
            # Set to UI ----
            updateNumericInput(session,"rhoAProbabilityOUP",value=rho)
            updateNumericInput(session,"muAProbabilityOUP",value=mu)
            updateNumericInput(session,"sigmaAProbabilityOUP",value=sigma)
            updateNumericInput(session,"tFromAProbabilityOUP",value=tFrom)
            updateNumericInput(session,"tToAProbabilityOUP",value=tTo)
            updateNumericInput(session,"tByAProbabilityOUP",value=tBy)
            updateNumericInput(session,"yFromAProbabilityOUP",value=yFrom)
            updateNumericInput(session,"yToAProbabilityOUP",value=yTo)
            updateNumericInput(session,"yByAProbabilityOUP",value=yBy)
            updateNumericInput(session,"sAProbabilityOUP",value=s)
            updateNumericInput(session,"xAProbabilityOUP",value=x)
            updateNumericInput(session,"psiAProbabilityOUP",value=psi)
          }
          FromUItoR6 <- function()
          {
            # Get from UI ----
            rho <- input$rhoAProbabilityOUP
            if(!is.numeric(rho)) { rho <- 0 }
            else if(rho < 0) { rho <- 0 }
            mu <- input$muAProbabilityOUP
            if(!is.numeric(mu)) { mu <- 0 }
            sigma <- input$sigmaAProbabilityOUP
            if(!is.numeric(sigma)) { sigma <- 0 }
            tfrom <- input$tFromAProbabilityOUP
            if(!is.numeric(tfrom)) { tfrom <- 0 }
            tto <- input$tToAProbabilityOUP
            if(!is.numeric(tto)) { tto <- 0 }
            tby <- input$tByAProbabilityOUP
            if(!is.numeric(tby)) { tby <- 0 }
            if(abs(tto-tfrom) < abs(tby)) { tby <- tto-tfrom }
            if(abs(tto-tfrom)/100 > abs(tby)) { tby <- (tto-tfrom)/100 }
            if(tto > tfrom) { t <- seq(from=tfrom,to=tto,by=abs(tby)) }
            else if(tto == tfrom) { t <- tfrom }
            else { t <- seq(from=tto,to=tfrom,by=abs(tby)) }
            yFrom <- input$yFromAProbabilityOUP
            if(!is.numeric(yFrom)) { yFrom <- 0 }
            yTo <- input$yToAProbabilityOUP
            if(!is.numeric(yTo)) { yTo <- 0 }
            yBy <- input$yByAProbabilityOUP
            if(!is.numeric(yBy)) { yBy <- 0 }
            if(abs(yTo-yFrom) < abs(yBy)) { yBy <- yTo-yFrom }
            if(abs(yTo-yFrom)/100 > abs(yBy)) { yBy <- (yTo-yFrom)/100 }
            if(yTo > yFrom) { y <- seq(from=yFrom,to=yTo,by=abs(yBy)) }
            else if(yTo == yFrom) { y <- yFrom }
            else { y <- seq(from=yTo,to=yFrom,by=abs(yBy)) }
            s <- input$sAProbabilityOUP
            if(!is.numeric(s)) { s <- t[1] }
            else if(s > t[1]) { s <- t[1] }
            x <- input$xAProbabilityOUP
            if(!is.numeric(x)) { x <- 0 }
            psi <- input$psiAProbabilityOUP
            if(!is.numeric(psi)) { psi <- -1 }
            else if(psi <= 0) { psi <- -1 }
            else { psi <- 1 }
            # Set to OUP ----
            A$set_oup_params(rho=rho,mu=mu,sigma=sigma)
            A$set_y_stoch_args(t=t,y=y,s=s,x=x,psi=psi)
          }
          # Initialize ----
          FromR6toUI()
          # Observe undo, sync, axes, plot (or enter key), left and rght ----
          undoButton <- reactiveVal(FALSE)
          syncButton <- reactiveVal(FALSE)
          axesButton <- reactiveVal(FALSE)
          plotButton <- reactiveVal(FALSE)
          leftButton <- reactiveVal(FALSE)
          rghtButton <- reactiveVal(FALSE)
          observe({
            undoButton(TRUE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$undoAProbabilityOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(TRUE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$syncAProbabilityOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(TRUE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$axesAProbabilityOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(TRUE)
            leftButton(FALSE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$plotAProbabilityOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(TRUE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$leftAProbabilityOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(TRUE)
          }) %>% bindEvent(input$rghtAProbabilityOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # User clicks save ----
          observe({
            FromUItoR6()
            A$default_save()
          }) %>% bindEvent(input$saveAProbabilityOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # User clicks undo, sync, axes, plot (or enter key), left or rght ----
          output$plotlyAProbabilityOUP <- renderPlotly({
            if(undoButton()) { A$default_read() }
            else if(syncButton())
            {
              FromUItoR6()
              A$sync_zyxt_stoch()
            }
            else if(axesButton())
            {
              FromUItoR6()
              A$axes_y_stoch()
            }
            else if(plotButton()) { FromUItoR6() }
            else if(leftButton())
            {
              plot_info <- A$get_plot_info()
              type <- plot_info$plottype[[1]]
              if(type > 5) { type <- 5 }
              else if(type > 4) { type <- 4 }
              else if(type > 3) { type <- 3 }
              else if(type > 2) { type <- 2 }
              else { type <- 5 }
              A$set_plot_info(type=type)
              FromUItoR6()
            }
            else if(rghtButton())
            {
              plot_info <- A$get_plot_info()
              type <- plot_info$plottype[[1]]
              if(type < 2) { type <- 2 }
              else if(type < 3) { type <- 3 }
              else if(type < 4) { type <- 4 }
              else if(type < 5) { type <- 5 }
              else { type <- 2 }
              A$set_plot_info(type=type)
              FromUItoR6()
            }
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(FALSE)
            FromR6toUI()
            A$PlotProbability()
          }) %>% bindEvent(input$undoAProbabilityOUP,input$syncAProbabilityOUP,input$axesAProbabilityOUP,input$plotAProbabilityOUP,input$leftAProbabilityOUP,input$rghtAProbabilityOUP)
          # User clicks info ----
          observe({
            showModal(modalDialog(
              title=div(img(src="Roar32x32.png"),"Transition Probability"),
              HTML("The Transition Probability integrates the Transition Density.  It sums the probabilities of observing states less than or equal to <i>y</i> at time <i>t</i>.  Alternatively, it sums the probabilities greater than or equal to <i>y</i>.  At initial time <i>t</i> equal to <i>s</i>, it sums the Dirac Density to become the Heavyside or Step Function, which steps from zero to one at <i>y</i> equal to the initial state <i>x</i>.  As time passes, the Transition Probability widens and moves away from <i>x</i>.  For the Ornstein-Uhlenbeck Process, as <i>t</i> goes to infinity, the Transition Probability converges to its Invariant Probability, with Asymptotic Mean <i>mu</i> and Asymptotic Variance <i>sigma</i><sup>2</sup>/2<i>rho</i>.<br><br>
              &emsp;&emsp;The R6 method:<br>
              &emsp;&emsp;&emsp;Probability(<i>t,y,s,x,rho,mu,sigma,phi</i>)<br>
              &emsp;&emsp;with arguments:<br>
              &emsp;&emsp;&emsp;<i>t</i> are the variable times;<br>
              &emsp;&emsp;&emsp;<i>y</i> are the stochastic states;<br>
              &emsp;&emsp;&emsp;<i>s</i> is the fixed initial time;<br>
              &emsp;&emsp;&emsp;<i>x</i> is the fixed initial state;<br>
              &emsp;&emsp;&emsp;<i>rho</i> is the rate parameter;<br>
              &emsp;&emsp;&emsp;<i>mu</i> is the location parameter;<br>
              &emsp;&emsp;&emsp;<i>sigma</i> is the scale parameter;<br>
              &emsp;&emsp;&emsp;<i>psi</i> is < 0 to integrate from -Inf to y and > 0 to integrate from y to Inf;<br>
              &emsp;&emsp;returns:<br>
              <table style='margin-left: 60px;'>
                <tr>
                  <td style='border-top: solid silver; border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'><i>P</i>(<i>t</i><sub>1</sub>,<i>y</i><sub>1</sub>)</td>
                  <td style='padding: 0px 4px 0px 4px;'>&hellip;</td>
                  <td style='padding: 0px 4px 0px 4px;'><i>P</i>(<i>t</i><sub>1</sub>,<i>y</i><sub>n</sub>)</td>
                  <td style='border-top: solid silver; border-right: solid silver'>&nbsp;</td>
                </tr>
                <tr>
                  <td style='border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'>&emsp;&vellip;</td>
                  <td style='padding: 0px 4px 0px 4px;'>&dtdot;</td>
                  <td style='padding: 0px 4px 0px 4px;'>&emsp;&vellip;</td>
                  <td style='border-right: solid silver'>&nbsp;</td>
                </tr>
                <tr>
                  <td style='border-bottom: solid silver; border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'><i>P</i>(<i>t</i><sub>m</sub>,<i>y</i><sub>1</sub>)</td>
                  <td style='padding: 0px 4px 0px 4px;'>&hellip;</td>
                  <td style='padding: 0px 4px 0px 4px;'><i>P</i>(<i>t</i><sub>m</sub>,<i>y</i><sub>n</sub>)</td>
                  <td style='border-bottom: solid silver; border-right: solid silver'>&nbsp;</td>
                  <td style='padding-left: 2px;'>;</td>
                </tr>
              </table>
              &emsp;&emsp;where:<br>
              &emsp;&emsp;&emsp;<i>P</i> is the Transition Probability."),
              easyClose = TRUE,
              footer = modalButton("Close")
            ))
          }) %>% bindEvent(input$infoAProbabilityOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
        # Double Integral ----
        else if(input$navAOUP == "ADoubleOUP")
        {
          # Define set/get functions ----
          FromR6toUI <- function()
          {
            # Get from OUP ----
            oup_params <- A$get_oup_params()
            y_stoch_args <- A$get_y_stoch_args()
            rho <- oup_params[[1]]
            mu <- oup_params[[2]]
            sigma <- oup_params[[3]]
            t <- y_stoch_args[[1]]
            y <- y_stoch_args[[2]]
            s <- y_stoch_args[[3]]
            x <- y_stoch_args[[4]]
            psi <- y_stoch_args[[5]]
            m <- length(t)
            n <- length(y)
            tFrom <- t[1]
            tTo <- t[m]
            if(m > 1) { tBy <- (tTo-tFrom)/(m-1) }
            else  {tBy <- 0 }
            yFrom <- y[1]
            yTo <- y[n]
            if(n > 1) { yBy <- (yTo-yFrom)/(n-1) }
            else  {yBy <- 0 }
            # Set to UI ----
            updateNumericInput(session,"rhoADoubleOUP",value=rho)
            updateNumericInput(session,"muADoubleOUP",value=mu)
            updateNumericInput(session,"sigmaADoubleOUP",value=sigma)
            updateNumericInput(session,"tFromADoubleOUP",value=tFrom)
            updateNumericInput(session,"tToADoubleOUP",value=tTo)
            updateNumericInput(session,"tByADoubleOUP",value=tBy)
            updateNumericInput(session,"yFromADoubleOUP",value=yFrom)
            updateNumericInput(session,"yToADoubleOUP",value=yTo)
            updateNumericInput(session,"yByADoubleOUP",value=yBy)
            updateNumericInput(session,"sADoubleOUP",value=s)
            updateNumericInput(session,"xADoubleOUP",value=x)
            updateNumericInput(session,"psiADoubleOUP",value=psi)
          }
          FromUItoR6 <- function()
          {
            # Get from UI ----
            rho <- input$rhoADoubleOUP
            if(!is.numeric(rho)) { rho <- 0 }
            else if(rho < 0) { rho <- 0 }
            mu <- input$muADoubleOUP
            if(!is.numeric(mu)) { mu <- 0 }
            sigma <- input$sigmaADoubleOUP
            if(!is.numeric(sigma)) { sigma <- 0 }
            tfrom <- input$tFromADoubleOUP
            if(!is.numeric(tfrom)) { tfrom <- 0 }
            tto <- input$tToADoubleOUP
            if(!is.numeric(tto)) { tto <- 0 }
            tby <- input$tByADoubleOUP
            if(!is.numeric(tby)) { tby <- 0 }
            if(abs(tto-tfrom) < abs(tby)) { tby <- tto-tfrom }
            if(abs(tto-tfrom)/100 > abs(tby)) { tby <- (tto-tfrom)/100 }
            if(tto > tfrom) { t <- seq(from=tfrom,to=tto,by=abs(tby)) }
            else if(tto == tfrom) { t <- tfrom }
            else { t <- seq(from=tto,to=tfrom,by=abs(tby)) }
            yFrom <- input$yFromADoubleOUP
            if(!is.numeric(yFrom)) { yFrom <- 0 }
            yTo <- input$yToADoubleOUP
            if(!is.numeric(yTo)) { yTo <- 0 }
            yBy <- input$yByADoubleOUP
            if(!is.numeric(yBy)) { yBy <- 0 }
            if(abs(yTo-yFrom) < abs(yBy)) { yBy <- yTo-yFrom }
            if(abs(yTo-yFrom)/100 > abs(yBy)) { yBy <- (yTo-yFrom)/100 }
            if(yTo > yFrom) { y <- seq(from=yFrom,to=yTo,by=abs(yBy)) }
            else if(yTo == yFrom) { y <- yFrom }
            else { y <- seq(from=yTo,to=yFrom,by=abs(yBy)) }
            s <- input$sADoubleOUP
            if(!is.numeric(s)) { s <- t[1] }
            else if(s > t[1]) { s <- t[1] }
            x <- input$xADoubleOUP
            if(!is.numeric(x)) { x <- 0 }
            psi <- input$psiADoubleOUP
            if(!is.numeric(psi)) { psi <- -1 }
            else if(psi <= 0) { psi <- -1 }
            else { psi <- 1 }
            # Set to OUP ----
            A$set_oup_params(rho=rho,mu=mu,sigma=sigma)
            A$set_y_stoch_args(t=t,y=y,s=s,x=x,psi=psi)
          }
          # Initialize ----
          FromR6toUI()
          # Observe undo, sync, axes, plot (or enter key), left and rght ----
          undoButton <- reactiveVal(FALSE)
          syncButton <- reactiveVal(FALSE)
          axesButton <- reactiveVal(FALSE)
          plotButton <- reactiveVal(FALSE)
          leftButton <- reactiveVal(FALSE)
          rghtButton <- reactiveVal(FALSE)
          observe({
            undoButton(TRUE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$undoADoubleOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(TRUE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$syncADoubleOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(TRUE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$axesADoubleOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(TRUE)
            leftButton(FALSE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$plotADoubleOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(TRUE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$leftADoubleOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(TRUE)
          }) %>% bindEvent(input$rghtADoubleOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # User clicks save ----
          observe({
            FromUItoR6()
            A$default_save()
          }) %>% bindEvent(input$saveADoubleOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # User clicks undo, sync, axes, plot (or enter key), left or rght ----
          output$plotlyADoubleOUP <- renderPlotly({
            if(undoButton()) { A$default_read() }
            else if(syncButton())
            {
              FromUItoR6()
              A$sync_zyxt_stoch()
            }
            else if(axesButton())
            {
              FromUItoR6()
              A$axes_y_stoch()
            }
            else if(plotButton()) { FromUItoR6() }
            else if(leftButton())
            {
              plot_info <- A$get_plot_info()
              type <- plot_info$plottype[[1]]
              if(type > 5) { type <- 5 }
              else if(type > 4) { type <- 4 }
              else if(type > 3) { type <- 3 }
              else if(type > 2) { type <- 2 }
              else { type <- 5 }
              A$set_plot_info(type=type)
              FromUItoR6()
            }
            else if(rghtButton())
            {
              plot_info <- A$get_plot_info()
              type <- plot_info$plottype[[1]]
              if(type < 2) { type <- 2 }
              else if(type < 3) { type <- 3 }
              else if(type < 4) { type <- 4 }
              else if(type < 5) { type <- 5 }
              else { type <- 2 }
              A$set_plot_info(type=type)
              FromUItoR6()
            }
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(FALSE)
            FromR6toUI()
            A$PlotDoubleIntegral()
          }) %>% bindEvent(input$undoADoubleOUP,input$syncADoubleOUP,input$axesADoubleOUP,input$plotADoubleOUP,input$leftADoubleOUP,input$rghtADoubleOUP)
          # User clicks info ----
          observe({
            showModal(modalDialog(
              title=div(img(src="Roar32x32.png"),"Double Integral"),
              HTML("The Double Integral sums the probabilities one more time.  The effect is easiest to see at initial time <i>t</i> equal to <i>s</i>, when the Transition Density is the Dirac Density and the Transition Probability is the Heavyside Function.  Integrating the Dirac Density gives the Heavyside Function and integrating the Heavyside Function gives the Threshold Function.  The Threshold Function is kinked, like a payoff function for an option, and the Double Integral is the precursor to an analytical option pricing formula.  Thresholds are a property of stochastic processes, including the Ornstein-Uhlenbeck Process, and in a world of uncertainty over time, Options are not optional.<br><br>
              &emsp;&emsp;The R6 method:<br>
              &emsp;&emsp;&emsp;DoubleIntegral(<i>t,y,s,x,rho,mu,sigma,psi</i>)<br>
              &emsp;&emsp;with arguments:<br>
              &emsp;&emsp;&emsp;<i>t</i> are the variable times;<br>
              &emsp;&emsp;&emsp;<i>y</i> are the stochastic states;<br>
              &emsp;&emsp;&emsp;<i>s</i> is the fixed initial time;<br>
              &emsp;&emsp;&emsp;<i>x</i> is the fixed initial state;<br>
              &emsp;&emsp;&emsp;<i>rho</i> is the rate parameter;<br>
              &emsp;&emsp;&emsp;<i>mu</i> is the location parameter;<br>
              &emsp;&emsp;&emsp;<i>sigma</i> is the scale parameter;<br>
              &emsp;&emsp;&emsp;<i>psi</i> is < 0 to integrate from -Inf to y and > 0 to integrate from y to Inf;<br>
              &emsp;&emsp;returns:<br>
              <table style='margin-left: 60px;'>
                <tr>
                  <td style='border-top: solid silver; border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'>&Popf;(<i>t</i><sub>1</sub>,<i>y</i><sub>1</sub>)</td>
                  <td style='padding: 0px 4px 0px 4px;'>&hellip;</td>
                  <td style='padding: 0px 4px 0px 4px;'>&Popf;(<i>t</i><sub>1</sub>,<i>y</i><sub>n</sub>)</td>
                  <td style='border-top: solid silver; border-right: solid silver'>&nbsp;</td>
                </tr>
                <tr>
                  <td style='border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'>&emsp;&vellip;</td>
                  <td style='padding: 0px 4px 0px 4px;'>&dtdot;</td>
                  <td style='padding: 0px 4px 0px 4px;'>&emsp;&vellip;</td>
                  <td style='border-right: solid silver'>&nbsp;</td>
                </tr>
                <tr>
                  <td style='border-bottom: solid silver; border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'>&Popf;(<i>t</i><sub>m</sub>,<i>y</i><sub>1</sub>)</td>
                  <td style='padding: 0px 4px 0px 4px;'>&hellip;</td>
                  <td style='padding: 0px 4px 0px 4px;'>&Popf;(<i>t</i><sub>m</sub>,<i>y</i><sub>n</sub>)</td>
                  <td style='border-bottom: solid silver; border-right: solid silver'>&nbsp;</td>
                  <td style='padding-left: 2px;'>;</td>
                </tr>
              </table>
              &emsp;&emsp;where:<br>
              &emsp;&emsp;&emsp;&Popf; is the Double Integral."),
              easyClose = TRUE,
              footer = modalButton("Close")
            ))
          }) %>% bindEvent(input$infoADoubleOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
        # Option ----
        else if(input$navAOUP == "AOptionOUP")
        {
          # Define set/get functions ----
          FromR6toUI <- function()
          {
            # Get from OUP ----
            oup_params <- A$get_oup_params()
            x_stoch_args <- A$get_x_stoch_args()
            rho <-oup_params[[1]]
            mu <- oup_params[[2]]
            sigma <- oup_params[[3]]
            s <- x_stoch_args[[1]]
            x <- x_stoch_args[[2]]
            t <- x_stoch_args[[3]]
            y <- x_stoch_args[[4]]
            r <- x_stoch_args[[5]]
            phi <- x_stoch_args[[6]]
            b <- x_stoch_args[[7]]
            c <- x_stoch_args[[8]]
            m <- length(s)
            n <- length(x)
            sFrom <- s[m]
            sTo <- s[1]
            if(m > 1) { sBy <- (sTo-sFrom)/(m-1) }
            else  {sBy <- 0 }
            xFrom <- x[1]
            xTo <- x[n]
            if(n > 1) { xBy <- (xTo-xFrom)/(n-1) }
            else  {xBy <- 0 }
            # Set to UI ----
            updateNumericInput(session,"rhoAOptionOUP",value=rho)
            updateNumericInput(session,"muAOptionOUP",value=mu)
            updateNumericInput(session,"sigmaAOptionOUP",value=sigma)
            updateNumericInput(session,"sFromAOptionOUP",value=sFrom)
            updateNumericInput(session,"sToAOptionOUP",value=sTo)
            updateNumericInput(session,"sByAOptionOUP",value=sBy)
            updateNumericInput(session,"xFromAOptionOUP",value=xFrom)
            updateNumericInput(session,"xToAOptionOUP",value=xTo)
            updateNumericInput(session,"xByAOptionOUP",value=xBy)
            updateNumericInput(session,"tAOptionOUP",value=t)
            updateNumericInput(session,"yAOptionOUP",value=y)
            updateNumericInput(session,"rAOptionOUP",value=r)
            updateNumericInput(session,"phiAOptionOUP",value=phi)
            if(phi > 0) { updateNumericInput(session,"bcAOptionOUP",label="b",value=b) }
            else { updateNumericInput(session,"bcAOptionOUP",label="c",value=c) }
          }
          FromUItoR6 <- function()
          {
            # Get from UI ----
            rho <- input$rhoAOptionOUP
            if(!is.numeric(rho)) { rho <- 0 }
            else if(rho < 0) { rho <- 0 }
            mu <- input$muAOptionOUP
            if(!is.numeric(mu)) { mu <- 0 }
            sigma <- input$sigmaAOptionOUP
            if(!is.numeric(sigma)) { sigma <- 0 }
            sFrom <- input$sFromAOptionOUP
            if(!is.numeric(sFrom)) { sFrom <- 0 }
            sTo <- input$sToAOptionOUP
            if(!is.numeric(sTo)) { sTo <- 0 }
            sBy <- input$sByAOptionOUP
            if(!is.numeric(sBy)) { sBy <- 0 }
            if(abs(sTo-sFrom) < abs(sBy)) { sBy <- sTo-sFrom }
            if(abs(sTo-sFrom)/100 > abs(sBy)) { sBy <- (sTo-sFrom)/100 }
            if(sTo > sFrom) { s <- seq(from=sTo,to=sFrom,by=-abs(sBy)) }
            else if(sTo == sFrom) { s <- sTo }
            else { s <- seq(from=sFrom,to=sTo,by=-abs(sBy)) }
            xFrom <- input$xFromAOptionOUP
            xTo <- input$xToAOptionOUP
            xBy <- input$xByAOptionOUP
            xOK <- FALSE
            if(is.numeric(xFrom) & is.numeric(xTo))
            {
              if(xTo > xFrom)
              {
                if(is.numeric(xBy))
                {
                  if(xBy > (xTo-xFrom)/100) { xBy <- (xTo-xFrom)/100 }
                  else if(xBy < (xTo-xFrom)/1000) { xBy <- (xTo-xFrom)/1000 }
                }
                else { xBy <- (xTo-xFrom)/100 }
                xOK <- TRUE
              }
            }
            else if(is.numeric(xFrom) & is.numeric(xBy))
            {
              if(xBy > 0)
              {
                xTo <- xFrom+100*xBy
                xOK <- TRUE
              }
            }
            else if(is.numeric(xTo) & is.numeric(xBy))
            {
              if(xBy > 0)
              {
                xFrom <- xTo-100*xBy
                xOK <- TRUE
              }
            }
            t <- input$tAOptionOUP
            if(!is.numeric(t)) { t <- s[1] }
            else if(t < s[1]) { t <- s[1] }
            y <- input$yAOptionOUP
            if(!is.numeric(y)) { y <- 0 }
            r <- input$rAOptionOUP
            if(!is.numeric(r)) { r <- 0 }
            phi <- input$phiAOptionOUP
            if(!is.numeric(phi)) { phi <- -1 }
            else if(phi <= 0) { phi <- -1 }
            else if(phi > 0) { phi <- 1 }
            bc <- input$bcAOptionOUP
            if(!is.numeric(bc)) { bc <- 0 }
            # Set to OUP ----
            A$set_oup_params(rho=rho,mu=mu,sigma=sigma)
            if(xOK)
            {
              x <- seq(from=xFrom,to=xTo,by=xBy)
              A$set_x_stoch_args(s=s,x=x,t=t,y=y,r=r,phi=phi)
            }
            else
            {
              A$axes_x_stoch()
              A$set_x_stoch_args(t=t,y=y,r=r,phi=phi)
            }
            if(phi > 0) { A$set_x_stoch_args(b=bc) }
            else { A$set_x_stoch_args(c=bc) }
          }
          # Initialize ----
          FromR6toUI()
          # Observe phi ----
          observe({
            if(is.numeric(input$phiAOptionOUP))
            {
              if(input$phiAOptionOUP > 0)
              {
                b <- A$get_x_stoch_args()[[7]]
                updateNumericInput(session,"bcAOptionOUP",label="b",value=b)
              }
              else
              {
                c <- A$get_x_stoch_args()[[8]]
                updateNumericInput(session,"bcAOptionOUP",label="c",value=c)
              }
            }
          }) %>% bindEvent(input$phiAOptionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # Observe undo, sync, axes, plot (or enter key), left and rght ----
          undoButton <- reactiveVal(FALSE)
          syncButton <- reactiveVal(FALSE)
          axesButton <- reactiveVal(FALSE)
          plotButton <- reactiveVal(FALSE)
          leftButton <- reactiveVal(FALSE)
          rghtButton <- reactiveVal(FALSE)
          observe({
            undoButton(TRUE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$undoAOptionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(TRUE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$syncAOptionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(TRUE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$axesAOptionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(TRUE)
            leftButton(FALSE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$plotAOptionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(TRUE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$leftAOptionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(TRUE)
          }) %>% bindEvent(input$rghtAOptionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # User clicks save ----
          observe({
            FromUItoR6()
            A$default_save()
          }) %>% bindEvent(input$saveAOptionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # User clicks undo, sync, axes, plot (or enter key), left or rght ----
          output$plotlyAOptionOUP <- renderPlotly({
            if(undoButton()) { A$default_read() }
            else if(syncButton())
            {
              FromUItoR6()
              A$sync_zyxt_stoch()
            }
            else if(axesButton())
            {
              FromUItoR6()
              A$axes_x_stoch()
            }
            else if(plotButton()) { FromUItoR6() }
            else if(leftButton())
            {
              plot_info <- A$get_plot_info()
              type <- plot_info$plottype[[1]]
              if(type > 5) { type <- 5 }
              else if(type > 4) { type <- 4 }
              else if(type > 3) { type <- 3 }
              else if(type > 2) { type <- 2 }
              else { type <- 5 }
              A$set_plot_info(type=type)
              FromUItoR6()
            }
            else if(rghtButton())
            {
              plot_info <- A$get_plot_info()
              type <- plot_info$plottype[[1]]
              if(type < 2) { type <- 2 }
              else if(type < 3) { type <- 3 }
              else if(type < 4) { type <- 4 }
              else if(type < 5) { type <- 5 }
              else { type <- 2 }
              A$set_plot_info(type=type)
              FromUItoR6()
            }
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(FALSE)
            FromR6toUI()
            A$PlotOption()
          }) %>% bindEvent(input$undoAOptionOUP,input$syncAOptionOUP,input$axesAOptionOUP,input$plotAOptionOUP,input$leftAOptionOUP,input$rghtAOptionOUP)
          # User clicks info ----
          observe({
            showModal(modalDialog(
              title=div(img(src="Roar32x32.png"),"Option"),
              HTML("Probabilities are an initial-value problem with fixed initial time and state.  Options are a terminal-value problem with fixed terminal time and state.  A Double Integral becomes an Option by reinterpreting time <i>s</i> and state <i>x</i> as variable and time <i>t</i> and state <i>y</i> as fixed.  Multiplying by a discount factor gives the value of an Option discounted to time <i>s</i>.  The Ornstein-Uhlenbeck Process has a Double Integral and, hence, an analytical Option pricing formula.<br><br>
              &emsp;&emsp;The R6 method:<br>
              &emsp;&emsp;&emsp;Option(<i>s,x,t,y,rho,mu,sigma,r,phi,b,c</i>)<br>
              &emsp;&emsp;with arguments:<br>
              &emsp;&emsp;&emsp;<i>s</i> are the variable times;<br>
              &emsp;&emsp;&emsp;<i>x</i> are the stochastic states;<br>
              &emsp;&emsp;&emsp;<i>t</i> is the fixed terminal time;<br>
              &emsp;&emsp;&emsp;<i>y</i> is the fixed terminal state;<br>
              &emsp;&emsp;&emsp;<i>rho</i> is the rate parameter;<br>
              &emsp;&emsp;&emsp;<i>mu</i> is the location parameter;<br>
              &emsp;&emsp;&emsp;<i>sigma</i> is the scale parameter;<br>
              &emsp;&emsp;&emsp;<i>r</i> is the discount rate;<br>
              &emsp;&emsp;&emsp;<i>phi</i> is < 0 for an Exit Option, > 0 for an Entry Option, = 0 for either;<br>
              &emsp;&emsp;&emsp;<i>b</i> is a benefit or subsidy for an Entry Option;<br>
              &emsp;&emsp;&emsp;<i>c</i> is a cost or tax for an Exit Option;<br>
              &emsp;&emsp;returns:<br>
              <table style='margin-left: 60px;'>
                <tr>
                  <td style='border-top: solid silver; border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'>&Oopf;(<i>s</i><sub>1</sub>,<i>x</i><sub>1</sub>)</td>
                  <td style='padding: 0px 4px 0px 4px;'>&hellip;</td>
                  <td style='padding: 0px 4px 0px 4px;'>&Oopf;(<i>s</i><sub>1</sub>,<i>x</i><sub>n</sub>)</td>
                  <td style='border-top: solid silver; border-right: solid silver'>&nbsp;</td>
                </tr>
                <tr>
                  <td style='border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'>&emsp;&vellip;</td>
                  <td style='padding: 0px 4px 0px 4px;'>&dtdot;</td>
                  <td style='padding: 0px 4px 0px 4px;'>&emsp;&vellip;</td>
                  <td style='border-right: solid silver'>&nbsp;</td>
                </tr>
                <tr>
                  <td style='border-bottom: solid silver; border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'>&Oopf;(<i>s</i><sub>m</sub>,<i>x</i><sub>1</sub>)</td>
                  <td style='padding: 0px 4px 0px 4px;'>&hellip;</td>
                  <td style='padding: 0px 4px 0px 4px;'>&Oopf;(<i>s</i><sub>m</sub>,<i>x</i><sub>n</sub>)</td>
                  <td style='border-bottom: solid silver; border-right: solid silver'>&nbsp;</td>
                  <td style='padding-left: 2px;'>;</td>
                </tr>
              </table>
              &emsp;&emsp;where:<br>
              &emsp;&emsp;&emsp;&Oopf; is an Option."),
              easyClose = TRUE,
              footer = modalButton("Close")
            ))
          }) %>% bindEvent(input$infoAOptionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
        # Option Envelope----
        else if(input$navAOUP == "AEnvelopeOUP")
        {
          # Define set/get functions ----
          FromR6toUI <- function()
          {
            # Get from OUP ----
            oup_params <- A$get_oup_params()
            x_stoch_args <- A$get_x_stoch_args()
            rho <-oup_params[[1]]
            mu <- oup_params[[2]]
            sigma <- oup_params[[3]]
            s <- x_stoch_args[[1]]
            x <- x_stoch_args[[2]]
            t <- x_stoch_args[[3]]
            y <- x_stoch_args[[4]]
            r <- x_stoch_args[[5]]
            phi <- x_stoch_args[[6]]
            b <- x_stoch_args[[7]]
            c <- x_stoch_args[[8]]
            m <- length(s)
            n <- length(x)
            sFrom <- s[m]
            sTo <- s[1]
            if(m > 1) { sBy <- (sTo-sFrom)/(m-1) }
            else  {sBy <- 0 }
            xFrom <- x[1]
            xTo <- x[n]
            if(n > 1) { xBy <- (xTo-xFrom)/(n-1) }
            else  {xBy <- 0 }
            # Set to UI ----
            updateNumericInput(session,"rhoAEnvelopeOUP",value=rho)
            updateNumericInput(session,"muAEnvelopeOUP",value=mu)
            updateNumericInput(session,"sigmaAEnvelopeOUP",value=sigma)
            updateNumericInput(session,"sFromAEnvelopeOUP",value=sFrom)
            updateNumericInput(session,"sToAEnvelopeOUP",value=sTo)
            updateNumericInput(session,"sByAEnvelopeOUP",value=sBy)
            updateNumericInput(session,"xFromAEnvelopeOUP",value=xFrom)
            updateNumericInput(session,"xToAEnvelopeOUP",value=xTo)
            updateNumericInput(session,"xByAEnvelopeOUP",value=xBy)
            updateNumericInput(session,"tAEnvelopeOUP",value=t)
            updateNumericInput(session,"yAEnvelopeOUP",value=y)
            updateNumericInput(session,"rAEnvelopeOUP",value=r)
            updateNumericInput(session,"phiAEnvelopeOUP",value=phi)
            if(phi > 0) { updateNumericInput(session,"bcAEnvelopeOUP",label="b",value=b) }
            else { updateNumericInput(session,"bcAEnvelopeOUP",label="c",value=c) }
          }
          FromUItoR6 <- function()
          {
            # Get from UI ----
            rho <- input$rhoAEnvelopeOUP
            if(!is.numeric(rho)) { rho <- 0 }
            else if(rho < 0) { rho <- 0 }
            mu <- input$muAEnvelopeOUP
            if(!is.numeric(mu)) { mu <- 0 }
            sigma <- input$sigmaAEnvelopeOUP
            if(!is.numeric(sigma)) { sigma <- 0 }
            sFrom <- input$sFromAEnvelopeOUP
            if(!is.numeric(sFrom)) { sFrom <- 0 }
            sTo <- input$sToAEnvelopeOUP
            if(!is.numeric(sTo)) { sTo <- 0 }
            sBy <- input$sByAEnvelopeOUP
            if(!is.numeric(sBy)) { sBy <- 0 }
            if(abs(sTo-sFrom) < abs(sBy)) { sBy <- sTo-sFrom }
            if(abs(sTo-sFrom)/100 > abs(sBy)) { sBy <- (sTo-sFrom)/100 }
            if(sTo > sFrom) { s <- seq(from=sTo,to=sFrom,by=-abs(sBy)) }
            else if(sTo == sFrom) { s <- sTo }
            else { s <- seq(from=sFrom,to=sTo,by=-abs(sBy)) }
            xFrom <- input$xFromAEnvelopeOUP
            xTo <- input$xToAEnvelopeOUP
            xBy <- input$xByAEnvelopeOUP
            xOK <- FALSE
            if(is.numeric(xFrom) & is.numeric(xTo))
            {
              if(xTo > xFrom)
              {
                if(is.numeric(xBy))
                {
                  if(xBy > (xTo-xFrom)/100) { xBy <- (xTo-xFrom)/100 }
                  else if(xBy < (xTo-xFrom)/1000) { xBy <- (xTo-xFrom)/1000 }
                }
                else { xBy <- (xTo-xFrom)/100 }
                xOK <- TRUE
              }
            }
            else if(is.numeric(xFrom) & is.numeric(xBy))
            {
              if(xBy > 0)
              {
                xTo <- xFrom+100*xBy
                xOK <- TRUE
              }
            }
            else if(is.numeric(xTo) & is.numeric(xBy))
            {
              if(xBy > 0)
              {
                xFrom <- xTo-100*xBy
                xOK <- TRUE
              }
            }
            t <- input$tAEnvelopeOUP
            if(!is.numeric(t)) { t <- s[1] }
            else if(t < s[1]) { t <- s[1] }
            y <- input$yAEnvelopeOUP
            if(!is.numeric(y)) { y <- 0 }
            r <- input$rAEnvelopeOUP
            if(!is.numeric(r)) { r <- 0 }
            phi <- input$phiAEnvelopeOUP
            if(!is.numeric(phi)) { phi <- -1 }
            else if(phi <= 0) { phi <- -1 }
            else if(phi > 0) { phi <- 1 }
            bc <- input$bcAEnvelopeOUP
            if(!is.numeric(bc)) { bc <- 0 }
            # Set to OUP ----
            A$set_oup_params(rho=rho,mu=mu,sigma=sigma)
            if(xOK)
            {
              x <- seq(from=xFrom,to=xTo,by=xBy)
              A$set_x_stoch_args(s=s,x=x,t=t,y=y,r=r,phi=phi)
            }
            else
            {
              A$axes_x_stoch()
              A$set_x_stoch_args(t=t,y=y,r=r,phi=phi)
            }
            if(phi > 0) { A$set_x_stoch_args(b=bc) }
            else { A$set_x_stoch_args(c=bc) }
          }
          # Initialize ----
          FromR6toUI()
          # Observe phi ----
          observe({
            if(is.numeric(input$phiAEnvelopeOUP))
            {
              if(input$phiAEnvelopeOUP > 0)
              {
                b <- A$get_x_stoch_args()[[7]]
                updateNumericInput(session,"bcAEnvelopeOUP",label="b",value=b)
              }
              else
              {
                c <- A$get_x_stoch_args()[[8]]
                updateNumericInput(session,"bcAEnvelopeOUP",label="c",value=c)
              }
            }
          }) %>% bindEvent(input$phiAEnvelopeOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # Observe undo, sync, axes, plot (or enter key), left and rght ----
          undoButton <- reactiveVal(FALSE)
          syncButton <- reactiveVal(FALSE)
          axesButton <- reactiveVal(FALSE)
          plotButton <- reactiveVal(FALSE)
          leftButton <- reactiveVal(FALSE)
          rghtButton <- reactiveVal(FALSE)
          observe({
            undoButton(TRUE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$undoAEnvelopeOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(TRUE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$syncAEnvelopeOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(TRUE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$axesAEnvelopeOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(TRUE)
            leftButton(FALSE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$plotAEnvelopeOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(TRUE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$leftAEnvelopeOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(TRUE)
          }) %>% bindEvent(input$rghtAEnvelopeOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # User clicks save ----
          observe({
            FromUItoR6()
            A$default_save()
          }) %>% bindEvent(input$saveAEnvelopeOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # User clicks undo, sync, axes, plot (or enter key), left or rght ----
          output$plotlyAEnvelopeOUP <- renderPlotly({
            if(undoButton()) { A$default_read() }
            else if(syncButton())
            {
              FromUItoR6()
              A$sync_zyxt_stoch()
            }
            else if(axesButton())
            {
              FromUItoR6()
              A$axes_x_stoch()
            }
            else if(plotButton()) { FromUItoR6() }
            else if(leftButton())
            {
              plot_info <- A$get_plot_info()
              type <- plot_info$plottype[[1]]
              if(type > 4) { type <- 4 }
              else if(type > 3) { type <- 3 }
              else { type <- 4 }
              A$set_plot_info(type=type)
              FromUItoR6()
            }
            else if(rghtButton())
            {
              plot_info <- A$get_plot_info()
              type <- plot_info$plottype[[1]]
              if(type < 3) { type <- 3 }
              else if(type < 4) { type <- 4 }
              else { type <- 3 }
              A$set_plot_info(type=type)
              FromUItoR6()
            }
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(FALSE)
            FromR6toUI()
            A$PlotOptionEnvelope()
          }) %>% bindEvent(input$undoAEnvelopeOUP,input$syncAEnvelopeOUP,input$axesAEnvelopeOUP,input$plotAEnvelopeOUP,input$leftAEnvelopeOUP,input$rghtAEnvelopeOUP)
          # User clicks info ----
          observe({
            showModal(modalDialog(
              title=div(img(src="Roar32x32.png"),"Option Envelope"),
              HTML("A Financial Option is a contract between a buyer and a seller with a fixed expiry date.  A Real Option is not a contract.  There is neither buyer nor seller.  There is no fixed expiry date.  It is a Perpetual Option that a decision-maker can exercise whenever they choose.  If the maximum value of the Option is the payoff function, it should be exercised immediately.  If the maximum value of the Option is greater than the payoff function, it should be held and possibly exercised in the future.  The Option Envelope is the maximum value of either exercising or holding the option for all states, <i>x</i>.<br><br>
              &emsp;&emsp;The R6 method:<br>
              &emsp;&emsp;&emsp;OptionEnvelope(<i>x,y,rho,mu,sigma,r,phi,b,c</i>)<br>
              &emsp;&emsp;with arguments:<br>
              &emsp;&emsp;&emsp;<i>x</i> are the stochastic states;<br>
              &emsp;&emsp;&emsp;<i>y</i> is the fixed terminal state;<br>
              &emsp;&emsp;&emsp;<i>rho</i> is the rate parameter;<br>
              &emsp;&emsp;&emsp;<i>mu</i> is the location parameter;<br>
              &emsp;&emsp;&emsp;<i>sigma</i> is the scale parameter;<br>
              &emsp;&emsp;&emsp;<i>r</i> is the discount rate;<br>
              &emsp;&emsp;&emsp;<i>phi</i> is < 0 for an Exit Option, > 0 for an Entry Option, = 0 for either;<br>
              &emsp;&emsp;&emsp;<i>b</i> is a benefit or subsidy for an Entry Option;<br>
              &emsp;&emsp;&emsp;<i>c</i> is a cost or tax for an Exit Option;<br>
              &emsp;&emsp;returns:<br>
              <table style='margin-left: 60px;'>
                <tr>
                  <td style='border-top: solid silver; border-bottom: solid silver; border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'>\u00D4(<i>x</i><sub>1</sub>)</td>
                  <td style='padding: 0px 4px 0px 4px;'>&hellip;</td>
                  <td style='padding: 0px 4px 0px 4px;'>\u00D4(<i>x</i><sub>n</sub>)</td>
                  <td style='border-top: solid silver; border-bottom: solid silver; border-right: solid silver'>&nbsp;</td>
                  <td style='padding-left: 2px;'>;</td>
                </tr>
              </table>
              &emsp;&emsp;where:<br>
              &emsp;&emsp;&emsp;\u00D4 is an Option on the Envelope."),
              easyClose = TRUE,
              footer = modalButton("Close")
            ))
          }) %>% bindEvent(input$infoAEnvelopeOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
        # Decision Threshold ----
        else if(input$navAOUP == "ADecisionOUP")
        {
          # Define set/get functions ----
          FromR6toUI <- function()
          {
            # Get from OUP ----
            oup_params <- A$get_oup_params()
            x_stoch_args <- A$get_x_stoch_args()
            rho <-oup_params[[1]]
            mu <- oup_params[[2]]
            sigma <- oup_params[[3]]
            x <- x_stoch_args[[2]]
            y <- x_stoch_args[[4]]
            r <- x_stoch_args[[5]]
            phi <- x_stoch_args[[6]]
            b <- x_stoch_args[[7]]
            c <- x_stoch_args[[8]]
            n <- length(x)
            xFrom <- x[1]
            xTo <- x[n]
            if(n > 1) { xBy <- (xTo-xFrom)/(n-1) }
            else  {xBy <- 0 }
            # Set to UI ----
            updateNumericInput(session,"rhoADecisionOUP",value=rho)
            updateNumericInput(session,"muADecisionOUP",value=mu)
            updateNumericInput(session,"sigmaADecisionOUP",value=sigma)
            updateNumericInput(session,"xFromADecisionOUP",value=xFrom)
            updateNumericInput(session,"xToADecisionOUP",value=xTo)
            updateNumericInput(session,"xByADecisionOUP",value=xBy)
            updateNumericInput(session,"yADecisionOUP",value=y)
            updateNumericInput(session,"rADecisionOUP",value=r)
            updateNumericInput(session,"phiADecisionOUP",value=phi)
            if(phi > 0) { updateNumericInput(session,"bcADecisionOUP",label="b",value=b) }
            else { updateNumericInput(session,"bcADecisionOUP",label="c",value=c) }
          }
          FromUItoR6 <- function()
          {
            # Get from UI ----
            rho <- input$rhoADecisionOUP
            if(!is.numeric(rho)) { rho <- 0 }
            else if(rho < 0) { rho <- 0 }
            mu <- input$muADecisionOUP
            if(!is.numeric(mu)) { mu <- 0 }
            sigma <- input$sigmaADecisionOUP
            if(!is.numeric(sigma)) { sigma <- 0 }
            xFrom <- input$xFromADecisionOUP
            xTo <- input$xToADecisionOUP
            xBy <- input$xByADecisionOUP
            xOK <- FALSE
            if(is.numeric(xFrom) & is.numeric(xTo))
            {
              if(xTo > xFrom)
              {
                if(is.numeric(xBy))
                {
                  if(xBy > (xTo-xFrom)/100) { xBy <- (xTo-xFrom)/100 }
                  else if(xBy < (xTo-xFrom)/1000) { xBy <- (xTo-xFrom)/1000 }
                }
                else { xBy <- (xTo-xFrom)/100 }
                xOK <- TRUE
              }
            }
            else if(is.numeric(xFrom) & is.numeric(xBy))
            {
              if(xBy > 0)
              {
                xTo <- xFrom+100*xBy
                xOK <- TRUE
              }
            }
            else if(is.numeric(xTo) & is.numeric(xBy))
            {
              if(xBy > 0)
              {
                xFrom <- xTo-100*xBy
                xOK <- TRUE
              }
            }
            y <- input$yADecisionOUP
            if(!is.numeric(y)) { y <- 0 }
            r <- input$rADecisionOUP
            if(!is.numeric(r)) { r <- 0 }
            phi <- input$phiADecisionOUP
            if(!is.numeric(phi)) { phi <- -1 }
            else if(phi <= 0) { phi <- -1 }
            else if(phi > 0) { phi <- 1 }
            bc <- input$bcADecisionOUP
            if(!is.numeric(bc)) { bc <- 0 }
            # Set to OUP ----
            A$set_oup_params(rho=rho,mu=mu,sigma=sigma)
            if(xOK)
            {
              x <- seq(from=xFrom,to=xTo,by=xBy)
              A$set_x_stoch_args(x=x,y=y,r=r,phi=phi)
            }
            else
            {
              A$axes_x_stoch()
              A$set_x_stoch_args(y=y,r=r,phi=phi)
            }
            if(phi > 0) { A$set_x_stoch_args(b=bc) }
            else { A$set_x_stoch_args(c=bc) }
          }
          # Initialize ----
          FromR6toUI()
          # Observe phi ----
          observe({
            if(is.numeric(input$phiADecisionOUP))
            {
              if(input$phiADecisionOUP > 0)
              {
                b <- A$get_x_stoch_args()[[7]]
                updateNumericInput(session,"bcADecisionOUP",label="b",value=b)
              }
              else
              {
                c <- A$get_x_stoch_args()[[8]]
                updateNumericInput(session,"bcADecisionOUP",label="c",value=c)
              }
            }
          }) %>% bindEvent(input$phiADecisionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # Observe undo, sync, axes and plot (or enter key) ----
          undoButton <- reactiveVal(FALSE)
          syncButton <- reactiveVal(FALSE)
          axesButton <- reactiveVal(FALSE)
          plotButton <- reactiveVal(FALSE)
          observe({
            undoButton(TRUE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
          }) %>% bindEvent(input$undoADecisionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(TRUE)
            axesButton(FALSE)
            plotButton(FALSE)
          }) %>% bindEvent(input$syncADecisionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(TRUE)
            plotButton(FALSE)
          }) %>% bindEvent(input$axesADecisionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(TRUE)
          }) %>% bindEvent(input$plotADecisionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # User clicks save ----
          observe({
            FromUItoR6()
            A$default_save()
          }) %>% bindEvent(input$saveADecisionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # User clicks undo, sync, axes or plot (or enter key) ----
          output$plotlyADecisionOUP <- renderPlotly({
            if(undoButton()) { A$default_read() }
            else if(syncButton())
            {
              FromUItoR6()
              A$sync_zyxt_stoch()
            }
            else if(axesButton())
            {
              FromUItoR6()
              A$axes_x_stoch()
            }
            else if(plotButton()) { FromUItoR6() }
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            FromR6toUI()
            A$PlotDecisionThreshold()
          }) %>% bindEvent(input$undoADecisionOUP,input$syncADecisionOUP,input$axesADecisionOUP,input$plotADecisionOUP)
          # User clicks info ----
          observe({
            showModal(modalDialog(
              title=div(img(src="Roar32x32.png"),"Decision Threshold"),
              HTML("The Decision Threshold is the state <i>k</i> where a decision-maker will be indifferent between holding or exercising a Real Option.  The Option value at the threshold is the price of flexibility&mdash;the price of keeping options open.  It is the most a decision-maker will pay in costs rather than exit prematurely, or the most a decision-maker will forego in benefits rather than enter prematurely.<br><br>
              &emsp;&emsp;The R6 method:<br>
              &emsp;&emsp;&emsp;DecisionThreshold(<i>y,rho,mu,sigma,r,phi,b,c</i>)<br>
              &emsp;&emsp;with arguments:<br>
              &emsp;&emsp;&emsp;<i>y</i> is the fixed terminal state;<br>
              &emsp;&emsp;&emsp;<i>rho</i> is the rate parameter;<br>
              &emsp;&emsp;&emsp;<i>mu</i> is the location parameter;<br>
              &emsp;&emsp;&emsp;<i>sigma</i> is the scale parameter;<br>
              &emsp;&emsp;&emsp;<i>r</i> is the discount rate;<br>
              &emsp;&emsp;&emsp;<i>phi</i> is < 0 for an Exit Option, > 0 for an Entry Option, = 0 for either;<br>
              &emsp;&emsp;&emsp;<i>b</i> is a benefit or subsidy for an Entry Option;<br>
              &emsp;&emsp;&emsp;<i>c</i> is a cost or tax for an Exit Option;<br>
              &emsp;&emsp;returns:<br>
              <table style='margin-left: 60px;'>
                <tr>
                  <td style='border-top: solid silver; border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'><i>k</i></td>
                  <td style='border-top: solid silver; border-right: solid silver'>&nbsp;</td>
                </tr>
                <tr>
                  <td style='border-bottom: solid silver; border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'>\u00D4(<i>k</i>)</td>
                  <td style='border-bottom: solid silver; border-right: solid silver'>&nbsp;</td>
                  <td style='padding-left: 2px;'>;</td>
                </tr>
              </table>
              &emsp;&emsp;where:<br>
              &emsp;&emsp;&emsp;<i>k</i> is the state at the Decision Threshold;<br>
              &emsp;&emsp;&emsp;\u00D4 is the Option at the Decision Threshold."),
              easyClose = TRUE,
              footer = modalButton("Close")
            ))
          }) %>% bindEvent(input$infoADecisionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
        # Obligation ----
        else if(input$navAOUP == "AObligationOUP")
        {
          # Define set/get functions ----
          FromR6toUI <- function()
          {
            # Get from OUP ----
            oup_params <- A$get_oup_params()
            x_stoch_args <- A$get_x_stoch_args()
            rho <-oup_params[[1]]
            mu <- oup_params[[2]]
            s <- x_stoch_args[[1]]
            x <- x_stoch_args[[2]]
            t <- x_stoch_args[[3]]
            y <- x_stoch_args[[4]]
            r <- x_stoch_args[[5]]
            phi <- x_stoch_args[[6]]
            b <- x_stoch_args[[7]]
            c <- x_stoch_args[[8]]
            m <- length(s)
            n <- length(x)
            sFrom <- s[m]
            sTo <- s[1]
            if(m > 1) { sBy <- (sTo-sFrom)/(m-1) }
            else  {sBy <- 0 }
            xFrom <- x[1]
            xTo <- x[n]
            if(n > 1) { xBy <- (xTo-xFrom)/(n-1) }
            else  {xBy <- 0 }
            # Set to UI ----
            updateNumericInput(session,"rhoAObligationOUP",value=rho)
            updateNumericInput(session,"muAObligationOUP",value=mu)
            updateNumericInput(session,"sFromAObligationOUP",value=sFrom)
            updateNumericInput(session,"sToAObligationOUP",value=sTo)
            updateNumericInput(session,"sByAObligationOUP",value=sBy)
            updateNumericInput(session,"xFromAObligationOUP",value=xFrom)
            updateNumericInput(session,"xToAObligationOUP",value=xTo)
            updateNumericInput(session,"xByAObligationOUP",value=xBy)
            updateNumericInput(session,"tAObligationOUP",value=t)
            updateNumericInput(session,"yAObligationOUP",value=y)
            updateNumericInput(session,"rAObligationOUP",value=r)
            updateNumericInput(session,"phiAObligationOUP",value=phi)
            if(phi > 0) { updateNumericInput(session,"bcAObligationOUP",label="b",value=b) }
            else { updateNumericInput(session,"bcAObligationOUP",label="c",value=c) }
          }
          FromUItoR6 <- function()
          {
            # Get from UI ----
            rho <- input$rhoAObligationOUP
            if(!is.numeric(rho)) { rho <- 0 }
            else if(rho < 0) { rho <- 0 }
            mu <- input$muAObligationOUP
            if(!is.numeric(mu)) { mu <- 0 }
            sFrom <- input$sFromAObligationOUP
            if(!is.numeric(sFrom)) { sFrom <- 0 }
            sTo <- input$sToAObligationOUP
            if(!is.numeric(sTo)) { sTo <- 0 }
            sBy <- input$sByAObligationOUP
            if(!is.numeric(sBy)) { sBy <- 0 }
            if(abs(sTo-sFrom) < abs(sBy)) { sBy <- sTo-sFrom }
            if(abs(sTo-sFrom)/100 > abs(sBy)) { sBy <- (sTo-sFrom)/100 }
            if(sTo > sFrom) { s <- seq(from=sTo,to=sFrom,by=-abs(sBy)) }
            else if(sTo == sFrom) { s <- sTo }
            else { s <- seq(from=sFrom,to=sTo,by=-abs(sBy)) }
            xFrom <- input$xFromAObligationOUP
            xTo <- input$xToAObligationOUP
            xBy <- input$xByAObligationOUP
            xOK <- FALSE
            if(is.numeric(xFrom) & is.numeric(xTo))
            {
              if(xTo > xFrom)
              {
                if(is.numeric(xBy))
                {
                  if(xBy > (xTo-xFrom)/100) { xBy <- (xTo-xFrom)/100 }
                  else if(xBy < (xTo-xFrom)/1000) { xBy <- (xTo-xFrom)/1000 }
                }
                else { xBy <- (xTo-xFrom)/100 }
                xOK <- TRUE
              }
            }
            else if(is.numeric(xFrom) & is.numeric(xBy))
            {
              if(xBy > 0)
              {
                xTo <- xFrom+100*xBy
                xOK <- TRUE
              }
            }
            else if(is.numeric(xTo) & is.numeric(xBy))
            {
              if(xBy > 0)
              {
                xFrom <- xTo-100*xBy
                xOK <- TRUE
              }
            }
            t <- input$tAObligationOUP
            if(!is.numeric(t)) { t <- s[1] }
            else if(t < s[1]) { t <- s[1] }
            y <- input$yAObligationOUP
            if(!is.numeric(y)) { y <- 0 }
            r <- input$rAObligationOUP
            if(!is.numeric(r)) { r <- 0 }
            phi <- input$phiAObligationOUP
            if(!is.numeric(phi)) { phi <- -1 }
            else if(phi <= 0) { phi <- -1 }
            else if(phi > 0) { phi <- 1 }
            bc <- input$bcAObligationOUP
            if(!is.numeric(bc)) { bc <- 0 }
            # Set to OUP ----
            A$set_oup_params(rho=rho,mu=mu)
            if(xOK)
            {
              x <- seq(from=xFrom,to=xTo,by=xBy)
              A$set_x_stoch_args(s=s,x=x,t=t,y=y,r=r,phi=phi)
            }
            else
            {
              A$axes_x_stoch()
              A$set_x_stoch_args(t=t,y=y,r=r,phi=phi)
            }
            if(phi > 0) { A$set_x_stoch_args(b=bc) }
            else { A$set_x_stoch_args(c=bc) }
          }
          # Initialize ----
          FromR6toUI()
          # Observe phi ----
          observe({
            if(is.numeric(input$phiAObligationOUP))
            {
              if(input$phiAObligationOUP > 0)
              {
                b <- A$get_x_stoch_args()[[7]]
                updateNumericInput(session,"bcAObligationOUP",label="b",value=b)
              }
              else
              {
                c <- A$get_x_stoch_args()[[8]]
                updateNumericInput(session,"bcAObligationOUP",label="c",value=c)
              }
            }
          }) %>% bindEvent(input$phiAObligationOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # Observe undo, sync, axes, plot (or enter key), left and rght ----
          undoButton <- reactiveVal(FALSE)
          syncButton <- reactiveVal(FALSE)
          axesButton <- reactiveVal(FALSE)
          plotButton <- reactiveVal(FALSE)
          leftButton <- reactiveVal(FALSE)
          rghtButton <- reactiveVal(FALSE)
          observe({
            undoButton(TRUE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$undoAObligationOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(TRUE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$syncAObligationOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(TRUE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$axesAObligationOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(TRUE)
            leftButton(FALSE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$plotAObligationOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(TRUE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$leftAObligationOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(TRUE)
          }) %>% bindEvent(input$rghtAObligationOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # User clicks save ----
          observe({
            FromUItoR6()
            A$default_save()
          }) %>% bindEvent(input$saveAObligationOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # User clicks undo, sync, axes, plot (or enter key), left or rght ----
          output$plotlyAObligationOUP <- renderPlotly({
            if(undoButton()) { A$default_read() }
            else if(syncButton())
            {
              FromUItoR6()
              A$sync_zyxt_stoch()
            }
            else if(axesButton())
            {
              FromUItoR6()
              A$axes_x_stoch()
            }
            else if(plotButton()) { FromUItoR6() }
            else if(leftButton())
            {
              plot_info <- A$get_plot_info()
              type <- plot_info$plottype[[1]]
              if(type > 5) { type <- 5 }
              else if(type > 4) { type <- 4 }
              else if(type > 3) { type <- 3 }
              else if(type > 2) { type <- 2 }
              else { type <- 5 }
              A$set_plot_info(type=type)
              FromUItoR6()
            }
            else if(rghtButton())
            {
              plot_info <- A$get_plot_info()
              type <- plot_info$plottype[[1]]
              if(type < 2) { type <- 2 }
              else if(type < 3) { type <- 3 }
              else if(type < 4) { type <- 4 }
              else if(type < 5) { type <- 5 }
              else { type <- 2 }
              A$set_plot_info(type=type)
              FromUItoR6()
            }
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(FALSE)
            FromR6toUI()
            A$PlotObligation()
          }) %>% bindEvent(input$undoAObligationOUP,input$syncAObligationOUP,input$axesAObligationOUP,input$plotAObligationOUP,input$leftAObligationOUP,input$rghtAObligationOUP)
          # User clicks info ----
          observe({
            showModal(modalDialog(
              title=div(img(src="Roar32x32.png"),"Obligation"),
              HTML("In finance, the call/put parity transforms options from one to the other.  In Real Options, the intermediate formula in the transformation is called the Obligation&mdash;the obligation to take losses.  An Obligation equals the Entry Option minus the Exit Option.  Another name for an Obligation is a Benefit/Cost Analysis.  A negative Obligation is a Prohibition&mdash;the prohibition from taking gains.  A Prohibition equals the Exit Option minus the Entry Option.  Neither an Obligation nor a Prohibition is uncertain.  All uncertainty is in the options.<br><br>
              &emsp;&emsp;The R6 method:<br>
              &emsp;&emsp;&emsp;Obligation(<i>s,x,t,y,rho,mu,r,phi,b,c</i>)<br>
              &emsp;&emsp;with arguments:<br>
              &emsp;&emsp;&emsp;<i>s</i> are the variable times;<br>
              &emsp;&emsp;&emsp;<i>x</i> are the stochastic states;<br>
              &emsp;&emsp;&emsp;<i>t</i> is the fixed terminal time;<br>
              &emsp;&emsp;&emsp;<i>y</i> is the fixed terminal state;<br>
              &emsp;&emsp;&emsp;<i>rho</i> is the rate parameter;<br>
              &emsp;&emsp;&emsp;<i>mu</i> is the location parameter;<br>
              &emsp;&emsp;&emsp;<i>r</i> is the discount rate;<br>
              &emsp;&emsp;&emsp;<i>phi</i> is <= for an Obligation; > 0 for a Prohibition;<br>
              &emsp;&emsp;&emsp;<i>b</i> is a benefit or subsidy for an Entry Option;<br>
              &emsp;&emsp;&emsp;<i>c</i> is a cost or tax for an Exit Option;<br>
              &emsp;&emsp;returns:<br>
              <table style='float: left; margin-left: 60px;'>
                <tr>
                  <td style='border-top: solid silver; border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'>\uD835\uDD39(<i>s</i><sub>1</sub>,<i>x</i><sub>1</sub>)</td>
                  <td style='padding: 0px 4px 0px 4px;'>&hellip;</td>
                  <td style='padding: 0px 4px 0px 4px;'>\uD835\uDD39(<i>s</i><sub>1</sub>,<i>x</i><sub>n</sub>)</td>
                  <td style='border-top: solid silver; border-right: solid silver'>&nbsp;</td>
                </tr>
                <tr>
                  <td style='border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'>&emsp;&vellip;</td>
                  <td style='padding: 0px 4px 0px 4px;'>&dtdot;</td>
                  <td style='padding: 0px 4px 0px 4px;'>&emsp;&vellip;</td>
                  <td style='border-right: solid silver'>&nbsp;</td>
                </tr>
                <tr>
                  <td style='border-bottom: solid silver; border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'>\uD835\uDD39(<i>s</i><sub>m</sub>,<i>x</i><sub>1</sub>)</td>
                  <td style='padding: 0px 4px 0px 4px;'>&hellip;</td>
                  <td style='padding: 0px 4px 0px 4px;'>\uD835\uDD39(<i>s</i><sub>m</sub>,<i>x</i><sub>n</sub>)</td>
                  <td style='border-bottom: solid silver; border-right: solid silver'>&nbsp;</td>
                  <td style='padding-left: 2px;'>;</td>
                </tr>
              </table>
              <table style='float: left; margin-left: 10px; margin-right: 10px;'>
                <tr>
                  <td>&nbsp;</td>
                </tr>
                <tr>
                  <td>or</td>
                </tr>
              </table>
              <table>
                <tr>
                  <td style='border-top: solid silver; border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'><strong>\u2102</strong>(<i>s</i><sub>1</sub>,<i>x</i><sub>1</sub>)</td>
                  <td style='padding: 0px 4px 0px 4px;'>&hellip;</td>
                  <td style='padding: 0px 4px 0px 4px;'><strong>\u2102</strong>(<i>s</i><sub>1</sub>,<i>x</i><sub>n</sub>)</td>
                  <td style='border-top: solid silver; border-right: solid silver'>&nbsp;</td>
                </tr>
                <tr>
                  <td style='border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'>&emsp;&vellip;</td>
                  <td style='padding: 0px 4px 0px 4px;'>&dtdot;</td>
                  <td style='padding: 0px 4px 0px 4px;'>&emsp;&vellip;</td>
                  <td style='border-right: solid silver'>&nbsp;</td>
                </tr>
                <tr>
                  <td style='border-bottom: solid silver; border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'><strong>\u2102</strong>(<i>s</i><sub>m</sub>,<i>x</i><sub>1</sub>)</td>
                  <td style='padding: 0px 4px 0px 4px;'>&hellip;</td>
                  <td style='padding: 0px 4px 0px 4px;'><strong>\u2102</strong>(<i>s</i><sub>m</sub>,<i>x</i><sub>n</sub>)</td>
                  <td style='border-bottom: solid silver; border-right: solid silver'>&nbsp;</td>
                  <td style='padding-left: 2px;'>;</td>
                </tr>
              </table>
              &emsp;&emsp;where:<br>
              &emsp;&emsp;&emsp;\uD835\uDD39 is an Obligation with positive benefits and negative costs;<br>
              &emsp;&emsp;&emsp;<strong>\u2102</strong> is a Prohibition with positive costs and negative benefits."),
              easyClose = TRUE,
              footer = modalButton("Close")
            ))
          }) %>% bindEvent(input$infoAObligationOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
        # Passage Time Mode, Median and Mean ----
        else if(input$navAOUP == "APTModeMedianMeanOUP")
        {
          # Define set/get functions ----
          FromR6toUI <- function()
          {
            # Get from OUP ----
            oup_params <- A$get_oup_params()
            t_stoch_args <- A$get_t_stoch_args()
            plot_info <- A$get_plot_info()
            rho <- oup_params[[1]]
            mu <- oup_params[[2]]
            sigma <- oup_params[[3]]
            t <- t_stoch_args[[1]]
            k <- t_stoch_args[[2]]
            s <- t_stoch_args[[3]]
            x <- t_stoch_args[[4]]
            z <- t_stoch_args[[5]]
            omega <- t_stoch_args[[6]]
            ptmax <- plot_info$plottype[[3]]
            m <- length(t)
            n <- length(z)
            tFrom <- t[1]
            tTo <- t[m]
            if(m > 1) { tBy <- (tTo-tFrom)/(m-1) }
            else  {tBy <- 0 }
            zFrom <- z[1]
            zTo <- z[n]
            if(n > 1) { zBy <- (zTo-zFrom)/(n-1) }
            else  {zBy <- 0 }
            # Set to UI ----
            updateNumericInput(session,"rhoAPTModeMedianMeanOUP",value=rho)
            updateNumericInput(session,"muAPTModeMedianMeanOUP",value=mu)
            updateNumericInput(session,"sigmaAPTModeMedianMeanOUP",value=sigma)
            updateNumericInput(session,"tFromAPTModeMedianMeanOUP",value=tFrom)
            updateNumericInput(session,"tToAPTModeMedianMeanOUP",value=tTo)
            updateNumericInput(session,"tByAPTModeMedianMeanOUP",value=tBy)
            updateNumericInput(session,"zFromAPTModeMedianMeanOUP",value=zFrom)
            updateNumericInput(session,"zToAPTModeMedianMeanOUP",value=zTo)
            updateNumericInput(session,"zByAPTModeMedianMeanOUP",value=zBy)
            updateNumericInput(session,"kAPTModeMedianMeanOUP",value=k)
            updateNumericInput(session,"sAPTModeMedianMeanOUP",value=s)
            updateNumericInput(session,"xAPTModeMedianMeanOUP",value=x)
            updateNumericInput(session,"omegaAPTModeMedianMeanOUP",value=omega)
            updateNumericInput(session,"ptmaxAPTModeMedianMeanOUP",value=ptmax)
          }
          FromUItoR6 <- function()
          {
            # Get from UI ----
            rho <- input$rhoAPTModeMedianMeanOUP
            if(!is.numeric(rho)) { rho <- 0 }
            else if(rho < 0) { rho <- 0 }
            mu <- input$muAPTModeMedianMeanOUP
            if(!is.numeric(mu)) { mu <- 0 }
            sigma <- input$sigmaAPTModeMedianMeanOUP
            if(!is.numeric(sigma)) { sigma <- 0 }
            tfrom <- input$tFromAPTModeMedianMeanOUP
            if(!is.numeric(tfrom)) { tfrom <- 0 }
            tto <- input$tToAPTModeMedianMeanOUP
            if(!is.numeric(tto)) { tto <- 0 }
            tby <- input$tByAPTModeMedianMeanOUP
            if(!is.numeric(tby)) { tby <- 0 }
            if(abs(tto-tfrom) < abs(tby)) { tby <- tto-tfrom }
            if(abs(tto-tfrom)/100 > abs(tby)) { tby <- (tto-tfrom)/100 }
            if(tto > tfrom) { t <- seq(from=tfrom,to=tto,by=abs(tby)) }
            else if(tto == tfrom) { t <- tfrom }
            else { t <- seq(from=tto,to=tfrom,by=abs(tby)) }
            zFrom <- input$zFromAPTModeMedianMeanOUP
            if(!is.numeric(zFrom)) { zFrom <- 0 }
            zTo <- input$zToAPTModeMedianMeanOUP
            if(!is.numeric(zTo)) { zTo <- 0 }
            zBy <- input$zByAPTModeMedianMeanOUP
            if(!is.numeric(zBy)) { zBy <- 0 }
            if(abs(zTo-zFrom) < abs(zBy)) { zBy <- zTo-zFrom }
            if(abs(zTo-zFrom)/100 > abs(zBy)) { zBy <- (zTo-zFrom)/100 }
            if(zTo > zFrom) { z <- seq(from=zFrom,to=zTo,by=abs(zBy)) }
            else if(zTo == zFrom) { z <- zFrom }
            else { z <- seq(from=zTo,to=zFrom,by=abs(zBy)) }
            k <- input$kAPTModeMedianMeanOUP
            if(!is.numeric(k)) { k <- 0 }
            s <- input$sAPTModeMedianMeanOUP
            if(!is.numeric(s)) { s <- t[1] }
            else if(s > t[1]) { s <- t[1] }
            x <- input$xAPTModeMedianMeanOUP
            if(!is.numeric(x)) { x <- -mu }
            omega <- input$omegaAPTModeMedianMeanOUP
            if(!is.numeric(omega)) { omega <- 1 }
            else if(omega < 0) { omega <- 0 }
            else if(omega > 1) { omega <- 1 }
            ptmax <- input$ptmaxAPTModeMedianMeanOUP
            if(!is.numeric(ptmax)) { ptmax <- NaN }
            # Set to OUP ----
            A$set_oup_params(rho=rho,mu=mu,sigma=sigma)
            A$set_t_stoch_args(t=t,k=k,s=s,x=x,z=z,omega=omega)
            A$set_plot_info(ptmax=ptmax)
          }
          # Initialize ----
          FromR6toUI()
          # Observe undo, sync, axes, plot (or enter key), left and rght ----
          undoButton <- reactiveVal(FALSE)
          syncButton <- reactiveVal(FALSE)
          axesButton <- reactiveVal(FALSE)
          plotButton <- reactiveVal(FALSE)
          leftButton <- reactiveVal(FALSE)
          rghtButton <- reactiveVal(FALSE)
          observe({
            undoButton(TRUE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$undoAPTModeMedianMeanOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(TRUE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$syncAPTModeMedianMeanOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(TRUE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$axesAPTModeMedianMeanOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(TRUE)
            leftButton(FALSE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$plotAPTModeMedianMeanOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(TRUE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$leftAPTModeMedianMeanOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(TRUE)
          }) %>% bindEvent(input$rghtAPTModeMedianMeanOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # User clicks save ----
          observe({
            FromUItoR6()
            A$default_save()
          }) %>% bindEvent(input$saveAPTModeMedianMeanOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # User clicks undo, sync, axes, plot (or enter key), left or rght ----
          output$plotlyAPTModeMedianMeanOUP <- renderPlotly({
            if(undoButton()) { A$default_read() }
            else if(syncButton())
            {
              FromUItoR6()
              A$sync_zyxt_stoch()
            }
            else if(axesButton())
            {
              FromUItoR6()
              A$axes_t_stoch()
            }
            else if(plotButton()) { FromUItoR6() }
            else if(leftButton())
            {
              plot_info <- A$get_plot_info()
              type <- plot_info$plottype[[1]]
              if(type > 6) { type <- 6 }
              else if(type > 5) { type <- 5 }
              else if(type > 4) { type <- 4 }
              else if(type > 3) { type <- 3 }
              else if(type > 2) { type <- 2 }
              else if(type > 1) { type <- 1 }
              else { type <- 6 }
              A$set_plot_info(type=type)
              FromUItoR6()
            }
            else if(rghtButton())
            {
              plot_info <- A$get_plot_info()
              type <- plot_info$plottype[[1]]
              if(type < 1) { type <- 1 }
              else if(type < 2) { type <- 2 }
              else if(type < 3) { type <- 3 }
              else if(type < 4) { type <- 4 }
              else if(type < 5) { type <- 5 }
              else if(type < 6) { type <- 6 }
              else { type <- 1 }
              A$set_plot_info(type=type)
              FromUItoR6()
            }
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(FALSE)
            FromR6toUI()
            A$PlotPassageTimeModeMedianMean()
          }) %>% bindEvent(input$undoAPTModeMedianMeanOUP,input$syncAPTModeMedianMeanOUP,input$axesAPTModeMedianMeanOUP,input$plotAPTModeMedianMeanOUP,input$leftAPTModeMedianMeanOUP,input$rghtAPTModeMedianMeanOUP)
          # User clicks info ----
          observe({
            showModal(modalDialog(
              title=div(img(src="Roar32x32.png"),"Passage Time Mode, Median and Mean"),
              HTML("If crossing a threshold is irreversible, the Mode is the most likely time to cross, the Median is the time with a 50% chance the threshold has already been crossed and the Mean is the expected time to cross.  If crossing is partially or completely reversible, net visits are crossings to the far side minus returns to the near side.  The Mode is when net visits are greatest.  The Median is when net visits reach 50% of the long-term proportion of time spent on the far side.  The Mean is the expected time of net visits to the far side.  If the Ornstein-Uhlenbeck Process is attracted across a threshold, the Mode is less than the Median is less than the Mean.  If, however, the process is attracted to a location away from the threshold, the Mean can be less than the Median can be less than the Mode.  If the process is not attracted at all, with a rate of convergence of zero, the Mean does not exist and the expected time to cross a threshold is unknown.<br><br>
              &emsp;&emsp;The R6 methods:<br>
              &emsp;&emsp;&emsp;PassageTimeMode(<i>k,s,x,z,omega,rho,mu,sigma</i>)<br>
              &emsp;&emsp;&emsp;PassageTimeMedian(<i>k,s,x,z,omega,rho,mu,sigma</i>)<br>
              &emsp;&emsp;&emsp;PassageTimeMean(<i>k,s,x,z,omega,rho,mu,sigma</i>)<br>
              &emsp;&emsp;with arguments:<br>
              &emsp;&emsp;&emsp;<i>k</i> is the threshold;<br>
              &emsp;&emsp;&emsp;<i>s</i> is the fixed initial time;<br>
              &emsp;&emsp;&emsp;<i>x</i> is the fixed initial state;<br>
              &emsp;&emsp;&emsp;<i>z</i> are alternate initial states;<br>
              &emsp;&emsp;&emsp;<i>omega</i> is the degree of irreversibility;<br>
              &emsp;&emsp;&emsp;<i>rho</i> is the rate parameter;<br>
              &emsp;&emsp;&emsp;<i>mu</i> is the location parameter;<br>
              &emsp;&emsp;&emsp;<i>sigma</i> is the scale parameter;<br>
              &emsp;&emsp;return:<br>
              <table style='float: left; margin-left: 60px;'>
                <tr>
                  <td style='border-top: solid silver; border-bottom: solid silver; border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'>mode(<i>x</i>)</td>
                  <td style='border-top: solid silver; border-bottom: solid silver; border-right: solid silver'>&nbsp;</td>
                </tr>
              </table>
              <table style='float: left; margin-left: 14px; margin-right: 18px;'>
                <tr>
                  <td>and</td>
                </tr>
              </table>
              <table>
                <tr>
                  <td style='border-top: solid silver; border-bottom: solid silver; border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'>mode(<i>z</i><sub>1</sub>)</td>
                  <td style='padding: 0px 4px 0px 4px;'>&hellip;</td>
                  <td style='padding: 0px 4px 0px 4px;'>mode(<i>z</i><sub>n</sub>)</td>
                  <td style='border-top: solid silver; border-bottom: solid silver; border-right: solid silver'>&nbsp;</td>
                  <td style='padding-left: 2px;'>;</td>
                </tr>
              </table>
              <table style='float: left; margin-left: 60px;'>
                <tr>
                  <td style='border-top: solid silver; border-bottom: solid silver; border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'>median(<i>x</i>)</td>
                  <td style='border-top: solid silver; border-bottom: solid silver; border-right: solid silver'>&nbsp;</td>
                </tr>
              </table>
              <table style='float: left; margin-left: 10px; margin-right: 10px;'>
                <tr>
                  <td>and</td>
                </tr>
              </table>
              <table>
                <tr>
                  <td style='border-top: solid silver; border-bottom: solid silver; border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'>median(<i>z</i><sub>1</sub>)</td>
                  <td style='padding: 0px 4px 0px 4px;'>&hellip;</td>
                  <td style='padding: 0px 4px 0px 4px;'>median(<i>z</i><sub>n</sub>)</td>
                  <td style='border-top: solid silver; border-bottom: solid silver; border-right: solid silver'>&nbsp;</td>
                  <td style='padding-left: 2px;'>;</td>
                </tr>
              </table>
              <table style='float: left; margin-left: 60px;'>
                <tr>
                  <td style='border-top: solid silver; border-bottom: solid silver; border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'>mean(<i>x</i>)</td>
                  <td style='border-top: solid silver; border-bottom: solid silver; border-right: solid silver'>&nbsp;</td>
                </tr>
              </table>
              <table style='float: left; margin-left: 14px; margin-right: 18px;'>
                <tr>
                  <td>and</td>
                </tr>
              </table>
              <table>
                <tr>
                  <td style='border-top: solid silver; border-bottom: solid silver; border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'>mean(<i>z</i><sub>1</sub>)</td>
                  <td style='padding: 0px 4px 0px 4px;'>&hellip;</td>
                  <td style='padding: 0px 4px 0px 4px;'>mean(<i>z</i><sub>n</sub>)</td>
                  <td style='border-top: solid silver; border-bottom: solid silver; border-right: solid silver'>&nbsp;</td>
                  <td style='padding-left: 2px;'>;</td>
                </tr>
              </table>
              &emsp;&emsp;where:<br>
              &emsp;&emsp;&emsp;mode(<i>x</i>), median(<i>x</i>) and mean(<i>x</i>) are the Passage Time Mode, Median and Mean at <i>x</i>;<br>
              &emsp;&emsp;&emsp;mode(<i>z</i><sub>j</sub>), median(<i>z</i><sub>j</sub>) and mean(<i>z</i><sub>j</sub>) are the Passage Time Mode, Median and Mean for <i>x=z</i><sub>j</sub>;<br>"),
              easyClose = TRUE,
              footer = modalButton("Close"),
              size = "l"
            ))
          }) %>% bindEvent(input$infoAPTModeMedianMeanOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
        # Passage Time Variance ----
        else if(input$navAOUP == "APTVarianceOUP")
        {
          # Define set/get functions ----
          FromR6toUI <- function()
          {
            # Get from OUP ----
            oup_params <- A$get_oup_params()
            t_stoch_args <- A$get_t_stoch_args()
            rho <- oup_params[[1]]
            mu <- oup_params[[2]]
            sigma <- oup_params[[3]]
            k <- t_stoch_args[[2]]
            s <- t_stoch_args[[3]]
            x <- t_stoch_args[[4]]
            z <- t_stoch_args[[5]]
            omega <- t_stoch_args[[6]]
            n <- length(z)
            zFrom <- z[1]
            zTo <- z[n]
            if(n > 1) { zBy <- (zTo-zFrom)/(n-1) }
            else  {zBy <- 0 }
            # Set to UI ----
            updateNumericInput(session,"rhoAPTVarianceOUP",value=rho)
            updateNumericInput(session,"muAPTVarianceOUP",value=mu)
            updateNumericInput(session,"sigmaAPTVarianceOUP",value=sigma)
            updateNumericInput(session,"zFromAPTVarianceOUP",value=zFrom)
            updateNumericInput(session,"zToAPTVarianceOUP",value=zTo)
            updateNumericInput(session,"zByAPTVarianceOUP",value=zBy)
            updateNumericInput(session,"kAPTVarianceOUP",value=k)
            updateNumericInput(session,"sAPTVarianceOUP",value=s)
            updateNumericInput(session,"xAPTVarianceOUP",value=x)
            updateNumericInput(session,"omegaAPTVarianceOUP",value=omega)
          }
          FromUItoR6 <- function()
          {
            # Get from UI ----
            t_stoch_args <- A$get_t_stoch_args()
            t <- t_stoch_args[[1]]
            rho <- input$rhoAPTVarianceOUP
            if(!is.numeric(rho)) { rho <- 0 }
            else if(rho < 0) { rho <- 0 }
            mu <- input$muAPTVarianceOUP
            if(!is.numeric(mu)) { mu <- 0 }
            sigma <- input$sigmaAPTVarianceOUP
            if(!is.numeric(sigma)) { sigma <- 0 }
            zFrom <- input$zFromAPTVarianceOUP
            if(!is.numeric(zFrom)) { zFrom <- 0 }
            zTo <- input$zToAPTVarianceOUP
            if(!is.numeric(zTo)) { zTo <- 0 }
            zBy <- input$zByAPTVarianceOUP
            if(!is.numeric(zBy)) { zBy <- 0 }
            if(abs(zTo-zFrom) < abs(zBy)) { zBy <- zTo-zFrom }
            if(abs(zTo-zFrom)/100 > abs(zBy)) { zBy <- (zTo-zFrom)/100 }
            if(zTo > zFrom) { z <- seq(from=zFrom,to=zTo,by=abs(zBy)) }
            else if(zTo == zFrom) { z <- zFrom }
            else { z <- seq(from=zTo,to=zFrom,by=abs(zBy)) }
            k <- input$kAPTVarianceOUP
            if(!is.numeric(k)) { k <- 0 }
            s <- input$sAPTVarianceOUP
            if(!is.numeric(s)) { s <- t[1] }
            else if(s > t[1]) { s <- t[1] }
            x <- input$xAPTVarianceOUP
            if(!is.numeric(x)) { x <- -mu }
            omega <- input$omegaAPTVarianceOUP
            if(!is.numeric(omega)) { omega <- 1 }
            else if(omega < 0) { omega <- 0 }
            else if(omega > 1) { omega <- 1 }
            # Set to OUP ----
            A$set_oup_params(rho=rho,mu=mu,sigma=sigma)
            A$set_t_stoch_args(NULL,k=k,s=s,x=x,z=z,omega=omega)
          }
          # Initialize ----
          FromR6toUI()
          # Observe undo, sync, axes, plot (or enter key), left and rght ----
          undoButton <- reactiveVal(FALSE)
          syncButton <- reactiveVal(FALSE)
          axesButton <- reactiveVal(FALSE)
          plotButton <- reactiveVal(FALSE)
          leftButton <- reactiveVal(FALSE)
          rghtButton <- reactiveVal(FALSE)
          observe({
            undoButton(TRUE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$undoAPTVarianceOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(TRUE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$syncAPTVarianceOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(TRUE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$axesAPTVarianceOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(TRUE)
            leftButton(FALSE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$plotAPTVarianceOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(TRUE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$leftAPTVarianceOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(TRUE)
          }) %>% bindEvent(input$rghtAPTVarianceOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # User clicks save ----
          observe({
            FromUItoR6()
            A$default_save()
          }) %>% bindEvent(input$saveAPTVarianceOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # User clicks undo, sync, axes, plot (or enter key), left or rght ----
          output$plotlyAPTVarianceOUP <- renderPlotly({
            if(undoButton()) { A$default_read() }
            else if(syncButton())
            {
              FromUItoR6()
              A$sync_zyxt_stoch()
            }
            else if(axesButton())
            {
              FromUItoR6()
              A$axes_t_stoch()
            }
            else if(plotButton()) { FromUItoR6() }
            else if(leftButton())
            {
              plot_info <- A$get_plot_info()
              type <- plot_info$plottype[[1]]
              if(type > 4) { type <- 4 }
              else if(type > 3) { type <- 3 }
              else { type <- 4 }
              A$set_plot_info(type=type)
              FromUItoR6()
            }
            else if(rghtButton())
            {
              plot_info <- A$get_plot_info()
              type <- plot_info$plottype[[1]]
              if(type < 3) { type <- 3 }
              else if(type < 4) { type <- 4 }
              else { type <- 3 }
              A$set_plot_info(type=type)
              FromUItoR6()
            }
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(FALSE)
            FromR6toUI()
            A$PlotPassageTimeVariance()
          }) %>% bindEvent(input$undoAPTVarianceOUP,input$syncAPTVarianceOUP,input$axesAPTVarianceOUP,input$plotAPTVarianceOUP,input$leftAPTVarianceOUP,input$rghtAPTVarianceOUP)
          # User clicks info ----
          observe({
            showModal(modalDialog(
              title=div(img(src="Roar32x32.png"),"Passage Time Variance"),
              HTML("If the Ornstein-Uhlenbeck Process has a larger Variance, the chance of bouncing across a threshold will be greater and the Passage Time will have a smaller Variance.  More uncertainty about the evolution of the state translates to less uncertainty about crossing a threshold.  If the Ornstein-Uhlenbeck Process converges slowly, the Passage Time Density is 'fat-tailed' and the Passage Time Variance may not exist.  The uncertainty about crossing a threshold may be unknown.<br><br>
              &emsp;&emsp;The R6 method:<br>
              &emsp;&emsp;&emsp;PassageTimeVariance(<i>k,s,x,z,omega,rho,mu,sigma</i>)<br>
              &emsp;&emsp;with arguments:<br>
              &emsp;&emsp;&emsp;<i>k</i> is the threshold;<br>
              &emsp;&emsp;&emsp;<i>s</i> is the fixed initial time;<br>
              &emsp;&emsp;&emsp;<i>x</i> is the fixed initial state;<br>
              &emsp;&emsp;&emsp;<i>z</i> are alternate initial states;<br>
              &emsp;&emsp;&emsp;<i>omega</i> is the degree of irreversibility;<br>
              &emsp;&emsp;&emsp;<i>rho</i> is the rate parameter;<br>
              &emsp;&emsp;&emsp;<i>mu</i> is the location parameter;<br>
              &emsp;&emsp;&emsp;<i>sigma</i> is the scale parameter;<br>
              &emsp;&emsp;returns:<br>
              <table style='float: left; margin-left: 60px;'>
                <tr>
                  <td style='border-top: solid silver; border-bottom: solid silver; border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'>variance(<i>x</i>)</td>
                  <td style='border-top: solid silver; border-bottom: solid silver; border-right: solid silver'>&nbsp;</td>
                </tr>
              </table>
              <table style='float: left; margin-left: 10px; margin-right: 10px;'>
                <tr>
                  <td>and</td>
                </tr>
              </table>
              <table>
                <tr>
                  <td style='border-top: solid silver; border-bottom: solid silver; border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'>variance(<i>z</i><sub>1</sub>)</td>
                  <td style='padding: 0px 4px 0px 4px;'>&hellip;</td>
                  <td style='padding: 0px 4px 0px 4px;'>variance(<i>z</i><sub>n</sub>)</td>
                  <td style='border-top: solid silver; border-bottom: solid silver; border-right: solid silver'>&nbsp;</td>
                  <td style='padding-left: 2px;'>;</td>
                </tr>
              </table>
              &emsp;&emsp;where:<br>
              &emsp;&emsp;&emsp;variance(<i>x</i>) is the Passage Time Variance at <i>x</i>;<br>
              &emsp;&emsp;&emsp;variance(<i>z</i><sub>j</sub>) is the Passage Time Variance for <i>x=z</i><sub>j</sub>."),
              easyClose = TRUE,
              footer = modalButton("Close")
            ))
          }) %>% bindEvent(input$infoAPTVarianceOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
        # Passage Time Percentiles ----
        else if(input$navAOUP == "APTPercentilesOUP")
        {
          # Define set/get functions ----
          FromR6toUI <- function()
          {
            # Get from OUP ----
            oup_params <- A$get_oup_params()
            t_stoch_args <- A$get_t_stoch_args()
            plot_info <- A$get_plot_info()
            rho <- oup_params[[1]]
            mu <- oup_params[[2]]
            sigma <- oup_params[[3]]
            t <- t_stoch_args[[1]]
            k <- t_stoch_args[[2]]
            s <- t_stoch_args[[3]]
            x <- t_stoch_args[[4]]
            z <- t_stoch_args[[5]]
            omega <- t_stoch_args[[6]]
            Ppct <- t_stoch_args[[7]]
            ptmax <- plot_info$plottype[[3]]
            m <- length(t)
            n <- length(z)
            tFrom <- t[1]
            tTo <- t[m]
            if(m > 1) { tBy <- (tTo-tFrom)/(m-1) }
            else  {tBy <- 0 }
            zFrom <- z[1]
            zTo <- z[n]
            if(n > 1) { zBy <- (zTo-zFrom)/(n-1) }
            else  {zBy <- 0 }
            # Set to UI ----
            updateNumericInput(session,"rhoAPTPercentilesOUP",value=rho)
            updateNumericInput(session,"muAPTPercentilesOUP",value=mu)
            updateNumericInput(session,"sigmaAPTPercentilesOUP",value=sigma)
            updateNumericInput(session,"tFromAPTPercentilesOUP",value=tFrom)
            updateNumericInput(session,"tToAPTPercentilesOUP",value=tTo)
            updateNumericInput(session,"tByAPTPercentilesOUP",value=tBy)
            updateNumericInput(session,"zFromAPTPercentilesOUP",value=zFrom)
            updateNumericInput(session,"zToAPTPercentilesOUP",value=zTo)
            updateNumericInput(session,"zByAPTPercentilesOUP",value=zBy)
            updateNumericInput(session,"kAPTPercentilesOUP",value=k)
            updateNumericInput(session,"sAPTPercentilesOUP",value=s)
            updateNumericInput(session,"xAPTPercentilesOUP",value=x)
            updateNumericInput(session,"omegaAPTPercentilesOUP",value=omega)
            updateNumericInput(session,"PpctAPTPercentilesOUP",value=Ppct)
            updateNumericInput(session,"ptmaxAPTPercentilesOUP",value=ptmax)
          }
          FromUItoR6 <- function()
          {
            # Get from UI ----
            rho <- input$rhoAPTPercentilesOUP
            if(!is.numeric(rho)) { rho <- 0 }
            else if(rho < 0) { rho <- 0 }
            mu <- input$muAPTPercentilesOUP
            if(!is.numeric(mu)) { mu <- 0 }
            sigma <- input$sigmaAPTPercentilesOUP
            if(!is.numeric(sigma)) { sigma <- 0 }
            tfrom <- input$tFromAPTPercentilesOUP
            if(!is.numeric(tfrom)) { tfrom <- 0 }
            tto <- input$tToAPTPercentilesOUP
            if(!is.numeric(tto)) { tto <- 0 }
            tby <- input$tByAPTPercentilesOUP
            if(!is.numeric(tby)) { tby <- 0 }
            if(abs(tto-tfrom) < abs(tby)) { tby <- tto-tfrom }
            if(abs(tto-tfrom)/100 > abs(tby)) { tby <- (tto-tfrom)/100 }
            if(tto > tfrom) { t <- seq(from=tfrom,to=tto,by=abs(tby)) }
            else if(tto == tfrom) { t <- tfrom }
            else { t <- seq(from=tto,to=tfrom,by=abs(tby)) }
            zFrom <- input$zFromAPTPercentilesOUP
            if(!is.numeric(zFrom)) { zFrom <- 0 }
            zTo <- input$zToAPTPercentilesOUP
            if(!is.numeric(zTo)) { zTo <- 0 }
            zBy <- input$zByAPTPercentilesOUP
            if(!is.numeric(zBy)) { zBy <- 0 }
            if(abs(zTo-zFrom) < abs(zBy)) { zBy <- zTo-zFrom }
            if(abs(zTo-zFrom)/100 > abs(zBy)) { zBy <- (zTo-zFrom)/100 }
            if(zTo > zFrom) { z <- seq(from=zFrom,to=zTo,by=abs(zBy)) }
            else if(zTo == zFrom) { z <- zFrom }
            else { z <- seq(from=zTo,to=zFrom,by=abs(zBy)) }
            k <- input$kAPTPercentilesOUP
            if(!is.numeric(k)) { k <- 0 }
            s <- input$sAPTPercentilesOUP
            if(!is.numeric(s)) { s <- t[1] }
            else if(s > t[1]) { s <- t[1] }
            x <- input$xAPTPercentilesOUP
            if(!is.numeric(x)) { x <- -mu }
            omega <- input$omegaAPTPercentilesOUP
            if(!is.numeric(omega)) { omega <- 1 }
            else if(omega < 0) { omega <- 0 }
            else if(omega > 1) { omega <- 1 }
            Ppct <- input$PpctAPTPercentilesOUP
            if(!is.numeric(Ppct)) { Ppct <- 0.841345 }
            else if(Ppct < 0.01) { Ppct <- 0.01 }
            else if(Ppct > 0.99) { Ppct <- 0.99 }
            ptmax <- input$ptmaxAPTPercentilesOUP
            if(!is.numeric(ptmax)) { ptmax <- NaN }
            # Set to OUP ----
            A$set_oup_params(rho=rho,mu=mu,sigma=sigma)
            A$set_t_stoch_args(t=t,k=k,s=s,x=x,z=z,omega=omega,Ppct=Ppct)
            A$set_plot_info(ptmax=ptmax)
          }
          # Initialize ----
          FromR6toUI()
          # Observe undo, sync, axes, plot (or enter key), left and rght ----
          undoButton <- reactiveVal(FALSE)
          syncButton <- reactiveVal(FALSE)
          axesButton <- reactiveVal(FALSE)
          plotButton <- reactiveVal(FALSE)
          leftButton <- reactiveVal(FALSE)
          rghtButton <- reactiveVal(FALSE)
          observe({
            undoButton(TRUE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$undoAPTPercentilesOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(TRUE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$syncAPTPercentilesOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(TRUE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$axesAPTPercentilesOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(TRUE)
            leftButton(FALSE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$plotAPTPercentilesOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(TRUE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$leftAPTPercentilesOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(TRUE)
          }) %>% bindEvent(input$rghtAPTPercentilesOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # User clicks save ----
          observe({
            FromUItoR6()
            A$default_save()
          }) %>% bindEvent(input$saveAPTPercentilesOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # User clicks undo, sync, axes, plot (or enter key), left or rght ----
          output$plotlyAPTPercentilesOUP <- renderPlotly({
            if(undoButton()) { A$default_read() }
            else if(syncButton())
            {
              FromUItoR6()
              A$sync_zyxt_stoch()
            }
            else if(axesButton())
            {
              FromUItoR6()
              A$axes_t_stoch()
            }
            else if(plotButton()) { FromUItoR6() }
            else if(leftButton())
            {
              plot_info <- A$get_plot_info()
              type <- plot_info$plottype[[1]]
              if(type > 6) { type <- 6 }
              else if(type > 5) { type <- 5 }
              else if(type > 4) { type <- 4 }
              else if(type > 3) { type <- 3 }
              else if(type > 2) { type <- 2 }
              else if(type > 1) { type <- 1 }
              else { type <- 6 }
              A$set_plot_info(type=type)
              FromUItoR6()
            }
            else if(rghtButton())
            {
              plot_info <- A$get_plot_info()
              type <- plot_info$plottype[[1]]
              if(type < 1) { type <- 1 }
              else if(type < 2) { type <- 2 }
              else if(type < 3) { type <- 3 }
              else if(type < 4) { type <- 4 }
              else if(type < 5) { type <- 5 }
              else if(type < 6) { type <- 6 }
              else { type <- 1 }
              A$set_plot_info(type=type)
              FromUItoR6()
            }
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(FALSE)
            FromR6toUI()
            A$PlotPassageTimePercentiles()
          }) %>% bindEvent(input$undoAPTPercentilesOUP,input$syncAPTPercentilesOUP,input$axesAPTPercentilesOUP,input$plotAPTPercentilesOUP,input$leftAPTPercentilesOUP,input$rghtAPTPercentilesOUP)
          # User clicks info ----
          observe({
            showModal(modalDialog(
              title=div(img(src="Roar32x32.png"),"Passage Time Percentiles"),
              HTML("The Transition Densities and Probabilites for the Ornstein-Uhlenbeck Process are symmetric and easy to interpret.  The only measure of central tendency is the Mean and the only measure of dispersion is the Variance.  Adding and subtracting the square-root of the Variance gives Percentiles above and below the Mean.<span hidden>Transition Density Transition Probability</span>  Passage Time Densities and Probabilities are not symmetric.  There are three measures of central tendency, the Mode, Median and Mean.  Adding and subtracting the square-root of the Variance gives weird results.  If a stochastic process does not converge, its Passage Time Mean and Variance do not exist.<span hidden>Passage Time Density Passage Time Probability</span>  An easier alternative is to calculate Percentiles.  The Median is the time with a 50% chance the threshold has been crossed and a 50% chance it is yet to be crossed.  Higher and lower Percentiles have similar interpretations.  Percentiles for Passage Time Probabilities of 0.841345 and 0.158655 measure the same dispersion as adding and subtracting the square-root of the Variance to the Mean of the Ornstein-Uhlenbeck Process.<br><br>
              &emsp;&emsp;The R6 method:<br>
              &emsp;&emsp;&emsp;PassageTimePercentile(<i>k,s,x,z,omega,rho,mu,sigma,Ppct</i>)<br>
              &emsp;&emsp;with arguments:<br>
              &emsp;&emsp;&emsp;<i>k</i> is the threshold;<br>
              &emsp;&emsp;&emsp;<i>s</i> is the fixed initial time;<br>
              &emsp;&emsp;&emsp;<i>x</i> is the fixed initial state;<br>
              &emsp;&emsp;&emsp;<i>z</i> are alternate initial states;<br>
              &emsp;&emsp;&emsp;<i>omega</i> is the degree of irreversibility;<br>
              &emsp;&emsp;&emsp;<i>rho</i> is the rate parameter;<br>
              &emsp;&emsp;&emsp;<i>mu</i> is the location parameter;<br>
              &emsp;&emsp;&emsp;<i>sigma</i> is the scale parameter;<br>
              &emsp;&emsp;&emsp;<i>Ppct</i> is a passage time probability;<br>
              &emsp;&emsp;returns:<br>
              <table style='float: left; margin-left: 60px;'>
                <tr>
                  <td style='border-top: solid silver; border-bottom: solid silver; border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'>percentile(<i>x</i>)</td>
                  <td style='border-top: solid silver; border-bottom: solid silver; border-right: solid silver'>&nbsp;</td>
                </tr>
              </table>
              <table style='float: left; margin-left: 10px; margin-right: 10px;'>
                <tr>
                  <td>and</td>
                </tr>
              </table>
              <table>
                <tr>
                  <td style='border-top: solid silver; border-bottom: solid silver; border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'>percentile(<i>z</i><sub>1</sub>)</td>
                  <td style='padding: 0px 4px 0px 4px;'>&hellip;</td>
                  <td style='padding: 0px 4px 0px 4px;'>percentile(<i>z</i><sub>n</sub>)</td>
                  <td style='border-top: solid silver; border-bottom: solid silver; border-right: solid silver'>&nbsp;</td>
                  <td style='padding-left: 2px;'>;</td>
                </tr>
              </table>
              &emsp;&emsp;where:<br>
              &emsp;&emsp;&emsp;percentile(<i>x</i>) is the Passage Time Percentile for Ppct at <i>x</i>;<br>
              &emsp;&emsp;&emsp;percentile(<i>z</i><sub>j</sub>) are the Passage Time Percentiles for Ppct at <i>x=z</i><sub>j</sub>;<br>"),
              easyClose = TRUE,
              footer = modalButton("Close"),
              size = "l"
            ))
          }) %>% bindEvent(input$infoAPTPercentilesOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
        # Passage Time Density ----
        else if(input$navAOUP == "APTDensityOUP")
        {
          # Define set/get functions ----
          FromR6toUI <- function()
          {
            # Get from OUP ----
            oup_params <- A$get_oup_params()
            t_stoch_args <- A$get_t_stoch_args()
            plot_info <- A$get_plot_info()
            rho <- oup_params[[1]]
            mu <- oup_params[[2]]
            sigma <- oup_params[[3]]
            t <- t_stoch_args[[1]]
            k <- t_stoch_args[[2]]
            s <- t_stoch_args[[3]]
            x <- t_stoch_args[[4]]
            z <- t_stoch_args[[5]]
            omega <- t_stoch_args[[6]]
            ptmax <- plot_info$plottype[[3]]
            m <- length(t)
            n <- length(z)
            tFrom <- t[1]
            tTo <- t[m]
            if(m > 1) { tBy <- (tTo-tFrom)/(m-1) }
            else  {tBy <- 0 }
            zFrom <- z[1]
            zTo <- z[n]
            if(n > 1) { zBy <- (zTo-zFrom)/(n-1) }
            else  {zBy <- 0 }
            # Set to UI ----
            updateNumericInput(session,"rhoAPTDensityOUP",value=rho)
            updateNumericInput(session,"muAPTDensityOUP",value=mu)
            updateNumericInput(session,"sigmaAPTDensityOUP",value=sigma)
            updateNumericInput(session,"tFromAPTDensityOUP",value=tFrom)
            updateNumericInput(session,"tToAPTDensityOUP",value=tTo)
            updateNumericInput(session,"tByAPTDensityOUP",value=tBy)
            updateNumericInput(session,"zFromAPTDensityOUP",value=zFrom)
            updateNumericInput(session,"zToAPTDensityOUP",value=zTo)
            updateNumericInput(session,"zByAPTDensityOUP",value=zBy)
            updateNumericInput(session,"kAPTDensityOUP",value=k)
            updateNumericInput(session,"sAPTDensityOUP",value=s)
            updateNumericInput(session,"xAPTDensityOUP",value=x)
            updateNumericInput(session,"omegaAPTDensityOUP",value=omega)
            updateNumericInput(session,"ptmaxAPTDensityOUP",value=ptmax)
          }
          FromUItoR6 <- function()
          {
            # Get from UI ----
            rho <- input$rhoAPTDensityOUP
            if(!is.numeric(rho)) { rho <- 0 }
            else if(rho < 0) { rho <- 0 }
            mu <- input$muAPTDensityOUP
            if(!is.numeric(mu)) { mu <- 0 }
            sigma <- input$sigmaAPTDensityOUP
            if(!is.numeric(sigma)) { sigma <- 0 }
            tfrom <- input$tFromAPTDensityOUP
            if(!is.numeric(tfrom)) { tfrom <- 0 }
            tto <- input$tToAPTDensityOUP
            if(!is.numeric(tto)) { tto <- 0 }
            tby <- input$tByAPTDensityOUP
            if(!is.numeric(tby)) { tby <- 0 }
            if(abs(tto-tfrom) < abs(tby)) { tby <- tto-tfrom }
            if(abs(tto-tfrom)/100 > abs(tby)) { tby <- (tto-tfrom)/100 }
            if(tto > tfrom) { t <- seq(from=tfrom,to=tto,by=abs(tby)) }
            else if(tto == tfrom) { t <- tfrom }
            else { t <- seq(from=tto,to=tfrom,by=abs(tby)) }
            zFrom <- input$zFromAPTDensityOUP
            if(!is.numeric(zFrom)) { zFrom <- 0 }
            zTo <- input$zToAPTDensityOUP
            if(!is.numeric(zTo)) { zTo <- 0 }
            zBy <- input$zByAPTDensityOUP
            if(!is.numeric(zBy)) { zBy <- 0 }
            if(abs(zTo-zFrom) < abs(zBy)) { zBy <- zTo-zFrom }
            if(abs(zTo-zFrom)/100 > abs(zBy)) { zBy <- (zTo-zFrom)/100 }
            if(zTo > zFrom) { z <- seq(from=zFrom,to=zTo,by=abs(zBy)) }
            else if(zTo == zFrom) { z <- zFrom }
            else { z <- seq(from=zTo,to=zFrom,by=abs(zBy)) }
            k <- input$kAPTDensityOUP
            if(!is.numeric(k)) { k <- 0 }
            s <- input$sAPTDensityOUP
            if(!is.numeric(s)) { s <- t[1] }
            else if(s > t[1]) { s <- t[1] }
            x <- input$xAPTDensityOUP
            if(!is.numeric(x)) { x <- -mu }
            omega <- input$omegaAPTDensityOUP
            if(!is.numeric(omega)) { omega <- 1 }
            else if(omega < 0) { omega <- 0 }
            else if(omega > 1) { omega <- 1 }
            ptmax <- input$ptmaxAPTDensityOUP
            if(!is.numeric(ptmax)) { ptmax <- NaN }
            # Set to OUP ----
            A$set_oup_params(rho=rho,mu=mu,sigma=sigma)
            A$set_t_stoch_args(t=t,k=k,s=s,x=x,z=z,omega=omega)
            A$set_plot_info(ptmax=ptmax)
          }
          # Initialize ----
          FromR6toUI()
          # Observe undo, sync, axes, plot (or enter key), left and rght ----
          undoButton <- reactiveVal(FALSE)
          syncButton <- reactiveVal(FALSE)
          axesButton <- reactiveVal(FALSE)
          plotButton <- reactiveVal(FALSE)
          leftButton <- reactiveVal(FALSE)
          rghtButton <- reactiveVal(FALSE)
          observe({
            undoButton(TRUE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$undoAPTDensityOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(TRUE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$syncAPTDensityOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(TRUE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$axesAPTDensityOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(TRUE)
            leftButton(FALSE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$plotAPTDensityOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(TRUE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$leftAPTDensityOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(TRUE)
          }) %>% bindEvent(input$rghtAPTDensityOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # User clicks save ----
          observe({
            FromUItoR6()
            A$default_save()
          }) %>% bindEvent(input$saveAPTDensityOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # User clicks undo, sync, axes, plot (or enter key), left or rght ----
          output$plotlyAPTDensityOUP <- renderPlotly({
            if(undoButton()) { A$default_read() }
            else if(syncButton())
            {
              FromUItoR6()
              A$sync_zyxt_stoch()
            }
            else if(axesButton())
            {
              FromUItoR6()
              A$axes_t_stoch()
            }
            else if(plotButton()) { FromUItoR6() }
            else if(leftButton())
            {
              plot_info <- A$get_plot_info()
              type <- plot_info$plottype[[1]]
              if(type > 5) { type <- 5 }
              else if(type > 4) { type <- 4 }
              else if(type > 3) { type <- 3 }
              else if(type > 2) { type <- 2 }
              else if(type > 1) { type <- 1 }
              else { type <- 5 }
              A$set_plot_info(type=type)
              FromUItoR6()
            }
            else if(rghtButton())
            {
              plot_info <- A$get_plot_info()
              type <- plot_info$plottype[[1]]
              if(type < 1) { type <- 1 }
              else if(type < 2) { type <- 2 }
              else if(type < 3) { type <- 3 }
              else if(type < 4) { type <- 4 }
              else if(type < 5) { type <- 5 }
              else { type <- 1 }
              A$set_plot_info(type=type)
              FromUItoR6()
            }
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(FALSE)
            FromR6toUI()
            A$PlotPassageTimeDensity()
          }) %>% bindEvent(input$undoAPTDensityOUP,input$syncAPTDensityOUP,input$axesAPTDensityOUP,input$plotAPTDensityOUP,input$leftAPTDensityOUP,input$rghtAPTDensityOUP)
          # User clicks info ----
          observe({
            showModal(modalDialog(
              title=div(img(src="Roar32x32.png"),"Passage Time Density"),
              HTML("An additional proportion of time an Ornstein-Uhlenbeck Process spends on the far side of a threshold is the Passage Time Density.  If crossing a threshold is irreversible, it is the First Passage Time Density.  If crossing a threshold can be completely reversed, it is the Visiting Time Density.  In between is the Passage Time Density.  A Passage Time Density is typically skewed, but can also be bi-modal and even negative if the process is attracted away from a threshold.<br><br>
              &emsp;&emsp;The R6 method:<br>
              &emsp;&emsp;&emsp;PassageTimeDensity(<i>t,k,s,x,z,omega,rho,mu,sigma</i>)<br>
              &emsp;&emsp;with arguments:<br>
              &emsp;&emsp;&emsp;<i>t</i> are stochastic times;<br>
              &emsp;&emsp;&emsp;<i>k</i> is the threshold;<br>
              &emsp;&emsp;&emsp;<i>s</i> is the fixed initial time;<br>
              &emsp;&emsp;&emsp;<i>x</i> is the fixed initial state;<br>
              &emsp;&emsp;&emsp;<i>z</i> are alternate initial states;<br>
              &emsp;&emsp;&emsp;<i>omega</i> is the degree of irreversibility;<br>
              &emsp;&emsp;&emsp;<i>rho</i> is the rate parameter;<br>
              &emsp;&emsp;&emsp;<i>mu</i> is the location parameter;<br>
              &emsp;&emsp;&emsp;<i>sigma</i> is the scale parameter;<br>
              &emsp;&emsp;returns:<br>
              <table style='float: left; margin-left: 60px;'>
                <tr>
                  <td style='border-top: solid silver; border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'><i>p<sub>t</sub></i>(<i>t</i><sub>1</sub>|<i>x</i>)</td>
                  <td style='border-top: solid silver; border-right: solid silver'>&nbsp;</td>
                </tr>
                <tr>
                  <td style='border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'>&emsp;&vellip;</td>
                  <td style='border-right: solid silver'>&nbsp;</td>
                </tr>
                <tr>
                  <td style='border-bottom: solid silver; border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'><i>p<sub>t</sub></i>(<i>t</i><sub>m</sub>|<i>x</i>)</td>
                  <td style='border-bottom: solid silver; border-right: solid silver'>&nbsp;</td>
                </tr>
              </table>
              <table style='float: left; margin-left: 10px; margin-right: 10px;'>
                <tr>
                  <td>&nbsp;</td>
                </tr>
                <tr>
                  <td>&nbsp;</td>
                </tr>
                <tr>
                  <td>and</td>
                </tr>
              </table>
              <table>
                <tr>
                  <td style='border-top: solid silver; border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'><i>p<sub>t</sub></i>(<i>t</i><sub>1</sub>|<i>z</i><sub>1</sub>)</td>
                  <td style='padding: 0px 4px 0px 4px;'>&hellip;</td>
                  <td style='padding: 0px 4px 0px 4px;'><i>p<sub>t</sub></i>(<i>t</i><sub>1</sub>|<i>z</i><sub>n</sub>)</td>
                  <td style='border-top: solid silver; border-right: solid silver'>&nbsp;</td>
                </tr>
                <tr>
                  <td style='border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'>&emsp;&vellip;</td>
                  <td style='padding: 0px 4px 0px 4px;'>&dtdot;</td>
                  <td style='padding: 0px 4px 0px 4px;'>&emsp;&vellip;</td>
                  <td style='border-right: solid silver'>&nbsp;</td>
                </tr>
                <tr>
                  <td style='border-bottom: solid silver; border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'><i>p<sub>t</sub></i>(<i>t</i><sub>m</sub>|<i>z</i><sub>1</sub>)</td>
                  <td style='padding: 0px 4px 0px 4px;'>&hellip;</td>
                  <td style='padding: 0px 4px 0px 4px;'><i>p<sub>t</sub></i>(<i>t</i><sub>m</sub>|<i>z</i><sub>n</sub>)</td>
                  <td style='border-bottom: solid silver; border-right: solid silver'>&nbsp;</td>
                  <td style='padding-left: 2px;'>;</td>
                </tr>
              </table>
              &emsp;&emsp;where:<br>
              &emsp;&emsp;&emsp;<i>p<sub>t</sub></i>(<i>t</i>|<i>x</i>) is the Passage Time Density at <i>x</i>;<br>
              &emsp;&emsp;&emsp;<i>p<sub>t</sub></i>(<i>t</i>|<i>z</i><sub>j</sub>) is the Passage Time Density for <i>x=z</i><sub>j</sub>."),
              easyClose = TRUE,
              footer = modalButton("Close")
            ))
          }) %>% bindEvent(input$infoAPTDensityOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
        # Passage Time Probability ----
        else if(input$navAOUP == "APTProbabilityOUP")
        {
          # Define set/get functions ----
          FromR6toUI <- function()
          {
            # Get from OUP ----
            oup_params <- A$get_oup_params()
            t_stoch_args <- A$get_t_stoch_args()
            rho <- oup_params[[1]]
            mu <- oup_params[[2]]
            sigma <- oup_params[[3]]
            t <- t_stoch_args[[1]]
            k <- t_stoch_args[[2]]
            s <- t_stoch_args[[3]]
            x <- t_stoch_args[[4]]
            z <- t_stoch_args[[5]]
            omega <- t_stoch_args[[6]]
            m <- length(t)
            n <- length(z)
            tFrom <- t[1]
            tTo <- t[m]
            if(m > 1) { tBy <- (tTo-tFrom)/(m-1) }
            else  {tBy <- 0 }
            zFrom <- z[1]
            zTo <- z[n]
            if(n > 1) { zBy <- (zTo-zFrom)/(n-1) }
            else  {zBy <- 0 }
            # Set to UI ----
            updateNumericInput(session,"rhoAPTProbabilityOUP",value=rho)
            updateNumericInput(session,"muAPTProbabilityOUP",value=mu)
            updateNumericInput(session,"sigmaAPTProbabilityOUP",value=sigma)
            updateNumericInput(session,"tFromAPTProbabilityOUP",value=tFrom)
            updateNumericInput(session,"tToAPTProbabilityOUP",value=tTo)
            updateNumericInput(session,"tByAPTProbabilityOUP",value=tBy)
            updateNumericInput(session,"zFromAPTProbabilityOUP",value=zFrom)
            updateNumericInput(session,"zToAPTProbabilityOUP",value=zTo)
            updateNumericInput(session,"zByAPTProbabilityOUP",value=zBy)
            updateNumericInput(session,"kAPTProbabilityOUP",value=k)
            updateNumericInput(session,"sAPTProbabilityOUP",value=s)
            updateNumericInput(session,"xAPTProbabilityOUP",value=x)
            updateNumericInput(session,"omegaAPTProbabilityOUP",value=omega)
          }
          FromUItoR6 <- function()
          {
            # Get from UI ----
            rho <- input$rhoAPTProbabilityOUP
            if(!is.numeric(rho)) { rho <- 0 }
            else if(rho < 0) { rho <- 0 }
            mu <- input$muAPTProbabilityOUP
            if(!is.numeric(mu)) { mu <- 0 }
            sigma <- input$sigmaAPTProbabilityOUP
            if(!is.numeric(sigma)) { sigma <- 0 }
            tfrom <- input$tFromAPTProbabilityOUP
            if(!is.numeric(tfrom)) { tfrom <- 0 }
            tto <- input$tToAPTProbabilityOUP
            if(!is.numeric(tto)) { tto <- 0 }
            tby <- input$tByAPTProbabilityOUP
            if(!is.numeric(tby)) { tby <- 0 }
            if(abs(tto-tfrom) < abs(tby)) { tby <- tto-tfrom }
            if(abs(tto-tfrom)/100 > abs(tby)) { tby <- (tto-tfrom)/100 }
            if(tto > tfrom) { t <- seq(from=tfrom,to=tto,by=abs(tby)) }
            else if(tto == tfrom) { t <- tfrom }
            else { t <- seq(from=tto,to=tfrom,by=abs(tby)) }
            zFrom <- input$zFromAPTProbabilityOUP
            if(!is.numeric(zFrom)) { zFrom <- 0 }
            zTo <- input$zToAPTProbabilityOUP
            if(!is.numeric(zTo)) { zTo <- 0 }
            zBy <- input$zByAPTProbabilityOUP
            if(!is.numeric(zBy)) { zBy <- 0 }
            if(abs(zTo-zFrom) < abs(zBy)) { zBy <- zTo-zFrom }
            if(abs(zTo-zFrom)/100 > abs(zBy)) { zBy <- (zTo-zFrom)/100 }
            if(zTo > zFrom) { z <- seq(from=zFrom,to=zTo,by=abs(zBy)) }
            else if(zTo == zFrom) { z <- zFrom }
            else { z <- seq(from=zTo,to=zFrom,by=abs(zBy)) }
            k <- input$kAPTProbabilityOUP
            if(!is.numeric(k)) { k <- 0 }
            s <- input$sAPTProbabilityOUP
            if(!is.numeric(s)) { s <- t[1] }
            else if(s > t[1]) { s <- t[1] }
            x <- input$xAPTProbabilityOUP
            if(!is.numeric(x)) { x <- -mu }
            omega <- input$omegaAPTProbabilityOUP
            if(!is.numeric(omega)) { omega <- 1 }
            else if(omega < 0) { omega <- 0 }
            else if(omega > 1) { omega <- 1 }
            # Set to OUP ----
            A$set_oup_params(rho=rho,mu=mu,sigma=sigma)
            A$set_t_stoch_args(t=t,k=k,s=s,x=x,z=z,omega=omega)
          }
          # Initialize ----
          FromR6toUI()
          # Observe undo, sync, axes, plot (or enter key), left and rght ----
          undoButton <- reactiveVal(FALSE)
          syncButton <- reactiveVal(FALSE)
          axesButton <- reactiveVal(FALSE)
          plotButton <- reactiveVal(FALSE)
          leftButton <- reactiveVal(FALSE)
          rghtButton <- reactiveVal(FALSE)
          observe({
            undoButton(TRUE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$undoAPTProbabilityOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(TRUE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$syncAPTProbabilityOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(TRUE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$axesAPTProbabilityOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(TRUE)
            leftButton(FALSE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$plotAPTProbabilityOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(TRUE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$leftAPTProbabilityOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(TRUE)
          }) %>% bindEvent(input$rghtAPTProbabilityOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # User clicks save ----
          observe({
            FromUItoR6()
            A$default_save()
          }) %>% bindEvent(input$saveAPTProbabilityOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # User clicks undo, sync, axes, plot (or enter key), left or rght ----
          output$plotlyAPTProbabilityOUP <- renderPlotly({
            if(undoButton()) { A$default_read() }
            else if(syncButton())
            {
              FromUItoR6()
              A$sync_zyxt_stoch()
            }
            else if(axesButton())
            {
              FromUItoR6()
              A$axes_t_stoch()
            }
            else if(plotButton()) { FromUItoR6() }
            else if(leftButton())
            {
              plot_info <- A$get_plot_info()
              type <- plot_info$plottype[[1]]
              if(type > 5) { type <- 5 }
              else if(type > 4) { type <- 4 }
              else if(type > 3) { type <- 3 }
              else if(type > 2) { type <- 2 }
              else if(type > 1) { type <- 1 }
              else { type <- 5 }
              A$set_plot_info(type=type)
              FromUItoR6()
            }
            else if(rghtButton())
            {
              plot_info <- A$get_plot_info()
              type <- plot_info$plottype[[1]]
              if(type < 1) { type <- 1 }
              else if(type < 2) { type <- 2 }
              else if(type < 3) { type <- 3 }
              else if(type < 4) { type <- 4 }
              else if(type < 5) { type <- 5 }
              else { type <- 1 }
              A$set_plot_info(type=type)
              FromUItoR6()
            }
            undoButton(FALSE)
            syncButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(FALSE)
            FromR6toUI()
            A$PlotPassageTimeProbability()
          }) %>% bindEvent(input$undoAPTProbabilityOUP,input$syncAPTProbabilityOUP,input$axesAPTProbabilityOUP,input$plotAPTProbabilityOUP,input$leftAPTProbabilityOUP,input$rghtAPTProbabilityOUP)
          # User clicks info ----
          observe({
            showModal(modalDialog(
              title=div(img(src="Roar32x32.png"),"Passage Time Probability"),
              HTML("The proportion of time an Ornstein-Uhlenbeck Process spends on the far side of a threshold is the Passage Time Probability.  At one extreme is the First Passage Time Probability and at the other is the Visiting Time Probability.  The First Passage Time Probability goes to one because the Ornstein-Uhlenbeck Process will eventually cross the threshold and be trapped on the far side.  A Passage Time Probability does not go to one because the process may return to spend time on the near side.<br><br>
              &emsp;&emsp;The R6 method:<br>
              &emsp;&emsp;&emsp;PassageTimeProbability(<i>t,k,s,x,z,omega,rho,mu,sigma</i>)<br>
              &emsp;&emsp;with arguments:<br>
              &emsp;&emsp;&emsp;<i>t</i> are stochastic times;<br>
              &emsp;&emsp;&emsp;<i>k</i> is the threshold;<br>
              &emsp;&emsp;&emsp;<i>s</i> is the fixed initial time;<br>
              &emsp;&emsp;&emsp;<i>x</i> is the fixed initial state;<br>
              &emsp;&emsp;&emsp;<i>z</i> are alternate initial states;<br>
              &emsp;&emsp;&emsp;<i>omega</i> is the degree of irreversibility;<br>
              &emsp;&emsp;&emsp;<i>rho</i> is the rate parameter;<br>
              &emsp;&emsp;&emsp;<i>mu</i> is the location parameter;<br>
              &emsp;&emsp;&emsp;<i>sigma</i> is the scale parameter;<br>
              &emsp;&emsp;returns:<br>
              <table style='float: left; margin-left: 60px;'>
                <tr>
                  <td style='border-top: solid silver; border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'><i>P<sub>t</sub></i>(<i>t</i><sub>1</sub>|<i>x</i>)</td>
                  <td style='border-top: solid silver; border-right: solid silver'>&nbsp;</td>
                </tr>
                <tr>
                  <td style='border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'>&emsp;&vellip;</td>
                  <td style='border-right: solid silver'>&nbsp;</td>
                </tr>
                <tr>
                  <td style='border-bottom: solid silver; border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'><i>P<sub>t</sub></i>(<i>t</i><sub>m</sub>|<i>x</i>)</td>
                  <td style='border-bottom: solid silver; border-right: solid silver'>&nbsp;</td>
                </tr>
              </table>
              <table style='float: left; margin-left: 10px; margin-right: 10px;'>
                <tr>
                  <td>&nbsp;</td>
                </tr>
                <tr>
                  <td>&nbsp;</td>
                </tr>
                <tr>
                  <td>and</td>
                </tr>
              </table>
              <table>
                <tr>
                  <td style='border-top: solid silver; border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'><i>P<sub>t</sub></i>(<i>t</i><sub>1</sub>|<i>z</i><sub>1</sub>)</td>
                  <td style='padding: 0px 4px 0px 4px;'>&hellip;</td>
                  <td style='padding: 0px 4px 0px 4px;'><i>P<sub>t</sub></i>(<i>t</i><sub>1</sub>|<i>z</i><sub>n</sub>)</td>
                  <td style='border-top: solid silver; border-right: solid silver'>&nbsp;</td>
                </tr>
                <tr>
                  <td style='border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'>&emsp;&vellip;</td>
                  <td style='padding: 0px 4px 0px 4px;'>&dtdot;</td>
                  <td style='padding: 0px 4px 0px 4px;'>&emsp;&vellip;</td>
                  <td style='border-right: solid silver'>&nbsp;</td>
                </tr>
                <tr>
                  <td style='border-bottom: solid silver; border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'><i>P<sub>t</sub></i>(<i>t</i><sub>m</sub>|<i>z</i><sub>1</sub>)</td>
                  <td style='padding: 0px 4px 0px 4px;'>&hellip;</td>
                  <td style='padding: 0px 4px 0px 4px;'><i>P<sub>t</sub></i>(<i>t</i><sub>m</sub>|<i>z</i><sub>n</sub>)</td>
                  <td style='border-bottom: solid silver; border-right: solid silver'>&nbsp;</td>
                  <td style='padding-left: 2px;'>;</td>
                </tr>
              </table>
              &emsp;&emsp;where:<br>
              &emsp;&emsp;&emsp;<i>P<sub>t</sub></i>(<i>t</i>|<i>x</i>) is the Passage Time Probability at <i>x</i>;<br>
              &emsp;&emsp;&emsp;<i>P<sub>t</sub></i>(<i>t</i>|<i>z</i><sub>j</sub>) is the Passage Time Probability for <i>x=z</i><sub>j</sub>."),
              easyClose = TRUE,
              footer = modalButton("Close")
            ))
          }) %>% bindEvent(input$infoAPTProbabilityOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
      })
    }
    else if(input$navBar == "tabFDOUP")
    {
      observeEvent(input$navFDOUP,{
        # Drift ----
        if(input$navFDOUP == "FDDriftOUP")
        {
          # Define set/get functions ----
          FromR6toUI <- function()
          {
            # Get from OUP ----
            oup_params <- FD$get_oup_params()
            x_stoch_args <- FD$get_x_stoch_args()
            rho <- oup_params[[1]]
            mu <- oup_params[[2]]
            x <- x_stoch_args[[2]]
            n <- length(x)
            xFrom <- x[1]
            xTo <- x[n]
            if(n > 1) { xBy <- (xTo-xFrom)/(n-1) }
            else  {xBy <- 0 }
            # Set to UI ----
            updateNumericInput(session,"rhoFDDriftOUP",value=rho)
            updateNumericInput(session,"muFDDriftOUP",value=mu)
            updateNumericInput(session,"xFromFDDriftOUP",value=xFrom)
            updateNumericInput(session,"xToFDDriftOUP",value=xTo)
            updateNumericInput(session,"xByFDDriftOUP",value=xBy)
          }
          FromUItoR6 <- function()
          {
            # Get from UI ----
            rho <- input$rhoFDDriftOUP
            if(!is.numeric(rho)) { rho <- 0 }
            else if(rho < 0) { rho <- 0 }
            mu <- input$muFDDriftOUP
            if(!is.numeric(mu)) { mu <- 0 }
            xFrom <- input$xFromFDDriftOUP
            xTo <- input$xToFDDriftOUP
            xBy <- input$xByFDDriftOUP
            xOK <- FALSE
            if(is.numeric(xFrom) & is.numeric(xTo))
            {
              if(xTo > xFrom)
              {
                if(is.numeric(xBy))
                {
                  if(xBy > (xTo-xFrom)/100) { xBy <- (xTo-xFrom)/100 }
                  else if(xBy < (xTo-xFrom)/1000) { xBy <- (xTo-xFrom)/1000 }
                }
                else { xBy <- (xTo-xFrom)/100 }
                xOK <- TRUE
              }
            }
            else if(is.numeric(xFrom) & is.numeric(xBy))
            {
              if(xBy > 0)
              {
                xTo <- xFrom+100*xBy
                xOK <- TRUE
              }
            }
            else if(is.numeric(xTo) & is.numeric(xBy))
            {
              if(xBy > 0)
              {
                xFrom <- xTo-100*xBy
                xOK <- TRUE
              }
            }
            # Set to OUP ----
            FD$set_oup_params(rho=rho,mu=mu)
            if(xOK) { FD$set_x_stoch_args(x=seq(from=xFrom,to=xTo,by=xBy)) }
            else { FD$axes_x_stoch() }
          }
          # Initialize ----
          FromR6toUI()
          # Observe undo, axes and plot (or enter key) ----
          undoButton <- reactiveVal(FALSE)
          axesButton <- reactiveVal(FALSE)
          plotButton <- reactiveVal(FALSE)
          observe({
            undoButton(TRUE)
            axesButton(FALSE)
            plotButton(FALSE)
          }) %>% bindEvent(input$undoFDDriftOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            axesButton(TRUE)
            plotButton(FALSE)
          }) %>% bindEvent(input$axesFDDriftOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            axesButton(FALSE)
            plotButton(TRUE)
          }) %>% bindEvent(input$plotFDDriftOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # User clicks save ----
          observe({
            FromUItoR6()
            FD$default_save()
          }) %>% bindEvent(input$saveFDDriftOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # User clicks undo, axes or plot (or enter key) ----
          output$plotlyFDDriftOUP <- renderPlotly({
            if(undoButton()) { FD$default_read() }
            else if(axesButton())
            {
              FromUItoR6()
              FD$axes_x_stoch()
            }
            else if(plotButton()) { FromUItoR6() }
            undoButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            FromR6toUI()
            FD$PlotDrift()
          }) %>% bindEvent(input$undoFDDriftOUP,input$axesFDDriftOUP,input$plotFDDriftOUP)
          # User clicks info ----
          observe({
            showModal(modalDialog(
              title=div(img(src="Roar32x32.png"),"Drift"),
              HTML("Drift is the expected change in the state of a stochastic process over a brief instant.  It is a component of the partial differential equation solved by the Finite Difference Method.<br><br>
              &emsp;&emsp;The R6 method:<br>
              &emsp;&emsp;&emsp;Drift(<i>x,rho,mu</i>)<br>
              &emsp;&emsp;with arguments:<br>
              &emsp;&emsp;&emsp;<i>x</i> are the stochastic states;<br>
              &emsp;&emsp;&emsp;<i>rho</i> is the rate parameter;<br>
              &emsp;&emsp;&emsp;<i>mu</i> is the location parameter;<br>
              &emsp;&emsp;returns:<br>
              <table style='margin-left: 60px;'>
                <tr>
                  <td style='border-top: solid silver; border-bottom: solid silver; border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'><i>g</i>(<i>x</i><sub>1</sub>)</td>
                  <td style='padding: 0px 4px 0px 4px;'>&hellip;</td>
                  <td style='padding: 0px 4px 0px 4px;'><i>g</i>(<i>x</i><sub>n</sub>)</td>
                  <td style='border-top: solid silver; border-bottom: solid silver; border-right: solid silver'>&nbsp;</td>
                  <td style='padding-left: 2px;'>;</td>
                </tr>
              </table>
              &emsp;&emsp;where:<br>
              &emsp;&emsp;&emsp;<i>g</i> is the Drift."),
              easyClose = TRUE,
              footer = modalButton("Close")
            ))
          }) %>% bindEvent(input$infoFDDriftOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
        # Diffusion ----
        if(input$navFDOUP == "FDDiffusionOUP")
        {
          # Define set/get functions ----
          FromR6toUI <- function()
          {
            # Get from OUP ----
            oup_params <- FD$get_oup_params()
            x_stoch_args <- FD$get_x_stoch_args()
            rho <- oup_params[[1]]
            mu <- oup_params[[2]]
            sigma <- oup_params[[3]]
            x <- x_stoch_args[[2]]
            n <- length(x)
            xFrom <- x[1]
            xTo <- x[n]
            if(n > 1) { xBy <- (xTo-xFrom)/(n-1) }
            else  {xBy <- 0 }
            # Set to UI ----
            updateNumericInput(session,"rhoFDDiffusionOUP",value=rho)
            updateNumericInput(session,"muFDDiffusionOUP",value=mu)
            updateNumericInput(session,"sigmaFDDiffusionOUP",value=sigma)
            updateNumericInput(session,"xFromFDDiffusionOUP",value=xFrom)
            updateNumericInput(session,"xToFDDiffusionOUP",value=xTo)
            updateNumericInput(session,"xByFDDiffusionOUP",value=xBy)
          }
          FromUItoR6 <- function()
          {
            # Get from UI ----
            rho <- input$rhoFDDiffusionOUP
            if(!is.numeric(rho)) { rho <- 0 }
            else if(rho < 0) { rho <- 0 }
            mu <- input$muFDDiffusionOUP
            if(!is.numeric(mu)) { mu <- 0 }
            sigma <- input$sigmaFDDiffusionOUP
            if(!is.numeric(sigma)) { sigma <- 0 }
            xFrom <- input$xFromFDDiffusionOUP
            xTo <- input$xToFDDiffusionOUP
            xBy <- input$xByFDDiffusionOUP
            xOK <- FALSE
            if(is.numeric(xFrom) & is.numeric(xTo))
            {
              if(xTo > xFrom)
              {
                if(is.numeric(xBy))
                {
                  if(xBy > (xTo-xFrom)/100) { xBy <- (xTo-xFrom)/100 }
                  else if(xBy < (xTo-xFrom)/1000) { xBy <- (xTo-xFrom)/1000 }
                }
                else { xBy <- (xTo-xFrom)/100 }
                xOK <- TRUE
              }
            }
            else if(is.numeric(xFrom) & is.numeric(xBy))
            {
              if(xBy > 0)
              {
                xTo <- xFrom+100*xBy
                xOK <- TRUE
              }
            }
            else if(is.numeric(xTo) & is.numeric(xBy))
            {
              if(xBy > 0)
              {
                xFrom <- xTo-100*xBy
                xOK <- TRUE
              }
            }
            # Set to OUP ----
            FD$set_oup_params(rho=rho,mu=mu,sigma=sigma)
            if(xOK) { FD$set_x_stoch_args(x=seq(from=xFrom,to=xTo,by=xBy)) }
            else { FD$axes_x_stoch() }
          }
          # Initialize ----
          FromR6toUI()
          # Observe undo, axes, plot (or enter key), left and rght ----
          undoButton <- reactiveVal(FALSE)
          axesButton <- reactiveVal(FALSE)
          plotButton <- reactiveVal(FALSE)
          leftButton <- reactiveVal(FALSE)
          rghtButton <- reactiveVal(FALSE)
          observe({
            undoButton(TRUE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$undoFDDiffusionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            axesButton(TRUE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$axesFDDiffusionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            axesButton(FALSE)
            plotButton(TRUE)
            leftButton(FALSE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$plotFDDiffusionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(TRUE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$leftFDDiffusionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(TRUE)
          }) %>% bindEvent(input$rghtFDDiffusionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # User clicks save ----
          observe({
            FromUItoR6()
            FD$default_save()
          }) %>% bindEvent(input$saveFDDiffusionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # User clicks undo, axes, plot (or enter key), left or rght ----
          output$plotlyFDDiffusionOUP <- renderPlotly({
            if(undoButton()) { FD$default_read() }
            else if(axesButton())
            {
              FromUItoR6()
              FD$axes_x_stoch()
            }
            else if(plotButton()) { FromUItoR6() }
            else if(leftButton())
            {
              plot_info <- A$get_plot_info()
              type <- plot_info$plottype[[1]]
              if(type > 3) { type <- 3 }
              else if(type > 2) { type <- 2 }
              else { type <- 3 }
              A$set_plot_info(type=type)
              FromUItoR6()
            }
            else if(rghtButton())
            {
              plot_info <- A$get_plot_info()
              type <- plot_info$plottype[[1]]
              if(type < 2) { type <- 2 }
              else if(type < 3) { type <- 3 }
              else { type <- 2 }
              A$set_plot_info(type=type)
              FromUItoR6()
            }
            undoButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(FALSE)
            FromR6toUI()
            FD$PlotDiffusion()
          }) %>% bindEvent(input$undoFDDiffusionOUP,input$axesFDDiffusionOUP,input$plotFDDiffusionOUP,input$leftFDDiffusionOUP,input$rghtFDDiffusionOUP)
          # User clicks info ----
          observe({
            showModal(modalDialog(
              title=div(img(src="Roar32x32.png"),"Diffusion"),
              HTML("An error is the difference between the actual and expected changes in the state of a stochastic process.  Diffusion is the error squared over a brief instant.  It is a component of the partial differential equation solved by the Finite Difference Method.<br><br>
              &emsp;&emsp;The R6 method:<br>
              &emsp;&emsp;&emsp;Diffusion(<i>sigma</i>)<br>
              &emsp;&emsp;with argument:<br>
              &emsp;&emsp;&emsp;<i>sigma</i> is the scale parameter;<br>
              &emsp;&emsp;returns:<br>
              <table style='margin-left: 60px;'>
                <tr>
                  <td style='border-top: solid silver; border-bottom: solid silver; border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'><i>h</i><sup>2</sup></td>
                  <td style='padding: 0px 4px 0px 4px;'>&hellip;</td>
                  <td style='padding: 0px 4px 0px 4px;'><i>h</i><sup>2</sup></td>
                  <td style='border-top: solid silver; border-bottom: solid silver; border-right: solid silver'>&nbsp;</td>
                  <td style='padding-left: 2px;'>;</td>
                </tr>
              </table>
              &emsp;&emsp;where:<br>
              &emsp;&emsp;&emsp;<i>h</i><sup>2</sup> is the Diffusion."),
              easyClose = TRUE,
              footer = modalButton("Close")
            ))
          }) %>% bindEvent(input$infoFDDiffusionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
        # Terminal Values ----
        if(input$navFDOUP == "FDTerminalOUP")
        {
          # Dynamic UI ----
          V_info <- FD$get_V_info()
          names <- V_info[[3]]
          output$VFDTerminalOUP <- renderUI({ selectInput("VFDTerminalOUP",label="V",choices=names) })
          # Define set/get functions ----
          FromR6toUI <- function()
          {
            # Get from OUP ----
            x_stoch_args <- FD$get_x_stoch_args()
            V_args <- FD$get_V_args()
            V_info <- FD$get_V_info()
            x <- x_stoch_args[[2]]
            n <- length(x)
            xFrom <- x[1]
            xTo <- x[n]
            if(n > 1) { xBy <- (xTo-xFrom)/(n-1) }
            else  {xBy <- 0 }
            Ix <- V_info[[1]]
            # Set to UI ----
            updateSelectInput(session,"VFDTerminalOUP",selected=names[Ix])
            updateNumericInput(session,"xFromFDTerminalOUP",value=xFrom)
            updateNumericInput(session,"xToFDTerminalOUP",value=xTo)
            updateNumericInput(session,"xByFDTerminalOUP",value=xBy)
            updateNumericInput(session,"V1FDTerminalOUP",label="~",value=NA)
            updateNumericInput(session,"V2FDTerminalOUP",label="~",value=NA)
            updateNumericInput(session,"V3FDTerminalOUP",label="~",value=NA)
            updateNumericInput(session,"V4FDTerminalOUP",label="~",value=NA)
            updateNumericInput(session,"V5FDTerminalOUP",label="~",value=NA)
            n <- length(V_args)
            i <- 0
            while(i < n)
            {
              i <- i+1
              argname <- names(V_args[i])
              arg <- V_args[[i]]
              if(i == 1) { updateNumericInput(session,"V1FDTerminalOUP",label=argname,value=arg) }
              else if(i == 2) { updateNumericInput(session,"V2FDTerminalOUP",label=argname,value=arg) }
              else if(i == 3) { updateNumericInput(session,"V3FDTerminalOUP",label=argname,value=arg) }
              else if(i == 4) { updateNumericInput(session,"V4FDTerminalOUP",label=argname,value=arg) }
              else if(i == 5) { updateNumericInput(session,"V5FDTerminalOUP",label=argname,value=arg) }
            }
            while(i < 5)
            {
              i <- i+1
              if(i == 1) { updateNumericInput(session,"V1FDTerminalOUP",label="~",value=NA) }
              else if(i == 2) { updateNumericInput(session,"V2FDTerminalOUP",label="~",value=NA) }
              else if(i == 3) { updateNumericInput(session,"V3FDTerminalOUP",label="~",value=NA) }
              else if(i == 4) { updateNumericInput(session,"V4FDTerminalOUP",label="~",value=NA) }
              else if(i == 5) { updateNumericInput(session,"V5FDTerminalOUP",label="~",value=NA) }
            }
          }
          FromUItoR6 <- function()
          {
            # Get from UI ----
            xFrom <- input$xFromFDTerminalOUP
            xTo <- input$xToFDTerminalOUP
            xBy <- input$xByFDTerminalOUP
            xOK <- FALSE
            if(is.numeric(xFrom) & is.numeric(xTo))
            {
              if(xTo > xFrom)
              {
                if(is.numeric(xBy))
                {
                  if(xBy > (xTo-xFrom)/100) { xBy <- (xTo-xFrom)/100 }
                  else if(xBy < (xTo-xFrom)/1000) { xBy <- (xTo-xFrom)/1000 }
                }
                else { xBy <- (xTo-xFrom)/100 }
                xOK <- TRUE
              }
            }
            else if(is.numeric(xFrom) & is.numeric(xBy))
            {
              if(xBy > 0)
              {
                xTo <- xFrom+100*xBy
                xOK <- TRUE
              }
            }
            else if(is.numeric(xTo) & is.numeric(xBy))
            {
              if(xBy > 0)
              {
                xFrom <- xTo-100*xBy
                xOK <- TRUE
              }
            }
            v1 <- input$V1FDTerminalOUP
            if(!is.numeric(v1)) { v1 <- NULL }
            v2 <- input$V2FDTerminalOUP
            if(!is.numeric(v2)) { v2 <- NULL }
            v3 <- input$V3FDTerminalOUP
            if(!is.numeric(v3)) { v3 <- NULL }
            v4 <- input$V4FDTerminalOUP
            if(!is.numeric(v4)) { v4 <- NULL }
            v5 <- input$V5FDTerminalOUP
            if(!is.numeric(v5)) { v5 <- NULL }
            # Set to OUP ----
            if(xOK) { FD$set_x_stoch_args(x=seq(from=xFrom,to=xTo,by=xBy)) }
            else { FD$axes_x_stoch() }
            FD$set_V_args(NULL,NULL,v1,v2,v3,v4,v5)
          }
          # Initialize ----
          FromR6toUI()
          # Select ----
          observe({
            FD$set_V_info(input$VFDTerminalOUP)
            FromR6toUI()
          }) %>% bindEvent(input$VFDTerminalOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # Observe undo, axes and plot (or enter key) ----
          undoButton <- reactiveVal(FALSE)
          axesButton <- reactiveVal(FALSE)
          plotButton <- reactiveVal(FALSE)
          observe({
            undoButton(TRUE)
            axesButton(FALSE)
            plotButton(FALSE)
          }) %>% bindEvent(input$undoFDTerminalOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            axesButton(TRUE)
            plotButton(FALSE)
          }) %>% bindEvent(input$axesFDTerminalOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            axesButton(FALSE)
            plotButton(TRUE)
          }) %>% bindEvent(input$plotFDTerminalOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # User clicks save ----
          observe({
            FromUItoR6()
            FD$default_save()
          }) %>% bindEvent(input$saveFDTerminalOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # User clicks undo, axes or plot (or enter key) ----
          output$plotlyFDTerminalOUP <- renderPlotly({
            if(undoButton()) { FD$default_read() }
            else if(axesButton())
            {
              FromUItoR6()
              FD$axes_x_stoch()
            }
            else if(plotButton()) { FromUItoR6() }
            undoButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            FromR6toUI()
            FD$PlotTerminalValue()
          }) %>% bindEvent(input$undoFDTerminalOUP,input$axesFDTerminalOUP,input$plotFDTerminalOUP)
          # User clicks info ----
          observe({
            showModal(modalDialog(
              title=div(img(src="Roar32x32.png"),"Terminal Values"),
              HTML("Analytical option pricing has a kinked terminal value, but the Finite Difference Method is more flexible.  Any terminal value can be pre-calculated and entered into the option pricing calculations.  Some likely terminal values are programmed here for convenience.<br><br>
              &emsp;&emsp;The R6 methods:<br>
              &emsp;&emsp;&emsp;TerminalValue_Linear(<i>x,x</i>o<i>,v</i>s)<br>
              &emsp;&emsp;&emsp;TerminalValue_Kinked(<i>x,x</i>o<i>,v</i>s<i>,V</i>max<i>,V</i>min)<br>
              &emsp;&emsp;&emsp;TerminalValue_Stepped(<i>x,x</i>i<i>,v</i>s<i>,V</i>max<i>,V</i>min)<br>
              &emsp;&emsp;&emsp;TerminalValue_Mitscherlich(<i>x,x</i>i<i>,v</i>r<i>,V</i>max<i>,V</i>min)<br>
              &emsp;&emsp;&emsp;TerminalValue_Gompertz(<i>x,x</i>i<i>,v</i>r<i>,V</i>max<i>,V</i>min)<br>
              &emsp;&emsp;&emsp;TerminalValue_Logistic(<i>x,x</i>i<i>,v</i>r<i>,V</i>max<i>,V</i>min)<br>
              &emsp;&emsp;&emsp;TerminalValue_Transcendental(<i>x,x</i>o<i>,x</i>i<i>,x</i>m<i>,V</i>max<i>,V</i>min)<br>
              &emsp;&emsp;&emsp;TerminalValue_YieldIndex(<i>x,x</i>o<i>,x</i>i<i>,x</i>m<i>,V</i>max<i>,V</i>min)<br>
              &emsp;&emsp;with arguments:<br>
              &emsp;&emsp;&emsp;<i>x</i> are the stochastic states;<br>
              &emsp;&emsp;&emsp;<i>x</i>o is the state at the origin, kink, or step;<br>
              &emsp;&emsp;&emsp;<i>x</i>i is the state at the inflection point;<br>
              &emsp;&emsp;&emsp;<i>x</i>m is the state at the maximum;<br>
              &emsp;&emsp;&emsp;<i>v</i>s is the slope or the direction of a step;<br>
              &emsp;&emsp;&emsp;<i>v</i>r is the rate of change;<br>
              &emsp;&emsp;&emsp;<i>V</i>max is the maximum terminal value;<br>
              &emsp;&emsp;&emsp;<i>V</i>min is the minimum terminal value;<br>
              &emsp;&emsp;return:<br>
              <table style='margin-left: 60px;'>
                <tr>
                  <td style='border-top: solid silver; border-bottom: solid silver; border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'><i>V</i>(<i>x</i><sub>1</sub>)</td>
                  <td style='padding: 0px 4px 0px 4px;'>&hellip;</td>
                  <td style='padding: 0px 4px 0px 4px;'><i>V</i>(<i>x</i><sub>n</sub>)</td>
                  <td style='border-top: solid silver; border-bottom: solid silver; border-right: solid silver'>&nbsp;</td>
                  <td style='padding-left: 2px;'>;</td>
                </tr>
              </table>
              &emsp;&emsp;where:<br>
              &emsp;&emsp;&emsp;<i>V</i> is the Terminal Value."),
              easyClose = TRUE,
              footer = modalButton("Close")
            ))
          }) %>% bindEvent(input$infoFDTerminalOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
        # Option ----
        if(input$navFDOUP == "FDOptionOUP")
        {
          # Dynamic UI ----
          V_info <- FD$get_V_info()
          names <- V_info[[3]]
          output$VFDOptionOUP <- renderUI({ selectInput("VFDOptionOUP",label="V",choices=names) })
          # Define set/get functions ----
          FromR6toUI <- function()
          {
            # Get from OUP ----
            oup_params <- FD$get_oup_params()
            x_stoch_args <- FD$get_x_stoch_args()
            V_args <- FD$get_V_args()
            V_info <- FD$get_V_info()
            rho <-oup_params[[1]]
            mu <- oup_params[[2]]
            sigma <- oup_params[[3]]
            s <- x_stoch_args[[1]]
            x <- x_stoch_args[[2]]
            r <- x_stoch_args[[4]]
            m <- length(s)
            n <- length(x)
            sFrom <- s[m]
            sTo <- s[1]
            if(m > 1) { sBy <- (sTo-sFrom)/(m-1) }
            else  {sBy <- 0 }
            xFrom <- x[1]
            xTo <- x[n]
            if(n > 1) { xBy <- (xTo-xFrom)/(n-1) }
            else  {xBy <- 0 }
            Ix <- V_info[[1]]
            # Set to UI ----
            updateSelectInput(session,"VFDOptionOUP",selected=names[Ix])
            updateNumericInput(session,"rhoFDOptionOUP",value=rho)
            updateNumericInput(session,"muFDOptionOUP",value=mu)
            updateNumericInput(session,"sigmaFDOptionOUP",value=sigma)
            updateNumericInput(session,"rFDOptionOUP",value=r)
            updateNumericInput(session,"sFromFDOptionOUP",value=sFrom)
            updateNumericInput(session,"sToFDOptionOUP",value=sTo)
            updateNumericInput(session,"sByFDOptionOUP",value=sBy)
            updateNumericInput(session,"xFromFDOptionOUP",value=xFrom)
            updateNumericInput(session,"xToFDOptionOUP",value=xTo)
            updateNumericInput(session,"xByFDOptionOUP",value=xBy)
            updateNumericInput(session,"v1FDOptionOUP",label="~",value=NA)
            updateNumericInput(session,"v2FDOptionOUP",label="~",value=NA)
            updateNumericInput(session,"v3FDOptionOUP",label="~",value=NA)
            updateNumericInput(session,"v4FDOptionOUP",label="~",value=NA)
            updateNumericInput(session,"v5FDOptionOUP",label="~",value=NA)
            n <- length(V_args)
            i <- 0
            while(i < n)
            {
              i <- i+1
              argname <- names(V_args[i])
              arg <- V_args[[i]]
              if(i == 1) { updateNumericInput(session,"V1FDOptionOUP",label=argname,value=arg) }
              else if(i == 2) { updateNumericInput(session,"V2FDOptionOUP",label=argname,value=arg) }
              else if(i == 3) { updateNumericInput(session,"V3FDOptionOUP",label=argname,value=arg) }
              else if(i == 4) { updateNumericInput(session,"V4FDOptionOUP",label=argname,value=arg) }
              else if(i == 5) { updateNumericInput(session,"V5FDOptionOUP",label=argname,value=arg) }
            }
            while(i < 5)
            {
              i <- i+1
              if(i == 1) { updateNumericInput(session,"V1FDOptionOUP",label="~",value=NA) }
              else if(i == 2) { updateNumericInput(session,"V2FDOptionOUP",label="~",value=NA) }
              else if(i == 3) { updateNumericInput(session,"V3FDOptionOUP",label="~",value=NA) }
              else if(i == 4) { updateNumericInput(session,"V4FDOptionOUP",label="~",value=NA) }
              else if(i == 5) { updateNumericInput(session,"V5FDOptionOUP",label="~",value=NA) }
            }
          }
          FromUItoR6 <- function()
          {
            # Get from UI ----
            rho <- input$rhoFDOptionOUP
            if(!is.numeric(rho)) { rho <- 0 }
            else if(rho < 0) { rho <- 0 }
            mu <- input$muFDOptionOUP
            if(!is.numeric(mu)) { mu <- 0 }
            sigma <- input$sigmaFDOptionOUP
            if(!is.numeric(sigma)) { sigma <- 0 }
            sFrom <- input$sFromFDOptionOUP
            if(!is.numeric(sFrom)) { sFrom <- 0 }
            sTo <- input$sToFDOptionOUP
            if(!is.numeric(sTo)) { sTo <- 0 }
            sBy <- input$sByFDOptionOUP
            if(!is.numeric(sBy)) { sBy <- 0 }
            if(abs(sTo-sFrom) < abs(sBy)) { sBy <- sTo-sFrom }
            if(abs(sTo-sFrom)/100 > abs(sBy)) { sBy <- (sTo-sFrom)/100 }
            if(sTo > sFrom) { s <- seq(from=sTo,to=sFrom,by=-abs(sBy)) }
            else if(sTo == sFrom) { s <- sTo }
            else { s <- seq(from=sFrom,to=sTo,by=-abs(sBy)) }
            xFrom <- input$xFromFDOptionOUP
            xTo <- input$xToFDOptionOUP
            xBy <- input$xByFDOptionOUP
            xOK <- FALSE
            if(is.numeric(xFrom) & is.numeric(xTo))
            {
              if(xTo > xFrom)
              {
                if(is.numeric(xBy))
                {
                  if(xBy > (xTo-xFrom)/100) { xBy <- (xTo-xFrom)/100 }
                  else if(xBy < (xTo-xFrom)/1000) { xBy <- (xTo-xFrom)/1000 }
                }
                else { xBy <- (xTo-xFrom)/100 }
                xOK <- TRUE
              }
            }
            else if(is.numeric(xFrom) & is.numeric(xBy))
            {
              if(xBy > 0)
              {
                xTo <- xFrom+100*xBy
                xOK <- TRUE
              }
            }
            else if(is.numeric(xTo) & is.numeric(xBy))
            {
              if(xBy > 0)
              {
                xFrom <- xTo-100*xBy
                xOK <- TRUE
              }
            }
            r <- input$rFDOptionOUP
            if(!is.numeric(r)) { r <- 0 }
            v1 <- input$V1FDOptionOUP
            if(!is.numeric(v1)) { v1 <- NULL }
            v2 <- input$V2FDOptionOUP
            if(!is.numeric(v2)) { v2 <- NULL }
            v3 <- input$V3FDOptionOUP
            if(!is.numeric(v3)) { v3 <- NULL }
            v4 <- input$V4FDOptionOUP
            if(!is.numeric(v4)) { v4 <- NULL }
            v5 <- input$V5FDOptionOUP
            if(!is.numeric(v5)) { v5 <- NULL }
            # Set to OUP ----
            FD$set_oup_params(rho=rho,mu=mu,sigma=sigma)
            if(xOK)
            {
              x <- seq(from=xFrom,to=xTo,by=xBy)
              skip <- as.integer(sBy/xBy*100)
              if(skip < 1) { skip <- 1 }
              else if(skip >20) { skip <- 20 }
              FD$set_x_stoch_args(s=s,x=x,r=r,skip=skip)
            }
            else
            {
              FD$axes_x_stoch()
              FD$set_x_stoch_args(r=r,skip=skip)
            }
            FD$set_V_args(NULL,NULL,v1,v2,v3,v4,v5)
          }
          # Initialize ----
          FromR6toUI()
          # Select ----
          observe({
            FD$set_V_info(input$VFDOptionOUP)
            FromR6toUI()
          }) %>% bindEvent(input$VFDOptionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # Observe undo, axes, plot (or enter key), left and rght ----
          undoButton <- reactiveVal(FALSE)
          axesButton <- reactiveVal(FALSE)
          plotButton <- reactiveVal(FALSE)
          leftButton <- reactiveVal(FALSE)
          rghtButton <- reactiveVal(FALSE)
          observe({
            undoButton(TRUE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$undoFDOptionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            axesButton(TRUE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$axesFDOptionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            axesButton(FALSE)
            plotButton(TRUE)
            leftButton(FALSE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$plotFDOptionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(TRUE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$leftFDOptionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(TRUE)
          }) %>% bindEvent(input$rghtFDOptionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # User clicks save ----
          observe({
            FromUItoR6()
            FD$default_save()
          }) %>% bindEvent(input$saveFDOptionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # User clicks undo, axes, plot (or enter key), left or rght ----
          output$plotlyFDOptionOUP <- renderPlotly({
            if(undoButton()) { FD$default_read() }
            else if(axesButton())
            {
              FromUItoR6()
              FD$axes_x_stoch()
            }
            else if(plotButton()) { FromUItoR6() }
            else if(leftButton())
            {
              plot_info <- FD$get_plot_info()
              type <- plot_info$plottype[[1]]
              if(type > 5) { type <- 5 }
              else if(type > 4) { type <- 4 }
              else if(type > 3) { type <- 3 }
              else if(type > 2) { type <- 2 }
              else { type <- 5 }
              FD$set_plot_info(type=type)
              FromUItoR6()
            }
            else if(rghtButton())
            {
              plot_info <- FD$get_plot_info()
              type <- plot_info$plottype[[1]]
              if(type < 2) { type <- 2 }
              else if(type < 3) { type <- 3 }
              else if(type < 4) { type <- 4 }
              else if(type < 5) { type <- 5 }
              else { type <- 2 }
              FD$set_plot_info(type=type)
              FromUItoR6()
            }
            undoButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(FALSE)
            FromR6toUI()
            FD$PlotOption()
          }) %>% bindEvent(input$undoFDOptionOUP,input$axesFDOptionOUP,input$plotFDOptionOUP,input$leftFDOptionOUP,input$rghtFDOptionOUP)
          # User clicks info ----
          observe({
            showModal(modalDialog(
              title=div(img(src="Roar32x32.png"),"Option"),
              HTML("Options are the value of flexibility&mdash;the value of keeping your options open.  Options with kinked terminal values are a fundamental property of the Ornstein-Uhlenbeck Process and have analytical solutions.  Options with arbitrary terminal values can be calculated using the Finite Difference Method.  However, the Ornstein-Uhlenbeck Process has no boundary conditions, which makes finite difference solutions more difficult.  If possible, the Finite Difference Method should be calibrated with an analytical solution.<br><br>
              &emsp;&emsp;The R6 method:<br>
              &emsp;&emsp;&emsp;Option(<i>s,x,V,rho,mu,sigma,r</i>)<br>
              &emsp;&emsp;with arguments:<br>
              &emsp;&emsp;&emsp;<i>s</i> are the variable times;<br>
              &emsp;&emsp;&emsp;<i>x</i> are the stochastic states;<br>
              &emsp;&emsp;&emsp;<i>V</i> are the terminal values;<br>
              &emsp;&emsp;&emsp;<i>rho</i> is the rate parameter;<br>
              &emsp;&emsp;&emsp;<i>mu</i> is the location parameter;<br>
              &emsp;&emsp;&emsp;<i>sigma</i> is the scale parameter;<br>
              &emsp;&emsp;&emsp;<i>r</i> is the discount rate;<br>
              &emsp;&emsp;returns:<br>
              <table style='margin-left: 60px;'>
                <tr>
                  <td style='border-top: solid silver; border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'>&Oopf;(<i>s</i><sub>1</sub>,<i>x</i><sub>1</sub>)</td>
                  <td style='padding: 0px 4px 0px 4px;'>&hellip;</td>
                  <td style='padding: 0px 4px 0px 4px;'>&Oopf;(<i>s</i><sub>1</sub>,<i>x</i><sub>n</sub>)</td>
                  <td style='border-top: solid silver; border-right: solid silver'>&nbsp;</td>
                </tr>
                <tr>
                  <td style='border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'>&emsp;&vellip;</td>
                  <td style='padding: 0px 4px 0px 4px;'>&dtdot;</td>
                  <td style='padding: 0px 4px 0px 4px;'>&emsp;&vellip;</td>
                  <td style='border-right: solid silver'>&nbsp;</td>
                </tr>
                <tr>
                  <td style='border-bottom: solid silver; border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'>&Oopf;(<i>s</i><sub>m</sub>,<i>x</i><sub>1</sub>)</td>
                  <td style='padding: 0px 4px 0px 4px;'>&hellip;</td>
                  <td style='padding: 0px 4px 0px 4px;'>&Oopf;(<i>s</i><sub>m</sub>,<i>x</i><sub>n</sub>)</td>
                  <td style='border-bottom: solid silver; border-right: solid silver'>&nbsp;</td>
                  <td style='padding-left: 2px;'>;</td>
                </tr>
              </table>
              &emsp;&emsp;where:<br>
              &emsp;&emsp;&emsp;&Oopf; is an Option."),
              easyClose = TRUE,
              footer = modalButton("Close")
            ))
          }) %>% bindEvent(input$infoFDOptionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
        # Option Envelope ----
        if(input$navFDOUP == "FDEnvelopeOUP")
        {
          # Dynamic UI ----
          V_info <- FD$get_V_info()
          names <- V_info[[3]]
          output$VFDEnvelopeOUP <- renderUI({ selectInput("VFDEnvelopeOUP",label="V",choices=names) })
          # Define set/get functions ----
          FromR6toUI <- function()
          {
            # Get from OUP ----
            oup_params <- FD$get_oup_params()
            x_stoch_args <- FD$get_x_stoch_args()
            V_args <- FD$get_V_args()
            V_info <- FD$get_V_info()
            rho <-oup_params[[1]]
            mu <- oup_params[[2]]
            sigma <- oup_params[[3]]
            s <- x_stoch_args[[1]]
            x <- x_stoch_args[[2]]
            r <- x_stoch_args[[4]]
            m <- length(s)
            n <- length(x)
            sFrom <- s[m]
            sTo <- s[1]
            if(m > 1) { sBy <- (sTo-sFrom)/(m-1) }
            else  {sBy <- 0 }
            xFrom <- x[1]
            xTo <- x[n]
            if(n > 1) { xBy <- (xTo-xFrom)/(n-1) }
            else  {xBy <- 0 }
            Ix <- V_info[[1]]
            # Set to UI ----
            updateSelectInput(session,"VFDEnvelopeOUP",selected=names[Ix])
            updateNumericInput(session,"rhoFDEnvelopeOUP",value=rho)
            updateNumericInput(session,"muFDEnvelopeOUP",value=mu)
            updateNumericInput(session,"sigmaFDEnvelopeOUP",value=sigma)
            updateNumericInput(session,"rFDEnvelopeOUP",value=r)
            updateNumericInput(session,"sFromFDEnvelopeOUP",value=sFrom)
            updateNumericInput(session,"sToFDEnvelopeOUP",value=sTo)
            updateNumericInput(session,"sByFDEnvelopeOUP",value=sBy)
            updateNumericInput(session,"xFromFDEnvelopeOUP",value=xFrom)
            updateNumericInput(session,"xToFDEnvelopeOUP",value=xTo)
            updateNumericInput(session,"xByFDEnvelopeOUP",value=xBy)
            updateNumericInput(session,"v1FDEnvelopeOUP",label="~",value=NA)
            updateNumericInput(session,"v2FDEnvelopeOUP",label="~",value=NA)
            updateNumericInput(session,"v3FDEnvelopeOUP",label="~",value=NA)
            updateNumericInput(session,"v4FDEnvelopeOUP",label="~",value=NA)
            updateNumericInput(session,"v5FDEnvelopeOUP",label="~",value=NA)
            n <- length(V_args)
            i <- 0
            while(i < n)
            {
              i <- i+1
              argname <- names(V_args[i])
              arg <- V_args[[i]]
              if(i == 1) { updateNumericInput(session,"V1FDEnvelopeOUP",label=argname,value=arg) }
              else if(i == 2) { updateNumericInput(session,"V2FDEnvelopeOUP",label=argname,value=arg) }
              else if(i == 3) { updateNumericInput(session,"V3FDEnvelopeOUP",label=argname,value=arg) }
              else if(i == 4) { updateNumericInput(session,"V4FDEnvelopeOUP",label=argname,value=arg) }
              else if(i == 5) { updateNumericInput(session,"V5FDEnvelopeOUP",label=argname,value=arg) }
            }
            while(i < 5)
            {
              i <- i+1
              if(i == 1) { updateNumericInput(session,"V1FDEnvelopeOUP",label="~",value=NA) }
              else if(i == 2) { updateNumericInput(session,"V2FDEnvelopeOUP",label="~",value=NA) }
              else if(i == 3) { updateNumericInput(session,"V3FDEnvelopeOUP",label="~",value=NA) }
              else if(i == 4) { updateNumericInput(session,"V4FDEnvelopeOUP",label="~",value=NA) }
              else if(i == 5) { updateNumericInput(session,"V5FDEnvelopeOUP",label="~",value=NA) }
            }
          }
          FromUItoR6 <- function()
          {
            # Get from UI ----
            rho <- input$rhoFDEnvelopeOUP
            if(!is.numeric(rho)) { rho <- 0 }
            else if(rho < 0) { rho <- 0 }
            mu <- input$muFDEnvelopeOUP
            if(!is.numeric(mu)) { mu <- 0 }
            sigma <- input$sigmaFDEnvelopeOUP
            if(!is.numeric(sigma)) { sigma <- 0 }
            sFrom <- input$sFromFDOptionOUP
            if(!is.numeric(sFrom)) { sFrom <- 0 }
            sTo <- input$sToFDOptionOUP
            if(!is.numeric(sTo)) { sTo <- 0 }
            sBy <- input$sByFDOptionOUP
            if(!is.numeric(sBy)) { sBy <- 0 }
            if(abs(sTo-sFrom) < abs(sBy)) { sBy <- sTo-sFrom }
            if(abs(sTo-sFrom)/100 > abs(sBy)) { sBy <- (sTo-sFrom)/100 }
            if(sTo > sFrom) { s <- seq(from=sTo,to=sFrom,by=-abs(sBy)) }
            else if(sTo == sFrom) { s <- sTo }
            else { s <- seq(from=sFrom,to=sTo,by=-abs(sBy)) }
            xFrom <- input$xFromFDEnvelopeOUP
            xTo <- input$xToFDEnvelopeOUP
            xBy <- input$xByFDEnvelopeOUP
            xOK <- FALSE
            if(is.numeric(xFrom) & is.numeric(xTo))
            {
              if(xTo > xFrom)
              {
                if(is.numeric(xBy))
                {
                  if(xBy > (xTo-xFrom)/100) { xBy <- (xTo-xFrom)/100 }
                  else if(xBy < (xTo-xFrom)/1000) { xBy <- (xTo-xFrom)/1000 }
                }
                else { xBy <- (xTo-xFrom)/100 }
                xOK <- TRUE
              }
            }
            else if(is.numeric(xFrom) & is.numeric(xBy))
            {
              if(xBy > 0)
              {
                xTo <- xFrom+100*xBy
                xOK <- TRUE
              }
            }
            else if(is.numeric(xTo) & is.numeric(xBy))
            {
              if(xBy > 0)
              {
                xFrom <- xTo-100*xBy
                xOK <- TRUE
              }
            }
            r <- input$rFDEnvelopeOUP
            if(!is.numeric(r)) { r <- 0 }
            skip <- as.integer(sBy/xBy*100)
            if(skip < 1) { skip <- 1 }
            else if(skip >20) { skip <- 20 }
            v1 <- input$V1FDEnvelopeOUP
            if(!is.numeric(v1)) { v1 <- NULL }
            v2 <- input$V2FDEnvelopeOUP
            if(!is.numeric(v2)) { v2 <- NULL }
            v3 <- input$V3FDEnvelopeOUP
            if(!is.numeric(v3)) { v3 <- NULL }
            v4 <- input$V4FDEnvelopeOUP
            if(!is.numeric(v4)) { v4 <- NULL }
            v5 <- input$V5FDEnvelopeOUP
            if(!is.numeric(v5)) { v5 <- NULL }
            # Set to OUP ----
            FD$set_oup_params(rho=rho,mu=mu,sigma=sigma)
            if(xOK)
            {
              x <- seq(from=xFrom,to=xTo,by=xBy)
              skip <- as.integer(sBy/xBy*100)
              if(skip < 1) { skip <- 1 }
              else if(skip >20) { skip <- 20 }
              FD$set_x_stoch_args(s=s,x=x,r=r,skip=skip)
            }
            else
            {
              FD$axes_x_stoch()
              FD$set_x_stoch_args(r=r,skip=skip)
            }
            FD$set_V_args(NULL,NULL,v1,v2,v3,v4,v5)
          }
          # Initialize ----
          FromR6toUI()
          # Select ----
          observe({
            FD$set_V_info(input$VFDEnvelopeOUP)
            FromR6toUI()
          }) %>% bindEvent(input$VFDEnvelopeOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # Observe undo, axes, plot (or enter key), left and rght ----
          undoButton <- reactiveVal(FALSE)
          axesButton <- reactiveVal(FALSE)
          plotButton <- reactiveVal(FALSE)
          leftButton <- reactiveVal(FALSE)
          rghtButton <- reactiveVal(FALSE)
          observe({
            undoButton(TRUE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$undoFDEnvelopeOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            axesButton(TRUE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$axesFDEnvelopeOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            axesButton(FALSE)
            plotButton(TRUE)
            leftButton(FALSE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$plotFDEnvelopeOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(TRUE)
            rghtButton(FALSE)
          }) %>% bindEvent(input$leftFDEnvelopeOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(TRUE)
          }) %>% bindEvent(input$rghtFDEnvelopeOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # User clicks save ----
          observe({
            FromUItoR6()
            FD$default_save()
          }) %>% bindEvent(input$saveFDEnvelopeOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # User clicks undo, axes, plot (or enter key), left or rght ----
          output$plotlyFDEnvelopeOUP <- renderPlotly({
            if(undoButton()) { FD$default_read() }
            else if(axesButton())
            {
              FromUItoR6()
              FD$axes_x_stoch()
            }
            else if(plotButton()) { FromUItoR6() }
            else if(leftButton())
            {
              plot_info <- A$get_plot_info()
              type <- plot_info$plottype[[1]]
              if(type > 4) { type <- 4 }
              else if(type > 3) { type <- 3 }
              else { type <- 4 }
              A$set_plot_info(type=type)
              FromUItoR6()
            }
            else if(rghtButton())
            {
              plot_info <- A$get_plot_info()
              type <- plot_info$plottype[[1]]
              if(type < 3) { type <- 3 }
              else if(type < 4) { type <- 4 }
              else { type <- 3 }
              A$set_plot_info(type=type)
              FromUItoR6()
            }
            undoButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            leftButton(FALSE)
            rghtButton(FALSE)
            FromR6toUI()
            FD$PlotOptionEnvelope()
          }) %>% bindEvent(input$undoFDEnvelopeOUP,input$axesFDEnvelopeOUP,input$plotFDEnvelopeOUP,input$leftFDEnvelopeOUP,input$rghtFDEnvelopeOUP)
          # User clicks info ----
          observe({
            showModal(modalDialog(
              title=div(img(src="Roar32x32.png"),"Option Envelope"),
              HTML("The Option Envelope is the maximum value of either holding or exercising an option for all possible states of nature.  Using the Finite Difference Method, a matrix of Options is first calculated at discrete nodes.  Then the nodes are searched.  The discrete nodes limit the accuracy of the Option Envelope compared with an analytical solution.<br><br>
              &emsp;&emsp;The R6 method:<br>
              &emsp;&emsp;&emsp;OptionEnvelope(<i>x,V,rho,mu,sigma,r</i>)<br>
              &emsp;&emsp;with arguments:<br>
              &emsp;&emsp;&emsp;<i>x</i> are the stochastic states;<br>
              &emsp;&emsp;&emsp;<i>V</i> are the terminal values;<br>
              &emsp;&emsp;&emsp;<i>rho</i> is the rate parameter;<br>
              &emsp;&emsp;&emsp;<i>mu</i> is the location parameter;<br>
              &emsp;&emsp;&emsp;<i>sigma</i> is the scale parameter;<br>
              &emsp;&emsp;&emsp;<i>r</i> is the discount rate;<br>
              &emsp;&emsp;returns:<br>
              <table style='margin-left: 60px;'>
                <tr>
                  <td style='border-top: solid silver; border-bottom: solid silver; border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'>\u00D4(<i>x</i><sub>1</sub>)</td>
                  <td style='padding: 0px 4px 0px 4px;'>&hellip;</td>
                  <td style='padding: 0px 4px 0px 4px;'>\u00D4(<i>x</i><sub>n</sub>)</td>
                  <td style='border-top: solid silver; border-bottom: solid silver; border-right: solid silver'>&nbsp;</td>
                  <td style='padding-left: 2px;'>;</td>
                </tr>
              </table>
              &emsp;&emsp;where:<br>
              &emsp;&emsp;&emsp;\u00D4 is an Option on the Envelope."),
              easyClose = TRUE,
              footer = modalButton("Close")
            ))
          }) %>% bindEvent(input$infoFDEnvelopeOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
        # Decision Threshold ----
        if(input$navFDOUP == "FDDecisionOUP")
        {
          # Dynamic UI ----
          V_info <- FD$get_V_info()
          names <- V_info[[3]]
          output$VFDDecisionOUP <- renderUI({ selectInput("VFDDecisionOUP",label="V",choices=names) })
          # Define set/get functions ----
          FromR6toUI <- function()
          {
            # Get from OUP ----
            oup_params <- FD$get_oup_params()
            x_stoch_args <- FD$get_x_stoch_args()
            V_args <- FD$get_V_args()
            V_info <- FD$get_V_info()
            rho <-oup_params[[1]]
            mu <- oup_params[[2]]
            sigma <- oup_params[[3]]
            x <- x_stoch_args[[2]]
            r <- x_stoch_args[[4]]
            phi <- x_stoch_args[[5]]
            n <- length(x)
            xFrom <- x[1]
            xTo <- x[n]
            if(n > 1) { xBy <- (xTo-xFrom)/(n-1) }
            else  {xBy <- 0 }
            Ix <- V_info[[1]]
            # Set to UI ----
            updateSelectInput(session,"VFDDecisionOUP",selected=names[Ix])
            updateNumericInput(session,"rhoFDDecisionOUP",value=rho)
            updateNumericInput(session,"muFDDecisionOUP",value=mu)
            updateNumericInput(session,"sigmaFDDecisionOUP",value=sigma)
            updateNumericInput(session,"rFDDecisionOUP",value=r)
            updateNumericInput(session,"phiFDDecisionOUP",value=phi)
            updateNumericInput(session,"xFromFDDecisionOUP",value=xFrom)
            updateNumericInput(session,"xToFDDecisionOUP",value=xTo)
            updateNumericInput(session,"xByFDDecisionOUP",value=xBy)
            updateNumericInput(session,"v1FDDecisionOUP",label="~",value=NA)
            updateNumericInput(session,"v2FDDecisionOUP",label="~",value=NA)
            updateNumericInput(session,"v3FDDecisionOUP",label="~",value=NA)
            updateNumericInput(session,"v4FDDecisionOUP",label="~",value=NA)
            updateNumericInput(session,"v5FDDecisionOUP",label="~",value=NA)
            n <- length(V_args)
            i <- 0
            while(i < n)
            {
              i <- i+1
              argname <- names(V_args[i])
              arg <- V_args[[i]]
              if(i == 1) { updateNumericInput(session,"V1FDDecisionOUP",label=argname,value=arg) }
              else if(i == 2) { updateNumericInput(session,"V2FDDecisionOUP",label=argname,value=arg) }
              else if(i == 3) { updateNumericInput(session,"V3FDDecisionOUP",label=argname,value=arg) }
              else if(i == 4) { updateNumericInput(session,"V4FDDecisionOUP",label=argname,value=arg) }
              else if(i == 5) { updateNumericInput(session,"V5FDDecisionOUP",label=argname,value=arg) }
            }
            while(i < 5)
            {
              i <- i+1
              if(i == 1) { updateNumericInput(session,"V1FDDecisionOUP",label="~",value=NA) }
              else if(i == 2) { updateNumericInput(session,"V2FDDecisionOUP",label="~",value=NA) }
              else if(i == 3) { updateNumericInput(session,"V3FDDecisionOUP",label="~",value=NA) }
              else if(i == 4) { updateNumericInput(session,"V4FDDecisionOUP",label="~",value=NA) }
              else if(i == 5) { updateNumericInput(session,"V5FDDecisionOUP",label="~",value=NA) }
            }
          }
          FromUItoR6 <- function()
          {
            # Get from UI ----
            x_stoch_args <- FD$get_x_stoch_args()
            s <- x_stoch_args[[1]]
            m <- length(s)
            if(m > 1) { sBy <- (s[1]-s[m])/(m-1) }
            else  {sBy <- 0 }
            rho <- input$rhoFDDecisionOUP
            if(!is.numeric(rho)) { rho <- 0 }
            else if(rho < 0) { rho <- 0 }
            mu <- input$muFDDecisionOUP
            if(!is.numeric(mu)) { mu <- 0 }
            sigma <- input$sigmaFDDecisionOUP
            if(!is.numeric(sigma)) { sigma <- 0 }
            xFrom <- input$xFromFDDecisionOUP
            xTo <- input$xToFDDecisionOUP
            xBy <- input$xByFDDecisionOUP
            xOK <- FALSE
            if(is.numeric(xFrom) & is.numeric(xTo))
            {
              if(xTo > xFrom)
              {
                if(is.numeric(xBy))
                {
                  if(xBy > (xTo-xFrom)/100) { xBy <- (xTo-xFrom)/100 }
                  else if(xBy < (xTo-xFrom)/1000) { xBy <- (xTo-xFrom)/1000 }
                }
                else { xBy <- (xTo-xFrom)/100 }
                xOK <- TRUE
              }
            }
            else if(is.numeric(xFrom) & is.numeric(xBy))
            {
              if(xBy > 0)
              {
                xTo <- xFrom+100*xBy
                xOK <- TRUE
              }
            }
            else if(is.numeric(xTo) & is.numeric(xBy))
            {
              if(xBy > 0)
              {
                xFrom <- xTo-100*xBy
                xOK <- TRUE
              }
            }
            r <- input$rFDDecisionOUP
            if(!is.numeric(r)) { r <- 0 }
            phi <- input$phiFDDecisionOUP
            if(!is.numeric(phi)) { phi <- 0 }
            else if(phi < 0) { phi <- -1 }
            else if(phi > 0) { phi <- 1 }
            v1 <- input$V1FDDecisionOUP
            if(!is.numeric(v1)) { v1 <- NULL }
            v2 <- input$V2FDDecisionOUP
            if(!is.numeric(v2)) { v2 <- NULL }
            v3 <- input$V3FDDecisionOUP
            if(!is.numeric(v3)) { v3 <- NULL }
            v4 <- input$V4FDDecisionOUP
            if(!is.numeric(v4)) { v4 <- NULL }
            v5 <- input$V5FDDecisionOUP
            if(!is.numeric(v5)) { v5 <- NULL }
            # Set to OUP ----
            FD$set_oup_params(rho=rho,mu=mu,sigma=sigma)
            if(xOK)
            {
              x <- seq(from=xFrom,to=xTo,by=xBy)
              skip <- as.integer(sBy/xBy*100)
              if(skip < 1) { skip <- 1 }
              else if(skip >20) { skip <- 20 }
              FD$set_x_stoch_args(x=x,r=r,phi=phi,skip=skip)
            }
            else
            {
              FD$axes_x_stoch()
              FD$set_x_stoch_args(r=r,phi=phi)
            }
            FD$set_V_args(NULL,NULL,v1,v2,v3,v4,v5)
          }
          # Initialize ----
          FromR6toUI()
          # Select ----
          observe({
            FD$set_V_info(input$VFDDecisionOUP)
            FromR6toUI()
          }) %>% bindEvent(input$VFDDecisionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # Observe undo, axes and plot (or enter key) ----
          undoButton <- reactiveVal(FALSE)
          axesButton <- reactiveVal(FALSE)
          plotButton <- reactiveVal(FALSE)
          observe({
            undoButton(TRUE)
            axesButton(FALSE)
            plotButton(FALSE)
          }) %>% bindEvent(input$undoFDDecisionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            axesButton(TRUE)
            plotButton(FALSE)
          }) %>% bindEvent(input$axesFDDecisionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            undoButton(FALSE)
            axesButton(FALSE)
            plotButton(TRUE)
          }) %>% bindEvent(input$plotFDDecisionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # User clicks save ----
          observe({
            FromUItoR6()
            FD$default_save()
          }) %>% bindEvent(input$saveFDDecisionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # User clicks undo, axes or plot (or enter key) ----
          output$plotlyFDDecisionOUP <- renderPlotly({
            if(undoButton()) { FD$default_read() }
            else if(axesButton())
            {
              FromUItoR6()
              FD$axes_x_stoch()
            }
            else if(plotButton()) { FromUItoR6() }
            undoButton(FALSE)
            axesButton(FALSE)
            plotButton(FALSE)
            FromR6toUI()
            FD$PlotDecisionThreshold()
          }) %>% bindEvent(input$undoFDDecisionOUP,input$axesFDDecisionOUP,input$plotFDDecisionOUP)
          # User clicks info ----
          observe({
            showModal(modalDialog(
              title=div(img(src="Roar32x32.png"),"Decision Threshold"),
              HTML("The Decision Threshold is the point of indifference between holding and exercising a perpetual option.  The Finite Difference Method calculates Options at discrete nodes, which gives an Option Envelope at discrete nodes.  Choosing a node as the indifference point is inaccurate.  To improve the accuracy, a polynomial interpolation of the Option Envelope is used to approximate the Decision Threshold. For reliability, the Finite Difference Method with a Kinked Terminal Value can be calibrated against an Analytical solution.<br><br>
              &emsp;&emsp;The R6 method:<br>
              &emsp;&emsp;&emsp;DecisionThreshold(<i>x,V,rho,mu,sigma,r,phi</i>)<br>
              &emsp;&emsp;with arguments:<br>
              &emsp;&emsp;&emsp;<i>x</i> are the stochastic states;<br>
              &emsp;&emsp;&emsp;<i>V</i> are the terminal values;<br>
              &emsp;&emsp;&emsp;<i>rho</i> is the rate parameter;<br>
              &emsp;&emsp;&emsp;<i>mu</i> is the location parameter;<br>
              &emsp;&emsp;&emsp;<i>sigma</i> is the scale parameter;<br>
              &emsp;&emsp;&emsp;<i>r</i> is the discount rate;<br>
              &emsp;&emsp;&emsp;<i>phi</i> is < 0 for an Exit Option, > 0 for an Entry Option, = 0 for either;<br>
              &emsp;&emsp;returns:<br>
              <table style='margin-left: 60px;'>
                <tr>
                  <td style='border-top: solid silver; border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'><i>k</i></td>
                  <td style='border-top: solid silver; border-right: solid silver'>&nbsp;</td>
                </tr>
                <tr>
                  <td style='border-bottom: solid silver; border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'>\u00D4(<i>k</i>)</td>
                  <td style='border-bottom: solid silver; border-right: solid silver'>&nbsp;</td>
                  <td style='padding-left: 2px;'>;</td>
                </tr>
              </table>
              &emsp;&emsp;where:<br>
              &emsp;&emsp;&emsp;<i>k</i> is the state at the Decision Threshold;<br>
              &emsp;&emsp;&emsp;\u00D4 is the Option at the Decision Threshold."),
              easyClose = TRUE,
              footer = modalButton("Close")
            ))
          }) %>% bindEvent(input$infoFDDecisionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
      })
    }
    else if(input$navBar == "tabMLOUP")
    {
      observeEvent(input$navMLOUP,{
        # Data ----
        if(input$navMLOUP == "MLDataOUP")
        {
          # Define set functions ----
          DataInfo <- function()
          {
            output$descrMLDataOUP <- renderUI({
              HTML(paste(sep="",
                "<table align='center'>
                  <tr>
                    <th style='text-align: right; padding: 2px; border-bottom: 1px solid grey;'>First</th>
                    <th style='text-align: right; padding: 2px; border-bottom: 1px solid grey;'>Last</th>
                    <th style='text-align: right; padding: 2px;>&emsp;'</th>
                    <th style='text-align: right; padding: 2px; border-bottom: 1px solid grey;'>Rows</th>
                    <th style='text-align: right; padding: 2px; border-bottom: 1px solid grey;'>Cols</th>
                  </tr>
                  <tr>
                    <td style='text-align: right; padding: 8px;'>",nfirst,"</td>
                    <td style='text-align: right; padding: 8px;'>",nlast,"</td>
                    <td style='text-align: right; padding: 8px;'>&emsp;</td>
                    <td style='text-align: right; padding: 8px;'>",nrows,"</td>
                    <td style='text-align: right; padding: 8px;'>",ncols,"</td>
                  </tr>
                </table>"
              ))
            })
          }
          FromR6toUI <- function()
          {
# message("Data FromR6toUI")
            if(first)
            {
# message("first")
              df <<- read.csv(uploadpath,fileEncoding="UTF-8-BOM")
              framenames <<- colnames(df)
              dname[3] <<- uploadname
              tname[3] <<- framenames[1]
              sname[3] <<- framenames[2]
              nrows <<- nrow(df)
              ncols <<- ncol(df)
              nfirst <<- df[1,1]
              nlast <<- df[nrows,1]
              series <- ML$set_timeseries(df=df,taucol=1,zcol=2)
              Ixend <<- nrow(series)
              end <<- series[Ixend,1]
              if(Ixend > 200) { Ixbeg <<- Ixend-200 }
              else { Ixbeg <<- 1 }
              beg <<- series[Ixbeg,1]
              ML$set_timeseries_info(tbeg=beg,tend=end,dataname=dname[3],timename=tname[3],statename=sname[3],NULL)
# df_info <- ML$get_timeseries_info()
# message(df_info$tbeg,df_info$tend,df_info$dataname,df_info$timename,df_info$statename)
              updateSelectInput(session,"filesMLDataOUP",choices=filelist,selected=dname[3])
              updateSelectInput(session,"timeMLDataOUP",choices=framenames,selected=tname[3])
              updateSelectInput(session,"stateMLDataOUP",choices=framenames,selected=sname[3])
              DataInfo()
              first <<- FALSE
              initialize[1] <<- FALSE
              bounce[1] <<- 1
              bounce[2] <<- 1
              bounce[3] <<- 1
            }
            else if(initialize[1])
            {
# message("initialize")
              df_info <- ML$get_timeseries_info()
              dname[3] <<- df_info[[5]]
              tname[3] <<- df_info[[6]]
              sname[3] <<- df_info[[7]]
              updateSelectInput(session,"filesMLDataOUP",choices=filelist,selected=dname[3])
              updateSelectInput(session,"timeMLDataOUP",choices=framenames,selected=tname[3])
              updateSelectInput(session,"stateMLDataOUP",choices=framenames,selected=sname[3])
              DataInfo()
              initialize[1] <<- FALSE
              bounce[1] <<- 1
              bounce[2] <<- 1
              bounce[3] <<- 1
            }
            else
            {
# message("else")
              df_info <- ML$get_timeseries_info()
              dataname <- df_info[[5]]
              timename <- df_info[[6]]
              statename <- df_info[[7]]
              if(dataname != dname[3] | timename != tname[3] | statename != sname[3])
              {
# message(dataname,", ",dname[3],", ",timename,", ",tname[3],", ",statename,", ",sname[3])
                updateSelectInput(session,"filesMLDataOUP",choices=filelist,selected=dataname)
                updateSelectInput(session,"timeMLDataOUP",choices=framenames,selected=timename)
                updateSelectInput(session,"stateMLDataOUP",choices=framenames,selected=statename)
                DataInfo()
                bounce[1] <<- 1
                bounce[2] <<- 1
                bounce[3] <<- 1
                dname[3] <<- dataname
                tname[3] <<- timename
                sname[3] <<- statename
              }
            }
            updateNumericInput(session,"begMLDataOUP",value=beg)
            updateNumericInput(session,"endMLDataOUP",value=end)
          }
          # initialize ----
          FromR6toUI()
          # select ----
          observe({
# message("Data observe file")
# message(bounce[1])
            if(bounce[1] > 0) { bounce[1] <<- bounce[1]-1 }
            else
            {
              if(dname[3] != input$filesMLDataOUP)
              {
# message(dname[3],", ",input$filesMLDataOUP)
                dname[3] <<- input$filesMLDataOUP
                if(dname[3] == uploadname) { filepath <- uploadpath }
                else { filepath <- paste(sep="",datapath,input$filesMLDataOUP,".csv")  }
                df <<- read.csv(filepath,fileEncoding="UTF-8-BOM")
                framenames <<- colnames(df)
                tname[3] <<- framenames[1]
                sname[3] <<- framenames[2]
                nrows <<- nrow(df)
                ncols <<- ncol(df)
                nfirst <<- df[1,1]
                nlast <<- df[nrows,1]
                series <- ML$set_timeseries(df=df,taucol=1,zcol=2)
                Ixend <<- nrow(series)
                end <<- series[Ixend,1]
                if(Ixend > 200) { Ixbeg <<- Ixend-200 }
                else { Ixbeg <<- 1 }
                beg <<- series[Ixbeg,1]
                ML$set_timeseries_info(tbeg=beg,tend=end,dataname=dname[3],timename=tname[3],statename=sname[3],NULL)
# df_info <- ML$get_timeseries_info()
# message(df_info$tbeg,df_info$tend,df_info$dataname,df_info$timename,df_info$statename)
                updateSelectInput(session,"timeMLDataOUP",choices=framenames,selected=tname[3])
                updateSelectInput(session,"stateMLDataOUP",choices=framenames,selected=sname[3])
                updateNumericInput(session,"begMLDataOUP",value=beg)
                updateNumericInput(session,"endMLDataOUP",value=end)
                DataInfo()
                lnL_params[1] <<- 0
                lnL_params[2] <<- 0
                lnL_params[3] <<- 0
                bounce[2] <<- bounce[2]+1
                bounce[3] <<- bounce[3]+1
              }
            }
          }) %>% bindEvent(input$filesMLDataOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
# message("Data observe time")
# message(bounce[2])
            if(bounce[2] > 0) { bounce[2] <<- bounce[2]-1 }
            else
            {
              if(tname[3] != input$timeMLDataOUP)
              {
# message(tname[3],", ",input$timeMLDataOUP)
                tname[3] <<- input$timeMLDataOUP
                taucol <- match(tname[3],framenames)
                zcol <- match(sname[3],framenames)
                series <- ML$set_timeseries(df=df,taucol=taucol,zcol=zcol)
                Ixend <<- nrow(series)
                end <<- series[Ixend,1]
                if(Ixend > 200) { Ixbeg <<- Ixend-200 }
                else { Ixbeg <<- 1 }
                beg <<- series[Ixbeg,1]
                ML$set_timeseries_info(tbeg=beg,tend=end,dataname=dname[3],timename=tname[3],statename=sname[3],NULL)
# df_info <- ML$get_timeseries_info()
# message(df_info$tbeg,df_info$tend,df_info$dataname,df_info$timename,df_info$statename)
                updateNumericInput(session,"begMLDataOUP",value=beg)
                updateNumericInput(session,"endMLDataOUP",value=end)
                lnL_params[1] <<- 0
                lnL_params[2] <<- 0
                lnL_params[3] <<- 0
              }
            }
          }) %>% bindEvent(input$timeMLDataOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
# message("Data observe state")
# message(bounce[3])
            if(bounce[3] > 0) { bounce[3] <<- bounce[3]-1 }
            else
            {
              if(sname[3] != input$stateMLDataOUP)
              {
# message(sname[3],", ",input$stateMLDataOUP)
                sname[3] <<- input$stateMLDataOUP
                taucol <- match(tname[3],framenames)
                zcol <- match(sname[3],framenames)
                series <- ML$set_timeseries(df=df,taucol=taucol,zcol=zcol)
                Ixend <<- nrow(series)
                end <<- series[Ixend,1]
                if(Ixend > 200) { Ixbeg <<- Ixend-200 }
                else { Ixbeg <<- 1 }
                beg <<- series[Ixbeg,1]
                ML$set_timeseries_info(tbeg=beg,tend=end,dataname=dname[3],timename=tname[3],statename=sname[3],NULL)
# df_info <- ML$get_timeseries_info()
# message(df_info$tbeg,df_info$tend,df_info$dataname,df_info$timename,df_info$statename)
                updateNumericInput(session,"begMLDataOUP",value=beg)
                updateNumericInput(session,"endMLDataOUP",value=end)
                lnL_params[1] <<- 0
                lnL_params[2] <<- 0
                lnL_params[3] <<- 0
              }
            }
          }) %>% bindEvent(input$stateMLDataOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # upload ----
          observe({
            uploadname <<- file_path_sans_ext(input$filesMLUploadOUP$name)
            uploadpath <<- input$filesMLUploadOUP$datapath
            filelist[1] <<- uploadname
# message(uploadname)
# print(filelist)
            first <<- TRUE
            FromR6toUI()
          }) %>% bindEvent(input$filesMLUploadOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # Observe reset, begin and end ----
          resetButton <- reactiveVal(FALSE)
          beginInput <- reactiveVal(FALSE)
          endInput <- reactiveVal(FALSE)
          observe({
            resetButton(TRUE)
          }) %>% bindEvent(input$resetMLDataOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            beginInput(TRUE)
          }) %>% bindEvent(input$begMLDataOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            endInput(TRUE)
          }) %>% bindEvent(input$endMLDataOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # plot ----
          observe({
# message("plot")
            if(resetButton())
            {
              begin <- -Inf
              endin <- Inf
              df_info <- ML$set_timeseries_info(tbeg=begin,tend=endin,NULL,NULL,NULL,NULL)
              updateNumericInput(session,"begMLDataOUP",value=df_info$tbeg)
              updateNumericInput(session,"endMLDataOUP",value=df_info$tend)
              beg <<- df_info$tbeg
              end <<- df_info$tend
            }
            else
            {
              if(beginInput())
              {
                begin <- input$begMLDataOUP
                if(!is.numeric(begin)) { begin <- -Inf }
                df_info <- ML$set_timeseries_info(tbeg=begin,NULL,NULL,NULL,NULL,NULL)
                updateNumericInput(session,"begMLDataOUP",value=df_info$tbeg)
                beg <<- df_info$tbeg
              }
              if(endInput())
              {
                endin <- input$endMLDataOUP
                if(!is.numeric(endin)) { endin <- Inf }
                df_info <- ML$set_timeseries_info(NULL,tend=endin,NULL,NULL,NULL,NULL)
                updateNumericInput(session,"endMLDataOUP",value=df_info$tend)
                end <<- df_info$tend
              }
            }
            resetButton(FALSE)
            beginInput(FALSE)
            endInput(FALSE)
            output$plotlyMLDataOUP <- renderPlotly({ ML$PlotTimeSeries() })
          }) %>% bindEvent(input$resetMLDataOUP,input$plotMLDataOUP)
          # User clicks i ----
          observe({
            htmlname <- paste(sep="",htmlpath,input$filesMLDataOUP,".html")
            if(!file.exists(htmlname)) { htmlname <- paste(sep="",htmlpath,"MyData.html") }
            rawtext <- read_html(htmlname)
            halo <- input$filesMLDataOUP
            body <- html_element(rawtext,"body")
            gen1 <- html_children(body)
            gen2 <- html_children(gen1)
            m <- length(gen2)-2
            soul <- as.character(gen2[2:m])
            style <- "<style>h2 { font-size: 120% } h3 { font-size: 110% }</style>"
            showModal(modalDialog(
              title=div(img(src="Roar32x32.png"),halo),
              HTML(paste(sep="",style,soul)),
              easyClose = TRUE,
              footer = modalButton("Close"),
              size = "l"
            ))
          }) %>% bindEvent(input$fileinfoMLDataOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # User clicks info ----
          observe({
            showModal(modalDialog(
              title=div(img(src="Roar32x32.png"),"Data"),
              HTML("The rate, location and scale parameters of the Ornstein-Uhlenbeck Process can be plucked out of the air, cogitated by experts, deduced from theory or estimated using data.<br><br>
              Data must be a time-series, with observations of times and states of nature.  Within the time-series, each observation has its own initial time and state, and its own terminal time and state.  Typically, the terminal time and state of one observation will be the initial time and state of the next observation.  Therefore, if measurements are taken at <i>m</i>  times, there will be <i>m</i>-1 observations.<br><br>
              Data is read from 'csv' (comma separated value) files.  Typically the files would be organized as in this table.
              <table style='margin-left: 60px;'>
                <tr>
                  <td style='border-top: solid silver; border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'><i>tau</i></td>
                  <td style='padding: 0px 4px 0px 4px;'><i>z</i><sub>1</sub></td>
                  <td style='padding: 0px 4px 0px 4px;'>&hellip;</td>
                  <td style='padding: 0px 4px 0px 4px;'><i>z</i><sub>n</sub></td>
                  <td style='border-top: solid silver; border-right: solid silver'>&nbsp;</td>
                </tr>
                <tr>
                  <td style='border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'>1</td>
                  <td style='padding: 0px 4px 0px 4px;'>16.3</td>
                  <td style='padding: 0px 4px 0px 4px;'>&hellip;</td>
                  <td style='padding: 0px 4px 0px 4px;'>12.7</td>
                  <td style='border-right: solid silver'>&nbsp;</td>
                </tr>
                <tr>
                  <td style='border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'>2</td>
                  <td style='padding: 0px 4px 0px 4px;'>5.1</td>
                  <td style='padding: 0px 4px 0px 4px;'>&hellip;</td>
                  <td style='padding: 0px 4px 0px 4px;'>13.9</td>
                  <td style='border-right: solid silver'>&nbsp;</td>
                </tr>
                <tr>
                  <td style='border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'>&nbsp;&vellip;</td>
                  <td style='padding: 0px 4px 0px 4px;'>&emsp;&vellip;</td>
                  <td style='padding: 0px 4px 0px 4px;'>&dtdot;</td>
                  <td style='padding: 0px 4px 0px 4px;'>&emsp;&vellip;</td>
                  <td style='border-right: solid silver'>&nbsp;</td>
                </tr>
                <tr>
                  <td style='border-bottom: solid silver; border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'><i>m</i></td>
                  <td style='padding: 0px 4px 0px 4px;'>14.3</td>
                  <td style='padding: 0px 4px 0px 4px;'>&hellip;</td>
                  <td style='padding: 0px 4px 0px 4px;'>8.9</td>
                  <td style='border-bottom: solid silver; border-right: solid silver'>&nbsp;</td>
                  <td style='padding-left: 2px;'>;</td>
                </tr>
              </table>
              Names are in the first row.  Numbers start in the second row.  Time is in the first column and states start in the second column.  There can be more than one time column.  There must be <i>m</i>+1 rows in all columns, but there can be blank elements if there is no measurment at that time.  Data is sorted by time and time intervals can be unequal.  Indeed, unequal time intervals seem to improve the estimation.<br><br>
              How the time intervals are measured affects the estimation of parameters <i>rho</i> and <i>sigma</i>.  For example, if measurements are taken once per year and time is reported in years, time interval <i>t-s</i> will be 1 year for a typical observation.  Parameter <i>rho</i> will likely range from 0.1 to 4.0 and <i>sigma</i> will range from 10 to 50.  If measurements are daily but time is reported in years, time interval <i>t-s</i> will be 1/365 years.  Parameter <i>rho</i> will be about 365 times larger and parameter <i>sigma</i> will be about (2<i>rho</i>)<sup>0.5</sup> times larger."),
              easyClose = TRUE,
              footer = modalButton("Close")
            ))
          }) %>% bindEvent(input$infoMLDataOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
        # Log Likelihood ----
        else if(input$navMLOUP == "MLLikelihoodOUP")
        {
          # define set function ----
          FromR6toUI <- function()
          {
# message("Likelihood FromR6toUI")
            if(first)
            {
# message("first")
              df <<- read.csv(uploadpath,fileEncoding="UTF-8-BOM")
              framenames <<- colnames(df)
              dname[4] <<- uploadname
              tname[4] <<- framenames[1]
              sname[4] <<- framenames[2]
              nrows <<- nrow(df)
              ncols <<- ncol(df)
              nfirst <<- df[1,1]
              nlast <<- df[nrows,1]
              series <- ML$set_timeseries(df=df,taucol=1,zcol=2)
              Ixend <<- nrow(series)
              end <<- series[Ixend,1]
              if(Ixend > 200) { Ixbeg <<- Ixend-200 }
              else { Ixbeg <<- 1 }
              beg <<- series[Ixbeg,1]
              ML$set_timeseries_info(tbeg=beg,tend=end,dataname=dname[4],timename=tname[4],statename=sname[4],NULL)
# df_info <- ML$get_timeseries_info()
# message(df_info$tbeg,df_info$tend,df_info$dataname,df_info$timename,df_info$statename)
              updateSelectInput(session,"filesMLLikelihoodOUP",choices=filelist,selected=dname[4])
              updateSelectInput(session,"timeMLLikelihoodOUP",choices=framenames,selected=tname[4])
              updateSelectInput(session,"stateMLLikelihoodOUP",choices=framenames,selected=sname[4])
              first <<- FALSE
              initialize[2] <<- FALSE
              bounce[1] <<- 1
              bounce[2] <<- 1
              bounce[3] <<- 1
            }
            else if(initialize[2])
            {
# message("initialize")
              df_info <- ML$get_timeseries_info()
              dname[4] <<- df_info[[5]]
              tname[4] <<- df_info[[6]]
              sname[4] <<- df_info[[7]]
              updateSelectInput(session,"filesMLLikelihoodOUP",choices=filelist,selected=dname[4])
              updateSelectInput(session,"timeMLLikelihoodOUP",choices=framenames,selected=tname[4])
              updateSelectInput(session,"stateMLLikelihoodOUP",choices=framenames,selected=sname[4])
              initialize[2] <<- FALSE
              bounce[1] <<- 1
              bounce[2] <<- 1
              bounce[3] <<- 1
            }
            else
            {
# message("else")
              df_info <- ML$get_timeseries_info()
              dataname <- df_info[[5]]
              timename <- df_info[[6]]
              statename <- df_info[[7]]
              if(dataname != dname[4] | timename != tname[4] | statename != sname[4])
              {
# message(dataname,", ",dname[4],", ",timename,", ",tname[4],", ",statename,", ",sname[4])
                updateSelectInput(session,"filesMLLikelihoodOUP",choices=filelist,selected=dataname)
                updateSelectInput(session,"timeMLLikelihoodOUP",choices=framenames,selected=timename)
                updateSelectInput(session,"stateMLLikelihoodOUP",choices=framenames,selected=statename)
                bounce[1] <<- 1
                bounce[2] <<- 1
                bounce[3] <<- 1
                dname[4] <<- dataname
                tname[4] <<- timename
                sname[4] <<- statename
              }
            }
            updateNumericInput(session,"begMLLikelihoodOUP",value=beg)
            updateNumericInput(session,"endMLLikelihoodOUP",value=end)
            updateNumericInput(session,"lnLMLLikelihoodOUP",value=NaN)
            updateNumericInput(session,"rhoMLLikelihoodOUP",value=lnL_params[1])
            updateNumericInput(session,"muMLLikelihoodOUP",value=lnL_params[2])
            updateNumericInput(session,"sigmaMLLikelihoodOUP",value=lnL_params[3])
          }
          # initialize ----
          FromR6toUI()
          # select ----
          observe({
# message("Likelihood observe file")
# message(bounce[1])
            if(bounce[1] > 0) { bounce[1] <<- bounce[1]-1 }
            else
            {
              if(dname[4] != input$filesMLLikelihoodOUP)
              {
# message(dname[4],", ",input$filesMLLikelihoodOUP)
                dname[4] <<- input$filesMLLikelihoodOUP
                if(dname[4] == uploadname) { filepath <- uploadpath }
                else { filepath <- paste(sep="",datapath,input$filesMLLikelihoodOUP,".csv")  }
                df <<- read.csv(filepath,fileEncoding="UTF-8-BOM")
                framenames <<- colnames(df)
                tname[4] <<- framenames[1]
                sname[4] <<- framenames[2]
                nrows <<- nrow(df)
                ncols <<- ncol(df)
                nfirst <<- df[1,1]
                nlast <<- df[nrows,1]
                series <- ML$set_timeseries(df=df,taucol=1,zcol=2)
                Ixend <<- nrow(series)
                end <<- series[Ixend,1]
                if(Ixend > 200) { Ixbeg <<- Ixend-200 }
                else { Ixbeg <<- 1 }
                beg <<- series[Ixbeg,1]
                ML$set_timeseries_info(tbeg=beg,tend=end,dataname=dname[4],timename=tname[4],statename=sname[4])
# df_info <- ML$get_timeseries_info()
# message(df_info$tbeg,df_info$tend,df_info$dataname,df_info$timename,df_info$statename)
                updateSelectInput(session,"timeMLLikelihoodOUP",choices=framenames,selected=tname[4])
                updateSelectInput(session,"stateMLLikelihoodOUP",choices=framenames,selected=sname[4])
                updateNumericInput(session,"begMLLikelihoodOUP",value=beg)
                updateNumericInput(session,"endMLLikelihoodOUP",value=end)
                updateNumericInput(session,"lnLMLLikelihoodOUP",value=NaN)
                updateNumericInput(session,"rhoMLLikelihoodOUP",value=0)
                updateNumericInput(session,"muMLLikelihoodOUP",value=0)
                updateNumericInput(session,"sigmaMLLikelihoodOUP",value=0)
                lnL_params[1] <<- 0
                lnL_params[2] <<- 0
                lnL_params[3] <<- 0
                LRT_params[1] <<- NA
                LRT_params[2] <<- NA
                LRT_params[3] <<- NA
                bounce[2] <<- bounce[2]+1
                bounce[3] <<- bounce[3]+1
              }
            }
          }) %>% bindEvent(input$filesMLLikelihoodOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
# message("Likelihood observe time")
# message(bounce[2])
            if(bounce[2] > 0) { bounce[2] <<- bounce[2]-1 }
            else
            {
              if(tname[4] != input$timeMLLikelihoodOUP)
              {
# message(tname[4],", ",input$timeMLLikelihoodOUP)
                tname[4] <<- input$timeMLLikelihoodOUP
                taucol <- match(tname[4],framenames)
                zcol <- match(sname[4],framenames)
                series <- ML$set_timeseries(df=df,taucol=taucol,zcol=zcol)
                Ixend <<- nrow(series)
                end <<- series[Ixend,1]
                if(Ixend > 200) { Ixbeg <<- Ixend-200 }
                else { Ixbeg <<- 1 }
                beg <<- series[Ixbeg,1]
                ML$set_timeseries_info(tbeg=beg,tend=end,dataname=dname[4],timename=tname[4],statename=sname[4],NULL)
# df_info <- ML$get_timeseries_info()
# message(df_info$tbeg,df_info$tend,df_info$dataname,df_info$timename,df_info$statename)
                updateNumericInput(session,"begMLLikelihoodOUP",value=beg)
                updateNumericInput(session,"endMLLikelihoodOUP",value=end)
                updateNumericInput(session,"lnLMLLikelihoodOUP",value=NaN)
                updateNumericInput(session,"rhoMLLikelihoodOUP",value=0)
                updateNumericInput(session,"muMLLikelihoodOUP",value=0)
                updateNumericInput(session,"sigmaMLLikelihoodOUP",value=0)
                lnL_params[1] <<- 0
                lnL_params[2] <<- 0
                lnL_params[3] <<- 0
                LRT_params[1] <<- NA
                LRT_params[2] <<- NA
                LRT_params[3] <<- NA
              }
            }
          }) %>% bindEvent(input$timeMLLikelihoodOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
# message("Likelihood observe state")
# message(bounce[3])
            if(bounce[3] > 0) { bounce[3] <<- bounce[3]-1 }
            else
            {
              if(sname[4] != input$stateMLLikelihoodOUP)
              {
# message(sname[4],", ",input$stateMLLikelihoodOUP)
                sname[4] <<- input$stateMLLikelihoodOUP
                taucol <- match(tname[4],framenames)
                zcol <- match(sname[4],framenames)
                series <- ML$set_timeseries(df=df,taucol=taucol,zcol=zcol)
                Ixend <<- nrow(series)
                end <<- series[Ixend,1]
                if(Ixend > 200) { Ixbeg <<- Ixend-200 }
                else { Ixbeg <<- 1 }
                beg <<- series[Ixbeg,1]
                ML$set_timeseries_info(tbeg=beg,tend=end,dataname=dname[4],timename=tname[4],statename=sname[4],NULL)
# df_info <- ML$get_timeseries_info()
# message(df_info$tbeg,df_info$tend,df_info$dataname,df_info$timename,df_info$statename)
                updateNumericInput(session,"begMLLikelihoodOUP",value=beg)
                updateNumericInput(session,"endMLLikelihoodOUP",value=end)
                updateNumericInput(session,"lnLMLLikelihoodOUP",value=NaN)
                updateNumericInput(session,"rhoMLLikelihoodOUP",value=0)
                updateNumericInput(session,"muMLLikelihoodOUP",value=0)
                updateNumericInput(session,"sigmaMLLikelihoodOUP",value=0)
                lnL_params[1] <<- 0
                lnL_params[2] <<- 0
                lnL_params[3] <<- 0
                LRT_params[1] <<- NA
                LRT_params[2] <<- NA
                LRT_params[3] <<- NA
              }
            }
          }) %>% bindEvent(input$stateMLLikelihoodOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # Observe rho, mu and sigma ----
          rhoInput <- reactiveVal(FALSE)
          muInput <- reactiveVal(FALSE)
          sigmaInput <- reactiveVal(FALSE)
          observe({
            rhoInput(TRUE)
          }) %>% bindEvent(input$rhoMLLikelihoodOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            muInput(TRUE)
          }) %>% bindEvent(input$muMLLikelihoodOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            sigmaInput(TRUE)
          }) %>% bindEvent(input$sigmaMLLikelihoodOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # Observe reset, begin and end ----
          resetButton <- reactiveVal(FALSE)
          beginInput <- reactiveVal(FALSE)
          endInput <- reactiveVal(FALSE)
          observe({
            resetButton(TRUE)
          }) %>% bindEvent(input$resetMLLikelihoodOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            beginInput(TRUE)
          }) %>% bindEvent(input$begMLLikelihoodOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            endInput(TRUE)
          }) %>% bindEvent(input$endMLLikelihoodOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # go ----
          observe({
# message("go")
            if(rhoInput()) { lnL_params[1] <<- input$rhoMLLikelihoodOUP }
            if(muInput()) { lnL_params[2] <<- input$muMLLikelihoodOUP }
            if(sigmaInput()) { lnL_params[3] <<- input$sigmaMLLikelihoodOUP }
            rhoInput(FALSE)
            muInput(FALSE)
            sigmaInput(FALSE)
            if(resetButton())
            {
              begin <- -Inf
              endin <- Inf
              timeseries_info <- ML$set_timeseries_info(tbeg=begin,tend=endin,NULL,NULL,NULL,NULL)
              updateNumericInput(session,"begMLLikelihoodOUP",value=timeseries_info$tbeg)
              updateNumericInput(session,"endMLLikelihoodOUP",value=timeseries_info$tend)
              beg <<- timeseries_info$tbeg
              end <<- timeseries_info$tend
            }
            else
            {
              if(beginInput())
              {
                begin <- input$begMLLikelihoodOUP
                if(!is.numeric(begin)) { begin <- -Inf }
                df_info <- ML$set_timeseries_info(tbeg=begin,NULL,NULL,NULL,NULL,NULL)
                updateNumericInput(session,"begMLLikelihoodOUP",value=df_info$tbeg)
                beg <<- df_info$tbeg
              }
              if(endInput())
              {
                endin <- input$endMLLikelihoodOUP
                if(!is.numeric(endin)) { endin <- Inf }
                df_info <- ML$set_timeseries_info(NULL,tend=endin,NULL,NULL,NULL,NULL)
                updateNumericInput(session,"endMLLikelihoodOUP",value=df_info$tend)
                end <<- df_info$tend
              }
            }
            resetButton(FALSE)
            beginInput(FALSE)
            endInput(FALSE)
            rho <- lnL_params[1]
            mu <- lnL_params[2]
            sigma <- lnL_params[3]
            if(!is.numeric(rho) | !is.numeric(mu) | is.na(rho) | is.na(mu))
            {
              series <- ML$get_timeseries()
              m <- nrow(series)
              rho <- 0
              mu <- 0
              sigma <- 0
              for(i in 1:(m-1)) { sigma <- sigma+(series[i+1,2]-series[i,2])^2/(series[i+1,1]-series[i,1]) }
              sigma <- (sigma/(m-1))^0.5
            }
            else if(!is.numeric(sigma) | is.na(sigma))
            {
              series <- ML$get_timeseries()
              m <- nrow(series)
              rho <- 9999
              mu <- 0
              for(i in 1:(m-1)) { mu <- mu+series[i+1,2] }
              mu <- mu/(m-1)
              sigma <- 0
              for(i in 1:(m-1)) { sigma <- sigma+(series[i+1,2]-mu)^2 }
              sigma <- (sigma/(m-1))^0.5
            }
            if(rho < 0) { rho <- 0 }
            ML$set_oup_params(rho=rho,mu=mu,sigma=sigma)
            lnL_params[1] <<- rho
            lnL_params[2] <<- mu
            lnL_params[3] <<- sigma
            LRT_params[1] <<- rho
            LRT_params[2] <<- mu
            LRT_params[3] <<- sigma
            updateNumericInput(session,"rhoMLLikelihoodOUP",value=rho)
            updateNumericInput(session,"muMLLikelihoodOUP",value=mu)
            updateNumericInput(session,"sigmaMLLikelihoodOUP",value=sigma)
            lnL <- ML$LogLikelihood()[[1]]
            lnL <- format(lnL,digits=6)
            output$lnLMLLikelihoodOUP <- renderUI({
              HTML(paste(sep="",
                "<table align='center'>
                   <tr style='border-bottom: 1px solid grey;'>
                     <th style='text-align: right; padding: 2px 8px 2px 8px;'>LnL</th>
                   </tr>
                   <tr>
                     <td style='text-align: right; padding: 8px;'>",lnL,"</td>
                   </tr>
                 </table>"
              ))
            })
            output$plotlyMLLikelihoodOUP <- renderPlotly({ ML$PlotEstimates() })
          }) %>% bindEvent(input$resetMLLikelihoodOUP,input$plotMLLikelihoodOUP)
          # User clicks i ----
          observe({
            htmlname <- paste(sep="",htmlpath,input$filesMLLikelihoodOUP,".html")
            if(!file.exists(htmlname)) { htmlname <- paste(sep="",htmlpath,"MyData.html") }
            rawtext <- read_html(htmlname)
            halo <- input$filesMLLikelihoodOUP
            body <- html_element(rawtext,"body")
            gen1 <- html_children(body)
            gen2 <- html_children(gen1)
            m <- length(gen2)-2
            soul <- as.character(gen2[2:m])
            style <- "<style>h2 { font-size: 120% } h3 { font-size: 110% }</style>"
            showModal(modalDialog(
              title=div(img(src="Roar32x32.png"),halo),
              HTML(paste(sep="",style,soul)),
              easyClose = TRUE,
              footer = modalButton("Close"),
              size = "l"
            ))
          }) %>% bindEvent(input$fileinfoMLLikelihoodOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # User clicks info ----
          observe({
            showModal(modalDialog(
              title=div(img(src="Roar32x32.png"),"Log Likelihood"),
              HTML("The Likelihood is the joint probability of observing a time-series as a random sample.  For numerical reasons, the natural logarithm of the Likelihood, or Log Likelihood, is calculated instead.  The Log Likelihood can be maximized to estimate the parameters of the Ornstein-Uhlenbeck Process.  It can be calculated for a restricted set of parameters to test hypotheses.  An example would compare two sets of parameters by calculating their Log Likelihoods and conducting a Likelihood Ratio Test.<br><br>
              &emsp;&emsp;The R6 method:<br>
              &emsp;&emsp;&emsp;LogLikelihood(<i>rho,mu,sigma,tau,z</i>)<br>
              &emsp;&emsp;with arguments:<br>
              &emsp;&emsp;&emsp;<i>rho</i> is the random rate parameter;<br>
              &emsp;&emsp;&emsp;<i>mu</i> is the random location parameter;<br>
              &emsp;&emsp;&emsp;<i>sigma</i> is the random scale parameter;<br>
              &emsp;&emsp;&emsp;<i>tau</i> are the fixed times;<br>
              &emsp;&emsp;&emsp;<i>z</i> are the fixed states;<br>
              &emsp;&emsp;returns:<br>
              <table style='margin-left: 60px;'>
                <tr>
                  <td style='border-top: solid silver; border-bottom: solid silver; border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'>ln<i>L</i></td>
                  <td style='border-top: solid silver; border-bottom: solid silver; border-right: solid silver'>&nbsp;</td>
                  <td style='padding-left: 2px;'>;</td>
                </tr>
              </table>
              &emsp;&emsp;where:<br>
              &emsp;&emsp;&emsp;ln<i>L</i> is the Log Likelihood."),
              easyClose = TRUE,
              footer = modalButton("Close")
            ))
          }) %>% bindEvent(input$infoMLLikelihoodOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
        # Estimates ----
        else if(input$navMLOUP == "MLEstimatesOUP")
        {
          # define set function ----
          FromR6toUI <- function()
          {
# message("Estimates FromR6toUI")
            if(first)
            {
# message("first")
              df <<- read.csv(uploadpath,fileEncoding="UTF-8-BOM")
              framenames <<- colnames(df)
              dname[5] <<- uploadname
              tname[5] <<- framenames[1]
              sname[5] <<- framenames[2]
              nrows <<- nrow(df)
              ncols <<- ncol(df)
              nfirst <<- df[1,1]
              nlast <<- df[nrows,1]
              series <- ML$set_timeseries(df=df,taucol=1,zcol=2)
              Ixend <<- nrow(series)
              end <<- series[Ixend,1]
              if(Ixend > 200) { Ixbeg <<- Ixend-200 }
              else { Ixbeg <<- 1 }
              beg <<- series[Ixbeg,1]
              ML$set_timeseries_info(tbeg=beg,tend=end,dataname=dname[5],timename=tname[5],statename=sname[5],NULL)
# df_info <- ML$get_timeseries_info()
# message(df_info$tbeg,df_info$tend,df_info$dataname,df_info$timename,df_info$statename)
              updateSelectInput(session,"filesMLEstimatesOUP",choices=filelist,selected=dname[5])
              updateSelectInput(session,"timeMLEstimatesOUP",choices=framenames,selected=tname[5])
              updateSelectInput(session,"stateMLEstimatesOUP",choices=framenames,selected=sname[5])
              first <<- FALSE
              initialize[3] <<- FALSE
              bounce[1] <<- 1
              bounce[2] <<- 1
              bounce[3] <<- 1
            }
            else if(initialize[3])
            {
# message("initialize")
              df_info <- ML$get_timeseries_info()
              dname[5] <<- df_info[[5]]
              tname[5] <<- df_info[[6]]
              sname[5] <<- df_info[[7]]
              updateSelectInput(session,"filesMLEstimatesOUP",choices=filelist,selected=dname[5])
              updateSelectInput(session,"timeMLEstimatesOUP",choices=framenames,selected=tname[5])
              updateSelectInput(session,"stateMLEstimatesOUP",choices=framenames,selected=sname[5])
              initialize[3] <<- FALSE
              bounce[1] <<- 1
              bounce[2] <<- 1
              bounce[3] <<- 1
            }
            else
            {
# message("else")
              df_info <- ML$get_timeseries_info()
              dataname <- df_info[[5]]
              timename <- df_info[[6]]
              statename <- df_info[[7]]
              if(dataname != dname[5] | timename != tname[5] | statename != sname[5])
              {
# message(dataname,", ",dname[5],", ",timename,", ",tname[5],", ",statename,", ",sname[5])
                updateSelectInput(session,"filesMLEstimatesOUP",choices=filelist,selected=dataname)
                updateSelectInput(session,"timeMLEstimatesOUP",choices=framenames,selected=timename)
                updateSelectInput(session,"stateMLEstimatesOUP",choices=framenames,selected=statename)
                bounce[1] <<- 1
                bounce[2] <<- 1
                bounce[3] <<- 1
                dname[5] <<- dataname
                tname[5] <<- timename
                sname[5] <<- statename
              }
            }
            updateNumericInput(session,"begMLEstimatesOUP",value=beg)
            updateNumericInput(session,"endMLEstimatesOUP",value=end)
          }
          # initialize ----
          FromR6toUI()
          # select ----
          observe({
# message("Estimates observe file")
# message(bounce[1])
            if(bounce[1] > 0) { bounce[1] <<- bounce[1]-1 }
            else
            {
              if(dname[5] != input$filesMLEstimatesOUP)
              {
# message(dname[5],", ",input$filesMLEstimatesOUP)
                dname[5] <<- input$filesMLEstimatesOUP
                if(dname[5] == uploadname) { filepath <- uploadpath }
                else { filepath <- paste(sep="",datapath,input$filesMLEstimatesOUP,".csv")  }
                df <<- read.csv(filepath,fileEncoding="UTF-8-BOM")
                framenames <<- colnames(df)
                tname[5] <<- framenames[1]
                sname[5] <<- framenames[2]
                nrows <<- nrow(df)
                ncols <<- ncol(df)
                nfirst <<- df[1,1]
                nlast <<- df[nrows,1]
                series <- ML$set_timeseries(df=df,taucol=1,zcol=2)
                Ixend <<- nrow(series)
                end <<- series[Ixend,1]
                if(Ixend > 200) { Ixbeg <<- Ixend-200 }
                else { Ixbeg <<- 1 }
                beg <<- series[Ixbeg,1]
                ML$set_timeseries_info(tbeg=beg,tend=end,dataname=dname[5],timename=tname[5],statename=sname[5])
# df_info <- ML$get_timeseries_info()
# message(df_info$tbeg,df_info$tend,df_info$dataname,df_info$timename,df_info$statename)
                updateSelectInput(session,"timeMLEstimatesOUP",choices=framenames,selected=tname[5])
                updateSelectInput(session,"stateMLEstimatesOUP",choices=framenames,selected=sname[5])
                updateNumericInput(session,"begMLEstimatesOUP",value=beg)
                updateNumericInput(session,"endMLEstimatesOUP",value=end)
                lnL_params[1] <<- 0
                lnL_params[2] <<- 0
                lnL_params[3] <<- 0
                LRT_params[1] <<- NA
                LRT_params[2] <<- NA
                LRT_params[3] <<- NA
                bounce[2] <<- bounce[2]+1
                bounce[3] <<- bounce[3]+1
              }
            }
          }) %>% bindEvent(input$filesMLEstimatesOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
# message("Estimates observe time")
# message(bounce[2])
            if(bounce[2] > 0) { bounce[2] <<- bounce[2]-1 }
            else
            {
              if(tname[5] != input$timeMLEstimatesOUP)
              {
# message(tname[5],", ",input$timeMLEstimatesOUP)
                tname[5] <<- input$timeMLEstimatesOUP
                taucol <- match(tname[5],framenames)
                zcol <- match(sname[5],framenames)
                series <- ML$set_timeseries(df=df,taucol=taucol,zcol=zcol)
                Ixend <<- nrow(series)
                end <<- series[Ixend,1]
                if(Ixend > 200) { Ixbeg <<- Ixend-200 }
                else { Ixbeg <<- 1 }
                beg <<- series[Ixbeg,1]
                ML$set_timeseries_info(tbeg=beg,tend=end,dataname=dname[5],timename=tname[5],statename=sname[5],NULL)
# df_info <- ML$get_timeseries_info()
# message(df_info$tbeg,df_info$tend,df_info$dataname,df_info$timename,df_info$statename)
                updateNumericInput(session,"begMLEstimatesOUP",value=beg)
                updateNumericInput(session,"endMLEstimatesOUP",value=end)
                lnL_params[1] <<- 0
                lnL_params[2] <<- 0
                lnL_params[3] <<- 0
                LRT_params[1] <<- NA
                LRT_params[2] <<- NA
                LRT_params[3] <<- NA
              }
            }
          }) %>% bindEvent(input$timeMLEstimatesOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
# message("Estimates observe state")
# message(bounce[3])
            if(bounce[3] > 0) { bounce[3] <<- bounce[3]-1 }
            else
            {
              if(sname[5] != input$stateMLEstimatesOUP)
              {
# message(sname[5],", ",input$stateMLEstimatesOUP)
                sname[5] <<- input$stateMLEstimatesOUP
                taucol <- match(tname[5],framenames)
                zcol <- match(sname[5],framenames)
                series <- ML$set_timeseries(df=df,taucol=taucol,zcol=zcol)
                Ixend <<- nrow(series)
                end <<- series[Ixend,1]
                if(Ixend > 200) { Ixbeg <<- Ixend-200 }
                else { Ixbeg <<- 1 }
                beg <<- series[Ixbeg,1]
                ML$set_timeseries_info(tbeg=beg,tend=end,dataname=dname[5],timename=tname[5],statename=sname[5],NULL)
# df_info <- ML$get_timeseries_info()
# message(df_info$tbeg,df_info$tend,df_info$dataname,df_info$timename,df_info$statename)
                lnL_params[1] <<- 0
                lnL_params[2] <<- 0
                lnL_params[3] <<- 0
                LRT_params[1] <<- NA
                LRT_params[2] <<- NA
                LRT_params[3] <<- NA
              }
            }
          }) %>% bindEvent(input$stateMLEstimatesOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # observe reset, begin and end ----
          resetButton <- reactiveVal(FALSE)
          beginInput <- reactiveVal(FALSE)
          endInput <- reactiveVal(FALSE)
          observe({
            resetButton(TRUE)
          }) %>% bindEvent(input$resetMLEstimatesOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            beginInput(TRUE)
          }) %>% bindEvent(input$begMLEstimatesOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            endInput(TRUE)
          }) %>% bindEvent(input$endMLEstimatesOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # go ----
          observe({
            if(resetButton())
            {
              begin <- -Inf
              endin <- Inf
              timeseries_info <- ML$set_timeseries_info(tbeg=begin,tend=endin,NULL,NULL,NULL,NULL)
              updateNumericInput(session,"begMLEstimatesOUP",value=timeseries_info$tbeg)
              updateNumericInput(session,"endMLEstimatesOUP",value=timeseries_info$tend)
              beg <<- timeseries_info$tbeg
              end <<- timeseries_info$tend
            }
            else
            {
              if(beginInput())
              {
                begin <- input$begMLEstimatesOUP
                if(!is.numeric(begin)) { begin <- -Inf }
                df_info <- ML$set_timeseries_info(tbeg=begin,NULL,NULL,NULL,NULL,NULL)
                updateNumericInput(session,"begMLEstimatesOUP",value=df_info$tbeg)
                beg <<- df_info$tbeg
              }
              if(endInput())
              {
                endin <- input$endMLEstimatesOUP
                if(!is.numeric(endin)) { endin <- Inf }
                df_info <- ML$set_timeseries_info(NULL,tend=endin,NULL,NULL,NULL,NULL)
                updateNumericInput(session,"endMLEstimatesOUP",value=df_info$tend)
                end <<- df_info$tend
              }
            }
            resetButton(FALSE)
            beginInput(FALSE)
            endInput(FALSE)
            est <- ML$Estimates(plotit=FALSE)
            rho <- format(est[[1]],digits=6)
            mu <- format(est[[2]],digits=6)
            sigma <- format(est[[3]],digits=6)
            lnL <- format(est[[4]],digits=6)
            k <- est[[5]]
            alpha <- format(est[[6]],digits=6)
            m1 <- est[[7]]
            lnL_params[1] <<- est[[1]]
            lnL_params[2] <<- est[[2]]
            lnL_params[3] <<- est[[3]]
            output$paramMLEstimatesOUP <- renderUI({
              HTML(paste(sep="",
                "<table align='center'>
                  <tr style='border-bottom: 1px solid grey;'>
                    <th></th>
                    <th style='text-align: right; padding: 2px 6px 2px 8px;'>rho</th>
                    <th style='text-align: right; padding: 2px 6px 2px 6px;'>mu</th>
                    <th style='text-align: right; padding: 2px 6px 2px 6px;'>sigma</th>
                    <th style='text-align: right; padding: 2px 6px 2px 6px;'>LnL</th>
                    <th style='text-align: right; padding: 2px 6px 2px 6px;'>k</th>
                    <th style='text-align: right; padding: 2px 6px 2px 6px;'>alpha</th>
                    <th style='text-align: right; padding: 2px 8px 2px 6px;'>m-1</th>
                  </tr>
                  <tr>
                    <td style='text-align: right; padding: 8px 6px 8px 8px;'><b>Unrestricted:</b></td>
                    <td style='text-align: right; padding: 8px 6px 8px 6px;'>",rho,"</td>
                    <td style='text-align: right; padding: 8px 6px 8px 6px;'>",mu,"</td>
                    <td style='text-align: right; padding: 8px 6px 8px 6px;'>",sigma,"</td>
                    <td style='text-align: right; padding: 8px 6px 8px 6px;'>",lnL,"</td>
                    <td style='text-align: right; padding: 8px 6px 8px 6px;'>",k,"</td>
                    <td style='text-align: right; padding: 8px 6px 8px 6px;'>",alpha,"</td>
                    <td style='text-align: right; padding: 8px 8px 8px 6px;'>",m1,"</td>
                  </tr>
                </table>"
              ))
            })
            output$plotlyMLEstimatesOUP <- renderPlotly({ ML$PlotEstimates() })
          }) %>% bindEvent(input$resetMLEstimatesOUP,input$plotMLEstimatesOUP)
          # User clicks i ----
          observe({
            htmlname <- paste(sep="",htmlpath,input$filesMLEstimatesOUP,".html")
            if(!file.exists(htmlname)) { htmlname <- paste(sep="",htmlpath,"MyData.html") }
            rawtext <- read_html(htmlname)
            halo <- input$filesMLEstimatesOUP
            body <- html_element(rawtext,"body")
            gen1 <- html_children(body)
            gen2 <- html_children(gen1)
            m <- length(gen2)-2
            soul <- as.character(gen2[2:m])
            style <- "<style>h2 { font-size: 120% } h3 { font-size: 110% }</style>"
            showModal(modalDialog(
              title=div(img(src="Roar32x32.png"),halo),
              HTML(paste(sep="",style,soul)),
              easyClose = TRUE,
              footer = modalButton("Close"),
              size = "l"
            ))
          }) %>% bindEvent(input$fileinfoMLEstimatesOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # User clicks info ----
          observe({
            showModal(modalDialog(
              title=div(img(src="Roar32x32.png"),"Estimates"),
              HTML("Maximum Likelihood Estimation finds the rate, location and scale parameters of the Ornstein-Uhlenbeck Process which maximize the Log Likelihood.  Some or all the parameters can be fixed to constants and other parameters re-estimated.  This gives the Restricted Log Likelihood, which must be less than the Unrestricted Log Likelihood.  The probability distribution of a Log Likelihood is identified by parameter <i>&alpha;</i>, where <i>&alpha;</i>=0.5 for a <i>&chi;</i><sup>2</sup> distribution, <i>&alpha;</i>=1 for an Erlang distribution and 0.5&lt;<i>&alpha;</i>&lt;1 for a Gamma distribution.<br><br>
              &emsp;&emsp;The R6 method:<br>
              &emsp;&emsp;&emsp;Estimates(<i>tau,z,rhor,mur,sigmar,rhos,mus,sigmas</i>)<br>
              &emsp;&emsp;with arguments:<br>
              &emsp;&emsp;&emsp;<i>tau</i> are the fixed times;<br>
              &emsp;&emsp;&emsp;<i>z</i> are the fixed states;<br>
              &emsp;&emsp;and optional arguments:<br>
              &emsp;&emsp;&emsp;<i>rhor</i> is a constant for the rate parameter;<br>
              &emsp;&emsp;&emsp;<i>mur</i> is a constant for the location parameter;<br>
              &emsp;&emsp;&emsp;<i>sigmar</i> is a constant for the scale parameter;<br>
              &emsp;&emsp;&emsp;<i>rhos</i> is a starting value for the rate parameter;<br>
              &emsp;&emsp;&emsp;<i>mus</i> is a starting value for the location parameter;<br>
              &emsp;&emsp;&emsp;<i>sigmas</i> is a starting value for the scale parameter;<br>
              &emsp;&emsp;returns:<br>
              <table style='float: left; margin-left: 60px;'>
                <tr>
                  <td style='border-top: solid silver; border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'><i>rhohat</i></td>
                  <td style='border-top: solid silver; border-right: solid silver'>&nbsp;</td>
                </tr>
                <tr>
                  <td style='border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'><i>muhat</i></td>
                  <td style='border-right: solid silver'>&nbsp;</td>
                </tr>
                <tr>
                  <td style='border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'><i>sigmahat</i></td>
                  <td style='border-right: solid silver'>&nbsp;</td>
                </tr>
                <tr>
                  <td style='border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'>ln<i>Lhat</i></td>
                  <td style='border-right: solid silver'>&nbsp;</td>
                </tr>
                <tr>
                  <td style='border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'><i>ku</i></td>
                  <td style='border-right: solid silver'>&nbsp;</td>
                </tr>
                <tr>
                  <td style='border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'><i>alphau</i></td>
                  <td style='border-right: solid silver'>&nbsp;</td>
                </tr>
                <tr>
                  <td style='border-bottom: solid silver; border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'><i>m</i>-1</td>
                  <td style='border-bottom: solid silver; border-right: solid silver'>&nbsp;</td>
                </tr>
              </table>
              <table style='float: left; margin-left: 10px; margin-right: 10px;'>
                <tr>
                  <td>&nbsp;</td>
                </tr>
                <tr>
                  <td>&nbsp;</td>
                </tr>
                <tr>
                  <td>or</td>
                </tr>
              </table>
              <table>
                <tr>
                  <td style='border-top: solid silver; border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'><i>rhobar</i></td>
                  <td style='padding: 0px 4px 0px 4px;'><i>or</i></td>
                  <td style='padding: 0px 4px 0px 4px;'><i>rhor</i></td>
                  <td style='border-top: solid silver; border-right: solid silver'>&nbsp;</td>
                </tr>
                <tr>
                  <td style='border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'><i>mubar</i></td>
                  <td style='padding: 0px 4px 0px 4px;'><i>or</i></td>
                  <td style='padding: 0px 4px 0px 4px;'><i>mur</i></td>
                  <td style='border-right: solid silver'>&nbsp;</td>
                </tr>
                <tr>
                  <td style='border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'><i>sigmabar</i></td>
                  <td style='padding: 0px 4px 0px 4px;'><i>or</i></td>
                  <td style='padding: 0px 4px 0px 4px;'><i>sigmar</i></td>
                  <td style='border-right: solid silver'>&nbsp;</td>
                </tr>
                <tr>
                  <td style='border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'>ln<i>Lbar</i></td>
                  <td style='padding: 0px 4px 0px 4px;'><i></i></td>
                  <td style='padding: 0px 4px 0px 4px;'><i></i></td>
                  <td style='border-right: solid silver'>&nbsp;</td>
                </tr>
                <tr>
                  <td style='border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'><i>kr</i></td>
                  <td style='padding: 0px 4px 0px 4px;'><i></i></td>
                  <td style='padding: 0px 4px 0px 4px;'><i></i></td>
                  <td style='border-right: solid silver'>&nbsp;</td>
                </tr>
                <tr>
                  <td style='border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'><i>alphar</i></td>
                  <td style='padding: 0px 4px 0px 4px;'><i></i></td>
                  <td style='padding: 0px 4px 0px 4px;'><i></i></td>
                  <td style='border-right: solid silver'>&nbsp;</td>
                </tr>
                <tr>
                  <td style='border-bottom: solid silver; border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 4px;'><i>m</i>-1</td>
                  <td style='padding: 0px 4px 0px 4px;'><i></i></td>
                  <td style='padding: 0px 4px 0px 4px;'><i></i></td>
                  <td style='border-bottom: solid silver; border-right: solid silver'>&nbsp;</td>
                  <td style='padding-left: 2px;'>;</td>
                </tr>
              </table>
              &emsp;&emsp;where:<br>
              &emsp;&emsp;&emsp;<i>rhohat</i>, <i>muhat</i> and <i>sigmahat</i> are estimates with no restrictions;<br>
              &emsp;&emsp;&emsp;ln<i>Lhat</i> is the maximized unrestricted Log Likelihood;<br>
              &emsp;&emsp;&emsp;<i>ku</i> is the number of parameters before restrictions;<br>
              &emsp;&emsp;&emsp;<i>alphau</i> identifies the distribution of <i>ln</i>Lhat;<br>
              &emsp;&emsp;&emsp;<i>rhobar</i>, <i>mubar</i> and <i>sigmabar</i> are estimates with other paramerts restricted;<br>
              &emsp;&emsp;&emsp;ln<i>Lbar</i> is the maximized restricted Log Likelihood;<br>
              &emsp;&emsp;&emsp;<i>kr</i> is the number of estimated parameters after restrictions;<br>
              &emsp;&emsp;&emsp;<i>alphar</i> identifies the distribution of <i>ln</i>Lbar;<br>
              &emsp;&emsp;&emsp;<i>m</i>-1 is the number of observations."),
              easyClose = TRUE,
              footer = modalButton("Close"),
              size = "l"
            ))
          }) %>% bindEvent(input$infoMLEstimatesOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
        # Goodness-of-Fit ----
        else if(input$navMLOUP == "MLGoodnessOUP")
        {
          # define go function ----
          Go <- function()
          {
            est <- ML$Estimates(plotit=FALSE)
            goods <- ML$GoodnessOfFit()
            rho <- format(est[[1]],digits=6)
            mu <- format(est[[2]],digits=6)
            sigma <- format(est[[3]],digits=6)
            lnL <- format(est[[4]],digits=6)
            k <- est[[5]]
            alpha <- format(est[[6]],digits=6)
            m1 <- est[[7]]
            inv <- format(goods[[1]],digits=6)
            sbm <- format(goods[[2]],digits=6)
            lnL_params[1] <<- est[[1]]
            lnL_params[2] <<- est[[2]]
            lnL_params[3] <<- est[[3]]
            output$paramMLGoodnessOUP <- renderUI({
              HTML(paste(sep="",
                "<table align='center'>
                  <tr style='border-bottom: 1px solid grey;'>
                    <th></th>
                    <th style='text-align: right; padding: 2px 6px 2px 8px;'>rho</th>
                    <th style='text-align: right; padding: 2px 6px 2px 6px;'>mu</th>
                    <th style='text-align: right; padding: 2px 6px 2px 6px;'>sigma</th>
                    <th style='text-align: right; padding: 2px 6px 2px 6px;'>LnL</th>
                    <th style='text-align: right; padding: 2px 6px 2px 6px;'>k</th>
                    <th style='text-align: right; padding: 2px 6px 2px 6px;'>alpha</th>
                    <th style='text-align: right; padding: 2px 8px 2px 6px;'>m-1</th>
                  </tr>
                  <tr>
                    <td style='text-align: right; padding: 8px 6px 8px 8px;'><b>Unrestricted</b></td>
                    <td style='text-align: right; padding: 8px 6px 8px 6px;'>",rho,"</td>
                    <td style='text-align: right; padding: 8px 6px 8px 6px;'>",mu,"</td>
                    <td style='text-align: right; padding: 8px 6px 8px 6px;'>",sigma,"</td>
                    <td style='text-align: right; padding: 8px 6px 8px 6px;'>",lnL,"</td>
                    <td style='text-align: right; padding: 8px 6px 8px 6px;'>",k,"</td>
                    <td style='text-align: right; padding: 8px 6px 8px 6px;'>",alpha,"</td>
                    <td style='text-align: right; padding: 8px 8px 8px 6px;'>",m1,"</td>
                  </tr>
                </table>"
              ))
            })
            output$goodsMLGoodnessOUP <- renderUI({
              HTML(paste(sep="",
                "<table align='center'>
                  <tr style='border-bottom: 1px solid grey;'>
                    <th></th>
                    <th style='text-align: right; padding: 6px;'>Invariant</th>
                    <th style='text-align: right; padding: 6px;'>Scaled BM</th>
                  </tr>
                  <tr>
                    <td style='text-align: right; padding: 6px;'><i>R</i>&hairsp;<sup>2</sup></td>
                    <td style='text-align: right; padding: 6px;'>",inv[1],"</td>
                    <td style='text-align: right; padding: 6px;'>",sbm[1],"</td>
                  </tr>
                  <tr style='border-bottom: 1px solid grey;'>
                    <td style='text-align: right; padding: 6px;'>1-<i>P</i></td>
                    <td style='text-align: right; padding: 6px;'>",inv[2],"</td>
                    <td style='text-align: right; padding: 6px;'>",sbm[2],"</td>
                  </tr>
                </table>"
              ))
            })
          }
          # define set function ----
          FromR6toUI <- function()
          {
# message("Goodness FromR6toUI")
            if(first)
            {
# message("first")
              df <<- read.csv(uploadpath,fileEncoding="UTF-8-BOM")
              framenames <<- colnames(df)
              dname[6] <<- uploadname
              tname[6] <<- framenames[1]
              sname[6] <<- framenames[2]
              nrows <<- nrow(df)
              ncols <<- ncol(df)
              nfirst <<- df[1,1]
              nlast <<- df[nrows,1]
              series <- ML$set_timeseries(df=df,taucol=1,zcol=2)
              Ixend <<- nrow(series)
              end <<- series[Ixend,1]
              if(Ixend > 200) { Ixbeg <<- Ixend-200 }
              else { Ixbeg <<- 1 }
              beg <<- series[Ixbeg,1]
              ML$set_timeseries_info(tbeg=beg,tend=end,dataname=dname[6],timename=tname[6],statename=sname[6],NULL)
# df_info <- ML$get_timeseries_info()
# message(df_info$tbeg,df_info$tend,df_info$dataname,df_info$timename,df_info$statename)
              updateSelectInput(session,"filesMLGoodnessOUP",choices=filelist,selected=dname[6])
              updateSelectInput(session,"timeMLGoodnessOUP",choices=framenames,selected=tname[6])
              updateSelectInput(session,"stateMLGoodnessOUP",choices=framenames,selected=sname[6])
              first <<- FALSE
              initialize[4] <<- FALSE
              bounce[1] <<- 1
              bounce[2] <<- 1
              bounce[3] <<- 1
            }
            else if(initialize[4])
            {
# message("initialize")
              df_info <- ML$get_timeseries_info()
              dname[6] <<- df_info[[5]]
              tname[6] <<- df_info[[6]]
              sname[6] <<- df_info[[7]]
              updateSelectInput(session,"filesMLGoodnessOUP",choices=filelist,selected=dname[6])
              updateSelectInput(session,"timeMLGoodnessOUP",choices=framenames,selected=tname[6])
              updateSelectInput(session,"stateMLGoodnessOUP",choices=framenames,selected=sname[6])
              initialize[4] <<- FALSE
              bounce[1] <<- 1
              bounce[2] <<- 1
              bounce[3] <<- 1
            }
            else
            {
# message("else")
              df_info <- ML$get_timeseries_info()
              dataname <- df_info[[5]]
              timename <- df_info[[6]]
              statename <- df_info[[7]]
              if(dataname != dname[6] | timename != tname[6] | statename != sname[6])
              {
# message(dataname,", ",dname[6],", ",timename,", ",tname[6],", ",statename,", ",sname[6])
                updateSelectInput(session,"filesMLGoodnessOUP",choices=filelist,selected=dataname)
                updateSelectInput(session,"timeMLGoodnessOUP",choices=framenames,selected=timename)
                updateSelectInput(session,"stateMLGoodnessOUP",choices=framenames,selected=statename)
                updateNumericInput(session,"rhorMLGoodnessOUP",value=NA)
                bounce[1] <<- 1
                bounce[2] <<- 1
                bounce[3] <<- 1
                dname[6] <<- dataname
                tname[6] <<- timename
                sname[6] <<- statename
              }
            }
          }
          # initialize ----
          FromR6toUI()
          Go()
          # select ----
          observe({
# message("Goodness observe file")
# message(bounce[1])
            if(bounce[1] > 0) { bounce[1] <<- bounce[1]-1 }
            else
            {
              if(dname[6] != input$filesMLGoodnessOUP)
              {
# message(dname[6],", ",input$filesMLGoodnessOUP)
                dname[6] <<- input$filesMLGoodnessOUP
                if(dname[6] == uploadname) { filepath <- uploadpath }
                else { filepath <- paste(sep="",datapath,input$filesMLGoodnessOUP,".csv")  }
                df <<- read.csv(filepath,fileEncoding="UTF-8-BOM")
                framenames <<- colnames(df)
                tname[6] <<- framenames[1]
                sname[6] <<- framenames[2]
                nrows <<- nrow(df)
                ncols <<- ncol(df)
                nfirst <<- df[1,1]
                nlast <<- df[nrows,1]
                series <- ML$set_timeseries(df=df,taucol=1,zcol=2)
                Ixend <<- nrow(series)
                end <<- series[Ixend,1]
                if(Ixend > 200) { Ixbeg <<- Ixend-200 }
                else { Ixbeg <<- 1 }
                beg <<- series[Ixbeg,1]
                ML$set_timeseries_info(tbeg=beg,tend=end,dataname=dname[6],timename=tname[6],statename=sname[6])
# df_info <- ML$get_timeseries_info()
# message(df_info$tbeg,df_info$tend,df_info$dataname,df_info$timename,df_info$statename)
                updateSelectInput(session,"timeMLGoodnessOUP",choices=framenames,selected=tname[6])
                updateSelectInput(session,"stateMLGoodnessOUP",choices=framenames,selected=sname[6])
                lnL_params[1] <<- 0
                lnL_params[2] <<- 0
                lnL_params[3] <<- 0
                LRT_params[1] <<- NA
                LRT_params[2] <<- NA
                LRT_params[3] <<- NA
                bounce[2] <<- bounce[2]+1
                bounce[3] <<- bounce[3]+1
              }
            }
          }) %>% bindEvent(input$filesMLGoodnessOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
# message("Goodness observe time")
# message(bounce[2])
            if(bounce[2] > 0) { bounce[2] <<- bounce[2]-1 }
            else
            {
              if(tname[6] != input$timeMLGoodnessOUP)
              {
# message(tname[6],", ",input$timeMLGoodnessOUP)
                tname[6] <<- input$timeMLGoodnessOUP
                taucol <- match(tname[6],framenames)
                zcol <- match(sname[6],framenames)
                series <- ML$set_timeseries(df=df,taucol=taucol,zcol=zcol)
                Ixend <<- nrow(series)
                end <<- series[Ixend,1]
                if(Ixend > 200) { Ixbeg <<- Ixend-200 }
                else { Ixbeg <<- 1 }
                beg <<- series[Ixbeg,1]
                ML$set_timeseries_info(tbeg=beg,tend=end,dataname=dname[6],timename=tname[6],statename=sname[6],NULL)
# df_info <- ML$get_timeseries_info()
# message(df_info$tbeg,df_info$tend,df_info$dataname,df_info$timename,df_info$statename)
                updateNumericInput(session,"begMLGoodnessOUP",value=beg)
                updateNumericInput(session,"endMLGoodnessOUP",value=end)
                lnL_params[1] <<- 0
                lnL_params[2] <<- 0
                lnL_params[3] <<- 0
                LRT_params[1] <<- NA
                LRT_params[2] <<- NA
                LRT_params[3] <<- NA
              }
            }
          }) %>% bindEvent(input$timeMLGoodnessOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
# message("Goodness observe state")
# message(bounce[3])
            if(bounce[3] > 0) { bounce[3] <<- bounce[3]-1 }
            else
            {
              if(sname[6] != input$stateMLGoodnessOUP)
              {
# message(sname[6],", ",input$stateMLGoodnessOUP)
                sname[6] <<- input$stateMLGoodnessOUP
                taucol <- match(tname[6],framenames)
                zcol <- match(sname[6],framenames)
                series <- ML$set_timeseries(df=df,taucol=taucol,zcol=zcol)
                Ixend <<- nrow(series)
                end <<- series[Ixend,1]
                if(Ixend > 200) { Ixbeg <<- Ixend-200 }
                else { Ixbeg <<- 1 }
                beg <<- series[Ixbeg,1]
                ML$set_timeseries_info(tbeg=beg,tend=end,dataname=dname[6],timename=tname[6],statename=sname[6],NULL)
# df_info <- ML$get_timeseries_info()
# message(df_info$tbeg,df_info$tend,df_info$dataname,df_info$timename,df_info$statename)
                lnL_params[1] <<- 0
                lnL_params[2] <<- 0
                lnL_params[3] <<- 0
                LRT_params[1] <<- NA
                LRT_params[2] <<- NA
                LRT_params[3] <<- NA
              }
            }
          }) %>% bindEvent(input$stateMLGoodnessOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # go ----
          observe({
            Go()
          }) %>% bindEvent(input$plotMLGoodnessOUP)
          # User clicks i ----
          observe({
            htmlname <- paste(sep="",htmlpath,input$filesMLGoodnessOUP,".html")
            if(!file.exists(htmlname)) { htmlname <- paste(sep="",htmlpath,"MyData.html") }
            rawtext <- read_html(htmlname)
            halo <- input$filesMLGoodnessOUP
            body <- html_element(rawtext,"body")
            gen1 <- html_children(body)
            gen2 <- html_children(gen1)
            m <- length(gen2)-2
            soul <- as.character(gen2[2:m])
            style <- "<style>h2 { font-size: 120% } h3 { font-size: 110% }</style>"
            showModal(modalDialog(
              title=div(img(src="Roar32x32.png"),halo),
              HTML(paste(sep="",style,soul)),
              easyClose = TRUE,
              footer = modalButton("Close"),
              size = "l"
            ))
          }) %>% bindEvent(input$fileinfoMLGoodnessOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # User clicks info ----
          observe({
            showModal(modalDialog(
              title=div(img(src="Roar32x32.png"),"Goodness-of-Fit"),
              HTML("Goodness of Fit compares the Log Likelihood of the estimated parameters to the Invariant Log Likelihood and to the Log Likelihood of Scaled Brownian Motion.  Comparing with the Invariant Likelihood tests the null hypothesis H<sub>0</sub>:  'the Ornstein-Uhlenbeck Process has converged'.  Comparing with the Likelihood of Scaled Brownian Motion tests the null hypothesis H<sub>0</sub>:  'the Ornstein-Uhlenbeck does not converge'.  Goodness of Fit is summarized by two Pseudo-<i>R</i>&hairsp;<sup>2</sup> statistics and two probabilities.  A null hypothesis is rejected if the <i>R</i>&hairsp;<sup>2</sup> is at least 0.5 and the probability is small.<br><br>
              &emsp;&emsp;The R6 method:<br>
              &emsp;&emsp;&emsp;GoodnessOfFit(<i>rho,mu,sigma,tau,z</i>)<br>
              &emsp;&emsp;with arguments:<br>
              &emsp;&emsp;&emsp;<i>rho</i> is the random rate parameter;<br>
              &emsp;&emsp;&emsp;<i>mu</i> is the random location parameter;<br>
              &emsp;&emsp;&emsp;<i>sigma</i> is the random scale parameter;<br>
              &emsp;&emsp;&emsp;<i>tau</i> are the fixed times;<br>
              &emsp;&emsp;&emsp;<i>z</i> are the fixed states;<br>
              &emsp;&emsp;returns:<br>
              <table style='margin-left: 60px;'>
                <tr>
                  <th>&nbsp;</th>
                  <th style='padding: 0px 4px 0px 4px;'>Invariant</th>
                  <th style='padding: 0px 4px 0px 4px;'>Scaled BM</th>
                  <th>&nbsp;</th>
                </tr>
                <tr>
                  <td style='border-top: solid silver; border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 30px;'><i>R</i>&hairsp;<sup>2</sup><sub>&infin;</sub></td>
                  <td style='padding: 0px 4px 0px 30px;'><i>R</i>&hairsp;<sup>2</sup><sub>0</sub></td>
                  <td style='border-top: solid silver; border-right: solid silver'>&nbsp;</td>
                </tr>
                <tr>
                  <td style='border-bottom: solid silver; border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 30px;'>1-<i>P</i><sub>&infin;</sub></td>
                  <td style='padding: 0px 4px 0px 30px;'>1-<i>P</i><sub>0</sub></td>
                  <td style='border-bottom: solid silver; border-right: solid silver'>&nbsp;</td>
                  <td style='padding-left: 2px;'>;</td>
                </tr>
              </table>
              &emsp;&emsp;where:<br>
              &emsp;&emsp;&emsp;<i>R</i>&hairsp;<sup>2</sup><sub>&infin;</sub> and <i>R</i>&hairsp;<sup>2</sup><sub>0</sub> are Pseudo-<i>R</i>&hairsp;<sup>2</sup> statistics;<br>
              &emsp;&emsp;&emsp;1-<i>P</i><sub>&infin;</sub> is the right-tail of a <i>Chi</i>&hairsp;<sup>2</sup> probability;<br>
              &emsp;&emsp;&emsp;1-<i>P</i><sub>0</sub> is the right-tail of an Erlang probability."),
              easyClose = TRUE,
              footer = modalButton("Close")
            ))
          }) %>% bindEvent(input$infoMLGoodnessOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
        # Likelihood Ratio Test ----
        else if(input$navMLOUP == "MLRatioOUP")
        {
          # define set function ----
          FromR6toUI <- function()
          {
# message("Ratio FromR6toUI")
            if(first)
            {
# message("first")
              df <<- read.csv(uploadpath,fileEncoding="UTF-8-BOM")
              framenames <<- colnames(df)
              dname[7] <<- uploadname
              tname[7] <<- framenames[1]
              sname[7] <<- framenames[2]
              nrows <<- nrow(df)
              ncols <<- ncol(df)
              nfirst <<- df[1,1]
              nlast <<- df[nrows,1]
              series <- ML$set_timeseries(df=df,taucol=1,zcol=2)
              Ixend <<- nrow(series)
              end <<- series[Ixend,1]
              if(Ixend > 200) { Ixbeg <<- Ixend-200 }
              else { Ixbeg <<- 1 }
              beg <<- series[Ixbeg,1]
              ML$set_timeseries_info(tbeg=beg,tend=end,dataname=dname[7],timename=tname[7],statename=sname[7],NULL)
# df_info <- ML$get_timeseries_info()
# message(df_info$tbeg,df_info$tend,df_info$dataname,df_info$timename,df_info$statename)
              updateSelectInput(session,"filesMLRatioOUP",choices=filelist,selected=dname[7])
              updateSelectInput(session,"timeMLRatioOUP",choices=framenames,selected=tname[7])
              updateSelectInput(session,"stateMLRatioOUP",choices=framenames,selected=sname[7])
              first <<- FALSE
              initialize[5] <<- FALSE
              bounce[1] <<- 1
              bounce[2] <<- 1
              bounce[3] <<- 1
            }
            else if(initialize[5])
            {
# message("initialize")
              df_info <- ML$get_timeseries_info()
              dname[7] <<- df_info[[5]]
              tname[7] <<- df_info[[6]]
              sname[7] <<- df_info[[7]]
              updateSelectInput(session,"filesMLRatioOUP",choices=filelist,selected=dname[7])
              updateSelectInput(session,"timeMLRatioOUP",choices=framenames,selected=tname[7])
              updateSelectInput(session,"stateMLRatioOUP",choices=framenames,selected=sname[7])
              initialize[5] <<- FALSE
              bounce[1] <<- 1
              bounce[2] <<- 1
              bounce[3] <<- 1
            }
            else
            {
# message("else")
              df_info <- ML$get_timeseries_info()
              dataname <- df_info[[5]]
              timename <- df_info[[6]]
              statename <- df_info[[7]]
              if(dataname != dname[7] | timename != tname[7] | statename != sname[7])
              {
# message(dataname,", ",dname[7],", ",timename,", ",tname[7],", ",statename,", ",sname[7])
                updateSelectInput(session,"filesMLRatioOUP",choices=filelist,selected=dataname)
                updateSelectInput(session,"timeMLRatioOUP",choices=framenames,selected=timename)
                updateSelectInput(session,"stateMLRatioOUP",choices=framenames,selected=statename)
                bounce[1] <<- 1
                bounce[2] <<- 1
                bounce[3] <<- 1
                dname[7] <<- dataname
                tname[7] <<- timename
                sname[7] <<- statename
              }
              updateNumericInput(session,"rhorMLRatioOUP",value=LRT_params[1])
              updateNumericInput(session,"murMLRatioOUP",value=LRT_params[2])
              updateNumericInput(session,"sigmarMLRatioOUP",value=LRT_params[3])
            }
          }
          # initialize ----
          FromR6toUI()
          # select ----
          observe({
# message("Ratio observe file")
# message(bounce[1])
            if(bounce[1] > 0) { bounce[1] <<- bounce[1]-1 }
            else
            {
              if(dname[7] != input$filesMLRatioOUP)
              {
# message(dname[7],", ",input$filesMLRatioOUP)
                dname[7] <<- input$filesMLRatioOUP
                if(dname[7] == uploadname) { filepath <- uploadpath }
                else { filepath <- paste(sep="",datapath,input$filesMLRatioOUP,".csv")  }
                df <<- read.csv(filepath,fileEncoding="UTF-8-BOM")
                framenames <<- colnames(df)
                tname[7] <<- framenames[1]
                sname[7] <<- framenames[2]
                nrows <<- nrow(df)
                ncols <<- ncol(df)
                nfirst <<- df[1,1]
                nlast <<- df[nrows,1]
                series <- ML$set_timeseries(df=df,taucol=1,zcol=2)
                Ixend <<- nrow(series)
                end <<- series[Ixend,1]
                if(Ixend > 200) { Ixbeg <<- Ixend-200 }
                else { Ixbeg <<- 1 }
                beg <<- series[Ixbeg,1]
                ML$set_timeseries_info(tbeg=beg,tend=end,dataname=dname[7],timename=tname[7],statename=sname[7])
# df_info <- ML$get_timeseries_info()
# message(df_info$tbeg,df_info$tend,df_info$dataname,df_info$timename,df_info$statename)
                updateSelectInput(session,"timeMLRatioOUP",choices=framenames,selected=tname[7])
                updateSelectInput(session,"stateMLRatioOUP",choices=framenames,selected=sname[7])
                updateNumericInput(session,"rhorMLRatioOUP",value=NA)
                updateNumericInput(session,"murMLRatioOUP",value=NA)
                updateNumericInput(session,"sigmarMLRatioOUP",value=NA)
                lnL_params[1] <<- 0
                lnL_params[2] <<- 0
                lnL_params[3] <<- 0
                LRT_params[1] <<- NA
                LRT_params[2] <<- NA
                LRT_params[3] <<- NA
                bounce[2] <<- bounce[2]+1
                bounce[3] <<- bounce[3]+1
              }
            }
          }) %>% bindEvent(input$filesMLRatioOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
# message("Ratio observe time")
# message(bounce[2])
            if(bounce[2] > 0) { bounce[2] <<- bounce[2]-1 }
            else
            {
              if(tname[7] != input$timeMLRatioOUP)
              {
# message(tname[7],", ",input$timeMLRatioOUP)
                tname[7] <<- input$timeMLRatioOUP
                taucol <- match(tname[7],framenames)
                zcol <- match(sname[7],framenames)
                series <- ML$set_timeseries(df=df,taucol=taucol,zcol=zcol)
                Ixend <<- nrow(series)
                end <<- series[Ixend,1]
                if(Ixend > 200) { Ixbeg <<- Ixend-200 }
                else { Ixbeg <<- 1 }
                beg <<- series[Ixbeg,1]
                ML$set_timeseries_info(tbeg=beg,tend=end,dataname=dname[7],timename=tname[7],statename=sname[7],NULL)
# df_info <- ML$get_timeseries_info()
# message(df_info$tbeg,df_info$tend,df_info$dataname,df_info$timename,df_info$statename)
                updateNumericInput(session,"rhorMLRatioOUP",value=NA)
                updateNumericInput(session,"murMLRatioOUP",value=NA)
                updateNumericInput(session,"sigmarMLRatioOUP",value=NA)
                lnL_params[1] <<- 0
                lnL_params[2] <<- 0
                lnL_params[3] <<- 0
                LRT_params[1] <<- NA
                LRT_params[2] <<- NA
                LRT_params[3] <<- NA
              }
            }
          }) %>% bindEvent(input$timeMLRatioOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
# message("Ratio observe state")
# message(bounce[3])
            if(bounce[3] > 0) { bounce[3] <<- bounce[3]-1 }
            else
            {
              if(sname[7] != input$stateMLRatioOUP)
              {
# message(sname[7],", ",input$stateMLRatioOUP)
                sname[7] <<- input$stateMLRatioOUP
                taucol <- match(tname[7],framenames)
                zcol <- match(sname[7],framenames)
                series <- ML$set_timeseries(df=df,taucol=taucol,zcol=zcol)
                Ixend <<- nrow(series)
                end <<- series[Ixend,1]
                if(Ixend > 200) { Ixbeg <<- Ixend-200 }
                else { Ixbeg <<- 1 }
                beg <<- series[Ixbeg,1]
                ML$set_timeseries_info(tbeg=beg,tend=end,dataname=dname[7],timename=tname[7],statename=sname[7],NULL)
# df_info <- ML$get_timeseries_info()
# message(df_info$tbeg,df_info$tend,df_info$dataname,df_info$timename,df_info$statename)
                updateNumericInput(session,"rhorMLRatioOUP",value=NA)
                updateNumericInput(session,"murMLRatioOUP",value=NA)
                updateNumericInput(session,"sigmarMLRatioOUP",value=NA)
                lnL_params[1] <<- 0
                lnL_params[2] <<- 0
                lnL_params[3] <<- 0
                LRT_params[1] <<- NA
                LRT_params[2] <<- NA
                LRT_params[3] <<- NA
              }
            }
          }) %>% bindEvent(input$stateMLRatioOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # Observe reset, rhor, mur and sigmar ----
          resetButton <- reactiveVal(FALSE)
          rhorInput <- reactiveVal(FALSE)
          murInput <- reactiveVal(FALSE)
          sigmarInput <- reactiveVal(FALSE)
          observe({
            resetButton(TRUE)
          }) %>% bindEvent(input$resetMLRatioOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            rhorInput(TRUE)
          }) %>% bindEvent(input$rhorMLRatioOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            murInput(TRUE)
          }) %>% bindEvent(input$murMLRatioOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            sigmarInput(TRUE)
          }) %>% bindEvent(input$sigmarMLRatioOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # go ----
          observe({
            # message("go")
            if(resetButton())
            {
              LRT_params[1] <<- NA
              LRT_params[2] <<- NA
              LRT_params[3] <<- NA
              updateNumericInput(session,"rhorMLRatioOUP",value=NA)
              updateNumericInput(session,"murMLRatioOUP",value=NA)
              updateNumericInput(session,"sigmarMLRatioOUP",value=NA)
            }
            else
            {
              if(rhorInput())
              {
                rhor <- input$rhorMLRatioOUP
                if(!is.numeric(rhor)) { rhor <- NA }
                else if(rhor < 0) { rhor <- 0 }
                LRT_params[1] <<- rhor
              }
              if(murInput())
              {
                mur <- input$murMLRatioOUP
                if(!is.numeric(mur)) { mur <- NA }
                else if(mur < 0) { mur <- 0 }
                LRT_params[2] <<- mur
              }
              if(sigmarInput())
              {
                sigmar <- input$sigmarMLRatioOUP
                if(!is.numeric(sigmar)) { sigmar <- NA }
                else if(sigmar < 0) { sigmar <- 0 }
                LRT_params[3] <<- sigmar
              }
            }
            resetButton(FALSE)
            rhorInput(FALSE)
            murInput(FALSE)
            sigmarInput(FALSE)
            rhor <- LRT_params[1]
            mur <- LRT_params[2]
            sigmar <- LRT_params[3]
            if(is.na(rhor)) { rhor <- NULL }
            if(is.na(mur)) { mur <- NULL }
            if(is.na(sigmar)) { sigmar <- NULL }
            unrestr <- ML$Estimates(plotit=FALSE)
            ML$set_oup_params_restr(rhor=rhor,mur=mur,sigmar=sigmar)
            restr <- ML$Estimates(rhor=rhor,mur=mur,sigmar=sigmar,plotit=FALSE)
            ML$set_oup_stats(lnLu=unrestr[[4]],lnLr=restr[[4]],alphar=restr[[6]],m1=restr[[7]])
            ratio <- ML$LikelihoodRatioTest()
            rhou <- format(unrestr[[1]],digits=6)
            muu <- format(unrestr[[2]],digits=6)
            sigmau <- format(unrestr[[3]],digits=6)
            lnLu <- format(unrestr[[4]],digits=6)
            ku <- unrestr[[5]]
            alphau <- format(unrestr[[6]],digits=6)
            m1u <- unrestr[[7]]
            rhor <- format(restr[[1]],digits=6)
            mur <- format(restr[[2]],digits=6)
            sigmar <- format(restr[[3]],digits=6)
            lnLr <- format(restr[[4]],digits=6)
            kr <- restr[[5]]
            alphar <- format(restr[[6]],digits=6)
            m1r <- restr[[7]]
            r2 <- format(ratio[[1]],digits=6)
            pval <- format(ratio[[2]],digits=6)
            lnL_params[1] <<- restr[[1]]
            lnL_params[2] <<- restr[[2]]
            lnL_params[3] <<- restr[[3]]
            output$paramMLRatioOUP <- renderUI({
              HTML(paste(sep="",
                "<table align='center'>
                  <tr style='border-bottom: 1px solid grey;'>
                    <th></th>
                    <th style='text-align: right; padding: 2px 6px 2px 8px;'>rho</th>
                    <th style='text-align: right; padding: 2px 6px 2px 6px;'>mu</th>
                    <th style='text-align: right; padding: 2px 6px 2px 6px;'>sigma</th>
                    <th style='text-align: right; padding: 2px 6px 2px 6px;'>LnL</th>
                    <th style='text-align: right; padding: 2px 6px 2px 6px;'>k</th>
                    <th style='text-align: right; padding: 2px 6px 2px 6px;'>alpha</th>
                    <th style='text-align: right; padding: 2px 8px 2px 6px;'>m-1</th>
                  </tr>
                  <tr>
                    <td style='text-align: right; padding: 8px 6px 2px 8px;'><b>Unrestricted</b></td>
                    <td style='text-align: right; padding: 8px 6px 2px 6px;'>",rhou,"</td>
                    <td style='text-align: right; padding: 8px 6px 2px 6px;'>",muu,"</td>
                    <td style='text-align: right; padding: 8px 6px 2px 6px;'>",sigmau,"</td>
                    <td style='text-align: right; padding: 8px 6px 2px 6px;'>",lnLu,"</td>
                    <td style='text-align: right; padding: 8px 6px 2px 6px;'>",ku,"</td>
                    <td style='text-align: right; padding: 8px 6px 2px 6px;'>",alphau,"</td>
                    <td style='text-align: right; padding: 8px 8px 2px 6px;'>",m1u,"</td>
                  </tr>
                  <tr>
                    <td style='text-align: right; padding: 2px 6px 8px 8px;'><b>Restricted</b></td>
                    <td style='text-align: right; padding: 2px 6px 8px 6px;'>",rhor,"</td>
                    <td style='text-align: right; padding: 2px 6px 8px 6px;'>",mur,"</td>
                    <td style='text-align: right; padding: 2px 6px 8px 6px;'>",sigmar,"</td>
                    <td style='text-align: right; padding: 2px 6px 8px 6px;'>",lnLr,"</td>
                    <td style='text-align: right; padding: 2px 6px 8px 6px;'>",kr,"</td>
                    <td style='text-align: right; padding: 2px 6px 8px 6px;'>",alphar,"</td>
                    <td style='text-align: right; padding: 2px 8px 8px 6px;'>",m1r,"</td>
                  </tr>
                </table>"
              ))
            })
            output$ratioMLRatioOUP <- renderUI({
              HTML(paste(sep="",
                "<table align='center'>
                  <tr style='border-bottom: 1px solid grey;'>
                    <th></th>
                    <th style='text-align: right; padding: 6px;'>Restricted</th>
                  </tr>
                  <tr>
                    <td style='text-align: right; padding: 6px;'><i>R</i>&hairsp;<sup>2</sup></td>
                    <td style='text-align: right; padding: 6px;'>",r2,"</td>
                  </tr>
                  <tr style='border-bottom: 1px solid grey;'>
                    <td style='text-align: right; padding: 6px;'>1-<i>P</i></td>
                    <td style='text-align: right; padding: 6px;'>",pval,"</td>
                  </tr>
                </table>"
              ))
            })
          }) %>% bindEvent(input$resetMLRatioOUP,input$plotMLRatioOUP)
          # User clicks i ----
          observe({
            htmlname <- paste(sep="",htmlpath,input$filesMLRatioOUP,".html")
            if(!file.exists(htmlname)) { htmlname <- paste(sep="",htmlpath,"MyData.html") }
            rawtext <- read_html(htmlname)
            halo <- input$filesMLRatioOUP
            body <- html_element(rawtext,"body")
            gen1 <- html_children(body)
            gen2 <- html_children(gen1)
            m <- length(gen2)-2
            soul <- as.character(gen2[2:m])
            style <- "<style>h2 { font-size: 120% } h3 { font-size: 110% }</style>"
            showModal(modalDialog(
              title=div(img(src="Roar32x32.png"),halo),
              HTML(paste(sep="",style,soul)),
              easyClose = TRUE,
              footer = modalButton("Close"),
              size = "l"
            ))
          }) %>% bindEvent(input$fileinfoMLRatioOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # User clicks info ----
          observe({
            showModal(modalDialog(
              title=div(img(src="Roar32x32.png"),"Likelihood Ratio Test"),
              HTML("Hypothesis tests are constrained optimization with restrictions placed on the parameters.  One form of the null hypothesis is H<sub>0</sub>:  'parameters can take their restricted values'. The alternate hypothesis is H<sub>1</sub>:  'parameters cannot take their restricted values'.  A Likelihood Ratio Test rejects the null hypothesis if the restricted Log Likelihood is significantly smaller than the unrestricted Log Likelihood.  A null hypothesis is rejected if the <i>R</i>&hairsp;<sup>2</sup> is at least 0.5 and the probability is small.<br><br>
              &emsp;&emsp;The R6 method:<br>
              &emsp;&emsp;&emsp;LikelihoodRatioTest(ln<i>Lu,</i>ln<i>Lr,alphar,m1</i>)<br>
              &emsp;&emsp;with arguments:<br>
              &emsp;&emsp;&emsp;ln<i>Lu</i> is the unrestricted Log Likelihood;<br>
              &emsp;&emsp;&emsp;ln<i>Lr</i> is the restricted Log Likelihood;<br>
              &emsp;&emsp;&emsp;<i>alphar</i> identifies the distribution of ln<i>Lr</i>;<br>
              &emsp;&emsp;&emsp;<i>m1</i> is <i>m</i>-1, the number of observations;<br>
              &emsp;&emsp;returns:<br>
              <table style='margin-left: 60px;'>
                <tr>
                  <th>&nbsp;</th>
                  <th style='padding: 0px 4px 0px 4px;'>Restricted</th>
                  <th>&nbsp;</th>
                </tr>
                <tr>
                  <td style='border-top: solid silver; border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 30px;'><i>R</i>&hairsp;<sup>2</sup></td>
                  <td style='border-top: solid silver; border-right: solid silver'>&nbsp;</td>
                </tr>
                <tr>
                  <td style='border-bottom: solid silver; border-left: solid silver'>&nbsp;</td>
                  <td style='padding: 0px 4px 0px 30px;'>1-<i>P</i></td>
                  <td style='border-bottom: solid silver; border-right: solid silver'>&nbsp;</td>
                  <td style='padding-left: 2px;'>;</td>
                </tr>
              </table>
              &emsp;&emsp;where:<br>
              &emsp;&emsp;&emsp;<i>R</i>&hairsp;<sup>2</sup> is a Pseudo-<i>R</i>&hairsp;<sup>2</sup> statistic;<br>
              &emsp;&emsp;&emsp;1-<i>P</i> is the right-tail of a Gamma probability."),
              easyClose = TRUE,
              footer = modalButton("Close")
            ))
          }) %>% bindEvent(input$infoMLRatioOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
      })
    }
    else if(input$navBar == "tabAboutOUP")
    {
      # tabAboutOUP ----
      showModal(modalDialog(
        title = div(img(src="Roar64x64.png"),"Real Options for Adoption and Resilience"),
        HTML("Description:  R Shiny implementation of the R6 objects, OUProcess, Analytical, FiniteDifference, MaximumLikelihood and MonteCarlo&mdash;a complete set of functions for maximum likelihood estimation and the calculation of probabilities, option prices, decision thresholds, visiting times, first passage times and more&mdash;everything for a real options analysis.<br><br>
            Version:  1.3.5.0 (stochastic process.modules.help.bugs)<br>
            License:  GPLv3<br><br>
            Author:  Greg Hertzler<br>
            email:  ghertzlerau@gmail.com<br>
            Roles:  author, creator<br>
            ORCID:  0000-0003-3123-7898<br><br>
            Author:  Tim Capon<br>
            email:  Tim.Capon@csiro.au<br>
            Roles:  contributor<br><br>
            This project was supported by:<br>
            &mdash;resources and expertise provided by CSIRO IMT Scientific Computing;<br>
            &mdash;resources provided by CSIRO Environment.
        "),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    }
    else if(input$navBar == "tabLicenseOUP")
    {
      # tabLicenseOUP ----
      showModal(modalDialog(
        title = "GNU General Public Licence version 3 (GPLv3)",
        HTML("This software is copyright (c) Greg Hertzler<br><br>
            Except where otherwise indicated, the copyright holder grants you a licence to the Software on the terms of the GNU General Public Licence version 3 (GPLv3), distributed at: http://www.gnu.org/licenses/gpl.html.
          "),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    }
    # end ----
  })
})
