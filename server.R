library(shiny)
library(bslib)
library(plotly)
library(tools)
library(rvest)
library(GregsOUPR6)

# server
shinyServer(function(input,output,session) {
session$setCurrentTheme(bs_theme(bootswatch="spacelab",bg="#ddeeff",fg="#001122",success="#11aa88"))

# instantiate objects ----
OUP <- OUProcess$new(session=session)
A <- OUP$get_Analytical()
FD <- OUP$get_FiniteDifference()
ML <- OUP$get_MaximumLikelihood()
MC <- OUP$get_MonteCarlo()
A$set_oup_params(rho=0.5,mu=15,sigma=15)
A$set_plot_info(opaque=0.0,walls=FALSE,floor=FALSE,labels=FALSE)
A$set_flags(plotit=FALSE,copyit=TRUE)
# globals for help ----
ouppath <- system.file(package="GregsOUPR6")
datapath <- paste(sep="",ouppath,"/data/")
htmlpath <- datapath
# global for Regime tab ----
regime <- 0
# global for Finite Difference tab ----
skipinput <- FALSE
populate <- c(TRUE,TRUE,TRUE,TRUE)
# globals for Maximum Likelihood and Data tabs ----
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
firsttab <- TRUE
initialize <- c(TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE)
dname <- c("data","data","data","data","data","data","data")
tname <- c("tau","tau","tau","tau","tau","tau","tau")
sname <- c("z","z","z","z","z","z","z")
# globals for buttons ----
RObtns <- matrix(0,5,8) # reset,undn,unup,sync,axes,plot,left(other),rght
Abtns <- matrix(0,17,7) # undn,unup,sync,axes,plot,left,rght
FDbtns <- matrix(0,6,6) # undn,unup,axes,plot,left,rght
MLbtns <- matrix(0,2,2) # reset,plot
MCbtns <- matrix(0,17,8) # reset,undn,unup,sync,axes,plot,left,rght
# globals and reactive for modal dialogs ----
ibutton <- ""
infobutton <- ""
infotoggle <- reactiveVal(FALSE)

# axis sequence function ----
axissequence <- function(from,to,by)
{
  if(is.numeric(from) & is.numeric(to))
  {
    if(from > to)
    {
      temp <- from
      from <- to
      to <- temp
    }
    if(is.numeric(by))
    {
      if(by < 0) { by <- -by }
      if(by > (to-from)/100) { by <- (to-from)/100 }
      else if(by < (to-from)/1000) { by <- (to-from)/1000 }
    }
    else { by <- (to-from)/100 }
    axisseq <- seq(from=from,to=to,by=by)
  }
  else if(is.numeric(from) & is.numeric(by))
  {
    if(by < 0)
    {
      by <- -by
      to <- from
      from <- to-100*by
    }
    else { to <- from+100*by }
    axisseq <- seq(from=from,to=to,by=by)
  }
  else if(is.numeric(to) & is.numeric(by))
  {
    if(by > 0)
    {
      from <- to
      to <- from+100*by
    }
    else
    {
      by <- -by
      from <- to-100*by
    }
    axisseq <- seq(from=from,to=to,by=by)
  }
  else { axisseq <- NULL }
  return(axisseq)
}
# clipboard event ----
  observeEvent(input$clipboardDeny, {
    A$set_flags(copyit=FALSE)
    showNotification(paste0("Clipboard blocked for ",input$clipboardDeny,".  To allow, close this tab, reset in browser's clipboard settings and open ROAR again."),id="clip",duration=10)
  })
# tab events ----
  observeEvent(input$navBar,{
    # navBar ----
    if(input$navBar == "tabROOUP")
    {
      observeEvent(input$navROOUP,{
        # Data ----
        if(input$navROOUP == "RODataOUP")
        {
          # define set/get functions ----
          FromR6toUI <- function()
          {
            # message("FromR6toUI")
            timeseries_info <- ML$get_timeseries_info()
            beg <- timeseries_info[[1]]
            end <- timeseries_info[[2]]
            isolate({
              updateNumericInput(session,"begRODataOUP",value=beg)
              updateNumericInput(session,"endRODataOUP",value=end)
            })
          }
          FromUItoR6 <- function()
          {
            # message("FromUItoR6")
            isolate({
              beg <- input$begRODataOUP
              end <- input$endRODataOUP
            })
            if(!is.numeric(beg)) { beg <- -Inf }
            if(!is.numeric(end)) { end <- Inf }
            ML$set_timeseries_info(tbeg=beg,tend=end)
          }
          # define data functions ----
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
          DataRead <- function()
          {
            # message("Data DataRead")
            if(firsttab)
            {
              # message("firsttab")
              df <<- utils::read.csv(uploadpath,fileEncoding="UTF-8-BOM")
              framenames <<- colnames(df)
              dname[1] <<- uploadname
              tname[1] <<- framenames[1]
              sname[1] <<- framenames[2]
              nrows <<- nrow(df)
              ncols <<- ncol(df)
              nfirst <<- df[1,1]
              nlast <<- df[nrows,1]
              series <- ML$set_timeseries(df=df,taucol=1,zcol=2)
              Ixend <- nrow(series)
              end <- series[Ixend,1]
              if(Ixend > 200) { Ixbeg <- Ixend-200 }
              else { Ixbeg <- 1 }
              beg <- series[Ixbeg,1]
              ML$set_timeseries_info(tbeg=beg,tend=end,dataname=dname[1],timename=tname[1],statename=sname[1],NULL)
              isolate({
                updateSelectInput(session,"filesRODataOUP",choices=filelist,selected=dname[1])
                updateSelectInput(session,"timeRODataOUP",choices=framenames,selected=tname[1])
                updateSelectInput(session,"stateRODataOUP",choices=framenames,selected=sname[1])
                updateNumericInput(session,"begRODataOUP",value=beg)
                updateNumericInput(session,"endRODataOUP",value=end)
              })
              DataInfo()
              firsttab <<- FALSE
              initialize[6] <<- FALSE
            }
            else if(initialize[6])
            {
              # message("initialize")
              df_info <- ML$get_timeseries_info()
              dname[1] <<- df_info[[3]]
              tname[1] <<- df_info[[4]]
              sname[1] <<- df_info[[5]]
              isolate({
                updateSelectInput(session,"filesRODataOUP",choices=filelist,selected=dname[1])
                updateSelectInput(session,"timeRODataOUP",choices=framenames,selected=tname[1])
                updateSelectInput(session,"stateRODataOUP",choices=framenames,selected=sname[1])
              })
              FromR6toUI()
              DataInfo()
              initialize[6] <<- FALSE
            }
            else
            {
              # message("else")
              df_info <- ML$get_timeseries_info()
              dataname <- df_info[[3]]
              timename <- df_info[[4]]
              statename <- df_info[[5]]
              if(dataname != dname[1] | timename != tname[1] | statename != sname[1])
              {
                isolate({
                  updateSelectInput(session,"filesRODataOUP",choices=filelist,selected=dataname)
                  updateSelectInput(session,"timeRODataOUP",choices=framenames,selected=timename)
                  updateSelectInput(session,"stateRODataOUP",choices=framenames,selected=statename)
                })
                DataInfo()
                dname[1] <<- dataname
                tname[1] <<- timename
                sname[1] <<- statename
              }
              FromR6toUI()
            }
          }
          # initialize ----
          DataRead()
          # select ----
          observe({
            # message("Data observe file")
            if(dname[1] != input$filesRODataOUP)
            {
              dname[1] <<- input$filesRODataOUP
              if(dname[1] == uploadname) { filepath <- uploadpath }
              else { filepath <- paste(sep="",datapath,input$filesRODataOUP,".csv")  }
              df <<- utils::read.csv(filepath,fileEncoding="UTF-8-BOM")
              framenames <<- colnames(df)
              tname[1] <<- framenames[1]
              sname[1] <<- framenames[2]
              nrows <<- nrow(df)
              ncols <<- ncol(df)
              nfirst <<- df[1,1]
              nlast <<- df[nrows,1]
              series <- ML$set_timeseries(df=df,taucol=1,zcol=2)
              Ixend <- nrow(series)
              end <- series[Ixend,1]
              if(Ixend > 200) { Ixbeg <- Ixend-200 }
              else { Ixbeg <- 1 }
              beg <- series[Ixbeg,1]
              ML$set_timeseries_info(tbeg=beg,tend=end,dataname=dname[1],timename=tname[1],statename=sname[1],NULL)
              isolate({
                updateSelectInput(session,"timeRODataOUP",choices=framenames,selected=tname[1])
                updateSelectInput(session,"stateRODataOUP",choices=framenames,selected=sname[1])
                updateNumericInput(session,"begRODataOUP",value=beg)
                updateNumericInput(session,"endRODataOUP",value=end)
              })
              ML$set_oup_params(rho=0,mu=0,sigma=0)
              DataInfo()
            }
          }) %>% bindEvent(input$filesRODataOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            # message("Data observe time")
            if(tname[1] != input$timeRODataOUP)
            {
              tname[1] <<- input$timeRODataOUP
              taucol <- match(tname[1],framenames)
              zcol <- match(sname[1],framenames)
              series <- ML$set_timeseries(df=df,taucol=taucol,zcol=zcol)
              Ixend <- nrow(series)
              end <- series[Ixend,1]
              if(Ixend > 200) { Ixbeg <- Ixend-200 }
              else { Ixbeg <- 1 }
              beg <- series[Ixbeg,1]
              ML$set_timeseries_info(tbeg=beg,tend=end,dataname=dname[1],timename=tname[1],statename=sname[1],NULL)
              isolate({
                updateNumericInput(session,"begRODataOUP",value=beg)
                updateNumericInput(session,"endRODataOUP",value=end)
              })
              ML$set_oup_params(rho=0,mu=0,sigma=0)
            }
          }) %>% bindEvent(input$timeRODataOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            # message("Data observe state")
            if(sname[1] != input$stateRODataOUP)
            {
              sname[1] <<- input$stateRODataOUP
              taucol <- match(tname[1],framenames)
              zcol <- match(sname[1],framenames)
              series <- ML$set_timeseries(df=df,taucol=taucol,zcol=zcol)
              Ixend <- nrow(series)
              end <- series[Ixend,1]
              if(Ixend > 200) { Ixbeg <- Ixend-200 }
              else { Ixbeg <- 1 }
              beg <- series[Ixbeg,1]
              ML$set_timeseries_info(tbeg=beg,tend=end,dataname=dname[1],timename=tname[1],statename=sname[1],NULL)
              isolate({
                updateNumericInput(session,"begRODataOUP",value=beg)
                updateNumericInput(session,"endRODataOUP",value=end)
              })
              ML$set_oup_params(rho=0,mu=0,sigma=0)
            }
          }) %>% bindEvent(input$stateRODataOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # upload ----
          observe({
            uploadname <<- file_path_sans_ext(input$filesROUploadOUP$name)
            uploadpath <<- input$filesROUploadOUP$datapath
            filelist[1] <<- uploadname
            firsttab <<- TRUE
            DataRead()
          }) %>% bindEvent(input$filesROUploadOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # user clicks reset or plot (or enter key) ----
          output$plotlyRODataOUP <- renderPlotly({
            # message("render")
            if(input$resetRODataOUP > RObtns[1,1])
            {
              RObtns[1,1] <<- input$resetRODataOUP
              ML$set_timeseries_info(tbeg=-Inf,tend=Inf)
            }
            else if(input$plotRODataOUP > RObtns[1,2])
            {
              RObtns[1,2] <<- input$plotRODataOUP
              FromUItoR6()
            }
            FromR6toUI()
            ML$PlotTimeSeries()
          }) %>% bindEvent(input$resetRODataOUP,input$plotRODataOUP)
          # observe i and info ----
          observe({
            ibutton <<- input$filesRODataOUP
            infobutton <<- ""
            if(infotoggle()) { infotoggle(FALSE) }
            else { infotoggle(TRUE) }
          }) %>% bindEvent(input$fileinfoRODataOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            ibutton <<- ""
            infobutton <<- "infoRODataOUP"
            if(infotoggle()) { infotoggle(FALSE) }
            else { infotoggle(TRUE) }
          }) %>% bindEvent(input$infoRODataOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            removeModal(session)
            updateTabsetPanel(session,"navBar",selected="tabMLOUP")
            updateTabsetPanel(session,"navMLOUP",selected="MLDataOUP")
          }) %>% bindEvent(input$alsoRODataOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
        # Estimates ----
        if(input$navROOUP == "ROEstimatesOUP")
        {
          # define data function ----
          DataRead <- function()
          {
            # message("Estimates DataRead")
            if(firsttab)
            {
              # message("firsttab")
              df <<- utils::read.csv(uploadpath,fileEncoding="UTF-8-BOM")
              framenames <<- colnames(df)
              dname[2] <<- uploadname
              tname[2] <<- framenames[1]
              sname[2] <<- framenames[2]
              nrows <<- nrow(df)
              ncols <<- ncol(df)
              nfirst <<- df[1,1]
              nlast <<- df[nrows,1]
              series <- ML$set_timeseries(df=df,taucol=1,zcol=2)
              Ixend <- nrow(series)
              end <- series[Ixend,1]
              if(Ixend > 200) { Ixbeg <- Ixend-200 }
              else { Ixbeg <- 1 }
              beg <- series[Ixbeg,1]
              ML$set_timeseries_info(tbeg=beg,tend=end,dataname=dname[2],timename=tname[2],statename=sname[2],NULL)
              isolate({
                updateSelectInput(session,"filesROEstimatesOUP",choices=filelist,selected=dname[2])
                updateSelectInput(session,"timeROEstimatesOUP",choices=framenames,selected=tname[2])
                updateSelectInput(session,"stateROEstimatesOUP",choices=framenames,selected=sname[2])
                updateNumericInput(session,"begROEstimatesOUP",value=beg)
                updateNumericInput(session,"endROEstimatesOUP",value=end)
              })
              firsttab <<- FALSE
              initialize[7] <<- FALSE
            }
            else if(initialize[7])
            {
              # message("initialize")
              df_info <- ML$get_timeseries_info()
              dname[2] <<- df_info[[3]]
              tname[2] <<- df_info[[4]]
              sname[2] <<- df_info[[5]]
              isolate({
                updateSelectInput(session,"filesROEstimatesOUP",choices=filelist,selected=dname[2])
                updateSelectInput(session,"timeROEstimatesOUP",choices=framenames,selected=tname[2])
                updateSelectInput(session,"stateROEstimatesOUP",choices=framenames,selected=sname[2])
              })
              FromR6toUI()
              initialize[7] <<- FALSE
            }
            else
            {
              # message("else")
              df_info <- ML$get_timeseries_info()
              dataname <- df_info[[3]]
              timename <- df_info[[4]]
              statename <- df_info[[5]]
              if(dataname != dname[2] | timename != tname[2] | statename != sname[2])
              {
                isolate({
                  updateSelectInput(session,"filesROEstimatesOUP",choices=filelist,selected=dataname)
                  updateSelectInput(session,"timeROEstimatesOUP",choices=framenames,selected=timename)
                  updateSelectInput(session,"stateROEstimatesOUP",choices=framenames,selected=statename)
                })
                ML$set_oup_params(rho=0,mu=0,sigma=0)
                dname[2] <<- dataname
                tname[2] <<- timename
                sname[2] <<- statename
              }
              FromR6toUI()
            }
          }
          # define set/get functions ----
          FromR6toUI <- function()
          {
            # message("FromR6toUI")
            timeseries_info <- ML$get_timeseries_info()
            beg <- timeseries_info[[1]]
            end <- timeseries_info[[2]]
            isolate({
              updateNumericInput(session,"begROEstimatesOUP",value=beg)
              updateNumericInput(session,"endROEstimatesOUP",value=end)
            })
          }
          FromUItoR6 <- function()
          {
            # message("FromUItoR6")
            isolate({
              beg <- input$begROEstimatesOUP
              end <- input$endROEstimatesOUP
            })
            if(!is.numeric(beg)) { beg <- -Inf }
            if(!is.numeric(end)) { end <- Inf }
            ML$set_timeseries_info(tbeg=beg,tend=end)
          }
          # initialize ----
          DataRead()
          # select ----
          observe({
            # message("Estimates observe file")
            if(dname[2] != input$filesROEstimatesOUP)
            {
              dname[2] <<- input$filesROEstimatesOUP
              if(dname[2] == uploadname) { filepath <- uploadpath }
              else { filepath <- paste(sep="",datapath,input$filesROEstimatesOUP,".csv")  }
              df <<- utils::read.csv(filepath,fileEncoding="UTF-8-BOM")
              framenames <<- colnames(df)
              tname[2] <<- framenames[1]
              sname[2] <<- framenames[2]
              nrows <<- nrow(df)
              ncols <<- ncol(df)
              nfirst <<- df[1,1]
              nlast <<- df[nrows,1]
              series <- ML$set_timeseries(df=df,taucol=1,zcol=2)
              Ixend <- nrow(series)
              end <- series[Ixend,1]
              if(Ixend > 200) { Ixbeg <- Ixend-200 }
              else { Ixbeg <- 1 }
              beg <- series[Ixbeg,1]
              ML$set_timeseries_info(tbeg=beg,tend=end,dataname=dname[2],timename=tname[2],statename=sname[2])
              isolate({
                updateSelectInput(session,"timeROEstimatesOUP",choices=framenames,selected=tname[2])
                updateSelectInput(session,"stateROEstimatesOUP",choices=framenames,selected=sname[2])
                updateNumericInput(session,"begROEstimatesOUP",value=beg)
                updateNumericInput(session,"endROEstimatesOUP",value=end)
              })
            }
          }) %>% bindEvent(input$filesROEstimatesOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            # message("Estimates observe time")
            if(tname[2] != input$timeROEstimatesOUP)
            {
              tname[2] <<- input$timeROEstimatesOUP
              taucol <- match(tname[2],framenames)
              zcol <- match(sname[2],framenames)
              series <- ML$set_timeseries(df=df,taucol=taucol,zcol=zcol)
              Ixend <- nrow(series)
              end <- series[Ixend,1]
              if(Ixend > 200) { Ixbeg <- Ixend-200 }
              else { Ixbeg <- 1 }
              beg <- series[Ixbeg,1]
              ML$set_timeseries_info(tbeg=beg,tend=end,dataname=dname[2],timename=tname[2],statename=sname[2],NULL)
              isolate({
                updateNumericInput(session,"begROEstimatesOUP",value=beg)
                updateNumericInput(session,"endROEstimatesOUP",value=end)
              })
            }
          }) %>% bindEvent(input$timeROEstimatesOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            # message("Estimates observe state")
            if(sname[2] != input$stateROEstimatesOUP)
            {
              sname[2] <<- input$stateROEstimatesOUP
              taucol <- match(tname[2],framenames)
              zcol <- match(sname[2],framenames)
              series <- ML$set_timeseries(df=df,taucol=taucol,zcol=zcol)
              Ixend <- nrow(series)
              end <- series[Ixend,1]
              if(Ixend > 200) { Ixbeg <- Ixend-200 }
              else { Ixbeg <- 1 }
              beg <- series[Ixbeg,1]
              ML$set_timeseries_info(tbeg=beg,tend=end,dataname=dname[2],timename=tname[2],statename=sname[2],NULL)
              isolate({
                updateNumericInput(session,"begROEstimatesOUP",value=beg)
                updateNumericInput(session,"endROEstimatesOUP",value=end)
              })
            }
          }) %>% bindEvent(input$stateROEstimatesOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # user clicks reset or plot (or enter key) ----
          observe({
            # message("render")
            if(input$resetROEstimatesOUP > RObtns[2,1])
            {
              RObtns[2,1] <<- input$resetROEstimatesOUP
              ML$set_timeseries_info(tbeg=-Inf,tend=Inf)
            }
            else if(input$plotROEstimatesOUP > RObtns[2,2])
            {
              RObtns[2,2] <<- input$plotROEstimatesOUP
              FromUItoR6()
            }
            FromR6toUI()
            est_u <- ML$Estimates()
            theta_u <- format(est_u,digits=6)
            output$paramROEstimatesOUP <- renderUI({
              HTML(paste(sep="",
                "<table align='center'>
                  <tr>
                    <th></th>
                    <th style='text-align: right; padding: 2px 6px 2px 8px; border-bottom: 1px solid grey;'>rho</th>
                    <th style='text-align: right; padding: 2px 6px 2px 6px; border-bottom: 1px solid grey;'>mu</th>
                    <th style='text-align: right; padding: 2px 6px 2px 6px; border-bottom: 1px solid grey;'>sigma</th>
                    <th style='text-align: right; padding: 2px 6px 2px 6px; border-bottom: 1px solid grey;'>alpha</th>
                  </tr>
                  <tr>
                    <td style='text-align: right; padding: 8px 6px 8px 8px;'><b>Parameters</b></td>
                    <td style='text-align: right; padding: 8px 6px 8px 6px;'>",theta_u[[1]],"</td>
                    <td style='text-align: right; padding: 8px 6px 8px 6px;'>",theta_u[[2]],"</td>
                    <td style='text-align: right; padding: 8px 6px 8px 6px;'>",theta_u[[3]],"</td>
                    <td style='text-align: right; padding: 8px 6px 8px 6px;'>",theta_u[[6]],"</td>
                  </tr>
                </table>"
              ))
            })
            output$plotlyROEstimatesOUP <- renderPlotly({ ML$PlotEstimates() })
          }) %>% bindEvent(input$resetROEstimatesOUP,input$plotROEstimatesOUP)
          # observe i and info ----
          observe({
            ibutton <<- input$filesROEstimatesOUP
            infobutton <<- ""
            if(infotoggle()) { infotoggle(FALSE) }
            else { infotoggle(TRUE) }
          }) %>% bindEvent(input$fileinfoROEstimatesOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            ibutton <<- ""
            infobutton <<- "infoROEstimatesOUP"
            if(infotoggle()) { infotoggle(FALSE) }
            else { infotoggle(TRUE) }
          }) %>% bindEvent(input$infoROEstimatesOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            removeModal(session)
            updateTabsetPanel(session,"navBar",selected="tabMLOUP")
            updateTabsetPanel(session,"navMLOUP",selected="MLEstimatesOUP")
          }) %>% bindEvent(input$alsoROEstimatesOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
        # Regime ----
        else if(input$navROOUP == "RORegimeOUP")
        {
          # define set/get functions ----
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
            isolate({
              updateNumericInput(session,"rhoRORegimeOUP",value=rho)
              updateNumericInput(session,"muRORegimeOUP",value=mu)
              updateNumericInput(session,"sigmaRORegimeOUP",value=sigma)
              updateNumericInput(session,"xFromRORegimeOUP",value=xFrom)
              updateNumericInput(session,"xToRORegimeOUP",value=xTo)
              updateNumericInput(session,"xByRORegimeOUP",value=xBy)
              updateNumericInput(session,"yRORegimeOUP",value=y)
              updateNumericInput(session,"rRORegimeOUP",value=r)
              updateNumericInput(session,"phiRORegimeOUP",value=phi)
              if(regime > 0)
              {
                if(phi > 0)
                {
                  updateNumericInput(session,"bRORegimeOUP",label="b",value=b)
                  updateNumericInput(session,"cRORegimeOUP",label="~",value=c)
                }
                else
                {
                  updateNumericInput(session,"bRORegimeOUP",label="~",value=b)
                  updateNumericInput(session,"cRORegimeOUP",label="c",value=c)
                }
              }
              else
              {
                updateNumericInput(session,"bRORegimeOUP",label="b",value=b)
                updateNumericInput(session,"cRORegimeOUP",label="c",value=c)
              }
            })
          }
          FromUItoR6 <- function()
          {
            # Get from UI ----
            isolate({
              rho <- input$rhoRORegimeOUP
              mu <- input$muRORegimeOUP
              sigma <- input$sigmaRORegimeOUP
              xFrom <- input$xFromRORegimeOUP
              xTo <- input$xToRORegimeOUP
              xBy <- input$xByRORegimeOUP
              y <- input$yRORegimeOUP
              r <- input$rRORegimeOUP
              phi <- input$phiRORegimeOUP
              b <- input$bRORegimeOUP
              c <- input$cRORegimeOUP
            })
            if(!is.numeric(rho)) { rho <- 0 }
            else if(rho < 0) { rho <- 0 }
            if(!is.numeric(mu)) { mu <- 0 }
            if(!is.numeric(sigma)) { sigma <- 0 }
            x <- axissequence(xFrom,xTo,xBy)
            if(!is.numeric(y)) { y <- 0 }
            if(!is.numeric(r)) { r <- 0 }
            if(!is.numeric(phi)) { phi <- -1 }
            else if(phi <= 0) { phi <- -1 }
            else if(phi > 0) { phi <- 1 }
            if(!is.numeric(b)) { b <- 0 }
            if(!is.numeric(c)) { c <- 0 }
            # Set to OUP ----
            A$set_oup_params(rho=rho,mu=mu,sigma=sigma)
            A$set_x_stoch_args(x=x,y=y,r=r,phi=phi,b=b,c=c)
          }
          # initialize ----
          FromR6toUI()
          # user clicks clear or save ----
          observe({
            FromUItoR6()
            A$undo_clear()
            showNotification("argument set 1 out of 1.",id="ROundo",duration=2)
          }) %>% bindEvent(input$clearRORegimeOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            FromUItoR6()
            n <- A$undo_save()
            showNotification(paste("argument set ",n," out of ",n,"."),id="ROundo",duration=2)
          }) %>% bindEvent(input$saveRORegimeOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # user clicks undn, unup, sync, axes, plot (or enter key) or other ----
          output$plotlyRORegimeOUP <- renderPlotly({
            if(input$undnRORegimeOUP > RObtns[3,2])
            {
              RObtns[3,2] <<- input$undnRORegimeOUP
              Ixn <- A$undo_undo()
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="ROundo",duration=2)
            }
            else if(input$unupRORegimeOUP > RObtns[3,3])
            {
              RObtns[3,3] <<- input$unupRORegimeOUP
              Ixn <- A$undo_undo(1)
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="ROundo",duration=2)
            }
            else if(input$syncRORegimeOUP > RObtns[3,4])
            {
              RObtns[3,4] <<- input$syncRORegimeOUP
              FromUItoR6()
              A$sync_zyxt_stoch()
            }
            else if(input$axesRORegimeOUP > RObtns[3,5])
            {
              RObtns[3,5] <<- input$axesRORegimeOUP
              FromUItoR6()
              A$axes_x_stoch()
            }
            else if(input$plotRORegimeOUP > RObtns[3,6])
            {
              RObtns[3,6] <<- input$plotRORegimeOUP
              FromUItoR6()
            }
            else if(input$otherRORegimeOUP > RObtns[3,7])
            {
              regime <<- regime-1
              if(regime < 0) regime <<- 1
              RObtns[3,7] <<- input$otherRORegimeOUP
              FromUItoR6()
            }
            FromR6toUI()
            phi <- A$get_x_stoch_args()[[6]]
            if(regime > 0)
            {
              if(phi > 0) { A$PlotOption(title="Entry Option",type=0) }
              else { A$PlotOption(title="Exit Option",type=0) }
            }
            else
            {
              if(phi > 0) { A$PlotObligation(title="Prohibition",type=0) }
              else { A$PlotObligation(title="Obligation",type=0) }
            }
          }) %>% bindEvent(input$undnRORegimeOUP,input$unupRORegimeOUP,input$syncRORegimeOUP,input$axesRORegimeOUP,input$plotRORegimeOUP,input$otherRORegimeOUP)
          # observe info ----
          observe({
            ibutton <<- ""
            infobutton <<- "infoRORegimeOUP"
            if(infotoggle()) { infotoggle(FALSE) }
            else { infotoggle(TRUE) }
          }) %>% bindEvent(input$infoRORegimeOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            removeModal(session)
            updateTabsetPanel(session,"navBar",selected="tabAOUP")
            updateTabsetPanel(session,"navAOUP",selected="AOptionOUP")
          }) %>% bindEvent(input$alsoRORegimeOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
        # Decision Threshold ----
        else if(input$navROOUP == "RODecisionOUP")
        {
          # define set/get functions ----
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
            isolate({
              updateNumericInput(session,"rhoRODecisionOUP",value=rho)
              updateNumericInput(session,"muRODecisionOUP",value=mu)
              updateNumericInput(session,"sigmaRODecisionOUP",value=sigma)
              updateNumericInput(session,"xFromRODecisionOUP",value=xFrom)
              updateNumericInput(session,"xToRODecisionOUP",value=xTo)
              updateNumericInput(session,"xByRODecisionOUP",value=xBy)
              updateNumericInput(session,"yRODecisionOUP",value=y)
              updateNumericInput(session,"rRODecisionOUP",value=r)
              updateNumericInput(session,"phiRODecisionOUP",value=phi)
              if(phi > 0)
              {
                updateNumericInput(session,"bRODecisionOUP",label="b",value=b)
                updateNumericInput(session,"cRODecisionOUP",label="~",value=c)
              }
              else
              {
                updateNumericInput(session,"bRODecisionOUP",label="~",value=b)
                updateNumericInput(session,"cRODecisionOUP",label="c",value=c)
              }
            })
          }
          FromUItoR6 <- function()
          {
            # Get from UI ----
            isolate({
              rho <- input$rhoRODecisionOUP
              mu <- input$muRODecisionOUP
              sigma <- input$sigmaRODecisionOUP
              xFrom <- input$xFromRODecisionOUP
              xTo <- input$xToRODecisionOUP
              xBy <- input$xByRODecisionOUP
              y <- input$yRODecisionOUP
              r <- input$rRODecisionOUP
              phi <- input$phiRODecisionOUP
              b <- input$bRODecisionOUP
              c <- input$cRODecisionOUP
            })
            if(!is.numeric(rho)) { rho <- 0 }
            else if(rho < 0) { rho <- 0 }
            if(!is.numeric(mu)) { mu <- 0 }
            if(!is.numeric(sigma)) { sigma <- 0 }
            x <- axissequence(xFrom,xTo,xBy)
            if(!is.numeric(y)) { y <- 0 }
            if(!is.numeric(r)) { r <- 0 }
            if(!is.numeric(phi)) { phi <- -1 }
            else if(phi <= 0) { phi <- -1 }
            else if(phi > 0) { phi <- 1 }
            if(!is.numeric(b)) { b <- 0 }
            if(!is.numeric(c)) { c <- 0 }
            # Set to OUP ----
            A$set_oup_params(rho=rho,mu=mu,sigma=sigma)
            A$set_x_stoch_args(x=x,y=y,r=r,phi=phi,b=b,c=c)
          }
          # observe phi ----
          observe({
            if(is.numeric(input$phiRODecisionOUP))
            {
              if(input$phiRODecisionOUP > 0)
              {
                b <- A$get_x_stoch_args()[[7]]
                isolate({
                  updateNumericInput(session,"bcRODecisionOUP",label="b",value=b)
                })
              }
              else
              {
                c <- A$get_x_stoch_args()[[8]]
                isolate({
                  updateNumericInput(session,"bcRODecisionOUP",label="c",value=c)
                })
              }
            }
          }) %>% bindEvent(input$phiRODecisionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # user clicks clear or save ----
          observe({
            FromUItoR6()
            A$undo_clear()
            showNotification("argument set 1 out of 1.",id="ROundo",duration=2)
          }) %>% bindEvent(input$clearRODecisionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            FromUItoR6()
            n <- A$undo_save()
            showNotification(paste("argument set ",n," out of ",n,"."),id="ROundo",duration=2)
          }) %>% bindEvent(input$saveRODecisionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # user clicks undn, unup, sync, axes, plot (or enter key) ----
          output$plotlyRODecisionOUP <- renderPlotly({
            if(input$undnRODecisionOUP > RObtns[4,2])
            {
              RObtns[4,2] <<- input$undnRODecisionOUP
              Ixn <- A$undo_undo()
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="ROundo",duration=2)
            }
            else if(input$unupRODecisionOUP > RObtns[4,3])
            {
              RObtns[4,3] <<- input$unupRODecisionOUP
              Ixn <- A$undo_undo(1)
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="ROundo",duration=2)
            }
            else if(input$syncRODecisionOUP > RObtns[4,4])
            {
              RObtns[4,4] <<- input$syncRODecisionOUP
              FromUItoR6()
              A$sync_zyxt_stoch()
            }
            else if(input$axesRODecisionOUP > RObtns[4,5])
            {
              RObtns[4,5] <<- input$axesRODecisionOUP
              FromUItoR6()
              A$axes_x_stoch()
            }
            else if(input$plotRODecisionOUP > RObtns[4,6])
            {
              RObtns[4,6] <<- input$plotRODecisionOUP
              FromUItoR6()
            }
            FromR6toUI()
            phi <- A$get_x_stoch_args()[[6]]
            if(phi > 0) { A$PlotDecisionThreshold(title="Entry") }
            else {A$PlotDecisionThreshold(title="Exit") }
          }) %>% bindEvent(input$undnRODecisionOUP,input$unupRODecisionOUP,input$syncRODecisionOUP,input$axesRODecisionOUP,input$plotRODecisionOUP)
          # observe info ----
          observe({
            ibutton <<- ""
            infobutton <<- "infoRODecisionOUP"
            if(infotoggle()) { infotoggle(FALSE) }
            else { infotoggle(TRUE) }
          }) %>% bindEvent(input$infoRODecisionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            removeModal(session)
            updateTabsetPanel(session,"navBar",selected="tabAOUP")
            updateTabsetPanel(session,"navAOUP",selected="ADecisionOUP")
          }) %>% bindEvent(input$alsoRODecisionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
        # Passage Time ----
        else if(input$navROOUP == "ROPassageTimeOUP")
        {
          # define set/get functions ----
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
            isolate({
              updateNumericInput(session,"rhoROPassageTimeOUP",value=rho)
              updateNumericInput(session,"muROPassageTimeOUP",value=mu)
              updateNumericInput(session,"sigmaROPassageTimeOUP",value=sigma)
              updateNumericInput(session,"zFromROPassageTimeOUP",value=zFrom)
              updateNumericInput(session,"zToROPassageTimeOUP",value=zTo)
              updateNumericInput(session,"zByROPassageTimeOUP",value=zBy)
              updateNumericInput(session,"kROPassageTimeOUP",value=k)
              updateNumericInput(session,"xROPassageTimeOUP",value=x)
              updateNumericInput(session,"omegaROPassageTimeOUP",value=omega)
              updateNumericInput(session,"PpctROPassageTimeOUP",value=Ppct)
              updateNumericInput(session,"sROPassageTimeOUP",value=s)
            })
          }
          FromUItoR6 <- function()
          {
            # Get from UI ----
            t_stoch_args <- A$get_t_stoch_args()
            t <- t_stoch_args[[1]]
            isolate({
              rho <- input$rhoROPassageTimeOUP
              mu <- input$muROPassageTimeOUP
              sigma <- input$sigmaROPassageTimeOUP
              zFrom <- input$zFromROPassageTimeOUP
              zTo <- input$zToROPassageTimeOUP
              zBy <- input$zByROPassageTimeOUP
              k <- input$kROPassageTimeOUP
              x <- input$xROPassageTimeOUP
              omega <- input$omegaROPassageTimeOUP
              Ppct <- input$PpctROPassageTimeOUP
              s <- input$sROPassageTimeOUP
            })
            if(!is.numeric(rho)) { rho <- 0 }
            else if(rho < 0) { rho <- 0 }
            if(!is.numeric(mu)) { mu <- 0 }
            if(!is.numeric(sigma)) { sigma <- 0 }
            z <- axissequence(zFrom,zTo,zBy)
            if(!is.numeric(k)) { k <- 0 }
            if(!is.numeric(x)) { x <- -mu }
            if(!is.numeric(omega)) { omega <- 1 }
            else if(omega < 0) { omega <- 0 }
            else if(omega > 1) { omega <- 1 }
            if(!is.numeric(Ppct)) { Ppct <- 0.841345 }
            else if(Ppct < 0.01) { Ppct <- 0.01 }
            else if(Ppct > 0.99) { Ppct <- 0.99 }
            if(!is.numeric(s)) { s <- t[1] }
            else if(s != t[1])
            {
              tadd <- s-t[1]
              t <- t+tadd
            }
            # Set to OUP ----
            A$set_oup_params(rho=rho,mu=mu,sigma=sigma)
            A$set_t_stoch_args(t=t,k=k,s=s,x=x,z=z,omega=omega,Ppct=Ppct)
          }
          # user clicks clear or save ----
          observe({
            FromUItoR6()
            A$undo_clear()
            showNotification("argument set 1 out of 1.",id="ROundo",duration=2)
          }) %>% bindEvent(input$clearROPassageTimeOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            FromUItoR6()
            n <- A$undo_save()
            showNotification(paste("argument set ",n," out of ",n,"."),id="ROundo",duration=2)
          }) %>% bindEvent(input$saveROPassageTimeOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # user clicks undn, unup, axes, plot (or enter key), left or rght ----
          output$plotlyROPassageTimeOUP <- renderPlotly({
            if(input$undnROPassageTimeOUP > RObtns[5,2])
            {
              RObtns[5,2] <<- input$undnROPassageTimeOUP
              Ixn <- A$undo_undo()
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="ROundo",duration=2)
            }
            else if(input$unupROPassageTimeOUP > RObtns[5,3])
            {
              RObtns[5,3] <<- input$unupROPassageTimeOUP
              Ixn <- A$undo_undo(1)
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="ROundo",duration=2)
            }
            else if(input$syncROPassageTimeOUP > RObtns[5,4])
            {
              RObtns[5,4] <<- input$syncROPassageTimeOUP
              FromUItoR6()
              A$sync_zyxt_stoch()
            }
            else if(input$axesROPassageTimeOUP > RObtns[5,5])
            {
              RObtns[5,5] <<- input$axesROPassageTimeOUP
              FromUItoR6()
              A$axes_t_stoch()
            }
            else if(input$plotROPassageTimeOUP > RObtns[5,6])
            {
              RObtns[5,6] <<- input$plotROPassageTimeOUP
              FromUItoR6()
            }
            else if(input$leftROPassageTimeOUP > RObtns[5,7])
            {
              RObtns[5,7] <<- input$leftROPassageTimeOUP
              FromUItoR6()
              A$set_plot_type("p",5)
            }
            else if(input$rghtROPassageTimeOUP > RObtns[5,8])
            {
              RObtns[5,8] <<- input$rghtROPassageTimeOUP
              FromUItoR6()
              A$set_plot_type("n",5)
            }
            FromR6toUI()
            A$PlotPassageTimePercentiles()
          }) %>% bindEvent(input$undnROPassageTimeOUP,input$unupROPassageTimeOUP,input$syncROPassageTimeOUP,input$axesROPassageTimeOUP,input$plotROPassageTimeOUP,input$leftROPassageTimeOUP,input$rghtROPassageTimeOUP)
          # observe info ----
          observe({
            ibutton <<- ""
            infobutton <<- "infoROPassageTimeOUP"
            if(infotoggle()) { infotoggle(FALSE) }
            else { infotoggle(TRUE) }
          }) %>% bindEvent(input$infoROPassageTimeOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            removeModal(session)
            updateTabsetPanel(session,"navBar",selected="tabAOUP")
            updateTabsetPanel(session,"navAOUP",selected="APTPercentilesOUP")
          }) %>% bindEvent(input$alsoROPassageTimeOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
       })
    }
    else if(input$navBar == "tabAOUP")
    {
      observeEvent(input$navAOUP,{
        # Drift ----
        if(input$navAOUP == "ADriftOUP")
        {
          # define set/get functions ----
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
            isolate({
              updateNumericInput(session,"rhoADriftOUP",value=rho)
              updateNumericInput(session,"muADriftOUP",value=mu)
              updateNumericInput(session,"zFromADriftOUP",value=zFrom)
              updateNumericInput(session,"zToADriftOUP",value=zTo)
              updateNumericInput(session,"zByADriftOUP",value=zBy)
            })
          }
          FromUItoR6 <- function()
          {
            # Get from UI ----
            isolate({
              rho <- input$rhoADriftOUP
              mu <- input$muADriftOUP
              zFrom <- input$zFromADriftOUP
              zTo <- input$zToADriftOUP
              zBy <- input$zByADriftOUP
            })
            if(!is.numeric(rho)) { rho <- 0 }
            else if(rho < 0) { rho <- 0 }
            if(!is.numeric(mu)) { mu <- 0 }
            z <- axissequence(zFrom,zTo,zBy)
            # Set to OUP ----
            A$set_oup_params(rho=rho,mu=mu)
            A$set_z_stoch_args(z=z)
          }
          # user clicks clear or save ----
          observe({
            FromUItoR6()
            A$undo_clear()
            showNotification("argument set 1 out of 1.",id="Aundo",duration=2)
          }) %>% bindEvent(input$clearADriftOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            FromUItoR6()
            n <- A$undo_save()
            showNotification(paste("argument set ",n," out of ",n,"."),id="Aundo",duration=2)
          }) %>% bindEvent(input$saveADriftOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # user clicks undn, unup, sync, axes or plot (or enter key) ----
          output$plotlyADriftOUP <- renderPlotly({
            if(input$undnADriftOUP > Abtns[1,1])
            {
              Abtns[1,1] <<- input$undnADriftOUP
              Ixn <- A$undo_undo()
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="Aundo",duration=2)
            }
            else if(input$unupADriftOUP > Abtns[1,2])
            {
              Abtns[1,2] <<- input$unupADriftOUP
              Ixn <- A$undo_undo(1)
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="Aundo",duration=2)
            }
            else if(input$syncADriftOUP > Abtns[1,3])
            {
              Abtns[1,3] <<- input$syncADriftOUP
              FromUItoR6()
              A$sync_zyxt_stoch()
            }
            else if(input$axesADriftOUP > Abtns[1,4])
            {
              Abtns[1,4] <<- input$axesADriftOUP
              FromUItoR6()
              A$axes_z_stoch()
            }
            else if(input$plotADriftOUP > Abtns[1,5])
            {
              Abtns[1,5] <<- input$plotADriftOUP
              FromUItoR6()
            }
            FromR6toUI()
            A$PlotDrift()
          }) %>% bindEvent(input$undnADriftOUP,input$unupADriftOUP,input$syncADriftOUP,input$axesADriftOUP,input$plotADriftOUP)
          # observe info ----
          observe({
            ibutton <<- ""
            infobutton <<- "infoADriftOUP"
            if(infotoggle()) { infotoggle(FALSE) }
            else { infotoggle(TRUE) }
          }) %>% bindEvent(input$infoADriftOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            removeModal(session)
            updateTabsetPanel(session,"navBar",selected="tabFDOUP")
            updateTabsetPanel(session,"navFDOUP",selected="FDDriftOUP")
          }) %>% bindEvent(input$alsoADriftOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
        # Diffusion ----
        else if(input$navAOUP == "ADiffusionOUP")
        {
          # define set/get functions ----
          FromR6toUI <- function()
          {
            # Get from OUP ----
            oup_params <- A$get_oup_params()
            z_stoch_args <- A$get_z_stoch_args()
            type <- A$get_plot_types()[[1]][2]
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
            isolate({
              if(type < -0.5)
              {
                updateNumericInput(session,"rhoADiffusionOUP",label="rho",value=rho)
                updateNumericInput(session,"muADiffusionOUP",label="rho",value=mu)
              }
              else
              {
                updateNumericInput(session,"rhoADiffusionOUP",label="~",value=rho)
                updateNumericInput(session,"muADiffusionOUP",label="~",value=mu)
              }
              updateNumericInput(session,"sigmaADiffusionOUP",value=sigma)
              updateNumericInput(session,"zFromADiffusionOUP",value=zFrom)
              updateNumericInput(session,"zToADiffusionOUP",value=zTo)
              updateNumericInput(session,"zByADiffusionOUP",value=zBy)
            })
          }
          FromUItoR6 <- function()
          {
            # Get from UI ----
            isolate({
              rho <- input$rhoADiffusionOUP
              mu <- input$muADiffusionOUP
              sigma <- input$sigmaADiffusionOUP
              zFrom <- input$zFromADiffusionOUP
              zTo <- input$zToADiffusionOUP
              zBy <- input$zByADiffusionOUP
            })
            if(!is.numeric(rho)) { rho <- 0 }
            else if(rho < 0) { rho <- 0 }
            if(!is.numeric(mu)) { mu <- 0 }
            if(!is.numeric(sigma)) { sigma <- 0 }
            z <- axissequence(zFrom,zTo,zBy)
            # Set to OUP ----
            A$set_oup_params(rho=rho,mu=mu,sigma=sigma)
            A$set_z_stoch_args(z=z)
          }
          # user clicks clear or save ----
          observe({
            FromUItoR6()
            A$undo_clear()
            showNotification("argument set 1 out of 1.",id="Aundo",duration=2)
          }) %>% bindEvent(input$clearADiffusionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            FromUItoR6()
            n <- A$undo_save()
            showNotification(paste("argument set ",n," out of ",n,"."),id="Aundo",duration=2)
          }) %>% bindEvent(input$saveADiffusionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # user clicks undn, unup, sync, axes, plot (or enter key) or other ----
          output$plotlyADiffusionOUP <- renderPlotly({
            if(input$undnADiffusionOUP > Abtns[2,1])
            {
              Abtns[2,1] <<- input$undnADiffusionOUP
              Ixn <- A$undo_undo()
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="Aundo",duration=2)
            }
            else if(input$unupADiffusionOUP > Abtns[2,2])
            {
              Abtns[2,2] <<- input$unupADiffusionOUP
              Ixn <- A$undo_undo(1)
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="Aundo",duration=2)
            }
            else if(input$syncADiffusionOUP > Abtns[2,3])
            {
              Abtns[2,3] <<- input$syncADiffusionOUP
              FromUItoR6()
              A$sync_zyxt_stoch()
            }
            else if(input$axesADiffusionOUP > Abtns[2,4])
            {
              Abtns[2,4] <<- input$axesADiffusionOUP
              FromUItoR6()
              A$axes_z_stoch()
            }
            else if(input$plotADiffusionOUP > Abtns[2,5])
            {
              Abtns[2,5] <<- input$plotADiffusionOUP
              FromUItoR6()
            }
            else if(input$otherADiffusionOUP > Abtns[2,6])
            {
              Abtns[2,6] <<- input$otherADiffusionOUP
              FromUItoR6()
              A$set_plot_type("p",2)
            }
            FromR6toUI()
            A$PlotDiffusion()
          }) %>% bindEvent(input$undnADiffusionOUP,input$unupADiffusionOUP,input$syncADiffusionOUP,input$axesADiffusionOUP,input$plotADiffusionOUP,input$otherADiffusionOUP)
          # observe info ----
          observe({
            ibutton <<- ""
            infobutton <<- "infoADiffusionOUP"
            if(infotoggle()) { infotoggle(FALSE) }
            else { infotoggle(TRUE) }
          }) %>% bindEvent(input$infoADiffusionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            removeModal(session)
            updateTabsetPanel(session,"navBar",selected="tabFDOUP")
            updateTabsetPanel(session,"navFDOUP",selected="FDDiffusionOUP")
          }) %>% bindEvent(input$alsoADiffusionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
        # Mean ----
        else if(input$navAOUP == "AMeanOUP")
        {
          # define set/get functions ----
          FromR6toUI <- function()
          {
            # Get from OUP ----
            oup_params <- A$get_oup_params()
            y_stoch_args <- A$get_y_stoch_args()
            plot_args <- A$get_plot_args()
            type <- A$get_plot_types()[[1]][3]
            rho <- oup_params[[1]]
            mu <- oup_params[[2]]
            sigma <- oup_params[[3]]
            t <- y_stoch_args[[1]]
            y <- y_stoch_args[[2]]
            s <- y_stoch_args[[3]]
            x <- y_stoch_args[[4]]
            psi <- y_stoch_args[[5]]
            eps <- y_stoch_args[[6]]
            pmax <- plot_args[[1]]
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
            isolate({
              updateNumericInput(session,"rhoAMeanOUP",value=rho)
              updateNumericInput(session,"muAMeanOUP",value=mu)
              if(type < 0.5) { updateNumericInput(session,"sigmaAMeanOUP",label="~",value=sigma) }
              else { updateNumericInput(session,"sigmaAMeanOUP",label="sigma",value=sigma) }
              updateNumericInput(session,"tFromAMeanOUP",value=tFrom)
              updateNumericInput(session,"tToAMeanOUP",value=tTo)
              updateNumericInput(session,"tByAMeanOUP",value=tBy)
              updateNumericInput(session,"sAMeanOUP",value=s)
              updateNumericInput(session,"yFromAMeanOUP",value=yFrom)
              updateNumericInput(session,"yToAMeanOUP",value=yTo)
              updateNumericInput(session,"yByAMeanOUP",value=yBy)
              updateNumericInput(session,"xAMeanOUP",value=x)
              if(type < 1.5) { updateNumericInput(session,"psiAMeanOUP",label="~",value=psi) }
              else { updateNumericInput(session,"psiAMeanOUP",label="psi",value=psi) }
              if(type < -0.5) { updateNumericInput(session,"epsAMeanOUP",label="epsilon",value=eps) }
              else { updateNumericInput(session,"epsAMeanOUP",label="~",value=eps) }
              if(type >= 0.5 && type < 1.5) { updateNumericInput(session,"pmaxAMeanOUP",label="p max",value=pmax) }
              else { updateNumericInput(session,"pmaxAMeanOUP",label="~",value=pmax) }
            })
          }
          FromUItoR6 <- function()
          {
            # Get from UI ----
            isolate({
              rho <- input$rhoAMeanOUP
              mu <- input$muAMeanOUP
              sigma <- input$sigmaAMeanOUP
              tFrom <- input$tFromAMeanOUP
              tTo <- input$tToAMeanOUP
              tBy <- input$tByAMeanOUP
              s <- input$sAMeanOUP
              yFrom <- input$yFromAMeanOUP
              yTo <- input$yToAMeanOUP
              yBy <- input$yByAMeanOUP
              x <- input$xAMeanOUP
              psi <- input$psiAMeanOUP
              eps <- input$epsAMeanOUP
              pmax <- input$pmaxAMeanOUP
            })
            if(!is.numeric(rho)) { rho <- 0 }
            else if(rho < 0) { rho <- 0 }
            if(!is.numeric(mu)) { mu <- 0 }
            if(!is.numeric(sigma)) { sigma <- 0 }
            t <- axissequence(tFrom,tTo,tBy)
            if(!is.numeric(s)) { s <- t[1] }
            else if(s > t[1]) { s <- t[1] }
            y <- axissequence(yFrom,yTo,yBy)
            if(!is.numeric(x)) { x <- 0 }
            if(!is.numeric(psi)) { psi <- -1 }
            else if(psi <= 0) { psi <- -1 }
            else { psi <- 1 }
            if(!is.numeric(eps)) { eps <- 0.05 }
            else if(eps < 0.01) { eps <- 0.01 }
            else if(eps > 0.99) { eps <- 0.99 }
            if(!is.numeric(pmax)) { pmax <- NaN }
            # Set to OUP ----
            A$set_oup_params(rho=rho,mu=mu,sigma=sigma)
            A$set_y_stoch_args(t=t,y=y,s=s,x=x,psi=psi,eps=eps)
            A$set_plot_args(pmax=pmax)
          }
          # user clicks clear or save ----
          observe({
            FromUItoR6()
            A$undo_clear()
            showNotification("argument set 1 out of 1.",id="Aundo",duration=2)
          }) %>% bindEvent(input$clearAMeanOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            FromUItoR6()
            n <- A$undo_save()
            showNotification(paste("argument set ",n," out of ",n,"."),id="Aundo",duration=2)
          }) %>% bindEvent(input$saveAMeanOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # user clicks undn, unup, sync, axes, plot (or enter key), left or rght ----
          output$plotlyAMeanOUP <- renderPlotly({
            if(input$undnAMeanOUP > Abtns[3,1])
            {
              Abtns[3,1] <<- input$undnAMeanOUP
              Ixn <- A$undo_undo()
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="Aundo",duration=2)
            }
            else if(input$unupAMeanOUP > Abtns[3,2])
            {
              Abtns[3,2] <<- input$unupAMeanOUP
              Ixn <- A$undo_undo(1)
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="Aundo",duration=2)
            }
            else if(input$syncAMeanOUP > Abtns[3,3])
            {
              Abtns[3,3] <<- input$syncAMeanOUP
              FromUItoR6()
              A$sync_zyxt_stoch()
            }
            else if(input$axesAMeanOUP > Abtns[3,4])
            {
              Abtns[3,4] <<- input$axesAMeanOUP
              FromUItoR6()
              A$axes_y_stoch()
            }
            else if(input$plotAMeanOUP > Abtns[3,5])
            {
              Abtns[3,5] <<- input$plotAMeanOUP
              FromUItoR6()
            }
            else if(input$leftAMeanOUP > Abtns[3,6])
            {
              Abtns[3,6] <<- input$leftAMeanOUP
              FromUItoR6()
              A$set_plot_type("p",3)
            }
            else if(input$rghtAMeanOUP > Abtns[3,7])
            {
              Abtns[3,7] <<- input$rghtAMeanOUP
              FromUItoR6()
              A$set_plot_type("n",3)
            }
            FromR6toUI()
            A$PlotMean()
          }) %>% bindEvent(input$undnAMeanOUP,input$unupAMeanOUP,input$syncAMeanOUP,input$axesAMeanOUP,input$plotAMeanOUP,input$leftAMeanOUP,input$rghtAMeanOUP)
          # observe info ----
          observe({
            ibutton <<- ""
            infobutton <<- "infoAMeanOUP"
            if(infotoggle()) { infotoggle(FALSE) }
            else { infotoggle(TRUE) }
          }) %>% bindEvent(input$infoAMeanOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            removeModal(session)
            updateTabsetPanel(session,"navBar",selected="tabMCOUP")
            updateTabsetPanel(session,"navMCOUP",selected="MCMeanOUP")
          }) %>% bindEvent(input$alsoAMeanOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
        # Variance ----
        else if(input$navAOUP == "AVarianceOUP")
        {
          # define set/get functions ----
          FromR6toUI <- function()
          {
            # Get from OUP ----
            oup_params <- A$get_oup_params()
            y_stoch_args <- A$get_y_stoch_args()
            plot_args <- A$get_plot_args()
            type <- A$get_plot_types()[[1]][3]
            rho <- oup_params[[1]]
            mu <- oup_params[[2]]
            sigma <- oup_params[[3]]
            t <- y_stoch_args[[1]]
            y <- y_stoch_args[[2]]
            s <- y_stoch_args[[3]]
            x <- y_stoch_args[[4]]
            psi <- y_stoch_args[[5]]
            eps <- y_stoch_args[[6]]
            pmax <- plot_args[[1]]
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
            isolate({
              updateNumericInput(session,"rhoAVarianceOUP",value=rho)
              if(type < 0.5) { updateNumericInput(session,"muAVarianceOUP",label="~",value=mu) }
              else { updateNumericInput(session,"muAVarianceOUP",label="mu",value=mu) }
              updateNumericInput(session,"sigmaAVarianceOUP",label="sigma",value=sigma)
              updateNumericInput(session,"tFromAVarianceOUP",value=tFrom)
              updateNumericInput(session,"tToAVarianceOUP",value=tTo)
              updateNumericInput(session,"tByAVarianceOUP",value=tBy)
              updateNumericInput(session,"sAVarianceOUP",value=s)
              updateNumericInput(session,"yFromAVarianceOUP",value=yFrom)
              updateNumericInput(session,"yToAVarianceOUP",value=yTo)
              updateNumericInput(session,"yByAVarianceOUP",value=yBy)
              updateNumericInput(session,"xAVarianceOUP",value=x)
              if(type < 1.5) { updateNumericInput(session,"psiAVarianceOUP",label="~",value=psi) }
              else { updateNumericInput(session,"psiAVarianceOUP",label="psi",value=psi) }
              if(type < -0.5) { updateNumericInput(session,"epsAVarianceOUP",label="epsilon",value=eps) }
              else { updateNumericInput(session,"epsAVarianceOUP",label="~",value=eps) }
              if(type >= 0.5 && type < 1.5) { updateNumericInput(session,"pmaxAVarianceOUP",label="p max",value=pmax) }
              else { updateNumericInput(session,"pmaxAVarianceOUP",label="~",value=pmax) }
            })
          }
          FromUItoR6 <- function()
          {
            # Get from UI ----
            isolate({
              rho <- input$rhoAVarianceOUP
              mu <- input$muAVarianceOUP
              sigma <- input$sigmaAVarianceOUP
              tFrom <- input$tFromAVarianceOUP
              tTo <- input$tToAVarianceOUP
              tBy <- input$tByAVarianceOUP
              s <- input$sAVarianceOUP
              yFrom <- input$yFromAVarianceOUP
              yTo <- input$yToAVarianceOUP
              yBy <- input$yByAVarianceOUP
              x <- input$xAVarianceOUP
              psi <- input$psiAVarianceOUP
              eps <- input$epsAVarianceOUP
              pmax <- input$pmaxAVarianceOUP
            })
            if(!is.numeric(rho)) { rho <- 0 }
            else if(rho < 0) { rho <- 0 }
            if(!is.numeric(mu)) { mu <- 0 }
            if(!is.numeric(sigma)) { sigma <- 0 }
            t <- axissequence(tFrom,tTo,tBy)
            if(!is.numeric(s)) { s <- t[1] }
            else if(s > t[1]) { s <- t[1] }
            y <- axissequence(yFrom,yTo,yBy)
            if(!is.numeric(x)) { x <- 0 }
            if(!is.numeric(psi)) { psi <- -1 }
            else if(psi <= 0) { psi <- -1 }
            else { psi <- 1 }
            if(!is.numeric(eps)) { eps <- 0.05 }
            else if(eps < 0.01) { eps <- 0.01 }
            else if(eps > 0.99) { eps <- 0.99 }
            if(!is.numeric(pmax)) { pmax <- NaN }
            # Set to OUP ----
            A$set_oup_params(rho=rho,mu=mu,sigma=sigma)
            A$set_y_stoch_args(t=t,y=y,s=s,x=x,psi=psi,eps=eps)
            A$set_plot_args(pmax=pmax)
          }
          # user clicks clear or save ----
          observe({
            FromUItoR6()
            A$undo_clear()
            showNotification("argument set 1 out of 1.",id="Aundo",duration=2)
          }) %>% bindEvent(input$clearAVarianceOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            FromUItoR6()
            n <- A$undo_save()
            showNotification(paste("argument set ",n," out of ",n,"."),id="Aundo",duration=2)
          }) %>% bindEvent(input$saveAVarianceOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # user clicks undn, unup, sync, axes, plot (or enter key), left or rght ----
          output$plotlyAVarianceOUP <- renderPlotly({
            if(input$undnAVarianceOUP > Abtns[5,1])
            {
              Abtns[5,1] <<- input$undnAVarianceOUP
              Ixn <- A$undo_undo()
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="Aundo",duration=2)
            }
            else if(input$unupAVarianceOUP > Abtns[5,2])
            {
              Abtns[5,2] <<- input$unupAVarianceOUP
              Ixn <- A$undo_undo(1)
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="Aundo",duration=2)
            }
            else if(input$syncAVarianceOUP > Abtns[5,3])
            {
              Abtns[5,3] <<- input$syncAVarianceOUP
              FromUItoR6()
              A$sync_zyxt_stoch()
            }
            else if(input$axesAVarianceOUP > Abtns[5,4])
            {
              Abtns[5,4] <<- input$axesAVarianceOUP
              FromUItoR6()
              A$axes_y_stoch()
            }
            else if(input$plotAVarianceOUP > Abtns[5,5])
            {
              Abtns[5,5] <<- input$plotAVarianceOUP
              FromUItoR6()
            }
            else if(input$leftAVarianceOUP > Abtns[5,6])
            {
              Abtns[5,6] <<- input$leftAVarianceOUP
              FromUItoR6()
              A$set_plot_type("p",3)
            }
            else if(input$rghtAVarianceOUP > Abtns[5,7])
            {
              Abtns[5,7] <<- input$rghtAVarianceOUP
              FromUItoR6()
              A$set_plot_type("n",3)
            }
            FromR6toUI()
            A$PlotVariance()
          }) %>% bindEvent(input$undnAVarianceOUP,input$unupAVarianceOUP,input$syncAVarianceOUP,input$axesAVarianceOUP,input$plotAVarianceOUP,input$leftAVarianceOUP,input$rghtAVarianceOUP)
          # observe info ----
          observe({
            ibutton <<- ""
            infobutton <<- "infoAVarianceOUP"
            if(infotoggle()) { infotoggle(FALSE) }
            else { infotoggle(TRUE) }
          }) %>% bindEvent(input$infoAVarianceOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            removeModal(session)
            updateTabsetPanel(session,"navBar",selected="tabMCOUP")
            updateTabsetPanel(session,"navMCOUP",selected="MCVarianceOUP")
          }) %>% bindEvent(input$alsoAVarianceOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
        # Transition Density ----
        else if(input$navAOUP == "ADensityOUP")
        {
          # define set/get functions ----
          FromR6toUI <- function()
          {
            # Get from OUP ----
            oup_params <- A$get_oup_params()
            y_stoch_args <- A$get_y_stoch_args()
            plot_args <- A$get_plot_args()
            rho <- oup_params[[1]]
            mu <- oup_params[[2]]
            sigma <- oup_params[[3]]
            t <- y_stoch_args[[1]]
            y <- y_stoch_args[[2]]
            s <- y_stoch_args[[3]]
            x <- y_stoch_args[[4]]
            pmax <- plot_args[[1]]
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
            isolate({
              updateNumericInput(session,"rhoADensityOUP",value=rho)
              updateNumericInput(session,"muADensityOUP",value=mu)
              updateNumericInput(session,"sigmaADensityOUP",value=sigma)
              updateNumericInput(session,"tFromADensityOUP",value=tFrom)
              updateNumericInput(session,"tToADensityOUP",value=tTo)
              updateNumericInput(session,"tByADensityOUP",value=tBy)
              updateNumericInput(session,"sADensityOUP",value=s)
              updateNumericInput(session,"yFromADensityOUP",value=yFrom)
              updateNumericInput(session,"yToADensityOUP",value=yTo)
              updateNumericInput(session,"yByADensityOUP",value=yBy)
              updateNumericInput(session,"xADensityOUP",value=x)
              updateNumericInput(session,"pmaxADensityOUP",value=pmax)
            })
          }
          FromUItoR6 <- function()
          {
            # Get from UI ----
            isolate({
              rho <- input$rhoADensityOUP
              mu <- input$muADensityOUP
              sigma <- input$sigmaADensityOUP
              tFrom <- input$tFromADensityOUP
              tTo <- input$tToADensityOUP
              tBy <- input$tByADensityOUP
              s <- input$sADensityOUP
              yFrom <- input$yFromADensityOUP
              yTo <- input$yToADensityOUP
              yBy <- input$yByADensityOUP
              x <- input$xADensityOUP
              pmax <- input$pmaxADensityOUP
            })
            if(!is.numeric(rho)) { rho <- 0 }
            else if(rho < 0) { rho <- 0 }
            if(!is.numeric(mu)) { mu <- 0 }
            if(!is.numeric(sigma)) { sigma <- 0 }
            t <- axissequence(tFrom,tTo,tBy)
            if(!is.numeric(s)) { s <- t[1] }
            else if(s > t[1]) { s <- t[1] }
            y <- axissequence(yFrom,yTo,yBy)
            if(!is.numeric(x)) { x <- 0 }
            if(!is.numeric(pmax)) { pmax <- NaN }
            # Set to OUP ----
            A$set_oup_params(rho=rho,mu=mu,sigma=sigma)
            A$set_y_stoch_args(t=t,y=y,s=s,x=x)
            A$set_plot_args(pmax=pmax)
          }
          # user clicks clear or save ----
          observe({
            FromUItoR6()
            A$undo_clear()
            showNotification("argument set 1 out of 1.",id="Aundo",duration=2)
          }) %>% bindEvent(input$clearADensityOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            FromUItoR6()
            n <- A$undo_save()
            showNotification(paste("argument set ",n," out of ",n,"."),id="Aundo",duration=2)
          }) %>% bindEvent(input$saveADensityOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # user clicks undn, unup, sync, axes, plot (or enter key) or other ----
          output$plotlyADensityOUP <- renderPlotly({
            if(input$undnADensityOUP > Abtns[7,1])
            {
              Abtns[7,1] <<- input$undnADensityOUP
              Ixn <- A$undo_undo()
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="Aundo",duration=2)
            }
            else if(input$unupADensityOUP > Abtns[7,2])
            {
              Abtns[7,2] <<- input$unupADensityOUP
              Ixn <- A$undo_undo(1)
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="Aundo",duration=2)
            }
            else if(input$syncADensityOUP > Abtns[7,3])
            {
              Abtns[7,3] <<- input$syncADensityOUP
              FromUItoR6()
              A$sync_zyxt_stoch()
            }
            else if(input$axesADensityOUP > Abtns[7,4])
            {
              Abtns[7,4] <<- input$axesADensityOUP
              FromUItoR6()
              A$axes_y_stoch()
            }
            else if(input$plotADensityOUP > Abtns[7,5])
            {
              Abtns[7,5] <<- input$plotADensityOUP
              FromUItoR6()
            }
            else if(input$otherADensityOUP > Abtns[7,6])
            {
              Abtns[7,6] <<- input$otherADensityOUP
              FromUItoR6()
              A$set_plot_type("p",4)
            }
            FromR6toUI()
            A$PlotDensity()
          }) %>% bindEvent(input$undnADensityOUP,input$unupADensityOUP,input$syncADensityOUP,input$axesADensityOUP,input$plotADensityOUP,input$otherADensityOUP)
          # observe info ----
          observe({
            ibutton <<- ""
            infobutton <<- "infoADensityOUP"
            if(infotoggle()) { infotoggle(FALSE) }
            else { infotoggle(TRUE) }
          }) %>% bindEvent(input$infoADensityOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            removeModal(session)
            updateTabsetPanel(session,"navBar",selected="tabMCOUP")
            updateTabsetPanel(session,"navMCOUP",selected="MCDensityOUP")
          }) %>% bindEvent(input$alsoADensityOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
        # Transition Probability ----
        else if(input$navAOUP == "AProbabilityOUP")
        {
          # define set/get functions ----
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
            isolate({
              updateNumericInput(session,"rhoAProbabilityOUP",value=rho)
              updateNumericInput(session,"muAProbabilityOUP",value=mu)
              updateNumericInput(session,"sigmaAProbabilityOUP",value=sigma)
              updateNumericInput(session,"tFromAProbabilityOUP",value=tFrom)
              updateNumericInput(session,"tToAProbabilityOUP",value=tTo)
              updateNumericInput(session,"tByAProbabilityOUP",value=tBy)
              updateNumericInput(session,"sAProbabilityOUP",value=s)
              updateNumericInput(session,"yFromAProbabilityOUP",value=yFrom)
              updateNumericInput(session,"yToAProbabilityOUP",value=yTo)
              updateNumericInput(session,"yByAProbabilityOUP",value=yBy)
              updateNumericInput(session,"xAProbabilityOUP",value=x)
              updateNumericInput(session,"psiAProbabilityOUP",value=psi)
            })
          }
          FromUItoR6 <- function()
          {
            # Get from UI ----
            isolate({
              rho <- input$rhoAProbabilityOUP
              mu <- input$muAProbabilityOUP
              sigma <- input$sigmaAProbabilityOUP
              tFrom <- input$tFromAProbabilityOUP
              tTo <- input$tToAProbabilityOUP
              tBy <- input$tByAProbabilityOUP
              s <- input$sAProbabilityOUP
              yFrom <- input$yFromAProbabilityOUP
              yTo <- input$yToAProbabilityOUP
              yBy <- input$yByAProbabilityOUP
              x <- input$xAProbabilityOUP
              psi <- input$psiAProbabilityOUP
            })
            if(!is.numeric(rho)) { rho <- 0 }
            else if(rho < 0) { rho <- 0 }
            if(!is.numeric(mu)) { mu <- 0 }
            if(!is.numeric(sigma)) { sigma <- 0 }
            t <- axissequence(tFrom,tTo,tBy)
            if(!is.numeric(s)) { s <- t[1] }
            else if(s > t[1]) { s <- t[1] }
            y <- axissequence(yFrom,yTo,yBy)
            if(!is.numeric(x)) { x <- 0 }
            if(!is.numeric(psi)) { psi <- -1 }
            else if(psi <= 0) { psi <- -1 }
            else { psi <- 1 }
            # Set to OUP ----
            A$set_oup_params(rho=rho,mu=mu,sigma=sigma)
            A$set_y_stoch_args(t=t,y=y,s=s,x=x,psi=psi)
          }
          # user clicks clear or save ----
          observe({
            FromUItoR6()
            A$undo_clear()
            showNotification("argument set 1 out of 1.",id="Aundo",duration=2)
          }) %>% bindEvent(input$clearAProbabilityOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            FromUItoR6()
            n <- A$undo_save()
            showNotification(paste("argument set ",n," out of ",n,"."),id="Aundo",duration=2)
          }) %>% bindEvent(input$saveAProbabilityOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # user clicks undn, unup, sync, axes, plot (or enter key) or other ----
          output$plotlyAProbabilityOUP <- renderPlotly({
            if(input$undnAProbabilityOUP > Abtns[8,1])
            {
              Abtns[8,1] <<- input$undnAProbabilityOUP
              Ixn <- A$undo_undo()
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="Aundo",duration=2)
            }
            else if(input$unupAProbabilityOUP > Abtns[8,2])
            {
              Abtns[8,2] <<- input$unupAProbabilityOUP
              Ixn <- A$undo_undo(1)
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="Aundo",duration=2)
            }
            else if(input$syncAProbabilityOUP > Abtns[8,3])
            {
              Abtns[8,3] <<- input$syncAProbabilityOUP
              FromUItoR6()
              A$sync_zyxt_stoch()
            }
            else if(input$axesAProbabilityOUP > Abtns[8,4])
            {
              Abtns[8,4] <<- input$axesAProbabilityOUP
              FromUItoR6()
              A$axes_y_stoch()
            }
            else if(input$plotAProbabilityOUP > Abtns[8,5])
            {
              Abtns[8,5] <<- input$plotAProbabilityOUP
              FromUItoR6()
            }
            else if(input$otherAProbabilityOUP > Abtns[8,6])
            {
              Abtns[8,6] <<- input$otherAProbabilityOUP
              FromUItoR6()
              A$set_plot_type("p",4)
            }
            FromR6toUI()
            A$PlotProbability()
          }) %>% bindEvent(input$undnAProbabilityOUP,input$unupAProbabilityOUP,input$syncAProbabilityOUP,input$axesAProbabilityOUP,input$plotAProbabilityOUP,input$otherAProbabilityOUP)
          # observe info ----
          observe({
            ibutton <<- ""
            infobutton <<- "infoAProbabilityOUP"
            if(infotoggle()) { infotoggle(FALSE) }
            else { infotoggle(TRUE) }
          }) %>% bindEvent(input$infoAProbabilityOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            removeModal(session)
            updateTabsetPanel(session,"navBar",selected="tabMCOUP")
            updateTabsetPanel(session,"navMCOUP",selected="MCProbabilityOUP")
          }) %>% bindEvent(input$alsoAProbabilityOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
        # Double Integral ----
        else if(input$navAOUP == "ADoubleOUP")
        {
          # define set/get functions ----
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
            isolate({
              updateNumericInput(session,"rhoADoubleOUP",value=rho)
              updateNumericInput(session,"muADoubleOUP",value=mu)
              updateNumericInput(session,"sigmaADoubleOUP",value=sigma)
              updateNumericInput(session,"tFromADoubleOUP",value=tFrom)
              updateNumericInput(session,"tToADoubleOUP",value=tTo)
              updateNumericInput(session,"tByADoubleOUP",value=tBy)
              updateNumericInput(session,"sADoubleOUP",value=s)
              updateNumericInput(session,"yFromADoubleOUP",value=yFrom)
              updateNumericInput(session,"yToADoubleOUP",value=yTo)
              updateNumericInput(session,"yByADoubleOUP",value=yBy)
              updateNumericInput(session,"xADoubleOUP",value=x)
              updateNumericInput(session,"psiADoubleOUP",value=psi)
            })
          }
          FromUItoR6 <- function()
          {
            # Get from UI ----
            isolate({
              rho <- input$rhoADoubleOUP
              mu <- input$muADoubleOUP
              sigma <- input$sigmaADoubleOUP
              tFrom <- input$tFromADoubleOUP
              tTo <- input$tToADoubleOUP
              tBy <- input$tByADoubleOUP
              s <- input$sADoubleOUP
              yFrom <- input$yFromADoubleOUP
              yTo <- input$yToADoubleOUP
              yBy <- input$yByADoubleOUP
              x <- input$xADoubleOUP
              psi <- input$psiADoubleOUP
            })
            if(!is.numeric(rho)) { rho <- 0 }
            else if(rho < 0) { rho <- 0 }
            if(!is.numeric(mu)) { mu <- 0 }
            if(!is.numeric(sigma)) { sigma <- 0 }
            t <- axissequence(tFrom,tTo,tBy)
            if(!is.numeric(s)) { s <- t[1] }
            else if(s > t[1]) { s <- t[1] }
            y <- axissequence(yFrom,yTo,yBy)
            if(!is.numeric(x)) { x <- 0 }
            if(!is.numeric(psi)) { psi <- -1 }
            else if(psi <= 0) { psi <- -1 }
            else { psi <- 1 }
            # Set to OUP ----
            A$set_oup_params(rho=rho,mu=mu,sigma=sigma)
            A$set_y_stoch_args(t=t,y=y,s=s,x=x,psi=psi)
          }
          # user clicks clear or save ----
          observe({
            FromUItoR6()
            A$undo_clear()
            showNotification("argument set 1 out of 1.",id="Aundo",duration=2)
          }) %>% bindEvent(input$clearADoubleOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            FromUItoR6()
            n <- A$undo_save()
            showNotification(paste("argument set ",n," out of ",n,"."),id="Aundo",duration=2)
          }) %>% bindEvent(input$saveADoubleOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # user clicks undn, unup, sync, axes, plot (or enter key) or other ----
          output$plotlyADoubleOUP <- renderPlotly({
            if(input$undnADoubleOUP > Abtns[9,1])
            {
              Abtns[9,1] <<- input$undnADoubleOUP
              Ixn <- A$undo_undo()
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="Aundo",duration=2)
            }
            else if(input$unupADoubleOUP > Abtns[9,2])
            {
              Abtns[9,2] <<- input$unupADoubleOUP
              Ixn <- A$undo_undo(1)
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="Aundo",duration=2)
            }
            else if(input$syncADoubleOUP > Abtns[9,3])
            {
              Abtns[9,3] <<- input$syncADoubleOUP
              FromUItoR6()
              A$sync_zyxt_stoch()
            }
            else if(input$axesADoubleOUP > Abtns[9,4])
            {
              Abtns[9,4] <<- input$axesADoubleOUP
              FromUItoR6()
              A$axes_y_stoch()
            }
            else if(input$plotADoubleOUP > Abtns[9,5])
            {
              Abtns[9,5] <<- input$plotADoubleOUP
              FromUItoR6()
            }
            else if(input$otherADoubleOUP > Abtns[9,6])
            {
              Abtns[9,6] <<- input$otherADoubleOUP
              FromUItoR6()
              A$set_plot_type("p",4)
            }
            FromR6toUI()
            A$PlotDoubleIntegral()
          }) %>% bindEvent(input$undnADoubleOUP,input$unupADoubleOUP,input$syncADoubleOUP,input$axesADoubleOUP,input$plotADoubleOUP,input$otherADoubleOUP)
          # observe info ----
          observe({
            ibutton <<- ""
            infobutton <<- "infoADoubleOUP"
            if(infotoggle()) { infotoggle(FALSE) }
            else { infotoggle(TRUE) }
          }) %>% bindEvent(input$infoADoubleOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            removeModal(session)
            updateTabsetPanel(session,"navBar",selected="tabMCOUP")
            updateTabsetPanel(session,"navMCOUP",selected="MCDoubleOUP")
          }) %>% bindEvent(input$alsoADoubleOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
        # Option ----
        else if(input$navAOUP == "AOptionOUP")
        {
          # define set/get functions ----
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
            isolate({
              updateNumericInput(session,"rhoAOptionOUP",value=rho)
              updateNumericInput(session,"muAOptionOUP",value=mu)
              updateNumericInput(session,"sigmaAOptionOUP",value=sigma)
              updateNumericInput(session,"sFromAOptionOUP",value=sFrom)
              updateNumericInput(session,"sToAOptionOUP",value=sTo)
              updateNumericInput(session,"sByAOptionOUP",value=sBy)
              updateNumericInput(session,"tAOptionOUP",value=t)
              updateNumericInput(session,"xFromAOptionOUP",value=xFrom)
              updateNumericInput(session,"xToAOptionOUP",value=xTo)
              updateNumericInput(session,"xByAOptionOUP",value=xBy)
              updateNumericInput(session,"yAOptionOUP",value=y)
              updateNumericInput(session,"rAOptionOUP",value=r)
              updateNumericInput(session,"phiAOptionOUP",value=phi)
              if(phi > 0)
              {
                updateNumericInput(session,"bAOptionOUP",label="b",value=b)
                updateNumericInput(session,"cAOptionOUP",label="~",value=c)
              }
              else
              {
                updateNumericInput(session,"bAOptionOUP",label="~",value=b)
                updateNumericInput(session,"cAOptionOUP",label="c",value=c)
              }
            })
          }
          FromUItoR6 <- function()
          {
            # Get from UI ----
            isolate({
              rho <- input$rhoAOptionOUP
              mu <- input$muAOptionOUP
              sigma <- input$sigmaAOptionOUP
              sFrom <- input$sFromAOptionOUP
              sTo <- input$sToAOptionOUP
              sBy <- input$sByAOptionOUP
              t <- input$tAOptionOUP
              xFrom <- input$xFromAOptionOUP
              xTo <- input$xToAOptionOUP
              xBy <- input$xByAOptionOUP
              y <- input$yAOptionOUP
              r <- input$rAOptionOUP
              phi <- input$phiAOptionOUP
              b <- input$bAOptionOUP
              c <- input$cAOptionOUP
            })
            if(!is.numeric(rho)) { rho <- 0 }
            else if(rho < 0) { rho <- 0 }
            if(!is.numeric(mu)) { mu <- 0 }
            if(!is.numeric(sigma)) { sigma <- 0 }
            s <- axissequence(sFrom,sTo,sBy)
            if(!is.numeric(t)) { t <- s[1] }
            else if(t < s[1]) { t <- s[1] }
            x <- axissequence(xFrom,xTo,xBy)
            if(!is.numeric(y)) { y <- 0 }
            if(!is.numeric(r)) { r <- 0 }
            if(!is.numeric(phi)) { phi <- -1 }
            else if(phi <= 0) { phi <- -1 }
            else if(phi > 0) { phi <- 1 }
            if(!is.numeric(b)) { b <- 0 }
            if(!is.numeric(c)) { c <- 0 }
            # Set to OUP ----
            A$set_oup_params(rho=rho,mu=mu,sigma=sigma)
            A$set_x_stoch_args(s=s,x=x,t=t,y=y,r=r,phi=phi,b=b,c=c)
          }
          # observe phi ----
          observe({
            if(is.numeric(input$phiAOptionOUP))
            {
              if(input$phiAOptionOUP > 0)
              {
                b <- A$get_x_stoch_args()[[7]]
                isolate({
                  updateNumericInput(session,"bcAOptionOUP",label="b",value=b)
                })
              }
              else
              {
                c <- A$get_x_stoch_args()[[8]]
                isolate({
                  updateNumericInput(session,"bcAOptionOUP",label="c",value=c)
                })
              }
            }
          }) %>% bindEvent(input$phiAOptionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # user clicks clear or save ----
          observe({
            FromUItoR6()
            A$undo_clear()
            showNotification("argument set 1 out of 1.",id="Aundo",duration=2)
          }) %>% bindEvent(input$clearAOptionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            FromUItoR6()
            n <- A$undo_save()
            showNotification(paste("argument set ",n," out of ",n,"."),id="Aundo",duration=2)
          }) %>% bindEvent(input$saveAOptionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # user clicks undn, unup, sync, axes, plot (or enter key) or other ----
          output$plotlyAOptionOUP <- renderPlotly({
            if(input$undnAOptionOUP > Abtns[10,1])
            {
              Abtns[10,1] <<- input$undnAOptionOUP
              Ixn <- A$undo_undo()
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="Aundo",duration=2)
            }
            else if(input$unupAOptionOUP > Abtns[10,2])
            {
              Abtns[10,2] <<- input$unupAOptionOUP
              Ixn <- A$undo_undo(1)
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="Aundo",duration=2)
            }
            else if(input$syncAOptionOUP > Abtns[10,3])
            {
              Abtns[10,3] <<- input$syncAOptionOUP
              FromUItoR6()
              A$sync_zyxt_stoch()
            }
            else if(input$axesAOptionOUP > Abtns[10,4])
            {
              Abtns[10,4] <<- input$axesAOptionOUP
              FromUItoR6()
              A$axes_x_stoch()
            }
            else if(input$plotAOptionOUP > Abtns[10,5])
            {
              Abtns[10,5] <<- input$plotAOptionOUP
              FromUItoR6()
            }
            else if(input$otherAOptionOUP > Abtns[10,6])
            {
              Abtns[10,6] <<- input$otherAOptionOUP
              FromUItoR6()
              A$set_plot_type("p",4)
            }
            FromR6toUI()
            A$PlotOption()
          }) %>% bindEvent(input$undnAOptionOUP,input$unupAOptionOUP,input$syncAOptionOUP,input$axesAOptionOUP,input$plotAOptionOUP,input$otherAOptionOUP)
          # observe info ----
          observe({
            ibutton <<- ""
            infobutton <<- "infoAOptionOUP"
            if(infotoggle()) { infotoggle(FALSE) }
            else { infotoggle(TRUE) }
          }) %>% bindEvent(input$infoAOptionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            removeModal(session)
            updateTabsetPanel(session,"navBar",selected="tabFDOUP")
            updateTabsetPanel(session,"navFDOUP",selected="FDOptionOUP")
          }) %>% bindEvent(input$alsoAOptionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
        # Option Envelope----
        else if(input$navAOUP == "AEnvelopeOUP")
        {
          # define set/get functions ----
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
            isolate({
              updateNumericInput(session,"rhoAEnvelopeOUP",value=rho)
              updateNumericInput(session,"muAEnvelopeOUP",value=mu)
              updateNumericInput(session,"sigmaAEnvelopeOUP",value=sigma)
              updateNumericInput(session,"tAEnvelopeOUP",value=t)
              updateNumericInput(session,"sFromAEnvelopeOUP",value=sFrom)
              updateNumericInput(session,"sToAEnvelopeOUP",value=sTo)
              updateNumericInput(session,"sByAEnvelopeOUP",value=sBy)
              updateNumericInput(session,"xFromAEnvelopeOUP",value=xFrom)
              updateNumericInput(session,"xToAEnvelopeOUP",value=xTo)
              updateNumericInput(session,"xByAEnvelopeOUP",value=xBy)
              updateNumericInput(session,"yAEnvelopeOUP",value=y)
              updateNumericInput(session,"rAEnvelopeOUP",value=r)
              updateNumericInput(session,"phiAEnvelopeOUP",value=phi)
              if(phi > 0)
              {
                updateNumericInput(session,"bAEnvelopeOUP",label="b",value=b)
                updateNumericInput(session,"cAEnvelopeOUP",label="~",value=c)
              }
              else
              {
                updateNumericInput(session,"bAEnvelopeOUP",label="~",value=b)
                updateNumericInput(session,"cAEnvelopeOUP",label="c",value=c)
              }
            })
          }
          FromUItoR6 <- function()
          {
            # Get from UI ----
            isolate({
              rho <- input$rhoAEnvelopeOUP
              mu <- input$muAEnvelopeOUP
              sigma <- input$sigmaAEnvelopeOUP
              sFrom <- input$sFromAEnvelopeOUP
              sTo <- input$sToAEnvelopeOUP
              sBy <- input$sByAEnvelopeOUP
              t <- input$tAEnvelopeOUP
              xFrom <- input$xFromAEnvelopeOUP
              xTo <- input$xToAEnvelopeOUP
              xBy <- input$xByAEnvelopeOUP
              y <- input$yAEnvelopeOUP
              r <- input$rAEnvelopeOUP
              phi <- input$phiAEnvelopeOUP
              b <- input$bAEnvelopeOUP
              c <- input$cAEnvelopeOUP
            })
            if(!is.numeric(rho)) { rho <- 0 }
            else if(rho < 0) { rho <- 0 }
            if(!is.numeric(mu)) { mu <- 0 }
            if(!is.numeric(sigma)) { sigma <- 0 }
            s <- axissequence(sFrom,sTo,sBy)
            if(!is.numeric(t)) { t <- s[1] }
            else if(t < s[1]) { t <- s[1] }
            x <- axissequence(xFrom,xTo,xBy)
            if(!is.numeric(y)) { y <- 0 }
            if(!is.numeric(r)) { r <- 0 }
            if(!is.numeric(phi)) { phi <- -1 }
            else if(phi <= 0) { phi <- -1 }
            else if(phi > 0) { phi <- 1 }
            if(!is.numeric(b)) { b <- 0 }
            if(!is.numeric(c)) { c <- 0 }
            # Set to OUP ----
            A$set_oup_params(rho=rho,mu=mu,sigma=sigma)
            A$set_x_stoch_args(s=s,x=x,t=t,y=y,r=r,phi=phi,b=b,c=c)
          }
          # observe phi ----
          observe({
            if(is.numeric(input$phiAEnvelopeOUP))
            {
              if(input$phiAEnvelopeOUP > 0)
              {
                b <- A$get_x_stoch_args()[[7]]
                isolate({
                  updateNumericInput(session,"bcAEnvelopeOUP",label="b",value=b)
                })
              }
              else
              {
                c <- A$get_x_stoch_args()[[8]]
                isolate({
                  updateNumericInput(session,"bcAEnvelopeOUP",label="c",value=c)
                })
              }
            }
          }) %>% bindEvent(input$phiAEnvelopeOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # user clicks clear or save ----
          observe({
            FromUItoR6()
            A$undo_clear()
            showNotification("argument set 1 out of 1.",id="Aundo",duration=2)
          }) %>% bindEvent(input$clearAEnvelopeOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            FromUItoR6()
            n <- A$undo_save()
            showNotification(paste("argument set ",n," out of ",n,"."),id="Aundo",duration=2)
          }) %>% bindEvent(input$saveAEnvelopeOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # user clicks undn, unup, sync, axes, plot (or enter key) or other ----
          output$plotlyAEnvelopeOUP <- renderPlotly({
            if(input$undnAEnvelopeOUP > Abtns[11,1])
            {
              Abtns[11,1] <<- input$undnAEnvelopeOUP
              Ixn <- A$undo_undo()
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="Aundo",duration=2)
            }
            else if(input$unupAEnvelopeOUP > Abtns[11,2])
            {
              Abtns[11,2] <<- input$unupAEnvelopeOUP
              Ixn <- A$undo_undo(1)
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="Aundo",duration=2)
            }
            else if(input$syncAEnvelopeOUP > Abtns[11,3])
            {
              Abtns[11,3] <<- input$syncAEnvelopeOUP
              FromUItoR6()
              A$sync_zyxt_stoch()
            }
            else if(input$axesAEnvelopeOUP > Abtns[11,4])
            {
              Abtns[11,4] <<- input$axesAEnvelopeOUP
              FromUItoR6()
              A$axes_x_stoch()
            }
            else if(input$plotAEnvelopeOUP > Abtns[11,5])
            {
              Abtns[11,5] <<- input$plotAEnvelopeOUP
              FromUItoR6()
            }
            else if(input$otherAEnvelopeOUP > Abtns[11,6])
            {
              Abtns[11,6] <<- input$otherAEnvelopeOUP
              FromUItoR6()
              A$set_plot_type("p",4)
            }
            FromR6toUI()
            A$PlotOptionEnvelope()
          }) %>% bindEvent(input$undnAEnvelopeOUP,input$unupAEnvelopeOUP,input$syncAEnvelopeOUP,input$axesAEnvelopeOUP,input$plotAEnvelopeOUP,input$otherAEnvelopeOUP)
          # observe info ----
          observe({
            ibutton <<- ""
            infobutton <<- "infoAEnvelopeOUP"
            if(infotoggle()) { infotoggle(FALSE) }
            else { infotoggle(TRUE) }
          }) %>% bindEvent(input$infoAEnvelopeOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            removeModal(session)
            updateTabsetPanel(session,"navBar",selected="tabFDOUP")
            updateTabsetPanel(session,"navFDOUP",selected="FDEnvelopeOUP")
          }) %>% bindEvent(input$alsoAEnvelopeOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
        # Decision Threshold ----
        else if(input$navAOUP == "ADecisionOUP")
        {
          # define set/get functions ----
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
            isolate({
              updateNumericInput(session,"rhoADecisionOUP",value=rho)
              updateNumericInput(session,"muADecisionOUP",value=mu)
              updateNumericInput(session,"sigmaADecisionOUP",value=sigma)
              updateNumericInput(session,"xFromADecisionOUP",value=xFrom)
              updateNumericInput(session,"xToADecisionOUP",value=xTo)
              updateNumericInput(session,"xByADecisionOUP",value=xBy)
              updateNumericInput(session,"yADecisionOUP",value=y)
              updateNumericInput(session,"rADecisionOUP",value=r)
              updateNumericInput(session,"phiADecisionOUP",value=phi)
              if(phi > 0)
              {
                updateNumericInput(session,"bADecisionOUP",label="b",value=b)
                updateNumericInput(session,"cADecisionOUP",label="~",value=c)
              }
              else
              {
                updateNumericInput(session,"bADecisionOUP",label="~",value=b)
                updateNumericInput(session,"cADecisionOUP",label="c",value=c)
              }
            })
          }
          FromUItoR6 <- function()
          {
            # Get from UI ----
            isolate({
              rho <- input$rhoADecisionOUP
              mu <- input$muADecisionOUP
              sigma <- input$sigmaADecisionOUP
              xFrom <- input$xFromADecisionOUP
              xTo <- input$xToADecisionOUP
              xBy <- input$xByADecisionOUP
              y <- input$yADecisionOUP
              r <- input$rADecisionOUP
              phi <- input$phiADecisionOUP
              b <- input$bADecisionOUP
              c <- input$cADecisionOUP
            })
            if(!is.numeric(rho)) { rho <- 0 }
            else if(rho < 0) { rho <- 0 }
            if(!is.numeric(mu)) { mu <- 0 }
            if(!is.numeric(sigma)) { sigma <- 0 }
            x <- axissequence(xFrom,xTo,xBy)
            if(!is.numeric(y)) { y <- 0 }
            if(!is.numeric(r)) { r <- 0 }
            if(!is.numeric(phi)) { phi <- -1 }
            else if(phi <= 0) { phi <- -1 }
            else if(phi > 0) { phi <- 1 }
            if(!is.numeric(b)) { b <- 0 }
            if(!is.numeric(c)) { c <- 0 }
            # Set to OUP ----
            A$set_oup_params(rho=rho,mu=mu,sigma=sigma)
            A$set_x_stoch_args(x=x,y=y,r=r,phi=phi,b=b,c=c)
          }
          # observe phi ----
          observe({
            if(is.numeric(input$phiADecisionOUP))
            {
              if(input$phiADecisionOUP > 0)
              {
                b <- A$get_x_stoch_args()[[7]]
                isolate({
                  updateNumericInput(session,"bcADecisionOUP",label="b",value=b)
                })
              }
              else
              {
                c <- A$get_x_stoch_args()[[8]]
                isolate({
                  updateNumericInput(session,"bcADecisionOUP",label="c",value=c)
                })
              }
            }
          }) %>% bindEvent(input$phiADecisionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # user clicks clear or save ----
          observe({
            FromUItoR6()
            A$undo_clear()
            showNotification("argument set 1 out of 1.",id="Aundo",duration=2)
          }) %>% bindEvent(input$clearADecisionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            FromUItoR6()
            n <- A$undo_save()
            showNotification(paste("argument set ",n," out of ",n,"."),id="Aundo",duration=2)
          }) %>% bindEvent(input$saveADecisionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # user clicks undn, unup, sync, axes or plot (or enter key) ----
          output$plotlyADecisionOUP <- renderPlotly({
            if(input$undnADecisionOUP > Abtns[12,1])
            {
              Abtns[12,1] <<- input$undnADecisionOUP
              Ixn <- A$undo_undo()
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="Aundo",duration=2)
            }
            else if(input$unupADecisionOUP > Abtns[12,2])
            {
              Abtns[12,2] <<- input$unupADecisionOUP
              Ixn <- A$undo_undo(1)
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="Aundo",duration=2)
            }
            else if(input$syncADecisionOUP > Abtns[12,3])
            {
              Abtns[12,3] <<- input$syncADecisionOUP
              FromUItoR6()
              A$sync_zyxt_stoch()
            }
            else if(input$axesADecisionOUP > Abtns[12,4])
            {
              Abtns[12,4] <<- input$axesADecisionOUP
              FromUItoR6()
              A$axes_x_stoch()
            }
            else if(input$plotADecisionOUP > Abtns[12,5])
            {
              Abtns[12,5] <<- input$plotADecisionOUP
              FromUItoR6()
            }
            FromR6toUI()
            A$PlotDecisionThreshold()
          }) %>% bindEvent(input$undnADecisionOUP,input$unupADecisionOUP,input$syncADecisionOUP,input$axesADecisionOUP,input$plotADecisionOUP)
          # observe info ----
          observe({
            ibutton <<- ""
            infobutton <<- "infoADecisionOUP"
            if(infotoggle()) { infotoggle(FALSE) }
            else { infotoggle(TRUE) }
          }) %>% bindEvent(input$infoADecisionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            removeModal(session)
            updateTabsetPanel(session,"navBar",selected="tabFDOUP")
            updateTabsetPanel(session,"navFDOUP",selected="FDDecisionOUP")
          }) %>% bindEvent(input$alsoADecisionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
        # Obligation ----
        else if(input$navAOUP == "AObligationOUP")
        {
          # define set/get functions ----
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
            isolate({
              updateNumericInput(session,"rhoAObligationOUP",value=rho)
              updateNumericInput(session,"muAObligationOUP",value=mu)
              updateNumericInput(session,"tAObligationOUP",value=t)
              updateNumericInput(session,"sFromAObligationOUP",value=sFrom)
              updateNumericInput(session,"sToAObligationOUP",value=sTo)
              updateNumericInput(session,"sByAObligationOUP",value=sBy)
              updateNumericInput(session,"xFromAObligationOUP",value=xFrom)
              updateNumericInput(session,"xToAObligationOUP",value=xTo)
              updateNumericInput(session,"xByAObligationOUP",value=xBy)
              updateNumericInput(session,"yAObligationOUP",value=y)
              updateNumericInput(session,"rAObligationOUP",value=r)
              updateNumericInput(session,"phiAObligationOUP",value=phi)
              updateNumericInput(session,"bAObligationOUP",value=b)
              updateNumericInput(session,"cAObligationOUP",value=c)
            })
          }
          FromUItoR6 <- function()
          {
            # Get from UI ----
            isolate({
              rho <- input$rhoAObligationOUP
              mu <- input$muAObligationOUP
              sFrom <- input$sFromAObligationOUP
              sTo <- input$sToAObligationOUP
              sBy <- input$sByAObligationOUP
              t <- input$tAObligationOUP
              xFrom <- input$xFromAObligationOUP
              xTo <- input$xToAObligationOUP
              xBy <- input$xByAObligationOUP
              y <- input$yAObligationOUP
              r <- input$rAObligationOUP
              phi <- input$phiAObligationOUP
              b <- input$bAObligationOUP
              c <- input$cAObligationOUP
            })
            if(!is.numeric(rho)) { rho <- 0 }
            else if(rho < 0) { rho <- 0 }
            if(!is.numeric(mu)) { mu <- 0 }
            s <- axissequence(sFrom,sTo,sBy)
            if(!is.numeric(t)) { t <- s[1] }
            else if(t < s[1]) { t <- s[1] }
            x <- axissequence(xFrom,xTo,xBy)
            if(!is.numeric(y)) { y <- 0 }
            if(!is.numeric(r)) { r <- 0 }
            if(!is.numeric(phi)) { phi <- -1 }
            else if(phi <= 0) { phi <- -1 }
            else if(phi > 0) { phi <- 1 }
            if(!is.numeric(b)) { b <- 0 }
            if(!is.numeric(c)) { c <- 0 }
            # Set to OUP ----
            A$set_oup_params(rho=rho,mu=mu)
            A$set_x_stoch_args(s=s,x=x,t=t,y=y,r=r,phi=phi,b=b,c=c)
          }
          # observe phi ----
          observe({
            if(is.numeric(input$phiAObligationOUP))
            {
              if(input$phiAObligationOUP > 0)
              {
                b <- A$get_x_stoch_args()[[7]]
                isolate({
                  updateNumericInput(session,"bcAObligationOUP",label="b",value=b)
                })
              }
              else
              {
                c <- A$get_x_stoch_args()[[8]]
                isolate({
                  updateNumericInput(session,"bcAObligationOUP",label="c",value=c)
                })
              }
            }
          }) %>% bindEvent(input$phiAObligationOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # user clicks clear or save ----
          observe({
            FromUItoR6()
            A$undo_clear()
            showNotification("argument set 1 out of 1.",id="Aundo",duration=2)
          }) %>% bindEvent(input$clearAObligationOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            FromUItoR6()
            n <- A$undo_save()
            showNotification(paste("argument set ",n," out of ",n,"."),id="Aundo",duration=2)
          }) %>% bindEvent(input$saveAObligationOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # user clicks undn, unup, sync, axes, plot (or enter key) or other ----
          output$plotlyAObligationOUP <- renderPlotly({
            if(input$undnAObligationOUP > Abtns[13,1])
            {
              Abtns[13,1] <<- input$undnAObligationOUP
              Ixn <- A$undo_undo()
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="Aundo",duration=2)
            }
            else if(input$unupAObligationOUP > Abtns[13,2])
            {
              Abtns[13,2] <<- input$unupAObligationOUP
              Ixn <- A$undo_undo(1)
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="Aundo",duration=2)
            }
            else if(input$syncAObligationOUP > Abtns[13,3])
            {
              Abtns[13,3] <<- input$syncAObligationOUP
              FromUItoR6()
              A$sync_zyxt_stoch()
            }
            else if(input$axesAObligationOUP > Abtns[13,4])
            {
              Abtns[13,4] <<- input$axesAObligationOUP
              FromUItoR6()
              A$axes_x_stoch()
            }
            else if(input$plotAObligationOUP > Abtns[13,5])
            {
              Abtns[13,5] <<- input$plotAObligationOUP
              FromUItoR6()
            }
            else if(input$otherAObligationOUP > Abtns[13,6])
            {
              Abtns[13,6] <<- input$otherAObligationOUP
              FromUItoR6()
              A$set_plot_type("p",4)
            }
            FromR6toUI()
            A$PlotObligation()
          }) %>% bindEvent(input$undnAObligationOUP,input$unupAObligationOUP,input$syncAObligationOUP,input$axesAObligationOUP,input$plotAObligationOUP,input$otherAObligationOUP)
          # observe info ----
          observe({
            ibutton <<- ""
            infobutton <<- "infoAObligationOUP"
            if(infotoggle()) { infotoggle(FALSE) }
            else { infotoggle(TRUE) }
          }) %>% bindEvent(input$infoAObligationOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
        # Passage Time Mode, Median and Mean ----
        else if(input$navAOUP == "APTModeMedianMeanOUP")
        {
          # define set/get functions ----
          FromR6toUI <- function()
          {
            # Get from OUP ----
            oup_params <- A$get_oup_params()
            t_stoch_args <- A$get_t_stoch_args()
            plot_args <- A$get_plot_args()
            rho <- oup_params[[1]]
            mu <- oup_params[[2]]
            sigma <- oup_params[[3]]
            t <- t_stoch_args[[1]]
            k <- t_stoch_args[[2]]
            s <- t_stoch_args[[3]]
            x <- t_stoch_args[[4]]
            z <- t_stoch_args[[5]]
            omega <- t_stoch_args[[6]]
            ptmax <- plot_args[[2]]
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
            isolate({
              updateNumericInput(session,"rhoAPTModeMedianMeanOUP",value=rho)
              updateNumericInput(session,"muAPTModeMedianMeanOUP",value=mu)
              updateNumericInput(session,"sigmaAPTModeMedianMeanOUP",value=sigma)
              updateNumericInput(session,"tFromAPTModeMedianMeanOUP",value=tFrom)
              updateNumericInput(session,"tToAPTModeMedianMeanOUP",value=tTo)
              updateNumericInput(session,"tByAPTModeMedianMeanOUP",value=tBy)
              updateNumericInput(session,"sAPTModeMedianMeanOUP",value=s)
              updateNumericInput(session,"zFromAPTModeMedianMeanOUP",value=zFrom)
              updateNumericInput(session,"zToAPTModeMedianMeanOUP",value=zTo)
              updateNumericInput(session,"zByAPTModeMedianMeanOUP",value=zBy)
              updateNumericInput(session,"kAPTModeMedianMeanOUP",value=k)
              updateNumericInput(session,"xAPTModeMedianMeanOUP",value=x)
              updateNumericInput(session,"omegaAPTModeMedianMeanOUP",value=omega)
              updateNumericInput(session,"ptmaxAPTModeMedianMeanOUP",value=ptmax)
            })
          }
          FromUItoR6 <- function()
          {
            # Get from UI ----
            isolate({
              rho <- input$rhoAPTModeMedianMeanOUP
              mu <- input$muAPTModeMedianMeanOUP
              sigma <- input$sigmaAPTModeMedianMeanOUP
              tFrom <- input$tFromAPTModeMedianMeanOUP
              tTo <- input$tToAPTModeMedianMeanOUP
              tBy <- input$tByAPTModeMedianMeanOUP
              s <- input$sAPTModeMedianMeanOUP
              zFrom <- input$zFromAPTModeMedianMeanOUP
              zTo <- input$zToAPTModeMedianMeanOUP
              zBy <- input$zByAPTModeMedianMeanOUP
              k <- input$kAPTModeMedianMeanOUP
              x <- input$xAPTModeMedianMeanOUP
              omega <- input$omegaAPTModeMedianMeanOUP
              ptmax <- input$ptmaxAPTModeMedianMeanOUP
            })
            if(!is.numeric(rho)) { rho <- 0 }
            else if(rho < 0) { rho <- 0 }
            if(!is.numeric(mu)) { mu <- 0 }
            if(!is.numeric(sigma)) { sigma <- 0 }
            t <- axissequence(tFrom,tTo,tBy)
            if(!is.numeric(s)) { s <- t[1] }
            else if(s > t[1]) { s <- t[1] }
            z <- axissequence(zFrom,zTo,zBy)
            if(!is.numeric(k)) { k <- 0 }
            if(!is.numeric(x)) { x <- -mu }
            if(!is.numeric(omega)) { omega <- 1 }
            else if(omega < 0) { omega <- 0 }
            else if(omega > 1) { omega <- 1 }
            if(!is.numeric(ptmax)) { ptmax <- NaN }
            # Set to OUP ----
            A$set_oup_params(rho=rho,mu=mu,sigma=sigma)
            A$set_t_stoch_args(t=t,k=k,s=s,x=x,z=z,omega=omega)
            A$set_plot_args(ptmax=ptmax)
          }
          # user clicks clear or save ----
          observe({
            FromUItoR6()
            A$undo_clear()
            showNotification("argument set 1 out of 1.",id="Aundo",duration=2)
          }) %>% bindEvent(input$clearAPTModeMedianMeanOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            FromUItoR6()
            n <- A$undo_save()
            showNotification(paste("argument set ",n," out of ",n,"."),id="Aundo",duration=2)
          }) %>% bindEvent(input$saveAPTModeMedianMeanOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # user clicks undn, unup, sync, axes, plot (or enter key), left or rght ----
          output$plotlyAPTModeMedianMeanOUP <- renderPlotly({
            if(input$undnAPTModeMedianMeanOUP > Abtns[14,1])
            {
              Abtns[14,1] <<- input$undnAPTModeMedianMeanOUP
              Ixn <- A$undo_undo()
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="Aundo",duration=2)
            }
            else if(input$unupAPTModeMedianMeanOUP > Abtns[14,2])
            {
              Abtns[14,2] <<- input$unupAPTModeMedianMeanOUP
              Ixn <- A$undo_undo(1)
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="Aundo",duration=2)
            }
            else if(input$syncAPTModeMedianMeanOUP > Abtns[14,3])
            {
              Abtns[14,3] <<- input$syncAPTModeMedianMeanOUP
              FromUItoR6()
              A$sync_zyxt_stoch()
            }
            else if(input$axesAPTModeMedianMeanOUP > Abtns[14,4])
            {
              Abtns[14,4] <<- input$axesAPTModeMedianMeanOUP
              FromUItoR6()
              A$axes_t_stoch()
            }
            else if(input$plotAPTModeMedianMeanOUP > Abtns[14,5])
            {
              Abtns[14,5] <<- input$plotAPTModeMedianMeanOUP
              FromUItoR6()
            }
            else if(input$leftAPTModeMedianMeanOUP > Abtns[14,6])
            {
              Abtns[14,6] <<- input$leftAPTModeMedianMeanOUP
              FromUItoR6()
              A$set_plot_type("p",5)
            }
            else if(input$rghtAPTModeMedianMeanOUP > Abtns[14,7])
            {
              Abtns[14,7] <<- input$rghtAPTModeMedianMeanOUP
              FromUItoR6()
              A$set_plot_type("n",5)
            }
            FromR6toUI()
            A$PlotPassageTimeModeMedianMean()
          }) %>% bindEvent(input$undnAPTModeMedianMeanOUP,input$unupAPTModeMedianMeanOUP,input$syncAPTModeMedianMeanOUP,input$axesAPTModeMedianMeanOUP,input$plotAPTModeMedianMeanOUP,input$leftAPTModeMedianMeanOUP,input$rghtAPTModeMedianMeanOUP)
          # observe info ----
          observe({
            ibutton <<- ""
            infobutton <<- "infoAPTModeMedianMeanOUP"
            if(infotoggle()) { infotoggle(FALSE) }
            else { infotoggle(TRUE) }
          }) %>% bindEvent(input$infoAPTModeMedianMeanOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            removeModal(session)
            updateTabsetPanel(session,"navBar",selected="tabMCOUP")
            omega <- A$get_t_stoch_args()[[6]]
            if(omega < 0.5) { updateTabsetPanel(session,"navMCOUP",selected="MCVTModeMedianMeanOUP") }
            else { updateTabsetPanel(session,"navMCOUP",selected="MCFPTModeMedianMeanOUP") }
          }) %>% bindEvent(input$alsoAPTModeMedianMeanOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
        # Passage Time Percentiles ----
        else if(input$navAOUP == "APTPercentilesOUP")
        {
          # define set/get functions ----
          FromR6toUI <- function()
          {
            # Get from OUP ----
            oup_params <- A$get_oup_params()
            t_stoch_args <- A$get_t_stoch_args()
            plot_args <- A$get_plot_args()
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
            ptmax <- plot_args[[2]]
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
            isolate({
              updateNumericInput(session,"rhoAPTPercentilesOUP",value=rho)
              updateNumericInput(session,"muAPTPercentilesOUP",value=mu)
              updateNumericInput(session,"sigmaAPTPercentilesOUP",value=sigma)
              updateNumericInput(session,"tFromAPTPercentilesOUP",value=tFrom)
              updateNumericInput(session,"tToAPTPercentilesOUP",value=tTo)
              updateNumericInput(session,"tByAPTPercentilesOUP",value=tBy)
              updateNumericInput(session,"sAPTPercentilesOUP",value=s)
              updateNumericInput(session,"zFromAPTPercentilesOUP",value=zFrom)
              updateNumericInput(session,"zToAPTPercentilesOUP",value=zTo)
              updateNumericInput(session,"zByAPTPercentilesOUP",value=zBy)
              updateNumericInput(session,"kAPTPercentilesOUP",value=k)
              updateNumericInput(session,"xAPTPercentilesOUP",value=x)
              updateNumericInput(session,"omegaAPTPercentilesOUP",value=omega)
              updateNumericInput(session,"PpctAPTPercentilesOUP",value=Ppct)
              updateNumericInput(session,"ptmaxAPTPercentilesOUP",value=ptmax)
            })
          }
          FromUItoR6 <- function()
          {
            # Get from UI ----
            isolate({
              rho <- input$rhoAPTPercentilesOUP
              mu <- input$muAPTPercentilesOUP
              sigma <- input$sigmaAPTPercentilesOUP
              tFrom <- input$tFromAPTPercentilesOUP
              tTo <- input$tToAPTPercentilesOUP
              tBy <- input$tByAPTPercentilesOUP
              s <- input$sAPTPercentilesOUP
              zFrom <- input$zFromAPTPercentilesOUP
              zTo <- input$zToAPTPercentilesOUP
              zBy <- input$zByAPTPercentilesOUP
              k <- input$kAPTPercentilesOUP
              x <- input$xAPTPercentilesOUP
              omega <- input$omegaAPTPercentilesOUP
              Ppct <- input$PpctAPTPercentilesOUP
              ptmax <- input$ptmaxAPTPercentilesOUP
            })
            if(!is.numeric(rho)) { rho <- 0 }
            else if(rho < 0) { rho <- 0 }
            if(!is.numeric(mu)) { mu <- 0 }
            if(!is.numeric(sigma)) { sigma <- 0 }
            t <- axissequence(tFrom,tTo,tBy)
            if(!is.numeric(s)) { s <- t[1] }
            else if(s > t[1]) { s <- t[1] }
            z <- axissequence(zFrom,zTo,zBy)
            if(!is.numeric(k)) { k <- 0 }
            if(!is.numeric(x)) { x <- -mu }
            if(!is.numeric(omega)) { omega <- 1 }
            else if(omega < 0) { omega <- 0 }
            else if(omega > 1) { omega <- 1 }
            if(!is.numeric(Ppct)) { Ppct <- 0.75 }
            else if(Ppct < 0.01) { Ppct <- 0.01 }
            else if(Ppct > 0.99) { Ppct <- 0.99 }
            if(!is.numeric(ptmax)) { ptmax <- NaN }
            # Set to OUP ----
            A$set_oup_params(rho=rho,mu=mu,sigma=sigma)
            A$set_t_stoch_args(t=t,k=k,s=s,x=x,z=z,omega=omega,Ppct=Ppct)
            A$set_plot_args(ptmax=ptmax)
          }
          # user clicks clear or save ----
          observe({
            FromUItoR6()
            A$undo_clear()
            showNotification("argument set 1 out of 1.",id="Aundo",duration=2)
          }) %>% bindEvent(input$clearAPTPercentilesOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            FromUItoR6()
            n <- A$undo_save()
            showNotification(paste("argument set ",n," out of ",n,"."),id="Aundo",duration=2)
          }) %>% bindEvent(input$saveAPTPercentilesOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # user clicks undn, unup, sync, axes, plot (or enter key), left or rght ----
          output$plotlyAPTPercentilesOUP <- renderPlotly({
            if(input$undnAPTPercentilesOUP > Abtns[15,1])
            {
              Abtns[15,1] <<- input$undnAPTPercentilesOUP
              Ixn <- A$undo_undo()
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="Aundo",duration=2)
            }
            else if(input$unupAPTPercentilesOUP > Abtns[15,2])
            {
              Abtns[15,2] <<- input$unupAPTPercentilesOUP
              Ixn <- A$undo_undo(1)
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="Aundo",duration=2)
            }
            else if(input$syncAPTPercentilesOUP > Abtns[15,3])
            {
              Abtns[15,3] <<- input$syncAPTPercentilesOUP
              FromUItoR6()
              A$sync_zyxt_stoch()
            }
            else if(input$axesAPTPercentilesOUP > Abtns[15,4])
            {
              Abtns[15,4] <<- input$axesAPTPercentilesOUP
              FromUItoR6()
              A$axes_t_stoch()
            }
            else if(input$plotAPTPercentilesOUP > Abtns[15,5])
            {
              Abtns[15,5] <<- input$plotAPTPercentilesOUP
              FromUItoR6()
            }
            else if(input$leftAPTPercentilesOUP > Abtns[15,6])
            {
              Abtns[15,6] <<- input$leftAPTPercentilesOUP
              FromUItoR6()
              A$set_plot_type("p",5)
            }
            else if(input$rghtAPTPercentilesOUP > Abtns[15,7])
            {
              Abtns[15,7] <<- input$rghtAPTPercentilesOUP
              FromUItoR6()
              A$set_plot_type("n",5)
            }
            FromR6toUI()
            A$PlotPassageTimePercentiles()
          }) %>% bindEvent(input$undnAPTPercentilesOUP,input$unupAPTPercentilesOUP,input$syncAPTPercentilesOUP,input$axesAPTPercentilesOUP,input$plotAPTPercentilesOUP,input$leftAPTPercentilesOUP,input$rghtAPTPercentilesOUP)
          # observe info ----
          observe({
            ibutton <<- ""
            infobutton <<- "infoAPTPercentilesOUP"
            if(infotoggle()) { infotoggle(FALSE) }
            else { infotoggle(TRUE) }
          }) %>% bindEvent(input$infoAPTPercentilesOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            removeModal(session)
            updateTabsetPanel(session,"navBar",selected="tabMCOUP")
            omega <- A$get_t_stoch_args()[[6]]
            if(omega < 0.5) { updateTabsetPanel(session,"navMCOUP",selected="MCVTPercentilesOUP") }
            else { updateTabsetPanel(session,"navMCOUP",selected="MCFPTPercentilesOUP") }
          }) %>% bindEvent(input$alsoAPTPercentilesOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
        # Passage Time Density ----
        else if(input$navAOUP == "APTDensityOUP")
        {
          # define set/get functions ----
          FromR6toUI <- function()
          {
            # Get from OUP ----
            oup_params <- A$get_oup_params()
            t_stoch_args <- A$get_t_stoch_args()
            plot_args <- A$get_plot_args()
            rho <- oup_params[[1]]
            mu <- oup_params[[2]]
            sigma <- oup_params[[3]]
            t <- t_stoch_args[[1]]
            k <- t_stoch_args[[2]]
            s <- t_stoch_args[[3]]
            x <- t_stoch_args[[4]]
            z <- t_stoch_args[[5]]
            omega <- t_stoch_args[[6]]
            ptmax <- plot_args[[2]]
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
            isolate({
              updateNumericInput(session,"rhoAPTDensityOUP",value=rho)
              updateNumericInput(session,"muAPTDensityOUP",value=mu)
              updateNumericInput(session,"sigmaAPTDensityOUP",value=sigma)
              updateNumericInput(session,"tFromAPTDensityOUP",value=tFrom)
              updateNumericInput(session,"tToAPTDensityOUP",value=tTo)
              updateNumericInput(session,"tByAPTDensityOUP",value=tBy)
              updateNumericInput(session,"sAPTDensityOUP",value=s)
              updateNumericInput(session,"zFromAPTDensityOUP",value=zFrom)
              updateNumericInput(session,"zToAPTDensityOUP",value=zTo)
              updateNumericInput(session,"zByAPTDensityOUP",value=zBy)
              updateNumericInput(session,"kAPTDensityOUP",value=k)
              updateNumericInput(session,"xAPTDensityOUP",value=x)
              updateNumericInput(session,"omegaAPTDensityOUP",value=omega)
              updateNumericInput(session,"ptmaxAPTDensityOUP",value=ptmax)
            })
          }
          FromUItoR6 <- function()
          {
            # Get from UI ----
            rho <- input$rhoAPTDensityOUP
            mu <- input$muAPTDensityOUP
            sigma <- input$sigmaAPTDensityOUP
            tFrom <- input$tFromAPTDensityOUP
            tTo <- input$tToAPTDensityOUP
            tBy <- input$tByAPTDensityOUP
            s <- input$sAPTDensityOUP
            zFrom <- input$zFromAPTDensityOUP
            zTo <- input$zToAPTDensityOUP
            zBy <- input$zByAPTDensityOUP
            k <- input$kAPTDensityOUP
            x <- input$xAPTDensityOUP
            omega <- input$omegaAPTDensityOUP
            ptmax <- input$ptmaxAPTDensityOUP
            if(!is.numeric(rho)) { rho <- 0 }
            else if(rho < 0) { rho <- 0 }
            if(!is.numeric(mu)) { mu <- 0 }
            if(!is.numeric(sigma)) { sigma <- 0 }
            t <- axissequence(tFrom,tTo,tBy)
            if(!is.numeric(s)) { s <- t[1] }
            else if(s > t[1]) { s <- t[1] }
            z <- axissequence(zFrom,zTo,zBy)
            if(!is.numeric(k)) { k <- 0 }
            if(!is.numeric(x)) { x <- -mu }
            if(!is.numeric(omega)) { omega <- 1 }
            else if(omega < 0) { omega <- 0 }
            else if(omega > 1) { omega <- 1 }
            if(!is.numeric(ptmax)) { ptmax <- NaN }
            # Set to OUP ----
            A$set_oup_params(rho=rho,mu=mu,sigma=sigma)
            A$set_t_stoch_args(t=t,k=k,s=s,x=x,z=z,omega=omega)
            A$set_plot_args(ptmax=ptmax)
          }
          # user clicks clear or save ----
          observe({
            FromUItoR6()
            A$undo_clear()
            showNotification("argument set 1 out of 1.",id="Aundo",duration=2)
          }) %>% bindEvent(input$clearAPTDensityOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            FromUItoR6()
            n <- A$undo_save()
            showNotification(paste("argument set ",n," out of ",n,"."),id="Aundo",duration=2)
          }) %>% bindEvent(input$saveAPTDensityOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # user clicks undn, unup, sync, axes, plot (or enter key) or other ----
          output$plotlyAPTDensityOUP <- renderPlotly({
            if(input$undnAPTDensityOUP > Abtns[16,1])
            {
              Abtns[16,1] <<- input$undnAPTDensityOUP
              Ixn <- A$undo_undo()
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="Aundo",duration=2)
            }
            else if(input$unupAPTDensityOUP > Abtns[16,2])
            {
              Abtns[16,2] <<- input$unupAPTDensityOUP
              Ixn <- A$undo_undo(1)
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="Aundo",duration=2)
            }
            else if(input$syncAPTDensityOUP > Abtns[16,3])
            {
              Abtns[16,3] <<- input$syncAPTDensityOUP
              FromUItoR6()
              A$sync_zyxt_stoch()
            }
            else if(input$axesAPTDensityOUP > Abtns[16,4])
            {
              Abtns[16,4] <<- input$axesAPTDensityOUP
              FromUItoR6()
              A$axes_t_stoch()
            }
            else if(input$plotAPTDensityOUP > Abtns[16,5])
            {
              Abtns[16,5] <<- input$plotAPTDensityOUP
              FromUItoR6()
            }
            else if(input$otherAPTDensityOUP > Abtns[16,6])
            {
              Abtns[16,6] <<- input$otherAPTDensityOUP
              FromUItoR6()
              A$set_plot_type("p",6)
            }
            FromR6toUI()
            A$PlotPassageTimeDensity()
          }) %>% bindEvent(input$undnAPTDensityOUP,input$unupAPTDensityOUP,input$syncAPTDensityOUP,input$axesAPTDensityOUP,input$plotAPTDensityOUP,input$otherAPTDensityOUP)
          # user clicks info ----
          observe({
            ibutton <<- ""
            infobutton <<- "infoAPTDensityOUP"
            if(infotoggle()) { infotoggle(FALSE) }
            else { infotoggle(TRUE) }
          }) %>% bindEvent(input$infoAPTDensityOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            removeModal(session)
            updateTabsetPanel(session,"navBar",selected="tabMCOUP")
            omega <- A$get_t_stoch_args()[[6]]
            if(omega < 0.5) { updateTabsetPanel(session,"navMCOUP",selected="MCVTDensityOUP") }
            else { updateTabsetPanel(session,"navMCOUP",selected="MCFPTDensityOUP") }
          }) %>% bindEvent(input$alsoAPTDensityOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
        # Passage Time Probability ----
        else if(input$navAOUP == "APTProbabilityOUP")
        {
          # define set/get functions ----
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
            isolate({
              updateNumericInput(session,"rhoAPTProbabilityOUP",value=rho)
              updateNumericInput(session,"muAPTProbabilityOUP",value=mu)
              updateNumericInput(session,"sigmaAPTProbabilityOUP",value=sigma)
              updateNumericInput(session,"tFromAPTProbabilityOUP",value=tFrom)
              updateNumericInput(session,"tToAPTProbabilityOUP",value=tTo)
              updateNumericInput(session,"tByAPTProbabilityOUP",value=tBy)
              updateNumericInput(session,"sAPTProbabilityOUP",value=s)
              updateNumericInput(session,"zFromAPTProbabilityOUP",value=zFrom)
              updateNumericInput(session,"zToAPTProbabilityOUP",value=zTo)
              updateNumericInput(session,"zByAPTProbabilityOUP",value=zBy)
              updateNumericInput(session,"kAPTProbabilityOUP",value=k)
              updateNumericInput(session,"xAPTProbabilityOUP",value=x)
              updateNumericInput(session,"omegaAPTProbabilityOUP",value=omega)
            })
          }
          FromUItoR6 <- function()
          {
            # Get from UI ----
            rho <- input$rhoAPTProbabilityOUP
            mu <- input$muAPTProbabilityOUP
            sigma <- input$sigmaAPTProbabilityOUP
            tFrom <- input$tFromAPTProbabilityOUP
            tTo <- input$tToAPTProbabilityOUP
            tBy <- input$tByAPTProbabilityOUP
            s <- input$sAPTProbabilityOUP
            zFrom <- input$zFromAPTProbabilityOUP
            zTo <- input$zToAPTProbabilityOUP
            zBy <- input$zByAPTProbabilityOUP
            k <- input$kAPTProbabilityOUP
            x <- input$xAPTProbabilityOUP
            omega <- input$omegaAPTProbabilityOUP
            if(!is.numeric(rho)) { rho <- 0 }
            else if(rho < 0) { rho <- 0 }
            if(!is.numeric(mu)) { mu <- 0 }
            if(!is.numeric(sigma)) { sigma <- 0 }
            t <- axissequence(tFrom,tTo,tBy)
            if(!is.numeric(s)) { s <- t[1] }
            else if(s > t[1]) { s <- t[1] }
            z <- axissequence(zFrom,zTo,zBy)
            if(!is.numeric(k)) { k <- 0 }
            if(!is.numeric(x)) { x <- -mu }
            if(!is.numeric(omega)) { omega <- 1 }
            else if(omega < 0) { omega <- 0 }
            else if(omega > 1) { omega <- 1 }
            # Set to OUP ----
            A$set_oup_params(rho=rho,mu=mu,sigma=sigma)
            A$set_t_stoch_args(t=t,k=k,s=s,x=x,z=z,omega=omega)
          }
          # user clicks clear or save ----
          observe({
            FromUItoR6()
            A$undo_clear()
            showNotification("argument set 1 out of 1.",id="Aundo",duration=2)
          }) %>% bindEvent(input$clearAPTProbabilityOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            FromUItoR6()
            n <- A$undo_save()
            showNotification(paste("argument set ",n," out of ",n,"."),id="Aundo",duration=2)
          }) %>% bindEvent(input$saveAPTProbabilityOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # user clicks undn, unup, sync, axes, plot (or enter key) or other ----
          output$plotlyAPTProbabilityOUP <- renderPlotly({
            if(input$undnAPTProbabilityOUP > Abtns[17,1])
            {
              Abtns[17,1] <<- input$undnAPTProbabilityOUP
              Ixn <- A$undo_undo()
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="Aundo",duration=2)
            }
            else if(input$unupAPTProbabilityOUP > Abtns[17,2])
            {
              Abtns[17,2] <<- input$unupAPTProbabilityOUP
              Ixn <- A$undo_undo(1)
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="Aundo",duration=2)
            }
            else if(input$syncAPTProbabilityOUP > Abtns[17,3])
            {
              Abtns[17,3] <<- input$syncAPTProbabilityOUP
              FromUItoR6()
              A$sync_zyxt_stoch()
            }
            else if(input$axesAPTProbabilityOUP > Abtns[17,4])
            {
              Abtns[17,4] <<- input$axesAPTProbabilityOUP
              FromUItoR6()
              A$axes_t_stoch()
            }
            else if(input$plotAPTProbabilityOUP > Abtns[17,5])
            {
              Abtns[17,5] <<- input$plotAPTProbabilityOUP
              FromUItoR6()
            }
            else if(input$otherAPTProbabilityOUP > Abtns[17,6])
            {
              Abtns[17,6] <<- input$otherAPTProbabilityOUP
              FromUItoR6()
              A$set_plot_type("p",6)
            }
            FromR6toUI()
            A$PlotPassageTimeProbability()
          }) %>% bindEvent(input$undnAPTProbabilityOUP,input$unupAPTProbabilityOUP,input$syncAPTProbabilityOUP,input$axesAPTProbabilityOUP,input$plotAPTProbabilityOUP,input$otherAPTProbabilityOUP)
          # observe info ----
          observe({
            ibutton <<- ""
            infobutton <<- "infoAPTProbabilityOUP"
            if(infotoggle()) { infotoggle(FALSE) }
            else { infotoggle(TRUE) }
          }) %>% bindEvent(input$infoAPTProbabilityOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            removeModal(session)
            updateTabsetPanel(session,"navBar",selected="tabMCOUP")
            omega <- A$get_t_stoch_args()[[6]]
            if(omega < 0.5) { updateTabsetPanel(session,"navMCOUP",selected="MCVTProbabilityOUP") }
            else { updateTabsetPanel(session,"navMCOUP",selected="MCFPTProbabilityOUP") }
          }) %>% bindEvent(input$alsoAPTProbabilityOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
      })
    }
    else if(input$navBar == "tabFDOUP")
    {
      observeEvent(input$navFDOUP,{
        # Drift ----
        if(input$navFDOUP == "FDDriftOUP")
        {
          # define set/get functions ----
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
            isolate({
              updateNumericInput(session,"rhoFDDriftOUP",value=rho)
              updateNumericInput(session,"muFDDriftOUP",value=mu)
              updateNumericInput(session,"xFromFDDriftOUP",value=xFrom)
              updateNumericInput(session,"xToFDDriftOUP",value=xTo)
              updateNumericInput(session,"xByFDDriftOUP",value=xBy)
            })
          }
          FromUItoR6 <- function()
          {
            # Get from UI ----
            isolate({
              rho <- input$rhoFDDriftOUP
              mu <- input$muFDDriftOUP
              xFrom <- input$xFromFDDriftOUP
              xTo <- input$xToFDDriftOUP
              xBy <- input$xByFDDriftOUP
            })
            if(!is.numeric(rho)) { rho <- 0 }
            else if(rho < 0) { rho <- 0 }
            if(!is.numeric(mu)) { mu <- 0 }
            x <- axissequence(xFrom,xTo,xBy)
            # Set to OUP ----
            FD$set_oup_params(rho=rho,mu=mu)
            FD$set_x_stoch_args(x=x)
          }
          # user clicks clear or save ----
          observe({
            FromUItoR6()
            FD$undo_clear()
            showNotification("argument set 1 out of 1.",id="FDundo",duration=2)
          }) %>% bindEvent(input$clearFDDriftOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            FromUItoR6()
            n <- FD$undo_save()
            showNotification(paste("argument set ",n," out of ",n,"."),id="FDundo",duration=2)
          }) %>% bindEvent(input$saveFDDriftOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # user clicks undn, unup, axes or plot (or enter key) ----
          output$plotlyFDDriftOUP <- renderPlotly({
            if(input$undnFDDriftOUP > FDbtns[1,1])
            {
              FDbtns[1,1] <<- input$undnFDDriftOUP
              Ixn <- FD$undo_undo()
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="FDundo",duration=2)
            }
            else if(input$unupFDDriftOUP > FDbtns[1,2])
            {
              FDbtns[1,2] <<- input$unupFDDriftOUP
              Ixn <- FD$undo_undo(1)
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="FDundo",duration=2)
            }
            else if(input$axesFDDriftOUP > FDbtns[1,3])
            {
              FDbtns[1,3] <<- input$axesFDDriftOUP
              FromUItoR6()
              FD$axes_x_stoch()
            }
            else if(input$plotFDDriftOUP > FDbtns[1,4])
            {
              FDbtns[1,4] <<- input$plotFDDriftOUP
              FromUItoR6()
            }
            FromR6toUI()
            FD$PlotDrift()
          }) %>% bindEvent(input$undnFDDriftOUP,input$unupFDDriftOUP,input$axesFDDriftOUP,input$plotFDDriftOUP)
          # observe info ----
          observe({
            ibutton <<- ""
            infobutton <<- "infoFDDriftOUP"
            if(infotoggle()) { infotoggle(FALSE) }
            else { infotoggle(TRUE) }
          }) %>% bindEvent(input$infoFDDriftOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            removeModal(session)
            updateTabsetPanel(session,"navBar",selected="tabAOUP")
            updateTabsetPanel(session,"navAOUP",selected="ADriftOUP")
          }) %>% bindEvent(input$alsoFDDriftOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
        # Diffusion ----
        if(input$navFDOUP == "FDDiffusionOUP")
        {
          # define set/get functions ----
          FromR6toUI <- function()
          {
            # Get from OUP ----
            oup_params <- FD$get_oup_params()
            x_stoch_args <- FD$get_x_stoch_args()
            type <- FD$get_plot_types()[[1]][2]
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
            isolate({
              if(type < -0.5)
              {
                updateNumericInput(session,"rhoFDDiffusionOUP",label="rho",value=rho)
                updateNumericInput(session,"muFDDiffusionOUP",label="mu",value=mu)
              }
              else
              {
                updateNumericInput(session,"rhoFDDiffusionOUP",label="~",value=rho)
                updateNumericInput(session,"muFDDiffusionOUP",label="~",value=mu)
              }
              updateNumericInput(session,"sigmaFDDiffusionOUP",value=sigma)
              updateNumericInput(session,"xFromFDDiffusionOUP",value=xFrom)
              updateNumericInput(session,"xToFDDiffusionOUP",value=xTo)
              updateNumericInput(session,"xByFDDiffusionOUP",value=xBy)
            })
          }
          FromUItoR6 <- function()
          {
            # Get from UI ----
            isolate({
              rho <- input$rhoFDDiffusionOUP
              mu <- input$muFDDiffusionOUP
              sigma <- input$sigmaFDDiffusionOUP
              xFrom <- input$xFromFDDiffusionOUP
              xTo <- input$xToFDDiffusionOUP
              xBy <- input$xByFDDiffusionOUP
            })
            if(!is.numeric(rho)) { rho <- 0 }
            else if(rho < 0) { rho <- 0 }
            if(!is.numeric(mu)) { mu <- 0 }
            if(!is.numeric(sigma)) { sigma <- 0 }
            x <- axissequence(xFrom,xTo,xBy)
            # Set to OUP ----
            FD$set_oup_params(rho=rho,mu=mu,sigma=sigma)
            FD$set_x_stoch_args(x=x)
          }
          # user clicks clear or save ----
          observe({
            FromUItoR6()
            FD$undo_clear()
            showNotification("argument set 1 out of 1.",id="FDundo",duration=2)
          }) %>% bindEvent(input$clearFDDiffusionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            FromUItoR6()
            n <- FD$undo_save()
            showNotification(paste("argument set ",n," out of ",n,"."),id="FDundo",duration=2)
          }) %>% bindEvent(input$saveFDDiffusionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # user clicks undn, unup, axes, plot (or enter key) or other ----
          output$plotlyFDDiffusionOUP <- renderPlotly({
            if(input$undnFDDiffusionOUP > FDbtns[2,1])
            {
              FDbtns[2,1] <<- input$undnFDDiffusionOUP
              Ixn <- FD$undo_undo()
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="FDundo",duration=2)
            }
            else if(input$unupFDDiffusionOUP > FDbtns[2,2])
            {
              FDbtns[2,2] <<- input$unupFDDiffusionOUP
              Ixn <- FD$undo_undo(1)
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="FDundo",duration=2)
            }
            else if(input$axesFDDiffusionOUP > FDbtns[2,3])
            {
              FDbtns[2,3] <<- input$axesFDDiffusionOUP
              FromUItoR6()
              FD$axes_x_stoch()
            }
            else if(input$plotFDDiffusionOUP > FDbtns[2,4])
            {
              FDbtns[2,4] <<- input$plotFDDiffusionOUP
              FromUItoR6()
            }
            else if(input$otherFDDiffusionOUP > FDbtns[2,5])
            {
              FDbtns[2,5] <<- input$otherFDDiffusionOUP
              FromUItoR6()
              FD$set_plot_type("p",2)
            }
            FromR6toUI()
            FD$PlotDiffusion()
          }) %>% bindEvent(input$undnFDDiffusionOUP,input$unupFDDiffusionOUP,input$axesFDDiffusionOUP,input$plotFDDiffusionOUP,input$otherFDDiffusionOUP)
          # observe info ----
          observe({
            ibutton <<- ""
            infobutton <<- "infoFDDiffusionOUP"
            if(infotoggle()) { infotoggle(FALSE) }
            else { infotoggle(TRUE) }
          }) %>% bindEvent(input$infoFDDiffusionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            removeModal(session)
            updateTabsetPanel(session,"navBar",selected="tabAOUP")
            updateTabsetPanel(session,"navAOUP",selected="ADiffusionOUP")
          }) %>% bindEvent(input$alsoFDDiffusionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
        # Terminal Values ----
        if(input$navFDOUP == "FDTerminalOUP")
        {
          # define terminal function ----
          TerminalChoices <- function()
          {
            if(populate[1])
            {
              V_info <- FD$get_V_info()
              isolate({
                updateSelectInput(session,"VFDTerminalOUP",choices=V_info[[3]],selected=V_info[[2]])
              })
              populate[1] <<- FALSE
            }
            else
            {
              V_info <- FD$get_V_info()
              isolate({
                updateSelectInput(session,"VFDTerminalOUP",selected=V_info[[2]])
              })
            }
          }
          # define set/get functions ----
          FromR6toUI <- function()
          {
            # Get from OUP ----
            x_stoch_args <- FD$get_x_stoch_args()
            V_args <- FD$get_V_args()
            V_info <- FD$get_V_info()
            x <- x_stoch_args[[2]]
            name <- V_info[[2]]
            n <- length(x)
            xFrom <- x[1]
            xTo <- x[n]
            if(n > 1) { xBy <- (xTo-xFrom)/(n-1) }
            else  {xBy <- 0 }
            # Set to UI ----
            isolate({
              updateSelectInput(session,"VFDTerminalOUP",selected=name)
              updateNumericInput(session,"xFromFDTerminalOUP",value=xFrom)
              updateNumericInput(session,"xToFDTerminalOUP",value=xTo)
              updateNumericInput(session,"xByFDTerminalOUP",value=xBy)
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
                if(i == 1) { updateNumericInput(session,"V1FDTerminalOUP",label="~",value="") }
                else if(i == 2) { updateNumericInput(session,"V2FDTerminalOUP",label="~",value="") }
                else if(i == 3) { updateNumericInput(session,"V3FDTerminalOUP",label="~",value="") }
                else if(i == 4) { updateNumericInput(session,"V4FDTerminalOUP",label="~",value="") }
                else if(i == 5) { updateNumericInput(session,"V5FDTerminalOUP",label="~",value="") }
              }
            })
          }
          FromUItoR6 <- function()
          {
            # Get from UI ----
            isolate({
              xFrom <- input$xFromFDTerminalOUP
              xTo <- input$xToFDTerminalOUP
              xBy <- input$xByFDTerminalOUP
              v1 <- input$V1FDTerminalOUP
              v2 <- input$V2FDTerminalOUP
              v3 <- input$V3FDTerminalOUP
              v4 <- input$V4FDTerminalOUP
              v5 <- input$V5FDTerminalOUP
            })
            x <- axissequence(xFrom,xTo,xBy)
            if(is.na(v1)) { v1 <- NULL }
            if(is.na(v2)) { v2 <- NULL }
            if(is.na(v3)) { v3 <- NULL }
            if(is.na(v4)) { v4 <- NULL }
            if(is.na(v5)) { v5 <- NULL }
            # Set to OUP ----
            FD$set_x_stoch_args(x=x)
            FD$set_V_args(NULL,NULL,v1,v2,v3,v4,v5)
          }
          # initialize ----
          TerminalChoices()
          # select ----
          observe({
            isolate({
              FD$set_V_info(NULL,input$VFDTerminalOUP)
            })
            FromR6toUI()
          }) %>% bindEvent(input$VFDTerminalOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # user clicks clear or save ----
          observe({
            FromUItoR6()
            FD$undo_clear()
            showNotification("argument set 1 out of 1.",id="FDundo",duration=2)
          }) %>% bindEvent(input$clearFDTerminalOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            FromUItoR6()
            n <- FD$undo_save()
            showNotification(paste("argument set ",n," out of ",n,"."),id="FDundo",duration=2)
          }) %>% bindEvent(input$saveFDTerminalOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # user clicks undn, unup, axes or plot (or enter key) ----
          output$plotlyFDTerminalOUP <- renderPlotly({
            if(input$undnFDTerminalOUP > FDbtns[3,1])
            {
              FDbtns[3,1] <<- input$undnFDTerminalOUP
              Ixn <- FD$undo_undo()
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="FDundo",duration=2)
            }
            else if(input$unupFDTerminalOUP > FDbtns[3,2])
            {
              FDbtns[3,2] <<- input$unupFDTerminalOUP
              Ixn <- FD$undo_undo(1)
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="FDundo",duration=2)
            }
            else if(input$axesFDTerminalOUP > FDbtns[3,3])
            {
              FDbtns[3,3] <<- input$axesFDTerminalOUP
              FromUItoR6()
              FD$axes_x_stoch()
            }
            else if(input$plotFDTerminalOUP > FDbtns[3,4])
            {
              FDbtns[3,4] <<- input$plotFDTerminalOUP
              FromUItoR6()
            }
            FromR6toUI()
            FD$PlotTerminalValue()
          }) %>% bindEvent(input$undnFDTerminalOUP,input$unupFDTerminalOUP,input$axesFDTerminalOUP,input$plotFDTerminalOUP)
          # observe info ----
          observe({
            ibutton <<- ""
            infobutton <<- "infoFDTerminalOUP"
            if(infotoggle()) { infotoggle(FALSE) }
            else { infotoggle(TRUE) }
          }) %>% bindEvent(input$infoFDTerminalOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
        # Option ----
        if(input$navFDOUP == "FDOptionOUP")
        {
          # define terminal function ----
          TerminalChoices <- function()
          {
            if(populate[2])
            {
              V_info <- FD$get_V_info()
              isolate({
                updateSelectInput(session,"VFDOptionOUP",choices=V_info[[3]],selected=V_info[[2]])
              })
              populate[2] <<- FALSE
            }
            else
            {
              V_info <- FD$get_V_info()
              isolate({
                updateSelectInput(session,"VFDOptionOUP",selected=V_info[[2]])
              })
            }
          }
          # define set/get functions ----
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
            skip <- x_stoch_args[[7]]
            name <- V_info[[2]]
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
            isolate({
              updateSelectInput(session,"VFDOptionOUP",selected=name)
              updateNumericInput(session,"rhoFDOptionOUP",value=rho)
              updateNumericInput(session,"muFDOptionOUP",value=mu)
              updateNumericInput(session,"sigmaFDOptionOUP",value=sigma)
              updateNumericInput(session,"rFDOptionOUP",value=r)
              updateNumericInput(session,"skipFDOptionOUP",value=skip)
              updateNumericInput(session,"sFromFDOptionOUP",value=sFrom)
              updateNumericInput(session,"sToFDOptionOUP",value=sTo)
              updateNumericInput(session,"sByFDOptionOUP",value=sBy)
              updateNumericInput(session,"xFromFDOptionOUP",value=xFrom)
              updateNumericInput(session,"xToFDOptionOUP",value=xTo)
              updateNumericInput(session,"xByFDOptionOUP",value=xBy)
              updateNumericInput(session,"v1FDOptionOUP",label="~",value=NA)
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
                if(i == 1) { updateNumericInput(session,"V1FDOptionOUP",label="~",value="") }
                else if(i == 2) { updateNumericInput(session,"V2FDOptionOUP",label="~",value="") }
                else if(i == 3) { updateNumericInput(session,"V3FDOptionOUP",label="~",value="") }
                else if(i == 4) { updateNumericInput(session,"V4FDOptionOUP",label="~",value="") }
                else if(i == 5) { updateNumericInput(session,"V5FDOptionOUP",label="~",value="") }
              }
            })
          }
          FromUItoR6 <- function()
          {
            # Get from UI ----
            isolate({
              rho <- input$rhoFDOptionOUP
              mu <- input$muFDOptionOUP
              sigma <- input$sigmaFDOptionOUP
              sFrom <- input$sFromFDOptionOUP
              sTo <- input$sToFDOptionOUP
              sBy <- input$sByFDOptionOUP
              xFrom <- input$xFromFDOptionOUP
              xTo <- input$xToFDOptionOUP
              xBy <- input$xByFDOptionOUP
              r <- input$rFDOptionOUP
              skip <- input$skipFDOptionOUP
              v1 <- input$V1FDOptionOUP
              v2 <- input$V2FDOptionOUP
              v3 <- input$V3FDOptionOUP
              v4 <- input$V4FDOptionOUP
              v5 <- input$V5FDOptionOUP
            })
            if(!is.numeric(rho)) { rho <- 0 }
            else if(rho < 0) { rho <- 0 }
            if(!is.numeric(mu)) { mu <- 0 }
            if(!is.numeric(sigma)) { sigma <- 0 }
            s <- axissequence(sFrom,sTo,sBy)
            x <- axissequence(xFrom,xTo,xBy)
            if(!is.numeric(r)) { r <- 0 }
            if(!is.numeric(skip)) { skip <- 1 }
            if(skip < 1) { skip <- 1 }
            if(skip > 20) {skip <- 20 }
            if(is.na(v1)) { v1 <- NULL }
            if(is.na(v2)) { v2 <- NULL }
            if(is.na(v3)) { v3 <- NULL }
            if(is.na(v4)) { v4 <- NULL }
            if(is.na(v5)) { v5 <- NULL }
            # Set to OUP ----
            FD$set_oup_params(rho=rho,mu=mu,sigma=sigma)
            if(!skipinput)
            {
              m <- length(s)
              n <- length(x)
              if(m > 1) { sBy <- (s[m]-s[1])/(m-1) }
              else  {sBy <- 0 }
              xBy <- (x[n]-x[1])/(n-1)
              skip <- as.integer(sBy/xBy*100)
              if(skip < 1) { skip <- 1 }
              else if(skip > 20) { skip <- 20 }
            }
            FD$set_x_stoch_args(s=s,x=x,r=r,skip=skip)
            FD$set_V_args(NULL,NULL,v1,v2,v3,v4,v5)
          }
          # initialize ----
          TerminalChoices()
          # select ----
          observe({
            isolate({
              FD$set_V_info(NULL,input$VFDOptionOUP)
            })
            FromR6toUI()
          }) %>% bindEvent(input$VFDOptionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # observe s, x and skip ----
          observe({
            skipinput <<- FALSE
          }) %>% bindEvent(input$sFromFDOptionOUP,input$sToFDOptionOUP,input$sByFDOptionOUP,input$xFromFDOptionOUP,input$xToFDOptionOUP,input$xByFDOptionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            skipinput <<- TRUE
          }) %>% bindEvent(input$skipFDOptionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # user clicks clear or save ----
          observe({
            FromUItoR6()
            FD$undo_clear()
            showNotification("argument set 1 out of 1.",id="FDundo",duration=2)
          }) %>% bindEvent(input$clearFDOptionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            FromUItoR6()
            n <- FD$undo_save()
            showNotification(paste("argument set ",n," out of ",n,"."),id="FDundo",duration=2)
          }) %>% bindEvent(input$saveFDOptionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # user clicks undn, unup, axes, plot (or enter key) or other ----
          output$plotlyFDOptionOUP <- renderPlotly({
            if(input$undnFDOptionOUP > FDbtns[4,1])
            {
              FDbtns[4,1] <<- input$undnFDOptionOUP
              Ixn <- FD$undo_undo()
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="FDundo",duration=2)
            }
            else if(input$unupFDOptionOUP > FDbtns[4,2])
            {
              FDbtns[4,2] <<- input$unupFDOptionOUP
              Ixn <- FD$undo_undo(1)
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="FDundo",duration=2)
            }
            else if(input$axesFDOptionOUP > FDbtns[4,3])
            {
              FDbtns[4,3] <<- input$axesFDOptionOUP
              FromUItoR6()
              FD$axes_x_stoch()
            }
            else if(input$plotFDOptionOUP > FDbtns[4,4])
            {
              FDbtns[4,4] <<- input$plotFDOptionOUP
              FromUItoR6()
            }
            else if(input$otherFDOptionOUP > FDbtns[4,5])
            {
              FDbtns[4,5] <<- input$otherFDOptionOUP
              FromUItoR6()
              FD$set_plot_type("p",3)
            }
            FromR6toUI()
            FD$PlotOption()
          }) %>% bindEvent(input$undnFDOptionOUP,input$unupFDOptionOUP,input$axesFDOptionOUP,input$plotFDOptionOUP,input$otherFDOptionOUP)
          # observe info ----
          observe({
            ibutton <<- ""
            infobutton <<- "infoFDOptionOUP"
            if(infotoggle()) { infotoggle(FALSE) }
            else { infotoggle(TRUE) }
          }) %>% bindEvent(input$infoFDOptionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            removeModal(session)
            updateTabsetPanel(session,"navBar",selected="tabMCOUP")
            updateTabsetPanel(session,"navMCOUP",selected="MCOptionOUP")
          }) %>% bindEvent(input$alsoFDOptionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
        # Option Envelope ----
        if(input$navFDOUP == "FDEnvelopeOUP")
        {
          # define terminal function ----
          TerminalChoices <- function()
          {
            if(populate[3])
            {
              V_info <- FD$get_V_info()
              isolate({
                updateSelectInput(session,"VFDEnvelopeOUP",choices=V_info[[3]],selected=V_info[[2]])
              })
              populate[3] <<- FALSE
            }
            else
            {
              V_info <- FD$get_V_info()
              isolate({
                updateSelectInput(session,"VFDEnvelopeOUP",selected=V_info[[2]])
              })
            }
          }
          # define set/get functions ----
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
            skip <- x_stoch_args[[7]]
            name <- V_info[[2]]
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
            isolate({
              updateSelectInput(session,"VFDEnvelopeOUP",selected=name)
              updateNumericInput(session,"rhoFDEnvelopeOUP",value=rho)
              updateNumericInput(session,"muFDEnvelopeOUP",value=mu)
              updateNumericInput(session,"sigmaFDEnvelopeOUP",value=sigma)
              updateNumericInput(session,"rFDEnvelopeOUP",value=r)
              updateNumericInput(session,"skipFDEnvelopeOUP",value=skip)
              updateNumericInput(session,"sFromFDEnvelopeOUP",value=sFrom)
              updateNumericInput(session,"sToFDEnvelopeOUP",value=sTo)
              updateNumericInput(session,"sByFDEnvelopeOUP",value=sBy)
              updateNumericInput(session,"xFromFDEnvelopeOUP",value=xFrom)
              updateNumericInput(session,"xToFDEnvelopeOUP",value=xTo)
              updateNumericInput(session,"xByFDEnvelopeOUP",value=xBy)
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
                if(i == 1) { updateNumericInput(session,"V1FDEnvelopeOUP",label="~",value="") }
                else if(i == 2) { updateNumericInput(session,"V2FDEnvelopeOUP",label="~",value="") }
                else if(i == 3) { updateNumericInput(session,"V3FDEnvelopeOUP",label="~",value="") }
                else if(i == 4) { updateNumericInput(session,"V4FDEnvelopeOUP",label="~",value="") }
                else if(i == 5) { updateNumericInput(session,"V5FDEnvelopeOUP",label="~",value="") }
              }
            })
          }
          FromUItoR6 <- function()
          {
            # Get from UI ----
            isolate({
              rho <- input$rhoFDEnvelopeOUP
              mu <- input$muFDEnvelopeOUP
              sigma <- input$sigmaFDEnvelopeOUP
              sFrom <- input$sFromFDEnvelopeOUP
              sTo <- input$sToFDEnvelopeOUP
              sBy <- input$sByFDEnvelopeOUP
              xFrom <- input$xFromFDEnvelopeOUP
              xTo <- input$xToFDEnvelopeOUP
              xBy <- input$xByFDEnvelopeOUP
              r <- input$rFDEnvelopeOUP
              skip <- input$skipFDEnvelopeOUP
              v1 <- input$V1FDEnvelopeOUP
              v2 <- input$V2FDEnvelopeOUP
              v3 <- input$V3FDEnvelopeOUP
              v4 <- input$V4FDEnvelopeOUP
              v5 <- input$V5FDEnvelopeOUP
            })
            if(!is.numeric(rho)) { rho <- 0 }
            else if(rho < 0) { rho <- 0 }
            if(!is.numeric(mu)) { mu <- 0 }
            if(!is.numeric(sigma)) { sigma <- 0 }
            s <- axissequence(sFrom,sTo,sBy)
            x <- axissequence(xFrom,xTo,xBy)
            if(!is.numeric(r)) { r <- 0 }
            if(!is.numeric(skip)) { skip <- 1 }
            if(skip < 1) {skip <-1 }
            if(skip > 20) {skip <- 20 }
            if(is.na(v1)) { v1 <- NULL }
            if(is.na(v2)) { v2 <- NULL }
            if(is.na(v3)) { v3 <- NULL }
            if(is.na(v4)) { v4 <- NULL }
            if(is.na(v5)) { v5 <- NULL }
            # Set to OUP ----
            FD$set_oup_params(rho=rho,mu=mu,sigma=sigma)
            if(!skipinput)
            {
              m <- length(s)
              n <- length(x)
              if(m > 1) { sBy <- (s[m]-s[1])/(m-1) }
              else  {sBy <- 0 }
              xBy <- (x[n]-x[1])/(n-1)
              skip <- as.integer(sBy/xBy*100)
              if(skip < 1) { skip <- 1 }
              else if(skip > 20) { skip <- 20 }
            }
            FD$set_x_stoch_args(s=s,x=x,r=r,skip=skip)
            FD$set_V_args(NULL,NULL,v1,v2,v3,v4,v5)
          }
          # initialize ----
          TerminalChoices()
          # select ----
          observe({
            isolate({
              FD$set_V_info(NULL,input$VFDEnvelopeOUP)
            })
            FromR6toUI()
          }) %>% bindEvent(input$VFDEnvelopeOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # observe s, x and skip ----
          observe({
            skipinput <<- FALSE
          }) %>% bindEvent(input$sFromFDEnvelopeOUP,input$sToFDEnvelopeOUP,input$sByFDEnvelopeOUP,input$xFromFDEnvelopeOUP,input$xToFDEnvelopeOUP,input$xByFDEnvelopeOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            skipinput <<- TRUE
          }) %>% bindEvent(input$skipFDEnvelopeOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # user clicks clear or save ----
          observe({
            FromUItoR6()
            FD$undo_clear()
            showNotification("argument set 1 out of 1.",id="FDundo",duration=2)
          }) %>% bindEvent(input$clearFDEnvelopeOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            FromUItoR6()
            n <- FD$undo_save()
            showNotification(paste("argument set ",n," out of ",n,"."),id="FDundo",duration=2)
          }) %>% bindEvent(input$saveFDEnvelopeOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # user clicks undn, unup, axes, plot (or enter key) or other ----
          output$plotlyFDEnvelopeOUP <- renderPlotly({
            if(input$undnFDEnvelopeOUP > FDbtns[5,1])
            {
              FDbtns[5,1] <<- input$undnFDEnvelopeOUP
              Ixn <- FD$undo_undo()
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="FDundo",duration=2)
            }
            else if(input$unupFDEnvelopeOUP > FDbtns[5,2])
            {
              FDbtns[5,2] <<- input$unupFDEnvelopeOUP
              Ixn <- FD$undo_undo(1)
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="FDundo",duration=2)
            }
            else if(input$axesFDEnvelopeOUP > FDbtns[5,3])
            {
              FDbtns[5,3] <<- input$axesFDEnvelopeOUP
              FromUItoR6()
              FD$axes_x_stoch()
            }
            else if(input$plotFDEnvelopeOUP > FDbtns[5,4])
            {
              FDbtns[5,4] <<- input$plotFDEnvelopeOUP
              FromUItoR6()
            }
            else if(input$otherFDEnvelopeOUP > FDbtns[5,5])
            {
              FDbtns[5,5] <<- input$otherFDEnvelopeOUP
              FromUItoR6()
              FD$set_plot_type("p",3)
            }
            FromR6toUI()
            FD$PlotOptionEnvelope()
          }) %>% bindEvent(input$undnFDEnvelopeOUP,input$unupFDEnvelopeOUP,input$axesFDEnvelopeOUP,input$plotFDEnvelopeOUP,input$otherFDEnvelopeOUP)
          # observe info ----
          observe({
            ibutton <<- ""
            infobutton <<- "infoFDEnvelopeOUP"
            if(infotoggle()) { infotoggle(FALSE) }
            else { infotoggle(TRUE) }
          }) %>% bindEvent(input$infoFDEnvelopeOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            removeModal(session)
            updateTabsetPanel(session,"navBar",selected="tabAOUP")
            updateTabsetPanel(session,"navAOUP",selected="AEnvelopeOUP")
          }) %>% bindEvent(input$alsoFDEnvelopeOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
        # Decision Threshold ----
        if(input$navFDOUP == "FDDecisionOUP")
        {
          # define terminal function ----
          TerminalChoices <- function()
          {
            if(populate[4])
            {
              V_info <- FD$get_V_info()
              isolate({
                updateSelectInput(session,"VFDDecisionOUP",choices=V_info[[3]],selected=V_info[[2]])
              })
              populate[4] <<- FALSE
            }
            else
            {
              V_info <- FD$get_V_info()
              isolate({
                updateSelectInput(session,"VFDDecisionOUP",selected=V_info[[2]])
              })
            }
          }
          # define set/get functions ----
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
            name <- V_info[[2]]
            n <- length(x)
            xFrom <- x[1]
            xTo <- x[n]
            if(n > 1) { xBy <- (xTo-xFrom)/(n-1) }
            else  {xBy <- 0 }
            # Set to UI ----
            isolate({
              updateSelectInput(session,"VFDDecisionOUP",selected=name)
              updateNumericInput(session,"rhoFDDecisionOUP",value=rho)
              updateNumericInput(session,"muFDDecisionOUP",value=mu)
              updateNumericInput(session,"sigmaFDDecisionOUP",value=sigma)
              updateNumericInput(session,"rFDDecisionOUP",value=r)
              updateNumericInput(session,"phiFDDecisionOUP",value=phi)
              updateNumericInput(session,"xFromFDDecisionOUP",value=xFrom)
              updateNumericInput(session,"xToFDDecisionOUP",value=xTo)
              updateNumericInput(session,"xByFDDecisionOUP",value=xBy)
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
                if(i == 1) { updateNumericInput(session,"V1FDDecisionOUP",label="~",value="") }
                else if(i == 2) { updateNumericInput(session,"V2FDDecisionOUP",label="~",value="") }
                else if(i == 3) { updateNumericInput(session,"V3FDDecisionOUP",label="~",value="") }
                else if(i == 4) { updateNumericInput(session,"V4FDDecisionOUP",label="~",value="") }
                else if(i == 5) { updateNumericInput(session,"V5FDDecisionOUP",label="~",value="") }
              }
            })
          }
          FromUItoR6 <- function()
          {
            # Get from UI ----
            isolate({
              rho <- input$rhoFDDecisionOUP
              mu <- input$muFDDecisionOUP
              sigma <- input$sigmaFDDecisionOUP
              xFrom <- input$xFromFDDecisionOUP
              xTo <- input$xToFDDecisionOUP
              xBy <- input$xByFDDecisionOUP
              r <- input$rFDDecisionOUP
              phi <- input$phiFDDecisionOUP
              v1 <- input$V1FDDecisionOUP
              v2 <- input$V2FDDecisionOUP
              v3 <- input$V3FDDecisionOUP
              v4 <- input$V4FDDecisionOUP
              v5 <- input$V5FDDecisionOUP
            })
            if(!is.numeric(rho)) { rho <- 0 }
            else if(rho < 0) { rho <- 0 }
            if(!is.numeric(mu)) { mu <- 0 }
            if(!is.numeric(sigma)) { sigma <- 0 }
            x <- axissequence(xFrom,xTo,xBy)
            if(!is.numeric(r)) { r <- 0 }
            if(!is.numeric(phi)) { phi <- 0 }
            else if(phi < 0) { phi <- -1 }
            else if(phi > 0) { phi <- 1 }
            if(is.na(v1)) { v1 <- NULL }
            if(is.na(v2)) { v2 <- NULL }
            if(is.na(v3)) { v3 <- NULL }
            if(is.na(v4)) { v4 <- NULL }
            if(is.na(v5)) { v5 <- NULL }
            # Set to OUP ----
            FD$set_oup_params(rho=rho,mu=mu,sigma=sigma)
            s <- FD$get_x_stoch_args()[[1]]
            m <- length(s)
            n <- length(x)
            if(m > 1) { sBy <- (s[1]-s[m])/(m-1) }
            else  {sBy <- 0 }
            xBy <- (x[n]-x[1])/(n-1)
            skip <- as.integer(sBy/xBy*100)
            if(skip < 1) { skip <- 1 }
            else if(skip > 20) { skip <- 20 }
            FD$set_x_stoch_args(x=x,r=r,phi=phi,skip=skip)
            FD$set_V_args(NULL,NULL,v1,v2,v3,v4,v5)
          }
          # initialize ----
          TerminalChoices()
          # select ----
          observe({
            isolate({
              FD$set_V_info(NULL,input$VFDDecisionOUP)
            })
            FromR6toUI()
          }) %>% bindEvent(input$VFDDecisionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # user clicks clear or save ----
          observe({
            FromUItoR6()
            FD$undo_clear()
            showNotification("argument set 1 out of 1.",id="FDundo",duration=2)
          }) %>% bindEvent(input$clearFDDecisionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            FromUItoR6()
            n <- FD$undo_save()
            showNotification(paste("argument set ",n," out of ",n,"."),id="FDundo",duration=2)
          }) %>% bindEvent(input$saveFDDecisionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # user clicks undn, unup, axes or plot (or enter key) ----
          output$plotlyFDDecisionOUP <- renderPlotly({
            if(input$undnFDDecisionOUP > FDbtns[6,1])
            {
              FDbtns[6,1] <<- input$undnFDDecisionOUP
              Ixn <- FD$undo_undo()
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="FDundo",duration=2)
            }
            else if(input$unupFDDecisionOUP > FDbtns[6,2])
            {
              FDbtns[6,2] <<- input$unupFDDecisionOUP
              Ixn <- FD$undo_undo(1)
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="FDundo",duration=2)
            }
            else if(input$axesFDDecisionOUP > FDbtns[6,3])
            {
              FDbtns[6,3] <<- input$axesFDDecisionOUP
              FromUItoR6()
              FD$axes_x_stoch()
            }
            else if(input$plotFDDecisionOUP > FDbtns[6,4])
            {
              FDbtns[6,4] <<- input$plotFDDecisionOUP
              FromUItoR6()
            }
            FromR6toUI()
            FD$PlotDecisionThreshold()
          }) %>% bindEvent(input$undnFDDecisionOUP,input$unupFDDecisionOUP,input$axesFDDecisionOUP,input$plotFDDecisionOUP)
          # observe info ----
          observe({
            ibutton <<- ""
            infobutton <<- "infoFDDecisionOUP"
            if(infotoggle()) { infotoggle(FALSE) }
            else { infotoggle(TRUE) }
          }) %>% bindEvent(input$infoFDDecisionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            removeModal(session)
            updateTabsetPanel(session,"navBar",selected="tabAOUP")
            updateTabsetPanel(session,"navAOUP",selected="ADecisionOUP")
          }) %>% bindEvent(input$alsoFDDecisionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
      })
    }
    else if(input$navBar == "tabMLOUP")
    {
      observeEvent(input$navMLOUP,{
        # Data ----
        if(input$navMLOUP == "MLDataOUP")
        {
          # define data functions ----
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
          DataRead <- function()
          {
            # message("Data DataRead")
            if(firsttab)
            {
              # message("firsttab")
              df <<- utils::read.csv(uploadpath,fileEncoding="UTF-8-BOM")
              framenames <<- colnames(df)
              dname[3] <<- uploadname
              tname[3] <<- framenames[1]
              sname[3] <<- framenames[2]
              nrows <<- nrow(df)
              ncols <<- ncol(df)
              nfirst <<- df[1,1]
              nlast <<- df[nrows,1]
              series <- ML$set_timeseries(df=df,taucol=1,zcol=2)
              Ixend <- nrow(series)
              end <- series[Ixend,1]
              if(Ixend > 200) { Ixbeg <- Ixend-200 }
              else { Ixbeg <- 1 }
              beg <- series[Ixbeg,1]
              ML$set_timeseries_info(tbeg=beg,tend=end,dataname=dname[3],timename=tname[3],statename=sname[3],NULL)
              isolate({
                updateSelectInput(session,"filesMLDataOUP",choices=filelist,selected=dname[3])
                updateSelectInput(session,"timeMLDataOUP",choices=framenames,selected=tname[3])
                updateSelectInput(session,"stateMLDataOUP",choices=framenames,selected=sname[3])
                updateNumericInput(session,"begMLDataOUP",value=beg)
                updateNumericInput(session,"endMLDataOUP",value=end)
              })
              DataInfo()
              firsttab <<- FALSE
              initialize[1] <<- FALSE
            }
            else if(initialize[1])
            {
              # message("initialize")
              df_info <- ML$get_timeseries_info()
              dname[3] <<- df_info[[3]]
              tname[3] <<- df_info[[4]]
              sname[3] <<- df_info[[5]]
              isolate({
                updateSelectInput(session,"filesMLDataOUP",choices=filelist,selected=dname[3])
                updateSelectInput(session,"timeMLDataOUP",choices=framenames,selected=tname[3])
                updateSelectInput(session,"stateMLDataOUP",choices=framenames,selected=sname[3])
              })
              FromR6toUI()
              DataInfo()
              initialize[1] <<- FALSE
            }
            else
            {
              # message("else")
              df_info <- ML$get_timeseries_info()
              dataname <- df_info[[3]]
              timename <- df_info[[4]]
              statename <- df_info[[5]]
              if(dataname != dname[3] | timename != tname[3] | statename != sname[3])
              {
                isolate({
                  updateSelectInput(session,"filesMLDataOUP",choices=filelist,selected=dataname)
                  updateSelectInput(session,"timeMLDataOUP",choices=framenames,selected=timename)
                  updateSelectInput(session,"stateMLDataOUP",choices=framenames,selected=statename)
                })
                DataInfo()
                dname[3] <<- dataname
                tname[3] <<- timename
                sname[3] <<- statename
              }
              FromR6toUI()
            }
          }
          # define set/get functions ----
          FromR6toUI <- function()
          {
            # message("FromR6toUI")
            timeseries_info <- ML$get_timeseries_info()
            beg <- timeseries_info[[1]]
            end <- timeseries_info[[2]]
            isolate({
              updateNumericInput(session,"begMLDataOUP",value=beg)
              updateNumericInput(session,"endMLDataOUP",value=end)
            })
          }
          FromUItoR6 <- function()
          {
            # message("FromUItoR6")
            isolate({
              beg <- input$begMLDataOUP
              end <- input$endMLDataOUP
            })
            if(!is.numeric(beg)) { beg <- -Inf }
            if(!is.numeric(end)) { end <- Inf }
            ML$set_timeseries_info(tbeg=beg,tend=end)
          }
          # initialize ----
          DataRead()
          # select ----
          observe({
            # message("Data observe file")
            if(dname[3] != input$filesMLDataOUP)
            {
              dname[3] <<- input$filesMLDataOUP
              if(dname[3] == uploadname) { filepath <- uploadpath }
              else { filepath <- paste(sep="",datapath,input$filesMLDataOUP,".csv")  }
              df <<- utils::read.csv(filepath,fileEncoding="UTF-8-BOM")
              framenames <<- colnames(df)
              tname[3] <<- framenames[1]
              sname[3] <<- framenames[2]
              nrows <<- nrow(df)
              ncols <<- ncol(df)
              nfirst <<- df[1,1]
              nlast <<- df[nrows,1]
              series <- ML$set_timeseries(df=df,taucol=1,zcol=2)
              Ixend <- nrow(series)
              end <- series[Ixend,1]
              if(Ixend > 200) { Ixbeg <- Ixend-200 }
              else { Ixbeg <- 1 }
              beg <- series[Ixbeg,1]
              ML$set_timeseries_info(tbeg=beg,tend=end,dataname=dname[3],timename=tname[3],statename=sname[3],NULL)
              isolate({
                updateSelectInput(session,"timeMLDataOUP",choices=framenames,selected=tname[3])
                updateSelectInput(session,"stateMLDataOUP",choices=framenames,selected=sname[3])
                updateNumericInput(session,"begMLDataOUP",value=beg)
                updateNumericInput(session,"endMLDataOUP",value=end)
              })
              ML$set_oup_params(rho=0,mu=0,sigma=0)
              DataInfo()
            }
          }) %>% bindEvent(input$filesMLDataOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            # message("Data observe time")
            if(tname[3] != input$timeMLDataOUP)
            {
              tname[3] <<- input$timeMLDataOUP
              taucol <- match(tname[3],framenames)
              zcol <- match(sname[3],framenames)
              series <- ML$set_timeseries(df=df,taucol=taucol,zcol=zcol)
              Ixend <- nrow(series)
              end <- series[Ixend,1]
              if(Ixend > 200) { Ixbeg <- Ixend-200 }
              else { Ixbeg <- 1 }
              beg <- series[Ixbeg,1]
              ML$set_timeseries_info(tbeg=beg,tend=end,dataname=dname[3],timename=tname[3],statename=sname[3],NULL)
              isolate({
                updateNumericInput(session,"begMLDataOUP",value=beg)
                updateNumericInput(session,"endMLDataOUP",value=end)
              })
              ML$set_oup_params(rho=0,mu=0,sigma=0)
            }
          }) %>% bindEvent(input$timeMLDataOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            # message("Data observe state")
            if(sname[3] != input$stateMLDataOUP)
            {
              sname[3] <<- input$stateMLDataOUP
              taucol <- match(tname[3],framenames)
              zcol <- match(sname[3],framenames)
              series <- ML$set_timeseries(df=df,taucol=taucol,zcol=zcol)
              Ixend <- nrow(series)
              end <- series[Ixend,1]
              if(Ixend > 200) { Ixbeg <- Ixend-200 }
              else { Ixbeg <- 1 }
              beg <- series[Ixbeg,1]
              ML$set_timeseries_info(tbeg=beg,tend=end,dataname=dname[3],timename=tname[3],statename=sname[3],NULL)
              isolate({
                updateNumericInput(session,"begMLDataOUP",value=beg)
                updateNumericInput(session,"endMLDataOUP",value=end)
              })
              ML$set_oup_params(rho=0,mu=0,sigma=0)
            }
          }) %>% bindEvent(input$stateMLDataOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # upload ----
          observe({
            uploadname <<- file_path_sans_ext(input$filesMLUploadOUP$name)
            uploadpath <<- input$filesMLUploadOUP$datapath
            filelist[1] <<- uploadname
            firsttab <<- TRUE
            DataRead()
          }) %>% bindEvent(input$filesMLUploadOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # user clicks reset or plot (or enter key) ----
          output$plotlyMLDataOUP <- renderPlotly({
            # message("render")
            if(input$resetMLDataOUP > MLbtns[1,1])
            {
              MLbtns[1,1] <<- input$resetMLDataOUP
              ML$set_timeseries_info(tbeg=-Inf,tend=Inf)
            }
            else if(input$plotMLDataOUP > MLbtns[1,2])
            {
              MLbtns[1,2] <<- input$plotMLDataOUP
              FromUItoR6()
            }
            FromR6toUI()
            ML$PlotTimeSeries()
          }) %>% bindEvent(input$resetMLDataOUP,input$plotMLDataOUP)
          # observe i and info ----
          observe({
            ibutton <<- input$filesMLDataOUP
            infobutton <<- ""
            if(infotoggle()) { infotoggle(FALSE) }
            else { infotoggle(TRUE) }
          }) %>% bindEvent(input$fileinfoMLDataOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            ibutton <<- ""
            infobutton <<- "infoMLDataOUP"
            if(infotoggle()) { infotoggle(FALSE) }
            else { infotoggle(TRUE) }
          }) %>% bindEvent(input$infoMLDataOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
        # Log Likelihood ----
        else if(input$navMLOUP == "MLLikelihoodOUP")
        {
          # define data function ----
          DataRead <- function()
          {
            # message("Likelihood DataRead")
            if(firsttab)
            {
              # message("firsttab")
              df <<- utils::read.csv(uploadpath,fileEncoding="UTF-8-BOM")
              framenames <<- colnames(df)
              dname[4] <<- uploadname
              tname[4] <<- framenames[1]
              sname[4] <<- framenames[2]
              nrows <<- nrow(df)
              ncols <<- ncol(df)
              nfirst <<- df[1,1]
              nlast <<- df[nrows,1]
              series <- ML$set_timeseries(df=df,taucol=1,zcol=2)
              Ixend <- nrow(series)
              end <- series[Ixend,1]
              if(Ixend > 200) { Ixbeg <- Ixend-200 }
              else { Ixbeg <- 1 }
              beg <- series[Ixbeg,1]
              ML$set_timeseries_info(tbeg=beg,tend=end,dataname=dname[4],timename=tname[4],statename=sname[4],NULL)
              isolate({
                updateSelectInput(session,"filesMLLikelihoodOUP",choices=filelist,selected=dname[4])
                updateSelectInput(session,"timeMLLikelihoodOUP",choices=framenames,selected=tname[4])
                updateSelectInput(session,"stateMLLikelihoodOUP",choices=framenames,selected=sname[4])
              })
              firsttab <<- FALSE
              initialize[2] <<- FALSE
            }
            else if(initialize[2])
            {
              # message("initialize")
              df_info <- ML$get_timeseries_info()
              dname[4] <<- df_info[[3]]
              tname[4] <<- df_info[[4]]
              sname[4] <<- df_info[[5]]
              isolate({
                updateSelectInput(session,"filesMLLikelihoodOUP",choices=filelist,selected=dname[4])
                updateSelectInput(session,"timeMLLikelihoodOUP",choices=framenames,selected=tname[4])
                updateSelectInput(session,"stateMLLikelihoodOUP",choices=framenames,selected=sname[4])
              })
              initialize[2] <<- FALSE
            }
            else
            {
              # message("else")
              df_info <- ML$get_timeseries_info()
              dataname <- df_info[[3]]
              timename <- df_info[[4]]
              statename <- df_info[[5]]
              if(dataname != dname[4] | timename != tname[4] | statename != sname[4])
              {
                isolate({
                  updateSelectInput(session,"filesMLLikelihoodOUP",choices=filelist,selected=dataname)
                  updateSelectInput(session,"timeMLLikelihoodOUP",choices=framenames,selected=timename)
                  updateSelectInput(session,"stateMLLikelihoodOUP",choices=framenames,selected=statename)
                })
                dname[4] <<- dataname
                tname[4] <<- timename
                sname[4] <<- statename
              }
            }
            FromR6toUI()
          }
          # define set/get functions ----
          FromR6toUI <- function()
          {
            # message("FromR6toUI")
            oup_params <- ML$get_oup_params()
            timeseries_info <- ML$get_timeseries_info()
            rho <- oup_params[[1]]
            mu <- oup_params[[2]]
            sigma <- oup_params[[3]]
            beg <- timeseries_info[[1]]
            end <- timeseries_info[[2]]
            isolate({
              updateNumericInput(session,"rhoMLLikelihoodOUP",value=rho)
              updateNumericInput(session,"muMLLikelihoodOUP",value=mu)
              updateNumericInput(session,"sigmaMLLikelihoodOUP",value=sigma)
              updateNumericInput(session,"begMLLikelihoodOUP",value=beg)
              updateNumericInput(session,"endMLLikelihoodOUP",value=end)
            })
          }
          FromUItoR6 <- function()
          {
            # message("FromUItoR6")
            isolate({
              rho <- input$rhoMLLikelihoodOUP
              mu <- input$muMLLikelihoodOUP
              sigma <- input$sigmaMLLikelihoodOUP
              beg <- input$begMLLikelihoodOUP
              end <- input$endMLLikelihoodOUP
            })
            if(!is.numeric(rho)) { rho <- 0 }
            else if(rho < 0) { rho <- 0 }
            if(!is.numeric(mu)) { mu <- 0 }
            if(!is.numeric(sigma)) { sigma <- 0 }
            if(!is.numeric(beg)) { beg <- -Inf }
            if(!is.numeric(end)) { end <- Inf }
            ML$set_oup_params(rho=rho,mu=mu,sigma=sigma)
            ML$set_timeseries_info(tbeg=beg,tend=end)
          }
          # initialize ----
          DataRead()
          # select ----
          observe({
            # message("Likelihood observe file")
            if(dname[4] != input$filesMLLikelihoodOUP)
            {
              dname[4] <<- input$filesMLLikelihoodOUP
              if(dname[4] == uploadname) { filepath <- uploadpath }
              else { filepath <- paste(sep="",datapath,input$filesMLLikelihoodOUP,".csv")  }
              df <<- utils::read.csv(filepath,fileEncoding="UTF-8-BOM")
              framenames <<- colnames(df)
              tname[4] <<- framenames[1]
              sname[4] <<- framenames[2]
              nrows <<- nrow(df)
              ncols <<- ncol(df)
              nfirst <<- df[1,1]
              nlast <<- df[nrows,1]
              series <- ML$set_timeseries(df=df,taucol=1,zcol=2)
              Ixend <- nrow(series)
              end <- series[Ixend,1]
              if(Ixend > 200) { Ixbeg <- Ixend-200 }
              else { Ixbeg <- 1 }
              beg <- series[Ixbeg,1]
              ML$set_timeseries_info(tbeg=beg,tend=end,dataname=dname[4],timename=tname[4],statename=sname[4])
              isolate({
                updateSelectInput(session,"timeMLLikelihoodOUP",choices=framenames,selected=tname[4])
                updateSelectInput(session,"stateMLLikelihoodOUP",choices=framenames,selected=sname[4])
                updateNumericInput(session,"rhoMLLikelihoodOUP",value=0)
                updateNumericInput(session,"muMLLikelihoodOUP",value=0)
                updateNumericInput(session,"sigmaMLLikelihoodOUP",value=0)
                updateNumericInput(session,"begMLLikelihoodOUP",value=beg)
                updateNumericInput(session,"endMLLikelihoodOUP",value=end)
              })
            }
          }) %>% bindEvent(input$filesMLLikelihoodOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            # message("Likelihood observe time")
            if(tname[4] != input$timeMLLikelihoodOUP)
            {
              tname[4] <<- input$timeMLLikelihoodOUP
              taucol <- match(tname[4],framenames)
              zcol <- match(sname[4],framenames)
              series <- ML$set_timeseries(df=df,taucol=taucol,zcol=zcol)
              Ixend <- nrow(series)
              end <- series[Ixend,1]
              if(Ixend > 200) { Ixbeg <- Ixend-200 }
              else { Ixbeg <- 1 }
              beg <- series[Ixbeg,1]
              ML$set_timeseries_info(tbeg=beg,tend=end,dataname=dname[4],timename=tname[4],statename=sname[4],NULL)
              isolate({
                updateNumericInput(session,"rhoMLLikelihoodOUP",value=0)
                updateNumericInput(session,"muMLLikelihoodOUP",value=0)
                updateNumericInput(session,"sigmaMLLikelihoodOUP",value=0)
                updateNumericInput(session,"begMLLikelihoodOUP",value=beg)
                updateNumericInput(session,"endMLLikelihoodOUP",value=end)
              })
            }
          }) %>% bindEvent(input$timeMLLikelihoodOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            # message("Likelihood observe state")
            if(sname[4] != input$stateMLLikelihoodOUP)
            {
              sname[4] <<- input$stateMLLikelihoodOUP
              taucol <- match(tname[4],framenames)
              zcol <- match(sname[4],framenames)
              series <- ML$set_timeseries(df=df,taucol=taucol,zcol=zcol)
              Ixend <- nrow(series)
              end <- series[Ixend,1]
              if(Ixend > 200) { Ixbeg <- Ixend-200 }
              else { Ixbeg <- 1 }
              beg <- series[Ixbeg,1]
              ML$set_timeseries_info(tbeg=beg,tend=end,dataname=dname[4],timename=tname[4],statename=sname[4],NULL)
              isolate({
                updateNumericInput(session,"rhoMLLikelihoodOUP",value=0)
                updateNumericInput(session,"muMLLikelihoodOUP",value=0)
                updateNumericInput(session,"sigmaMLLikelihoodOUP",value=0)
                updateNumericInput(session,"begMLLikelihoodOUP",value=beg)
                updateNumericInput(session,"endMLLikelihoodOUP",value=end)
              })
            }
          }) %>% bindEvent(input$stateMLLikelihoodOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # user clicks reset or plot (or enter key) ----
          observe({
            # message("plot")
            if(input$resetMLLikelihoodOUP > MLbtns[2,1])
            {
              MLbtns[2,1] <<- input$resetMLLikelihoodOUP
              FromUItoR6()
              ML$set_timeseries_info(tbeg=-Inf,tend=Inf)
            }
            else if(input$plotMLLikelihoodOUP > MLbtns[2,2])
            {
              MLbtns[2,2] <<- input$plotMLLikelihoodOUP
              FromUItoR6()
            }
            FromR6toUI()
            output$plotlyMLLikelihoodOUP <- renderPlotly({ ML$PlotEstimates() })
            loglikely <- ML$LogLikelihood()
            theta <- format(loglikely,digits=6)
            output$lnLMLLikelihoodOUP <- renderUI({
              HTML(paste(sep="",
                "<table align='center'>
                  <tr>
                    <th></th>
                    <th style='text-align: right; padding: 2px 6px 2px 8px; border-bottom: 1px solid grey;'>LnL</th>
                    <th style='text-align: right; padding: 2px 6px 2px 6px; border-bottom: 1px solid grey;'>k</th>
                    <th style='text-align: right; padding: 2px 6px 2px 6px; border-bottom: 1px solid grey;'>alpha</th>
                    <th style='text-align: right; padding: 2px 8px 2px 6px; border-bottom: 1px solid grey;'>m-1</th>
                  </tr>
                  <tr>
                    <td style='text-align: right; padding: 8px 6px 8px 8px;'></td>
                    <td style='text-align: right; padding: 8px 6px 8px 6px;'>",theta[[4]],"</td>
                    <td style='text-align: right; padding: 8px 6px 8px 6px;'>",theta[[5]],"</td>
                    <td style='text-align: right; padding: 8px 6px 8px 6px;'>",theta[[6]],"</td>
                    <td style='text-align: right; padding: 8px 8px 8px 6px;'>",theta[[7]],"</td>
                  </tr>
                </table>"
              ))
            })
          }) %>% bindEvent(input$resetMLLikelihoodOUP,input$plotMLLikelihoodOUP)
          # observe i and info ----
          observe({
            ibutton <<- input$filesMLLikelihoodOUP
            infobutton <<- ""
            if(infotoggle()) { infotoggle(FALSE) }
            else { infotoggle(TRUE) }
          }) %>% bindEvent(input$fileinfoMLLikelihoodOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            ibutton <<- ""
            infobutton <<- "infoMLLikelihoodOUP"
            if(infotoggle()) { infotoggle(FALSE) }
            else { infotoggle(TRUE) }
          }) %>% bindEvent(input$infoMLLikelihoodOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
        # Estimates ----
        else if(input$navMLOUP == "MLEstimatesOUP")
        {
          # define go function ----
          Go <- function()
          {
            # message("go")
            oup_params_restr <- ML$get_oup_params_restr()
            rhor <- oup_params_restr[[1]]
            mur <- oup_params_restr[[2]]
            sigmar <- oup_params_restr[[3]]
            est_u <- ML$Estimates()
            est_r <- ML$Estimates(rhor=rhor,mur=mur,sigmar=sigmar)
            estimation <- ML$get_timeseries_info()[[6]]
            theta_u <- format(est_u,digits=6)
            theta_r <- format(est_r,digits=6)
            output$paramMLEstimatesOUP <- renderUI({
              HTML(paste(sep="",
                "<table align='center'>
                  <tr>
                    <th></th>
                    <th style='text-align: right; padding: 2px 6px 2px 8px; border-bottom: 1px solid grey;'>rho</th>
                    <th style='text-align: right; padding: 2px 6px 2px 6px; border-bottom: 1px solid grey;'>mu</th>
                    <th style='text-align: right; padding: 2px 6px 2px 6px; border-bottom: 1px solid grey;'>sigma</th>
                    <th style='text-align: right; padding: 2px 6px 2px 6px; border-bottom: 1px solid grey;'>LnL</th>
                    <th style='text-align: right; padding: 2px 6px 2px 6px; border-bottom: 1px solid grey;'>k</th>
                    <th style='text-align: right; padding: 2px 6px 2px 6px; border-bottom: 1px solid grey;'>alpha</th>
                    <th style='text-align: right; padding: 2px 8px 2px 6px; border-bottom: 1px solid grey;'>m-1</th>
                  </tr>
                  <tr>
                    <td style='text-align: right; padding: 8px 6px 2px 8px;'><b>Unrestricted</b></td>
                    <td style='text-align: right; padding: 8px 6px 2px 6px;'>",theta_u[[1]],"</td>
                    <td style='text-align: right; padding: 8px 6px 2px 6px;'>",theta_u[[2]],"</td>
                    <td style='text-align: right; padding: 8px 6px 2px 6px;'>",theta_u[[3]],"</td>
                    <td style='text-align: right; padding: 8px 6px 2px 6px;'>",theta_u[[4]],"</td>
                    <td style='text-align: right; padding: 8px 6px 2px 6px;'>",theta_u[[5]],"</td>
                    <td style='text-align: right; padding: 8px 6px 2px 6px;'>",theta_u[[6]],"</td>
                    <td style='text-align: right; padding: 8px 8px 2px 6px;'>",theta_u[[7]],"</td>
                  </tr>
                  <tr>
                    <td style='text-align: right; padding: 2px 6px 8px 8px;'><b>",estimation,"</b></td>
                    <td style='text-align: right; padding: 2px 6px 8px 6px;'>",theta_r[[1]],"</td>
                    <td style='text-align: right; padding: 2px 6px 8px 6px;'>",theta_r[[2]],"</td>
                    <td style='text-align: right; padding: 2px 6px 8px 6px;'>",theta_r[[3]],"</td>
                    <td style='text-align: right; padding: 2px 6px 8px 6px;'>",theta_r[[4]],"</td>
                    <td style='text-align: right; padding: 2px 6px 8px 6px;'>",theta_r[[5]],"</td>
                    <td style='text-align: right; padding: 2px 6px 8px 6px;'>",theta_r[[6]],"</td>
                    <td style='text-align: right; padding: 2px 8px 8px 6px;'>",theta_r[[7]],"</td>
                </table>"
              ))
            })
          }
          # define data function ----
          DataRead <- function()
          {
            # message("Estimates DataRead")
            if(firsttab)
            {
              # message("firsttab")
              df <<- utils::read.csv(uploadpath,fileEncoding="UTF-8-BOM")
              framenames <<- colnames(df)
              dname[5] <<- uploadname
              tname[5] <<- framenames[1]
              sname[5] <<- framenames[2]
              nrows <<- nrow(df)
              ncols <<- ncol(df)
              nfirst <<- df[1,1]
              nlast <<- df[nrows,1]
              series <- ML$set_timeseries(df=df,taucol=1,zcol=2)
              Ixend <- nrow(series)
              end <- series[Ixend,1]
              if(Ixend > 200) { Ixbeg <- Ixend-200 }
              else { Ixbeg <- 1 }
              beg <- series[Ixbeg,1]
              ML$set_timeseries_info(tbeg=beg,tend=end,dataname=dname[5],timename=tname[5],statename=sname[5],NULL)
              isolate({
                updateSelectInput(session,"filesMLEstimatesOUP",choices=filelist,selected=dname[5])
                updateSelectInput(session,"timeMLEstimatesOUP",choices=framenames,selected=tname[5])
                updateSelectInput(session,"stateMLEstimatesOUP",choices=framenames,selected=sname[5])
              })
              firsttab <<- FALSE
              initialize[3] <<- FALSE
            }
            else if(initialize[3])
            {
              # message("initialize")
              df_info <- ML$get_timeseries_info()
              dname[5] <<- df_info[[3]]
              tname[5] <<- df_info[[4]]
              sname[5] <<- df_info[[5]]
              isolate({
                updateSelectInput(session,"filesMLEstimatesOUP",choices=filelist,selected=dname[5])
                updateSelectInput(session,"timeMLEstimatesOUP",choices=framenames,selected=tname[5])
                updateSelectInput(session,"stateMLEstimatesOUP",choices=framenames,selected=sname[5])
              })
              initialize[3] <<- FALSE
            }
            else
            {
              # message("else")
              df_info <- ML$get_timeseries_info()
              dataname <- df_info[[3]]
              timename <- df_info[[4]]
              statename <- df_info[[5]]
              if(dataname != dname[5] | timename != tname[5] | statename != sname[5])
              {
                isolate({
                  updateSelectInput(session,"filesMLEstimatesOUP",choices=filelist,selected=dataname)
                  updateSelectInput(session,"timeMLEstimatesOUP",choices=framenames,selected=timename)
                  updateSelectInput(session,"stateMLEstimatesOUP",choices=framenames,selected=statename)
                })
                ML$set_oup_params_restr(rhor=NULL,mur=NULL,sigmar=NULL)
                dname[5] <<- dataname
                tname[5] <<- timename
                sname[5] <<- statename
              }
            }
            FromR6toUI()
          }
          # define set/get functions ----
          FromR6toUI <- function()
          {
            # message("FromR6toUI")
            oup_params_restr <- ML$get_oup_params_restr()
            rhor <- oup_params_restr[[1]]
            mur <- oup_params_restr[[2]]
            sigmar <- oup_params_restr[[3]]
            isolate({
              if(is.null(rhor)) { updateNumericInput(session,"rhorMLEstimatesOUP",value="") }
              else { updateNumericInput(session,"rhorMLEstimatesOUP",value=rhor) }
              if(is.null(mur)) { updateNumericInput(session,"murMLEstimatesOUP",value="") }
              else { updateNumericInput(session,"murMLEstimatesOUP",value=mur) }
              if(is.null(sigmar)) { updateNumericInput(session,"sigmarMLEstimatesOUP",value="") }
              else { updateNumericInput(session,"sigmarMLEstimatesOUP",value=sigmar) }
            })
          }
          FromUItoR6 <- function()
          {
            # message("FromUItoR6")
            isolate({
              rhor <- input$rhorMLEstimatesOUP
              mur <- input$murMLEstimatesOUP
              sigmar <- input$sigmarMLEstimatesOUP
            })
            if(!is.numeric(rhor)) { rhor <- NULL }
            else if(rhor < 0) { rhor <-0 }
            if(!is.numeric(mur)) { mur <- NULL }
            if(!is.numeric(sigmar)) { sigmar <- NULL }
            ML$set_oup_params_restr(rhor=rhor,mur=mur,sigmar=sigmar)
          }
          # initialize ----
          DataRead()
          Go()  #no reactive in plot event which is not called on initialization
          # select ----
          observe({
            # message("Estimates observe file")
            if(dname[5] != input$filesMLEstimatesOUP)
            {
              dname[5] <<- input$filesMLEstimatesOUP
              if(dname[5] == uploadname) { filepath <- uploadpath }
              else { filepath <- paste(sep="",datapath,input$filesMLEstimatesOUP,".csv")  }
              df <<- utils::read.csv(filepath,fileEncoding="UTF-8-BOM")
              framenames <<- colnames(df)
              tname[5] <<- framenames[1]
              sname[5] <<- framenames[2]
              nrows <<- nrow(df)
              ncols <<- ncol(df)
              nfirst <<- df[1,1]
              nlast <<- df[nrows,1]
              series <- ML$set_timeseries(df=df,taucol=1,zcol=2)
              Ixend <- nrow(series)
              end <- series[Ixend,1]
              if(Ixend > 200) { Ixbeg <- Ixend-200 }
              else { Ixbeg <- 1 }
              beg <- series[Ixbeg,1]
              ML$set_timeseries_info(tbeg=beg,tend=end,dataname=dname[5],timename=tname[5],statename=sname[5])
              isolate({
                updateSelectInput(session,"timeMLEstimatesOUP",choices=framenames,selected=tname[5])
                updateSelectInput(session,"stateMLEstimatesOUP",choices=framenames,selected=sname[5])
                updateNumericInput(session,"rhoMLEstimatesOUP",value=0)
                updateNumericInput(session,"muMLEstimatesOUP",value=0)
                updateNumericInput(session,"sigmaMLEstimatesOUP",value=0)
                updateNumericInput(session,"rhorMLEstimatesOUP",value="")
                updateNumericInput(session,"murMLEstimatesOUP",value="")
                updateNumericInput(session,"sigmarMLEstimatesOUP",value="")
              })
            }
          }) %>% bindEvent(input$filesMLEstimatesOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            # message("Estimates observe time")
            if(tname[5] != input$timeMLEstimatesOUP)
            {
              tname[5] <<- input$timeMLEstimatesOUP
              taucol <- match(tname[5],framenames)
              zcol <- match(sname[5],framenames)
              series <- ML$set_timeseries(df=df,taucol=taucol,zcol=zcol)
              Ixend <- nrow(series)
              end <- series[Ixend,1]
              if(Ixend > 200) { Ixbeg <- Ixend-200 }
              else { Ixbeg <- 1 }
              beg <- series[Ixbeg,1]
              ML$set_timeseries_info(tbeg=beg,tend=end,dataname=dname[5],timename=tname[5],statename=sname[5],NULL)
               isolate({
                updateNumericInput(session,"rhoMLEstimatesOUP",value=0)
                updateNumericInput(session,"muMLEstimatesOUP",value=0)
                updateNumericInput(session,"sigmaMLEstimatesOUP",value=0)
                updateNumericInput(session,"rhorMLEstimatesOUP",value="")
                updateNumericInput(session,"murMLEstimatesOUP",value="")
                updateNumericInput(session,"sigmarMLEstimatesOUP",value="")
              })
            }
          }) %>% bindEvent(input$timeMLEstimatesOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            # message("Estimates observe state")
            if(sname[5] != input$stateMLEstimatesOUP)
            {
              sname[5] <<- input$stateMLEstimatesOUP
              taucol <- match(tname[5],framenames)
              zcol <- match(sname[5],framenames)
              series <- ML$set_timeseries(df=df,taucol=taucol,zcol=zcol)
              Ixend <- nrow(series)
              end <- series[Ixend,1]
              if(Ixend > 200) { Ixbeg <- Ixend-200 }
              else { Ixbeg <- 1 }
              beg <- series[Ixbeg,1]
              ML$set_timeseries_info(tbeg=beg,tend=end,dataname=dname[5],timename=tname[5],statename=sname[5],NULL)
              isolate({
                updateNumericInput(session,"rhoMLEstimatesOUP",value=0)
                updateNumericInput(session,"muMLEstimatesOUP",value=0)
                updateNumericInput(session,"sigmaMLEstimatesOUP",value=0)
                updateNumericInput(session,"rhorMLEstimatesOUP",value="")
                updateNumericInput(session,"murMLEstimatesOUP",value="")
                updateNumericInput(session,"sigmarMLEstimatesOUP",value="")
              })
            }
          }) %>% bindEvent(input$stateMLEstimatesOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # user clicks reset ----
          observe({
            # message("reset")
            ML$set_oup_params_restr(rhor=NULL,mur=NULL,sigmar=NULL)
            FromR6toUI()
            Go()
          }) %>% bindEvent(input$resetMLEstimatesOUP)
          # user clicks go (or enter key) ----
          observe({
            # message("go")
            FromUItoR6()
            FromR6toUI()
            Go()
          }) %>% bindEvent(input$plotMLEstimatesOUP)
          # observe i and info ----
          observe({
            ibutton <<- input$filesMLEstimatesOUP
            infobutton <<- ""
            if(infotoggle()) { infotoggle(FALSE) }
            else { infotoggle(TRUE) }
          }) %>% bindEvent(input$fileinfoMLEstimatesOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            ibutton <<- ""
            infobutton <<- "infoMLEstimatesOUP"
            if(infotoggle()) { infotoggle(FALSE) }
            else { infotoggle(TRUE) }
          }) %>% bindEvent(input$infoMLEstimatesOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
        # Goodness-of-Fit ----
        else if(input$navMLOUP == "MLGoodnessOUP")
        {
          # define go function ----
          Go <- function()
          {
            # message("go")
            goods <- ML$GoodnessOfFit()
            estimation <- ML$get_timeseries_info()[[6]]
            theta <- format(goods[[1]],digits=6)
            theta_i <- format(goods[[2]],digits=6)
            theta_s <- format(goods[[3]],digits=6)
            inv <- format(goods[[4]],digits=6)
            sbm <- format(goods[[5]],digits=6)
            output$paramMLGoodnessOUP <- renderUI({
              HTML(paste(sep="",
                "<table align='center'>
                  <tr>
                    <th></th>
                    <th style='text-align: right; padding: 2px 6px 2px 8px; border-bottom: 1px solid grey;'>rho</th>
                    <th style='text-align: right; padding: 2px 6px 2px 6px; border-bottom: 1px solid grey;'>mu</th>
                    <th style='text-align: right; padding: 2px 6px 2px 6px; border-bottom: 1px solid grey;'>sigma</th>
                    <th style='text-align: right; padding: 2px 6px 2px 6px; border-bottom: 1px solid grey;'>LnL</th>
                    <th style='text-align: right; padding: 2px 6px 2px 6px; border-bottom: 1px solid grey;'>k</th>
                    <th style='text-align: right; padding: 2px 6px 2px 6px; border-bottom: 1px solid grey;'>alpha</th>
                    <th style='text-align: right; padding: 2px 8px 2px 6px; border-bottom: 1px solid grey;'>m-1</th>
                  </tr>
                  <tr>
                    <td style='text-align: right; padding: 8px 6px 2px 8px;'><b>",estimation,"</b></td>
                    <td style='text-align: right; padding: 8px 6px 2px 6px;'>",theta[[1]],"</td>
                    <td style='text-align: right; padding: 8px 6px 2px 6px;'>",theta[[2]],"</td>
                    <td style='text-align: right; padding: 8px 6px 2px 6px;'>",theta[[3]],"</td>
                    <td style='text-align: right; padding: 8px 6px 2px 6px;'>",theta[[4]],"</td>
                    <td style='text-align: right; padding: 8px 6px 2px 6px;'>",theta[[5]],"</td>
                    <td style='text-align: right; padding: 8px 6px 2px 6px;'>",theta[[6]],"</td>
                    <td style='text-align: right; padding: 8px 8px 2px 6px;'>",theta[[7]],"</td>
                  </tr>
                  <tr>
                    <td style='text-align: right; padding: 2px 6px 2px 8px;'><b>Invariant</b></td>
                    <td style='text-align: right; padding: 2px 6px 2px 6px;'>",theta_i[[1]],"</td>
                    <td style='text-align: right; padding: 2px 6px 2px 6px;'>",theta_i[[2]],"</td>
                    <td style='text-align: right; padding: 2px 6px 2px 6px;'>",theta_i[[3]],"</td>
                    <td style='text-align: right; padding: 2px 6px 2px 6px;'>",theta_i[[4]],"</td>
                    <td style='text-align: right; padding: 2px 6px 2px 6px;'>",theta_i[[5]],"</td>
                    <td style='text-align: right; padding: 2px 6px 2px 6px;'>",theta_i[[6]],"</td>
                    <td style='text-align: right; padding: 2px 8px 2px 6px;'>",theta_i[[7]],"</td>
                  </tr>
                  <tr>
                    <td style='text-align: right; padding: 2px 6px 8px 8px;'><b>Scaled BM</b></td>
                    <td style='text-align: right; padding: 2px 6px 8px 6px;'>",theta_s[[1]],"</td>
                    <td style='text-align: right; padding: 2px 6px 8px 6px;'>",theta_s[[2]],"</td>
                    <td style='text-align: right; padding: 2px 6px 8px 6px;'>",theta_s[[3]],"</td>
                    <td style='text-align: right; padding: 2px 6px 8px 6px;'>",theta_s[[4]],"</td>
                    <td style='text-align: right; padding: 2px 6px 8px 6px;'>",theta_s[[5]],"</td>
                    <td style='text-align: right; padding: 2px 6px 8px 6px;'>",theta_s[[6]],"</td>
                    <td style='text-align: right; padding: 2px 8px 8px 6px;'>",theta_s[[7]],"</td>
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
                    <td style='text-align: right; padding: 6px;'>",inv[[1]],"</td>
                    <td style='text-align: right; padding: 6px;'>",sbm[[1]],"</td>
                  </tr>
                  <tr style='border-bottom: 1px solid grey;'>
                    <td style='text-align: right; padding: 6px;'>1-<i>P</i></td>
                    <td style='text-align: right; padding: 6px;'>",inv[[2]],"</td>
                    <td style='text-align: right; padding: 6px;'>",sbm[[2]],"</td>
                  </tr>
                </table>"
              ))
            })
          }
          # define data function ----
          DataRead <- function()
          {
            # message("Goodness DataRead")
            if(firsttab)
            {
              # message("firsttab")
              df <<- utils::read.csv(uploadpath,fileEncoding="UTF-8-BOM")
              framenames <<- colnames(df)
              dname[6] <<- uploadname
              tname[6] <<- framenames[1]
              sname[6] <<- framenames[2]
              nrows <<- nrow(df)
              ncols <<- ncol(df)
              nfirst <<- df[1,1]
              nlast <<- df[nrows,1]
              series <- ML$set_timeseries(df=df,taucol=1,zcol=2)
              Ixend <- nrow(series)
              end <- series[Ixend,1]
              if(Ixend > 200) { Ixbeg <- Ixend-200 }
              else { Ixbeg <- 1 }
              beg <- series[Ixbeg,1]
              ML$set_timeseries_info(tbeg=beg,tend=end,dataname=dname[6],timename=tname[6],statename=sname[6],NULL)
              isolate({
                updateSelectInput(session,"filesMLGoodnessOUP",choices=filelist,selected=dname[6])
                updateSelectInput(session,"timeMLGoodnessOUP",choices=framenames,selected=tname[6])
                updateSelectInput(session,"stateMLGoodnessOUP",choices=framenames,selected=sname[6])
              })
              firsttab <<- FALSE
              initialize[4] <<- FALSE
            }
            else if(initialize[4])
            {
              # message("initialize")
              df_info <- ML$get_timeseries_info()
              dname[6] <<- df_info[[3]]
              tname[6] <<- df_info[[4]]
              sname[6] <<- df_info[[5]]
              isolate({
                updateSelectInput(session,"filesMLGoodnessOUP",choices=filelist,selected=dname[6])
                updateSelectInput(session,"timeMLGoodnessOUP",choices=framenames,selected=tname[6])
                updateSelectInput(session,"stateMLGoodnessOUP",choices=framenames,selected=sname[6])
              })
              initialize[4] <<- FALSE
            }
            else
            {
              # message("else")
              df_info <- ML$get_timeseries_info()
              dataname <- df_info[[3]]
              timename <- df_info[[4]]
              statename <- df_info[[5]]
              if(dataname != dname[6] | timename != tname[6] | statename != sname[6])
              {
                isolate({
                  updateSelectInput(session,"filesMLGoodnessOUP",choices=filelist,selected=dataname)
                  updateSelectInput(session,"timeMLGoodnessOUP",choices=framenames,selected=timename)
                  updateSelectInput(session,"stateMLGoodnessOUP",choices=framenames,selected=statename)
                })
                dname[6] <<- dataname
                tname[6] <<- timename
                sname[6] <<- statename
              }
            }
          }
          # initialize ----
          DataRead()
          Go()  #no reactive in plot event which is not called on initialization
          # select ----
          observe({
            # message("Goodness observe file")
            if(dname[6] != input$filesMLGoodnessOUP)
            {
              dname[6] <<- input$filesMLGoodnessOUP
              if(dname[6] == uploadname) { filepath <- uploadpath }
              else { filepath <- paste(sep="",datapath,input$filesMLGoodnessOUP,".csv")  }
              df <<- utils::read.csv(filepath,fileEncoding="UTF-8-BOM")
              framenames <<- colnames(df)
              tname[6] <<- framenames[1]
              sname[6] <<- framenames[2]
              nrows <<- nrow(df)
              ncols <<- ncol(df)
              nfirst <<- df[1,1]
              nlast <<- df[nrows,1]
              series <- ML$set_timeseries(df=df,taucol=1,zcol=2)
              Ixend <- nrow(series)
              end <- series[Ixend,1]
              if(Ixend > 200) { Ixbeg <- Ixend-200 }
              else { Ixbeg <- 1 }
              beg <- series[Ixbeg,1]
              ML$set_timeseries_info(tbeg=beg,tend=end,dataname=dname[6],timename=tname[6],statename=sname[6])
              isolate({
                updateSelectInput(session,"timeMLGoodnessOUP",choices=framenames,selected=tname[6])
                updateSelectInput(session,"stateMLGoodnessOUP",choices=framenames,selected=sname[6])
              })
              ML$Estimates()
            }
          }) %>% bindEvent(input$filesMLGoodnessOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            # message("Goodness observe time")
            if(tname[6] != input$timeMLGoodnessOUP)
            {
              tname[6] <<- input$timeMLGoodnessOUP
              taucol <- match(tname[6],framenames)
              zcol <- match(sname[6],framenames)
              series <- ML$set_timeseries(df=df,taucol=taucol,zcol=zcol)
              Ixend <- nrow(series)
              end <- series[Ixend,1]
              if(Ixend > 200) { Ixbeg <- Ixend-200 }
              else { Ixbeg <- 1 }
              beg <- series[Ixbeg,1]
              ML$set_timeseries_info(tbeg=beg,tend=end,dataname=dname[6],timename=tname[6],statename=sname[6],NULL)
              ML$Estimates()
            }
          }) %>% bindEvent(input$timeMLGoodnessOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            # message("Goodness observe state")
            if(sname[6] != input$stateMLGoodnessOUP)
            {
              sname[6] <<- input$stateMLGoodnessOUP
              taucol <- match(tname[6],framenames)
              zcol <- match(sname[6],framenames)
              series <- ML$set_timeseries(df=df,taucol=taucol,zcol=zcol)
              Ixend <- nrow(series)
              end <- series[Ixend,1]
              if(Ixend > 200) { Ixbeg <- Ixend-200 }
              else { Ixbeg <- 1 }
              beg <- series[Ixbeg,1]
              ML$set_timeseries_info(tbeg=beg,tend=end,dataname=dname[6],timename=tname[6],statename=sname[6],NULL)
              ML$Estimates()
            }
          }) %>% bindEvent(input$stateMLGoodnessOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # user clicks go (or enter key) ----
          observe({
            Go()
          }) %>% bindEvent(input$plotMLGoodnessOUP)
          # observe i and info ----
          observe({
            ibutton <<- input$filesMLGoodnessOUP
            infobutton <<- ""
            if(infotoggle()) { infotoggle(FALSE) }
            else { infotoggle(TRUE) }
          }) %>% bindEvent(input$fileinfoMLGoodnessOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            ibutton <<- ""
            infobutton <<- "infoMLGoodnessOUP"
            if(infotoggle()) { infotoggle(FALSE) }
            else { infotoggle(TRUE) }
          }) %>% bindEvent(input$infoMLGoodnessOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
        # Likelihood Ratio Test ----
        else if(input$navMLOUP == "MLRatioOUP")
        {
          # define go function ----
          Go <- function()
          {
            # message("go")
            ratio <- ML$LikelihoodRatioTest()
            estimation <- ML$get_timeseries_info()[[6]]
            theta_u <- format(ratio[[1]],digits=6)
            theta <- format(ratio[[2]],digits=6)
            r2 <- format(ratio[[3]],digits=6)
            pval <- format(ratio[[4]],digits=6)
            output$paramMLRatioOUP <- renderUI({
              HTML(paste(sep="",
                "<table align='center'>
                  <tr>
                    <th></th>
                    <th style='text-align: right; padding: 2px 6px 2px 8px; border-bottom: 1px solid grey;'>rho</th>
                    <th style='text-align: right; padding: 2px 6px 2px 6px; border-bottom: 1px solid grey;'>mu</th>
                    <th style='text-align: right; padding: 2px 6px 2px 6px; border-bottom: 1px solid grey;'>sigma</th>
                    <th style='text-align: right; padding: 2px 6px 2px 6px; border-bottom: 1px solid grey;'>LnL</th>
                    <th style='text-align: right; padding: 2px 6px 2px 6px; border-bottom: 1px solid grey;'>k</th>
                    <th style='text-align: right; padding: 2px 6px 2px 6px; border-bottom: 1px solid grey;'>alpha</th>
                    <th style='text-align: right; padding: 2px 8px 2px 6px; border-bottom: 1px solid grey;'>m-1</th>
                  </tr>
                  <tr>
                    <td style='text-align: right; padding: 2px 6px 2px 8px;'><b>Unrestricted</b></td>
                    <td style='text-align: right; padding: 2px 6px 2px 6px;'>",theta_u[[1]],"</td>
                    <td style='text-align: right; padding: 2px 6px 2px 6px;'>",theta_u[[2]],"</td>
                    <td style='text-align: right; padding: 2px 6px 2px 6px;'>",theta_u[[3]],"</td>
                    <td style='text-align: right; padding: 2px 6px 2px 6px;'>",theta_u[[4]],"</td>
                    <td style='text-align: right; padding: 2px 6px 2px 6px;'>",theta_u[[5]],"</td>
                    <td style='text-align: right; padding: 2px 6px 2px 6px;'>",theta_u[[6]],"</td>
                    <td style='text-align: right; padding: 2px 8px 2px 6px;'>",theta_u[[7]],"</td>
                  </tr>
                  <tr>
                    <td style='text-align: right; padding: 2px 6px 8px 8px;'><b>",estimation,"</b></td>
                    <td style='text-align: right; padding: 2px 6px 8px 6px;'>",theta[[1]],"</td>
                    <td style='text-align: right; padding: 2px 6px 8px 6px;'>",theta[[2]],"</td>
                    <td style='text-align: right; padding: 2px 6px 8px 6px;'>",theta[[3]],"</td>
                    <td style='text-align: right; padding: 2px 6px 8px 6px;'>",theta[[4]],"</td>
                    <td style='text-align: right; padding: 2px 6px 8px 6px;'>",theta[[5]],"</td>
                    <td style='text-align: right; padding: 2px 6px 8px 6px;'>",theta[[6]],"</td>
                    <td style='text-align: right; padding: 2px 8px 8px 6px;'>",theta[[7]],"</td>
                  </tr>
                </table>"
              ))
            })
            output$ratioMLRatioOUP <- renderUI({
              HTML(paste(sep="",
                "<table align='center'>
                  <tr style='border-bottom: 1px solid grey;'>
                    <th></th>
                    <th style='text-align: right; padding: 6px;'>",estimation,"</th>
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
          }
          # define data function ----
          DataRead <- function()
          {
            # message("Ratio DataRead")
            if(firsttab)
            {
              # message("firsttab")
              df <<- utils::read.csv(uploadpath,fileEncoding="UTF-8-BOM")
              framenames <<- colnames(df)
              dname[7] <<- uploadname
              tname[7] <<- framenames[1]
              sname[7] <<- framenames[2]
              nrows <<- nrow(df)
              ncols <<- ncol(df)
              nfirst <<- df[1,1]
              nlast <<- df[nrows,1]
              series <- ML$set_timeseries(df=df,taucol=1,zcol=2)
              Ixend <- nrow(series)
              end <- series[Ixend,1]
              if(Ixend > 200) { Ixbeg <- Ixend-200 }
              else { Ixbeg <- 1 }
              beg <- series[Ixbeg,1]
              ML$set_timeseries_info(tbeg=beg,tend=end,dataname=dname[7],timename=tname[7],statename=sname[7],NULL)
              isolate({
                updateSelectInput(session,"filesMLRatioOUP",choices=filelist,selected=dname[7])
                updateSelectInput(session,"timeMLRatioOUP",choices=framenames,selected=tname[7])
                updateSelectInput(session,"stateMLRatioOUP",choices=framenames,selected=sname[7])
              })
              firsttab <<- FALSE
              initialize[5] <<- FALSE
            }
            else if(initialize[5])
            {
              # message("initialize")
              df_info <- ML$get_timeseries_info()
              dname[7] <<- df_info[[3]]
              tname[7] <<- df_info[[4]]
              sname[7] <<- df_info[[5]]
              isolate({
                updateSelectInput(session,"filesMLRatioOUP",choices=filelist,selected=dname[7])
                updateSelectInput(session,"timeMLRatioOUP",choices=framenames,selected=tname[7])
                updateSelectInput(session,"stateMLRatioOUP",choices=framenames,selected=sname[7])
              })
              initialize[5] <<- FALSE
            }
            else
            {
              # message("else")
              df_info <- ML$get_timeseries_info()
              dataname <- df_info[[3]]
              timename <- df_info[[4]]
              statename <- df_info[[5]]
              if(dataname != dname[7] | timename != tname[7] | statename != sname[7])
              {
                isolate({
                  updateSelectInput(session,"filesMLRatioOUP",choices=filelist,selected=dataname)
                  updateSelectInput(session,"timeMLRatioOUP",choices=framenames,selected=timename)
                  updateSelectInput(session,"stateMLRatioOUP",choices=framenames,selected=statename)
                })
                dname[7] <<- dataname
                tname[7] <<- timename
                sname[7] <<- statename
              }
            }
          }
          # initialize ----
          DataRead()
          Go() #no reactive in plot event which is not called on initialization
          # select ----
          observe({
            # message("Ratio observe file")
            if(dname[7] != input$filesMLRatioOUP)
            {
              dname[7] <<- input$filesMLRatioOUP
              if(dname[7] == uploadname) { filepath <- uploadpath }
              else { filepath <- paste(sep="",datapath,input$filesMLRatioOUP,".csv")  }
              df <<- utils::read.csv(filepath,fileEncoding="UTF-8-BOM")
              framenames <<- colnames(df)
              tname[7] <<- framenames[1]
              sname[7] <<- framenames[2]
              nrows <<- nrow(df)
              ncols <<- ncol(df)
              nfirst <<- df[1,1]
              nlast <<- df[nrows,1]
              series <- ML$set_timeseries(df=df,taucol=1,zcol=2)
              Ixend <- nrow(series)
              end <- series[Ixend,1]
              if(Ixend > 200) { Ixbeg <- Ixend-200 }
              else { Ixbeg <- 1 }
              beg <- series[Ixbeg,1]
              ML$set_timeseries_info(tbeg=beg,tend=end,dataname=dname[7],timename=tname[7],statename=sname[7])
              isolate({
                updateSelectInput(session,"timeMLRatioOUP",choices=framenames,selected=tname[7])
                updateSelectInput(session,"stateMLRatioOUP",choices=framenames,selected=sname[7])
              })
              ML$Estimates()
            }
          }) %>% bindEvent(input$filesMLRatioOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            # message("Ratio observe time")
            if(tname[7] != input$timeMLRatioOUP)
            {
              tname[7] <<- input$timeMLRatioOUP
              taucol <- match(tname[7],framenames)
              zcol <- match(sname[7],framenames)
              series <- ML$set_timeseries(df=df,taucol=taucol,zcol=zcol)
              Ixend <- nrow(series)
              end <- series[Ixend,1]
              if(Ixend > 200) { Ixbeg <- Ixend-200 }
              else { Ixbeg <- 1 }
              beg <- series[Ixbeg,1]
              ML$set_timeseries_info(tbeg=beg,tend=end,dataname=dname[7],timename=tname[7],statename=sname[7],NULL)
              ML$Estimates()
            }
          }) %>% bindEvent(input$timeMLRatioOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            # message("Ratio observe state")
            if(sname[7] != input$stateMLRatioOUP)
            {
              sname[7] <<- input$stateMLRatioOUP
              taucol <- match(tname[7],framenames)
              zcol <- match(sname[7],framenames)
              series <- ML$set_timeseries(df=df,taucol=taucol,zcol=zcol)
              Ixend <- nrow(series)
              end <- series[Ixend,1]
              if(Ixend > 200) { Ixbeg <- Ixend-200 }
              else { Ixbeg <- 1 }
              beg <- series[Ixbeg,1]
              ML$set_timeseries_info(tbeg=beg,tend=end,dataname=dname[7],timename=tname[7],statename=sname[7],NULL)
              ML$Estimates()
            }
          }) %>% bindEvent(input$stateMLRatioOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # user clicks go (or enter key) ----
          observe({
            Go()
          }) %>% bindEvent(input$plotMLRatioOUP)
          # observe i and info ----
          observe({
            ibutton <<- input$filesMLRatioOUP
            infobutton <<- ""
            if(infotoggle()) { infotoggle(FALSE) }
            else { infotoggle(TRUE) }
          }) %>% bindEvent(input$fileinfoMLRatioOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            ibutton <<- ""
            infobutton <<- "infoMLRatioOUP"
            if(infotoggle()) { infotoggle(FALSE) }
            else { infotoggle(TRUE) }
          }) %>% bindEvent(input$infoMLRatioOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
      })
    }
    else if(input$navBar == "tabMCOUP")
    {
      observeEvent(input$navMCOUP,{
        # Forward Paths ----
        if(input$navMCOUP == "MCForwardOUP")
        {
          # define set/get functions ----
          FromR6toUI <- function()
          {
            # Get from OUP ----
            oup_params <- MC$get_oup_params()
            y_stoch_args <- MC$get_y_stoch_args()
            t_stoch_args <- MC$get_t_stoch_args()
            path_args <- MC$get_path_args()
            plot_args <- MC$get_plot_args()
            type <- MC$get_plot_types()[[1]][1]
            rho <- oup_params[[1]]
            mu <- oup_params[[2]]
            sigma <- oup_params[[3]]
            if(type < -1.5)
            {
              t <- y_stoch_args[[1]]
              x <- y_stoch_args[[3]]
            }
            else
            {
              t <- t_stoch_args[[1]]
              x <- t_stoch_args[[3]]
            }
            k <- t_stoch_args[[2]]
            paths <- path_args[[1]]
            skip <- path_args[[2]]
            first <<- plot_args[[3]]
            last <<- plot_args[[4]]
            m <- length(t)
            tFrom <- t[1]
            tTo <- t[m]
            if(m > 1) { tBy <- (tTo-tFrom)/(m-1) }
            else  {tBy <- 0 }
            # Set to UI ----
            isolate({
              updateNumericInput(session,"rhoMCForwardOUP",value=rho)
              updateNumericInput(session,"muMCForwardOUP",value=mu)
              updateNumericInput(session,"sigmaMCForwardOUP",value=sigma)
              updateNumericInput(session,"tFromMCForwardOUP",value=tFrom)
              updateNumericInput(session,"tToMCForwardOUP",value=tTo)
              updateNumericInput(session,"tByMCForwardOUP",value=tBy)
              if(type < -1.5) { updateNumericInput(session,"kMCForwardOUP",label="~",value=k) }
              else { updateNumericInput(session,"kMCForwardOUP",label="k",value=k) }
              updateNumericInput(session,"xMCForwardOUP",value=x)
              updateNumericInput(session,"pathsMCForwardOUP",value=paths)
              updateNumericInput(session,"skipMCForwardOUP",value=skip)
              updateNumericInput(session,"firstMCForwardOUP",value=first)
              updateNumericInput(session,"lastMCForwardOUP",value=last)
            })
          }
          FromUItoR6 <- function()
          {
            # Get from UI ----
            isolate({
              rho <- input$rhoMCForwardOUP
              mu <- input$muMCForwardOUP
              sigma <- input$sigmaMCForwardOUP
              tFrom <- input$tFromMCForwardOUP
              tTo <- input$tToMCForwardOUP
              tBy <- input$tByMCForwardOUP
              k <- input$kMCForwardOUP
              x <- input$xMCForwardOUP
              paths <- input$pathsMCForwardOUP
              skip <- input$skipMCForwardOUP
              first <<- input$firstMCForwardOUP
              last <<- input$lastMCForwardOUP
            })
            if(!is.numeric(rho)) { rho <- 0 }
            else if(rho < 0) { rho <- 0 }
            if(!is.numeric(mu)) { mu <- 0 }
            if(!is.numeric(sigma)) { sigma <- 0 }
            t <- axissequence(tFrom,tTo,tBy)
            if(!is.numeric(k)) { k <- 0 }
            if(!is.numeric(x)) { x <- 0 }
            if(!is.numeric(paths)) { paths <- 100 }
            if(!is.numeric(skip)) { skip <- 1 }
            else if(skip > 5) { skip <- 5 }
            if(!is.numeric(first)) { first <- 1 }
            if(!is.numeric(last)) { last <- 10 }
            # Set to OUP ----
            MC$set_oup_params(rho=rho,mu=mu,sigma=sigma)
            type <- MC$get_plot_types()[[1]][1]
            if(type < -1.5) { MC$set_y_stoch_args(t=t,x=x) }
            else { MC$set_t_stoch_args(t=t,x=x) }
            MC$set_t_stoch_args(k=k)
            MC$set_path_args(paths=paths,skip=skip)
            MC$set_plot_args(first=first,last=last)
          }
          # user clicks clear or save ----
          observe({
            FromUItoR6()
            MC$undo_clear()
            showNotification("argument set 1 out of 1.",id="MCundo",duration=2)
          }) %>% bindEvent(input$clearMCForwardOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            FromUItoR6()
            n <- MC$undo_save()
            showNotification(paste("argument set ",n," out of ",n,"."),id="MCundo",duration=2)
          }) %>% bindEvent(input$saveMCForwardOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # user clicks reset, undn, unup, sync, axes, plot (or enter key), left or rght ----
          output$plotlyMCForwardOUP <- renderPlotly({
            if(input$resetMCForwardOUP > MCbtns[1,1])
            {
              MCbtns[1,1] <<- input$resetMCForwardOUP
              FromUItoR6()
              MC$set_plot_args(first=1,last=10)
            }
            else if(input$undnMCForwardOUP > MCbtns[1,2])
            {
              MCbtns[1,2] <<- input$undnMCForwardOUP
              Ixn <- MC$undo_undo()
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="MCundo",duration=2)
            }
            else if(input$unupMCForwardOUP > MCbtns[1,3])
            {
              MCbtns[1,3] <<- input$unupMCForwardOUP
              Ixn <- MC$undo_undo(1)
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="MCundo",duration=2)
            }
            else if(input$syncMCForwardOUP > MCbtns[1,4])
            {
              MCbtns[1,4] <<- input$syncMCForwardOUP
              FromUItoR6()
              MC$sync_yxt_stoch()
            }
            else if(input$axesMCForwardOUP > MCbtns[1,5])
            {
              MCbtns[1,5] <<- input$axesMCForwardOUP
              FromUItoR6()
              type <- MC$get_plot_types()[[1]][1]
              if(type < -1.5) { MC$axes_y_stoch() }
              else { MC$axes_t_stoch() }
            }
            else if(input$plotMCForwardOUP > MCbtns[1,6])
            {
              MCbtns[1,6] <<- input$plotMCForwardOUP
              FromUItoR6()
            }
            else if(input$leftMCForwardOUP > MCbtns[1,7])
            {
              MCbtns[1,7] <<- input$leftMCForwardOUP
              FromUItoR6()
              MC$set_plot_type("p",1)
           }
            else if(input$rghtMCForwardOUP > MCbtns[1,8])
            {
              MCbtns[1,8] <<- input$rghtMCForwardOUP
              FromUItoR6()
              MC$set_plot_type("n",1)
            }
            FromR6toUI()
            MC$PlotForwardPaths()
          }) %>% bindEvent(input$resetMCForwardOUP,input$undnMCForwardOUP,input$unupMCForwardOUP,input$syncMCForwardOUP,input$axesMCForwardOUP,input$plotMCForwardOUP,input$leftMCForwardOUP,input$rghtMCForwardOUP)
          # observe info ----
          observe({
            ibutton <<- ""
            infobutton <<- "infoMCForwardOUP"
            if(infotoggle()) { infotoggle(FALSE) }
            else { infotoggle(TRUE) }
          }) %>% bindEvent(input$infoMCForwardOUP,ignoreNULL=TRUE,ignoreInit=TRUE)

        }
        # Backward Paths ----
        else if(input$navMCOUP == "MCBackwardOUP")
        {
          # define set/get functions ----
          FromR6toUI <- function()
          {
            # Get from OUP ----
            oup_params <- MC$get_oup_params()
            x_stoch_args <- MC$get_x_stoch_args()
            path_args <- MC$get_path_args()
            plot_args <- MC$get_plot_args()
            rho <- oup_params[[1]]
            mu <- oup_params[[2]]
            sigma <- oup_params[[3]]
            s <- x_stoch_args[[1]]
            y <- x_stoch_args[[3]]
            paths <- path_args[[1]]
            skip <- path_args[[2]]
            first <<- plot_args[[3]]
            last <<- plot_args[[4]]
            m <- length(s)
            sFrom <- s[m]
            sTo <- s[1]
            if(m > 1) { sBy <- (sTo-sFrom)/(m-1) }
            else  {sBy <- 0 }
            # Set to UI ----
            isolate({
              updateNumericInput(session,"rhoMCBackwardOUP",value=rho)
              updateNumericInput(session,"muMCBackwardOUP",value=mu)
              updateNumericInput(session,"sigmaMCBackwardOUP",value=sigma)
              updateNumericInput(session,"sFromMCBackwardOUP",value=sFrom)
              updateNumericInput(session,"sToMCBackwardOUP",value=sTo)
              updateNumericInput(session,"sByMCBackwardOUP",value=sBy)
              updateNumericInput(session,"yMCBackwardOUP",value=y)
              updateNumericInput(session,"pathsMCBackwardOUP",value=paths)
              updateNumericInput(session,"skipMCBackwardOUP",value=skip)
              updateNumericInput(session,"firstMCBackwardOUP",value=first)
              updateNumericInput(session,"lastMCBackwardOUP",value=last)
            })
          }
          FromUItoR6 <- function()
          {
            # Get from UI ----
            isolate({
              rho <- input$rhoMCBackwardOUP
              mu <- input$muMCBackwardOUP
              sigma <- input$sigmaMCBackwardOUP
              sFrom <- input$sFromMCBackwardOUP
              sTo <- input$sToMCBackwardOUP
              sBy <- input$sByMCBackwardOUP
              y <- input$yMCBackwardOUP
              paths <- input$pathsMCBackwardOUP
              skip <- input$skipMCBackwardOUP
              first <<- input$firstMCBackwardOUP
              last <<- input$lastMCBackwardOUP
            })
            if(!is.numeric(rho)) { rho <- 0 }
            else if(rho < 0) { rho <- 0 }
            if(!is.numeric(mu)) { mu <- 0 }
            if(!is.numeric(sigma)) { sigma <- 0 }
            s <- axissequence(sFrom,sTo,sBy)
            if(!is.numeric(y)) { y <- 0 }
            if(!is.numeric(paths)) { paths <- 100 }
            if(!is.numeric(skip)) { skip <- 1 }
            else if(skip > 5) { skip <- 5 }
            if(!is.numeric(first)) { first <- 1 }
            if(!is.numeric(last)) { last <- 10 }
            # Set to OUP ----
            MC$set_oup_params(rho=rho,mu=mu,sigma=sigma)
            MC$set_x_stoch_args(s=s,y=y)
            MC$set_path_args(paths=paths,skip=skip)
            MC$set_plot_args(first=first,last=last)
          }
          # user clicks clear or save ----
          observe({
            FromUItoR6()
            MC$undo_clear()
            showNotification("argument set 1 out of 1.",id="MCundo",duration=2)
          }) %>% bindEvent(input$clearMCBackwardOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            FromUItoR6()
            n <- MC$undo_save()
            showNotification(paste("argument set ",n," out of ",n,"."),id="MCundo",duration=2)
          }) %>% bindEvent(input$saveMCBackwardOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # user clicks reset, undn, unup, sync, axes, plot (or enter key), left or rght ----
          output$plotlyMCBackwardOUP <- renderPlotly({
            if(input$resetMCBackwardOUP > MCbtns[2,1])
            {
              MCbtns[2,1] <<- input$resetMCBackwardOUP
              FromUItoR6()
              MC$set_plot_args(first=1,last=10)
            }
            else if(input$undnMCBackwardOUP > MCbtns[2,2])
            {
              MCbtns[2,2] <<- input$undnMCBackwardOUP
              Ixn <- MC$undo_undo()
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="MCundo",duration=2)
            }
            else if(input$unupMCBackwardOUP > MCbtns[2,3])
            {
              MCbtns[2,3] <<- input$unupMCBackwardOUP
              Ixn <- MC$undo_undo(1)
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="MCundo",duration=2)
            }
            else if(input$syncMCBackwardOUP > MCbtns[2,4])
            {
              MCbtns[2,4] <<- input$syncMCBackwardOUP
              FromUItoR6()
              MC$sync_yxt_stoch()
            }
            else if(input$axesMCBackwardOUP > MCbtns[2,5])
            {
              MCbtns[2,5] <<- input$axesMCBackwardOUP
              FromUItoR6()
              MC$axes_x_stoch()
            }
            else if(input$plotMCBackwardOUP > MCbtns[2,6])
            {
              MCbtns[2,6] <<- input$plotMCBackwardOUP
              FromUItoR6()
            }
            else if(input$leftMCBackwardOUP > MCbtns[2,7])
            {
              MCbtns[2,7] <<- input$leftMCBackwardOUP
              FromUItoR6()
              MC$set_plot_type("p",1)
            }
            else if(input$rghtMCBackwardOUP > MCbtns[2,8])
            {
              MCbtns[2,8] <<- input$rghtMCBackwardOUP
              FromUItoR6()
              MC$set_plot_type("n",1)
            }
            FromR6toUI()
            MC$PlotBackwardPaths()
          }) %>% bindEvent(input$resetMCBackwardOUP,input$undnMCBackwardOUP,input$unupMCBackwardOUP,input$syncMCBackwardOUP,input$axesMCBackwardOUP,input$plotMCBackwardOUP,input$leftMCBackwardOUP,input$rghtMCBackwardOUP)
          # observe info ----
          observe({
            ibutton <<- ""
            infobutton <<- "infoMCBackwardOUP"
            if(infotoggle()) { infotoggle(FALSE) }
            else { infotoggle(TRUE) }
          }) %>% bindEvent(input$infoMCBackwardOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
        # Bounded Paths ----
        else if(input$navMCOUP == "MCBoundedOUP")
        {
          # define set/get functions ----
          FromR6toUI <- function()
          {
            # Get from OUP ----
            oup_params <- MC$get_oup_params()
            t_stoch_args <- MC$get_t_stoch_args()
            path_args <- MC$get_path_args()
            plot_args <- MC$get_plot_args()
            rho <- oup_params[[1]]
            mu <- oup_params[[2]]
            sigma <- oup_params[[3]]
            t <- t_stoch_args[[1]]
            k <- t_stoch_args[[2]]
            x <- t_stoch_args[[3]]
            paths <- path_args[[1]]
            skip <- path_args[[2]]
            first <<- plot_args[[3]]
            last <<- plot_args[[4]]
            m <- length(t)
            tFrom <- t[1]
            tTo <- t[m]
            if(m > 1) { tBy <- (tTo-tFrom)/(m-1) }
            else  {tBy <- 0 }
            # Set to UI ----
            isolate({
              updateNumericInput(session,"rhoMCBoundedOUP",value=rho)
              updateNumericInput(session,"muMCBoundedOUP",value=mu)
              updateNumericInput(session,"sigmaMCBoundedOUP",value=sigma)
              updateNumericInput(session,"tFromMCBoundedOUP",value=tFrom)
              updateNumericInput(session,"tToMCBoundedOUP",value=tTo)
              updateNumericInput(session,"tByMCBoundedOUP",value=tBy)
              updateNumericInput(session,"kMCBoundedOUP",value=k)
              updateNumericInput(session,"xMCBoundedOUP",value=x)
              updateNumericInput(session,"pathsMCBoundedOUP",value=paths)
              updateNumericInput(session,"skipMCBoundedOUP",value=skip)
              updateNumericInput(session,"firstMCBoundedOUP",value=first)
              updateNumericInput(session,"lastMCBoundedOUP",value=last)
            })
          }
          FromUItoR6 <- function()
          {
            # Get from UI ----
            isolate({
              rho <- input$rhoMCBoundedOUP
              mu <- input$muMCBoundedOUP
              sigma <- input$sigmaMCBoundedOUP
              tFrom <- input$tFromMCBoundedOUP
              tTo <- input$tToMCBoundedOUP
              tBy <- input$tByMCBoundedOUP
              k <- input$kMCBoundedOUP
              x <- input$xMCBoundedOUP
              paths <- input$pathsMCBoundedOUP
              skip <- input$skipMCBoundedOUP
              first <<- input$firstMCBoundedOUP
              last <<- input$lastMCBoundedOUP
            })
            if(!is.numeric(rho)) { rho <- 0 }
            else if(rho < 0) { rho <- 0 }
            if(!is.numeric(mu)) { mu <- 0 }
            if(!is.numeric(sigma)) { sigma <- 0 }
            t <- axissequence(tFrom,tTo,tBy)
            if(!is.numeric(k)) { k <- 0 }
            if(!is.numeric(x)) { x <- 0 }
            if(!is.numeric(paths)) { paths <- 100 }
            if(!is.numeric(skip)) { skip <- 1 }
            else if(skip > 5) { skip <- 5 }
            if(!is.numeric(first)) { first <- 1 }
            if(!is.numeric(last)) { last <- 10 }
            # Set to OUP ----
            MC$set_oup_params(rho=rho,mu=mu,sigma=sigma)
            MC$set_t_stoch_args(t=t,k=k,x=x)
            MC$set_path_args(paths=paths,skip=skip)
            MC$set_plot_args(first=first,last=last)
          }
          # user clicks clear or save ----
          observe({
            FromUItoR6()
            MC$undo_clear()
            showNotification("argument set 1 out of 1.",id="MCundo",duration=2)
          }) %>% bindEvent(input$clearMCBoundedOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            FromUItoR6()
            n <- MC$undo_save()
            showNotification(paste("argument set ",n," out of ",n,"."),id="MCundo",duration=2)
          }) %>% bindEvent(input$saveMCBoundedOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # user clicks reset, undn, unup, sync, axes, plot (or enter key), left or rght ----
          output$plotlyMCBoundedOUP <- renderPlotly({
            if(input$resetMCBoundedOUP > MCbtns[3,1])
            {
              MCbtns[3,1] <<- input$resetMCBoundedOUP
              FromUItoR6()
              MC$set_plot_args(first=1,last=10)
            }
            else if(input$undnMCBoundedOUP > MCbtns[3,2])
            {
              MCbtns[3,2] <<- input$undnMCBoundedOUP
              Ixn <- MC$undo_undo()
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="MCundo",duration=2)
            }
            else if(input$unupMCBoundedOUP > MCbtns[3,3])
            {
              MCbtns[3,3] <<- input$unupMCBoundedOUP
              Ixn <- MC$undo_undo(1)
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="MCundo",duration=2)
            }
            else if(input$syncMCBoundedOUP > MCbtns[3,4])
            {
              MCbtns[3,4] <<- input$syncMCBoundedOUP
              FromUItoR6()
              MC$sync_yxt_stoch()
            }
            else if(input$axesMCBoundedOUP > MCbtns[3,5])
            {
              MCbtns[3,5] <<- input$axesMCBoundedOUP
              FromUItoR6()
              MC$axes_t_stoch()
            }
            else if(input$plotMCBoundedOUP > MCbtns[3,6])
            {
              MCbtns[3,6] <<- input$plotMCBoundedOUP
              FromUItoR6()
            }
            else if(input$leftMCBoundedOUP > MCbtns[3,7])
            {
              MCbtns[3,7] <<- input$leftMCBoundedOUP
              FromUItoR6()
              MC$set_plot_type("p",1)
            }
            else if(input$rghtMCBoundedOUP > MCbtns[3,8])
            {
              MCbtns[3,8] <<- input$rghtMCBoundedOUP
              FromUItoR6()
              MC$set_plot_type("n",1)
            }
            FromR6toUI()
            MC$PlotBoundedPaths()
          }) %>% bindEvent(input$resetMCBoundedOUP,input$undnMCBoundedOUP,input$unupMCBoundedOUP,input$syncMCBoundedOUP,input$axesMCBoundedOUP,input$plotMCBoundedOUP,input$leftMCBoundedOUP,input$rghtMCBoundedOUP)
          # observe info ----
          observe({
            ibutton <<- ""
            infobutton <<- "infoMCBoundedOUP"
            if(infotoggle()) { infotoggle(FALSE) }
            else { infotoggle(TRUE) }
          }) %>% bindEvent(input$infoMCBoundedOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
        # Mean ----
        else if(input$navMCOUP == "MCMeanOUP")
        {
          # define set/get functions ----
          FromR6toUI <- function()
          {
            # Get from OUP ----
            oup_params <- MC$get_oup_params()
            y_stoch_args <- MC$get_y_stoch_args()
            path_args <- MC$get_path_args()
            plot_args <- MC$get_plot_args()
            type <- MC$get_plot_types()[[1]][2]
            rho <- oup_params[[1]]
            mu <- oup_params[[2]]
            sigma <- oup_params[[3]]
            t <- y_stoch_args[[1]]
            y <- y_stoch_args[[2]]
            x <- y_stoch_args[[3]]
            paths <- path_args[[1]]
            skip <- path_args[[2]]
            pmax <- plot_args[[1]]
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
            isolate({
              updateNumericInput(session,"rhoMCMeanOUP",value=rho)
              updateNumericInput(session,"muMCMeanOUP",value=mu)
              if(type < -0.5) { updateNumericInput(session,"sigmaMCMeanOUP",label="sigma",value=sigma) }
              else { updateNumericInput(session,"sigmaMCMeanOUP",label="~",value=sigma) }
              updateNumericInput(session,"tFromMCMeanOUP",value=tFrom)
              updateNumericInput(session,"tToMCMeanOUP",value=tTo)
              updateNumericInput(session,"tByMCMeanOUP",value=tBy)
              updateNumericInput(session,"yFromMCMeanOUP",value=yFrom)
              updateNumericInput(session,"yToMCMeanOUP",value=yTo)
              updateNumericInput(session,"yByMCMeanOUP",value=yBy)
              updateNumericInput(session,"xMCMeanOUP",value=x)
              if(type < -0.5) { updateNumericInput(session,"pmaxMCMeanOUP",label="p max",value=pmax) }
              else { updateNumericInput(session,"pmaxMCMeanOUP",label="~",value=pmax) }
              updateNumericInput(session,"pathsMCMeanOUP",value=paths)
              updateNumericInput(session,"skipMCMeanOUP",value=skip)
            })
          }
          FromUItoR6 <- function()
          {
            # Get from UI ----
            isolate({
              rho <- input$rhoMCMeanOUP
              mu <- input$muMCMeanOUP
              sigma <- input$sigmaMCMeanOUP
              tFrom <- input$tFromMCMeanOUP
              tTo <- input$tToMCMeanOUP
              tBy <- input$tByMCMeanOUP
              yFrom <- input$yFromMCMeanOUP
              yTo <- input$yToMCMeanOUP
              yBy <- input$yByMCMeanOUP
              x <- input$xMCMeanOUP
              pmax <- input$pmaxMCMeanOUP
              paths <- input$pathsMCMeanOUP
              skip <- input$skipMCMeanOUP
            })
            if(!is.numeric(rho)) { rho <- 0 }
            else if(rho < 0) { rho <- 0 }
            if(!is.numeric(mu)) { mu <- 0 }
            if(!is.numeric(sigma)) { sigma <- 0 }
            t <- axissequence(tFrom,tTo,tBy)
            y <- axissequence(yFrom,yTo,yBy)
            if(!is.numeric(x)) { x <- 0 }
            if(!is.numeric(pmax)) { pmax <- NaN }
            if(!is.numeric(paths)) { paths <- 100 }
            if(!is.numeric(skip)) { skip <- 1 }
            else if(skip > 5) { skip <- 5 }
            # Set to OUP ----
            MC$set_oup_params(rho=rho,mu=mu,sigma=sigma)
            MC$set_y_stoch_args(t=t,y=y,x=x)
            MC$set_path_args(paths=paths,skip=skip)
            MC$set_plot_args(pmax=pmax)
         }
          # user clicks clear or save ----
          observe({
            FromUItoR6()
            MC$undo_clear()
            showNotification("argument set 1 out of 1.",id="MCundo",duration=2)
          }) %>% bindEvent(input$clearMCMeanOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            FromUItoR6()
            n <- MC$undo_save()
            showNotification(paste("argument set ",n," out of ",n,"."),id="MCundo",duration=2)
          }) %>% bindEvent(input$saveMCMeanOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # user clicks undn, unup, sync, axes, plot (or enter key) or other ----
          output$plotlyMCMeanOUP <- renderPlotly({
            if(input$undnMCMeanOUP > MCbtns[4,2])
            {
              MCbtns[4,2] <<- input$undnMCMeanOUP
              Ixn <- MC$undo_undo()
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="MCundo",duration=2)
            }
            else if(input$unupMCMeanOUP > MCbtns[4,3])
            {
              MCbtns[4,3] <<- input$unupMCMeanOUP
              Ixn <- MC$undo_undo(1)
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="MCundo",duration=2)
            }
            else if(input$syncMCMeanOUP > MCbtns[4,4])
            {
              MCbtns[4,4] <<- input$syncMCMeanOUP
              FromUItoR6()
              MC$sync_yxt_stoch()
            }
            else if(input$axesMCMeanOUP > MCbtns[4,5])
            {
              MCbtns[4,5] <<- input$axesMCMeanOUP
              FromUItoR6()
              MC$axes_y_stoch()
            }
            else if(input$plotMCMeanOUP > MCbtns[4,6])
            {
              MCbtns[4,6] <<- input$plotMCMeanOUP
              FromUItoR6()
            }
            else if(input$otherMCMeanOUP > MCbtns[4,7])
            {
              MCbtns[4,7] <<- input$otherMCMeanOUP
              FromUItoR6()
              MC$set_plot_type("p",2)
            }
            FromR6toUI()
            MC$PlotMean()
          }) %>% bindEvent(input$undnMCMeanOUP,input$unupMCMeanOUP,input$syncMCMeanOUP,input$axesMCMeanOUP,input$plotMCMeanOUP,input$otherMCMeanOUP)
          # observe info ----
          observe({
            ibutton <<- ""
            infobutton <<- "infoMCMeanOUP"
            if(infotoggle()) { infotoggle(FALSE) }
            else { infotoggle(TRUE) }
          }) %>% bindEvent(input$infoMCMeanOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            removeModal(session)
            updateTabsetPanel(session,"navBar",selected="tabAOUP")
            updateTabsetPanel(session,"navAOUP",selected="AMeanOUP")
          }) %>% bindEvent(input$alsoMCMeanOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
        # Variance ----
        else if(input$navMCOUP == "MCVarianceOUP")
        {
          # define set/get functions ----
          FromR6toUI <- function()
          {
            # Get from OUP ----
            oup_params <- MC$get_oup_params()
            y_stoch_args <- MC$get_y_stoch_args()
            path_args <- MC$get_path_args()
            plot_args <- MC$get_plot_args()
            type <- MC$get_plot_types()[[1]][2]
            rho <- oup_params[[1]]
            mu <- oup_params[[2]]
            sigma <- oup_params[[3]]
            t <- y_stoch_args[[1]]
            y <- y_stoch_args[[2]]
            x <- y_stoch_args[[3]]
            paths <- path_args[[1]]
            skip <- path_args[[2]]
            pmax <- plot_args[[1]]
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
            isolate({
              updateNumericInput(session,"rhoMCVarianceOUP",value=rho)
              updateNumericInput(session,"muMCVarianceOUP",value=mu)
              if(type < -0.5) { updateNumericInput(session,"muMCVarianceOUP",label="mu",value=mu) }
              else { updateNumericInput(session,"muMCVarianceOUP",label="~",value=mu) }
              updateNumericInput(session,"sigmaMCVarianceOUP",value=sigma)
              updateNumericInput(session,"tFromMCVarianceOUP",value=tFrom)
              updateNumericInput(session,"tToMCVarianceOUP",value=tTo)
              updateNumericInput(session,"tByMCVarianceOUP",value=tBy)
              updateNumericInput(session,"yFromMCVarianceOUP",value=yFrom)
              updateNumericInput(session,"yToMCVarianceOUP",value=yTo)
              updateNumericInput(session,"yByMCVarianceOUP",value=yBy)
              updateNumericInput(session,"xMCVarianceOUP",value=x)
              if(type < -0.5) { updateNumericInput(session,"pmaxMCVarianceOUP",label="p max",value=pmax) }
              else { updateNumericInput(session,"pmaxMCVarianceOUP",label="~",value=pmax) }
              updateNumericInput(session,"pathsMCVarianceOUP",value=paths)
              updateNumericInput(session,"skipMCVarianceOUP",value=skip)
            })
          }
          FromUItoR6 <- function()
          {
            # Get from UI ----
            isolate({
              rho <- input$rhoMCVarianceOUP
              mu <- input$muMCVarianceOUP
              sigma <- input$sigmaMCVarianceOUP
              tFrom <- input$tFromMCVarianceOUP
              tTo <- input$tToMCVarianceOUP
              tBy <- input$tByMCVarianceOUP
              yFrom <- input$yFromMCVarianceOUP
              yTo <- input$yToMCVarianceOUP
              yBy <- input$yByMCVarianceOUP
              x <- input$xMCVarianceOUP
              pmax <- input$pmaxMCVarianceOUP
              paths <- input$pathsMCVarianceOUP
              skip <- input$skipMCVarianceOUP
            })
            if(!is.numeric(rho)) { rho <- 0 }
            else if(rho < 0) { rho <- 0 }
            if(!is.numeric(mu)) { mu <- 0 }
            if(!is.numeric(sigma)) { sigma <- 0 }
            t <- axissequence(tFrom,tTo,tBy)
            y <- axissequence(yFrom,yTo,yBy)
            if(!is.numeric(x)) { x <- 0 }
            if(!is.numeric(pmax)) { pmax <- NaN }
            if(!is.numeric(paths)) { paths <- 100 }
            if(!is.numeric(skip)) { skip <- 1 }
            else if(skip > 5) { skip <- 5 }
            # Set to OUP ----
            MC$set_oup_params(rho=rho,mu=mu,sigma=sigma)
            MC$set_y_stoch_args(t=t,y=y,x=x)
            MC$set_path_args(paths=paths,skip=skip)
            MC$set_plot_args(pmax=pmax)
         }
          # user clicks clear or save ----
          observe({
            FromUItoR6()
            MC$undo_clear()
            showNotification("argument set 1 out of 1.",id="MCundo",duration=2)
          }) %>% bindEvent(input$clearMCVarianceOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            FromUItoR6()
            n <- MC$undo_save()
            showNotification(paste("argument set ",n," out of ",n,"."),id="MCundo",duration=2)
          }) %>% bindEvent(input$saveMCVarianceOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # user clicks undn, unup, sync, axes, plot (or enter key) or other ----
          output$plotlyMCVarianceOUP <- renderPlotly({
            if(input$undnMCVarianceOUP > MCbtns[5,2])
            {
              MCbtns[5,2] <<- input$undnMCVarianceOUP
              Ixn <- MC$undo_undo()
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="MCundo",duration=2)
            }
            else if(input$unupMCVarianceOUP > MCbtns[5,3])
            {
              MCbtns[5,3] <<- input$unupMCVarianceOUP
              Ixn <- MC$undo_undo(1)
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="MCundo",duration=2)
            }
            else if(input$syncMCVarianceOUP > MCbtns[5,4])
            {
              MCbtns[5,4] <<- input$syncMCVarianceOUP
              FromUItoR6()
              MC$sync_yxt_stoch()
            }
            else if(input$axesMCVarianceOUP > MCbtns[5,5])
            {
              MCbtns[5,5] <<- input$axesMCVarianceOUP
              FromUItoR6()
              MC$axes_y_stoch()
            }
            else if(input$plotMCVarianceOUP > MCbtns[5,6])
            {
              MCbtns[5,6] <<- input$plotMCVarianceOUP
              FromUItoR6()
            }
            else if(input$otherMCVarianceOUP > MCbtns[5,7])
            {
              MCbtns[5,7] <<- input$otherMCVarianceOUP
              FromUItoR6()
              MC$set_plot_type("p",2)
            }
            FromR6toUI()
            MC$PlotVariance()
          }) %>% bindEvent(input$undnMCVarianceOUP,input$unupMCVarianceOUP,input$syncMCVarianceOUP,input$axesMCVarianceOUP,input$plotMCVarianceOUP,input$otherMCVarianceOUP)
          # observe info ----
          observe({
            ibutton <<- ""
            infobutton <<- "infoMCVarianceOUP"
            if(infotoggle()) { infotoggle(FALSE) }
            else { infotoggle(TRUE) }
          }) %>% bindEvent(input$infoMCVarianceOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            removeModal(session)
            updateTabsetPanel(session,"navBar",selected="tabAOUP")
            updateTabsetPanel(session,"navAOUP",selected="AVarianceOUP")
          }) %>% bindEvent(input$alsoMCVarianceOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
        # Transition Density ----
        else if(input$navMCOUP == "MCDensityOUP")
        {
          # define set/get functions ----
          FromR6toUI <- function()
          {
            # Get from OUP ----
            oup_params <- MC$get_oup_params()
            y_stoch_args <- MC$get_y_stoch_args()
            path_args <- MC$get_path_args()
            plot_args <- MC$get_plot_args()
            rho <- oup_params[[1]]
            mu <- oup_params[[2]]
            sigma <- oup_params[[3]]
            t <- y_stoch_args[[1]]
            y <- y_stoch_args[[2]]
            x <- y_stoch_args[[3]]
            paths <- path_args[[1]]
            skip <- path_args[[2]]
            pmax <- plot_args[[1]]
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
            isolate({
              updateNumericInput(session,"rhoMCDensityOUP",value=rho)
              updateNumericInput(session,"muMCDensityOUP",value=mu)
              updateNumericInput(session,"sigmaMCDensityOUP",value=sigma)
              updateNumericInput(session,"tFromMCDensityOUP",value=tFrom)
              updateNumericInput(session,"tToMCDensityOUP",value=tTo)
              updateNumericInput(session,"tByMCDensityOUP",value=tBy)
              updateNumericInput(session,"yFromMCDensityOUP",value=yFrom)
              updateNumericInput(session,"yToMCDensityOUP",value=yTo)
              updateNumericInput(session,"yByMCDensityOUP",value=yBy)
              updateNumericInput(session,"xMCDensityOUP",value=x)
              updateNumericInput(session,"pmaxMCDensityOUP",value=pmax)
              updateNumericInput(session,"pathsMCDensityOUP",value=paths)
              updateNumericInput(session,"skipMCDensityOUP",value=skip)
            })
          }
          FromUItoR6 <- function()
          {
            # Get from UI ----
            isolate({
              rho <- input$rhoMCDensityOUP
              mu <- input$muMCDensityOUP
              sigma <- input$sigmaMCDensityOUP
              tFrom <- input$tFromMCDensityOUP
              tTo <- input$tToMCDensityOUP
              tBy <- input$tByMCDensityOUP
              yFrom <- input$yFromMCDensityOUP
              yTo <- input$yToMCDensityOUP
              yBy <- input$yByMCDensityOUP
              x <- input$xMCDensityOUP
              pmax <- input$pmaxMCDensityOUP
              paths <- input$pathsMCDensityOUP
              skip <- input$skipMCDensityOUP
            })
            if(!is.numeric(rho)) { rho <- 0 }
            else if(rho < 0) { rho <- 0 }
            if(!is.numeric(mu)) { mu <- 0 }
            if(!is.numeric(sigma)) { sigma <- 0 }
            t <- axissequence(tFrom,tTo,tBy)
            y <- axissequence(yFrom,yTo,yBy)
            if(!is.numeric(x)) { x <- 0 }
            if(!is.numeric(pmax)) { pmax <- NaN }
            if(!is.numeric(paths)) { paths <- 100 }
            if(!is.numeric(skip)) { skip <- 1 }
            else if(skip > 5) { skip <- 5 }
            # Set to OUP ----
            MC$set_oup_params(rho=rho,mu=mu,sigma=sigma)
            MC$set_y_stoch_args(t=t,y=y,x=x)
            MC$set_path_args(paths=paths,skip=skip)
            MC$set_plot_args(pmax=pmax)
         }
          # user clicks clear or save ----
          observe({
            FromUItoR6()
            MC$undo_clear()
            showNotification("argument set 1 out of 1.",id="MCundo",duration=2)
          }) %>% bindEvent(input$clearMCDensityOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            FromUItoR6()
            n <- MC$undo_save()
            showNotification(paste("argument set ",n," out of ",n,"."),id="MCundo",duration=2)
          }) %>% bindEvent(input$saveMCDensityOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # user clicks undn, unup, sync, axes, plot (or enter key) or other ----
          output$plotlyMCDensityOUP <- renderPlotly({
            if(input$undnMCDensityOUP > MCbtns[6,2])
            {
              MCbtns[6,2] <<- input$undnMCDensityOUP
              Ixn <- MC$undo_undo()
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="MCundo",duration=2)
            }
            else if(input$unupMCDensityOUP > MCbtns[6,3])
            {
              MCbtns[6,3] <<- input$unupMCDensityOUP
              Ixn <- MC$undo_undo(1)
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="MCundo",duration=2)
            }
            else if(input$syncMCDensityOUP > MCbtns[6,4])
            {
              MCbtns[6,4] <<- input$syncMCDensityOUP
              FromUItoR6()
              MC$sync_yxt_stoch()
            }
            else if(input$axesMCDensityOUP > MCbtns[6,5])
            {
              MCbtns[6,5] <<- input$axesMCDensityOUP
              FromUItoR6()
              MC$axes_y_stoch()
            }
            else if(input$plotMCDensityOUP > MCbtns[6,6])
            {
              MCbtns[6,6] <<- input$plotMCDensityOUP
              FromUItoR6()
            }
            else if(input$otherMCDensityOUP > MCbtns[6,7])
            {
              MCbtns[6,7] <<- input$otherMCDensityOUP
              FromUItoR6()
              MC$set_plot_type("p",3)
            }
            FromR6toUI()
            MC$PlotDensity()
          }) %>% bindEvent(input$undnMCDensityOUP,input$unupMCDensityOUP,input$syncMCDensityOUP,input$axesMCDensityOUP,input$plotMCDensityOUP,input$otherMCDensityOUP)
          # observe info ----
          observe({
            ibutton <<- ""
            infobutton <<- "infoMCDensityOUP"
            if(infotoggle()) { infotoggle(FALSE) }
            else { infotoggle(TRUE) }
          }) %>% bindEvent(input$infoMCDensityOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
           observe({
            removeModal(session)
            updateTabsetPanel(session,"navBar",selected="tabAOUP")
            updateTabsetPanel(session,"navAOUP",selected="ADensityOUP")
          }) %>% bindEvent(input$alsoMCDensityOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
       }
        # Transition Probability ----
        else if(input$navMCOUP == "MCProbabilityOUP")
        {
          # define set/get functions ----
          FromR6toUI <- function()
          {
            # Get from OUP ----
            oup_params <- MC$get_oup_params()
            y_stoch_args <- MC$get_y_stoch_args()
            path_args <- MC$get_path_args()
            plot_args <- MC$get_plot_args()
            type <- MC$get_plot_types()[[1]][3]
            rho <- oup_params[[1]]
            mu <- oup_params[[2]]
            sigma <- oup_params[[3]]
            t <- y_stoch_args[[1]]
            y <- y_stoch_args[[2]]
            x <- y_stoch_args[[3]]
            psi <- y_stoch_args[[4]]
            paths <- path_args[[1]]
            skip <- path_args[[2]]
            pmax <- plot_args[[1]]
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
            isolate({
              updateNumericInput(session,"rhoMCProbabilityOUP",value=rho)
              updateNumericInput(session,"muMCProbabilityOUP",value=mu)
              updateNumericInput(session,"sigmaMCProbabilityOUP",value=sigma)
              updateNumericInput(session,"tFromMCProbabilityOUP",value=tFrom)
              updateNumericInput(session,"tToMCProbabilityOUP",value=tTo)
              updateNumericInput(session,"tByMCProbabilityOUP",value=tBy)
              updateNumericInput(session,"yFromMCProbabilityOUP",value=yFrom)
              updateNumericInput(session,"yToMCProbabilityOUP",value=yTo)
              updateNumericInput(session,"yByMCProbabilityOUP",value=yBy)
              updateNumericInput(session,"xMCProbabilityOUP",value=x)
              updateNumericInput(session,"psiMCProbabilityOUP",value=psi)
              if(type < 0.5) { updateNumericInput(session,"pmaxMCProbabilityOUP",label="~",value=pmax) }
              else { updateNumericInput(session,"pmaxMCProbabilityOUP",label="p max",value=pmax) }
              updateNumericInput(session,"pathsMCProbabilityOUP",value=paths)
              updateNumericInput(session,"skipMCProbabilityOUP",value=skip)
            })
          }
          FromUItoR6 <- function()
          {
            # Get from UI ----
            isolate({
              rho <- input$rhoMCProbabilityOUP
              mu <- input$muMCProbabilityOUP
              sigma <- input$sigmaMCProbabilityOUP
              tFrom <- input$tFromMCProbabilityOUP
              tTo <- input$tToMCProbabilityOUP
              tBy <- input$tByMCProbabilityOUP
              yFrom <- input$yFromMCProbabilityOUP
              yTo <- input$yToMCProbabilityOUP
              yBy <- input$yByMCProbabilityOUP
              x <- input$xMCProbabilityOUP
              psi <- input$psiMCProbabilityOUP
              pmax <- input$pmaxMCProbabilityOUP
              paths <- input$pathsMCProbabilityOUP
              skip <- input$skipMCProbabilityOUP
            })
            if(!is.numeric(rho)) { rho <- 0 }
            else if(rho < 0) { rho <- 0 }
            if(!is.numeric(mu)) { mu <- 0 }
            if(!is.numeric(sigma)) { sigma <- 0 }
            t <- axissequence(tFrom,tTo,tBy)
            y <- axissequence(yFrom,yTo,yBy)
            if(!is.numeric(x)) { x <- 0 }
            if(!is.numeric(psi)) { psi <- -1 }
            else if(psi <= 0) { psi <- -1 }
            else { psi <- 1 }
            if(!is.numeric(pmax)) { pmax <- NaN }
            if(!is.numeric(paths)) { paths <- 100 }
            if(!is.numeric(skip)) { skip <- 1 }
            else if(skip > 5) { skip <- 5 }
            # Set to OUP ----
            MC$set_oup_params(rho=rho,mu=mu,sigma=sigma)
            MC$set_y_stoch_args(t=t,y=y,x=x,psi=psi)
            MC$set_path_args(paths=paths,skip=skip)
            MC$set_plot_args(pmax=pmax)
         }
          # user clicks clear or save ----
          observe({
            FromUItoR6()
            MC$undo_clear()
            showNotification("argument set 1 out of 1.",id="MCundo",duration=2)
          }) %>% bindEvent(input$clearMCProbabilityOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            FromUItoR6()
            n <- MC$undo_save()
            showNotification(paste("argument set ",n," out of ",n,"."),id="MCundo",duration=2)
          }) %>% bindEvent(input$saveMCProbabilityOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # user clicks undn, unup, sync, axes, plot (or enter key) or other ----
          output$plotlyMCProbabilityOUP <- renderPlotly({
            if(input$undnMCProbabilityOUP > MCbtns[7,2])
            {
              MCbtns[7,2] <<- input$undnMCProbabilityOUP
              Ixn <- MC$undo_undo()
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="MCundo",duration=2)
            }
            else if(input$unupMCProbabilityOUP > MCbtns[7,3])
            {
              MCbtns[7,3] <<- input$unupMCProbabilityOUP
              Ixn <- MC$undo_undo(1)
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="MCundo",duration=2)
            }
            else if(input$syncMCProbabilityOUP > MCbtns[7,4])
            {
              MCbtns[7,4] <<- input$syncMCProbabilityOUP
              FromUItoR6()
              MC$sync_yxt_stoch()
            }
            else if(input$axesMCProbabilityOUP > MCbtns[7,5])
            {
              MCbtns[7,5] <<- input$axesMCProbabilityOUP
              FromUItoR6()
              MC$axes_y_stoch()
            }
            else if(input$plotMCProbabilityOUP > MCbtns[7,6])
            {
              MCbtns[7,6] <<- input$plotMCProbabilityOUP
              FromUItoR6()
            }
            else if(input$otherMCProbabilityOUP > MCbtns[7,7])
            {
              MCbtns[7,7] <<- input$otherMCProbabilityOUP
              FromUItoR6()
              MC$set_plot_type("p",3)
            }
            FromR6toUI()
            MC$PlotProbability()
          }) %>% bindEvent(input$undnMCProbabilityOUP,input$unupMCProbabilityOUP,input$syncMCProbabilityOUP,input$axesMCProbabilityOUP,input$plotMCProbabilityOUP,input$otherMCProbabilityOUP)
          # observe info ----
          observe({
            ibutton <<- ""
            infobutton <<- "infoMCProbabilityOUP"
            if(infotoggle()) { infotoggle(FALSE) }
            else { infotoggle(TRUE) }
          }) %>% bindEvent(input$infoMCProbabilityOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            removeModal(session)
            updateTabsetPanel(session,"navBar",selected="tabAOUP")
            updateTabsetPanel(session,"navAOUP",selected="AProbabilityOUP")
          }) %>% bindEvent(input$alsoMCProbabilityOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
        # Double Integral ----
        else if(input$navMCOUP == "MCDoubleOUP")
        {
          # define set/get functions ----
          FromR6toUI <- function()
          {
            # Get from OUP ----
            oup_params <- MC$get_oup_params()
            y_stoch_args <- MC$get_y_stoch_args()
            path_args <- MC$get_path_args()
            plot_args <- MC$get_plot_args()
            type <- MC$get_plot_types()[[1]][3]
            rho <- oup_params[[1]]
            mu <- oup_params[[2]]
            sigma <- oup_params[[3]]
            t <- y_stoch_args[[1]]
            y <- y_stoch_args[[2]]
            x <- y_stoch_args[[3]]
            psi <- y_stoch_args[[4]]
            paths <- path_args[[1]]
            skip <- path_args[[2]]
            pmax <- plot_args[[1]]
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
            isolate({
              updateNumericInput(session,"rhoMCDoubleOUP",value=rho)
              updateNumericInput(session,"muMCDoubleOUP",value=mu)
              updateNumericInput(session,"sigmaMCDoubleOUP",value=sigma)
              updateNumericInput(session,"tFromMCDoubleOUP",value=tFrom)
              updateNumericInput(session,"tToMCDoubleOUP",value=tTo)
              updateNumericInput(session,"tByMCDoubleOUP",value=tBy)
              updateNumericInput(session,"yFromMCDoubleOUP",value=yFrom)
              updateNumericInput(session,"yToMCDoubleOUP",value=yTo)
              updateNumericInput(session,"yByMCDoubleOUP",value=yBy)
              updateNumericInput(session,"xMCDoubleOUP",value=x)
              updateNumericInput(session,"psiMCDoubleOUP",value=psi)
              if(type < 0.5) { updateNumericInput(session,"pmaxMCDoubleOUP",label="~",value=pmax) }
              else { updateNumericInput(session,"pmaxMCDoubleOUP",label="p max",value=pmax) }
              updateNumericInput(session,"pathsMCDoubleOUP",value=paths)
              updateNumericInput(session,"skipMCDoubleOUP",value=skip)
            })
          }
          FromUItoR6 <- function()
          {
            # Get from UI ----
            isolate({
              rho <- input$rhoMCDoubleOUP
              mu <- input$muMCDoubleOUP
              sigma <- input$sigmaMCDoubleOUP
              tFrom <- input$tFromMCDoubleOUP
              tTo <- input$tToMCDoubleOUP
              tBy <- input$tByMCDoubleOUP
              yFrom <- input$yFromMCDoubleOUP
              yTo <- input$yToMCDoubleOUP
              yBy <- input$yByMCDoubleOUP
              x <- input$xMCDoubleOUP
              psi <- input$psiMCDoubleOUP
              pmax <- input$pmaxMCDoubleOUP
              paths <- input$pathsMCDoubleOUP
              skip <- input$skipMCDoubleOUP
            })
            if(!is.numeric(rho)) { rho <- 0 }
            else if(rho < 0) { rho <- 0 }
            if(!is.numeric(mu)) { mu <- 0 }
            if(!is.numeric(sigma)) { sigma <- 0 }
            t <- axissequence(tFrom,tTo,tBy)
            y <- axissequence(yFrom,yTo,yBy)
            if(!is.numeric(x)) { x <- 0 }
            if(!is.numeric(psi)) { psi <- -1 }
            else if(psi <= 0) { psi <- -1 }
            else { psi <- 1 }
            if(!is.numeric(pmax)) { pmax <- NaN }
            if(!is.numeric(paths)) { paths <- 100 }
            if(!is.numeric(skip)) { skip <- 1 }
            else if(skip > 5) { skip <- 5 }
            # Set to OUP ----
            MC$set_oup_params(rho=rho,mu=mu,sigma=sigma)
            MC$set_y_stoch_args(t=t,y=y,x=x,psi=psi)
            MC$set_path_args(paths=paths,skip=skip)
            MC$set_plot_args(pmax=pmax)
         }
          # user clicks clear or save ----
          observe({
            FromUItoR6()
            MC$undo_clear()
            showNotification("argument set 1 out of 1.",id="MCundo",duration=2)
          }) %>% bindEvent(input$clearMCDoubleOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            FromUItoR6()
            n <- MC$undo_save()
            showNotification(paste("argument set ",n," out of ",n,"."),id="MCundo",duration=2)
          }) %>% bindEvent(input$saveMCDoubleOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # user clicks undn, unup, sync, axes, plot (or enter key) or other ----
          output$plotlyMCDoubleOUP <- renderPlotly({
            if(input$undnMCDoubleOUP > MCbtns[8,2])
            {
              MCbtns[8,2] <<- input$undnMCDoubleOUP
              Ixn <- MC$undo_undo()
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="MCundo",duration=2)
            }
            else if(input$unupMCDoubleOUP > MCbtns[8,3])
            {
              MCbtns[8,3] <<- input$unupMCDoubleOUP
              Ixn <- MC$undo_undo(1)
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="MCundo",duration=2)
            }
            else if(input$syncMCDoubleOUP > MCbtns[8,4])
            {
              MCbtns[8,4] <<- input$syncMCDoubleOUP
              FromUItoR6()
              MC$sync_yxt_stoch()
            }
            else if(input$axesMCDoubleOUP > MCbtns[8,5])
            {
              MCbtns[8,5] <<- input$axesMCDoubleOUP
              FromUItoR6()
              MC$axes_y_stoch()
            }
            else if(input$plotMCDoubleOUP > MCbtns[8,6])
            {
              MCbtns[8,6] <<- input$plotMCDoubleOUP
              FromUItoR6()
            }
            else if(input$otherMCDoubleOUP > MCbtns[8,7])
            {
              MCbtns[8,7] <<- input$otherMCDoubleOUP
              FromUItoR6()
              MC$set_plot_type("p",3)
            }
            FromR6toUI()
            MC$PlotDoubleIntegral()
          }) %>% bindEvent(input$undnMCDoubleOUP,input$unupMCDoubleOUP,input$syncMCDoubleOUP,input$axesMCDoubleOUP,input$plotMCDoubleOUP,input$otherMCDoubleOUP)
          # observe info ----
          observe({
            ibutton <<- ""
            infobutton <<- "infoMCDoubleOUP"
            if(infotoggle()) { infotoggle(FALSE) }
            else { infotoggle(TRUE) }
          }) %>% bindEvent(input$infoMCDoubleOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            removeModal(session)
            updateTabsetPanel(session,"navBar",selected="tabAOUP")
            updateTabsetPanel(session,"navAOUP",selected="ADoubleOUP")
          }) %>% bindEvent(input$alsoMCDoubleOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
        # Option ----
        else if(input$navMCOUP == "MCOptionOUP")
        {
          # define set/get functions ----
          FromR6toUI <- function()
          {
            # Get from OUP ----
            oup_params <- MC$get_oup_params()
            x_stoch_args <- MC$get_x_stoch_args()
            path_args <- MC$get_path_args()
            plot_args <- MC$get_plot_args()
            type <- MC$get_plot_types()[[1]][3]
            rho <- oup_params[[1]]
            mu <- oup_params[[2]]
            sigma <- oup_params[[3]]
            s <- x_stoch_args[[1]]
            x <- x_stoch_args[[2]]
            y <- x_stoch_args[[3]]
            r <- x_stoch_args[[4]]
            phi <- x_stoch_args[[5]]
            paths <- path_args[[1]]
            skip <- path_args[[2]]
            pmax <- plot_args[[1]]
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
            isolate({
              updateNumericInput(session,"rhoMCOptionOUP",value=rho)
              updateNumericInput(session,"muMCOptionOUP",value=mu)
              updateNumericInput(session,"sigmaMCOptionOUP",value=sigma)
              updateNumericInput(session,"sFromMCOptionOUP",value=sFrom)
              updateNumericInput(session,"sToMCOptionOUP",value=sTo)
              updateNumericInput(session,"sByMCOptionOUP",value=sBy)
              updateNumericInput(session,"xFromMCOptionOUP",value=xFrom)
              updateNumericInput(session,"xToMCOptionOUP",value=xTo)
              updateNumericInput(session,"xByMCOptionOUP",value=xBy)
              updateNumericInput(session,"yMCOptionOUP",value=y)
              updateNumericInput(session,"rMCOptionOUP",value=r)
              updateNumericInput(session,"phiMCOptionOUP",value=phi)
              if(type < 0.5) { updateNumericInput(session,"pmaxMCOptionOUP",label="~",value=pmax) }
              else { updateNumericInput(session,"pmaxMCOptionOUP",label="p max",value=pmax) }
              updateNumericInput(session,"pathsMCOptionOUP",value=paths)
              updateNumericInput(session,"skipMCOptionOUP",value=skip)
            })
          }
          FromUItoR6 <- function()
          {
            # Get from UI ----
            isolate({
              rho <- input$rhoMCOptionOUP
              mu <- input$muMCOptionOUP
              sigma <- input$sigmaMCOptionOUP
              sFrom <- input$sFromMCOptionOUP
              sTo <- input$sToMCOptionOUP
              sBy <- input$sByMCOptionOUP
              xFrom <- input$xFromMCOptionOUP
              xTo <- input$xToMCOptionOUP
              xBy <- input$xByMCOptionOUP
              y <- input$yMCOptionOUP
              r <- input$rMCOptionOUP
              phi <- input$phiMCOptionOUP
              pmax <- input$pmaxMCOptionOUP
              paths <- input$pathsMCOptionOUP
              skip <- input$skipMCOptionOUP
            })
            if(!is.numeric(rho)) { rho <- 0 }
            else if(rho < 0) { rho <- 0 }
            if(!is.numeric(mu)) { mu <- 0 }
            if(!is.numeric(sigma)) { sigma <- 0 }
            s <- axissequence(sFrom,sTo,sBy)
            x <- axissequence(xFrom,xTo,xBy)
            if(!is.numeric(y)) { y <- 0 }
            if(!is.numeric(phi)) { phi <- -1 }
            else if(phi <= 0) { phi <- -1 }
            else { phi <- 1 }
            if(!is.numeric(pmax)) { pmax <- NaN }
            if(!is.numeric(paths)) { paths <- 100 }
            if(!is.numeric(skip)) { skip <- 1 }
            else if(skip > 5) { skip <- 5 }
            # Set to OUP ----
            MC$set_oup_params(rho=rho,mu=mu,sigma=sigma)
            MC$set_x_stoch_args(s=s,x=x,y=y,r=r,phi=phi)
            MC$set_path_args(paths=paths,skip=skip)
            MC$set_plot_args(pmax=pmax)
         }
          # user clicks clear or save ----
          observe({
            FromUItoR6()
            MC$undo_clear()
            showNotification("argument set 1 out of 1.",id="MCundo",duration=2)
          }) %>% bindEvent(input$clearMCOptionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            FromUItoR6()
            n <- MC$undo_save()
            showNotification(paste("argument set ",n," out of ",n,"."),id="MCundo",duration=2)
          }) %>% bindEvent(input$saveMCOptionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # user clicks undn, unup, sync, axes, plot (or enter key) or other ----
          output$plotlyMCOptionOUP <- renderPlotly({
            if(input$undnMCOptionOUP > MCbtns[9,2])
            {
              MCbtns[9,2] <<- input$undnMCOptionOUP
              Ixn <- MC$undo_undo()
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="MCundo",duration=2)
            }
            else if(input$unupMCOptionOUP > MCbtns[9,3])
            {
              MCbtns[9,3] <<- input$unupMCOptionOUP
              Ixn <- MC$undo_undo(1)
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="MCundo",duration=2)
            }
            else if(input$syncMCOptionOUP > MCbtns[9,4])
            {
              MCbtns[9,4] <<- input$syncMCOptionOUP
              FromUItoR6()
              MC$sync_yxt_stoch()
            }
            else if(input$axesMCOptionOUP > MCbtns[9,5])
            {
              MCbtns[9,5] <<- input$axesMCOptionOUP
              FromUItoR6()
              MC$axes_y_stoch()
            }
            else if(input$plotMCOptionOUP > MCbtns[9,6])
            {
              MCbtns[9,6] <<- input$plotMCOptionOUP
              FromUItoR6()
            }
            else if(input$otherMCOptionOUP > MCbtns[9,7])
            {
              MCbtns[9,7] <<- input$otherMCOptionOUP
              FromUItoR6()
              MC$set_plot_type("p",3)
            }
            FromR6toUI()
            MC$PlotOption()
          }) %>% bindEvent(input$undnMCOptionOUP,input$unupMCOptionOUP,input$syncMCOptionOUP,input$axesMCOptionOUP,input$plotMCOptionOUP,input$otherMCOptionOUP)
          # observe info ----
          observe({
            ibutton <<- ""
            infobutton <<- "infoMCOptionOUP"
            if(infotoggle()) { infotoggle(FALSE) }
            else { infotoggle(TRUE) }
          }) %>% bindEvent(input$infoMCOptionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            removeModal(session)
            updateTabsetPanel(session,"navBar",selected="tabAOUP")
            updateTabsetPanel(session,"navAOUP",selected="AOptionOUP")
          }) %>% bindEvent(input$alsoMCOptionOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
        # Visiting Time Mode Median Mean ----
        else if(input$navMCOUP == "MCVTModeMedianMeanOUP")
        {
          # define set/get functions ----
          FromR6toUI <- function()
          {
            # Get from OUP ----
            oup_params <- MC$get_oup_params()
            t_stoch_args <- MC$get_t_stoch_args()
            path_args <- MC$get_path_args()
            plot_args <- MC$get_plot_args()
            type <- MC$get_plot_types()[[1]][4]
            rho <- oup_params[[1]]
            mu <- oup_params[[2]]
            sigma <- oup_params[[3]]
            t <- t_stoch_args[[1]]
            k <- t_stoch_args[[2]]
            x <- t_stoch_args[[3]]
            paths <- path_args[[1]]
            skip <- path_args[[2]]
            ptmax <- plot_args[[2]]
            m <- length(t)
            tFrom <- t[1]
            tTo <- t[m]
            if(m > 1) { tBy <- (tTo-tFrom)/(m-1) }
            else  {tBy <- 0 }
            # Set to UI ----
            isolate({
              updateNumericInput(session,"rhoMCVTModeMedianMeanOUP",value=rho)
              updateNumericInput(session,"muMCVTModeMedianMeanOUP",value=mu)
              updateNumericInput(session,"sigmaMCVTModeMedianMeanOUP",value=sigma)
              updateNumericInput(session,"tFromMCVTModeMedianMeanOUP",value=tFrom)
              updateNumericInput(session,"tToMCVTModeMedianMeanOUP",value=tTo)
              updateNumericInput(session,"tByMCVTModeMedianMeanOUP",value=tBy)
              updateNumericInput(session,"xMCVTModeMedianMeanOUP",value=x)
              updateNumericInput(session,"kMCVTModeMedianMeanOUP",value=k)
              if(type < -0.5) { updateNumericInput(session,"ptmaxMCVTModeMedianMeanOUP",label="pv max",value=ptmax) }
              else { updateNumericInput(session,"ptmaxMCVTModeMedianMeanOUP",label="~",value=ptmax) }
              updateNumericInput(session,"pathsMCVTModeMedianMeanOUP",value=paths)
              updateNumericInput(session,"skipMCVTModeMedianMeanOUP",value=skip)
            })
          }
          FromUItoR6 <- function()
          {
            # Get from UI ----
            isolate({
              rho <- input$rhoMCVTModeMedianMeanOUP
              mu <- input$muMCVTModeMedianMeanOUP
              sigma <- input$sigmaMCVTModeMedianMeanOUP
              tFrom <- input$tFromMCVTModeMedianMeanOUP
              tTo <- input$tToMCVTModeMedianMeanOUP
              tBy <- input$tByMCVTModeMedianMeanOUP
              x <- input$xMCVTModeMedianMeanOUP
              k <- input$kMCVTModeMedianMeanOUP
              ptmax <- input$ptmaxMCVTModeMedianMeanOUP
              paths <- input$pathsMCVTModeMedianMeanOUP
              skip <- input$skipMCVTModeMedianMeanOUP
            })
            if(!is.numeric(rho)) { rho <- 0 }
            else if(rho < 0) { rho <- 0 }
            if(!is.numeric(mu)) { mu <- 0 }
            if(!is.numeric(sigma)) { sigma <- 0 }
            t <- axissequence(tFrom,tTo,tBy)
            if(!is.numeric(x)) { x <- 0 }
            if(!is.numeric(k)) { k <- 0 }
            if(!is.numeric(ptmax)) { ptmax <- NaN }
            if(!is.numeric(paths)) { paths <- 100 }
            if(!is.numeric(skip)) { skip <- 1 }
            else if(skip > 5) { skip <- 5 }
            # Set to OUP ----
            MC$set_oup_params(rho=rho,mu=mu,sigma=sigma)
            MC$set_t_stoch_args(t=t,k=k,x=x)
            MC$set_path_args(paths=paths,skip=skip)
            MC$set_plot_args(ptmax=ptmax)
         }
          # user clicks clear or save ----
          observe({
            FromUItoR6()
            MC$undo_clear()
            showNotification("argument set 1 out of 1.",id="MCundo",duration=2)
          }) %>% bindEvent(input$clearMCVTModeMedianMeanOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            FromUItoR6()
            n <- MC$undo_save()
            showNotification(paste("argument set ",n," out of ",n,"."),id="MCundo",duration=2)
          }) %>% bindEvent(input$saveMCVTModeMedianMeanOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # user clicks reset, undn, unup, sync, axes, plot (or enter key) or other ----
          output$plotlyMCVTModeMedianMeanOUP <- renderPlotly({
            if(input$undnMCVTModeMedianMeanOUP > MCbtns[10,2])
            {
              MCbtns[10,2] <<- input$undnMCVTModeMedianMeanOUP
              Ixn <- MC$undo_undo()
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="MCundo",duration=2)
            }
            else if(input$unupMCVTModeMedianMeanOUP > MCbtns[10,3])
            {
              MCbtns[10,3] <<- input$unupMCVTModeMedianMeanOUP
              Ixn <- MC$undo_undo(1)
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="MCundo",duration=2)
            }
            else if(input$syncMCVTModeMedianMeanOUP > MCbtns[10,4])
            {
              MCbtns[10,4] <<- input$syncMCVTModeMedianMeanOUP
              FromUItoR6()
              MC$sync_yxt_stoch()
            }
            else if(input$axesMCVTModeMedianMeanOUP > MCbtns[10,5])
            {
              MCbtns[10,5] <<- input$axesMCVTModeMedianMeanOUP
              FromUItoR6()
              MC$axes_t_stoch()
            }
            else if(input$plotMCVTModeMedianMeanOUP > MCbtns[10,6])
            {
              MCbtns[10,6] <<- input$plotMCVTModeMedianMeanOUP
              FromUItoR6()
            }
            else if(input$otherMCVTModeMedianMeanOUP > MCbtns[10,7])
            {
              MCbtns[10,7] <<- input$otherMCVTModeMedianMeanOUP
              FromUItoR6()
              MC$set_plot_type("p",4)
            }
            FromR6toUI()
            MC$PlotVisitingTimeModeMedianMean()
          }) %>% bindEvent(input$undnMCVTModeMedianMeanOUP,input$unupMCVTModeMedianMeanOUP,input$syncMCVTModeMedianMeanOUP,input$axesMCVTModeMedianMeanOUP,input$plotMCVTModeMedianMeanOUP,input$otherMCVTModeMedianMeanOUP)
          # observe info ----
          observe({
            ibutton <<- ""
            infobutton <<- "infoMCVTModeMedianMeanOUP"
            if(infotoggle()) { infotoggle(FALSE) }
            else { infotoggle(TRUE) }
          }) %>% bindEvent(input$infoMCVTModeMedianMeanOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            removeModal(session)
            updateTabsetPanel(session,"navBar",selected="tabAOUP")
            updateTabsetPanel(session,"navAOUP",selected="APTModeMedianMeanOUP")
          }) %>% bindEvent(input$alsoMCVTModeMedianMeanOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
        # Visiting Time Percentiles ----
        else if(input$navMCOUP == "MCVTPercentilesOUP")
        {
          # define set/get functions ----
          FromR6toUI <- function()
          {
            # Get from OUP ----
            oup_params <- MC$get_oup_params()
            t_stoch_args <- MC$get_t_stoch_args()
            path_args <- MC$get_path_args()
            plot_args <- MC$get_plot_args()
            type <- MC$get_plot_types()[[1]][4]
            rho <- oup_params[[1]]
            mu <- oup_params[[2]]
            sigma <- oup_params[[3]]
            t <- t_stoch_args[[1]]
            k <- t_stoch_args[[2]]
            x <- t_stoch_args[[3]]
            Ppct <- t_stoch_args[[5]]
            paths <- path_args[[1]]
            skip <- path_args[[2]]
            ptmax <- plot_args[[2]]
            m <- length(t)
            tFrom <- t[1]
            tTo <- t[m]
            if(m > 1) { tBy <- (tTo-tFrom)/(m-1) }
            else  {tBy <- 0 }
            # Set to UI ----
            isolate({
              updateNumericInput(session,"rhoMCVTPercentilesOUP",value=rho)
              updateNumericInput(session,"muMCVTPercentilesOUP",value=mu)
              updateNumericInput(session,"sigmaMCVTPercentilesOUP",value=sigma)
              updateNumericInput(session,"tFromMCVTPercentilesOUP",value=tFrom)
              updateNumericInput(session,"tToMCVTPercentilesOUP",value=tTo)
              updateNumericInput(session,"tByMCVTPercentilesOUP",value=tBy)
              updateNumericInput(session,"xMCVTPercentilesOUP",value=x)
              updateNumericInput(session,"kMCVTPercentilesOUP",value=k)
              updateNumericInput(session,"PpctMCVTPercentilesOUP",value=Ppct)
              if(type < -0.5) { updateNumericInput(session,"ptmaxMCVTPercentilesOUP",label="pv max",value=ptmax) }
              else { updateNumericInput(session,"ptmaxMCVTPercentilesOUP",label="~",value=ptmax) }
              updateNumericInput(session,"pathsMCVTPercentilesOUP",value=paths)
              updateNumericInput(session,"skipMCVTPercentilesOUP",value=skip)
            })
          }
          FromUItoR6 <- function()
          {
            # Get from UI ----
            isolate({
              rho <- input$rhoMCVTPercentilesOUP
              mu <- input$muMCVTPercentilesOUP
              sigma <- input$sigmaMCVTPercentilesOUP
              tFrom <- input$tFromMCVTPercentilesOUP
              tTo <- input$tToMCVTPercentilesOUP
              tBy <- input$tByMCVTPercentilesOUP
              x <- input$xMCVTPercentilesOUP
              k <- input$kMCVTPercentilesOUP
              Ppct <- input$PpctMCVTPercentilesOUP
              ptmax <- input$ptmaxMCVTPercentilesOUP
              paths <- input$pathsMCVTPercentilesOUP
              skip <- input$skipMCVTPercentilesOUP
            })
            if(!is.numeric(rho)) { rho <- 0 }
            else if(rho < 0) { rho <- 0 }
            if(!is.numeric(mu)) { mu <- 0 }
            if(!is.numeric(sigma)) { sigma <- 0 }
            t <- axissequence(tFrom,tTo,tBy)
            if(!is.numeric(x)) { x <- 0 }
            if(!is.numeric(k)) { k <- 0 }
            if(!is.numeric(Ppct)) { Ppct <- 0.75 }
            else if(Ppct < 0.01) { Ppct <- 0.01 }
            else if(Ppct > 0.99) { Ppct <- 0.99 }
            if(!is.numeric(ptmax)) { ptmax <- NaN }
            if(!is.numeric(paths)) { paths <- 100 }
            if(!is.numeric(skip)) { skip <- 1 }
            else if(skip > 5) { skip <- 5 }
            # Set to OUP ----
            MC$set_oup_params(rho=rho,mu=mu,sigma=sigma)
            MC$set_t_stoch_args(t=t,k=k,x=x,Ppct=Ppct)
            MC$set_path_args(paths=paths,skip=skip)
            MC$set_plot_args(ptmax=ptmax)
         }
          # user clicks clear or save ----
          observe({
            FromUItoR6()
            MC$undo_clear()
            showNotification("argument set 1 out of 1.",id="MCundo",duration=2)
          }) %>% bindEvent(input$clearMCVTPercentilesOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            FromUItoR6()
            n <- MC$undo_save()
            showNotification(paste("argument set ",n," out of ",n,"."),id="MCundo",duration=2)
          }) %>% bindEvent(input$saveMCVTPercentilesOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # user clicks reset, undn, unup, sync, axes, plot (or enter key) or other ----
          output$plotlyMCVTPercentilesOUP <- renderPlotly({
            if(input$undnMCVTPercentilesOUP > MCbtns[11,2])
            {
              MCbtns[11,2] <<- input$undnMCVTPercentilesOUP
              Ixn <- MC$undo_undo()
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="MCundo",duration=2)
            }
            else if(input$unupMCVTPercentilesOUP > MCbtns[11,3])
            {
              MCbtns[11,3] <<- input$unupMCVTPercentilesOUP
              Ixn <- MC$undo_undo(1)
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="MCundo",duration=2)
            }
            else if(input$syncMCVTPercentilesOUP > MCbtns[11,4])
            {
              MCbtns[11,4] <<- input$syncMCVTPercentilesOUP
              FromUItoR6()
              MC$sync_yxt_stoch()
            }
            else if(input$axesMCVTPercentilesOUP > MCbtns[11,5])
            {
              MCbtns[11,5] <<- input$axesMCVTPercentilesOUP
              FromUItoR6()
              MC$axes_t_stoch()
            }
            else if(input$plotMCVTPercentilesOUP > MCbtns[11,6])
            {
              MCbtns[11,6] <<- input$plotMCVTPercentilesOUP
              FromUItoR6()
            }
            else if(input$otherMCVTPercentilesOUP > MCbtns[11,7])
            {
              MCbtns[11,7] <<- input$otherMCVTPercentilesOUP
              FromUItoR6()
              MC$set_plot_type("p",4)
            }
            FromR6toUI()
            MC$PlotVisitingTimePercentiles()
          }) %>% bindEvent(input$undnMCVTPercentilesOUP,input$unupMCVTPercentilesOUP,input$syncMCVTPercentilesOUP,input$axesMCVTPercentilesOUP,input$plotMCVTPercentilesOUP,input$otherMCVTPercentilesOUP)
          # observe info ----
          observe({
            ibutton <<- ""
            infobutton <<- "infoMCVTPercentilesOUP"
            if(infotoggle()) { infotoggle(FALSE) }
            else { infotoggle(TRUE) }
          }) %>% bindEvent(input$infoMCVTPercentilesOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            removeModal(session)
            updateTabsetPanel(session,"navBar",selected="tabAOUP")
            updateTabsetPanel(session,"navAOUP",selected="APTPercentilesOUP")
          }) %>% bindEvent(input$alsoMCVTPercentilesOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
        # Visiting Time Density ----
        else if(input$navMCOUP == "MCVTDensityOUP")
        {
          # define set/get functions ----
          FromR6toUI <- function()
          {
            # Get from OUP ----
            oup_params <- MC$get_oup_params()
            t_stoch_args <- MC$get_t_stoch_args()
            path_args <- MC$get_path_args()
            plot_args <- MC$get_plot_args()
            type <- MC$get_plot_types()[[1]][5]
            rho <- oup_params[[1]]
            mu <- oup_params[[2]]
            sigma <- oup_params[[3]]
            t <- t_stoch_args[[1]]
            k <- t_stoch_args[[2]]
            x <- t_stoch_args[[3]]
            paths <- path_args[[1]]
            skip <- path_args[[2]]
            pmax <- plot_args[[1]]
            ptmax <- plot_args[[2]]
            zbeg <- plot_args[[5]]
            zend <- plot_args[[6]]
            m <- length(t)
            tFrom <- t[1]
            tTo <- t[m]
            if(m > 1) { tBy <- (tTo-tFrom)/(m-1) }
            else  {tBy <- 0 }
            # Set to UI ----
            isolate({
              updateNumericInput(session,"rhoMCVTDensityOUP",value=rho)
              updateNumericInput(session,"muMCVTDensityOUP",value=mu)
              updateNumericInput(session,"sigmaMCVTDensityOUP",value=sigma)
              updateNumericInput(session,"tFromMCVTDensityOUP",value=tFrom)
              updateNumericInput(session,"tToMCVTDensityOUP",value=tTo)
              updateNumericInput(session,"tByMCVTDensityOUP",value=tBy)
              updateNumericInput(session,"xMCVTDensityOUP",value=x)
              updateNumericInput(session,"kMCVTDensityOUP",value=k)
              if(type < 0.5)
              {
                updateNumericInput(session,"begMCVTDensityOUP",label="~",value=zbeg)
                updateNumericInput(session,"endMCVTDensityOUP",label="~",value=zend)
                updateNumericInput(session,"pmaxMCVTDensityOUP",label="~",value=pmax)
              }
              else
              {
                updateNumericInput(session,"begMCVTDensityOUP",label="begin",value=zbeg)
                updateNumericInput(session,"endMCVTDensityOUP",label="end",value=zend)
                updateNumericInput(session,"pmaxMCVTDensityOUP",label="p max",value=pmax)
              }
              updateNumericInput(session,"ptmaxMCVTDensityOUP",value=ptmax)
              updateNumericInput(session,"pathsMCVTDensityOUP",value=paths)
              updateNumericInput(session,"skipMCVTDensityOUP",value=skip)
            })
          }
          FromUItoR6 <- function()
          {
            # Get from UI ----
            isolate({
              rho <- input$rhoMCVTDensityOUP
              mu <- input$muMCVTDensityOUP
              sigma <- input$sigmaMCVTDensityOUP
              tFrom <- input$tFromMCVTDensityOUP
              tTo <- input$tToMCVTDensityOUP
              tBy <- input$tByMCVTDensityOUP
              x <- input$xMCVTDensityOUP
              k <- input$kMCVTDensityOUP
              zbeg <<- input$begMCVTDensityOUP
              zend <<- input$endMCVTDensityOUP
              pmax <- input$pmaxMCVTDensityOUP
              ptmax <- input$ptmaxMCVTDensityOUP
              paths <- input$pathsMCVTDensityOUP
              skip <- input$skipMCVTDensityOUP
            })
            if(!is.numeric(rho)) { rho <- 0 }
            else if(rho < 0) { rho <- 0 }
            if(!is.numeric(mu)) { mu <- 0 }
            if(!is.numeric(sigma)) { sigma <- 0 }
            t <- axissequence(tFrom,tTo,tBy)
            if(!is.numeric(x)) { x <- 0 }
            if(!is.numeric(k)) { k <- 0 }
            if(!is.numeric(zbeg)) { zbeg <- -Inf }
            if(!is.numeric(zend)) { zend <- Inf }
            if(!is.numeric(ptmax)) { ptmax <- NaN }
            if(!is.numeric(pmax)) { pmax <- NaN }
            if(!is.numeric(paths)) { paths <- 100 }
            if(!is.numeric(skip)) { skip <- 1 }
            else if(skip > 5) { skip <- 5 }
            # Set to OUP ----
            MC$set_oup_params(rho=rho,mu=mu,sigma=sigma)
            MC$set_t_stoch_args(t=t,k=k,x=x)
            MC$set_path_args(paths=paths,skip=skip)
            MC$set_plot_args(pmax=pmax,ptmax=ptmax,zbeg=zbeg,zend=zend)
         }
          # user clicks clear or save ----
          observe({
            FromUItoR6()
            MC$undo_clear()
            showNotification("argument set 1 out of 1.",id="MCundo",duration=2)
          }) %>% bindEvent(input$clearMCVTDensityOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            FromUItoR6()
            n <- MC$undo_save()
            showNotification(paste("argument set ",n," out of ",n,"."),id="MCundo",duration=2)
          }) %>% bindEvent(input$saveMCVTDensityOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # user clicks reset, undn, unup, sync, axes, plot (or enter key) or other ----
          output$plotlyMCVTDensityOUP <- renderPlotly({
            if(input$resetMCVTDensityOUP > MCbtns[12,1])
            {
              MCbtns[12,1] <<- input$resetMCVTDensityOUP
              FromUItoR6()
              MC$set_plot_args(zbeg=-Inf,zend=Inf)
            }
            if(input$undnMCVTDensityOUP > MCbtns[12,2])
            {
              MCbtns[12,2] <<- input$undnMCVTDensityOUP
              Ixn <- MC$undo_undo()
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="MCundo",duration=2)
            }
            else if(input$unupMCVTDensityOUP > MCbtns[12,3])
            {
              MCbtns[12,3] <<- input$unupMCVTDensityOUP
              Ixn <- MC$undo_undo(1)
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="MCundo",duration=2)
            }
            else if(input$syncMCVTDensityOUP > MCbtns[12,4])
            {
              MCbtns[12,4] <<- input$syncMCVTDensityOUP
              FromUItoR6()
              MC$sync_yxt_stoch()
            }
            else if(input$axesMCVTDensityOUP > MCbtns[12,5])
            {
              MCbtns[12,5] <<- input$axesMCVTDensityOUP
              FromUItoR6()
              MC$axes_t_stoch()
            }
            else if(input$plotMCVTDensityOUP > MCbtns[12,6])
            {
              MCbtns[12,6] <<- input$plotMCVTDensityOUP
              FromUItoR6()
            }
            else if(input$otherMCVTDensityOUP > MCbtns[12,7])
            {
              MCbtns[12,7] <<- input$otherMCVTDensityOUP
              FromUItoR6()
              MC$set_plot_type("p",5)
            }
            FromR6toUI()
            MC$PlotVisitingTimeDensity()
          }) %>% bindEvent(input$resetMCVTDensityOUP,input$undnMCVTDensityOUP,input$unupMCVTDensityOUP,input$syncMCVTDensityOUP,input$axesMCVTDensityOUP,input$plotMCVTDensityOUP,input$otherMCVTDensityOUP)
          # observe info ----
          observe({
            ibutton <<- ""
            infobutton <<- "infoMCVTDensityOUP"
            if(infotoggle()) { infotoggle(FALSE) }
            else { infotoggle(TRUE) }
          }) %>% bindEvent(input$infoMCVTDensityOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            removeModal(session)
            updateTabsetPanel(session,"navBar",selected="tabAOUP")
            updateTabsetPanel(session,"navAOUP",selected="APTDensityOUP")
          }) %>% bindEvent(input$alsoMCVTDensityOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
        # Visiting Time Probability ----
        else if(input$navMCOUP == "MCVTProbabilityOUP")
        {
          # define set/get functions ----
          FromR6toUI <- function()
          {
            # Get from OUP ----
            oup_params <- MC$get_oup_params()
            t_stoch_args <- MC$get_t_stoch_args()
            path_args <- MC$get_path_args()
            plot_args <- MC$get_plot_args()
            type <- MC$get_plot_types()[[1]][5]
            rho <- oup_params[[1]]
            mu <- oup_params[[2]]
            sigma <- oup_params[[3]]
            t <- t_stoch_args[[1]]
            k <- t_stoch_args[[2]]
            x <- t_stoch_args[[3]]
            paths <- path_args[[1]]
            skip <- path_args[[2]]
            pmax <- plot_args[[1]]
            zbeg <- plot_args[[5]]
            zend <- plot_args[[6]]
            m <- length(t)
            tFrom <- t[1]
            tTo <- t[m]
            if(m > 1) { tBy <- (tTo-tFrom)/(m-1) }
            else  {tBy <- 0 }
            # Set to UI ----
            isolate({
              updateNumericInput(session,"rhoMCVTProbabilityOUP",value=rho)
              updateNumericInput(session,"muMCVTProbabilityOUP",value=mu)
              updateNumericInput(session,"sigmaMCVTProbabilityOUP",value=sigma)
              updateNumericInput(session,"tFromMCVTProbabilityOUP",value=tFrom)
              updateNumericInput(session,"tToMCVTProbabilityOUP",value=tTo)
              updateNumericInput(session,"tByMCVTProbabilityOUP",value=tBy)
              updateNumericInput(session,"xMCVTProbabilityOUP",value=x)
              updateNumericInput(session,"kMCVTProbabilityOUP",value=k)
              if(type < 0.5)
              {
                updateNumericInput(session,"begMCVTProbabilityOUP",label="~",value=zbeg)
                updateNumericInput(session,"endMCVTProbabilityOUP",label="~",value=zend)
                updateNumericInput(session,"pmaxMCVTProbabilityOUP",label="~",value=pmax)
              }
              else
              {
                updateNumericInput(session,"begMCVTProbabilityOUP",label="begin",value=zbeg)
                updateNumericInput(session,"endMCVTProbabilityOUP",label="end",value=zend)
                updateNumericInput(session,"pmaxMCVTProbabilityOUP",label="p max",value=pmax)
              }
              updateNumericInput(session,"pathsMCVTProbabilityOUP",value=paths)
              updateNumericInput(session,"skipMCVTProbabilityOUP",value=skip)
            })
          }
          FromUItoR6 <- function()
          {
            # Get from UI ----
            isolate({
              rho <- input$rhoMCVTProbabilityOUP
              mu <- input$muMCVTProbabilityOUP
              sigma <- input$sigmaMCVTProbabilityOUP
              tFrom <- input$tFromMCVTProbabilityOUP
              tTo <- input$tToMCVTProbabilityOUP
              tBy <- input$tByMCVTProbabilityOUP
              x <- input$xMCVTProbabilityOUP
              k <- input$kMCVTProbabilityOUP
              zbeg <<- input$begMCVTProbabilityOUP
              zend <<- input$endMCVTProbabilityOUP
              pmax <- input$pmaxMCVTProbabilityOUP
              paths <- input$pathsMCVTProbabilityOUP
              skip <- input$skipMCVTProbabilityOUP
            })
            if(!is.numeric(rho)) { rho <- 0 }
            else if(rho < 0) { rho <- 0 }
            if(!is.numeric(mu)) { mu <- 0 }
            if(!is.numeric(sigma)) { sigma <- 0 }
            t <- axissequence(tFrom,tTo,tBy)
            if(!is.numeric(x)) { x <- 0 }
            if(!is.numeric(k)) { k <- 0 }
            if(!is.numeric(zbeg)) { zbeg <- -Inf }
            if(!is.numeric(zend)) { zend <- Inf }
            if(!is.numeric(pmax)) { pmax <- NaN }
            if(!is.numeric(paths)) { paths <- 100 }
            if(!is.numeric(skip)) { skip <- 1 }
            else if(skip > 5) { skip <- 5 }
            # Set to OUP ----
            MC$set_oup_params(rho=rho,mu=mu,sigma=sigma)
            MC$set_t_stoch_args(t=t,k=k,x=x)
            MC$set_path_args(paths=paths,skip=skip)
            MC$set_plot_args(pmax=pmax,zbeg=zbeg,zend=zend)
         }
          # user clicks clear or save ----
          observe({
            FromUItoR6()
            MC$undo_clear()
            showNotification("argument set 1 out of 1.",id="MCundo",duration=2)
          }) %>% bindEvent(input$clearMCVTProbabilityOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            FromUItoR6()
            n <- MC$undo_save()
            showNotification(paste("argument set ",n," out of ",n,"."),id="MCundo",duration=2)
          }) %>% bindEvent(input$saveMCVTProbabilityOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # user clicks reset, undn, unup, sync, axes, plot (or enter key) or other ----
          output$plotlyMCVTProbabilityOUP <- renderPlotly({
            if(input$resetMCVTProbabilityOUP > MCbtns[13,1])
            {
              MCbtns[13,1] <<- input$resetMCVTProbabilityOUP
              FromUItoR6()
              MC$set_plot_args(zbeg=-Inf,zend=Inf)
            }
            if(input$undnMCVTProbabilityOUP > MCbtns[13,2])
            {
              MCbtns[13,2] <<- input$undnMCVTProbabilityOUP
              Ixn <- MC$undo_undo()
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="MCundo",duration=2)
            }
            else if(input$unupMCVTProbabilityOUP > MCbtns[13,3])
            {
              MCbtns[13,3] <<- input$unupMCVTProbabilityOUP
              Ixn <- MC$undo_undo(1)
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="MCundo",duration=2)
            }
            else if(input$syncMCVTProbabilityOUP > MCbtns[13,4])
            {
              MCbtns[13,4] <<- input$syncMCVTProbabilityOUP
              FromUItoR6()
              MC$sync_yxt_stoch()
            }
            else if(input$axesMCVTProbabilityOUP > MCbtns[13,5])
            {
              MCbtns[13,5] <<- input$axesMCVTProbabilityOUP
              FromUItoR6()
              MC$axes_t_stoch()
            }
            else if(input$plotMCVTProbabilityOUP > MCbtns[13,6])
            {
              MCbtns[13,6] <<- input$plotMCVTProbabilityOUP
              FromUItoR6()
            }
            else if(input$otherMCVTProbabilityOUP > MCbtns[13,7])
            {
              MCbtns[13,7] <<- input$otherMCVTProbabilityOUP
              FromUItoR6()
              MC$set_plot_type("p",5)
            }
            FromR6toUI()
            MC$PlotVisitingTimeProbability()
          }) %>% bindEvent(input$resetMCVTProbabilityOUP,input$undnMCVTProbabilityOUP,input$unupMCVTProbabilityOUP,input$syncMCVTProbabilityOUP,input$axesMCVTProbabilityOUP,input$plotMCVTProbabilityOUP,input$otherMCVTProbabilityOUP)
          # observe info ----
          observe({
            ibutton <<- ""
            infobutton <<- "infoMCVTProbabilityOUP"
            if(infotoggle()) { infotoggle(FALSE) }
            else { infotoggle(TRUE) }
          }) %>% bindEvent(input$infoMCVTProbabilityOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            removeModal(session)
            updateTabsetPanel(session,"navBar",selected="tabAOUP")
            updateTabsetPanel(session,"navAOUP",selected="APTProbabilityOUP")
          }) %>% bindEvent(input$alsoMCVTProbabilityOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
        # First Passage Time Mode Median Mean ----
        else if(input$navMCOUP == "MCFPTModeMedianMeanOUP")
        {
          # define set/get functions ----
          FromR6toUI <- function()
          {
            # Get from OUP ----
            oup_params <- MC$get_oup_params()
            t_stoch_args <- MC$get_t_stoch_args()
            path_args <- MC$get_path_args()
            plot_args <- MC$get_plot_args()
            type <- MC$get_plot_types()[[1]][4]
            rho <- oup_params[[1]]
            mu <- oup_params[[2]]
            sigma <- oup_params[[3]]
            t <- t_stoch_args[[1]]
            k <- t_stoch_args[[2]]
            x <- t_stoch_args[[3]]
            paths <- path_args[[1]]
            skip <- path_args[[2]]
            ptmax <- plot_args[[2]]
            m <- length(t)
            tFrom <- t[1]
            tTo <- t[m]
            if(m > 1) { tBy <- (tTo-tFrom)/(m-1) }
            else  {tBy <- 0 }
            # Set to UI ----
            isolate({
              updateNumericInput(session,"rhoMCFPTModeMedianMeanOUP",value=rho)
              updateNumericInput(session,"muMCFPTModeMedianMeanOUP",value=mu)
              updateNumericInput(session,"sigmaMCFPTModeMedianMeanOUP",value=sigma)
              updateNumericInput(session,"tFromMCFPTModeMedianMeanOUP",value=tFrom)
              updateNumericInput(session,"tToMCFPTModeMedianMeanOUP",value=tTo)
              updateNumericInput(session,"tByMCFPTModeMedianMeanOUP",value=tBy)
              updateNumericInput(session,"xMCFPTModeMedianMeanOUP",value=x)
              updateNumericInput(session,"kMCFPTModeMedianMeanOUP",value=k)
              if(type < -0.5) { updateNumericInput(session,"ptmaxMCFPTModeMedianMeanOUP",label="pf max",value=ptmax) }
              else { updateNumericInput(session,"ptmaxMCFPTModeMedianMeanOUP",label="~",value=ptmax) }
              updateNumericInput(session,"pathsMCFPTModeMedianMeanOUP",value=paths)
              updateNumericInput(session,"skipMCFPTModeMedianMeanOUP",value=skip)
            })
          }
          FromUItoR6 <- function()
          {
            # Get from UI ----
            isolate({
              rho <- input$rhoMCFPTModeMedianMeanOUP
              mu <- input$muMCFPTModeMedianMeanOUP
              sigma <- input$sigmaMCFPTModeMedianMeanOUP
              tFrom <- input$tFromMCFPTModeMedianMeanOUP
              tTo <- input$tToMCFPTModeMedianMeanOUP
              tBy <- input$tByMCFPTModeMedianMeanOUP
              x <- input$xMCFPTModeMedianMeanOUP
              k <- input$kMCFPTModeMedianMeanOUP
              ptmax <- input$ptmaxMCFPTModeMedianMeanOUP
              paths <- input$pathsMCFPTModeMedianMeanOUP
              skip <- input$skipMCFPTModeMedianMeanOUP
            })
            if(!is.numeric(rho)) { rho <- 0 }
            else if(rho < 0) { rho <- 0 }
            if(!is.numeric(mu)) { mu <- 0 }
            if(!is.numeric(sigma)) { sigma <- 0 }
            t <- axissequence(tFrom,tTo,tBy)
            if(!is.numeric(x)) { x <- 0 }
            if(!is.numeric(k)) { k <- 0 }
            if(!is.numeric(ptmax)) { ptmax <- NaN }
            if(!is.numeric(paths)) { paths <- 100 }
            if(!is.numeric(skip)) { skip <- 1 }
            else if(skip > 5) { skip <- 5 }
            # Set to OUP ----
            MC$set_oup_params(rho=rho,mu=mu,sigma=sigma)
            MC$set_t_stoch_args(t=t,k=k,x=x)
            MC$set_path_args(paths=paths,skip=skip)
            MC$set_plot_args(ptmax=ptmax)
         }
          # user clicks clear or save ----
          observe({
            FromUItoR6()
            MC$undo_clear()
            showNotification("argument set 1 out of 1.",id="MCundo",duration=2)
          }) %>% bindEvent(input$clearMCFPTModeMedianMeanOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            FromUItoR6()
            n <- MC$undo_save()
            showNotification(paste("argument set ",n," out of ",n,"."),id="MCundo",duration=2)
          }) %>% bindEvent(input$saveMCFPTModeMedianMeanOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # user clicks reset, undn, unup, sync, axes, plot (or enter key) or other ----
          output$plotlyMCFPTModeMedianMeanOUP <- renderPlotly({
            if(input$undnMCFPTModeMedianMeanOUP > MCbtns[14,2])
            {
              MCbtns[14,2] <<- input$undnMCFPTModeMedianMeanOUP
              Ixn <- MC$undo_undo()
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="MCundo",duration=2)
            }
            else if(input$unupMCFPTModeMedianMeanOUP > MCbtns[14,3])
            {
              MCbtns[14,3] <<- input$unupMCFPTModeMedianMeanOUP
              Ixn <- MC$undo_undo(1)
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="MCundo",duration=2)
            }
            else if(input$syncMCFPTModeMedianMeanOUP > MCbtns[14,4])
            {
              MCbtns[14,4] <<- input$syncMCFPTModeMedianMeanOUP
              FromUItoR6()
              MC$sync_yxt_stoch()
            }
            else if(input$axesMCFPTModeMedianMeanOUP > MCbtns[14,5])
            {
              MCbtns[14,5] <<- input$axesMCFPTModeMedianMeanOUP
              FromUItoR6()
              MC$axes_t_stoch()
            }
            else if(input$plotMCFPTModeMedianMeanOUP > MCbtns[14,6])
            {
              MCbtns[14,6] <<- input$plotMCFPTModeMedianMeanOUP
              FromUItoR6()
            }
            else if(input$otherMCFPTModeMedianMeanOUP > MCbtns[14,7])
            {
              MCbtns[14,7] <<- input$otherMCFPTModeMedianMeanOUP
              FromUItoR6()
              MC$set_plot_type("p",4)
            }
            FromR6toUI()
            MC$PlotFirstPassageTimeModeMedianMean()
          }) %>% bindEvent(input$undnMCFPTModeMedianMeanOUP,input$unupMCFPTModeMedianMeanOUP,input$syncMCFPTModeMedianMeanOUP,input$axesMCFPTModeMedianMeanOUP,input$plotMCFPTModeMedianMeanOUP,input$otherMCFPTModeMedianMeanOUP)
          # observe info ----
          observe({
            ibutton <<- ""
            infobutton <<- "infoMCFPTModeMedianMeanOUP"
            if(infotoggle()) { infotoggle(FALSE) }
            else { infotoggle(TRUE) }
          }) %>% bindEvent(input$infoMCFPTModeMedianMeanOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            removeModal(session)
            updateTabsetPanel(session,"navBar",selected="tabAOUP")
            updateTabsetPanel(session,"navAOUP",selected="APTModeMedianMeanOUP")
          }) %>% bindEvent(input$alsoMCFPTModeMedianMeanOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
        # First Passage Time Percentiles ----
        else if(input$navMCOUP == "MCFPTPercentilesOUP")
        {
          # define set/get functions ----
          FromR6toUI <- function()
          {
            # Get from OUP ----
            oup_params <- MC$get_oup_params()
            t_stoch_args <- MC$get_t_stoch_args()
            path_args <- MC$get_path_args()
            plot_args <- MC$get_plot_args()
            type <- MC$get_plot_types()[[1]][4]
            rho <- oup_params[[1]]
            mu <- oup_params[[2]]
            sigma <- oup_params[[3]]
            t <- t_stoch_args[[1]]
            k <- t_stoch_args[[2]]
            x <- t_stoch_args[[3]]
            Ppct <- t_stoch_args[[5]]
            paths <- path_args[[1]]
            skip <- path_args[[2]]
            ptmax <- plot_args[[2]]
            m <- length(t)
            tFrom <- t[1]
            tTo <- t[m]
            if(m > 1) { tBy <- (tTo-tFrom)/(m-1) }
            else  {tBy <- 0 }
            # Set to UI ----
            isolate({
              updateNumericInput(session,"rhoMCFPTPercentilesOUP",value=rho)
              updateNumericInput(session,"muMCFPTPercentilesOUP",value=mu)
              updateNumericInput(session,"sigmaMCFPTPercentilesOUP",value=sigma)
              updateNumericInput(session,"tFromMCFPTPercentilesOUP",value=tFrom)
              updateNumericInput(session,"tToMCFPTPercentilesOUP",value=tTo)
              updateNumericInput(session,"tByMCFPTPercentilesOUP",value=tBy)
              updateNumericInput(session,"xMCFPTPercentilesOUP",value=x)
              updateNumericInput(session,"kMCFPTPercentilesOUP",value=k)
              updateNumericInput(session,"PpctMCFPTPercentilesOUP",value=Ppct)
              if(type < -0.5) { updateNumericInput(session,"ptmaxMCFPTPercentilesOUP",label="pf max",value=ptmax) }
              else { updateNumericInput(session,"ptmaxMCFPTPercentilesOUP",label="~",value=ptmax) }
              updateNumericInput(session,"pathsMCFPTPercentilesOUP",value=paths)
              updateNumericInput(session,"skipMCFPTPercentilesOUP",value=skip)
            })
          }
          FromUItoR6 <- function()
          {
            # Get from UI ----
            isolate({
              rho <- input$rhoMCFPTPercentilesOUP
              mu <- input$muMCFPTPercentilesOUP
              sigma <- input$sigmaMCFPTPercentilesOUP
              tFrom <- input$tFromMCFPTPercentilesOUP
              tTo <- input$tToMCFPTPercentilesOUP
              tBy <- input$tByMCFPTPercentilesOUP
              x <- input$xMCFPTPercentilesOUP
              k <- input$kMCFPTPercentilesOUP
              Ppct <- input$PpctMCFPTPercentilesOUP
              ptmax <- input$ptmaxMCFPTPercentilesOUP
              paths <- input$pathsMCFPTPercentilesOUP
              skip <- input$skipMCFPTPercentilesOUP
            })
            if(!is.numeric(rho)) { rho <- 0 }
            else if(rho < 0) { rho <- 0 }
            if(!is.numeric(mu)) { mu <- 0 }
            if(!is.numeric(sigma)) { sigma <- 0 }
            t <- axissequence(tFrom,tTo,tBy)
            if(!is.numeric(x)) { x <- 0 }
            if(!is.numeric(k)) { k <- 0 }
            if(!is.numeric(Ppct)) { Ppct <- 0.75 }
            else if(Ppct < 0.01) { Ppct <- 0.01 }
            else if(Ppct > 0.99) { Ppct <- 0.99 }
            if(!is.numeric(ptmax)) { ptmax <- NaN }
            if(!is.numeric(paths)) { paths <- 100 }
            if(!is.numeric(skip)) { skip <- 1 }
            else if(skip > 5) { skip <- 5 }
            # Set to OUP ----
            MC$set_oup_params(rho=rho,mu=mu,sigma=sigma)
            MC$set_t_stoch_args(t=t,k=k,x=x,Ppct=Ppct)
            MC$set_path_args(paths=paths,skip=skip)
            MC$set_plot_args(ptmax=ptmax)
         }
          # user clicks clear or save ----
          observe({
            FromUItoR6()
            MC$undo_clear()
            showNotification("argument set 1 out of 1.",id="MCundo",duration=2)
          }) %>% bindEvent(input$clearMCFPTPercentilesOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            FromUItoR6()
            n <- MC$undo_save()
            showNotification(paste("argument set ",n," out of ",n,"."),id="MCundo",duration=2)
          }) %>% bindEvent(input$saveMCFPTPercentilesOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # user clicks reset, undn, unup, sync, axes, plot (or enter key) or other ----
          output$plotlyMCFPTPercentilesOUP <- renderPlotly({
            if(input$undnMCFPTPercentilesOUP > MCbtns[15,2])
            {
              MCbtns[15,2] <<- input$undnMCFPTPercentilesOUP
              Ixn <- MC$undo_undo()
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="MCundo",duration=2)
            }
            else if(input$unupMCFPTPercentilesOUP > MCbtns[15,3])
            {
              MCbtns[15,3] <<- input$unupMCFPTPercentilesOUP
              Ixn <- MC$undo_undo(1)
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="MCundo",duration=2)
            }
            else if(input$syncMCFPTPercentilesOUP > MCbtns[15,4])
            {
              MCbtns[15,4] <<- input$syncMCFPTPercentilesOUP
              FromUItoR6()
              MC$sync_yxt_stoch()
            }
            else if(input$axesMCFPTPercentilesOUP > MCbtns[15,5])
            {
              MCbtns[15,5] <<- input$axesMCFPTPercentilesOUP
              FromUItoR6()
              MC$axes_t_stoch()
            }
            else if(input$plotMCFPTPercentilesOUP > MCbtns[15,6])
            {
              MCbtns[15,6] <<- input$plotMCFPTPercentilesOUP
              FromUItoR6()
            }
            else if(input$otherMCFPTPercentilesOUP > MCbtns[15,7])
            {
              MCbtns[15,7] <<- input$otherMCFPTPercentilesOUP
              FromUItoR6()
              MC$set_plot_type("p",4)
            }
            FromR6toUI()
            MC$PlotFirstPassageTimePercentiles()
          }) %>% bindEvent(input$undnMCFPTPercentilesOUP,input$unupMCFPTPercentilesOUP,input$syncMCFPTPercentilesOUP,input$axesMCFPTPercentilesOUP,input$plotMCFPTPercentilesOUP,input$otherMCFPTPercentilesOUP)
          # observe info ----
          observe({
            ibutton <<- ""
            infobutton <<- "infoMCFPTPercentilesOUP"
            if(infotoggle()) { infotoggle(FALSE) }
            else { infotoggle(TRUE) }
          }) %>% bindEvent(input$infoMCFPTPercentilesOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            removeModal(session)
            updateTabsetPanel(session,"navBar",selected="tabAOUP")
            updateTabsetPanel(session,"navAOUP",selected="APTPercentilesOUP")
          }) %>% bindEvent(input$alsoMCFPTPercentilesOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
        # First Passage Time Density ----
        else if(input$navMCOUP == "MCFPTDensityOUP")
        {
          # define set/get functions ----
          FromR6toUI <- function()
          {
            # Get from OUP ----
            oup_params <- MC$get_oup_params()
            t_stoch_args <- MC$get_t_stoch_args()
            path_args <- MC$get_path_args()
            plot_args <- MC$get_plot_args()
            type <- MC$get_plot_types()[[1]][5]
            rho <- oup_params[[1]]
            mu <- oup_params[[2]]
            sigma <- oup_params[[3]]
            t <- t_stoch_args[[1]]
            k <- t_stoch_args[[2]]
            x <- t_stoch_args[[3]]
            paths <- path_args[[1]]
            skip <- path_args[[2]]
            pmax <- plot_args[[1]]
            ptmax <- plot_args[[2]]
            zbeg <- plot_args[[5]]
            zend <- plot_args[[6]]
            m <- length(t)
            tFrom <- t[1]
            tTo <- t[m]
            if(m > 1) { tBy <- (tTo-tFrom)/(m-1) }
            else  {tBy <- 0 }
            # Set to UI ----
            isolate({
              updateNumericInput(session,"rhoMCFPTDensityOUP",value=rho)
              updateNumericInput(session,"muMCFPTDensityOUP",value=mu)
              updateNumericInput(session,"sigmaMCFPTDensityOUP",value=sigma)
              updateNumericInput(session,"tFromMCFPTDensityOUP",value=tFrom)
              updateNumericInput(session,"tToMCFPTDensityOUP",value=tTo)
              updateNumericInput(session,"tByMCFPTDensityOUP",value=tBy)
              updateNumericInput(session,"xMCFPTDensityOUP",value=x)
              updateNumericInput(session,"kMCFPTDensityOUP",value=k)
              if(type < 0.5)
              {
                updateNumericInput(session,"begMCFPTDensityOUP",label="~",value=zbeg)
                updateNumericInput(session,"endMCFPTDensityOUP",label="~",value=zend)
                updateNumericInput(session,"pmaxMCFPTDensityOUP",label="~",value=pmax)
              }
              else
              {
                updateNumericInput(session,"begMCFPTDensityOUP",label="begin",value=zbeg)
                updateNumericInput(session,"endMCFPTDensityOUP",label="end",value=zend)
                updateNumericInput(session,"pmaxMCFPTDensityOUP",label="p max",value=pmax)
              }
              updateNumericInput(session,"ptmaxMCFPTDensityOUP",value=ptmax)
              updateNumericInput(session,"pathsMCFPTDensityOUP",value=paths)
              updateNumericInput(session,"skipMCFPTDensityOUP",value=skip)
            })
          }
          FromUItoR6 <- function()
          {
            # Get from UI ----
            isolate({
              rho <- input$rhoMCFPTDensityOUP
              mu <- input$muMCFPTDensityOUP
              sigma <- input$sigmaMCFPTDensityOUP
              tFrom <- input$tFromMCFPTDensityOUP
              tTo <- input$tToMCFPTDensityOUP
              tBy <- input$tByMCFPTDensityOUP
              x <- input$xMCFPTDensityOUP
              k <- input$kMCFPTDensityOUP
              zbeg <<- input$begMCFPTDensityOUP
              zend <<- input$endMCFPTDensityOUP
              pmax <- input$pmaxMCFPTDensityOUP
              ptmax <- input$ptmaxMCFPTDensityOUP
              paths <- input$pathsMCFPTDensityOUP
              skip <- input$skipMCFPTDensityOUP
            })
            if(!is.numeric(rho)) { rho <- 0 }
            else if(rho < 0) { rho <- 0 }
            if(!is.numeric(mu)) { mu <- 0 }
            if(!is.numeric(sigma)) { sigma <- 0 }
            t <- axissequence(tFrom,tTo,tBy)
            if(!is.numeric(x)) { x <- 0 }
            if(!is.numeric(k)) { k <- 0 }
            if(!is.numeric(zbeg)) { zbeg <- -Inf }
            if(!is.numeric(zend)) { zend <- Inf }
            if(!is.numeric(ptmax)) { ptmax <- NaN }
            if(!is.numeric(pmax)) { pmax <- NaN }
            if(!is.numeric(paths)) { paths <- 100 }
            if(!is.numeric(skip)) { skip <- 1 }
            else if(skip > 5) { skip <- 5 }
            # Set to OUP ----
            MC$set_oup_params(rho=rho,mu=mu,sigma=sigma)
            MC$set_t_stoch_args(t=t,k=k,x=x)
            MC$set_path_args(paths=paths,skip=skip)
            MC$set_plot_args(pmax=pmax,ptmax=ptmax,zbeg=zbeg,zend=zend)
         }
          # user clicks clear or save ----
          observe({
            FromUItoR6()
            MC$undo_clear()
            showNotification("argument set 1 out of 1.",id="MCundo",duration=2)
          }) %>% bindEvent(input$clearMCFPTDensityOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            FromUItoR6()
            n <- MC$undo_save()
            showNotification(paste("argument set ",n," out of ",n,"."),id="MCundo",duration=2)
          }) %>% bindEvent(input$saveMCFPTDensityOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # user clicks reset, undn, unup, sync, axes, plot (or enter key) or other ----
          output$plotlyMCFPTDensityOUP <- renderPlotly({
            if(input$resetMCFPTDensityOUP > MCbtns[16,1])
            {
              MCbtns[16,1] <<- input$resetMCFPTDensityOUP
              FromUItoR6()
              MC$set_plot_args(zbeg=-Inf,zend=Inf)
            }
            if(input$undnMCFPTDensityOUP > MCbtns[16,2])
            {
              MCbtns[16,2] <<- input$undnMCFPTDensityOUP
              Ixn <- MC$undo_undo()
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="MCundo",duration=2)
            }
            else if(input$unupMCFPTDensityOUP > MCbtns[16,3])
            {
              MCbtns[16,3] <<- input$unupMCFPTDensityOUP
              Ixn <- MC$undo_undo(1)
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="MCundo",duration=2)
            }
            else if(input$syncMCFPTDensityOUP > MCbtns[16,4])
            {
              MCbtns[16,4] <<- input$syncMCFPTDensityOUP
              FromUItoR6()
              MC$sync_yxt_stoch()
            }
            else if(input$axesMCFPTDensityOUP > MCbtns[16,5])
            {
              MCbtns[16,5] <<- input$axesMCFPTDensityOUP
              FromUItoR6()
              MC$axes_t_stoch()
            }
            else if(input$plotMCFPTDensityOUP > MCbtns[16,6])
            {
              MCbtns[16,6] <<- input$plotMCFPTDensityOUP
              FromUItoR6()
            }
            else if(input$otherMCFPTDensityOUP > MCbtns[16,7])
            {
              MCbtns[16,7] <<- input$otherMCFPTDensityOUP
              FromUItoR6()
              MC$set_plot_type("p",5)
            }
            FromR6toUI()
            MC$PlotFirstPassageTimeDensity()
          }) %>% bindEvent(input$resetMCFPTDensityOUP,input$undnMCFPTDensityOUP,input$unupMCFPTDensityOUP,input$syncMCFPTDensityOUP,input$axesMCFPTDensityOUP,input$plotMCFPTDensityOUP,input$otherMCFPTDensityOUP)
          # observe info ----
          observe({
            ibutton <<- ""
            infobutton <<- "infoMCFPTDensityOUP"
            if(infotoggle()) { infotoggle(FALSE) }
            else { infotoggle(TRUE) }
          }) %>% bindEvent(input$infoMCFPTDensityOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            removeModal(session)
            updateTabsetPanel(session,"navBar",selected="tabAOUP")
            updateTabsetPanel(session,"navAOUP",selected="APTDensityOUP")
          }) %>% bindEvent(input$alsoMCFPTDensityOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
        # First Passage Time Probability ----
        else if(input$navMCOUP == "MCFPTProbabilityOUP")
        {
          # define set/get functions ----
          FromR6toUI <- function()
          {
            # Get from OUP ----
            oup_params <- MC$get_oup_params()
            t_stoch_args <- MC$get_t_stoch_args()
            path_args <- MC$get_path_args()
            plot_args <- MC$get_plot_args()
            type <- MC$get_plot_types()[[1]][5]
            rho <- oup_params[[1]]
            mu <- oup_params[[2]]
            sigma <- oup_params[[3]]
            t <- t_stoch_args[[1]]
            k <- t_stoch_args[[2]]
            x <- t_stoch_args[[3]]
            paths <- path_args[[1]]
            skip <- path_args[[2]]
            pmax <- plot_args[[1]]
            zbeg <- plot_args[[5]]
            zend <- plot_args[[6]]
            m <- length(t)
            tFrom <- t[1]
            tTo <- t[m]
            if(m > 1) { tBy <- (tTo-tFrom)/(m-1) }
            else  {tBy <- 0 }
            # Set to UI ----
            isolate({
              updateNumericInput(session,"rhoMCFPTProbabilityOUP",value=rho)
              updateNumericInput(session,"muMCFPTProbabilityOUP",value=mu)
              updateNumericInput(session,"sigmaMCFPTProbabilityOUP",value=sigma)
              updateNumericInput(session,"tFromMCFPTProbabilityOUP",value=tFrom)
              updateNumericInput(session,"tToMCFPTProbabilityOUP",value=tTo)
              updateNumericInput(session,"tByMCFPTProbabilityOUP",value=tBy)
              updateNumericInput(session,"xMCFPTProbabilityOUP",value=x)
              updateNumericInput(session,"kMCFPTProbabilityOUP",value=k)
              if(type < 0.5)
              {
                updateNumericInput(session,"begMCFPTProbabilityOUP",label="~",value=zbeg)
                updateNumericInput(session,"endMCFPTProbabilityOUP",label="~",value=zend)
                updateNumericInput(session,"pmaxMCFPTProbabilityOUP",label="~",value=pmax)
              }
              else
              {
                updateNumericInput(session,"begMCFPTProbabilityOUP",label="begin",value=zbeg)
                updateNumericInput(session,"endMCFPTProbabilityOUP",label="end",value=zend)
                updateNumericInput(session,"pmaxMCFPTProbabilityOUP",label="p max",value=pmax)
              }
              updateNumericInput(session,"pathsMCFPTProbabilityOUP",value=paths)
              updateNumericInput(session,"skipMCFPTProbabilityOUP",value=skip)
            })
          }
          FromUItoR6 <- function()
          {
            # Get from UI ----
            isolate({
              rho <- input$rhoMCFPTProbabilityOUP
              mu <- input$muMCFPTProbabilityOUP
              sigma <- input$sigmaMCFPTProbabilityOUP
              tFrom <- input$tFromMCFPTProbabilityOUP
              tTo <- input$tToMCFPTProbabilityOUP
              tBy <- input$tByMCFPTProbabilityOUP
              x <- input$xMCFPTProbabilityOUP
              k <- input$kMCFPTProbabilityOUP
              zbeg <<- input$begMCFPTProbabilityOUP
              zend <<- input$endMCFPTProbabilityOUP
              pmax <- input$pmaxMCFPTProbabilityOUP
              paths <- input$pathsMCFPTProbabilityOUP
              skip <- input$skipMCFPTProbabilityOUP
            })
            if(!is.numeric(rho)) { rho <- 0 }
            else if(rho < 0) { rho <- 0 }
            if(!is.numeric(mu)) { mu <- 0 }
            if(!is.numeric(sigma)) { sigma <- 0 }
            t <- axissequence(tFrom,tTo,tBy)
            if(!is.numeric(x)) { x <- 0 }
            if(!is.numeric(k)) { k <- 0 }
            if(!is.numeric(zbeg)) { zbeg <- -Inf }
            if(!is.numeric(zend)) { zend <- Inf }
            if(!is.numeric(pmax)) { pmax <- NaN }
            if(!is.numeric(paths)) { paths <- 100 }
            if(!is.numeric(skip)) { skip <- 1 }
            else if(skip > 5) { skip <- 5 }
            # Set to OUP ----
            MC$set_oup_params(rho=rho,mu=mu,sigma=sigma)
            MC$set_t_stoch_args(t=t,k=k,x=x)
            MC$set_path_args(paths=paths,skip=skip)
            MC$set_plot_args(pmax=pmax,zbeg=zbeg,zend=zend)
         }
          # user clicks clear or save ----
          observe({
            FromUItoR6()
            MC$undo_clear()
            showNotification("argument set 1 out of 1.",id="MCundo",duration=2)
          }) %>% bindEvent(input$clearMCFPTProbabilityOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            FromUItoR6()
            n <- MC$undo_save()
            showNotification(paste("argument set ",n," out of ",n,"."),id="MCundo",duration=2)
          }) %>% bindEvent(input$saveMCFPTProbabilityOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          # user clicks reset, undn, unup, sync, axes, plot (or enter key) or other ----
          output$plotlyMCFPTProbabilityOUP <- renderPlotly({
            if(input$resetMCFPTProbabilityOUP > MCbtns[17,1])
            {
              MCbtns[17,1] <<- input$resetMCFPTProbabilityOUP
              FromUItoR6()
              MC$set_plot_args(zbeg=-Inf,zend=Inf)
            }
            if(input$undnMCFPTProbabilityOUP > MCbtns[17,2])
            {
              MCbtns[17,2] <<- input$undnMCFPTProbabilityOUP
              Ixn <- MC$undo_undo()
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="MCundo",duration=2)
            }
            else if(input$unupMCFPTProbabilityOUP > MCbtns[17,3])
            {
              MCbtns[17,3] <<- input$unupMCFPTProbabilityOUP
              Ixn <- MC$undo_undo(1)
              showNotification(paste("argument set ",Ixn[1]," out of ",Ixn[2],"."),id="MCundo",duration=2)
            }
            else if(input$syncMCFPTProbabilityOUP > MCbtns[17,4])
            {
              MCbtns[17,4] <<- input$syncMCFPTProbabilityOUP
              FromUItoR6()
              MC$sync_yxt_stoch()
            }
            else if(input$axesMCFPTProbabilityOUP > MCbtns[17,5])
            {
              MCbtns[17,5] <<- input$axesMCFPTProbabilityOUP
              FromUItoR6()
              MC$axes_t_stoch()
            }
            else if(input$plotMCFPTProbabilityOUP > MCbtns[17,6])
            {
              MCbtns[17,6] <<- input$plotMCFPTProbabilityOUP
              FromUItoR6()
            }
            else if(input$otherMCFPTProbabilityOUP > MCbtns[17,7])
            {
              MCbtns[17,7] <<- input$otherMCFPTProbabilityOUP
              FromUItoR6()
              MC$set_plot_type("p",5)
            }
            FromR6toUI()
            MC$PlotFirstPassageTimeProbability()
          }) %>% bindEvent(input$resetMCFPTProbabilityOUP,input$undnMCFPTProbabilityOUP,input$unupMCFPTProbabilityOUP,input$syncMCFPTProbabilityOUP,input$axesMCFPTProbabilityOUP,input$plotMCFPTProbabilityOUP,input$otherMCFPTProbabilityOUP)
          # observe info ----
          observe({
            ibutton <<- ""
            infobutton <<- "infoMCFPTProbabilityOUP"
            if(infotoggle()) { infotoggle(FALSE) }
            else { infotoggle(TRUE) }
          }) %>% bindEvent(input$infoMCFPTProbabilityOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
          observe({
            removeModal(session)
            updateTabsetPanel(session,"navBar",selected="tabAOUP")
            updateTabsetPanel(session,"navAOUP",selected="APTProbabilityOUP")
          }) %>% bindEvent(input$alsoMCFPTProbabilityOUP,ignoreNULL=TRUE,ignoreInit=TRUE)
        }
      })
    }
    else if(input$navBar == "tabOnTheMenuOUP")
    {
      ibutton <<- ""
      infobutton <<- ""
      if(infotoggle()) { infotoggle(FALSE) }
      else { infotoggle(TRUE) }
    }
    else if(input$navBar == "tabAboutOUP")
    {
      ibutton <<- ""
      infobutton <<- "tabAboutOUP"
      if(infotoggle()) { infotoggle(FALSE) }
      else { infotoggle(TRUE) }
    }
    else if(input$navBar == "tabLicenseOUP")
    {
      ibutton <<- ""
      infobutton <<- "tabLicenseOUP"
      if(infotoggle()) { infotoggle(FALSE) }
      else { infotoggle(TRUE) }
    }
    # end ----
  })
  # dark mode switch ----
  observeEvent(input$darkmodeswitch, {
    if (input$darkmodeswitch == "light") { A$set_plot_info(theme="light") }
    else { A$set_plot_info(theme="dark") }
  })
  # modal dialog ----
  observe({
    tabName <- ""
    # splash screen ----
      bodyText <- "Greetings!<br><br>
          The Ornstein-Uhlenbeck menu:<br>
          &emsp;&emsp;Real Options:  A selection of tabs for quick results;<br>
          &emsp;&emsp;Analytical:  Formulas for most problems;<br>
          &emsp;&emsp;Finite Difference:  Method for trickier problems;<br>
          &emsp;&emsp;Maximum Likelihood: Estimation and hypothesis testing;<br>
          &emsp;&emsp;Monte Carlo: Simulations to explain the formulas.<br><br>
          The Help menu:<br>
          &emsp;&emsp;Tutorials:  Case Studies for solved problems and Methods for solving problems;<br>
          &emsp;&emsp;Reference:  Explanations in more detail than you probably want;<br>
          &emsp;&emsp;About:  Who we are, how to contact us and how to cite us;<br>
          &emsp;&emsp;License:  Your rights and our rights."
    seeAlso <- ""
    # file info ----
    if(ibutton != "")
    {
      htmlname <- paste(sep="",htmlpath,ibutton,".html")
      if(!file.exists(htmlname)) { htmlname <- paste(sep="",htmlpath,"MyData.html") }
      if(file.exists(htmlname)) {
        rawtext <- rvest::read_html(htmlname)
        elms <- rvest::html_children(rvest::html_element(rawtext,xpath="//main"))
        thistext <- ""
        for(elm in elms) {
          elmn <- rvest::html_name(elm)
          if(elmn == "h2" | elmn == "h3" | elmn == "p" | elmn == "ul") {thistext <- paste0(thistext,elm) }
        }
      }
      else { thistext <- paste(sep="",ibutton,".html was not found.")}
      tabName <- ibutton
      h2h3 <- "<style>h2 { font-size: 120% } h3 { font-size: 110% }</style>"
      bodyText <- (paste(sep="",h2h3,thistext))
    }
    # ROData ----
    else if(infobutton == "infoRODataOUP")
    {
      tabName <- "Data"
      bodyText <- "The rate, location and scale parameters of the Ornstein-Uhlenbeck Process can be plucked out of the air, cogitated by experts, deduced from theory or estimated using data.<br><br>
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
          How the time intervals are measured affects the estimation of parameters <i>rho</i> and <i>sigma</i>.  For example, if measurements are taken once per year and time is reported in years, time interval <i>t-s</i> will be 1 year for a typical observation.  Parameter <i>rho</i> will likely range from 0.1 to 4.0 and <i>sigma</i> will range from 10 to 50.  If measurements are daily but time is reported in years, time interval <i>t-s</i> will be 1/365 years.  Parameter <i>rho</i> will be about 365 times larger and parameter <i>sigma</i> will be about (2<i>rho</i>)<sup>0.5</sup> times larger."
      seeAlso <- "alsoRODataOUP"
    }
    # ROEstimates ----
    else if(infobutton == "infoROEstimatesOUP")
    {
      tabName <- "Estimates"
      bodyText <- "If you know the parameters of the Ornstein-Uhlenbeck Process, you can enter them directly.  If you have data, you can use it to estimate the parameters.  Maximum Likelihood Estimation finds the rate, location and scale parameters of the Ornstein-Uhlenbeck Process which maximize the Log Likelihood of observing the data as a random sample.<br><br>
          &emsp;&emsp;Arguments:<br>
          &emsp;&emsp;&emsp;<i>tau</i> are times;<br>
          &emsp;&emsp;&emsp;<i>z</i> are states.<br>
          &emsp;&emsp;Returns:<br>
          &emsp;&emsp;&emsp;<i>rho</i> is the rate parameter;<br>
          &emsp;&emsp;&emsp;<i>mu</i> is the location parameter;<br>
          &emsp;&emsp;&emsp;<i>sigma</i> is the scale parameter;<br>
          &emsp;&emsp;&emsp;<i>alpha</i> identifies the distribution of the Log Likelihood."
      seeAlso <- "alsoROEstimatesOUP"
    }
    # RORegime ----
    else if(infobutton == "infoRORegimeOUP")
    {
      tabName <- "Regime"
      bodyText <- "A Regime is a benefit/cost analysis with options.  The benefit/cost analysis is called an Obligation.  It is how benefits are gained and costs are lost.  An Obligation is linear in the state of nature and, hence, certain.  The options are Exit and Entry Options.   Options value the flexibility to exit from and enter into an Obligation.  Exit and Entry Options are highly convex and, hence, uncertain.<br><br>
          An Entry Option without an Exit Option is an Obligation.  Exercising an Exit Option eliminates the Obligation.<br><br>
          Conversely, an Exit Option without an Entry Option is a Prohibition.  A Prohibition is a negative Obligation.  Exercising an Entry Option eliminates the Prohibition.<br><br>
	        Entry and Exit Options are perpetual options.  There is no fixed expiry date.  If the value of flexibility exceeds the benefits to be gained or the costs being lost, decision-makers will keep their options open.  Otherwise, they will exercise one of their options.<br><br>
          &emsp;&emsp;Arguments:<br>
          &emsp;&emsp;&emsp;<i>rho</i> is the rate parameter;<br>
          &emsp;&emsp;&emsp;<i>mu</i> is the location parameter;<br>
          &emsp;&emsp;&emsp;<i>sigma</i> is the scale parameter;<br>
          &emsp;&emsp;&emsp;<i>y</i> is the break-even point;<br>
          &emsp;&emsp;&emsp;<i>r</i> is the discount rate;<br>
          &emsp;&emsp;&emsp;<i>phi</i> is < 0 for an Exit Option, > 0 for an Entry Option;<br>
          &emsp;&emsp;&emsp;<i>b</i> is a benefit or subsidy for an Entry Option;<br>
          &emsp;&emsp;&emsp;<i>c</i> is a cost or tax for an Exit Option."
      seeAlso <- "alsoRORegimeOUP"
    }
    # RODecision ----
    else if(infobutton == "infoRODecisionOUP")
    {
      tabName <- "Decision Threshold"
      bodyText <- "The Decision Threshold is the state of the system where a decision-maker will be indifferent between holding or exercising an Entry or Exit Option.  The Option value at the threshold is the price of flexibility&mdash;the price of keeping options open.  It is the most a decision-maker will pay in costs rather than exit prematurely, or the most a decision-maker will forego in benefits rather than enter prematurely.<br><br>
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
          &emsp;&emsp;&emsp;\u00D4 is the Option at the Decision Threshold."
      seeAlso <- "alsoRODecisionOUP"
    }
    # ROPassageTime ----
    else if(infobutton == "infoROPassageTimeOUP")
    {
      tabName <- "Passage Times"
      bodyText <- "A Passage Time is the time until a system crosses a threshold.  The longer the passage time, the more resilient the system.  Passage Times will be longer if the state of the system is far from the threshold, is moving slowly and is less stochastic.<br><br>
          The probabilities of the Ornstein-Uhlenbeck Process are symmetric.  The measure of central tendency is the Mean and the measure of dispersion is the Variance.  The probabilities of Passage Times are not symmetric.  There are three measures of central tendency, the Mode, Median and Mean.  However, the Mean and Variance may not exist.<br><br>
          Percentiles are a reliable alternative. The Median is the Passage Time with a 50% chance the threshold has been crossed and a 50% chance it is yet to be crossed.  Higher and lower Percentiles have similar interpretations.<br><br>
          If crossing a threshold is irreversible, Passage Times are First Passage Times.  If crossing a threshold is completely reversible, Passage Times are Visiting Times.  If crossing a threshold may be partially reversible, Passage Times are in between First Passage Times and Visiting Times.<br><br>
          &emsp;&emsp;Arguments:<br>
          &emsp;&emsp;&emsp;<i>k</i> is the threshold;<br>
          &emsp;&emsp;&emsp;<i>x</i> is the fixed initial state;<br>
          &emsp;&emsp;&emsp;<i>z</i> are alternate initial states;<br>
          &emsp;&emsp;&emsp;<i>omega</i> is the degree of irreversibility;<br>
          &emsp;&emsp;&emsp;<i>rho</i> is the rate parameter;<br>
          &emsp;&emsp;&emsp;<i>mu</i> is the location parameter;<br>
          &emsp;&emsp;&emsp;<i>sigma</i> is the scale parameter;<br>
          &emsp;&emsp;&emsp;<i>Ppct</i> is a passage time probability;<br>
          &emsp;&emsp;&emsp;<i>s</i> is the fixed initial time.<br>
          &emsp;&emsp;Returns:<br>
          &emsp;&emsp;&emsp;<i>t</i><sub>0.5</sub> is the Median Passage Time;<br>
          &emsp;&emsp;&emsp;<i>t<sub>Ppct</sub></i> and <i>t</i><sub>1-<i>Ppct</i></sub> are Passage Time Percentiles for <i>Ppct</i> and 1-<i>Ppct</i>."
      seeAlso <- "alsoROPassageTimeOUP"
    }
    # ADrift ----
    else if(infobutton == "infoADriftOUP")
    {
      tabName <- "Drift"
      bodyText <- "Drift is the expected change in the state of a stochastic process over a brief instant.  It is also called the Instantaneous Mean.  For the Ornstein-Uhlenbeck Process, it depends upon the current state <i>z</i>.<br><br>
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
          &emsp;&emsp;&emsp;<i>g</i> is the Drift."
      seeAlso <- "alsoADriftOUP"
    }
    # ADiffusion ----
    else if(infobutton == "infoADiffusionOUP")
    {
      tabName <- "Diffusion"
      bodyText <- "An error is the difference between the actual and expected changes in the state of a stochastic process.  Diffusion is the error squared over a brief instant.  It is also called the Instantaneous Variance.  For the Ornstein-Uhlenbeck Process, it is constant.<br><br>
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
          &emsp;&emsp;&emsp;<i>h</i><sup>2</sup> is the Diffusion."
      seeAlso <- "alsoADiffusionOUP"
    }
    # AMean ----
    else if(infobutton == "infoAMeanOUP")
    {
      tabName <- "Mean"
      bodyText <- "A Mean of a stochastic process is the expected state <i>y</i> at time <i>t</i> in the future.  For all stochastic processes, including the Ornstein-Uhlenbeck Process, it depends upon the initial time <i>s</i> and the initial state <i>x</i>.<br><br>
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
          &emsp;&emsp;&emsp;<i>G</i> is the Mean."
      seeAlso <- "alsoAMeanOUP"
    }
    # AVariance ----
    else if(infobutton == "infoAVarianceOUP")
    {
      tabName <- "Variance"
      bodyText <- "An error is the difference between the actual and expected state of a stochastic process for time <i>t</i> in the future.  A Variance is the error squared.  For the Ornstein-Uhlenbeck Process, it depends upon the initial time <i>s</i>.<br><br>
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
          &emsp;&emsp;&emsp;<i>H</i>&hairsp;<sup>2</sup> is the Variance."
      seeAlso <- "alsoAVarianceOUP"
    }
    # ADensity ----
    else if(infobutton == "infoADensityOUP")
    {
      tabName <- "Transition Density"
      bodyText <- "The Transition Density is the probability of state <i>y</i> being observed at time <i>t</i>.  At initial time <i>t</i> equal to <i>s</i>, the probability of <i>y</i> equal to <i>x</i> is one and the probability of <i>y</i> not equal to <i>x</i> is zero.  The Transition Density is the Dirac or Degenerate Density.  As time passes, the probability of <i>y</i> equal to <i>x</i> decreases, the probability of <i>y</i> not equal to <i>x</i> increases and the Transition Density widens and moves away from <i>x</i>.  In the limit as <i>t</i> goes to infinity, the Transition Density loses its dependence on <i>s</i> and <i>x</i> and converges to its Invariant Density, with Asymptotic Mean <i>mu</i> and Asymptotic Variance <i>sigma</i><sup>2</sup>/2<i>rho</i>.<br><br>
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
          &emsp;&emsp;&emsp;<i>p</i> is the Transition Density."
      seeAlso <- "alsoADensityOUP"
    }
    # AProbability ----
    else if(infobutton == "infoAProbabilityOUP")
    {
      tabName <- "Transition Probability"
      bodyText <- "The Transition Probability integrates the Transition Density.  It sums the probabilities of observing states less than or equal to <i>y</i> at time <i>t</i>.  Alternatively, it sums the probabilities greater than or equal to <i>y</i>.  At initial time <i>t</i> equal to <i>s</i>, it sums the Dirac Density to become the Heavyside or Step Function, which steps from zero to one at <i>y</i> equal to the initial state <i>x</i>.  As time passes, the Transition Probability widens and moves away from <i>x</i>.  For the Ornstein-Uhlenbeck Process, as <i>t</i> goes to infinity, the Transition Probability converges to its Invariant Probability, with Asymptotic Mean <i>mu</i> and Asymptotic Variance <i>sigma</i><sup>2</sup>/2<i>rho</i>.<br><br>
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
          &emsp;&emsp;&emsp;<i>P</i> is the Transition Probability."
      seeAlso <- "alsoAProbabilityOUP"
    }
    # ADouble ----
    else if(infobutton == "infoADoubleOUP")
    {
      tabName <- "Double Integral"
      bodyText <- "The Double Integral sums the probabilities one more time.  The effect is easiest to see at initial time <i>t</i> equal to <i>s</i>, when the Transition Density is the Dirac Density and the Transition Probability is the Heavyside Function.  Integrating the Dirac Density gives the Heavyside Function and integrating the Heavyside Function gives the Threshold Function.  The Threshold Function is kinked, like a payoff function for an option, and the Double Integral is the precursor to an analytical option pricing formula.  Thresholds are a property of stochastic processes, including the Ornstein-Uhlenbeck Process, and in a world of uncertainty over time, Options are not optional.<br><br>
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
          &emsp;&emsp;&emsp;&Popf; is the Double Integral."
      seeAlso <- "alsoADoubleOUP"
    }
    # AOption ----
    else if(infobutton == "infoAOptionOUP")
    {
      tabName <- "Option"
      bodyText <- "Probabilities are an initial-value problem with fixed initial time and state.  Options are a terminal-value problem with fixed terminal time and state.  A Double Integral becomes an Option by reinterpreting time <i>s</i> and state <i>x</i> as variable and time <i>t</i> and state <i>y</i> as fixed.  Multiplying by a discount factor gives the value of an Option discounted to time <i>s</i>.  The Ornstein-Uhlenbeck Process has a Double Integral and, hence, an analytical Option pricing formula.<br><br>
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
          &emsp;&emsp;&emsp;&Oopf; is an Option."
      seeAlso <- "alsoAOptionOUP"
    }
    # AEnvelope ----
    else if(infobutton == "infoAEnvelopeOUP")
    {
      tabName <- "Option Envelope"
      bodyText <- "A Financial Option is a contract between a buyer and a seller with a fixed expiry date.  A Real Option is not a contract.  There is neither buyer nor seller.  There is no fixed expiry date.  It is a Perpetual Option that a decision-maker can exercise whenever they choose.  If the maximum value of the Option is the payoff function, it should be exercised immediately.  If the maximum value of the Option is greater than the payoff function, it should be held and possibly exercised in the future.  The Option Envelope is the maximum value of either exercising or holding the option for all states, <i>x</i>.<br><br>
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
          &emsp;&emsp;&emsp;\u00D4 is an Option on the Envelope."
      seeAlso <- "alsoAEnvelopeOUP"
    }
    # ADecision ----
    else if(infobutton == "infoADecisionOUP")
    {
      tabName <- "Decision Threshold"
      bodyText <- "The Decision Threshold is the state <i>k</i> where a decision-maker will be indifferent between holding or exercising a Real Option.  The Option value at the threshold is the price of flexibility&mdash;the price of keeping options open.  It is the most a decision-maker will pay in costs rather than exit prematurely, or the most a decision-maker will forego in benefits rather than enter prematurely.<br><br>
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
          &emsp;&emsp;&emsp;\u00D4 is the Option at the Decision Threshold."
      seeAlso <- "alsoADecisionOUP"
    }
    # AObligation ----
    else if(infobutton == "infoAObligationOUP")
    {
      tabName <- "Obligation"
      bodyText <- "In finance, the call/put parity transforms options from one to the other.  In Real Options, the intermediate formula in the transformation is called the Obligation&mdash;the obligation to take losses.  An Obligation equals the Entry Option minus the Exit Option.  Another name for an Obligation is a Benefit/Cost Analysis.  A negative Obligation is a Prohibition&mdash;the prohibition from taking gains.  A Prohibition equals the Exit Option minus the Entry Option.  Neither an Obligation nor a Prohibition is uncertain.  All uncertainty is in the options.<br><br>
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
          &emsp;&emsp;&emsp;<strong>\u2102</strong> is a Prohibition with positive costs and negative benefits."
    }
    # APTModeMedianMean ----
    else if(infobutton == "infoAPTModeMedianMeanOUP")
    {
      tabName <- "Passage Time Mode, Median and Mean"
      bodyText <- "If crossing a threshold is irreversible, the Mode is the most likely time to cross, the Median is the time with a 50% chance the threshold has already been crossed and the Mean is the expected time to cross.<br><br>
          If crossing is partially or completely reversible, net visits are crossings to the far side minus returns to the near side.  The Mode is when net visits are greatest.  The Median is when net visits reach 50% of the long-term proportion of time spent on the far side.  The Mean is the expected time of net visits to the far side.<br><br>
          If the Ornstein-Uhlenbeck Process is attracted across a threshold, the Mode is less than the Median is less than the Mean.  If, however, the process is attracted to a location away from the threshold, the Mean can be less than the Median can be less than the Mode.  If the process is not attracted at all, with a rate of convergence of zero, the Mean does not exist and the expected time to cross a threshold is unknown.<br><br>
          &emsp;&emsp;The R6 method:<br>
          &emsp;&emsp;&emsp;PassageTimeModeMedianMean(<i>k,s,x,z,omega,rho,mu,sigma</i>)<br>
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
              <td style='border-top: solid silver; border-left: solid silver'>&nbsp;</td>
              <td style='padding: 0px 4px 0px 4px;'><i>t</i><sub>mode</sub>(<i>x</i>)</td>
              <td style='padding: 0px 4px 0px 4px;'><i>pt</i><sub>mode</sub>(<i>x</i>)</td>
              <td style='padding: 0px 4px 0px 4px;'><i>Pt</i><sub>mode</sub>(<i>x</i>)</td>
              <td style='border-top: solid silver; border-right: solid silver'>&nbsp;</td>
            </tr>
            <tr>
              <td style='border-left: solid silver'>&nbsp;</td>
              <td style='padding: 0px 4px 0px 4px;'><i>t</i><sub>median</sub>(<i>x</i>)</td>
              <td style='padding: 0px 4px 0px 4px;'><i>pt</i><sub>median</sub>(<i>x</i>)</td>
              <td style='padding: 0px 4px 0px 4px;'><i>Pt</i><sub>median</sub>(<i>x</i>)</td>
              <td style='border-right: solid silver'>&nbsp;</td>
            </tr>
            <tr>
              <td style='border-bottom: solid silver; border-left: solid silver'>&nbsp;</td>
              <td style='padding: 0px 4px 0px 4px;'><i>t</i><sub>mean</sub>(<i>x</i>)</td>
              <td style='padding: 0px 4px 0px 4px;'><i>pt</i><sub>mean</sub>(<i>x</i>)</td>
              <td style='padding: 0px 4px 0px 4px;'><i>Pt</i><sub>mean</sub>(<i>x</i>)</td>
              <td style='border-bottom: solid silver; border-right: solid silver'>&nbsp;</td>
            </tr>
          </table>
          <table style='float: left; margin-left: 10px; margin-right: 10px;'>
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
              <td style='padding: 0px 4px 0px 4px;'><i>t</i><sub>mode</sub>(<i>z<sub>1</sub></i>)</td>
              <td style='padding: 0px 4px 0px 4px;'>&hellip;</td>
              <td style='padding: 0px 4px 0px 4px;'><i>t</i><sub>mode</sub>(<i>z<sub>n</sub></i>)</td>
              <td style='border-top: solid silver; border-right: solid silver'>&nbsp;</td>
            </tr>
            <tr>
              <td style='border-left: solid silver'>&nbsp;</td>
              <td style='padding: 0px 4px 0px 4px;'><i>t</i><sub>median</sub>(<i>z<sub>1</sub></i>)</td>
              <td style='padding: 0px 4px 0px 4px;'>&hellip;</td>
              <td style='padding: 0px 4px 0px 4px;'><i>t</i><sub>median</sub>(<i>z<sub>n</sub></i>)</td>
              <td style='border-right: solid silver'>&nbsp;</td>
            </tr>
            <tr>
              <td style='border-bottom: solid silver; border-left: solid silver'>&nbsp;</td>
              <td style='padding: 0px 4px 0px 4px;'><i>t</i><sub>mean</sub>(<i>z<sub>1</sub></i>)</td>
              <td style='padding: 0px 4px 0px 4px;'>&hellip;</td>
              <td style='padding: 0px 4px 0px 4px;'><i>t</i><sub>mean</sub>(<i>z<sub>n</sub></i>)</td>
              <td style='border-bottom: solid silver; border-right: solid silver'>&nbsp;</td>
              <td style='padding-left: 2px;'>;</td>
            </tr>
          </table>
          &emsp;&emsp;where:<br>
          &emsp;&emsp;&emsp;<i>t</i><sub>mode</sub>(<i>x</i>), <i>t</i><sub>median</sub>(<i>x</i>) and <i>t</i><sub>mean</sub>(<i>x</i>) are the Passage Time Mode, Median and Mean at <i>x</i>;<br>
          &emsp;&emsp;&emsp;<i>t</i><sub>mode</sub>(<i>z</i><sub>j</sub>), <i>t</i><sub>median</sub>(<i>z</i><sub>j</sub>) and <i>t</i><sub>mean</sub>(<i>z</i><sub>j</sub>) are the Passage Time Mode, Median and Mean for <i>x=z</i><sub>j</sub>;<br>
          &emsp;&emsp;&emsp;<i>pt</i> are Passage Time Densities;<br>
          &emsp;&emsp;&emsp;<i>Pt</i> are Passage Time Probabilities."
      seeAlso <- "alsoAPTModeMedianMeanOUP"
    }
    # APTPercentiles ----
    else if(infobutton == "infoAPTPercentilesOUP")
    {
      tabName <- "Passage Time Percentiles"
      bodyText <- "The Transition Densities and Probabilites for the Ornstein-Uhlenbeck Process are symmetric and easy to interpret.  The only measure of central tendency is the Mean and the only measure of dispersion is the Variance.  Adding and subtracting the square-root of the Variance gives Percentiles above and below the Mean.<br><br>
          Passage Time Densities and Probabilities are not symmetric.  There are three measures of central tendency, the Mode, Median and Mean.  Adding and subtracting the square-root of the Variance gives weird results.  If a stochastic process does not converge, its Passage Time Mean and Variance do not exist.<br><br>
          An easier alternative is to calculate Percentiles.  The Median is the time with a 50% chance the threshold has been crossed and a 50% chance it is yet to be crossed.  Higher and lower Percentiles have similar interpretations.<br><br>
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
              <td style='border-top: solid silver; border-left: solid silver'>&nbsp;</td>
              <td style='padding: 0px 4px 0px 4px;'><i>t</i><sub>1-Ppct</sub>(<i>x</i>)</td>
              <td style='padding: 0px 4px 0px 4px;'><i>pt</i><sub>1-Ppct</sub>(<i>x</i>)</td>
              <td style='padding: 0px 4px 0px 4px;'><i>Pt</i><sub>1-Ppct</sub>(<i>x</i>)</td>
              <td style='border-top: solid silver; border-right: solid silver'>&nbsp;</td>
            </tr>
            <tr>
              <td style='border-left: solid silver'>&nbsp;</td>
              <td style='padding: 0px 4px 0px 4px;'><i>t</i><sub>0.5</sub>(<i>x</i>)</td>
              <td style='padding: 0px 4px 0px 4px;'><i>pt</i><sub>0.5</sub>(<i>x</i>)</td>
              <td style='padding: 0px 4px 0px 4px;'><i>Pt</i><sub>0.5</sub>(<i>x</i>)</td>
              <td style='border-right: solid silver'>&nbsp;</td>
            </tr>
            <tr>
              <td style='border-bottom: solid silver; border-left: solid silver'>&nbsp;</td>
              <td style='padding: 0px 4px 0px 4px;'><i>t</i><sub>Ppct</sub>(<i>x</i>)</td>
              <td style='padding: 0px 4px 0px 4px;'><i>pt</i><sub>Ppct</sub>(<i>x</i>)</td>
              <td style='padding: 0px 4px 0px 4px;'><i>Pt</i><sub>Ppct</sub>(<i>x</i>)</td>
              <td style='border-bottom: solid silver; border-right: solid silver'>&nbsp;</td>
            </tr>
          </table>
          <table style='float: left; margin-left: 10px; margin-right: 10px;'>
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
              <td style='padding: 0px 4px 0px 4px;'><i>t</i><sub>1-Ppct</sub>(<i>z<sub>1</sub></i>)</td>
              <td style='padding: 0px 4px 0px 4px;'>&hellip;</td>
              <td style='padding: 0px 4px 0px 4px;'><i>t</i><sub>1-Ppct</sub>(<i>z<sub>n</sub></i>)</td>
              <td style='border-top: solid silver; border-right: solid silver'>&nbsp;</td>
            </tr>
            <tr>
              <td style='border-left: solid silver'>&nbsp;</td>
              <td style='padding: 0px 4px 0px 4px;'><i>t</i><sub>0.5</sub>(<i>z<sub>1</sub></i>)</td>
              <td style='padding: 0px 4px 0px 4px;'>&hellip;</td>
              <td style='padding: 0px 4px 0px 4px;'><i>t</i><sub>0.5</sub>(<i>z<sub>n</sub></i>)</td>
              <td style='border-right: solid silver'>&nbsp;</td>
            </tr>
            <tr>
              <td style='border-bottom: solid silver; border-left: solid silver'>&nbsp;</td>
              <td style='padding: 0px 4px 0px 4px;'><i>t</i><sub>Ppct</sub>(<i>z<sub>1</sub></i>)</td>
              <td style='padding: 0px 4px 0px 4px;'>&hellip;</td>
              <td style='padding: 0px 4px 0px 4px;'><i>t</i><sub>Ppct</sub>(<i>z<sub>n</sub></i>)</td>
              <td style='border-bottom: solid silver; border-right: solid silver'>&nbsp;</td>
              <td style='padding-left: 2px;'>;</td>
            </tr>
          </table>
          &emsp;&emsp;where:<br>
          &emsp;&emsp;&emsp;<i>t</i><sub>1-Ppct</sub>(<i>x</i>), <i>t</i><sub>0.5</sub>(<i>x</i>) and <i>t</i><sub>Ppct</sub>(<i>x</i>) are Passage Time Percentiles at <i>x</i>;<br>
          &emsp;&emsp;&emsp;<i>t</i><sub>1-Ppct</sub>(<i>z</i><sub>j</sub>), <i>t</i><sub>0.5</sub>(<i>z</i><sub>j</sub>) and <i>t</i><sub>Ppct</sub>(<i>z</i><sub>j</sub>) are Passage Time Percentiles for <i>x=z</i><sub>j</sub>;<br>
          &emsp;&emsp;&emsp;<i>pt</i> are Passage Time Densities;<br>
          &emsp;&emsp;&emsp;<i>Pt</i> are Passage Time Probabilities."
      seeAlso <- "alsoAPTPercentilesOUP"
    }
    # APTDensity ----
    else if(infobutton == "infoAPTDensityOUP")
    {
      tabName <- "Passage Time Density"
      bodyText <- "An additional proportion of time an Ornstein-Uhlenbeck Process spends on the far side of a threshold is the Passage Time Density.  If crossing a threshold is irreversible, it is the First Passage Time Density.  If crossing a threshold can be completely reversed, it is the Visiting Time Density.  In between is the Passage Time Density.  A Passage Time Density is typically skewed, but can also be bi-modal and even negative if the process is attracted away from a threshold.<br><br>
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
          &emsp;&emsp;&emsp;<i>p<sub>t</sub></i>(<i>t</i>|<i>z</i><sub>j</sub>) is the Passage Time Density for <i>x=z</i><sub>j</sub>."
      seeAlso <- "alsoAPTDensityOUP"
    }
    # APTProbability ----
    else if(infobutton == "infoAPTProbabilityOUP")
    {
      tabName <- "Passage Time Probability"
      bodyText <- "The proportion of time an Ornstein-Uhlenbeck Process spends on the far side of a threshold is the Passage Time Probability.  At one extreme is the First Passage Time Probability and at the other is the Visiting Time Probability.  The First Passage Time Probability goes to one because the Ornstein-Uhlenbeck Process will eventually cross the threshold and be trapped on the far side.  In general, a Passage Time Probability does not go to one because the process may return to spend time on the near side.<br><br>
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
          &emsp;&emsp;&emsp;<i>P<sub>t</sub></i>(<i>t</i>|<i>z</i><sub>j</sub>) is the Passage Time Probability for <i>x=z</i><sub>j</sub>."
      seeAlso <- "alsoAPTProbabilityOUP"
    }
    # FDDrift ----
    else if(infobutton == "infoFDDriftOUP")
    {
      tabName <- "Drift"
      bodyText <- "Drift is the expected change in the state of a stochastic process over a brief instant.  It is a component of the partial differential equation solved by the Finite Difference Method.<br><br>
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
          &emsp;&emsp;&emsp;<i>g</i> is the Drift."
      seeAlso <- "alsoFDDriftOUP"
    }
    # FDDiffusion ----
    else if(infobutton == "infoFDDiffusionOUP")
    {
      tabName <- "Diffusion"
      bodyText <- "An error is the difference between the actual and expected changes in the state of a stochastic process.  Diffusion is the error squared over a brief instant.  It is a component of the partial differential equation solved by the Finite Difference Method.<br><br>
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
          &emsp;&emsp;&emsp;<i>h</i><sup>2</sup> is the Diffusion."
      seeAlso <- "alsoFDDiffusionOUP"
    }
    # FDTerminal ----
    else if(infobutton == "infoFDTerminalOUP")
    {
      tabName <- "Terminal Values"
      bodyText <- "Analytical option pricing has a kinked terminal value, but the Finite Difference Method is more flexible.  Any terminal value can be pre-calculated and entered into the option pricing calculations.  Some likely terminal values are programmed here for convenience.<br><br>
          &emsp;&emsp;The R6 methods:<br>
          &emsp;&emsp;&emsp;TerminalValue_Linear(<i>x,x</i>o<i>,v</i>s)<br>
          &emsp;&emsp;&emsp;TerminalValue_Degenerate(<i>x,x</i>o<i>,V</i>max<i>,V</i>min)<br>
          &emsp;&emsp;&emsp;TerminalValue_Stepped(<i>x,x</i>i<i>,v</i>s<i>,V</i>max<i>,V</i>min)<br>
          &emsp;&emsp;&emsp;TerminalValue_Kinked(<i>x,x</i>o<i>,v</i>s<i>,V</i>max<i>,V</i>min)<br>
          &emsp;&emsp;&emsp;TerminalValue_Butterfly(<i>x,x</i>i<i>,x</i>m<i>,v</i>s<i>,V</i>max<i>,V</i>min)<br>
          &emsp;&emsp;&emsp;TerminalValue_Mitscherlich(<i>x,x</i>i<i>,v</i>r<i>,V</i>max<i>,V</i>min)<br>
          &emsp;&emsp;&emsp;TerminalValue_Gompertz(<i>x,x</i>i<i>,v</i>r<i>,V</i>max<i>,V</i>min)<br>
          &emsp;&emsp;&emsp;TerminalValue_Logistic(<i>x,x</i>i<i>,v</i>r<i>,V</i>max<i>,V</i>min)<br>
          &emsp;&emsp;&emsp;TerminalValue_Transcendental(<i>x,x</i>o<i>,x</i>i<i>,x</i>m<i>,V</i>max<i>,V</i>min)<br>
          &emsp;&emsp;&emsp;TerminalValue_YieldIndex(<i>x,x</i>o<i>,x</i>i<i>,x</i>m<i>,V</i>max<i>,V</i>min)<br>
          &emsp;&emsp;with arguments:<br>
          &emsp;&emsp;&emsp;<i>x</i> are the stochastic states;<br>
          &emsp;&emsp;&emsp;<i>x</i>o is the state at the origin, kink, or step;<br>
          &emsp;&emsp;&emsp;<i>x</i>i is the state at the inflection point;<br>
          &emsp;&emsp;&emsp;<i>x</i>m is the state at the maximum or kink;<br>
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
          &emsp;&emsp;&emsp;<i>V</i> is the Terminal Value."
    }
    # FDOption ----
    else if(infobutton == "infoFDOptionOUP")
    {
      tabName <- "Option"
      bodyText <- "Options are the value of flexibility&mdash;the value of keeping your options open.  Options with kinked terminal values are a fundamental property of the Ornstein-Uhlenbeck Process and have analytical solutions.  Options with arbitrary terminal values can be calculated using the Finite Difference Method.  However, the Ornstein-Uhlenbeck Process has no boundary conditions, which makes finite difference solutions more difficult.  If possible, the Finite Difference Method should be calibrated with an analytical solution.<br><br>
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
          &emsp;&emsp;&emsp;&Oopf; is an Option."
      seeAlso <- "alsoFDOptionOUP"
    }
    # FDEnvelope ----
    else if(infobutton == "infoFDEnvelopeOUP")
    {
      tabName <- "Option Envelope"
      bodyText <- "The Option Envelope is the maximum value of either holding or exercising an option for all possible states of nature.  Using the Finite Difference Method, a matrix of Options is first calculated at discrete nodes.  Then the nodes are searched.  The discrete nodes limit the accuracy of the Option Envelope compared with an analytical solution.<br><br>
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
          &emsp;&emsp;&emsp;\u00D4 is an Option on the Envelope."
      seeAlso <- "alsoFDEnvelopeOUP"
    }
    # FDDecision ----
    else if(infobutton == "infoFDDecisionOUP")
    {
      tabName <- "Decision Threshold"
      bodyText <- "The Decision Threshold is the point of indifference between holding and exercising a perpetual option.  The Finite Difference Method calculates Options at discrete nodes, which gives an Option Envelope at discrete nodes.  Choosing a node as the indifference point is inaccurate.  To improve the accuracy, a polynomial interpolation of the Option Envelope is used to approximate the Decision Threshold. For reliability, the Finite Difference Method with a Kinked Terminal Value can be calibrated against an Analytical solution.<br><br>
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
          &emsp;&emsp;&emsp;\u00D4 is the Option at the Decision Threshold."
      seeAlso <- "alsoFDDecisionOUP"
    }
    # MLData ----
    else if(infobutton == "infoMLDataOUP")
    {
      tabName <- "Data"
      bodyText <- "The rate, location and scale parameters of the Ornstein-Uhlenbeck Process can be plucked out of the air, cogitated by experts, deduced from theory or estimated using data.<br><br>
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
          How the time intervals are measured affects the estimation of parameters <i>rho</i> and <i>sigma</i>.  For example, if measurements are taken once per year and time is reported in years, time interval <i>t-s</i> will be 1 year for a typical observation.  Parameter <i>rho</i> will likely range from 0.1 to 4.0 and <i>sigma</i> will range from 10 to 50.  If measurements are daily but time is reported in years, time interval <i>t-s</i> will be 1/365 years.  Parameter <i>rho</i> will be about 365 times larger and parameter <i>sigma</i> will be about (2<i>rho</i>)<sup>0.5</sup> times larger."
    }
    # MLLikelihood ----
    else if(infobutton == "infoMLLikelihoodOUP")
    {
      tabName <- "Log Likelihood"
      bodyText <- "The Likelihood is the joint probability of observing a time-series as a random sample.  For numerical reasons, the natural logarithm of the Likelihood, or Log Likelihood, is calculated instead.  The Log Likelihood can be maximized to estimate the parameters of the Ornstein-Uhlenbeck Process.  It can be calculated for a restricted set of parameters to test hypotheses.  An example would compare two sets of parameters by calculating their Log Likelihoods and conducting a Likelihood Ratio Test.<br><br>
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
          &emsp;&emsp;&emsp;ln<i>L</i> is the Log Likelihood."
    }
    # MLEstimates ----
    else if(infobutton == "infoMLEstimatesOUP")
    {
      tabName <- "Estimates"
      bodyText <- "Maximum Likelihood Estimation finds the rate, location and scale parameters of the Ornstein-Uhlenbeck Process which maximize the Log Likelihood.  Some or all the parameters can be fixed to constants and other parameters re-estimated.  This gives the Restricted Log Likelihood, which must be less than the Unrestricted Log Likelihood.  The probability distribution of a Log Likelihood is identified by parameter <i>&alpha;</i>, where <i>&alpha;</i>=0.5 for a <i>&chi;</i><sup>2</sup> distribution, <i>&alpha;</i>=1 for an Erlang distribution.  These are special cases of 0.5&le;<i>&alpha;</i>&le;1 for a Gamma distribution.<br><br>
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
          &emsp;&emsp;&emsp;<i>m</i>-1 is the number of observations."
    }
    # MLGoodness ----
    else if(infobutton == "infoMLGoodnessOUP")
    {
      tabName <- "Goodness-of-Fit"
      bodyText <- "Goodness of Fit compares the Log Likelihood of the estimated parameters to the Invariant Log Likelihood and to the Log Likelihood of Scaled Brownian Motion.  Comparing with the Invariant Likelihood tests the null hypothesis H<sub>0</sub>:  'the Ornstein-Uhlenbeck Process has converged'.  Comparing with the Likelihood of Scaled Brownian Motion tests the null hypothesis H<sub>0</sub>:  'the Ornstein-Uhlenbeck does not converge'.  Goodness of Fit is summarized by two Pseudo-<i>R</i>&hairsp;<sup>2</sup> statistics and two probabilities.  A null hypothesis is rejected if the <i>R</i>&hairsp;<sup>2</sup> is at least 0.5 and the probability is small.<br><br>
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
          &emsp;&emsp;&emsp;1-<i>P</i><sub>&infin;</sub> and 1-<i>P</i><sub>0</sub> are the right-tails of Gamma probabilities."
    }
    # MLRatio ----
    else if(infobutton == "infoMLRatioOUP")
    {
      tabName <- "Likelihood Ratio Test"
      bodyText <- "Hypothesis tests are constrained optimization with restrictions placed on the parameters.  One form of the null hypothesis is H<sub>0</sub>:  'parameters can take their restricted values'. The alternate hypothesis is H<sub>1</sub>:  'parameters cannot take their restricted values'.  A Likelihood Ratio Test rejects the null hypothesis if the restricted Log Likelihood is significantly smaller than the unrestricted Log Likelihood.  A null hypothesis is rejected if the <i>R</i>&hairsp;<sup>2</sup> is at least 0.5 and the probability is small.<br><br>
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
          &emsp;&emsp;&emsp;1-<i>P</i> is the right-tail of a Gamma probability."
    }
    # MCForward----
    else if(infobutton == "infoMCForwardOUP")
    {
      tabName <- "Forward Paths"
      bodyText <- "A Forward Path starts from the backward state <i>x</i> and goes forward over times <i>t</i>.  A single path, sampled from all possible paths, is a Sample Path.  A Sample Path is enough for Maximum Likelihood Estimation of the Ornstein-Uhlenbeck Process.  An ensemble of paths can be counted to approximate Transition Densities and Probabilities, and Visiting Time Densities and Probabilities.  The larger the ensemble, the better the approximations.<br><br>
          &emsp;&emsp;The R6 method:<br>
          &emsp;&emsp;&emsp;ForwardPaths(<i>t,x,rho,mu,sigma,paths,skip</i>)<br>
          &emsp;&emsp;with arguments:<br>
          &emsp;&emsp;&emsp;<i>t</i> a vector of forward times;<br>
          &emsp;&emsp;&emsp;<i>x</i> is the fixed initial state;<br>
          &emsp;&emsp;&emsp;<i>rho</i> is the rate parameter;<br>
          &emsp;&emsp;&emsp;<i>mu</i> is the location parameter;<br>
          &emsp;&emsp;&emsp;<i>sigma</i> is the scale parameter;<br>
          &emsp;&emsp;&emsp;<i>paths</i> is the number of paths;<br>
          &emsp;&emsp;&emsp;<i>skip</i> subdivides the time interval but reports at times t;<br>
          &emsp;&emsp;returns:<br>
          <table style='margin-left: 60px;'>
            <tr>
              <td style='border-top: solid silver; border-left: solid silver'>&nbsp;</td>
              <td style='padding: 0px 4px 0px 4px;'><i>y</i><sub>1,1</sub></td>
              <td style='padding: 0px 4px 0px 4px;'>&hellip;</td>
              <td style='padding: 0px 4px 0px 4px;'><i>y</i><sub>1,n</sub></td>
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
              <td style='padding: 0px 4px 0px 4px;'><i>y</i><sub>m,1</sub></td>
              <td style='padding: 0px 4px 0px 4px;'>&hellip;</td>
              <td style='padding: 0px 4px 0px 4px;'><i>y</i><sub>m,n</sub></td>
              <td style='border-bottom: solid silver; border-right: solid silver'>&nbsp;</td>
              <td style='padding-left: 2px;'>;</td>
            </tr>
          </table>
          &emsp;&emsp;where:<br>
          &emsp;&emsp;&emsp;<i>y</i><sub>i,j</sub> is a stochastic state at time i on path j."
    }
    # MCBackward----
    else if(infobutton == "infoMCBackwardOUP")
    {
      tabName <- "Backward Paths"
      bodyText <- "A Forward Path begins from a known initial state and travels forward into an uncertain future.  A Backward Path ends with a known terminal state and trudges backward into an uncertain past.  Turning around and travelling back to the future resolves the uncertainty over time.  An example is a Bayesian analysis which begins with a Prior Distribution and ends with certainty.  Another example is an Option.  Simulating and counting Backward Paths approximates Prior Densities, Prior Probabilities and Options.<br><br>
          &emsp;&emsp;The R6 method:<br>
          &emsp;&emsp;&emsp;BackwardPaths(<i>s,y,rho,mu,sigma,paths,skip</i>)<br>
          &emsp;&emsp;with arguments:<br>
          &emsp;&emsp;&emsp;<i>s</i> is a vector of backward times;<br>
          &emsp;&emsp;&emsp;<i>y</i> is the fixed terminal state;<br>
          &emsp;&emsp;&emsp;<i>rho</i> is the rate parameter;<br>
          &emsp;&emsp;&emsp;<i>mu</i> is the location parameter;<br>
          &emsp;&emsp;&emsp;<i>sigma</i> is the scale parameter;<br>
          &emsp;&emsp;&emsp;<i>paths</i> is the number of paths;<br>
          &emsp;&emsp;&emsp;<i>skip</i> subdivides the time interval but reports at times s;<br>
          &emsp;&emsp;returns:<br>
          <table style='margin-left: 60px;'>
            <tr>
              <td style='border-top: solid silver; border-left: solid silver'>&nbsp;</td>
              <td style='padding: 0px 4px 0px 4px;'><i>x</i><sub>m,1</sub></td>
              <td style='padding: 0px 4px 0px 4px;'>&hellip;</td>
              <td style='padding: 0px 4px 0px 4px;'><i>x</i><sub>m,n</sub></td>
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
              <td style='padding: 0px 4px 0px 4px;'><i>x</i><sub>1,1</sub></td>
              <td style='padding: 0px 4px 0px 4px;'>&hellip;</td>
              <td style='padding: 0px 4px 0px 4px;'><i>x</i><sub>1,n</sub></td>
              <td style='border-bottom: solid silver; border-right: solid silver'>&nbsp;</td>
              <td style='padding-left: 2px;'>;</td>
            </tr>
          </table>
          &emsp;&emsp;where:<br>
          &emsp;&emsp;&emsp;<i>x</i><sub>i,j</sub> is a stochastic state at time i on path j."
    }
    # MCBounded ----
    else if(infobutton == "infoMCBoundedOUP")
    {
      tabName <- "Bounded Paths"
      bodyText <- "Paths may hit a reflecting boundary and bounce off, reach an absorbing boundary and stop, or cross an irreversible threshold and become trapped on the other side.  These paths are bounded.  Binning and counting the Bounded Paths that reach an absorbing boundary or cross an irreversible threshold gives First Passage Time Densities.  Summing gives First Passage Time Probabilities.<br><br>
          &emsp;&emsp;The R6 method:<br>
          &emsp;&emsp;&emsp;BoundedPaths(<i>t,x,rho,mu,sigma,paths,skip</i>)<br>
          &emsp;&emsp;with arguments:<br>
          &emsp;&emsp;&emsp;<i>t</i> is a vector of forward times;<br>
          &emsp;&emsp;&emsp;<i>x</i> is the fixed initial state;<br>
          &emsp;&emsp;&emsp;<i>k</i> is the threshold;<br>
          &emsp;&emsp;&emsp;<i>rho</i> is the rate parameter;<br>
          &emsp;&emsp;&emsp;<i>mu</i> is the location parameter;<br>
          &emsp;&emsp;&emsp;<i>sigma</i> is the scale parameter;<br>
          &emsp;&emsp;&emsp;<i>paths</i> is the number of paths;<br>
          &emsp;&emsp;&emsp;<i>skip</i> subdivides the time interval but reports at times t;<br>
          &emsp;&emsp;returns:<br>
          <table style='margin-left: 60px;'>
            <tr>
              <td style='border-top: solid silver; border-left: solid silver'>&nbsp;</td>
              <td style='padding: 0px 4px 0px 4px;'><i>y</i><sub>1,1</sub></td>
              <td style='padding: 0px 4px 0px 4px;'>&hellip;</td>
              <td style='padding: 0px 4px 0px 4px;'><i>y</i><sub>1,n</sub></td>
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
              <td style='padding: 0px 4px 0px 4px;'><i>y</i><sub>m,1</sub></td>
              <td style='padding: 0px 4px 0px 4px;'>&hellip;</td>
              <td style='padding: 0px 4px 0px 4px;'><i>y</i><sub>m,n</sub></td>
              <td style='border-bottom: solid silver; border-right: solid silver'>&nbsp;</td>
              <td style='padding-left: 2px;'>;</td>
            </tr>
          </table>
          &emsp;&emsp;where:<br>
          &emsp;&emsp;&emsp;<i>y</i><sub>i,j</sub> is a stochastic state at time i on path j or NA if a path has stopped."
    }
    # MCMean ----
    else if(infobutton == "infoMCMeanOUP")
    {
      tabName <- "Mean"
      bodyText <- "A Mean is what we expect a state nature to be sometime in the future.  Each time has a different Mean because Forward Paths begin at the initial state, <i>x</i> and are attracted toward location <i>mu</i>.  Given an ensemble of Forward Paths, a Mean is approximated by the average of states <i>y</i> at time <i>t</i>.<br><br>
          &emsp;&emsp;The R6 method:<br>
          &emsp;&emsp;&emsp;Mean(<i>tx,rho,mu,paths,skip</i>)<br>
          &emsp;&emsp;with arguments:<br>
          &emsp;&emsp;&emsp;<i>t</i> is a vector of forward times;<br>
          &emsp;&emsp;&emsp;<i>x</i> is the fixed initial state;<br>
          &emsp;&emsp;&emsp;<i>rho</i> is the rate parameter;<br>
          &emsp;&emsp;&emsp;<i>mu</i> is the location parameter;<br>
          &emsp;&emsp;&emsp;<i>paths</i> is the number of paths;<br>
          &emsp;&emsp;&emsp;<i>skip</i> subdivides the time interval but reports at times t;<br>
          &emsp;&emsp;returns:<br>
          <table style='margin-left: 60px;'>
            <tr>
              <td style='border-top: solid silver; border-left: solid silver'>&nbsp;</td>
              <td style='padding: 0px 4px 0px 4px;'><i>G</i><sub>1</sub></td>
              <td style='border-top: solid silver; border-right: solid silver'>&nbsp;</td>
            </tr>
            <tr>
              <td style='border-left: solid silver'>&nbsp;</td>
              <td style='padding: 0px 4px 0px 4px;'>&emsp;&vellip;</td>
              <td style='border-right: solid silver'>&nbsp;</td>
            </tr>
            <tr>
              <td style='border-bottom: solid silver; border-left: solid silver'>&nbsp;</td>
              <td style='padding: 0px 4px 0px 4px;'><i>G</i><sub>m</sub></td>
              <td style='border-bottom: solid silver; border-right: solid silver'>&nbsp;</td>
              <td style='padding-left: 2px;'>;</td>
            </tr>
          </table>
          &emsp;&emsp;where:<br>
          &emsp;&emsp;&emsp;<i>G</i><sub>i</sub> is the Mean at time i."
      seeAlso <- "alsoMCMeanOUP"
    }
    # MCVariance ----
    else if(infobutton == "infoMCVarianceOUP")
    {
      tabName <- "Variance"
      bodyText <- "We don't know future states of nature with certainty.  We only know what we expect--we only know the Means.  As time passes, our expectations will prove to be in error.  An error is the difference between what the state proves to be and the Mean.  An error is <i>y-G</i>.  A Variance is the error squared.  Given an ensemble of Forward Paths, it is approximated as the average of (<i>y-G</i>)<sup>2</sup> at time <i>t</i>.<br><br>
          &emsp;&emsp;The R6 method:<br>
          &emsp;&emsp;&emsp;Variance(<i>t,rho,sigma,paths,skip</i>)<br>
          &emsp;&emsp;with arguments:<br>
          &emsp;&emsp;&emsp;<i>t</i> is a vector of forward times;<br>
          &emsp;&emsp;&emsp;<i>rho</i> is the rate parameter;<br>
          &emsp;&emsp;&emsp;<i>sigma</i> is the scale parameter;<br>
          &emsp;&emsp;&emsp;<i>paths</i> is the number of paths;<br>
          &emsp;&emsp;&emsp;<i>skip</i> subdivides the time interval but reports at times t;<br>
          &emsp;&emsp;returns:<br>
          <table style='margin-left: 60px;'>
            <tr>
              <td style='border-top: solid silver; border-left: solid silver'>&nbsp;</td>
              <td style='padding: 0px 4px 0px 4px;'><i>H</i>&hairsp;<sup>2</sup><sub>1</sub></td>
              <td style='border-top: solid silver; border-right: solid silver'>&nbsp;</td>
            </tr>
            <tr>
              <td style='border-left: solid silver'>&nbsp;</td>
              <td style='padding: 0px 4px 0px 4px;'>&emsp;&vellip;</td>
              <td style='border-right: solid silver'>&nbsp;</td>
            </tr>
            <tr>
              <td style='border-bottom: solid silver; border-left: solid silver'>&nbsp;</td>
              <td style='padding: 0px 4px 0px 4px;'><i>H</i>&hairsp;<sup>2</sup><sub>m</sub></td>
              <td style='border-bottom: solid silver; border-right: solid silver'>&nbsp;</td>
              <td style='padding-left: 2px;'>;</td>
            </tr>
          </table>
          &emsp;&emsp;where:<br>
          &emsp;&emsp;&emsp;<i>H</i>&hairsp;<sup>2</sup><sub>i</sub> is the Variance at time i."
      seeAlso <- "alsoMCVarianceOUP"
    }
    # MCDensity ----
    else if(infobutton == "infoMCDensityOUP")
    {
      tabName <- "Transition Density"
      bodyText <- "How likely are our expectations to be right?  How likely are they to be wrong?  The likelihood our expectations are right or wrong is the Transition Density.  Given an ensemble of Forward Paths, it is the proportion of paths to reach each possible state.  It is approximated by assigning states <i>y</i> to bins, counting the states in each bin and dividing by the number of paths.<br><br>
          &emsp;&emsp;The R6 method:<br>
          &emsp;&emsp;&emsp;Density(<i>t,y,x,rho,mu,sigma,paths,skip</i>)<br>
          &emsp;&emsp;with arguments:<br>
          &emsp;&emsp;&emsp;<i>t</i> is a vector of forward times;<br>
          &emsp;&emsp;&emsp;<i>y</i> is a vector of stochastic states;<br>
          &emsp;&emsp;&emsp;<i>x</i> is the fixed initial state;<br>
          &emsp;&emsp;&emsp;<i>rho</i> is the rate parameter;<br>
          &emsp;&emsp;&emsp;<i>mu</i> is the location parameter;<br>
          &emsp;&emsp;&emsp;<i>sigma</i> is the scale parameter;<br>
          &emsp;&emsp;&emsp;<i>paths</i> is the number of paths;<br>
          &emsp;&emsp;&emsp;<i>skip</i> subdivides the time interval but reports at times t;<br>
          &emsp;&emsp;returns:<br>
          <table style='margin-left: 60px;'>
            <tr>
              <td style='border-top: solid silver; border-left: solid silver'>&nbsp;</td>
              <td style='padding: 0px 4px 0px 4px;'><i>p</i><sub>1,1</sub></td>
              <td style='padding: 0px 4px 0px 4px;'>&hellip;</td>
              <td style='padding: 0px 4px 0px 4px;'><i>p</i><sub>1,n</sub></td>
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
              <td style='padding: 0px 4px 0px 4px;'><i>p</i><sub>m,1</sub></td>
              <td style='padding: 0px 4px 0px 4px;'>&hellip;</td>
              <td style='padding: 0px 4px 0px 4px;'><i>p</i><sub>m,n</sub></td>
              <td style='border-bottom: solid silver; border-right: solid silver'>&nbsp;</td>
              <td style='padding-left: 2px;'>;</td>
            </tr>
          </table>
          &emsp;&emsp;where:<br>
          &emsp;&emsp;&emsp;<i>p</i><sub>i,j</sub> is the Transition Density at time i for bin j."
      seeAlso <- "alsoMCDensityOUP"
    }
    # MCProbability ----
    else if(infobutton == "infoMCProbabilityOUP")
    {
      tabName <- "Transition Probability"
      bodyText <- "The likelihood we expect too much or too little is the Transition Probability.  Given an ensemble of Forward Paths, it is the proportion of paths less than or greater than each possible state.  It is approximated by first approximating Transition Densities and then summing from small to large or from large to small.<br><br>
          &emsp;&emsp;The R6 method:<br>
          &emsp;&emsp;&emsp;Probability(<i>t,y,x,psi,rho,mu,sigma,paths,skip</i>)<br>
          &emsp;&emsp;with arguments:<br>
          &emsp;&emsp;&emsp;<i>t</i> is a vector of forward times;<br>
          &emsp;&emsp;&emsp;<i>y</i> is a vector of stochastic states;<br>
          &emsp;&emsp;&emsp;<i>x</i> is the fixed initial state;<br>
          &emsp;&emsp;&emsp;<i>psi</i> is < 0 for sums from -Inf to y, > 0 for sums from y to Inf;<br>
          &emsp;&emsp;&emsp;<i>rho</i> is the rate parameter;<br>
          &emsp;&emsp;&emsp;<i>mu</i> is the location parameter;<br>
          &emsp;&emsp;&emsp;<i>sigma</i> is the scale parameter;<br>
          &emsp;&emsp;&emsp;<i>paths</i> is the number of paths;<br>
          &emsp;&emsp;&emsp;<i>skip</i> subdivides the time interval but reports at times t;<br>
          &emsp;&emsp;returns:<br>
          <table style='margin-left: 60px;'>
            <tr>
              <td style='border-top: solid silver; border-left: solid silver'>&nbsp;</td>
              <td style='padding: 0px 4px 0px 4px;'><i>P</i><sub>1,1</sub></td>
              <td style='padding: 0px 4px 0px 4px;'>&hellip;</td>
              <td style='padding: 0px 4px 0px 4px;'><i>P</i><sub>1,n</sub></td>
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
              <td style='padding: 0px 4px 0px 4px;'><i>P</i><sub>m,1</sub></td>
              <td style='padding: 0px 4px 0px 4px;'>&hellip;</td>
              <td style='padding: 0px 4px 0px 4px;'><i>P</i><sub>m,n</sub></td>
              <td style='border-bottom: solid silver; border-right: solid silver'>&nbsp;</td>
              <td style='padding-left: 2px;'>;</td>
            </tr>
          </table>
          &emsp;&emsp;where:<br>
          &emsp;&emsp;&emsp;<i>P</i><sub>i,j</sub> is the Transition Probability at time i for bin j."
      seeAlso <- "alsoMCProbabilityOUP"
    }
    # MCDouble ----
    else if(infobutton == "infoMCDoubleOUP")
    {
      tabName <- "Double Integral"
      bodyText <- "Transition Probabilities are the sum of Transition Densities.  Suppose we sum the Transition Probabilities.  The result is Double Integrals.  These are not likelihoods because they are too large.  But they look suspiciously like Options.  The purpose of Double Integrals is to show that Options are not a fabulation of financial economists.  Like Means, Variances, Densities and Probabilities, Options are a fundamental property of stochastic processes.<br><br>
          &emsp;&emsp;The R6 method:<br>
          &emsp;&emsp;&emsp;DoubleIntegral(<i>t,y,x,psi,rho,mu,sigma,paths,skip</i>)<br>
          &emsp;&emsp;with arguments:<br>
          &emsp;&emsp;&emsp;<i>t</i> is a vector of forward times;<br>
          &emsp;&emsp;&emsp;<i>y</i> is a vector of stochastic states;<br>
          &emsp;&emsp;&emsp;<i>x</i> is the fixed initial state;<br>
          &emsp;&emsp;&emsp;<i>psi</i> is < 0 for sums from -Inf to y, > 0 for sums from y to Inf;<br>
          &emsp;&emsp;&emsp;<i>rho</i> is the rate parameter;<br>
          &emsp;&emsp;&emsp;<i>mu</i> is the location parameter;<br>
          &emsp;&emsp;&emsp;<i>sigma</i> is the scale parameter;<br>
          &emsp;&emsp;&emsp;<i>paths</i> is the number of paths;<br>
          &emsp;&emsp;&emsp;<i>skip</i> subdivides the time interval but reports at times t;<br>
          &emsp;&emsp;returns:<br>
          <table style='margin-left: 60px;'>
            <tr>
              <td style='border-top: solid silver; border-left: solid silver'>&nbsp;</td>
              <td style='padding: 0px 4px 0px 4px;'>&Popf;<sub>1,1</sub></td>
              <td style='padding: 0px 4px 0px 4px;'>&hellip;</td>
              <td style='padding: 0px 4px 0px 4px;'>&Popf;<sub>1,n</sub></td>
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
              <td style='padding: 0px 4px 0px 4px;'>&Popf;<sub>m,1</sub></td>
              <td style='padding: 0px 4px 0px 4px;'>&hellip;</td>
              <td style='padding: 0px 4px 0px 4px;'>&Popf;<sub>m,n</sub></td>
              <td style='border-bottom: solid silver; border-right: solid silver'>&nbsp;</td>
              <td style='padding-left: 2px;'>;</td>
            </tr>
          </table>
          &emsp;&emsp;where:<br>
          &emsp;&emsp;&emsp;&Popf;<sub>i,j</sub> is the Double Integral at time i for bin j."
      seeAlso <- "alsoMCDoubleOUP"
    }
    # MCOption ----
    else if(infobutton == "infoMCOptionOUP")
    {
      tabName <- "Option"
      bodyText <- "What if time can run backwards?  What if we simulate Backward Paths from a future time and state back to the present?  If we bin and count the Backward Paths, we will have a density.  If we sum the density, we will have a probability and if we sum the probability, we will approximate an Option.  Where Forward Paths converge, Backward Paths diverge at rate <i>rho</i>.  To get a finite result, we discount at rate <i>rho</i> and counteract the divergence.  Options are usually considered to be investments.  We can compare Options with the opportunity cost of investment by also discounting at rate <i>r</i>.<br><br>
          &emsp;&emsp;The R6 method:<br>
          &emsp;&emsp;&emsp;Option(<i>s,x,y,phi,rho,mu,sigma,paths,skip</i>)<br>
          &emsp;&emsp;with arguments:<br>
          &emsp;&emsp;&emsp;<i>s</i> is a vector of backward times;<br>
          &emsp;&emsp;&emsp;<i>x</i> is a vector of stochastic states;<br>
          &emsp;&emsp;&emsp;<i>y</i> is the fixed terminal state;<br>
          &emsp;&emsp;&emsp;<i>r</i> is the discount rate;<br>
          &emsp;&emsp;&emsp;<i>phi</i> is < 0 for sums from -Inf to x, > 0 for sums from x to Inf;<br>
          &emsp;&emsp;&emsp;<i>rho</i> is the rate parameter;<br>
          &emsp;&emsp;&emsp;<i>mu</i> is the location parameter;<br>
          &emsp;&emsp;&emsp;<i>sigma</i> is the scale parameter;<br>
          &emsp;&emsp;&emsp;<i>paths</i> is the number of paths;<br>
          &emsp;&emsp;&emsp;<i>skip</i> subdivides the time interval but reports at times t;<br>
          &emsp;&emsp;returns:<br>
          <table style='margin-left: 60px;'>
            <tr>
              <td style='border-top: solid silver; border-left: solid silver'>&nbsp;</td>
              <td style='padding: 0px 4px 0px 4px;'>&Oopf;<sub>1,1</sub></td>
              <td style='padding: 0px 4px 0px 4px;'>&hellip;</td>
              <td style='padding: 0px 4px 0px 4px;'>&Oopf;<sub>1,n</sub></td>
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
              <td style='padding: 0px 4px 0px 4px;'>&Oopf;<sub>m,1</sub></td>
              <td style='padding: 0px 4px 0px 4px;'>&hellip;</td>
              <td style='padding: 0px 4px 0px 4px;'>&Oopf;<sub>m,n</sub></td>
              <td style='border-bottom: solid silver; border-right: solid silver'>&nbsp;</td>
              <td style='padding-left: 2px;'>;</td>
            </tr>
          </table>
          &emsp;&emsp;where:<br>
          &emsp;&emsp;&emsp;&Oopf;<sub>i,j</sub> is the Option at time i for bin j."
      seeAlso <- "alsoMCOptionOUP"
    }
    # MCVTModeMedianMean ----
    else if(infobutton == "infoMCVTModeMedianMeanOUP")
    {
      tabName <- "Visiting Time Mode, Median and Mean"
      bodyText <- "If crossing a threshold is reversible, we can visit the far side and return later. Visiting Times are the net visits.  To find net visits, Forward Paths are counted in the time direction instead of the state direction.  The mode is the time when crossings to the far side minus returns to the near side are greatest.  The median is the time when crossings minus returns are 50% of the proportion of time spent on the far side in a long-term equilibrium.  The mean is the average of all crossing times minus all return times.<br><br>
          &emsp;&emsp;The R6 method:<br>
          &emsp;&emsp;&emsp;VisitingTimeModeMedianMean(<i>t,k,x,rho,mu,sigma,paths,skip</i>)<br>
          &emsp;&emsp;with arguments:<br>
          &emsp;&emsp;&emsp;<i>t</i> is a vector of forward times;<br>
          &emsp;&emsp;&emsp;<i>k</i> is a reversible threshold;<br>
          &emsp;&emsp;&emsp;<i>x</i> is the fixed initial state;<br>
          &emsp;&emsp;&emsp;<i>rho</i> is the rate parameter;<br>
          &emsp;&emsp;&emsp;<i>mu</i> is the location parameter;<br>
          &emsp;&emsp;&emsp;<i>sigma</i> is the scale parameter;<br>
          &emsp;&emsp;&emsp;<i>paths</i> is the number of paths;<br>
          &emsp;&emsp;&emsp;<i>skip</i> subdivides the time interval but reports at times t;<br>
          &emsp;&emsp;returns:<br>
          <table style='margin-left: 60px;'>
            <tr>
              <td style='border-top: solid silver; border-left: solid silver'>&nbsp;</td>
              <td style='padding: 0px 4px 0px 4px;'><i>t</i><sub>mode</sub></td>
              <td style='padding: 0px 4px 0px 4px;'><i>pv</i><sub>mode</sub></td>
              <td style='padding: 0px 4px 0px 4px;'><i>Pv</i><sub>mode</sub></td>
              <td style='border-top: solid silver; border-right: solid silver'>&nbsp;</td>
            </tr>
            <tr>
              <td style='border-left: solid silver'>&nbsp;</td>
              <td style='padding: 0px 4px 0px 4px;'><i>t</i><sub>median</sub></td>
              <td style='padding: 0px 4px 0px 4px;'><i>pv</i><sub>median</sub></td>
              <td style='padding: 0px 4px 0px 4px;'><i>Pv</i><sub>median</sub></td>
              <td style='border-right: solid silver'>&nbsp;</td>
            </tr>
            <tr>
              <td style='border-bottom: solid silver; border-left: solid silver'>&nbsp;</td>
              <td style='padding: 0px 4px 0px 4px;'><i>t</i><sub>mean</sub></td>
              <td style='padding: 0px 4px 0px 4px;'><i>pv</i><sub>mean</sub></td>
              <td style='padding: 0px 4px 0px 4px;'><i>Pv</i><sub>mean</sub></td>
              <td style='border-bottom: solid silver; border-right: solid silver'>&nbsp;</td>
              <td style='padding-left: 2px;'>;</td>
            </tr>
          </table>
          &emsp;&emsp;where:<br>
          &emsp;&emsp;&emsp;<i>t</i> are times;<br>
          &emsp;&emsp;&emsp;<i>pv</i> are Visiting Time Densities;<br>
          &emsp;&emsp;&emsp;<i>Pv</i> are Visiting Time Probabilities."
      seeAlso <- "alsoMCVTModeMedianMeanOUP"
    }
    # MCVTPercentiles ----
    else if(infobutton == "infoMCVTPercentilesOUP")
    {
      tabName <- "Visiting Time Percentiles"
      bodyText <- "Visiting Time Densities and Probabilities are skewed with three measures of central tendency, the Mode, Median and Mean.  As a measure of dispersion, the square-root of the Variance is difficult to calculate and interpret.  An easier alternative is Percentiles.  The Median is the time with a 50% chance.  Higher and lower Percentiles are evenly dispersed around the Median.  In principle, Percentiles could be calculated by sorting the crossing and return times from soonest to latest and counting the first 25%, 50% and 75%, for example.  In practice, a million Forward Paths can have billions of crossing and return times.  A more practical approach finds the Visiting Time Probabilities and then counts along the probabilities to find percentiles.<br><br>
          &emsp;&emsp;The R6 method:<br>
          &emsp;&emsp;&emsp;VisitingTimePercentiles(<i>t,k,x,Ppct,rho,mu,sigma,paths,skip</i>)<br>
          &emsp;&emsp;with arguments:<br>
          &emsp;&emsp;&emsp;<i>t</i> is a vector of forward times;<br>
          &emsp;&emsp;&emsp;<i>k</i> is a reversible threshold;<br>
          &emsp;&emsp;&emsp;<i>x</i> is the fixed initial state;<br>
          &emsp;&emsp;&emsp;<i>Ppct</i> is a probability for a percentile;<br>
          &emsp;&emsp;&emsp;<i>rho</i> is the rate parameter;<br>
          &emsp;&emsp;&emsp;<i>mu</i> is the location parameter;<br>
          &emsp;&emsp;&emsp;<i>sigma</i> is the scale parameter;<br>
          &emsp;&emsp;&emsp;<i>paths</i> is the number of paths;<br>
          &emsp;&emsp;&emsp;<i>skip</i> subdivides the time interval but reports at times t;<br>
          &emsp;&emsp;returns:<br>
          <table style='margin-left: 60px;'>
            <tr>
              <td style='border-top: solid silver; border-left: solid silver'>&nbsp;</td>
              <td style='padding: 0px 4px 0px 4px;'><i>t</i><sub>1-Ppct</sub></td>
              <td style='padding: 0px 4px 0px 4px;'><i>pv</i><sub>1-Ppct</sub></td>
              <td style='padding: 0px 4px 0px 4px;'><i>Pv</i><sub>1-Ppct</sub></td>
              <td style='border-top: solid silver; border-right: solid silver'>&nbsp;</td>
            </tr>
            <tr>
              <td style='border-left: solid silver'>&nbsp;</td>
              <td style='padding: 0px 4px 0px 4px;'><i>t</i><sub>0.5</sub></td>
              <td style='padding: 0px 4px 0px 4px;'><i>pv</i><sub>0.5</sub></td>
              <td style='padding: 0px 4px 0px 4px;'><i>Pv</i><sub>0.5</sub></td>
              <td style='border-right: solid silver'>&nbsp;</td>
            </tr>
            <tr>
              <td style='border-bottom: solid silver; border-left: solid silver'>&nbsp;</td>
              <td style='padding: 0px 4px 0px 4px;'><i>t</i><sub>Ppct</sub></td>
              <td style='padding: 0px 4px 0px 4px;'><i>pv</i><sub>Ppct</sub></td>
              <td style='padding: 0px 4px 0px 4px;'><i>Pv</i><sub>Ppct</sub></td>
              <td style='border-bottom: solid silver; border-right: solid silver'>&nbsp;</td>
              <td style='padding-left: 2px;'>;</td>
            </tr>
          </table>
          &emsp;&emsp;where:<br>
          &emsp;&emsp;&emsp;<i>t</i> are times;<br>
          &emsp;&emsp;&emsp;<i>pv</i> are Visiting Time Densities;<br>
          &emsp;&emsp;&emsp;<i>Pv</i> are Visiting Time Probabilities."
      seeAlso <- "alsoMCVTPercentilesOUP"
    }
    # MCVTDensity ----
    else if(infobutton == "infoMCVTDensityOUP")
    {
      tabName <- "Visiting Time Density"
      bodyText <- "A Visiting Time Density is the additional proportion of Forward Paths visiting the far side of a threshold.  In principle, the crossings to the far side, minus returns to the near side could be binned and counted.  There can be 100's of millions of crossings and returns, however.  In practice, Visiting Time Densities are calculated as the change in Visiting Time Probabilities.<br><br>
          &emsp;&emsp;The R6 method:<br>
          &emsp;&emsp;&emsp;VisitingTimeDensity(<i>t,k,x,rho,mu,sigma,paths,skip</i>)<br>
          &emsp;&emsp;with arguments:<br>
          &emsp;&emsp;&emsp;<i>t</i> is a vector of forward times;<br>
          &emsp;&emsp;&emsp;<i>k</i> is a reversible threshold;<br>
          &emsp;&emsp;&emsp;<i>x</i> is the fixed initial state;<br>
          &emsp;&emsp;&emsp;<i>rho</i> is the rate parameter;<br>
          &emsp;&emsp;&emsp;<i>mu</i> is the location parameter;<br>
          &emsp;&emsp;&emsp;<i>sigma</i> is the scale parameter;<br>
          &emsp;&emsp;&emsp;<i>paths</i> is the number of paths;<br>
          &emsp;&emsp;&emsp;<i>skip</i> subdivides the time interval but reports at times t;<br>
          &emsp;&emsp;returns:<br>
          <table style='margin-left: 60px;'>
            <tr>
              <td style='border-top: solid silver; border-left: solid silver'>&nbsp;</td>
              <td style='padding: 0px 4px 0px 4px;'><i>pv</i><sub>1</sub></td>
              <td style='border-top: solid silver; border-right: solid silver'>&nbsp;</td>
            </tr>
            <tr>
              <td style='border-left: solid silver'>&nbsp;</td>
              <td style='padding: 0px 4px 0px 4px;'>&emsp;&vellip;</td>
              <td style='border-right: solid silver'>&nbsp;</td>
            </tr>
            <tr>
              <td style='border-bottom: solid silver; border-left: solid silver'>&nbsp;</td>
              <td style='padding: 0px 4px 0px 4px;'><i>pv</i><sub>m</sub></td>
              <td style='border-bottom: solid silver; border-right: solid silver'>&nbsp;</td>
              <td style='padding-left: 2px;'>;</td>
            </tr>
          </table>
          &emsp;&emsp;where:<br>
          &emsp;&emsp;&emsp;<i>pv</i><sub>i</sub> are Visiting Time Densities at time i."
      seeAlso <- "alsoMCVTDensityOUP"
    }
    # MCVTProbability ----
    else if(infobutton == "infoMCVTProbabilityOUP")
    {
      tabName <- "Visiting Time Probability"
      bodyText <- "A Visiting Time Probability is the proportion of Forward Paths on the far side of a completely reversible threshold.  Unlike most probabilities, it need not go to one because some Forward Paths will return to the near side.  Rather than try to count all the crossings and returns in the time direction, the Visiting Time Probability counts in the state direction.  It counts the Forward Paths on the far side of the threshold at a given time and divides by the number of paths.  This simple count must equal the cumulative crossings and returns over time.<br><br>
          &emsp;&emsp;The R6 method:<br>
          &emsp;&emsp;&emsp;VisitingTimeProbability(<i>t,k,x,rho,mu,sigma,paths,skip</i>)<br>
          &emsp;&emsp;with arguments:<br>
          &emsp;&emsp;&emsp;<i>t</i> is a vector of forward times;<br>
          &emsp;&emsp;&emsp;<i>k</i> is a reversible threshold;<br>
          &emsp;&emsp;&emsp;<i>x</i> is the fixed initial state;<br>
          &emsp;&emsp;&emsp;<i>rho</i> is the rate parameter;<br>
          &emsp;&emsp;&emsp;<i>mu</i> is the location parameter;<br>
          &emsp;&emsp;&emsp;<i>sigma</i> is the scale parameter;<br>
          &emsp;&emsp;&emsp;<i>paths</i> is the number of paths;<br>
          &emsp;&emsp;&emsp;<i>skip</i> subdivides the time interval but reports at times t;<br>
          &emsp;&emsp;returns:<br>
          <table style='margin-left: 60px;'>
            <tr>
              <td style='border-top: solid silver; border-left: solid silver'>&nbsp;</td>
              <td style='padding: 0px 4px 0px 4px;'><i>Pv</i><sub>1</sub></td>
              <td style='border-top: solid silver; border-right: solid silver'>&nbsp;</td>
            </tr>
            <tr>
              <td style='border-left: solid silver'>&nbsp;</td>
              <td style='padding: 0px 4px 0px 4px;'>&emsp;&vellip;</td>
              <td style='border-right: solid silver'>&nbsp;</td>
            </tr>
            <tr>
              <td style='border-bottom: solid silver; border-left: solid silver'>&nbsp;</td>
              <td style='padding: 0px 4px 0px 4px;'><i>Pv</i><sub>m</sub></td>
              <td style='border-bottom: solid silver; border-right: solid silver'>&nbsp;</td>
              <td style='padding-left: 2px;'>;</td>
            </tr>
          </table>
          &emsp;&emsp;where:<br>
          &emsp;&emsp;&emsp;<i>Pv</i><sub>i</sub> are Visiting Time Probabilities at time i."
      seeAlso <- "alsoMCVTProbabilityOUP"
    }
    # MCFPTModeMedianMean ----
    else if(infobutton == "infoMCFPTModeMedianMeanOUP")
    {
      tabName <- "First Passage Time Mode, Median and Mean"
      bodyText <- "If a threshold is lethal or a trap, hitting it or crossing it is irreversible.  Times to an irreversible threshold are First Passage Times.  The Mode is the most likely time.  The Median is when there is a 50% chance the threshold has been hit or crossed.  The Mean is the expected time to the threshold.  These are calculated in various ways.  The Mode is the maximum of the First Passage Time Density; the Median is the time when the First Passage Time Probability equals 0.5; and the Mean is the average of times to the threshold.<br><br>
          &emsp;&emsp;The R6 method:<br>
          &emsp;&emsp;&emsp;FirstPassageTimeModeMedianMean(<i>t,k,x,rho,mu,sigma,paths,skip</i>)<br>
          &emsp;&emsp;with arguments:<br>
          &emsp;&emsp;&emsp;<i>t</i> is a vector of forward times;<br>
          &emsp;&emsp;&emsp;<i>k</i> is a reversible threshold;<br>
          &emsp;&emsp;&emsp;<i>x</i> is the fixed initial state;<br>
          &emsp;&emsp;&emsp;<i>rho</i> is the rate parameter;<br>
          &emsp;&emsp;&emsp;<i>mu</i> is the location parameter;<br>
          &emsp;&emsp;&emsp;<i>sigma</i> is the scale parameter;<br>
          &emsp;&emsp;&emsp;<i>paths</i> is the number of paths;<br>
          &emsp;&emsp;&emsp;<i>skip</i> subdivides the time interval but reports at times t;<br>
          &emsp;&emsp;returns:<br>
          <table style='margin-left: 60px;'>
            <tr>
              <td style='border-top: solid silver; border-left: solid silver'>&nbsp;</td>
              <td style='padding: 0px 4px 0px 4px;'><i>t</i><sub>mode</sub></td>
              <td style='padding: 0px 4px 0px 4px;'><i>pf</i><sub>mode</sub></td>
              <td style='padding: 0px 4px 0px 4px;'><i>Pf</i><sub>mode</sub></td>
              <td style='border-top: solid silver; border-right: solid silver'>&nbsp;</td>
            </tr>
            <tr>
              <td style='border-left: solid silver'>&nbsp;</td>
              <td style='padding: 0px 4px 0px 4px;'><i>t</i><sub>median</sub></td>
              <td style='padding: 0px 4px 0px 4px;'><i>pf</i><sub>median</sub></td>
              <td style='padding: 0px 4px 0px 4px;'><i>Pf</i><sub>median</sub></td>
              <td style='border-right: solid silver'>&nbsp;</td>
            </tr>
            <tr>
              <td style='border-bottom: solid silver; border-left: solid silver'>&nbsp;</td>
              <td style='padding: 0px 4px 0px 4px;'><i>t</i><sub>mean</sub></td>
              <td style='padding: 0px 4px 0px 4px;'><i>pf</i><sub>mean</sub></td>
              <td style='padding: 0px 4px 0px 4px;'><i>Pf</i><sub>mean</sub></td>
              <td style='border-bottom: solid silver; border-right: solid silver'>&nbsp;</td>
              <td style='padding-left: 2px;'>;</td>
            </tr>
          </table>
          &emsp;&emsp;where:<br>
          &emsp;&emsp;&emsp;<i>t</i> are times;<br>
          &emsp;&emsp;&emsp;<i>pf</i> are First Passage Time Densities;<br>
          &emsp;&emsp;&emsp;<i>Pf</i> are First Passage Time Probabilities."
      seeAlso <- "alsoMCFPTModeMedianMeanOUP"
    }
    # MCFPTPercentiles ----
    else if(infobutton == "infoMCFPTPercentilesOUP")
    {
      tabName <- "First Passage Time Percentiles"
      bodyText <- "As with Visiting Times, First Passage Time Densities and Probabilities are skewed with three measures of central tendency, the Mode, Median and Mean.  A practical measure of dispersion is Percentiles.  The Median is the 50<sup>th</sup> Percentile and higher and lower Percentiles are evenly dispersed around the Median.  Percentiles are easily calculated.  There is at most one First Passage Time for a Bounded Path and we can sort the times from soonest to latest and count them.<br><br>
          &emsp;&emsp;The R6 method:<br>
          &emsp;&emsp;&emsp;FirstPassageTimePercentiles(<i>t,k,x,Ppct,rho,mu,sigma,paths,skip</i>)<br>
          &emsp;&emsp;with arguments:<br>
          &emsp;&emsp;&emsp;<i>t</i> is a vector of forward times;<br>
          &emsp;&emsp;&emsp;<i>k</i> is a reversible threshold;<br>
          &emsp;&emsp;&emsp;<i>x</i> is the fixed initial state;<br>
          &emsp;&emsp;&emsp;<i>Ppct</i> is a probability for a percentile;<br>
          &emsp;&emsp;&emsp;<i>rho</i> is the rate parameter;<br>
          &emsp;&emsp;&emsp;<i>mu</i> is the location parameter;<br>
          &emsp;&emsp;&emsp;<i>sigma</i> is the scale parameter;<br>
          &emsp;&emsp;&emsp;<i>paths</i> is the number of paths;<br>
          &emsp;&emsp;&emsp;<i>skip</i> subdivides the time interval but reports at times t;<br>
          &emsp;&emsp;returns:<br>
          <table style='margin-left: 60px;'>
            <tr>
              <td style='border-top: solid silver; border-left: solid silver'>&nbsp;</td>
              <td style='padding: 0px 4px 0px 4px;'><i>t</i><sub>1-Ppct</sub></td>
              <td style='padding: 0px 4px 0px 4px;'><i>pf</i><sub>1-Ppct</sub></td>
              <td style='padding: 0px 4px 0px 4px;'><i>Pf</i><sub>1-Ppct</sub></td>
              <td style='border-top: solid silver; border-right: solid silver'>&nbsp;</td>
            </tr>
            <tr>
              <td style='border-left: solid silver'>&nbsp;</td>
              <td style='padding: 0px 4px 0px 4px;'><i>t</i><sub>0.5</sub></td>
              <td style='padding: 0px 4px 0px 4px;'><i>pf</i><sub>0.5</sub></td>
              <td style='padding: 0px 4px 0px 4px;'><i>Pf</i><sub>0.5</sub></td>
              <td style='border-right: solid silver'>&nbsp;</td>
            </tr>
            <tr>
              <td style='border-bottom: solid silver; border-left: solid silver'>&nbsp;</td>
              <td style='padding: 0px 4px 0px 4px;'><i>t</i><sub>Ppct</sub></td>
              <td style='padding: 0px 4px 0px 4px;'><i>pf</i><sub>Ppct</sub></td>
              <td style='padding: 0px 4px 0px 4px;'><i>Pf</i><sub>Ppct</sub></td>
              <td style='border-bottom: solid silver; border-right: solid silver'>&nbsp;</td>
              <td style='padding-left: 2px;'>;</td>
            </tr>
          </table>
          &emsp;&emsp;where:<br>
          &emsp;&emsp;&emsp;<i>t</i> are times;<br>
          &emsp;&emsp;&emsp;<i>pf</i> are First Passage Time Densities;<br>
          &emsp;&emsp;&emsp;<i>Pf</i> are First Passage Time Probabilities."
      seeAlso <- "alsoMCFPTPercentilesOUP"
    }
    # MCFPTDensity ----
    else if(infobutton == "infoMCFPTDensityOUP")
    {
      tabName <- "First Passage Time Density"
      bodyText <- "A First Passage Time Density is the additional proportion of Bounded Paths which cross an irreversible threshold.  It is easily calculated by binning the First Passage Times and dividing by the number of Bounded Paths.<br><br>
          &emsp;&emsp;The R6 method:<br>
          &emsp;&emsp;&emsp;First PassageTimeDensity(<i>t,k,x,rho,mu,sigma,paths,skip</i>)<br>
          &emsp;&emsp;with arguments:<br>
          &emsp;&emsp;&emsp;<i>t</i> is a vector of forward times;<br>
          &emsp;&emsp;&emsp;<i>k</i> is a reversible threshold;<br>
          &emsp;&emsp;&emsp;<i>x</i> is the fixed initial state;<br>
          &emsp;&emsp;&emsp;<i>rho</i> is the rate parameter;<br>
          &emsp;&emsp;&emsp;<i>mu</i> is the location parameter;<br>
          &emsp;&emsp;&emsp;<i>sigma</i> is the scale parameter;<br>
          &emsp;&emsp;&emsp;<i>paths</i> is the number of paths;<br>
          &emsp;&emsp;&emsp;<i>skip</i> subdivides the time interval but reports at times t;<br>
          &emsp;&emsp;returns:<br>
          <table style='margin-left: 60px;'>
            <tr>
              <td style='border-top: solid silver; border-left: solid silver'>&nbsp;</td>
              <td style='padding: 0px 4px 0px 4px;'><i>pf</i><sub>1</sub></td>
              <td style='border-top: solid silver; border-right: solid silver'>&nbsp;</td>
            </tr>
            <tr>
              <td style='border-left: solid silver'>&nbsp;</td>
              <td style='padding: 0px 4px 0px 4px;'>&emsp;&vellip;</td>
              <td style='border-right: solid silver'>&nbsp;</td>
            </tr>
            <tr>
              <td style='border-bottom: solid silver; border-left: solid silver'>&nbsp;</td>
              <td style='padding: 0px 4px 0px 4px;'><i>pf</i><sub>m</sub></td>
              <td style='border-bottom: solid silver; border-right: solid silver'>&nbsp;</td>
              <td style='padding-left: 2px;'>;</td>
            </tr>
          </table>
          &emsp;&emsp;where:<br>
          &emsp;&emsp;&emsp;<i>pf</i><sub>i</sub> are First Passage Time Densities at time i."
      seeAlso <- "alsoMCFPTDensityOUP"
    }
    # MCFPTProbability ----
    else if(infobutton == "infoMCFPTProbabilityOUP")
    {
      tabName <- "First Passage Time Probability"
      bodyText <- "A First Passage Time Probability is the proportion of Bounded Paths which have crossed an irreversible threshold.  The proportion which have yet to cross is sometimes called the Survival Probability.  Therefore, the First Passage Time Probability is the proportion which haven't survived.  It is easily calculated by summing the First Passage Time Densities, but can also be calculated as one minus the Survival Probability.<br><br>
          &emsp;&emsp;The R6 method:<br>
          &emsp;&emsp;&emsp;FirstPassageTimeProbability(<i>t,k,x,rho,mu,sigma,paths,skip</i>)<br>
          &emsp;&emsp;with arguments:<br>
          &emsp;&emsp;&emsp;<i>t</i> is a vector of forward times;<br>
          &emsp;&emsp;&emsp;<i>k</i> is a reversible threshold;<br>
          &emsp;&emsp;&emsp;<i>x</i> is the fixed initial state;<br>
          &emsp;&emsp;&emsp;<i>rho</i> is the rate parameter;<br>
          &emsp;&emsp;&emsp;<i>mu</i> is the location parameter;<br>
          &emsp;&emsp;&emsp;<i>sigma</i> is the scale parameter;<br>
          &emsp;&emsp;&emsp;<i>paths</i> is the number of paths;<br>
          &emsp;&emsp;&emsp;<i>skip</i> subdivides the time interval but reports at times t;<br>
          &emsp;&emsp;returns:<br>
          <table style='margin-left: 60px;'>
            <tr>
              <td style='border-top: solid silver; border-left: solid silver'>&nbsp;</td>
              <td style='padding: 0px 4px 0px 4px;'><i>Pf</i><sub>1</sub></td>
              <td style='border-top: solid silver; border-right: solid silver'>&nbsp;</td>
            </tr>
            <tr>
              <td style='border-left: solid silver'>&nbsp;</td>
              <td style='padding: 0px 4px 0px 4px;'>&emsp;&vellip;</td>
              <td style='border-right: solid silver'>&nbsp;</td>
            </tr>
            <tr>
              <td style='border-bottom: solid silver; border-left: solid silver'>&nbsp;</td>
              <td style='padding: 0px 4px 0px 4px;'><i>Pf</i><sub>m</sub></td>
              <td style='border-bottom: solid silver; border-right: solid silver'>&nbsp;</td>
              <td style='padding-left: 2px;'>;</td>
            </tr>
          </table>
          &emsp;&emsp;where:<br>
          &emsp;&emsp;&emsp;<i>Pf</i><sub>i</sub> are First Passage Time Probabilities at time i."
      seeAlso <- "alsoMCFPTProbabilityOUP"
    }
    # About ----
    else if(infobutton == "tabAboutOUP")
    {
      condcomp <- "Rcpp only"
      if(RcppParallelInstalled())
      {
        if(RcppsitmoInstalled()) { condcomp <- "Rcpp and RcppParallel with sitmo" }
        else { condcomp <- "Rcpp and RcppParallel without sitmo" }
      }
      tabName <- "Real Options for Adoption and Resilience"
      bodyText <- paste0("Description:  R Shiny implementation of the R6 objects, OUProcess, Analytical, FiniteDifference, MaximumLikelihood and MonteCarlo&mdash;a complete set of functions for maximum likelihood estimation and the calculation of probabilities, option prices, decision thresholds, visiting times, first passage times and more&mdash;everything for a real options analysis.<br><br>
          Version:  1.4.5.0 (stochastic process.modules.help.build)<br>
          Conditional Compilation:  ",condcomp,"<br>
          License:  GPLv3<br><br>
          Author:  Greg Hertzler<br>
          email:  ghertzlerau@gmail.com<br>
          Roles:  author, creator<br>
          ORCID:  0000-0003-3123-7898<br><br>
          Author:  Tim Capon<br>
          email:  Tim.Capon@csiro.au<br>
          Roles:  contributor<br><br>
          Citation:<br>
          Hertzler, Greg, Capon, Tim. (2026). <i>Real Options for Adoption and Resilience</i> (Version 1.4.5.0) [Computer software]. Publisher. https:<wbr/>//real-options-<wbr/>adoption-<wbr/>resilience.<wbr/>shinyapps.io/<wbr/>GregsOUPShiny/<br><br>
          Project support:<br>
          &mdash;resources and expertise provided by CSIRO IMT Scientific Computing;<br>
          &mdash;resources provided by CSIRO Environment.")
    }
    # License ----
    else if(infobutton == "tabLicenseOUP")
    {
      tabName <- "GNU General Public Licence version 3 (GPLv3)"
      bodyText <- "This software is copyright (c) Greg Hertzler<br><br>
          Except where otherwise indicated, the copyright holder grants you a licence to the Software on the terms of the GNU General Public Licence version 3 (GPLv3), distributed at: http://www.gnu.org/licenses/gpl.html."
    }
    # end ----
    if(tabName != "")
    {
      if(seeAlso == "")
      {
        content <- modalDialog(
          title=div(img(src="Roar32x32.png"),tabName),
          div(
            style = "max-height: 580px; overflow-y: auto;",
            HTML(bodyText),
          ),
          footer = modalButton("Close"),
          easyClose = TRUE,
          size = "xl"
        )
      }
      else
      {
        content <- modalDialog(
          title=div(img(src="Roar32x32.png"),tabName),
          div(
            style = "max-height: 580px; overflow-y: auto;",
            HTML(bodyText),
          ),
          footer = tagList(actionButton(seeAlso,"See Also",class="btn-primary",title="other tabs"),modalButton("Close")),
          easyClose = TRUE,
          size = "xl"
        )
      }
    }
    else
    {
      content <- modalDialog(
        title=div(img(src="Roar32x32.png"),"Real Options for Adoption and Resilience"),
        div(
          style = "max-height: 580px; overflow-y: auto;",
          HTML(bodyText),
        ),
        footer = modalButton("Close"),
        easyClose = TRUE,
        size = "xl"
      )
    }
    showModal(content,session)
  }) %>% bindEvent(infotoggle())
})
