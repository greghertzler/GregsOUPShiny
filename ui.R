library(shiny)
library(bslib)
library(shinybusy)
library(plotly)

# ui
shinyUI(
  page_navbar(title=img(src="Roar32x32.png",alt="ROAR"),
    theme=bs_theme(bootswatch="spacelab",bg="#ddeeff",fg="#001122",success="#11aa88"),
    nav_menu("Ornstein-Uhlenbeck Process",
      nav_panel("Real Options",
        tags$head(HTML('<html lang="en"> <link rel="icon" href="favicon.png" type="image/png" sizes="16x16">'),
                  tags$link(rel="stylesheet",type="text/css",href="styles.css")),
        tags$script(src="script.js"),
        add_busy_spinner(spin="swapping-squares",color="rgb(115,33,38)",timeout=500,position=c("top-right"),margins=c(500,200),height="128px",width="128px"),
        navset_pill_list(
          # Data ----
          nav_panel("Data",
            # file, time and state
            fixedRow(
              column(actionButton("fileinfoRODataOUP","i",width="100%",class="btn-primary"),title="File info",style="padding-right: 2px; padding-top: 32px;",width=1),
              column(selectInput("filesRODataOUP",label="File",choices=""),title="data files",width=5),
              column(selectInput("timeRODataOUP",label="Time",choices=""),title="time variable",width=3),
              column(selectInput("stateRODataOUP",label="State",choices=""),title="state variable",width=3)
            ),
            # first and last times, number of rows and columns in data
            fixedRow(
              column(fileInput("filesROUploadOUP",NULL,multiple=FALSE,accept=".csv",buttonLabel="...",placeholder="Select a file to upload..."),title="upload a data file",width=6),
              column(wellPanel(class="wellTableOUP",style="padding: 0px; width=100%;",uiOutput("descrRODataOUP")),width=6)
            ),
            # buttons, begin and end dates
            fixedRow(
              column(actionButton("resetRODataOUP","Reset",width="100%",class="btn-success"),title="reset begin and end",style="padding-top: 32px;",width=2),
              column(numericInput("begRODataOUP",label="Begin",value="",step="any",width="100%"),title="time to begin plot",width=3),
              column(numericInput("endRODataOUP",label="End",value="",step="any",width="100%"),title="time to end plot",width=3),
              column(actionButton("plotRODataOUP","Plot",width="100%",class="btn-success"),title="refresh",style="padding-top: 32px;",width=2),
              column(actionButton("infoRODataOUP","Info",width="100%",class="btn-primary"),title="about Data",style="padding-top: 32px;",width=2)
            ),
            # plot
            fluidRow(
              wellPanel(class="wellPlotOUP",style="height: 402px;",plotlyOutput("plotlyRODataOUP"))
            ),
            value="RODataOUP"
          ),
          # Estimates ----
          nav_panel("Estimates",
            # file, time and state
            fixedRow(
              column(actionButton("fileinfoROEstimatesOUP","i",width="100%",class="btn-primary"),title="File info",style="padding-right: 2px; padding-top: 32px;",width=1),
              column(selectInput("filesROEstimatesOUP",label="File",choices=""),title="data files",width=5),
              column(selectInput("timeROEstimatesOUP",label="Time",choices=""),title="time variable",width=3),
              column(selectInput("stateROEstimatesOUP",label="State",choices=""),title="state variable",width=3)
            ),
            # parameters
            fixedRow(
              column(width=2),
              column(wellPanel(class="wellTableOUP",style="padding: 0px; width: 100%;",uiOutput("paramROEstimatesOUP")),width=8)
            ),
            # buttons, begin and end dates
            fixedRow(
              column(actionButton("resetROEstimatesOUP","Reset",width="100%",class="btn-success"),title="reset begin and end",style="padding-top: 32px;",width=2),
              column(numericInput("begROEstimatesOUP",label="Begin",value="",step="any",width="100%"),title="time to begin plot",width=3),
              column(numericInput("endROEstimatesOUP",label="End",value="",step="any",width="100%"),title="time to end plot",width=3),
              column(actionButton("plotROEstimatesOUP","Go",width="100%",class="btn-success"),title="estimate and plot",style="padding-top: 32px;",width=2),
              column(actionButton("infoROEstimatesOUP","Info",width="100%",class="btn-primary"),title="about Estimates",style="padding-top: 32px;",width=2)
            ),
            # plot
            fluidRow(
              wellPanel(class="wellPlotOUP",style="height: 402px;",plotlyOutput("plotlyROEstimatesOUP"))
            ),
            value="ROEstimatesOUP"
          ),
          # Regime ----
          nav_panel("Regime",
            # User input
            fixedRow(style="height: 60px;",
              column(actionButton("infoRORegimeOUP","Info",width="100%",class="btn-primary"),title="about Regimes",style="padding-top: 32px;",width=2),
              column(width=2),
              column(numericInput("xFromRORegimeOUP",label="x:From",value="",step="any",width="100%"),title="stochastic states",width=2),
              column(numericInput("xToRORegimeOUP",label="x:To",value="",step="any",width="100%"),title="stochastic states",width=2),
              column(numericInput("xByRORegimeOUP",label="x:By",value="",step="any",width="100%"),title="state increment",width=2)
            ),
            fixedRow(style="height: 60px;",
              column(width=2),
              column(numericInput("rhoRORegimeOUP",label="rho",value="",step="any",width="100%"),title="rate of convergence",width=2),
              column(numericInput("muRORegimeOUP",label="mu",value="",step="any",width="100%"),title="location",width=2),
              column(numericInput("sigmaRORegimeOUP",label="sigma",value="",step="any",width="100%"),title="scale",width=2)
            ),
            fixedRow(style="height: 68px;",
              column(width=2),
              column(numericInput("yRORegimeOUP",label="y",value="",step="any",width="100%"),title="fixed terminal state",width=2),
              column(numericInput("rRORegimeOUP",label="r",value="",step="any",width="100%"),title="discount rate",width=2),
              column(numericInput("phiRORegimeOUP",label="phi",value="",step="any",width="100%"),title="exit or entry option",width=2),
              column(numericInput("bRORegimeOUP",label="b",value="",step="any",width="100%"),title="entry benefit",width=2),
              column(numericInput("cRORegimeOUP",label="c",value="",step="any",width="100%"),title="exit cost",width=2)
            ),
            # User action
            fixedRow(
              column(actionButton("clearRORegimeOUP",HTML("&forall;"),width="100%",class="btn-info"),title="clear and save",style="padding-right: 2px;",width=1),
              column(actionButton("saveRORegimeOUP",HTML("&Vee;"),width="100%",class="btn-info"),title="save",style="padding-left: 2px;",width=1),
              column(actionButton("undoRORegimeOUP",HTML("&Wedge;"),width="100%",class="btn-success"),title="undo",width=2),
              column(width=2),
              column(actionButton("axesRORegimeOUP","Axes",width="100%",class="btn-success"),title="for x",width=2),
              column(actionButton("plotRORegimeOUP","Plot",width="100%",class="btn-success"),title="refresh",width=2),
              column(actionButton("leftRORegimeOUP","<",width="100%",class="btn-success"),title="previous",style="padding-right: 2px;",width=1),
              column(actionButton("rghtRORegimeOUP",">",width="100%",class="btn-success"),title="next",style="padding-left: 2px;",width=1)
            ),
            # Plot
            wellPanel(class="wellPlotOUP",
              style="margin: 0 auto; height: 402px; width: 580px;",plotlyOutput("plotlyRORegimeOUP")
            ),
            value="RORegimeOUP"
          ),
          # Decision Threshold ----
          nav_panel("Decision Threshold",
            # User input
            fixedRow(style="height: 60px;",
              column(actionButton("infoRODecisionOUP","Info",width="100%",class="btn-primary"),title="about Decision Threshold",style="padding-top: 32px;",width=2),
              column(width=2),
              column(numericInput("xFromRODecisionOUP",label="x:From",value="",step="any",width="100%"),title="stochastic states",width=2),
              column(numericInput("xToRODecisionOUP",label="x:To",value="",step="any",width="100%"),title="stochastic states",width=2),
              column(numericInput("xByRODecisionOUP",label="x:By",value="",step="any",width="100%"),title="state increment",width=2)
            ),
            fixedRow(style="height: 60px;",
              column(width=2),
              column(numericInput("rhoRODecisionOUP",label="rho",value="",step="any",width="100%"),title="rate of convergence",width=2),
              column(numericInput("muRODecisionOUP",label="mu",value="",step="any",width="100%"),title="location",width=2),
              column(numericInput("sigmaRODecisionOUP",label="sigma",value="",step="any",width="100%"),title="scale",width=2)
            ),
            fixedRow(style="height: 68px;",
              column(width=2),
              column(numericInput("yRODecisionOUP",label="y",value="",step="any",width="100%"),title="fixed terminal state",width=2),
              column(numericInput("rRODecisionOUP",label="r",value="",step="any",width="100%"),title="discount rate of convergence",width=2),
              column(numericInput("phiRODecisionOUP",label="phi",value="",step="any",width="100%"),title="exit or entry option",width=2),
              column(numericInput("bRODecisionOUP",label="b",value="",step="any",width="100%"),title="entry benefit",width=2),
              column(numericInput("cRODecisionOUP",label="c",value="",step="any",width="100%"),title="exit cost",width=2)
            ),
            # User action
            fixedRow(
              column(actionButton("clearRODecisionOUP",HTML("&forall;"),width="100%",class="btn-info"),title="clear and save",style="padding-right: 2px;",width=1),
              column(actionButton("saveRODecisionOUP",HTML("&Vee;"),width="100%",class="btn-info"),title="save",style="padding-left: 2px;",width=1),
              column(actionButton("undoRODecisionOUP",HTML("&Wedge;"),width="100%",class="btn-success"),title="undo",width=2),
              column(actionButton("syncRODecisionOUP","Sync",width="100%",class="btn-success"),title="states and thresholds",width=2),
              column(actionButton("axesRODecisionOUP","Axes",width="100%",class="btn-success"),title="for x",width=2),
              column(actionButton("plotRODecisionOUP","Plot",width="100%",class="btn-success"),title="refresh",width=2),
              column(actionButton("leftRODecisionOUP","<",width="100%",class="btn-success"),title="previous",style="padding-right: 2px;",width=1),
              column(actionButton("rghtRODecisionOUP",">",width="100%",class="btn-success"),title="next",style="padding-left: 2px;",width=1)
            ),
            # Plot
            wellPanel(class="wellPlotOUP",
              style="margin: 0 auto; height: 402px; width: 580px;",plotlyOutput("plotlyRODecisionOUP")
            ),
            value="RODecisionOUP"
          ),
          # Passage Time ----
          nav_panel("Passage Times",
            # User input
            fixedRow(style="height: 60px;",
              column(actionButton("infoROPassageTimeOUP","Info",width="100%",class="btn-primary"),title="about Passage Time",style="padding-top: 32px;",width=2),
              column(width=2),
              column(numericInput("zFromROPassageTimeOUP",label="z:From",value="",step="any",width="100%"),title="alternate initial states",width=2),
              column(numericInput("zToROPassageTimeOUP",label="z:To",value="",step="any",width="100%"),title="alternate initial states",width=2),
              column(numericInput("zByROPassageTimeOUP",label="z:By",value="",step="any",width="100%"),title="state increment",width=2)
            ),
            fixedRow(style="height: 60px;",
              column(width=2),
              column(numericInput("rhoROPassageTimeOUP",label="rho",value="",step="any",width="100%"),title="rate of convergence",width=2),
              column(numericInput("muROPassageTimeOUP",label="mu",value="",step="any",width="100%"),title="location",width=2),
              column(numericInput("sigmaROPassageTimeOUP",label="sigma",value="",step="any",width="100%"),title="scale",width=2)
            ),
            fixedRow(style="height: 68px;",
              column(width=2),
              column(numericInput("kROPassageTimeOUP",label="k",value="",step="any",width="100%"),title="threshold",width=2),
              column(numericInput("sROPassageTimeOUP",label="s",value="",step="any",width="100%"),title="fixed initial time",width=2),
              column(numericInput("xROPassageTimeOUP",label="x",value="",step="any",width="100%"),title="fixed initial state",width=2),
              column(numericInput("omegaROPassageTimeOUP",label="omega",value="",step="any",width="100%"),title="degree of irreversibility",width=2),
              column(numericInput("PpctROPassageTimeOUP",label="P pct",value="",step="any",width="100%"),title="passage time probability",width=2)
            ),
            # User action
            fixedRow(
              column(actionButton("clearROPassageTimeOUP",HTML("&forall;"),width="100%",class="btn-info"),title="clear and save",style="padding-right: 2px;",width=1),
              column(actionButton("saveROPassageTimeOUP",HTML("&Vee;"),width="100%",class="btn-info"),title="save",style="padding-left: 2px;",width=1),
              column(actionButton("undoROPassageTimeOUP",HTML("&Wedge;"),width="100%",class="btn-success"),title="undo",width=2),
              column(actionButton("syncROPassageTimeOUP","Sync",width="100%",class="btn-success"),title="states and thresholds",width=2),
              column(actionButton("axesROPassageTimeOUP","Axes",width="100%",class="btn-success"),title="for t and z",width=2),
              column(actionButton("plotROPassageTimeOUP","Plot",width="100%",class="btn-success"),title="refresh",width=2),
              column(actionButton("leftROPassageTimeOUP","<",width="100%",class="btn-success"),title="previous",style="padding-right: 2px;",width=1),
              column(actionButton("rghtROPassageTimeOUP",">",width="100%",class="btn-success"),title="next",style="padding-left: 2px;",width=1)
            ),
            # Plot
            wellPanel(class="wellPlotOUP",
              style="margin: 0 auto; height: 402px; width: 580px;",plotlyOutput("plotlyROPassageTimeOUP")
            ),
            value="ROPassageTimeOUP"
          ),
          id="navROOUP",widths=c(3,9)
        ),
        value="tabROOUP",
        #end list ----
      ),
      nav_panel("Analytical",
        tags$head(HTML('<html lang="en"> <link rel="icon" href="favicon.png" type="image/png" sizes="16x16">'),
                  tags$link(rel="stylesheet",type="text/css",href="styles.css")),
        add_busy_spinner(spin="radar",color="rgb(0,90,46)",timeout=500,position=c("top-right"),margins=c(500,200),height="128px",width="128px"),
        navset_pill_list(
          # Drift ----
          nav_panel("Drift",
            # User input
            fixedRow(style="height: 60px;",
              column(actionButton("infoADriftOUP","Info",width="100%",class="btn-primary"),title="about Drift",style="padding-top: 32px;",width=2),
              column(width=2),
              column(numericInput("zFromADriftOUP",label="z:From",value="",step="any",width="100%"),title="stochastic states",width=2),
              column(numericInput("zToADriftOUP",label="z:To",value="",step="any",width="100%"),title="stochastic states",width=2),
              column(numericInput("zByADriftOUP",label="z:By",value="",step="any",width="100%"),title="state increment",width=2)
            ),
            fixedRow(style="height: 144px;",
              column(width=2),
              column(numericInput("rhoADriftOUP",label="rho",value="",step="any",width="100%"),title="rate of convergence",width=2),
              column(numericInput("muADriftOUP",label="mu",value="",step="any",width="100%"),title="location",width=2)
            ),
            # User action
            fixedRow(
              column(actionButton("clearADriftOUP",HTML("&forall;"),width="100%",class="btn-info"),title="clear and save",style="padding-right: 2px;",width=1),
              column(actionButton("saveADriftOUP",HTML("&Vee;"),width="100%",class="btn-info"),title="save",style="padding-left: 2px;",width=1),
              column(actionButton("undoADriftOUP",HTML("&Wedge;"),width="100%",class="btn-success"),title="undo",width=2),
              column(actionButton("syncADriftOUP","Sync",width="100%",class="btn-success"),title="states and thresholds",width=2),
              column(actionButton("axesADriftOUP","Axes",width="100%",class="btn-success"),title="for z",width=2),
              column(actionButton("plotADriftOUP","Plot",width="100%",class="btn-success"),title="refresh",width=2)
            ),
            # Plot
            wellPanel(class="wellPlotOUP",
              style="margin: 0 auto; height: 402px; width: 580px;",plotlyOutput("plotlyADriftOUP")
            ),
            value="ADriftOUP"
          ),
          # Diffusion ----
          nav_panel("Diffusion",
            # User input
            fixedRow(style="height: 60px;",
              column(actionButton("infoADiffusionOUP","Info",width="100%",class="btn-primary"),title="about Diffusion",style="padding-top: 32px;",width=2),
              column(width=2),
              column(numericInput("zFromADiffusionOUP",label="z:From",value="",step="any",width="100%"),title="stochastic states",width=2),
              column(numericInput("zToADiffusionOUP",label="z:To",value="",step="any",width="100%"),title="stochastic states",width=2),
              column(numericInput("zByADiffusionOUP",label="z:By",value="",step="any",width="100%"),title="state increment",width=2)
            ),
            fixedRow(style="height: 144px;",
              column(width=2),
              column(numericInput("rhoADiffusionOUP",label="rho",value="",step="any",width="100%"),title="rate of convergence",width=2),
              column(numericInput("muADiffusionOUP",label="mu",value="",step="any",width="100%"),title="location",width=2),
              column(numericInput("sigmaADiffusionOUP",label="sigma",value="",step="any",width="100%"),title="scale",width=2)
            ),
            # User action
            fixedRow(
              column(actionButton("clearADiffusionOUP",HTML("&forall;"),width="100%",class="btn-info"),title="clear and save",style="padding-right: 2px;",width=1),
              column(actionButton("saveADiffusionOUP",HTML("&Vee;"),width="100%",class="btn-info"),title="save",style="padding-left: 2px;",width=1),
              column(actionButton("undoADiffusionOUP",HTML("&Wedge;"),width="100%",class="btn-success"),title="undo",width=2),
              column(actionButton("syncADiffusionOUP","Sync",width="100%",class="btn-success"),title="states and thresholds",width=2),
              column(actionButton("axesADiffusionOUP","Axes",width="100%",class="btn-success"),title="for z",width=2),
              column(actionButton("plotADiffusionOUP","Plot",width="100%",class="btn-success"),title="refresh",width=2),
              column(actionButton("leftADiffusionOUP","<",width="100%",class="btn-success"),title="previous",style="padding-right: 2px;",width=1),
              column(actionButton("rghtADiffusionOUP",">",width="100%",class="btn-success"),title="next",style="padding-left: 2px;",width=1)
            ),
            # Plot
            wellPanel(class="wellPlotOUP",
              style="margin: 0 auto; height: 402px; width: 580px;",plotlyOutput("plotlyADiffusionOUP")
            ),
            value="ADiffusionOUP"
          ),
          # Mean ----
          nav_panel("Mean",
            # User input
            fixedRow(style="height: 60px;",
              column(actionButton("infoAMeanOUP","Info",width="100%",class="btn-primary"),title="about Mean",style="padding-top: 32px;",width=2),
              column(width=2),
              column(numericInput("yFromAMeanOUP",label="y:From",value="",step="any",width="100%"),title="stochastic states",width=2),
              column(numericInput("yToAMeanOUP",label="y:To",value="",step="any",width="100%"),title="stochastic states",width=2),
              column(numericInput("yByAMeanOUP",label="y:By",value="",step="any",width="100%"),title="state increment",width=2),
              column(numericInput("tByAMeanOUP",label="t:By",value="",step="any",width="100%"),title="time increment",width=2)
            ),
            fixedRow(style="height: 60px;",
              column(width=2),
              column(numericInput("rhoAMeanOUP",label="rho",value="",step="any",width="100%"),title="rate of convergence",width=2),
              column(numericInput("muAMeanOUP",label="mu",value="",step="any",width="100%"),title="location",width=2),
              column(numericInput("sigmaAMeanOUP",label="sigma",value="",step="any",width="100%"),title="scale",width=2),
              column(numericInput("pmaxAMeanOUP",label="p max",value="",step="any",width="100%"),title="maximum density",width=2),
              column(numericInput("tToAMeanOUP",label="t:To",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            fixedRow(style="height: 68px;",
              column(numericInput("sAMeanOUP",label="s",value="",step="any",width="100%"),title="fixed initial time",width=2),
              column(numericInput("xAMeanOUP",label="x",value="",step="any",width="100%"),title="fixed initial state",width=2),
              column(width=2),
              column(numericInput("psiAMeanOUP",label="psi",value="",step="any",width="100%"),title="-inf to y or y to inf",width=2),
              column(width=2),
              column(numericInput("tFromAMeanOUP",label="t:From",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            # User action
            fixedRow(
              column(actionButton("clearAMeanOUP",HTML("&forall;"),width="100%",class="btn-info"),title="clear and save",style="padding-right: 2px;",width=1),
              column(actionButton("saveAMeanOUP",HTML("&Vee;"),width="100%",class="btn-info"),title="save",style="padding-left: 2px;",width=1),
              column(actionButton("undoAMeanOUP",HTML("&Wedge;"),width="100%",class="btn-success"),title="undo",width=2),
              column(actionButton("syncAMeanOUP","Sync",width="100%",class="btn-success"),title="states and thresholds",width=2),
              column(actionButton("axesAMeanOUP","Axes",width="100%",class="btn-success"),title="for t, y and p",width=2),
              column(actionButton("plotAMeanOUP","Plot",width="100%",class="btn-success"),title="refresh",width=2),
              column(actionButton("leftAMeanOUP","<",width="100%",class="btn-success"),title="previous",style="padding-right: 2px;",width=1),
              column(actionButton("rghtAMeanOUP",">",width="100%",class="btn-success"),title="next",style="padding-left: 2px;",width=1)
            ),
            # Plot
            wellPanel(class="wellPlotOUP",
              style="margin: 0 auto; height: 402px; width: 580px;",plotlyOutput("plotlyAMeanOUP")
            ),
            value="AMeanOUP"
          ),
          # Mean convergence----
          nav_panel("Mean Convergence",
            # User input
            fixedRow(style="height: 60px;",
              column(actionButton("infoAMeanCOUP","Info",width="100%",class="btn-primary"),title="about Mean Convergence",style="padding-top: 32px;",width=2),
              column(width=2),
              column(width=2),
              column(width=2),
              column(width=2),
              column(numericInput("tByAMeanCOUP",label="t:By",value="",step="any",width="100%"),title="time increment",width=2)
            ),
            fixedRow(style="height: 60px;",
              column(width=2),
              column(numericInput("rhoAMeanCOUP",label="rho",value="",step="any",width="100%"),title="rate of convergence",width=2),
              column(width=2),
              column(width=2),
              column(width=2),
              column(numericInput("tToAMeanCOUP",label="t:To",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            fixedRow(style="height: 68px;",
              column(numericInput("sAMeanCOUP",label="s",value="",step="any",width="100%"),title="fixed initial time",width=2),
              column(numericInput("xAMeanCOUP",label="x",value="",step="any",width="100%"),title="fixed initial state",width=2),
              column(numericInput("epsAMeanCOUP",label="epsilon",value="",step="any",width="100%"),title="proportion remaining",width=2),
              column(width=2),
              column(width=2),
              column(numericInput("tFromAMeanCOUP",label="t:From",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            # User action
            fixedRow(
              column(actionButton("clearAMeanCOUP",HTML("&forall;"),width="100%",class="btn-info"),title="clear and save",style="padding-right: 2px;",width=1),
              column(actionButton("saveAMeanCOUP",HTML("&Vee;"),width="100%",class="btn-info"),title="save",style="padding-left: 2px;",width=1),
              column(actionButton("undoAMeanCOUP",HTML("&Wedge;"),width="100%",class="btn-success"),title="undo",width=2),
              column(actionButton("syncAMeanCOUP","Sync",width="100%",class="btn-success"),title="states and thresholds",width=2),
              column(actionButton("axesAMeanCOUP","Axes",width="100%",class="btn-success"),title="for t, y and p",width=2),
              column(actionButton("plotAMeanCOUP","Plot",width="100%",class="btn-success"),title="refresh",width=2)
            ),
            # Plot
            wellPanel(class="wellPlotOUP",
              style="margin: 0 auto; height: 402px; width: 580px;",plotlyOutput("plotlyAMeanCOUP")
            ),
            value="AMeanCOUP"
          ),
          # Variance ----
          nav_panel("Variance",
            # User input
            fixedRow(style="height: 60px;",
              column(actionButton("infoAVarianceOUP","Info",width="100%",class="btn-primary"),title="about Variance",style="padding-top: 32px;",width=2),
              column(width=2),
              column(numericInput("yFromAVarianceOUP",label="y:From",value="",step="any",width="100%"),title="stochastic states",width=2),
              column(numericInput("yToAVarianceOUP",label="y:To",value="",step="any",width="100%"),title="stochastic states",width=2),
              column(numericInput("yByAVarianceOUP",label="y:By",value="",step="any",width="100%"),title="state increment",width=2),
              column(numericInput("tByAVarianceOUP",label="t:By",value="",step="any",width="100%"),title="time increment",width=2)
            ),
            fixedRow(style="height: 60px;",
              column(width=2),
              column(numericInput("rhoAVarianceOUP",label="rho",value="",step="any",width="100%"),title="rate of convergence",width=2),
              column(numericInput("muAVarianceOUP",label="mu",value="",step="any",width="100%"),title="location",width=2),
              column(numericInput("sigmaAVarianceOUP",label="sigma",value="",step="any",width="100%"),title="scale",width=2),
              column(numericInput("pmaxAVarianceOUP",label="p max",value="",step="any",width="100%"),title="maximum density",width=2),
              column(numericInput("tToAVarianceOUP",label="t:To",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            fixedRow(style="height: 68px;",
              column(numericInput("sAVarianceOUP",label="s",value="",step="any",width="100%"),title="fixed initial time",width=2),
              column(numericInput("xAVarianceOUP",label="x",value="",step="any",width="100%"),title="fixed initial state",width=2),
              column(width=2),
              column(numericInput("psiAVarianceOUP",label="psi",value="",step="any",width="100%"),title="-inf to y or y to inf",width=2),
              column(width=2),
              column(numericInput("tFromAVarianceOUP",label="t:From",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            # User action
            fixedRow(
              column(actionButton("clearAVarianceOUP",HTML("&forall;"),width="100%",class="btn-info"),title="clear and save",style="padding-right: 2px;",width=1),
              column(actionButton("saveAVarianceOUP",HTML("&Vee;"),width="100%",class="btn-info"),title="save",style="padding-left: 2px;",width=1),
              column(actionButton("undoAVarianceOUP",HTML("&Wedge;"),width="100%",class="btn-success"),title="undo",width=2),
              column(actionButton("axesAVarianceOUP","Axes",width="100%",class="btn-success"),title="for t, y and p",width=2),
              column(actionButton("plotAVarianceOUP","Plot",width="100%",class="btn-success"),title="refresh",width=2),
              column(actionButton("leftAVarianceOUP","<",width="100%",class="btn-success"),title="previous",style="padding-right: 2px;",width=1),
              column(actionButton("rghtAVarianceOUP",">",width="100%",class="btn-success"),title="next",style="padding-left: 2px;",width=1)
            ),
            # Plot
            wellPanel(class="wellPlotOUP",
              style="margin: 0 auto; height: 402px; width: 580px;",plotlyOutput("plotlyAVarianceOUP")
            ),
            value="AVarianceOUP"
          ),
          # Variance convergence----
          nav_panel("Variance Convergence",
            # User input
            fixedRow(style="height: 60px;",
              column(actionButton("infoAVarianceCOUP","Info",width="100%",class="btn-primary"),title="about Variance Convergence",style="padding-top: 32px;",width=2),
              column(width=2),
              column(width=2),
              column(width=2),
              column(width=2),
              column(numericInput("tByAVarianceCOUP",label="t:By",value="",step="any",width="100%"),title="time increment",width=2)
            ),
            fixedRow(style="height: 60px;",
              column(width=2),
              column(numericInput("rhoAVarianceCOUP",label="rho",value="",step="any",width="100%"),title="rate of convergence",width=2),
              column(width=2),
              column(numericInput("sigmaAVarianceCOUP",label="sigma",value="",step="any",width="100%"),title="scale",width=2),
              column(width=2),
              column(numericInput("tToAVarianceCOUP",label="t:To",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            fixedRow(style="height: 68px;",
              column(numericInput("sAVarianceCOUP",label="s",value="",step="any",width="100%"),title="fixed initial time",width=2),
              column(width=2),
              column(numericInput("epsAVarianceCOUP",label="epsilon",value="",step="any",width="100%"),title="proportion remaining",width=2),
              column(width=2),
              column(width=2),
              column(numericInput("tFromAVarianceCOUP",label="t:From",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            # User action
            fixedRow(
              column(actionButton("clearAVarianceCOUP",HTML("&forall;"),width="100%",class="btn-info"),title="clear and save",style="padding-right: 2px;",width=1),
              column(actionButton("saveAVarianceCOUP",HTML("&Vee;"),width="100%",class="btn-info"),title="save",style="padding-left: 2px;",width=1),
              column(actionButton("undoAVarianceCOUP",HTML("&Wedge;"),width="100%",class="btn-success"),title="undo",width=2),
              column(actionButton("syncAVarianceCOUP","Sync",width="100%",class="btn-success"),title="states and thresholds",width=2),
              column(actionButton("axesAVarianceCOUP","Axes",width="100%",class="btn-success"),title="for t, y and p",width=2),
              column(actionButton("plotAVarianceCOUP","Plot",width="100%",class="btn-success"),title="refresh",width=2)
            ),
            wellPanel(class="wellPlotOUP",
              style="margin: 0 auto; height: 402px; width: 580px;",plotlyOutput("plotlyAVarianceCOUP")
            ),
            value="AVarianceCOUP"
          ),
          # Transition Density ----
          nav_panel("Transition Density",
            # User input
            fixedRow(style="height: 60px;",
              column(actionButton("infoADensityOUP","Info",width="100%",class="btn-primary"),title="about Transition Density",style="padding-top: 32px;",width=2),
              column(width=2),
              column(numericInput("yFromADensityOUP",label="y:From",value="",step="any",width="100%"),title="stochastic states",width=2),
              column(numericInput("yToADensityOUP",label="y:To",value="",step="any",width="100%"),title="stochastic states",width=2),
              column(numericInput("yByADensityOUP",label="y:By",value="",step="any",width="100%"),title="state increment",width=2),
              column(numericInput("tByADensityOUP",label="t:By",value="",step="any",width="100%"),title="time increment",width=2)
            ),
            fixedRow(style="height: 60px;",
              column(width=2),
              column(numericInput("rhoADensityOUP",label="rho",value="",step="any",width="100%"),title="rate of convergence",width=2),
              column(numericInput("muADensityOUP",label="mu",value="",step="any",width="100%"),title="location",width=2),
              column(numericInput("sigmaADensityOUP",label="sigma",value="",step="any",width="100%"),title="scale",width=2),
              column(numericInput("pmaxADensityOUP",label="p max",value="",step="any",width="100%"),title="maximum density",width=2),
              column(numericInput("tToADensityOUP",label="t:To",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            fixedRow(style="height: 68px;",
              column(numericInput("sADensityOUP",label="s",value="",step="any",width="100%"),title="fixed initial time",width=2),
              column(numericInput("xADensityOUP",label="x",value="",step="any",width="100%"),title="fixed initial state",width=2),
              column(width=2),
              column(width=2),
              column(width=2),
              column(numericInput("tFromADensityOUP",label="t:From",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            # User action
            fixedRow(
              column(actionButton("clearADensityOUP",HTML("&forall;"),width="100%",class="btn-info"),title="clear and save",style="padding-right: 2px;",width=1),
              column(actionButton("saveADensityOUP",HTML("&Vee;"),width="100%",class="btn-info"),title="save",style="padding-left: 2px;",width=1),
              column(actionButton("undoADensityOUP",HTML("&Wedge;"),width="100%",class="btn-success"),title="undo",width=2),
              column(actionButton("syncADensityOUP","Sync",width="100%",class="btn-success"),title="states and thresholds",width=2),
              column(actionButton("axesADensityOUP","Axes",width="100%",class="btn-success"),title="for t, y and p",width=2),
              column(actionButton("plotADensityOUP","Plot",width="100%",class="btn-success"),title="refresh",width=2),
              column(actionButton("leftADensityOUP","<",width="100%",class="btn-success"),title="previous",style="padding-right: 2px;",width=1),
              column(actionButton("rghtADensityOUP",">",width="100%",class="btn-success"),title="next",style="padding-left: 2px;",width=1)
            ),
            # Plot
            wellPanel(class="wellPlotOUP",
              style="margin: 0 auto; height: 402px; width: 580px;",plotlyOutput("plotlyADensityOUP")
            ),
            value="ADensityOUP"
          ),
          # Transition Probability ----
          nav_panel("Transition Probability",
            # User input
            fixedRow(style="height: 60px;",
              column(actionButton("infoAProbabilityOUP","Info",width="100%",class="btn-primary"),title="about Transition Probability",style="padding-top: 32px;",width=2),
              column(width=2),
              column(numericInput("yFromAProbabilityOUP",label="y:From",value="",step="any",width="100%"),title="stochastic states",width=2),
              column(numericInput("yToAProbabilityOUP",label="y:To",value="",step="any",width="100%"),title="stochastic states",width=2),
              column(numericInput("yByAProbabilityOUP",label="y:By",value="",step="any",width="100%"),title="state increment",width=2),
              column(numericInput("tByAProbabilityOUP",label="t:By",value="",step="any",width="100%"),title="time increment",width=2)
            ),
            fixedRow(style="height: 60px;",
              column(width=2),
              column(numericInput("rhoAProbabilityOUP",label="rho",value="",step="any",width="100%"),title="rate of convergence",width=2),
              column(numericInput("muAProbabilityOUP",label="mu",value="",step="any",width="100%"),title="location",width=2),
              column(numericInput("sigmaAProbabilityOUP",label="sigma",value="",step="any",width="100%"),title="scale",width=2),
              column(width=2),
              column(numericInput("tToAProbabilityOUP",label="t:To",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            fixedRow(style="height: 68px;",
              column(numericInput("sAProbabilityOUP",label="s",value="",step="any",width="100%"),title="fixed initial time",width=2),
              column(numericInput("xAProbabilityOUP",label="x",value="",step="any",width="100%"),title="fixed initial state",width=2),
              column(width=2),
              column(numericInput("psiAProbabilityOUP",label="psi",value="",step="any",width="100%"),title="-inf to y or y to inf",width=2),
              column(width=2),
              column(numericInput("tFromAProbabilityOUP",label="t:From",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            # User action
            fixedRow(
              column(actionButton("clearAProbabilityOUP",HTML("&forall;"),width="100%",class="btn-info"),title="clear and save",style="padding-right: 2px;",width=1),
              column(actionButton("saveAProbabilityOUP",HTML("&Vee;"),width="100%",class="btn-info"),title="save",style="padding-left: 2px;",width=1),
              column(actionButton("undoAProbabilityOUP",HTML("&Wedge;"),width="100%",class="btn-success"),title="undo",width=2),
              column(actionButton("syncAProbabilityOUP","Sync",width="100%",class="btn-success"),title="states and thresholds",width=2),
              column(actionButton("axesAProbabilityOUP","Axes",width="100%",class="btn-success"),title="for t, y and p",width=2),
              column(actionButton("plotAProbabilityOUP","Plot",width="100%",class="btn-success"),title="refresh",width=2),
              column(actionButton("leftAProbabilityOUP","<",width="100%",class="btn-success"),title="previous",style="padding-right: 2px;",width=1),
              column(actionButton("rghtAProbabilityOUP",">",width="100%",class="btn-success"),title="next",style="padding-left: 2px;",width=1)
            ),
            # Plot
            wellPanel(class="wellPlotOUP",
              style="margin: 0 auto; height: 402px; width: 580px;",plotlyOutput("plotlyAProbabilityOUP")
            ),
            value="AProbabilityOUP"
          ),
          # Double Integral ----
          nav_panel("Double Integral",
            # User input
            fixedRow(style="height: 60px;",
              column(actionButton("infoADoubleOUP","Info",width="100%",class="btn-primary"),title="about Double Integral",style="padding-top: 32px;",width=2),
              column(width=2),
              column(numericInput("yFromADoubleOUP",label="y:From",value="",step="any",width="100%"),title="stochastic states",width=2),
              column(numericInput("yToADoubleOUP",label="y:To",value="",step="any",width="100%"),title="stochastic states",width=2),
              column(numericInput("yByADoubleOUP",label="y:By",value="",step="any",width="100%"),title="state increment",width=2),
              column(numericInput("tByADoubleOUP",label="t:By",value="",step="any",width="100%"),title="time increment",width=2)
            ),
            fixedRow(style="height: 60px;",
              column(width=2),
              column(numericInput("rhoADoubleOUP",label="rho",value="",step="any",width="100%"),title="rate of convergence",width=2),
              column(numericInput("muADoubleOUP",label="mu",value="",step="any",width="100%"),title="location",width=2),
              column(numericInput("sigmaADoubleOUP",label="sigma",value="",step="any",width="100%"),title="scale",width=2),
              column(width=2),
              column(numericInput("tToADoubleOUP",label="t:To",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            fixedRow(style="height: 68px;",
              column(numericInput("sADoubleOUP",label="s",value="",step="any",width="100%"),title="fixed initial time",width=2),
              column(numericInput("xADoubleOUP",label="x",value="",step="any",width="100%"),title="fixed initial state",width=2),
              column(width=2),
              column(numericInput("psiADoubleOUP",label="psi",value="",step="any",width="100%"),title="-inf to y or y to inf",width=2),
              column(width=2),
              column(numericInput("tFromADoubleOUP",label="t:From",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            # User action
            fixedRow(
              column(actionButton("clearADoubleOUP",HTML("&forall;"),width="100%",class="btn-info"),title="clear and save",style="padding-right: 2px;",width=1),
              column(actionButton("saveADoubleOUP",HTML("&Vee;"),width="100%",class="btn-info"),title="save",style="padding-left: 2px;",width=1),
              column(actionButton("undoADoubleOUP",HTML("&Wedge;"),width="100%",class="btn-success"),title="undo",width=2),
              column(actionButton("syncADoubleOUP","Sync",width="100%",class="btn-success"),title="states and thresholds",width=2),
              column(actionButton("axesADoubleOUP","Axes",width="100%",class="btn-success"),title="for t, y and p",width=2),
              column(actionButton("plotADoubleOUP","Plot",width="100%",class="btn-success"),title="refresh",width=2),
              column(actionButton("leftADoubleOUP","<",width="100%",class="btn-success"),title="previous",style="padding-right: 2px;",width=1),
              column(actionButton("rghtADoubleOUP",">",width="100%",class="btn-success"),title="next",style="padding-left: 2px;",width=1)
            ),
            # Plot
            wellPanel(class="wellPlotOUP",
              style="margin: 0 auto; height: 402px; width: 580px;",plotlyOutput("plotlyADoubleOUP")
            ),
            value="ADoubleOUP"
          ),
          # Option ----
          nav_panel("Option",
            # User input
            fixedRow(style="height: 60px;",
              column(actionButton("infoAOptionOUP","Info",width="100%",class="btn-primary"),title="about Option",style="padding-top: 32px;",width=2),
              column(width=2),
              column(numericInput("xFromAOptionOUP",label="x:From",value="",step="any",width="100%"),title="stochastic states",width=2),
              column(numericInput("xToAOptionOUP",label="x:To",value="",step="any",width="100%"),title="stochastic states",width=2),
              column(numericInput("xByAOptionOUP",label="x:By",value="",step="any",width="100%"),title="state increment",width=2),
              column(numericInput("sByAOptionOUP",label="s:By",value="",step="any",width="100%"),title="time increment",width=2)
            ),
            fixedRow(style="height: 60px;",
              column(width=2),
              column(numericInput("rhoAOptionOUP",label="rho",value="",step="any",width="100%"),title="rate of convergence",width=2),
              column(numericInput("muAOptionOUP",label="mu",value="",step="any",width="100%"),title="location",width=2),
              column(numericInput("sigmaAOptionOUP",label="sigma",value="",step="any",width="100%"),title="scale",width=2),
              column(width=2),
              column(numericInput("sToAOptionOUP",label="s:To",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            fixedRow(style="height: 68px;",
              column(numericInput("tAOptionOUP",label="t",value="",step="any",width="100%"),title="fixed terminal time",width=2),
              column(numericInput("yAOptionOUP",label="y",value="",step="any",width="100%"),title="fixed terminal state",width=2),
              column(numericInput("rAOptionOUP",label="r",value="",step="any",width="100%"),title="discount rate of convergence",width=2),
              column(numericInput("phiAOptionOUP",label="phi",value="",step="any",width="100%"),title="exit or entry option",width=2),
              column(numericInput("bcAOptionOUP",label="~",value="",step="any",width="100%"),title="exit cost or entry benefit",width=2),
              column(numericInput("sFromAOptionOUP",label="s:From",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            # User action
            fixedRow(
              column(actionButton("clearAOptionOUP",HTML("&forall;"),width="100%",class="btn-info"),title="clear and save",style="padding-right: 2px;",width=1),
              column(actionButton("saveAOptionOUP",HTML("&Vee;"),width="100%",class="btn-info"),title="save",style="padding-left: 2px;",width=1),
              column(actionButton("undoAOptionOUP",HTML("&Wedge;"),width="100%",class="btn-success"),title="undo",width=2),
              column(actionButton("syncAOptionOUP","Sync",width="100%",class="btn-success"),title="states and thresholds",width=2),
              column(actionButton("axesAOptionOUP","Axes",width="100%",class="btn-success"),title="for s and x",width=2),
              column(actionButton("plotAOptionOUP","Plot",width="100%",class="btn-success"),title="refresh",width=2),
              column(actionButton("leftAOptionOUP","<",width="100%",class="btn-success"),title="previous",style="padding-right: 2px;",width=1),
              column(actionButton("rghtAOptionOUP",">",width="100%",class="btn-success"),title="next",style="padding-left: 2px;",width=1)
            ),
            # Plot
            wellPanel(class="wellPlotOUP",
              style="margin: 0 auto; height: 402px; width: 580px;",plotlyOutput("plotlyAOptionOUP")
            ),
            value="AOptionOUP"
          ),
          # Option Envelope ----
          nav_panel("Option Envelope",
            # User input
            fixedRow(style="height: 60px;",
              column(actionButton("infoAEnvelopeOUP","Info",width="100%",class="btn-primary"),title="about Option Envelope",style="padding-top: 32px;",width=2),
              column(width=2),
              column(numericInput("xFromAEnvelopeOUP",label="x:From",value="",step="any",width="100%"),title="stochastic states",width=2),
              column(numericInput("xToAEnvelopeOUP",label="x:To",value="",step="any",width="100%"),title="stochastic states",width=2),
              column(numericInput("xByAEnvelopeOUP",label="x:By",value="",step="any",width="100%"),title="state increment",width=2),
              column(numericInput("sByAEnvelopeOUP",label="s:By",value="",step="any",width="100%"),title="time increment",width=2)
            ),
            fixedRow(style="height: 60px;",
              column(width=2),
              column(numericInput("rhoAEnvelopeOUP",label="rho",value="",step="any",width="100%"),title="rate of convergence",width=2),
              column(numericInput("muAEnvelopeOUP",label="mu",value="",step="any",width="100%"),title="location",width=2),
              column(numericInput("sigmaAEnvelopeOUP",label="sigma",value="",step="any",width="100%"),title="scale",width=2),
              column(width=2),
              column(numericInput("sToAEnvelopeOUP",label="s:To",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            fixedRow(style="height: 68px;",
              column(numericInput("tAEnvelopeOUP",label="t",value="",step="any",width="100%"),title="fixed terminal time",width=2),
              column(numericInput("yAEnvelopeOUP",label="y",value="",step="any",width="100%"),title="fixed terminal state",width=2),
              column(numericInput("rAEnvelopeOUP",label="r",value="",step="any",width="100%"),title="discount rate",width=2),
              column(numericInput("phiAEnvelopeOUP",label="phi",value="",step="any",width="100%"),title="exit or entry option",width=2),
              column(numericInput("bcAEnvelopeOUP",label="~",value="",step="any",width="100%"),title="exit cost or entry benefit",width=2),
              column(numericInput("sFromAEnvelopeOUP",label="s:From",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            # User action
            fixedRow(
              column(actionButton("clearAEnvelopeOUP",HTML("&forall;"),width="100%",class="btn-info"),title="clear and save",style="padding-right: 2px;",width=1),
              column(actionButton("saveAEnvelopeOUP",HTML("&Vee;"),width="100%",class="btn-info"),title="save",style="padding-left: 2px;",width=1),
              column(actionButton("undoAEnvelopeOUP",HTML("&Wedge;"),width="100%",class="btn-success"),title="undo",width=2),
              column(actionButton("syncAEnvelopeOUP","Sync",width="100%",class="btn-success"),title="states and thresholds",width=2),
              column(actionButton("axesAEnvelopeOUP","Axes",width="100%",class="btn-success"),title="for s and x",width=2),
              column(actionButton("plotAEnvelopeOUP","Plot",width="100%",class="btn-success"),title="refresh",width=2),
              column(actionButton("leftAEnvelopeOUP","<",width="100%",class="btn-success"),title="previous",style="padding-right: 2px;",width=1),
              column(actionButton("rghtAEnvelopeOUP",">",width="100%",class="btn-success"),title="next",style="padding-left: 2px;",width=1)
            ),
            # Plot
            wellPanel(class="wellPlotOUP",
              style="margin: 0 auto; height: 402px; width: 580px;",plotlyOutput("plotlyAEnvelopeOUP")
            ),
            value="AEnvelopeOUP"
          ),
          # Decision Threshold ----
          nav_panel("Decision Threshold",
            # User input
            fixedRow(style="height: 60px;",
              column(actionButton("infoADecisionOUP","Info",width="100%",class="btn-primary"),title="about Decision Threshold",style="padding-top: 32px;",width=2),
              column(width=2),
              column(numericInput("xFromADecisionOUP",label="x:From",value="",step="any",width="100%"),title="stochastic states",width=2),
              column(numericInput("xToADecisionOUP",label="x:To",value="",step="any",width="100%"),title="stochastic states",width=2),
              column(numericInput("xByADecisionOUP",label="x:By",value="",step="any",width="100%"),title="state increment",width=2)
            ),
            fixedRow(style="height: 60px;",
              column(width=2),
              column(numericInput("rhoADecisionOUP",label="rho",value="",step="any",width="100%"),title="rate of convergence",width=2),
              column(numericInput("muADecisionOUP",label="mu",value="",step="any",width="100%"),title="location",width=2),
              column(numericInput("sigmaADecisionOUP",label="sigma",value="",step="any",width="100%"),title="scale",width=2)
            ),
            fixedRow(style="height: 68px;",
              column(width=2),
              column(numericInput("yADecisionOUP",label="y",value="",step="any",width="100%"),title="fixed terminal state",width=2),
              column(numericInput("rADecisionOUP",label="r",value="",step="any",width="100%"),title="discount rate",width=2),
              column(numericInput("phiADecisionOUP",label="phi",value="",step="any",width="100%"),title="exit or entry option",width=2),
              column(numericInput("bcADecisionOUP",label="~",value="",step="any",width="100%"),title="exit cost or entry benefit",width=2)
            ),
            # User action
            fixedRow(
              column(actionButton("clearADecisionOUP",HTML("&forall;"),width="100%",class="btn-info"),title="clear and save",style="padding-right: 2px;",width=1),
              column(actionButton("saveADecisionOUP",HTML("&Vee;"),width="100%",class="btn-info"),title="save",style="padding-left: 2px;",width=1),
              column(actionButton("undoADecisionOUP",HTML("&Wedge;"),width="100%",class="btn-success"),title="undo",width=2),
              column(actionButton("syncADecisionOUP","Sync",width="100%",class="btn-success"),title="states and thresholds",width=2),
              column(actionButton("axesADecisionOUP","Axes",width="100%",class="btn-success"),title="for s and x",width=2),
              column(actionButton("plotADecisionOUP","Plot",width="100%",class="btn-success"),title="refresh",width=2)
            ),
            # Plot
            wellPanel(class="wellPlotOUP",
              style="margin: 0 auto; height: 402px; width: 580px;",plotlyOutput("plotlyADecisionOUP")
            ),
            value="ADecisionOUP"
          ),
          # Obligation ----
          nav_panel("Obligation",
            # User input
            fixedRow(style="height: 60px;",
              column(actionButton("infoAObligationOUP","Info",width="100%",class="btn-primary"),title="about Obligation",style="padding-top: 32px;",width=2),
              column(width=2),
              column(numericInput("xFromAObligationOUP",label="x:From",value="",step="any",width="100%"),title="stochastic states",width=2),
              column(numericInput("xToAObligationOUP",label="x:To",value="",step="any",width="100%"),title="stochastic states",width=2),
              column(numericInput("xByAObligationOUP",label="x:By",value="",step="any",width="100%"),title="state increment",width=2),
              column(numericInput("sByAObligationOUP",label="s:By",value="",step="any",width="100%"),title="time increment",width=2)
            ),
            fixedRow(style="height: 60px;",
              column(width=2),
              column(numericInput("rhoAObligationOUP",label="rho",value="",step="any",width="100%"),title="rate of convergence",width=2),
              column(numericInput("muAObligationOUP",label="mu",value="",step="any",width="100%"),title="location",width=2),
              column(width=2),
              column(width=2),
              column(numericInput("sToAObligationOUP",label="s:To",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            fixedRow(style="height: 68px;",
              column(numericInput("tAObligationOUP",label="t",value="",step="any",width="100%"),title="fixed terminal time",width=2),
              column(numericInput("yAObligationOUP",label="y",value="",step="any",width="100%"),title="fixed terminal state",width=2),
              column(numericInput("rAObligationOUP",label="r",value="",step="any",width="100%"),title="discount rate",width=2),
              column(numericInput("phiAObligationOUP",label="phi",value="",step="any",width="100%"),title="exit or entry option",width=2),
              column(numericInput("bcAObligationOUP",label="~",value="",step="any",width="100%"),title="exit cost or entry benefit",width=2),
              column(numericInput("sFromAObligationOUP",label="s:From",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            # User action
            fixedRow(
              column(actionButton("clearAObligationOUP",HTML("&forall;"),width="100%",class="btn-info"),title="clear and save",style="padding-right: 2px;",width=1),
              column(actionButton("saveAObligationOUP",HTML("&Vee;"),width="100%",class="btn-info"),title="save",style="padding-left: 2px;",width=1),
              column(actionButton("undoAObligationOUP",HTML("&Wedge;"),width="100%",class="btn-success"),title="undo",width=2),
              column(actionButton("syncAObligationOUP","Sync",width="100%",class="btn-success"),title="states and thresholds",width=2),
              column(actionButton("axesAObligationOUP","Axes",width="100%",class="btn-success"),title="for s and x",width=2),
              column(actionButton("plotAObligationOUP","Plot",width="100%",class="btn-success"),title="refresh",width=2),
              column(actionButton("leftAObligationOUP","<",width="100%",class="btn-success"),title="previous",style="padding-right: 2px;",width=1),
              column(actionButton("rghtAObligationOUP",">",width="100%",class="btn-success"),title="next",style="padding-left: 2px;",width=1)
            ),
            # Plot
            wellPanel(class="wellPlotOUP",
              style="margin: 0 auto; height: 402px; width: 580px;",plotlyOutput("plotlyAObligationOUP")
            ),
            value="AObligationOUP"
          ),
          # Passage Time Mode, Median and Mean ----
          nav_panel("Passage Time Mode, Median and Mean",
            # User input
            fixedRow(style="height: 60px;",
              column(actionButton("infoAPTModeMedianMeanOUP","Info",width="100%",class="btn-primary"),title="about Passage Time Mode, Median and Mean",style="padding-top: 32px;",width=2),
              column(width=2),
              column(numericInput("zFromAPTModeMedianMeanOUP",label="z:From",value="",step="any",width="100%"),title="alternate initial states",width=2),
              column(numericInput("zToAPTModeMedianMeanOUP",label="z:To",value="",step="any",width="100%"),title="alternate initial states",width=2),
              column(numericInput("zByAPTModeMedianMeanOUP",label="z:By",value="",step="any",width="100%"),title="state increment",width=2),
              column(numericInput("tByAPTModeMedianMeanOUP",label="t:By",value="",step="any",width="100%"),title="time increment",width=2)
            ),
            fixedRow(style="height: 60px;",
              column(width=2),
              column(numericInput("rhoAPTModeMedianMeanOUP",label="rho",value="",step="any",width="100%"),title="rate of convergence",width=2),
              column(numericInput("muAPTModeMedianMeanOUP",label="mu",value="",step="any",width="100%"),title="location",width=2),
              column(numericInput("sigmaAPTModeMedianMeanOUP",label="sigma",value="",step="any",width="100%"),title="scale",width=2),
              column(numericInput("ptmaxAPTModeMedianMeanOUP",label="pt max",value="",step="any",width="100%"),title="maximum density",width=2),
              column(numericInput("tToAPTModeMedianMeanOUP",label="t:To",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            fixedRow(style="height: 68px;",
              column(numericInput("kAPTModeMedianMeanOUP",label="k",value="",step="any",width="100%"),title="threshold",width=2),
              column(numericInput("sAPTModeMedianMeanOUP",label="s",value="",step="any",width="100%"),title="fixed initial time",width=2),
              column(numericInput("xAPTModeMedianMeanOUP",label="x",value="",step="any",width="100%"),title="fixed initial state",width=2),
              column(numericInput("omegaAPTModeMedianMeanOUP",label="omega",value="",step="any",width="100%"),title="degree of irreversibility",width=2),
              column(width=2),
              column(numericInput("tFromAPTModeMedianMeanOUP",label="t:From",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            # User action
            fixedRow(
              column(actionButton("clearAPTModeMedianMeanOUP",HTML("&forall;"),width="100%",class="btn-info"),title="clear and save",style="padding-right: 2px;",width=1),
              column(actionButton("saveAPTModeMedianMeanOUP",HTML("&Vee;"),width="100%",class="btn-info"),title="save",style="padding-left: 2px;",width=1),
              column(actionButton("undoAPTModeMedianMeanOUP",HTML("&Wedge;"),width="100%",class="btn-success"),title="undo",width=2),
              column(actionButton("syncAPTModeMedianMeanOUP","Sync",width="100%",class="btn-success"),title="states and thresholds",width=2),
              column(actionButton("axesAPTModeMedianMeanOUP","Axes",width="100%",class="btn-success"),title="for t, z and pt",width=2),
              column(actionButton("plotAPTModeMedianMeanOUP","Plot",width="100%",class="btn-success"),title="refresh",width=2),
              column(actionButton("leftAPTModeMedianMeanOUP","<",width="100%",class="btn-success"),title="previous",style="padding-right: 2px;",width=1),
              column(actionButton("rghtAPTModeMedianMeanOUP",">",width="100%",class="btn-success"),title="next",style="padding-left: 2px;",width=1)
            ),
            # Plot
            wellPanel(class="wellPlotOUP",
              style="margin: 0 auto; height: 402px; width: 580px;",plotlyOutput("plotlyAPTModeMedianMeanOUP")
            ),
            value="APTModeMedianMeanOUP"
          ),
          # Passage Time Variance ----
          nav_panel("Passage Time Variance",
            # User input
            fixedRow(style="height: 60px;",
              column(actionButton("infoAPTVarianceOUP","Info",width="100%",class="btn-primary"),title="about Passage Time Variance",style="padding-top: 32px;",width=2),
              column(width=2),
              column(numericInput("zFromAPTVarianceOUP",label="z:From",value="",step="any",width="100%"),title="alternate initial states",width=2),
              column(numericInput("zToAPTVarianceOUP",label="z:To",value="",step="any",width="100%"),title="alternate initial states",width=2),
              column(numericInput("zByAPTVarianceOUP",label="z:By",value="",step="any",width="100%"),title="state increment",width=2)
            ),
            fixedRow(style="height: 60px;",
              column(width=2),
              column(numericInput("rhoAPTVarianceOUP",label="rho",value="",step="any",width="100%"),title="rate of convergence",width=2),
              column(numericInput("muAPTVarianceOUP",label="mu",value="",step="any",width="100%"),title="location",width=2),
              column(numericInput("sigmaAPTVarianceOUP",label="sigma",value="",step="any",width="100%"),title="scale",width=2)
            ),
            fixedRow(style="height: 68px;",
              column(numericInput("kAPTVarianceOUP",label="k",value="",step="any",width="100%"),title="threshold",width=2),
              column(numericInput("sAPTVarianceOUP",label="s",value="",step="any",width="100%"),title="fixed initial time",width=2),
              column(numericInput("xAPTVarianceOUP",label="x",value="",step="any",width="100%"),title="fixed initial state",width=2),
              column(numericInput("omegaAPTVarianceOUP",label="omega",value="",step="any",width="100%"),title="degree of irreversibility",width=2)
            ),
            # User action
            fixedRow(
              column(actionButton("clearAPTVarianceOUP",HTML("&forall;"),width="100%",class="btn-info"),title="clear and save",style="padding-right: 2px;",width=1),
              column(actionButton("saveAPTVarianceOUP",HTML("&Vee;"),width="100%",class="btn-info"),title="save",style="padding-left: 2px;",width=1),
              column(actionButton("undoAPTVarianceOUP",HTML("&Wedge;"),width="100%",class="btn-success"),title="undo",width=2),
              column(actionButton("syncAPTVarianceOUP","Sync",width="100%",class="btn-success"),title="states and thresholds",width=2),
              column(actionButton("axesAPTVarianceOUP","Axes",width="100%",class="btn-success"),title="for t, z and pt",width=2),
              column(actionButton("plotAPTVarianceOUP","Plot",width="100%",class="btn-success"),title="refresh",width=2),
              column(actionButton("leftAPTVarianceOUP","<",width="100%",class="btn-success"),title="previous",style="padding-right: 2px;",width=1),
              column(actionButton("rghtAPTVarianceOUP",">",width="100%",class="btn-success"),title="next",style="padding-left: 2px;",width=1)
            ),
            # Plot
            wellPanel(class="wellPlotOUP",
              style="margin: 0 auto; height: 402px; width: 580px;",plotlyOutput("plotlyAPTVarianceOUP")
            ),
            value="APTVarianceOUP"
          ),
          # Passage Time Percentiles ----
          nav_panel("Passage Time Percentiles",
            # User input
            fixedRow(style="height: 60px;",
              column(actionButton("infoAPTPercentilesOUP","Info",width="100%",class="btn-primary"),title="about Passage Time Percentiles",style="padding-top: 32px;",width=2),
              column(width=2),
              column(numericInput("zFromAPTPercentilesOUP",label="z:From",value="",step="any",width="100%"),title="alternate initial states",width=2),
              column(numericInput("zToAPTPercentilesOUP",label="z:To",value="",step="any",width="100%"),title="alternate initial states",width=2),
              column(numericInput("zByAPTPercentilesOUP",label="z:By",value="",step="any",width="100%"),title="state increment",width=2),
              column(numericInput("tByAPTPercentilesOUP",label="t:By",value="",step="any",width="100%"),title="time increment",width=2)
            ),
            fixedRow(style="height: 60px;",
              column(width=2),
              column(numericInput("rhoAPTPercentilesOUP",label="rho",value="",step="any",width="100%"),title="rate of convergence",width=2),
              column(numericInput("muAPTPercentilesOUP",label="mu",value="",step="any",width="100%"),title="location",width=2),
              column(numericInput("sigmaAPTPercentilesOUP",label="sigma",value="",step="any",width="100%"),title="scale",width=2),
              column(numericInput("ptmaxAPTPercentilesOUP",label="pt max",value="",step="any",width="100%"),title="maximum density",width=2),
              column(numericInput("tToAPTPercentilesOUP",label="t:To",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            fixedRow(style="height: 68px;",
              column(numericInput("kAPTPercentilesOUP",label="k",value="",step="any",width="100%"),title="threshold",width=2),
              column(numericInput("sAPTPercentilesOUP",label="s",value="",step="any",width="100%"),title="fixed initial time",width=2),
              column(numericInput("xAPTPercentilesOUP",label="x",value="",step="any",width="100%"),title="fixed initial state",width=2),
              column(numericInput("omegaAPTPercentilesOUP",label="omega",value="",step="any",width="100%"),title="degree of irreversibility",width=2),
              column(numericInput("PpctAPTPercentilesOUP",label="P pct",value="",step="any",width="100%"),title="passage time probability",width=2),
              column(numericInput("tFromAPTPercentilesOUP",label="t:From",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            # User action
            fixedRow(
              column(actionButton("clearAPTPercentilesOUP",HTML("&forall;"),width="100%",class="btn-info"),title="clear and save",style="padding-right: 2px;",width=1),
              column(actionButton("saveAPTPercentilesOUP",HTML("&Vee;"),width="100%",class="btn-info"),title="save",style="padding-left: 2px;",width=1),
              column(actionButton("undoAPTPercentilesOUP",HTML("&Wedge;"),width="100%",class="btn-success"),title="undo",width=2),
              column(actionButton("syncAPTPercentilesOUP","Sync",width="100%",class="btn-success"),title="states and thresholds",width=2),
              column(actionButton("axesAPTPercentilesOUP","Axes",width="100%",class="btn-success"),title="for t, z and pt",width=2),
              column(actionButton("plotAPTPercentilesOUP","Plot",width="100%",class="btn-success"),title="refresh",width=2),
              column(actionButton("leftAPTPercentilesOUP","<",width="100%",class="btn-success"),title="previous",style="padding-right: 2px;",width=1),
              column(actionButton("rghtAPTPercentilesOUP",">",width="100%",class="btn-success"),title="next",style="padding-left: 2px;",width=1)
            ),
            # Plot
            wellPanel(class="wellPlotOUP",
              style="margin: 0 auto; height: 402px; width: 580px;",plotlyOutput("plotlyAPTPercentilesOUP")
            ),
            value="APTPercentilesOUP"
          ),
          # Passage Time Density ----
          nav_panel("Passage Time Density",
            # User input
            fixedRow(style="height: 60px;",
              column(actionButton("infoAPTDensityOUP","Info",width="100%",class="btn-primary"),title="about Passage Time Density",style="padding-top: 32px;",width=2),
              column(width=2),
              column(numericInput("zFromAPTDensityOUP",label="z:From",value="",step="any",width="100%"),title="alternate initial states",width=2),
              column(numericInput("zToAPTDensityOUP",label="z:To",value="",step="any",width="100%"),title="alternate initial states",width=2),
              column(numericInput("zByAPTDensityOUP",label="z:By",value="",step="any",width="100%"),title="state increment",width=2),
              column(numericInput("tByAPTDensityOUP",label="t:By",value="",step="any",width="100%"),title="time increment",width=2)
            ),
            fixedRow(style="height: 60px;",
              column(width=2),
              column(numericInput("rhoAPTDensityOUP",label="rho",value="",step="any",width="100%"),title="rate of convergence",width=2),
              column(numericInput("muAPTDensityOUP",label="mu",value="",step="any",width="100%"),title="location",width=2),
              column(numericInput("sigmaAPTDensityOUP",label="sigma",value="",step="any",width="100%"),title="scale",width=2),
              column(numericInput("ptmaxAPTDensityOUP",label="pt max",value="",step="any",width="100%"),title="maximum density",width=2),
              column(numericInput("tToAPTDensityOUP",label="t:To",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            fixedRow(style="height: 68px;",
              column(numericInput("kAPTDensityOUP",label="k",value="",step="any",width="100%"),title="threshold",width=2),
              column(numericInput("sAPTDensityOUP",label="s",value="",step="any",width="100%"),title="fixed initial time",width=2),
              column(numericInput("xAPTDensityOUP",label="x",value="",step="any",width="100%"),title="fixed initial state",width=2),
              column(numericInput("omegaAPTDensityOUP",label="omega",value="",step="any",width="100%"),title="degree of irreversibility",width=2),
              column(width=2),
              column(numericInput("tFromAPTDensityOUP",label="t:From",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            # User action
            fixedRow(
              column(actionButton("clearAPTDensityOUP",HTML("&forall;"),width="100%",class="btn-info"),title="clear and save",style="padding-right: 2px;",width=1),
              column(actionButton("saveAPTDensityOUP",HTML("&Vee;"),width="100%",class="btn-info"),title="save",style="padding-left: 2px;",width=1),
              column(actionButton("undoAPTDensityOUP",HTML("&Wedge;"),width="100%",class="btn-success"),title="undo",width=2),
              column(actionButton("syncAPTDensityOUP","Sync",width="100%",class="btn-success"),title="states and thresholds",width=2),
              column(actionButton("axesAPTDensityOUP","Axes",width="100%",class="btn-success"),title="for t, z and pt",width=2),
              column(actionButton("plotAPTDensityOUP","Plot",width="100%",class="btn-success"),title="refresh",width=2),
              column(actionButton("leftAPTDensityOUP","<",width="100%",class="btn-success"),title="previous",style="padding-right: 2px;",width=1),
              column(actionButton("rghtAPTDensityOUP",">",width="100%",class="btn-success"),title="next",style="padding-left: 2px;",width=1)
            ),
            # Plot
            wellPanel(class="wellPlotOUP",
              style="margin: 0 auto; height: 402px; width: 580px;",plotlyOutput("plotlyAPTDensityOUP")
            ),
            value="APTDensityOUP"
          ),
          # Passage Time Probability ----
          nav_panel("Passage Time Probability",
            # User input
            fixedRow(style="height: 60px;",
              column(actionButton("infoAPTProbabilityOUP","Info",width="100%",class="btn-primary"),title="about Passage Time Probability",style="padding-top: 32px;",width=2),
              column(width=2),
              column(numericInput("zFromAPTProbabilityOUP",label="z:From",value="",step="any",width="100%"),title="alternate initial states",width=2),
              column(numericInput("zToAPTProbabilityOUP",label="z:To",value="",step="any",width="100%"),title="alternate initial states",width=2),
              column(numericInput("zByAPTProbabilityOUP",label="z:By",value="",step="any",width="100%"),title="state increment",width=2),
              column(numericInput("tByAPTProbabilityOUP",label="t:By",value="",step="any",width="100%"),title="time increment",width=2)
            ),
            fixedRow(style="height: 60px;",
              column(width=2),
              column(numericInput("rhoAPTProbabilityOUP",label="rho",value="",step="any",width="100%"),title="rate of convergence",width=2),
              column(numericInput("muAPTProbabilityOUP",label="mu",value="",step="any",width="100%"),title="location",width=2),
              column(numericInput("sigmaAPTProbabilityOUP",label="sigma",value="",step="any",width="100%"),title="scale",width=2),
              column(width=2),
              column(numericInput("tToAPTProbabilityOUP",label="t:To",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            fixedRow(style="height: 68px;",
              column(numericInput("kAPTProbabilityOUP",label="k",value="",step="any",width="100%"),title="threshold",width=2),
              column(numericInput("sAPTProbabilityOUP",label="s",value="",step="any",width="100%"),title="fixed initial time",width=2),
              column(numericInput("xAPTProbabilityOUP",label="x",value="",step="any",width="100%"),title="fixed initial state",width=2),
              column(numericInput("omegaAPTProbabilityOUP",label="omega",value="",step="any",width="100%"),title="degree of irreversibility",width=2),
              column(width=2),
              column(numericInput("tFromAPTProbabilityOUP",label="t:From",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            # User action
            fixedRow(
              column(actionButton("clearAPTProbabilityOUP",HTML("&forall;"),width="100%",class="btn-info"),title="clear and save",style="padding-right: 2px;",width=1),
              column(actionButton("saveAPTProbabilityOUP",HTML("&Vee;"),width="100%",class="btn-info"),title="save",style="padding-left: 2px;",width=1),
              column(actionButton("undoAPTProbabilityOUP",HTML("&Wedge;"),width="100%",class="btn-success"),title="undo",width=2),
              column(actionButton("syncAPTProbabilityOUP","Sync",width="100%",class="btn-success"),title="states and thresholds",width=2),
              column(actionButton("axesAPTProbabilityOUP","Axes",width="100%",class="btn-success"),title="for t, z and pt",width=2),
              column(actionButton("plotAPTProbabilityOUP","Plot",width="100%",class="btn-success"),title="refresh",width=2),
              column(actionButton("leftAPTProbabilityOUP","<",width="100%",class="btn-success"),title="previous",style="padding-right: 2px;",width=1),
              column(actionButton("rghtAPTProbabilityOUP",">",width="100%",class="btn-success"),title="next",style="padding-left: 2px;",width=1)
            ),
            # Plot
            wellPanel(class="wellPlotOUP",
              style="margin: 0 auto; height: 402px; width: 580px;",plotlyOutput("plotlyAPTProbabilityOUP")
            ),
            value="APTProbabilityOUP"
          ),
          id="navAOUP",widths=c(3,9)
        ),
        value="tabAOUP"
        #end list ----
      ),
      nav_panel("Finite Difference",
        tags$head(HTML('<html lang="en"> <link rel="icon" href="favicon.png" type="image/png" sizes="16x16">'),
                  tags$link(rel="stylesheet",type="text/css",href="styles.css")),
        add_busy_spinner(spin="flower",color="rgb(115,33,38)",timeout=500,position=c("top-right"),margins=c(500,200),height="128px",width="128px"),
        navset_pill_list(
          # Drift ----
          nav_panel("Drift",
            # User input
            fixedRow(style="height: 60px;",
              column(width=2),
              column(width=2),
              column(numericInput("xFromFDDriftOUP",label="x:From",value="",step="any",width="100%"),title="stochastic states",width=2),
              column(numericInput("xToFDDriftOUP",label="x:To",value="",step="any",width="100%"),title="stochastic states",width=2),
              column(numericInput("xByFDDriftOUP",label="x:By",value="",step="any",width="100%"),title="state increment",width=2)
            ),
            fixedRow(style="height: 144px;",
              column(numericInput("rhoFDDriftOUP",label="rho",value="",step="any",width="100%"),title="rate of convergence",width=2),
              column(numericInput("muFDDriftOUP",label="mu",value="",step="any",width="100%"),title="location",width=2)
            ),
            # User action
            fixedRow(
              column(actionButton("infoFDDriftOUP","Info",width="100%",class="btn-primary"),title="about Drift",width=2),
              column(actionButton("clearFDDriftOUP",HTML("&forall;"),width="100%",class="btn-info"),title="clear and save",style="padding-right: 2px;",width=1),
              column(actionButton("saveFDDriftOUP",HTML("&Vee;"),width="100%",class="btn-info"),title="save",style="padding-left: 2px;",width=1),
              column(actionButton("undoFDDriftOUP",HTML("&Wedge;"),width="100%",class="btn-success"),title="undo",width=2),
              column(actionButton("axesFDDriftOUP","Axes",width="100%",class="btn-success"),title="for s and x",width=2),
              column(actionButton("plotFDDriftOUP","Plot",width="100%",class="btn-success"),title="refresh",width=2)
            ),
            # Plot
            wellPanel(class="wellPlotOUP",
              style="margin: 0 auto; height: 402px; width: 580px;",plotlyOutput("plotlyFDDriftOUP")
            ),
            value="FDDriftOUP"
          ),
          # Diffusion ----
          nav_panel("Diffusion",
            # User input
            fixedRow(style="height: 60px;",
              column(width=2),
              column(width=2),
              column(numericInput("xFromFDDiffusionOUP",label="x:From",value="",step="any",width="100%"),title="stochastic states",width=2),
              column(numericInput("xToFDDiffusionOUP",label="x:To",value="",step="any",width="100%"),title="stochastic states",width=2),
              column(numericInput("xByFDDiffusionOUP",label="x:By",value="",step="any",width="100%"),title="state increment",width=2)
            ),
            fixedRow(style="height: 144px;",
              column(numericInput("rhoFDDiffusionOUP",label="rho",value="",step="any",width="100%"),title="rate of convergence",width=2),
              column(numericInput("muFDDiffusionOUP",label="mu",value="",step="any",width="100%"),title="location",width=2),
              column(numericInput("sigmaFDDiffusionOUP",label="sigma",value="",step="any",width="100%"),title="scale",width=2)
            ),
            # User action
            fixedRow(
              column(actionButton("infoFDDiffusionOUP","Info",width="100%",class="btn-primary"),title="about Diffusion",width=2),
              column(actionButton("clearFDDiffusionOUP",HTML("&forall;"),width="100%",class="btn-info"),title="clear and save",style="padding-right: 2px;",width=1),
              column(actionButton("saveFDDiffusionOUP",HTML("&Vee;"),width="100%",class="btn-info"),title="save",style="padding-left: 2px;",width=1),
              column(actionButton("undoFDDiffusionOUP",HTML("&Wedge;"),width="100%",class="btn-success"),title="undo",width=2),
              column(actionButton("axesFDDiffusionOUP","Axes",width="100%",class="btn-success"),title="for s and x",width=2),
              column(actionButton("plotFDDiffusionOUP","Plot",width="100%",class="btn-success"),title="refresh",width=2),
              column(actionButton("leftFDDiffusionOUP","<",width="100%",class="btn-success"),title="previous",style="padding-right: 2px;",width=1),
              column(actionButton("rghtFDDiffusionOUP",">",width="100%",class="btn-success"),title="next",style="padding-left: 2px;",width=1)
            ),
            # Plot
            wellPanel(class="wellPlotOUP",
              style="margin: 0 auto; height: 402px; width: 580px;",plotlyOutput("plotlyFDDiffusionOUP")
            ),
            value="FDDiffusionOUP"
          ),
          # Terminal Values ----
          nav_panel("Terminal Values",
            # User input
            fixedRow(style="height: 136px;",
              column(selectInput("VFDTerminalOUP",label="Formula",choices=""),title="terminal values",width=4),
              column(numericInput("xFromFDTerminalOUP",label="x:From",value="",step="any",width="100%"),title="stochastic states",width=2),
              column(numericInput("xToFDTerminalOUP",label="x:To",value="",step="any",width="100%"),title="stochastic states",width=2),
              column(numericInput("xByFDTerminalOUP",label="x:By",value="",step="any",width="100%"),title="state increment",width=2)
            ),
            fixedRow(style="height: 68px;",
              column(numericInput("V1FDTerminalOUP",label="~",value="",step="any",width="100%"),title="argument",width=2),
              column(numericInput("V2FDTerminalOUP",label="~",value="",step="any",width="100%"),title="argument",width=2),
              column(numericInput("V3FDTerminalOUP",label="~",value="",step="any",width="100%"),title="argument",width=2),
              column(numericInput("V4FDTerminalOUP",label="~",value="",step="any",width="100%"),title="argument",width=2),
              column(numericInput("V5FDTerminalOUP",label="~",value="",step="any",width="100%"),title="argument",width=2)
            ),
            # User action
            fixedRow(
              column(actionButton("infoFDTerminalOUP","Info",width="100%",class="btn-primary"),title="about Terminal Values",width=2),
              column(actionButton("clearFDTerminalOUP",HTML("&forall;"),width="100%",class="btn-info"),title="clear and save",style="padding-right: 2px;",width=1),
              column(actionButton("saveFDTerminalOUP",HTML("&Vee;"),width="100%",class="btn-info"),title="save",style="padding-left: 2px;",width=1),
              column(actionButton("undoFDTerminalOUP",HTML("&Wedge;"),width="100%",class="btn-success"),title="undo",width=2),
              column(actionButton("axesFDTerminalOUP","Axes",width="100%",class="btn-success"),title="for s and x",width=2),
              column(actionButton("plotFDTerminalOUP","Plot",width="100%",class="btn-success"),title="refresh",width=2)
            ),
            # Plot
            wellPanel(class="wellPlotOUP",
              style="margin: 0 auto; height: 402px; width: 580px;",plotlyOutput("plotlyFDTerminalOUP")
            ),
            value="FDTerminalOUP"
          ),
          # Option ----
          nav_panel("Option",
            # User input
            fixedRow(style="height: 60px;",
              column(selectInput("VFDOptionOUP",label="Formula",choices=""),title="terminal values",width=4),
              column(numericInput("xFromFDOptionOUP",label="x:From",value="",step="any",width="100%"),title="stochastic states",width=2),
              column(numericInput("xToFDOptionOUP",label="x:To",value="",step="any",width="100%"),title="stochastic states",width=2),
              column(numericInput("xByFDOptionOUP",label="x:By",value="",step="any",width="100%"),title="state increment",width=2),
              column(numericInput("sByFDOptionOUP",label="s:By",value="",step="any",width="100%"),title="time increment",width=2)
            ),
            fixedRow(style="height: 60px;",
              column(numericInput("rhoFDOptionOUP",label="rho",value="",step="any",width="100%"),title="rate of convergence",width=2),
              column(numericInput("muFDOptionOUP",label="mu",value="",step="any",width="100%"),title="location",width=2),
              column(numericInput("sigmaFDOptionOUP",label="sigma",value="",step="any",width="100%"),title="scale",width=2),
              column(numericInput("rFDOptionOUP",label="r",value="",step="any",width="100%"),title="discount rate",width=2),
              column(width=2),
              column(numericInput("sToFDOptionOUP",label="s:To",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            fixedRow(style="height: 68px;",
              column(numericInput("V1FDOptionOUP",label="~",value="",step="any",width="100%"),title="argument",width=2),
              column(numericInput("V2FDOptionOUP",label="~",value="",step="any",width="100%"),title="argument",width=2),
              column(numericInput("V3FDOptionOUP",label="~",value="",step="any",width="100%"),title="argument",width=2),
              column(numericInput("V4FDOptionOUP",label="~",value="",step="any",width="100%"),title="argument",width=2),
              column(numericInput("V5FDOptionOUP",label="~",value="",step="any",width="100%"),title="argument",width=2),
              column(numericInput("sFromFDOptionOUP",label="s:From",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            # User action
            fixedRow(
              column(actionButton("infoFDOptionOUP","Info",width="100%",class="btn-primary"),title="about Option",width=2),
              column(actionButton("clearFDOptionOUP",HTML("&forall;"),width="100%",class="btn-info"),title="clear and save",style="padding-right: 2px;",width=1),
              column(actionButton("saveFDOptionOUP",HTML("&Vee;"),width="100%",class="btn-info"),title="save",style="padding-left: 2px;",width=1),
              column(actionButton("undoFDOptionOUP",HTML("&Wedge;"),width="100%",class="btn-success"),title="undo",width=2),
              column(actionButton("axesFDOptionOUP","Axes",width="100%",class="btn-success"),title="for s and x",width=2),
              column(actionButton("plotFDOptionOUP","Plot",width="100%",class="btn-success"),title="refresh",width=2),
              column(actionButton("leftFDOptionOUP","<",width="100%",class="btn-success"),title="previous",style="padding-right: 2px;",width=1),
              column(actionButton("rghtFDOptionOUP",">",width="100%",class="btn-success"),title="next",style="padding-left: 2px;",width=1)
            ),
            # Plot
            wellPanel(class="wellPlotOUP",
              style="margin: 0 auto; height: 402px; width: 580px;",plotlyOutput("plotlyFDOptionOUP")
            ),
            value="FDOptionOUP"
          ),
          # Option Envelope ----
          nav_panel("Option Envelope",
            # User input
            fixedRow(style="height: 60px;",
              column(selectInput("VFDEnvelopeOUP",label="Formula",choices=""),title="terminal values",width=4),
              column(numericInput("xFromFDEnvelopeOUP",label="x:From",value="",step="any",width="100%"),title="stochastic states",width=2),
              column(numericInput("xToFDEnvelopeOUP",label="x:To",value="",step="any",width="100%"),title="stochastic states",width=2),
              column(numericInput("xByFDEnvelopeOUP",label="x:By",value="",step="any",width="100%"),title="state increment",width=2),
              column(numericInput("sByFDEnvelopeOUP",label="s:By",value="",step="any",width="100%"),title="time increment",width=2)
            ),
            fixedRow(style="height: 60px;",
              column(numericInput("rhoFDEnvelopeOUP",label="rho",value="",step="any",width="100%"),title="rate of convergence",width=2),
              column(numericInput("muFDEnvelopeOUP",label="mu",value="",step="any",width="100%"),title="location",width=2),
              column(numericInput("sigmaFDEnvelopeOUP",label="sigma",value="",step="any",width="100%"),title="scale",width=2),
              column(numericInput("rFDEnvelopeOUP",label="r",value="",step="any",width="100%"),title="discount rate",width=2),
              column(width=2),
              column(numericInput("sToFDEnvelopeOUP",label="s:To",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            fixedRow(style="height: 68px;",
              column(numericInput("V1FDEnvelopeOUP",label="~",value="",step="any",width="100%"),title="argument",width=2),
              column(numericInput("V2FDEnvelopeOUP",label="~",value="",step="any",width="100%"),title="argument",width=2),
              column(numericInput("V3FDEnvelopeOUP",label="~",value="",step="any",width="100%"),title="argument",width=2),
              column(numericInput("V4FDEnvelopeOUP",label="~",value="",step="any",width="100%"),title="argument",width=2),
              column(numericInput("V5FDEnvelopeOUP",label="~",value="",step="any",width="100%"),title="argument",width=2),
              column(numericInput("sFromFDEnvelopeOUP",label="s:From",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            # User action
            fixedRow(
              column(actionButton("infoFDEnvelopeOUP","Info",width="100%",class="btn-primary"),title="about Option Envelope",width=2),
              column(actionButton("clearFDEnvelopeOUP",HTML("&forall;"),width="100%",class="btn-info"),title="clear and save",style="padding-right: 2px;",width=1),
              column(actionButton("saveFDEnvelopeOUP",HTML("&Vee;"),width="100%",class="btn-info"),title="save",style="padding-left: 2px;",width=1),
              column(actionButton("undoFDEnvelopeOUP",HTML("&Wedge;"),width="100%",class="btn-success"),title="undo",width=2),
              column(actionButton("axesFDEnvelopeOUP","Axes",width="100%",class="btn-success"),title="for s and x",width=2),
              column(actionButton("plotFDEnvelopeOUP","Plot",width="100%",class="btn-success"),title="refresh",width=2),
              column(actionButton("leftFDEnvelopeOUP","<",width="100%",class="btn-success"),title="previous",style="padding-right: 2px;",width=1),
              column(actionButton("rghtFDEnvelopeOUP",">",width="100%",class="btn-success"),title="next",style="padding-left: 2px;",width=1)
            ),
            # Plot
            wellPanel(class="wellPlotOUP",
              style="margin: 0 auto; height: 402px; width: 580px;",plotlyOutput("plotlyFDEnvelopeOUP")
            ),
            value="FDEnvelopeOUP"
          ),
          # Decision Threshold ----
          nav_panel("Decision Threshold",
            # User input
            fixedRow(style="height: 60px;",
              column(selectInput("VFDDecisionOUP",label="Formula",choices=""),title="terminal values",width=4),
              column(numericInput("xFromFDDecisionOUP",label="x:From",value="",step="any",width="100%"),title="stochastic states",width=2),
              column(numericInput("xToFDDecisionOUP",label="x:To",value="",step="any",width="100%"),title="stochastic states",width=2),
              column(numericInput("xByFDDecisionOUP",label="x:By",value="",step="any",width="100%"),title="state increment",width=2)
            ),
            fixedRow(style="height: 60px;",
              column(numericInput("rhoFDDecisionOUP",label="rho",value="",step="any",width="100%"),title="rate of convergence",width=2),
              column(numericInput("muFDDecisionOUP",label="mu",value="",step="any",width="100%"),title="location",width=2),
              column(numericInput("sigmaFDDecisionOUP",label="sigma",value="",step="any",width="100%"),title="scale",width=2),
              column(numericInput("rFDDecisionOUP",label="r",value="",step="any",width="100%"),title="discount rate",width=2),
              column(numericInput("phiFDDecisionOUP",label="phi",value="",step="any",width="100%"),title="exit or entry option",width=2)
            ),
            fixedRow(style="height: 68px;",
              column(numericInput("V1FDDecisionOUP",label="~",value="",step="any",width="100%"),title="argument",width=2),
              column(numericInput("V2FDDecisionOUP",label="~",value="",step="any",width="100%"),title="argument",width=2),
              column(numericInput("V3FDDecisionOUP",label="~",value="",step="any",width="100%"),title="argument",width=2),
              column(numericInput("V4FDDecisionOUP",label="~",value="",step="any",width="100%"),title="argument",width=2),
              column(numericInput("V5FDDecisionOUP",label="~",value="",step="any",width="100%"),title="argument",width=2)
            ),
            # User action
            fixedRow(
              column(actionButton("infoFDDecisionOUP","Info",width="100%",class="btn-primary"),title="about Decision Threshold",width=2),
              column(actionButton("clearFDDecisionOUP",HTML("&forall;"),width="100%",class="btn-info"),title="clear and save",style="padding-right: 2px;",width=1),
              column(actionButton("saveFDDecisionOUP",HTML("&Vee;"),width="100%",class="btn-info"),title="save",style="padding-left: 2px;",width=1),
              column(actionButton("undoFDDecisionOUP",HTML("&Wedge;"),width="100%",class="btn-success"),title="undo",width=2),
              column(actionButton("axesFDDecisionOUP","Axes",width="100%",class="btn-success"),title="for s and x",width=2),
              column(actionButton("plotFDDecisionOUP","Plot",width="100%",class="btn-success"),title="refresh",width=2)
            ),
            # Plot
            wellPanel(class="wellPlotOUP",
              style="margin: 0 auto; height: 402px; width: 580px;",plotlyOutput("plotlyFDDecisionOUP")
            ),
            value="FDDecisionOUP"
          ),
          id="navFDOUP",widths=c(3,9)
        ),
        value="tabFDOUP"
        #end list ----
      ),
      nav_panel("Maximum Likelihood",
        tags$head(HTML('<html lang="en"> <link rel="icon" href="favicon.png" type="image/png" sizes="16x16">'),
                  tags$link(rel="stylesheet",type="text/css",href="styles.css")),
        add_busy_spinner(spin="fulfilling-bouncing-circle",color="rgb(0,86,136)",timeout=500,position=c("top-right"),margins=c(500,200),height="128px",width="128px"),
        navset_pill_list(
          # Data ----
          nav_panel("Data",
            # file, time and state
            fixedRow(
              column(actionButton("fileinfoMLDataOUP","i",width="100%",class="btn-primary"),title="File info",style="padding-right: 2px; padding-top: 32px;",width=1),
              column(selectInput("filesMLDataOUP",label="File",choices=""),title="data files",width=5),
              column(selectInput("timeMLDataOUP",label="Time",choices=""),title="time variable",width=3),
              column(selectInput("stateMLDataOUP",label="State",choices=""),title="state variable",width=3)
            ),
            # first and last times, number of rows and columns in data
            fixedRow(
              column(fileInput("filesMLUploadOUP",NULL,multiple=FALSE,accept=".csv",buttonLabel="...",placeholder="Select a file to upload..."),title="upload a data file",width=6),
              column(wellPanel(class="wellTableOUP",style="padding: 0px; width=100%;",uiOutput("descrMLDataOUP")),width=6)
            ),
            # buttons, begin and end dates
            fixedRow(
              column(actionButton("resetMLDataOUP","Reset",width="100%",class="btn-success"),title="reset begin and end",style="padding-top: 32px;",width=2),
              column(numericInput("begMLDataOUP",label="Begin",value="",step="any",width="100%"),title="time to begin plot",width=3),
              column(numericInput("endMLDataOUP",label="End",value="",step="any",width="100%"),title="time to end plot",width=3),
              column(actionButton("plotMLDataOUP","Plot",width="100%",class="btn-success"),title="refresh",style="padding-top: 32px;",width=2),
              column(actionButton("infoMLDataOUP","Info",width="100%",class="btn-primary"),title="about Data",style="padding-top: 32px;",width=2)
            ),
            # plot
            fluidRow(
              wellPanel(class="wellPlotOUP",style="height: 402px;",plotlyOutput("plotlyMLDataOUP"))
            ),
            value="MLDataOUP"
          ),
          # Log Likelihood ----
          nav_panel("Log Likelihood",
            # file, time and state
            fixedRow(
              column(actionButton("fileinfoMLLikelihoodOUP","i",width="100%",class="btn-primary"),title="File info",style="padding-right: 2px; padding-top: 32px;",width=1),
              column(selectInput("filesMLLikelihoodOUP",label="File",choices=""),title="data files",width=5),
              column(selectInput("timeMLLikelihoodOUP",label="Time",choices=""),title="time variable",width=3),
              column(selectInput("stateMLLikelihoodOUP",label="State",choices=""),title="state variable",width=3)
            ),
            # parameters and Likelihood
            fixedRow(style="height: 71px;",
              column(numericInput("rhoMLLikelihoodOUP",label="rho",value="",step="any",width="100%"),title="rate of convergence",width=2),
              column(numericInput("muMLLikelihoodOUP",label="mu",value="",step="any",width="100%"),title="location",width=2),
              column(numericInput("sigmaMLLikelihoodOUP",label="sigma",value="",step="any",width="100%"),title="scale",width=2),
              column(wellPanel(class="wellTableOUP",style="padding: 0px; width=100%;",uiOutput("lnLMLLikelihoodOUP")),width=3)
            ),
            # buttons, begin and end dates
            fixedRow(
              column(actionButton("resetMLLikelihoodOUP","Reset",width="100%",class="btn-success"),title="reset begin and end",style="padding-top: 32px;",width=2),
              column(numericInput("begMLLikelihoodOUP",label="Begin",value="",step="any",width="100%"),title="time to begin plot",width=3),
              column(numericInput("endMLLikelihoodOUP",label="End",value="",step="any",width="100%"),title="time to end plot",width=3),
              column(actionButton("plotMLLikelihoodOUP","Go",width="100%",class="btn-success"),title="calculate and plot",style="padding-top: 32px;",width=2),
              column(actionButton("infoMLLikelihoodOUP","Info",width="100%",class="btn-primary"),title="about Log Likelihood",style="padding-top: 32px;",width=2)
            ),
            # plot
            fluidRow(
              wellPanel(class="wellPlotOUP",style="height: 402px;",plotlyOutput("plotlyMLLikelihoodOUP"))
            ),
            value="MLLikelihoodOUP"
          ),
          # Estimates ----
          nav_panel("Estimates",
            # file, time and state
            fixedRow(
              column(actionButton("fileinfoMLEstimatesOUP","i",width="100%",class="btn-primary"),title="File info",style="padding-right: 2px; padding-top: 32px;",width=1),
              column(selectInput("filesMLEstimatesOUP",label="File",choices=""),title="data files",width=5),
              column(selectInput("timeMLEstimatesOUP",label="Time",choices=""),title="time variable",width=3),
              column(selectInput("stateMLEstimatesOUP",label="State",choices=""),title="state variable",width=3)
            ),
            # parameters, likelihood and such
            fixedRow(
              column(wellPanel(class="wellTableOUP",style="padding: 0px; width: 100%;",uiOutput("paramMLEstimatesOUP")),width=12)
            ),
            # buttons, begin and end dates
            fixedRow(
              column(actionButton("resetMLEstimatesOUP","Reset",width="100%",class="btn-success"),title="reset begin and end",style="padding-top: 32px;",width=2),
              column(numericInput("begMLEstimatesOUP",label="Begin",value="",step="any",width="100%"),title="time to begin plot",width=3),
              column(numericInput("endMLEstimatesOUP",label="End",value="",step="any",width="100%"),title="time to end plot",width=3),
              column(actionButton("plotMLEstimatesOUP","Go",width="100%",class="btn-success"),title="estimate and plot",style="padding-top: 32px;",width=2),
              column(actionButton("infoMLEstimatesOUP","Info",width="100%",class="btn-primary"),title="about Estimates",style="padding-top: 32px;",width=2)
            ),
            # plot
            fluidRow(
              wellPanel(class="wellPlotOUP",style="height: 402px;",plotlyOutput("plotlyMLEstimatesOUP"))
            ),
            value="MLEstimatesOUP"
          ),
          # Goodness-of-Fit ----
          nav_panel("Goodness-of-Fit",
            # file, time and state
            fixedRow(
              column(actionButton("fileinfoMLGoodnessOUP","i",width="100%",class="btn-primary"),title="File info",style="padding-right: 2px; padding-top: 32px;",width=1),
              column(selectInput("filesMLGoodnessOUP",label="File",choices=""),title="data files",width=5),
              column(selectInput("timeMLGoodnessOUP",label="Time",choices=""),title="time variable",width=3),
              column(selectInput("stateMLGoodnessOUP",label="State",choices=""),title="state variable",width=3)
            ),
            # parameters, likelihood and such
            fixedRow(style="height: 98px;",
              column(wellPanel(class="wellTableOUP",style="padding: 0px; width: 100%;",uiOutput("paramMLGoodnessOUP")),width=12)
            ),
            # buttons
            fixedRow(
              column(width=2),
              column(width=2),
              column(width=2),
              column(width=2),
              column(actionButton("plotMLGoodnessOUP","Go",width="100%",class="btn-success"),title="calculate",style="padding-top: 32px;",width=2),
              column(actionButton("infoMLGoodnessOUP","Info",width="100%",class="btn-primary"),title="about Goodness-of-Fit",style="padding-top: 32px;",width=2)
            ),
            # table
            fixedRow(
              column(width=3),
              column(wellPanel(class="wellTableOUP",style="margin-top: 26px; padding: 6px 0px 18px 0px; width=100%;",uiOutput("goodsMLGoodnessOUP")),width=6)
            ),
            value="MLGoodnessOUP"
          ),
          # Likelihood Ratio Test ----
          nav_panel("Likelihood Ratio Test",
            # file, time and state
            fixedRow(
              column(actionButton("fileinfoMLRatioOUP","i",width="100%",class="btn-primary"),title="File info",style="padding-right: 2px; padding-top: 32px;",width=1),
              column(selectInput("filesMLRatioOUP",label="File",choices=""),title="data files",width=5),
              column(selectInput("timeMLRatioOUP",label="Time",choices=""),title="time variable",width=3),
              column(selectInput("stateMLRatioOUP",label="State",choices=""),title="state variable",width=3)
            ),
            # parameters, likelihood and such
            fixedRow(
              column(wellPanel(class="wellTableOUP",style="padding: 0px; width: 100%;",uiOutput("paramMLRatioOUP")),width=12)
            ),
            # restrictions and buttons
            fixedRow(
              column(actionButton("resetMLRatioOUP","Reset",width="100%",class="btn-success"),title="reset rhor, mur and sigmar",style="padding-top: 32px;",width=2),
              column(numericInput("rhorMLRatioOUP",label="rhor",value="",step="any",width="100%"),title="constant for rate",width=2),
              column(numericInput("murMLRatioOUP",label="mur",value="",step="any",width="100%"),title="constant for location",width=2),
              column(numericInput("sigmarMLRatioOUP",label="sigmar",value="",step="any",width="100%"),title="constant for scale",width=2),
              column(actionButton("plotMLRatioOUP","Go",width="100%",class="btn-success"),title="calculate",style="padding-top: 32px;",width=2),
              column(actionButton("infoMLRatioOUP","Info",width="100%",class="btn-primary"),title="about Likelihood Ratio Test",style="padding-top: 32px;",width=2)
            ),
            # table
            fixedRow(
              column(width=3),
              column(wellPanel(class="wellTableOUP",style="margin-top: 10px; padding: 6px 0px 18px 0px; width=100%;",uiOutput("ratioMLRatioOUP")),width=6)
            ),
            value="MLRatioOUP"
          ),
          id="navMLOUP",widths=c(3,9)
        ),
        value="tabMLOUP"
        #end list ----
      )
    ),
    nav_menu("Help",
      nav_item(
        a(href="https://greghertzler.github.io/GregsOUPShiny/OUP_Shiny.html","Tutorials",target="_blank")
      ),
      nav_item(
        a(href="https://greghertzler.github.io/GregsOUPR6/OUP_Help.html","Reference",target="_blank")
      ),
      nav_panel("About",
        value="tabAboutOUP"
      ),
      nav_panel("License",
        value="tabLicenseOUP"
      )
    ),
    nav_item(input_dark_mode(id="darkmodeswitch")),
    id="navBar",window_title="ROAR"
  )
)
