library(shiny)
library(bslib)
library(shinybusy)
library(plotly)

# copy button functions ----
copyPlot <- function() {
  tags$button(
    class = "btn-copy-plot",
    title="Copy data to the clipboard",
    tags$svg(xmlns = "http://www.w3.org/2000/svg",
      width = "16", height = "16",
      viewBox = "0 0 4.2333 4.2333",
      tags$path(d = "m0.52917 0h2.3812v0.79375h-2.3812zm0 1.0583h2.3812v0.52917h-2.3812zm0 0.79375h2.3812v0.52917h-2.3812zm0 0.79375h2.3812v0.79375h-2.3812zm2.3812-2.6458h0.52917v3.4396h-0.52917zm-2.9104 0h0.52917v3.4396h-0.52917zm3.7042 0.79375h0.52917v2.9104h-0.52917zm-2.9104 2.9104h3.4396v0.52917h-3.4396z")
    )
  )
}
copyTable <- function() {
  tags$button(
    class = "btn-copy-table",
    title="Copy results to the clipboard",
    tags$svg(xmlns = "http://www.w3.org/2000/svg",
      width = "16", height = "16",
      viewBox = "0 0 4.2333 4.2333",
      tags$path(d = "m0.52917 0h2.3812v0.79375h-2.3812zm0 1.0583h2.3812v0.52917h-2.3812zm0 0.79375h2.3812v0.52917h-2.3812zm0 0.79375h2.3812v0.79375h-2.3812zm2.3812-2.6458h0.52917v3.4396h-0.52917zm-2.9104 0h0.52917v3.4396h-0.52917zm3.7042 0.79375h0.52917v2.9104h-0.52917zm-2.9104 2.9104h3.4396v0.52917h-3.4396z")
    )
  )
}

# ui ----
shinyUI(
  page_navbar(title=img(src="Roar32x32.png",alt="ROAR"),
    theme=bs_theme(bootswatch="spacelab",bg="#ddeeff",fg="#001122",success="#11aa88"),
    nav_menu("Ornstein-Uhlenbeck Process",
      nav_panel("Real Options",
        tags$head(HTML('<html lang="en"> <link rel="icon" href="favicon.png" type="image/png" sizes="16x16">'),
                  tags$link(rel="stylesheet",type="text/css",href="styles.css")),
        tags$script(src="script.js"),
        add_busy_spinner(spin="swapping-squares",color="rgb(115,33,38)",timeout=1000,position=c("top-right"),margins=c(450,350),height="128px",width="128px"),
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
              column(wellPanel(class="wellTableOUP",style="padding: 0px; width=100%;",uiOutput("descrRODataOUP"),copyTable()),width=6)
            ),
            # buttons, begin and end dates
            fixedRow(
              column(actionButton("resetRODataOUP","Reset",width="100%",class="btn-success"),title="reset begin and end",style="padding-top: 32px;",width=2),
              column(numericInput("begRODataOUP",label="begin",value="",step="any",width="100%"),title="time to begin plot",width=2),
              column(numericInput("endRODataOUP",label="end",value="",step="any",width="100%"),title="time to end plot",width=2),
              column(width=2),
              column(actionButton("plotRODataOUP","Plot",width="100%",class="btn-success"),title="refresh plot",style="padding-top: 32px;",width=2),
              column(actionButton("infoRODataOUP","Info",width="100%",class="btn-primary"),title="information about Data",style="padding-top: 32px;",width=2)
            ),
            # plot
            wellPanel(class="wellPlotOUP",style="height: 402px;",plotlyOutput("plotlyRODataOUP"),copyPlot()),
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
              column(wellPanel(class="wellTableOUP",style="padding: 0px; width: 100%;",uiOutput("paramROEstimatesOUP"),copyTable()),width=12)
            ),
            # buttons, begin and end dates
            fixedRow(
              column(actionButton("resetROEstimatesOUP","Reset",width="100%",class="btn-success"),title="reset begin and end",style="padding-top: 32px;",width=2),
              column(numericInput("begROEstimatesOUP",label="begin",value="",step="any",width="100%"),title="time to begin plot",width=2),
              column(numericInput("endROEstimatesOUP",label="end",value="",step="any",width="100%"),title="time to end plot",width=2),
              column(width=2),
              column(actionButton("plotROEstimatesOUP","Go",width="100%",class="btn-success"),title="estimate and plot",style="padding-top: 32px;",width=2),
              column(actionButton("infoROEstimatesOUP","Info",width="100%",class="btn-primary"),title="information about Estimates",style="padding-top: 32px;",width=2)
            ),
            # plot
            wellPanel(class="wellPlotOUP",style="height: 402px;",plotlyOutput("plotlyROEstimatesOUP"),copyPlot()),
            value="ROEstimatesOUP"
          ),
          # Regime ----
          nav_panel("Regime",
            # User input
            fixedRow(style="height: 60px;",
              column(actionButton("infoRORegimeOUP","Info",width="100%",class="btn-primary"),title="information about Regimes",style="padding-top: 32px;",width=2),
              column(numericInput("yRORegimeOUP",label="y",value="",step="any",width="100%"),title="fixed terminal state",width=2),
              column(numericInput("xFromRORegimeOUP",label="x:From",value="",step="any",width="100%"),title="stochastic states",width=2),
              column(numericInput("xToRORegimeOUP",label="x:To",value="",step="any",width="100%"),title="stochastic states",width=2),
              column(numericInput("xByRORegimeOUP",label="x:By",value="",step="any",width="100%"),title="state increment",width=2)
            ),
            fixedRow(style="height: 60px;",
              column(numericInput("rhoRORegimeOUP",label="rho",value="",step="any",width="100%"),title="rate of convergence",width=2),
              column(numericInput("muRORegimeOUP",label="mu",value="",step="any",width="100%"),title="location",width=2),
              column(numericInput("sigmaRORegimeOUP",label="sigma",value="",step="any",width="100%"),title="scale",width=2),
              column(numericInput("rRORegimeOUP",label="r",value="",step="any",width="100%"),title="discount rate",width=2),
              column(numericInput("phiRORegimeOUP",label="phi",value="",step="any",width="100%"),title="exit or entry option",width=2)
            ),
            fixedRow(style="height: 68px;",
              column(width=2),
              column(width=2),
              column(numericInput("bRORegimeOUP",label="b",value="",step="any",width="100%"),title="entry benefit",width=2),
              column(numericInput("cRORegimeOUP",label="c",value="",step="any",width="100%"),title="exit cost",width=2)
            ),
            # User action
            fixedRow(
              column(actionButton("clearRORegimeOUP",HTML("_"),width="100%",class="btn-info"),title="clear and save arguments",style="padding-right: 2px;",width=1),
              column(actionButton("saveRORegimeOUP",HTML("&equiv;"),width="100%",class="btn-info"),title="save arguments",style="padding-left: 2px;",width=1),
              column(actionButton("undnRORegimeOUP",HTML("&Vee;"),width="100%",class="btn-success"),title="previous arguments",style="padding-right: 2px;",width=1),
              column(actionButton("unupRORegimeOUP",HTML("&Wedge;"),width="100%",class="btn-success"),title="next arguments",style="padding-left: 2px;",width=1),
              column(actionButton("syncRORegimeOUP","Sync",width="100%",class="btn-success"),title="states and thresholds",width=2),
              column(actionButton("axesRORegimeOUP","Axes",width="100%",class="btn-success"),title="for x",width=2),
              column(actionButton("plotRORegimeOUP","Plot",width="100%",class="btn-success"),title="refresh plot",width=2),
              column(actionButton("otherRORegimeOUP",HTML("&lessgtr;"),width="100%",class="btn-success"),title="other plot",style="padding-right: 2px;",width=1)
            ),
            # Plot
            wellPanel(class="wellPlotOUP",style="margin: 0 auto; height: 402px; width: 580px;",plotlyOutput("plotlyRORegimeOUP"),copyPlot()),
            value="RORegimeOUP"
          ),
          # Decision Threshold ----
          nav_panel("Decision Threshold",
            # User input
            fixedRow(style="height: 60px;",
              column(actionButton("infoRODecisionOUP","Info",width="100%",class="btn-primary"),title="information about Decision Threshold",style="padding-top: 32px;",width=2),
              column(numericInput("yRODecisionOUP",label="y",value="",step="any",width="100%"),title="fixed terminal state",width=2),
              column(numericInput("xFromRODecisionOUP",label="x:From",value="",step="any",width="100%"),title="stochastic states",width=2),
              column(numericInput("xToRODecisionOUP",label="x:To",value="",step="any",width="100%"),title="stochastic states",width=2),
              column(numericInput("xByRODecisionOUP",label="x:By",value="",step="any",width="100%"),title="state increment",width=2)
            ),
            fixedRow(style="height: 60px;",
              column(numericInput("rhoRODecisionOUP",label="rho",value="",step="any",width="100%"),title="rate of convergence",width=2),
              column(numericInput("muRODecisionOUP",label="mu",value="",step="any",width="100%"),title="location",width=2),
              column(numericInput("sigmaRODecisionOUP",label="sigma",value="",step="any",width="100%"),title="scale",width=2),
              column(numericInput("rRODecisionOUP",label="r",value="",step="any",width="100%"),title="discount rate of convergence",width=2),
              column(numericInput("phiRODecisionOUP",label="phi",value="",step="any",width="100%"),title="exit or entry option",width=2)
            ),
            fixedRow(style="height: 68px;",
              column(width=2),
              column(width=2),
              column(numericInput("bRODecisionOUP",label="b",value="",step="any",width="100%"),title="entry benefit",width=2),
              column(numericInput("cRODecisionOUP",label="c",value="",step="any",width="100%"),title="exit cost",width=2)
            ),
            # User action
            fixedRow(
              column(actionButton("clearRODecisionOUP",HTML("_"),width="100%",class="btn-info"),title="clear and save arguments",style="padding-right: 2px;",width=1),
              column(actionButton("saveRODecisionOUP",HTML("&equiv;"),width="100%",class="btn-info"),title="save arguments",style="padding-left: 2px;",width=1),
              column(actionButton("undnRODecisionOUP",HTML("&Vee;"),width="100%",class="btn-success"),title="previous arguments",style="padding-right: 2px;",width=1),
              column(actionButton("unupRODecisionOUP",HTML("&Wedge;"),width="100%",class="btn-success"),title="next arguments",style="padding-left: 2px;",width=1),
              column(actionButton("syncRODecisionOUP","Sync",width="100%",class="btn-success"),title="states and thresholds",width=2),
              column(actionButton("axesRODecisionOUP","Axes",width="100%",class="btn-success"),title="for x",width=2),
              column(actionButton("plotRODecisionOUP","Plot",width="100%",class="btn-success"),title="refresh plot",width=2)
            ),
            # Plot
            wellPanel(class="wellPlotOUP",style="margin: 0 auto; height: 402px; width: 580px;",plotlyOutput("plotlyRODecisionOUP"),copyPlot()),
            value="RODecisionOUP"
          ),
          # Passage Time ----
          nav_panel("Passage Times",
            # User input
            fixedRow(style="height: 60px;",
              column(actionButton("infoROPassageTimeOUP","Info",width="100%",class="btn-primary"),title="information about Passage Time",style="padding-top: 32px;",width=2),
              column(numericInput("xROPassageTimeOUP",label="x",value="",step="any",width="100%"),title="fixed initial state",width=2),
              column(numericInput("zFromROPassageTimeOUP",label="z:From",value="",step="any",width="100%"),title="alternate initial states",width=2),
              column(numericInput("zToROPassageTimeOUP",label="z:To",value="",step="any",width="100%"),title="alternate initial states",width=2),
              column(numericInput("zByROPassageTimeOUP",label="z:By",value="",step="any",width="100%"),title="state increment",width=2)
            ),
            fixedRow(style="height: 60px;",
              column(numericInput("rhoROPassageTimeOUP",label="rho",value="",step="any",width="100%"),title="rate of convergence",width=2),
              column(numericInput("muROPassageTimeOUP",label="mu",value="",step="any",width="100%"),title="location",width=2),
              column(numericInput("sigmaROPassageTimeOUP",label="sigma",value="",step="any",width="100%"),title="scale",width=2),
              column(width=2),
              column(numericInput("kROPassageTimeOUP",label="k",value="",step="any",width="100%"),title="threshold",width=2)
            ),
            fixedRow(style="height: 68px;",
              column(numericInput("PpctROPassageTimeOUP",label="P%",value="",step="any",width="100%"),title="passage time probability",width=2),
              column(width=2),
              column(width=2),
              column(numericInput("omegaROPassageTimeOUP",label="omega",value="",step="any",width="100%"),title="degree of irreversibility",width=2),
              column(numericInput("sROPassageTimeOUP",label="s",value="",step="any",width="100%"),title="fixed initial time",width=2)
            ),
            # User action
            fixedRow(
              column(actionButton("clearROPassageTimeOUP",HTML("_"),width="100%",class="btn-info"),title="clear and save arguments",style="padding-right: 2px;",width=1),
              column(actionButton("saveROPassageTimeOUP",HTML("&equiv;"),width="100%",class="btn-info"),title="save arguments",style="padding-left: 2px;",width=1),
              column(actionButton("undnROPassageTimeOUP",HTML("&Vee;"),width="100%",class="btn-success"),title="previous arguments",style="padding-right: 2px;",width=1),
              column(actionButton("unupROPassageTimeOUP",HTML("&Wedge;"),width="100%",class="btn-success"),title="next arguments",style="padding-left: 2px;",width=1),
              column(actionButton("syncROPassageTimeOUP","Sync",width="100%",class="btn-success"),title="states and thresholds",width=2),
              column(actionButton("axesROPassageTimeOUP","Axes",width="100%",class="btn-success"),title="for t and z",width=2),
              column(actionButton("plotROPassageTimeOUP","Plot",width="100%",class="btn-success"),title="refresh plot",width=2),
              column(actionButton("leftROPassageTimeOUP","<",width="100%",class="btn-success"),title="previous plot",style="padding-right: 2px;",width=1),
              column(actionButton("rghtROPassageTimeOUP",">",width="100%",class="btn-success"),title="next plot",style="padding-left: 2px;",width=1)
            ),
            # Plot
            wellPanel(class="wellPlotOUP",style="margin: 0 auto; height: 402px; width: 580px;",plotlyOutput("plotlyROPassageTimeOUP"),copyPlot()),
            value="ROPassageTimeOUP"
          ),
          id="navROOUP",widths=c(3,9)
        ),
        value="tabROOUP"
        #end list ----
      ),
      nav_panel("Analytical",
        tags$head(HTML('<html lang="en"> <link rel="icon" href="favicon.png" type="image/png" sizes="16x16">'),
                  tags$link(rel="stylesheet",type="text/css",href="styles.css")),
        add_busy_spinner(spin="radar",color="rgb(0,90,46)",timeout=1000,position=c("top-right"),margins=c(450,350),height="128px",width="128px"),
        navset_pill_list(
          # Drift ----
          nav_panel("Drift",
            # User input
            fixedRow(style="height: 60px;",
              column(actionButton("infoADriftOUP","Info",width="100%",class="btn-primary"),title="information about Drift",style="padding-top: 32px;",width=2),
              column(width=2),
              column(numericInput("zFromADriftOUP",label="z:From",value="",step="any",width="100%"),title="stochastic states",width=2),
              column(numericInput("zToADriftOUP",label="z:To",value="",step="any",width="100%"),title="stochastic states",width=2),
              column(numericInput("zByADriftOUP",label="z:By",value="",step="any",width="100%"),title="state increment",width=2)
            ),
            fixedRow(style="height: 144px;",
              column(numericInput("rhoADriftOUP",label="rho",value="",step="any",width="100%"),title="rate of convergence",width=2),
              column(numericInput("muADriftOUP",label="mu",value="",step="any",width="100%"),title="location",width=2)
            ),
            # User action
            fixedRow(
              column(actionButton("clearADriftOUP",HTML("_"),width="100%",class="btn-info"),title="clear and save arguments",style="padding-right: 2px;",width=1),
              column(actionButton("saveADriftOUP",HTML("&equiv;"),width="100%",class="btn-info"),title="save arguments",style="padding-left: 2px;",width=1),
              column(actionButton("undnADriftOUP",HTML("&Vee;"),width="100%",class="btn-success"),title="previous arguments",style="padding-right: 2px;",width=1),
              column(actionButton("unupADriftOUP",HTML("&Wedge;"),width="100%",class="btn-success"),title="next arguments",style="padding-left: 2px;",width=1),
              column(actionButton("syncADriftOUP","Sync",width="100%",class="btn-success"),title="states and thresholds",width=2),
              column(actionButton("axesADriftOUP","Axes",width="100%",class="btn-success"),title="for z",width=2),
              column(actionButton("plotADriftOUP","Plot",width="100%",class="btn-success"),title="refresh plot",width=2)
            ),
            # Plot
            wellPanel(class="wellPlotOUP",style="margin: 0 auto; height: 402px; width: 580px;",plotlyOutput("plotlyADriftOUP"),copyPlot()),
            value="ADriftOUP"
          ),
          # Diffusion ----
          nav_panel("Diffusion",
            # User input
            fixedRow(style="height: 60px;",
              column(actionButton("infoADiffusionOUP","Info",width="100%",class="btn-primary"),title="information about Diffusion",style="padding-top: 32px;",width=2),
              column(width=2),
              column(numericInput("zFromADiffusionOUP",label="z:From",value="",step="any",width="100%"),title="stochastic states",width=2),
              column(numericInput("zToADiffusionOUP",label="z:To",value="",step="any",width="100%"),title="stochastic states",width=2),
              column(numericInput("zByADiffusionOUP",label="z:By",value="",step="any",width="100%"),title="state increment",width=2)
            ),
            fixedRow(style="height: 144px;",
              column(numericInput("rhoADiffusionOUP",label="rho",value="",step="any",width="100%"),title="rate of convergence",width=2),
              column(numericInput("muADiffusionOUP",label="mu",value="",step="any",width="100%"),title="location",width=2),
              column(numericInput("sigmaADiffusionOUP",label="sigma",value="",step="any",width="100%"),title="scale",width=2)
            ),
            # User action
            fixedRow(
              column(actionButton("clearADiffusionOUP",HTML("_"),width="100%",class="btn-info"),title="clear and save arguments",style="padding-right: 2px;",width=1),
              column(actionButton("saveADiffusionOUP",HTML("&equiv;"),width="100%",class="btn-info"),title="save arguments",style="padding-left: 2px;",width=1),
              column(actionButton("undnADiffusionOUP",HTML("&Vee;"),width="100%",class="btn-success"),title="previous arguments",style="padding-right: 2px;",width=1),
              column(actionButton("unupADiffusionOUP",HTML("&Wedge;"),width="100%",class="btn-success"),title="next arguments",style="padding-left: 2px;",width=1),
              column(actionButton("syncADiffusionOUP","Sync",width="100%",class="btn-success"),title="states and thresholds",width=2),
              column(actionButton("axesADiffusionOUP","Axes",width="100%",class="btn-success"),title="for z",width=2),
              column(actionButton("plotADiffusionOUP","Plot",width="100%",class="btn-success"),title="refresh plot",width=2),
              column(actionButton("otherADiffusionOUP",HTML("&lessgtr;"),width="100%",class="btn-success"),title="other plot",style="padding-right: 2px;",width=1)
            ),
            # Plot
            wellPanel(class="wellPlotOUP",style="margin: 0 auto; height: 402px; width: 580px;",plotlyOutput("plotlyADiffusionOUP"),copyPlot()),
            value="ADiffusionOUP"
          ),
          # Mean ----
          nav_panel("Mean",
            # User input
            fixedRow(style="height: 60px;",
              column(actionButton("infoAMeanOUP","Info",width="100%",class="btn-primary"),title="information about Mean",style="padding-top: 32px;",width=2),
              column(numericInput("xAMeanOUP",label="x",value="",step="any",width="100%"),title="fixed initial state",width=2),
              column(numericInput("yFromAMeanOUP",label="y:From",value="",step="any",width="100%"),title="stochastic states",width=2),
              column(numericInput("yToAMeanOUP",label="y:To",value="",step="any",width="100%"),title="stochastic states",width=2),
              column(numericInput("yByAMeanOUP",label="y:By",value="",step="any",width="100%"),title="state increment",width=2),
              column(numericInput("tByAMeanOUP",label="t:By",value="",step="any",width="100%"),title="time increment",width=2)
            ),
            fixedRow(style="height: 60px;",
              column(numericInput("rhoAMeanOUP",label="rho",value="",step="any",width="100%"),title="rate of convergence",width=2),
              column(numericInput("muAMeanOUP",label="mu",value="",step="any",width="100%"),title="location",width=2),
              column(numericInput("sigmaAMeanOUP",label="sigma",value="",step="any",width="100%"),title="scale",width=2),
              column(width=2),
              column(numericInput("psiAMeanOUP",label="psi",value="",step="any",width="100%"),title="-inf to y or y to inf",width=2),
              column(numericInput("tToAMeanOUP",label="t:To",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            fixedRow(style="height: 68px;",
              column(width=2),
              column(width=2),
              column(numericInput("pmaxAMeanOUP",label="p max",value="",step="any",width="100%"),title="maximum density",width=2),
              column(numericInput("epsAMeanOUP",label="epsilon",value="",step="any",width="100%"),title="proportion remaining",width=2),
              column(numericInput("sAMeanOUP",label="s",value="",step="any",width="100%"),title="fixed initial time",width=2),
              column(numericInput("tFromAMeanOUP",label="t:From",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            # User action
            fixedRow(
              column(actionButton("clearAMeanOUP",HTML("_"),width="100%",class="btn-info"),title="clear and save arguments",style="padding-right: 2px;",width=1),
              column(actionButton("saveAMeanOUP",HTML("&equiv;"),width="100%",class="btn-info"),title="save arguments",style="padding-left: 2px;",width=1),
              column(actionButton("undnAMeanOUP",HTML("&Vee;"),width="100%",class="btn-success"),title="previous arguments",style="padding-right: 2px;",width=1),
              column(actionButton("unupAMeanOUP",HTML("&Wedge;"),width="100%",class="btn-success"),title="next arguments",style="padding-left: 2px;",width=1),
              column(actionButton("syncAMeanOUP","Sync",width="100%",class="btn-success"),title="states and thresholds",width=2),
              column(actionButton("axesAMeanOUP","Axes",width="100%",class="btn-success"),title="for t, y and p",width=2),
              column(actionButton("plotAMeanOUP","Plot",width="100%",class="btn-success"),title="refresh plot",width=2),
              column(actionButton("leftAMeanOUP","<",width="100%",class="btn-success"),title="previous plot",style="padding-right: 2px;",width=1),
              column(actionButton("rghtAMeanOUP",">",width="100%",class="btn-success"),title="next plot",style="padding-left: 2px;",width=1)
            ),
            # Plot
            wellPanel(class="wellPlotOUP",style="margin: 0 auto; height: 402px; width: 580px;",plotlyOutput("plotlyAMeanOUP"),copyPlot()),
            value="AMeanOUP"
          ),
          # Variance ----
          nav_panel("Variance",
            # User input
            fixedRow(style="height: 60px;",
              column(actionButton("infoAVarianceOUP","Info",width="100%",class="btn-primary"),title="information about Variance",style="padding-top: 32px;",width=2),
              column(numericInput("xAVarianceOUP",label="x",value="",step="any",width="100%"),title="fixed initial state",width=2),
              column(numericInput("yFromAVarianceOUP",label="y:From",value="",step="any",width="100%"),title="stochastic states",width=2),
              column(numericInput("yToAVarianceOUP",label="y:To",value="",step="any",width="100%"),title="stochastic states",width=2),
              column(numericInput("yByAVarianceOUP",label="y:By",value="",step="any",width="100%"),title="state increment",width=2),
              column(numericInput("tByAVarianceOUP",label="t:By",value="",step="any",width="100%"),title="time increment",width=2)
            ),
            fixedRow(style="height: 60px;",
              column(numericInput("rhoAVarianceOUP",label="rho",value="",step="any",width="100%"),title="rate of convergence",width=2),
              column(numericInput("muAVarianceOUP",label="mu",value="",step="any",width="100%"),title="location",width=2),
              column(numericInput("sigmaAVarianceOUP",label="sigma",value="",step="any",width="100%"),title="scale",width=2),
              column(width=2),
              column(numericInput("psiAVarianceOUP",label="psi",value="",step="any",width="100%"),title="-inf to y or y to inf",width=2),
              column(numericInput("tToAVarianceOUP",label="t:To",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            fixedRow(style="height: 68px;",
              column(width=2),
              column(width=2),
              column(numericInput("pmaxAVarianceOUP",label="p max",value="",step="any",width="100%"),title="maximum density",width=2),
              column(numericInput("epsAVarianceOUP",label="epsilon",value="",step="any",width="100%"),title="proportion remaining",width=2),
              column(numericInput("sAVarianceOUP",label="s",value="",step="any",width="100%"),title="fixed initial time",width=2),
              column(numericInput("tFromAVarianceOUP",label="t:From",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            # User action
            fixedRow(
              column(actionButton("clearAVarianceOUP",HTML("_"),width="100%",class="btn-info"),title="clear and save arguments",style="padding-right: 2px;",width=1),
              column(actionButton("saveAVarianceOUP",HTML("&equiv;"),width="100%",class="btn-info"),title="save arguments",style="padding-left: 2px;",width=1),
              column(actionButton("undnAVarianceOUP",HTML("&Vee;"),width="100%",class="btn-success"),title="previous arguments",style="padding-right: 2px;",width=1),
              column(actionButton("unupAVarianceOUP",HTML("&Wedge;"),width="100%",class="btn-success"),title="next arguments",style="padding-left: 2px;",width=1),
              column(actionButton("syncAVarianceOUP","Sync",width="100%",class="btn-success"),title="states and thresholds",width=2),
              column(actionButton("axesAVarianceOUP","Axes",width="100%",class="btn-success"),title="for t, y and p",width=2),
              column(actionButton("plotAVarianceOUP","Plot",width="100%",class="btn-success"),title="refresh plot",width=2),
              column(actionButton("leftAVarianceOUP","<",width="100%",class="btn-success"),title="previous plot",style="padding-right: 2px;",width=1),
              column(actionButton("rghtAVarianceOUP",">",width="100%",class="btn-success"),title="next plot",style="padding-left: 2px;",width=1)
            ),
            # Plot
            wellPanel(class="wellPlotOUP",style="margin: 0 auto; height: 402px; width: 580px;",plotlyOutput("plotlyAVarianceOUP"),copyPlot()),
            value="AVarianceOUP"
          ),
          # Transition Density ----
          nav_panel("Transition Density",
            # User input
            fixedRow(style="height: 60px;",
              column(actionButton("infoADensityOUP","Info",width="100%",class="btn-primary"),title="information about Transition Density",style="padding-top: 32px;",width=2),
              column(numericInput("xADensityOUP",label="x",value="",step="any",width="100%"),title="fixed initial state",width=2),
              column(numericInput("yFromADensityOUP",label="y:From",value="",step="any",width="100%"),title="stochastic states",width=2),
              column(numericInput("yToADensityOUP",label="y:To",value="",step="any",width="100%"),title="stochastic states",width=2),
              column(numericInput("yByADensityOUP",label="y:By",value="",step="any",width="100%"),title="state increment",width=2),
              column(numericInput("tByADensityOUP",label="t:By",value="",step="any",width="100%"),title="time increment",width=2)
            ),
            fixedRow(style="height: 60px;",
              column(numericInput("rhoADensityOUP",label="rho",value="",step="any",width="100%"),title="rate of convergence",width=2),
              column(numericInput("muADensityOUP",label="mu",value="",step="any",width="100%"),title="location",width=2),
              column(numericInput("sigmaADensityOUP",label="sigma",value="",step="any",width="100%"),title="scale",width=2),
              column(width=2),
              column(width=2),
              column(numericInput("tToADensityOUP",label="t:To",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            fixedRow(style="height: 68px;",
              column(width=2),
              column(width=2),
              column(numericInput("pmaxADensityOUP",label="p max",value="",step="any",width="100%"),title="maximum density",width=2),
              column(width=2),
              column(numericInput("sADensityOUP",label="s",value="",step="any",width="100%"),title="fixed initial time",width=2),
              column(numericInput("tFromADensityOUP",label="t:From",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            # User action
            fixedRow(
              column(actionButton("clearADensityOUP",HTML("_"),width="100%",class="btn-info"),title="clear and save arguments",style="padding-right: 2px;",width=1),
              column(actionButton("saveADensityOUP",HTML("&equiv;"),width="100%",class="btn-info"),title="save arguments",style="padding-left: 2px;",width=1),
              column(actionButton("undnADensityOUP",HTML("&Vee;"),width="100%",class="btn-success"),title="previous arguments",style="padding-right: 2px;",width=1),
              column(actionButton("unupADensityOUP",HTML("&Wedge;"),width="100%",class="btn-success"),title="next arguments",style="padding-left: 2px;",width=1),
              column(actionButton("syncADensityOUP","Sync",width="100%",class="btn-success"),title="states and thresholds",width=2),
              column(actionButton("axesADensityOUP","Axes",width="100%",class="btn-success"),title="for t, y and p",width=2),
              column(actionButton("plotADensityOUP","Plot",width="100%",class="btn-success"),title="refresh plot",width=2),
              column(actionButton("otherADensityOUP",HTML("&lessgtr;"),width="100%",class="btn-success"),title="other plot",style="padding-right: 2px;",width=1)
            ),
            # Plot
            wellPanel(class="wellPlotOUP",style="margin: 0 auto; height: 402px; width: 580px;",plotlyOutput("plotlyADensityOUP"),copyPlot()),
            value="ADensityOUP"
          ),
          # Transition Probability ----
          nav_panel("Transition Probability",
            # User input
            fixedRow(style="height: 60px;",
              column(actionButton("infoAProbabilityOUP","Info",width="100%",class="btn-primary"),title="information about Transition Probability",style="padding-top: 32px;",width=2),
              column(numericInput("xAProbabilityOUP",label="x",value="",step="any",width="100%"),title="fixed initial state",width=2),
              column(numericInput("yFromAProbabilityOUP",label="y:From",value="",step="any",width="100%"),title="stochastic states",width=2),
              column(numericInput("yToAProbabilityOUP",label="y:To",value="",step="any",width="100%"),title="stochastic states",width=2),
              column(numericInput("yByAProbabilityOUP",label="y:By",value="",step="any",width="100%"),title="state increment",width=2),
              column(numericInput("tByAProbabilityOUP",label="t:By",value="",step="any",width="100%"),title="time increment",width=2)
            ),
            fixedRow(style="height: 60px;",
              column(numericInput("rhoAProbabilityOUP",label="rho",value="",step="any",width="100%"),title="rate of convergence",width=2),
              column(numericInput("muAProbabilityOUP",label="mu",value="",step="any",width="100%"),title="location",width=2),
              column(numericInput("sigmaAProbabilityOUP",label="sigma",value="",step="any",width="100%"),title="scale",width=2),
              column(width=2),
              column(numericInput("psiAProbabilityOUP",label="psi",value="",step="any",width="100%"),title="-inf to y or y to inf",width=2),
              column(numericInput("tToAProbabilityOUP",label="t:To",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            fixedRow(style="height: 68px;",
              column(width=2),
              column(width=2),
              column(width=2),
              column(width=2),
              column(numericInput("sAProbabilityOUP",label="s",value="",step="any",width="100%"),title="fixed initial time",width=2),
              column(numericInput("tFromAProbabilityOUP",label="t:From",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            # User action
            fixedRow(
              column(actionButton("clearAProbabilityOUP",HTML("_"),width="100%",class="btn-info"),title="clear and save arguments",style="padding-right: 2px;",width=1),
              column(actionButton("saveAProbabilityOUP",HTML("&equiv;"),width="100%",class="btn-info"),title="save arguments",style="padding-left: 2px;",width=1),
              column(actionButton("undnAProbabilityOUP",HTML("&Vee;"),width="100%",class="btn-success"),title="previous arguments",style="padding-right: 2px;",width=1),
              column(actionButton("unupAProbabilityOUP",HTML("&Wedge;"),width="100%",class="btn-success"),title="next arguments",style="padding-left: 2px;",width=1),
              column(actionButton("syncAProbabilityOUP","Sync",width="100%",class="btn-success"),title="states and thresholds",width=2),
              column(actionButton("axesAProbabilityOUP","Axes",width="100%",class="btn-success"),title="for t, y and p",width=2),
              column(actionButton("plotAProbabilityOUP","Plot",width="100%",class="btn-success"),title="refresh plot",width=2),
              column(actionButton("otherAProbabilityOUP",HTML("&lessgtr;"),width="100%",class="btn-success"),title="other plot",style="padding-right: 2px;",width=1)
            ),
            # Plot
            wellPanel(class="wellPlotOUP",style="margin: 0 auto; height: 402px; width: 580px;",plotlyOutput("plotlyAProbabilityOUP"),copyPlot()),
            value="AProbabilityOUP"
          ),
          # Double Integral ----
          nav_panel("Double Integral",
            # User input
            fixedRow(style="height: 60px;",
              column(actionButton("infoADoubleOUP","Info",width="100%",class="btn-primary"),title="information about Double Integral",style="padding-top: 32px;",width=2),
              column(numericInput("xADoubleOUP",label="x",value="",step="any",width="100%"),title="fixed initial state",width=2),
              column(numericInput("yFromADoubleOUP",label="y:From",value="",step="any",width="100%"),title="stochastic states",width=2),
              column(numericInput("yToADoubleOUP",label="y:To",value="",step="any",width="100%"),title="stochastic states",width=2),
              column(numericInput("yByADoubleOUP",label="y:By",value="",step="any",width="100%"),title="state increment",width=2),
              column(numericInput("tByADoubleOUP",label="t:By",value="",step="any",width="100%"),title="time increment",width=2)
            ),
            fixedRow(style="height: 60px;",
              column(numericInput("rhoADoubleOUP",label="rho",value="",step="any",width="100%"),title="rate of convergence",width=2),
              column(numericInput("muADoubleOUP",label="mu",value="",step="any",width="100%"),title="location",width=2),
              column(numericInput("sigmaADoubleOUP",label="sigma",value="",step="any",width="100%"),title="scale",width=2),
              column(width=2),
              column(numericInput("psiADoubleOUP",label="psi",value="",step="any",width="100%"),title="-inf to y or y to inf",width=2),
              column(numericInput("tToADoubleOUP",label="t:To",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            fixedRow(style="height: 68px;",
              column(width=2),
              column(width=2),
              column(width=2),
              column(width=2),
              column(numericInput("sADoubleOUP",label="s",value="",step="any",width="100%"),title="fixed initial time",width=2),
              column(numericInput("tFromADoubleOUP",label="t:From",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            # User action
            fixedRow(
              column(actionButton("clearADoubleOUP",HTML("_"),width="100%",class="btn-info"),title="clear and save arguments",style="padding-right: 2px;",width=1),
              column(actionButton("saveADoubleOUP",HTML("&equiv;"),width="100%",class="btn-info"),title="save arguments",style="padding-left: 2px;",width=1),
              column(actionButton("undnADoubleOUP",HTML("&Vee;"),width="100%",class="btn-success"),title="previous arguments",style="padding-right: 2px;",width=1),
              column(actionButton("unupADoubleOUP",HTML("&Wedge;"),width="100%",class="btn-success"),title="next arguments",style="padding-left: 2px;",width=1),
              column(actionButton("syncADoubleOUP","Sync",width="100%",class="btn-success"),title="states and thresholds",width=2),
              column(actionButton("axesADoubleOUP","Axes",width="100%",class="btn-success"),title="for t, y and p",width=2),
              column(actionButton("plotADoubleOUP","Plot",width="100%",class="btn-success"),title="refresh plot",width=2),
              column(actionButton("otherADoubleOUP",HTML("&lessgtr;"),width="100%",class="btn-success"),title="other plot",style="padding-right: 2px;",width=1)
            ),
            # Plot
            wellPanel(class="wellPlotOUP",style="margin: 0 auto; height: 402px; width: 580px;",plotlyOutput("plotlyADoubleOUP"),copyPlot()),
            value="ADoubleOUP"
          ),
          # Option ----
          nav_panel("Option",
            # User input
            fixedRow(style="height: 60px;",
              column(actionButton("infoAOptionOUP","Info",width="100%",class="btn-primary"),title="information about Option",style="padding-top: 32px;",width=2),
              column(numericInput("yAOptionOUP",label="y",value="",step="any",width="100%"),title="fixed terminal state",width=2),
              column(numericInput("xFromAOptionOUP",label="x:From",value="",step="any",width="100%"),title="stochastic states",width=2),
              column(numericInput("xToAOptionOUP",label="x:To",value="",step="any",width="100%"),title="stochastic states",width=2),
              column(numericInput("xByAOptionOUP",label="x:By",value="",step="any",width="100%"),title="state increment",width=2),
              column(numericInput("sByAOptionOUP",label="s:By",value="",step="any",width="100%"),title="time increment",width=2)
            ),
            fixedRow(style="height: 60px;",
              column(numericInput("rhoAOptionOUP",label="rho",value="",step="any",width="100%"),title="rate of convergence",width=2),
              column(numericInput("muAOptionOUP",label="mu",value="",step="any",width="100%"),title="location",width=2),
              column(numericInput("sigmaAOptionOUP",label="sigma",value="",step="any",width="100%"),title="scale",width=2),
              column(numericInput("rAOptionOUP",label="r",value="",step="any",width="100%"),title="discount rate of convergence",width=2),
              column(numericInput("phiAOptionOUP",label="phi",value="",step="any",width="100%"),title="exit or entry option",width=2),
              column(numericInput("sToAOptionOUP",label="s:To",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            fixedRow(style="height: 68px;",
              column(width=2),
              column(width=2),
              column(numericInput("bAOptionOUP",label="b",value="",step="any",width="100%"),title="entry benefit",width=2),
              column(numericInput("cAOptionOUP",label="c",value="",step="any",width="100%"),title="exit cost",width=2),
              column(numericInput("tAOptionOUP",label="t",value="",step="any",width="100%"),title="fixed terminal time",width=2),
              column(numericInput("sFromAOptionOUP",label="s:From",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            # User action
            fixedRow(
              column(actionButton("clearAOptionOUP",HTML("_"),width="100%",class="btn-info"),title="clear and save arguments",style="padding-right: 2px;",width=1),
              column(actionButton("saveAOptionOUP",HTML("&equiv;"),width="100%",class="btn-info"),title="save arguments",style="padding-left: 2px;",width=1),
              column(actionButton("undnAOptionOUP",HTML("&Vee;"),width="100%",class="btn-success"),title="previous arguments",style="padding-right: 2px;",width=1),
              column(actionButton("unupAOptionOUP",HTML("&Wedge;"),width="100%",class="btn-success"),title="next arguments",style="padding-left: 2px;",width=1),
              column(actionButton("syncAOptionOUP","Sync",width="100%",class="btn-success"),title="states and thresholds",width=2),
              column(actionButton("axesAOptionOUP","Axes",width="100%",class="btn-success"),title="for s and x",width=2),
              column(actionButton("plotAOptionOUP","Plot",width="100%",class="btn-success"),title="refresh plot",width=2),
              column(actionButton("otherAOptionOUP",HTML("&lessgtr;"),width="100%",class="btn-success"),title="other plot",style="padding-right: 2px;",width=1)
            ),
            # Plot
            wellPanel(class="wellPlotOUP",style="margin: 0 auto; height: 402px; width: 580px;",plotlyOutput("plotlyAOptionOUP"),copyPlot()),
            value="AOptionOUP"
          ),
          # Option Envelope ----
          nav_panel("Option Envelope",
            # User input
            fixedRow(style="height: 60px;",
              column(actionButton("infoAEnvelopeOUP","Info",width="100%",class="btn-primary"),title="information about Option Envelope",style="padding-top: 32px;",width=2),
              column(numericInput("yAEnvelopeOUP",label="y",value="",step="any",width="100%"),title="fixed terminal state",width=2),
              column(numericInput("xFromAEnvelopeOUP",label="x:From",value="",step="any",width="100%"),title="stochastic states",width=2),
              column(numericInput("xToAEnvelopeOUP",label="x:To",value="",step="any",width="100%"),title="stochastic states",width=2),
              column(numericInput("xByAEnvelopeOUP",label="x:By",value="",step="any",width="100%"),title="state increment",width=2),
              column(numericInput("sByAEnvelopeOUP",label="s:By",value="",step="any",width="100%"),title="time increment",width=2)
            ),
            fixedRow(style="height: 60px;",
              column(numericInput("rhoAEnvelopeOUP",label="rho",value="",step="any",width="100%"),title="rate of convergence",width=2),
              column(numericInput("muAEnvelopeOUP",label="mu",value="",step="any",width="100%"),title="location",width=2),
              column(numericInput("sigmaAEnvelopeOUP",label="sigma",value="",step="any",width="100%"),title="scale",width=2),
              column(numericInput("rAEnvelopeOUP",label="r",value="",step="any",width="100%"),title="discount rate",width=2),
              column(numericInput("phiAEnvelopeOUP",label="phi",value="",step="any",width="100%"),title="exit or entry option",width=2),
              column(numericInput("sToAEnvelopeOUP",label="s:To",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            fixedRow(style="height: 68px;",
              column(width=2),
              column(width=2),
              column(numericInput("bAEnvelopeOUP",label="b",value="",step="any",width="100%"),title="entry benefit",width=2),
              column(numericInput("cAEnvelopeOUP",label="c",value="",step="any",width="100%"),title="exit cost",width=2),
              column(numericInput("tAEnvelopeOUP",label="t",value="",step="any",width="100%"),title="fixed terminal time",width=2),
              column(numericInput("sFromAEnvelopeOUP",label="s:From",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            # User action
            fixedRow(
              column(actionButton("clearAEnvelopeOUP",HTML("_"),width="100%",class="btn-info"),title="clear and save arguments",style="padding-right: 2px;",width=1),
              column(actionButton("saveAEnvelopeOUP",HTML("&equiv;"),width="100%",class="btn-info"),title="save arguments",style="padding-left: 2px;",width=1),
              column(actionButton("undnAEnvelopeOUP",HTML("&Vee;"),width="100%",class="btn-success"),title="previous arguments",style="padding-right: 2px;",width=1),
              column(actionButton("unupAEnvelopeOUP",HTML("&Wedge;"),width="100%",class="btn-success"),title="next arguments",style="padding-left: 2px;",width=1),
              column(actionButton("syncAEnvelopeOUP","Sync",width="100%",class="btn-success"),title="states and thresholds",width=2),
              column(actionButton("axesAEnvelopeOUP","Axes",width="100%",class="btn-success"),title="for s and x",width=2),
              column(actionButton("plotAEnvelopeOUP","Plot",width="100%",class="btn-success"),title="refresh plot",width=2),
              column(actionButton("otherAEnvelopeOUP",HTML("&lessgtr;"),width="100%",class="btn-success"),title="other plot",style="padding-right: 2px;",width=1)
            ),
            # Plot
            wellPanel(class="wellPlotOUP",style="margin: 0 auto; height: 402px; width: 580px;",plotlyOutput("plotlyAEnvelopeOUP"),copyPlot()),
            value="AEnvelopeOUP"
          ),
          # Decision Threshold ----
          nav_panel("Decision Threshold",
            # User input
            fixedRow(style="height: 60px;",
              column(actionButton("infoADecisionOUP","Info",width="100%",class="btn-primary"),title="information about Decision Threshold",style="padding-top: 32px;",width=2),
              column(numericInput("yADecisionOUP",label="y",value="",step="any",width="100%"),title="fixed terminal state",width=2),
              column(numericInput("xFromADecisionOUP",label="x:From",value="",step="any",width="100%"),title="stochastic states",width=2),
              column(numericInput("xToADecisionOUP",label="x:To",value="",step="any",width="100%"),title="stochastic states",width=2),
              column(numericInput("xByADecisionOUP",label="x:By",value="",step="any",width="100%"),title="state increment",width=2)
            ),
            fixedRow(style="height: 60px;",
              column(numericInput("rhoADecisionOUP",label="rho",value="",step="any",width="100%"),title="rate of convergence",width=2),
              column(numericInput("muADecisionOUP",label="mu",value="",step="any",width="100%"),title="location",width=2),
              column(numericInput("sigmaADecisionOUP",label="sigma",value="",step="any",width="100%"),title="scale",width=2),
              column(numericInput("rADecisionOUP",label="r",value="",step="any",width="100%"),title="discount rate",width=2),
              column(numericInput("phiADecisionOUP",label="phi",value="",step="any",width="100%"),title="exit or entry option",width=2)
            ),
            fixedRow(style="height: 68px;",
              column(width=2),
              column(width=2),
              column(numericInput("bADecisionOUP",label="b",value="",step="any",width="100%"),title="entry benefit",width=2),
              column(numericInput("cADecisionOUP",label="c",value="",step="any",width="100%"),title="exit cost",width=2)
            ),
            # User action
            fixedRow(
              column(actionButton("clearADecisionOUP",HTML("_"),width="100%",class="btn-info"),title="clear and save arguments",style="padding-right: 2px;",width=1),
              column(actionButton("saveADecisionOUP",HTML("&equiv;"),width="100%",class="btn-info"),title="save arguments",style="padding-left: 2px;",width=1),
              column(actionButton("undnADecisionOUP",HTML("&Vee;"),width="100%",class="btn-success"),title="previous arguments",style="padding-right: 2px;",width=1),
              column(actionButton("unupADecisionOUP",HTML("&Wedge;"),width="100%",class="btn-success"),title="next arguments",style="padding-left: 2px;",width=1),
              column(actionButton("syncADecisionOUP","Sync",width="100%",class="btn-success"),title="states and thresholds",width=2),
              column(actionButton("axesADecisionOUP","Axes",width="100%",class="btn-success"),title="for s and x",width=2),
              column(actionButton("plotADecisionOUP","Plot",width="100%",class="btn-success"),title="refresh plot",width=2)
            ),
            # Plot
            wellPanel(class="wellPlotOUP",style="margin: 0 auto; height: 402px; width: 580px;",plotlyOutput("plotlyADecisionOUP"),copyPlot()),
            value="ADecisionOUP"
          ),
          # Obligation ----
          nav_panel("Obligation / Prohibition",
            # User input
            fixedRow(style="height: 60px;",
              column(actionButton("infoAObligationOUP","Info",width="100%",class="btn-primary"),title="information about Obligation",style="padding-top: 32px;",width=2),
              column(numericInput("yAObligationOUP",label="y",value="",step="any",width="100%"),title="fixed terminal state",width=2),
              column(numericInput("xFromAObligationOUP",label="x:From",value="",step="any",width="100%"),title="stochastic states",width=2),
              column(numericInput("xToAObligationOUP",label="x:To",value="",step="any",width="100%"),title="stochastic states",width=2),
              column(numericInput("xByAObligationOUP",label="x:By",value="",step="any",width="100%"),title="state increment",width=2),
              column(numericInput("sByAObligationOUP",label="s:By",value="",step="any",width="100%"),title="time increment",width=2)
            ),
            fixedRow(style="height: 60px;",
              column(numericInput("rhoAObligationOUP",label="rho",value="",step="any",width="100%"),title="rate of convergence",width=2),
              column(numericInput("muAObligationOUP",label="mu",value="",step="any",width="100%"),title="location",width=2),
              column(width=2),
              column(numericInput("rAObligationOUP",label="r",value="",step="any",width="100%"),title="discount rate",width=2),
              column(numericInput("phiAObligationOUP",label="phi",value="",step="any",width="100%"),title="exit or entry option",width=2),
              column(numericInput("sToAObligationOUP",label="s:To",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            fixedRow(style="height: 68px;",
              column(width=2),
              column(width=2),
              column(numericInput("bAObligationOUP",label="b",value="",step="any",width="100%"),title="entry benefit",width=2),
              column(numericInput("cAObligationOUP",label="c",value="",step="any",width="100%"),title="exit cost",width=2),
              column(numericInput("tAObligationOUP",label="t",value="",step="any",width="100%"),title="fixed terminal time",width=2),
              column(numericInput("sFromAObligationOUP",label="s:From",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            # User action
            fixedRow(
              column(actionButton("clearAObligationOUP",HTML("_"),width="100%",class="btn-info"),title="clear and save arguments",style="padding-right: 2px;",width=1),
              column(actionButton("saveAObligationOUP",HTML("&equiv;"),width="100%",class="btn-info"),title="save arguments",style="padding-left: 2px;",width=1),
              column(actionButton("undnAObligationOUP",HTML("&Vee;"),width="100%",class="btn-success"),title="previous arguments",style="padding-right: 2px;",width=1),
              column(actionButton("unupAObligationOUP",HTML("&Wedge;"),width="100%",class="btn-success"),title="next arguments",style="padding-left: 2px;",width=1),
              column(actionButton("syncAObligationOUP","Sync",width="100%",class="btn-success"),title="states and thresholds",width=2),
              column(actionButton("axesAObligationOUP","Axes",width="100%",class="btn-success"),title="for s and x",width=2),
              column(actionButton("plotAObligationOUP","Plot",width="100%",class="btn-success"),title="refresh plot",width=2),
              column(actionButton("otherAObligationOUP",HTML("&lessgtr;"),width="100%",class="btn-success"),title="other plot",style="padding-right: 2px;",width=1)
            ),
            # Plot
            wellPanel(class="wellPlotOUP",style="margin: 0 auto; height: 402px; width: 580px;",plotlyOutput("plotlyAObligationOUP"),copyPlot()),
            value="AObligationOUP"
          ),
          # Passage Time Mode, Median and Mean ----
          nav_item(tags$span(HTML("&ensp;&nbsp;Passage Time..."),style="color: var(--bs-nav-link-color); font-weight: bold;")),
          nav_panel(HTML("&emsp;Mode, Median and Mean"),
            # User input
            fixedRow(style="height: 60px;",
              column(actionButton("infoAPTModeMedianMeanOUP","Info",width="100%",class="btn-primary"),title="information about Passage Time Mode, Median and Mean",style="padding-top: 32px;",width=2),
              column(numericInput("xAPTModeMedianMeanOUP",label="x",value="",step="any",width="100%"),title="fixed initial state",width=2),
              column(numericInput("zFromAPTModeMedianMeanOUP",label="z:From",value="",step="any",width="100%"),title="alternate initial states",width=2),
              column(numericInput("zToAPTModeMedianMeanOUP",label="z:To",value="",step="any",width="100%"),title="alternate initial states",width=2),
              column(numericInput("zByAPTModeMedianMeanOUP",label="z:By",value="",step="any",width="100%"),title="state increment",width=2),
              column(numericInput("tByAPTModeMedianMeanOUP",label="t:By",value="",step="any",width="100%"),title="time increment",width=2)
            ),
            fixedRow(style="height: 60px;",
              column(numericInput("rhoAPTModeMedianMeanOUP",label="rho",value="",step="any",width="100%"),title="rate of convergence",width=2),
              column(numericInput("muAPTModeMedianMeanOUP",label="mu",value="",step="any",width="100%"),title="location",width=2),
              column(numericInput("sigmaAPTModeMedianMeanOUP",label="sigma",value="",step="any",width="100%"),title="scale",width=2),
              column(width=2),
              column(numericInput("kAPTModeMedianMeanOUP",label="k",value="",step="any",width="100%"),title="threshold",width=2),
              column(numericInput("tToAPTModeMedianMeanOUP",label="t:To",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            fixedRow(style="height: 68px;",
              column(width=2),
              column(numericInput("ptmaxAPTModeMedianMeanOUP",label="pt max",value="",step="any",width="100%"),title="maximum density",width=2),
              column(width=2),
              column(numericInput("omegaAPTModeMedianMeanOUP",label="omega",value="",step="any",width="100%"),title="degree of irreversibility",width=2),
              column(numericInput("sAPTModeMedianMeanOUP",label="s",value="",step="any",width="100%"),title="fixed initial time",width=2),
              column(numericInput("tFromAPTModeMedianMeanOUP",label="t:From",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            # User action
            fixedRow(
              column(actionButton("clearAPTModeMedianMeanOUP",HTML("_"),width="100%",class="btn-info"),title="clear and save arguments",style="padding-right: 2px;",width=1),
              column(actionButton("saveAPTModeMedianMeanOUP",HTML("&equiv;"),width="100%",class="btn-info"),title="save arguments",style="padding-left: 2px;",width=1),
              column(actionButton("undnAPTModeMedianMeanOUP",HTML("&Vee;"),width="100%",class="btn-success"),title="previous arguments",style="padding-right: 2px;",width=1),
              column(actionButton("unupAPTModeMedianMeanOUP",HTML("&Wedge;"),width="100%",class="btn-success"),title="next arguments",style="padding-left: 2px;",width=1),
              column(actionButton("syncAPTModeMedianMeanOUP","Sync",width="100%",class="btn-success"),title="states and thresholds",width=2),
              column(actionButton("axesAPTModeMedianMeanOUP","Axes",width="100%",class="btn-success"),title="for t, z and pt",width=2),
              column(actionButton("plotAPTModeMedianMeanOUP","Plot",width="100%",class="btn-success"),title="refresh plot",width=2),
              column(actionButton("leftAPTModeMedianMeanOUP","<",width="100%",class="btn-success"),title="previous plot",style="padding-right: 2px;",width=1),
              column(actionButton("rghtAPTModeMedianMeanOUP",">",width="100%",class="btn-success"),title="next plot",style="padding-left: 2px;",width=1)
            ),
            # Plot
            wellPanel(class="wellPlotOUP",style="margin: 0 auto; height: 402px; width: 580px;",plotlyOutput("plotlyAPTModeMedianMeanOUP"),copyPlot()),
            value="APTModeMedianMeanOUP"
          ),
          # Passage Time Percentiles ----
          nav_panel(HTML("&emsp;Percentiles"),
            # User input
            fixedRow(style="height: 60px;",
              column(actionButton("infoAPTPercentilesOUP","Info",width="100%",class="btn-primary"),title="information about Passage Time Percentiles",style="padding-top: 32px;",width=2),
              column(numericInput("xAPTPercentilesOUP",label="x",value="",step="any",width="100%"),title="fixed initial state",width=2),
              column(numericInput("zFromAPTPercentilesOUP",label="z:From",value="",step="any",width="100%"),title="alternate initial states",width=2),
              column(numericInput("zToAPTPercentilesOUP",label="z:To",value="",step="any",width="100%"),title="alternate initial states",width=2),
              column(numericInput("zByAPTPercentilesOUP",label="z:By",value="",step="any",width="100%"),title="state increment",width=2),
              column(numericInput("tByAPTPercentilesOUP",label="t:By",value="",step="any",width="100%"),title="time increment",width=2)
            ),
            fixedRow(style="height: 60px;",
              column(numericInput("rhoAPTPercentilesOUP",label="rho",value="",step="any",width="100%"),title="rate of convergence",width=2),
              column(numericInput("muAPTPercentilesOUP",label="mu",value="",step="any",width="100%"),title="location",width=2),
              column(numericInput("sigmaAPTPercentilesOUP",label="sigma",value="",step="any",width="100%"),title="scale",width=2),
              column(width=2),
              column(numericInput("kAPTPercentilesOUP",label="k",value="",step="any",width="100%"),title="threshold",width=2),
              column(numericInput("tToAPTPercentilesOUP",label="t:To",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            fixedRow(style="height: 68px;",
              column(numericInput("PpctAPTPercentilesOUP",label="P%",value="",step="any",width="100%"),title="passage time probability",width=2),
              column(numericInput("ptmaxAPTPercentilesOUP",label="pt max",value="",step="any",width="100%"),title="maximum density",width=2),
              column(width=2),
              column(numericInput("omegaAPTPercentilesOUP",label="omega",value="",step="any",width="100%"),title="degree of irreversibility",width=2),
              column(numericInput("sAPTPercentilesOUP",label="s",value="",step="any",width="100%"),title="fixed initial time",width=2),
              column(numericInput("tFromAPTPercentilesOUP",label="t:From",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            # User action
            fixedRow(
              column(actionButton("clearAPTPercentilesOUP",HTML("_"),width="100%",class="btn-info"),title="clear and save arguments",style="padding-right: 2px;",width=1),
              column(actionButton("saveAPTPercentilesOUP",HTML("&equiv;"),width="100%",class="btn-info"),title="save arguments",style="padding-left: 2px;",width=1),
              column(actionButton("undnAPTPercentilesOUP",HTML("&Vee;"),width="100%",class="btn-success"),title="previous arguments",style="padding-right: 2px;",width=1),
              column(actionButton("unupAPTPercentilesOUP",HTML("&Wedge;"),width="100%",class="btn-success"),title="next arguments",style="padding-left: 2px;",width=1),
              column(actionButton("syncAPTPercentilesOUP","Sync",width="100%",class="btn-success"),title="states and thresholds",width=2),
              column(actionButton("axesAPTPercentilesOUP","Axes",width="100%",class="btn-success"),title="for t, z and pt",width=2),
              column(actionButton("plotAPTPercentilesOUP","Plot",width="100%",class="btn-success"),title="refresh plot",width=2),
              column(actionButton("leftAPTPercentilesOUP","<",width="100%",class="btn-success"),title="previous plot",style="padding-right: 2px;",width=1),
              column(actionButton("rghtAPTPercentilesOUP",">",width="100%",class="btn-success"),title="next plot",style="padding-left: 2px;",width=1)
            ),
            # Plot
            wellPanel(class="wellPlotOUP",style="margin: 0 auto; height: 402px; width: 580px;",plotlyOutput("plotlyAPTPercentilesOUP"),copyPlot()),
            value="APTPercentilesOUP"
          ),
          # Passage Time Density ----
          nav_panel(HTML("&emsp;Density"),
            # User input
            fixedRow(style="height: 60px;",
              column(actionButton("infoAPTDensityOUP","Info",width="100%",class="btn-primary"),title="information about Passage Time Density",style="padding-top: 32px;",width=2),
              column(numericInput("xAPTDensityOUP",label="x",value="",step="any",width="100%"),title="fixed initial state",width=2),
              column(numericInput("zFromAPTDensityOUP",label="z:From",value="",step="any",width="100%"),title="alternate initial states",width=2),
              column(numericInput("zToAPTDensityOUP",label="z:To",value="",step="any",width="100%"),title="alternate initial states",width=2),
              column(numericInput("zByAPTDensityOUP",label="z:By",value="",step="any",width="100%"),title="state increment",width=2),
              column(numericInput("tByAPTDensityOUP",label="t:By",value="",step="any",width="100%"),title="time increment",width=2)
            ),
            fixedRow(style="height: 60px;",
              column(numericInput("rhoAPTDensityOUP",label="rho",value="",step="any",width="100%"),title="rate of convergence",width=2),
              column(numericInput("muAPTDensityOUP",label="mu",value="",step="any",width="100%"),title="location",width=2),
              column(numericInput("sigmaAPTDensityOUP",label="sigma",value="",step="any",width="100%"),title="scale",width=2),
              column(width=2),
              column(numericInput("kAPTDensityOUP",label="k",value="",step="any",width="100%"),title="threshold",width=2),
              column(numericInput("tToAPTDensityOUP",label="t:To",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            fixedRow(style="height: 68px;",
              column(width=2),
              column(numericInput("ptmaxAPTDensityOUP",label="pt max",value="",step="any",width="100%"),title="maximum density",width=2),
              column(width=2),
              column(numericInput("omegaAPTDensityOUP",label="omega",value="",step="any",width="100%"),title="degree of irreversibility",width=2),
              column(numericInput("sAPTDensityOUP",label="s",value="",step="any",width="100%"),title="fixed initial time",width=2),
              column(numericInput("tFromAPTDensityOUP",label="t:From",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            # User action
            fixedRow(
              column(actionButton("clearAPTDensityOUP",HTML("_"),width="100%",class="btn-info"),title="clear and save arguments",style="padding-right: 2px;",width=1),
              column(actionButton("saveAPTDensityOUP",HTML("&equiv;"),width="100%",class="btn-info"),title="save arguments",style="padding-left: 2px;",width=1),
              column(actionButton("undnAPTDensityOUP",HTML("&Vee;"),width="100%",class="btn-success"),title="previous arguments",style="padding-right: 2px;",width=1),
              column(actionButton("unupAPTDensityOUP",HTML("&Wedge;"),width="100%",class="btn-success"),title="next arguments",style="padding-left: 2px;",width=1),
              column(actionButton("syncAPTDensityOUP","Sync",width="100%",class="btn-success"),title="states and thresholds",width=2),
              column(actionButton("axesAPTDensityOUP","Axes",width="100%",class="btn-success"),title="for t, z and pt",width=2),
              column(actionButton("plotAPTDensityOUP","Plot",width="100%",class="btn-success"),title="refresh plot",width=2),
              column(actionButton("otherAPTDensityOUP",HTML("&lessgtr;"),width="100%",class="btn-success"),title="other plot",style="padding-right: 2px;",width=1)
            ),
            # Plot
            wellPanel(class="wellPlotOUP",style="margin: 0 auto; height: 402px; width: 580px;",plotlyOutput("plotlyAPTDensityOUP"),copyPlot()),
            value="APTDensityOUP"
          ),
          # Passage Time Probability ----
          nav_panel(HTML("&emsp;Probability"),
            # User input
            fixedRow(style="height: 60px;",
              column(actionButton("infoAPTProbabilityOUP","Info",width="100%",class="btn-primary"),title="information about Passage Time Probability",style="padding-top: 32px;",width=2),
              column(numericInput("xAPTProbabilityOUP",label="x",value="",step="any",width="100%"),title="fixed initial state",width=2),
              column(numericInput("zFromAPTProbabilityOUP",label="z:From",value="",step="any",width="100%"),title="alternate initial states",width=2),
              column(numericInput("zToAPTProbabilityOUP",label="z:To",value="",step="any",width="100%"),title="alternate initial states",width=2),
              column(numericInput("zByAPTProbabilityOUP",label="z:By",value="",step="any",width="100%"),title="state increment",width=2),
              column(numericInput("tByAPTProbabilityOUP",label="t:By",value="",step="any",width="100%"),title="time increment",width=2)
            ),
            fixedRow(style="height: 60px;",
              column(numericInput("rhoAPTProbabilityOUP",label="rho",value="",step="any",width="100%"),title="rate of convergence",width=2),
              column(numericInput("muAPTProbabilityOUP",label="mu",value="",step="any",width="100%"),title="location",width=2),
              column(numericInput("sigmaAPTProbabilityOUP",label="sigma",value="",step="any",width="100%"),title="scale",width=2),
              column(width=2),
              column(numericInput("kAPTProbabilityOUP",label="k",value="",step="any",width="100%"),title="threshold",width=2),
              column(numericInput("tToAPTProbabilityOUP",label="t:To",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            fixedRow(style="height: 68px;",
              column(width=2),
              column(width=2),
              column(width=2),
              column(numericInput("omegaAPTProbabilityOUP",label="omega",value="",step="any",width="100%"),title="degree of irreversibility",width=2),
              column(numericInput("sAPTProbabilityOUP",label="s",value="",step="any",width="100%"),title="fixed initial time",width=2),
              column(numericInput("tFromAPTProbabilityOUP",label="t:From",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            # User action
            fixedRow(
              column(actionButton("clearAPTProbabilityOUP",HTML("_"),width="100%",class="btn-info"),title="clear and save arguments",style="padding-right: 2px;",width=1),
              column(actionButton("saveAPTProbabilityOUP",HTML("&equiv;"),width="100%",class="btn-info"),title="save arguments",style="padding-left: 2px;",width=1),
              column(actionButton("undnAPTProbabilityOUP",HTML("&Vee;"),width="100%",class="btn-success"),title="previous arguments",style="padding-right: 2px;",width=1),
              column(actionButton("unupAPTProbabilityOUP",HTML("&Wedge;"),width="100%",class="btn-success"),title="next arguments",style="padding-left: 2px;",width=1),
              column(actionButton("syncAPTProbabilityOUP","Sync",width="100%",class="btn-success"),title="states and thresholds",width=2),
              column(actionButton("axesAPTProbabilityOUP","Axes",width="100%",class="btn-success"),title="for t, z and pt",width=2),
              column(actionButton("plotAPTProbabilityOUP","Plot",width="100%",class="btn-success"),title="refresh plot",width=2),
              column(actionButton("otherAPTProbabilityOUP",HTML("&lessgtr;"),width="100%",class="btn-success"),title="other plot",style="padding-right: 2px;",width=1)
            ),
            # Plot
            wellPanel(class="wellPlotOUP",style="margin: 0 auto; height: 402px; width: 580px;",plotlyOutput("plotlyAPTProbabilityOUP"),copyPlot()),
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
        add_busy_spinner(spin="flower",color="rgb(115,33,38)",timeout=1000,position=c("top-right"),margins=c(450,350),height="128px",width="128px"),
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
              column(actionButton("infoFDDriftOUP","Info",width="100%",class="btn-primary"),title="information about Drift",width=2),
              column(actionButton("clearFDDriftOUP",HTML("_"),width="100%",class="btn-info"),title="clear and save arguments",style="padding-right: 2px;",width=1),
              column(actionButton("saveFDDriftOUP",HTML("&equiv;"),width="100%",class="btn-info"),title="save arguments",style="padding-left: 2px;",width=1),
              column(actionButton("undnFDDriftOUP",HTML("&Vee;"),width="100%",class="btn-success"),title="previous arguments",style="padding-right: 2px;",width=1),
              column(actionButton("unupFDDriftOUP",HTML("&Wedge;"),width="100%",class="btn-success"),title="next arguments",style="padding-left: 2px;",width=1),
              column(actionButton("axesFDDriftOUP","Axes",width="100%",class="btn-success"),title="for s and x",width=2),
              column(actionButton("plotFDDriftOUP","Plot",width="100%",class="btn-success"),title="refresh plot",width=2)
            ),
            # Plot
            wellPanel(class="wellPlotOUP",style="margin: 0 auto; height: 402px; width: 580px;",plotlyOutput("plotlyFDDriftOUP"),copyPlot()),
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
              column(actionButton("infoFDDiffusionOUP","Info",width="100%",class="btn-primary"),title="information about Diffusion",width=2),
              column(actionButton("clearFDDiffusionOUP",HTML("_"),width="100%",class="btn-info"),title="clear and save arguments",style="padding-right: 2px;",width=1),
              column(actionButton("saveFDDiffusionOUP",HTML("&equiv;"),width="100%",class="btn-info"),title="save arguments",style="padding-left: 2px;",width=1),
              column(actionButton("undnFDDiffusionOUP",HTML("&Vee;"),width="100%",class="btn-success"),title="previous arguments",style="padding-right: 2px;",width=1),
              column(actionButton("unupFDDiffusionOUP",HTML("&Wedge;"),width="100%",class="btn-success"),title="next arguments",style="padding-left: 2px;",width=1),
              column(actionButton("axesFDDiffusionOUP","Axes",width="100%",class="btn-success"),title="for s and x",width=2),
              column(actionButton("plotFDDiffusionOUP","Plot",width="100%",class="btn-success"),title="refresh plot",width=2),
              column(actionButton("otherFDDiffusionOUP",HTML("&lessgtr;"),width="100%",class="btn-success"),title="other plot",style="padding-right: 2px;",width=1)
            ),
            # Plot
            wellPanel(class="wellPlotOUP",style="margin: 0 auto; height: 402px; width: 580px;",plotlyOutput("plotlyFDDiffusionOUP"),copyPlot()),
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
              column(actionButton("infoFDTerminalOUP","Info",width="100%",class="btn-primary"),title="information about Terminal Values",width=2),
              column(actionButton("clearFDTerminalOUP",HTML("_"),width="100%",class="btn-info"),title="clear and save arguments",style="padding-right: 2px;",width=1),
              column(actionButton("saveFDTerminalOUP",HTML("&equiv;"),width="100%",class="btn-info"),title="save arguments",style="padding-left: 2px;",width=1),
              column(actionButton("undnFDTerminalOUP",HTML("&Vee;"),width="100%",class="btn-success"),title="previous arguments",style="padding-right: 2px;",width=1),
              column(actionButton("unupFDTerminalOUP",HTML("&Wedge;"),width="100%",class="btn-success"),title="next arguments",style="padding-left: 2px;",width=1),
              column(actionButton("axesFDTerminalOUP","Axes",width="100%",class="btn-success"),title="for s and x",width=2),
              column(actionButton("plotFDTerminalOUP","Plot",width="100%",class="btn-success"),title="refresh plot",width=2)
            ),
            # Plot
            wellPanel(class="wellPlotOUP",style="margin: 0 auto; height: 402px; width: 580px;",plotlyOutput("plotlyFDTerminalOUP"),copyPlot()),
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
              column(numericInput("skipFDOptionOUP",label="skip",value="",step="any",width="100%"),title="divide s:By into smaller intervals",width=2),
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
              column(actionButton("infoFDOptionOUP","Info",width="100%",class="btn-primary"),title="information about Option",width=2),
              column(actionButton("clearFDOptionOUP",HTML("_"),width="100%",class="btn-info"),title="clear and save arguments",style="padding-right: 2px;",width=1),
              column(actionButton("saveFDOptionOUP",HTML("&equiv;"),width="100%",class="btn-info"),title="save arguments",style="padding-left: 2px;",width=1),
              column(actionButton("undnFDOptionOUP",HTML("&Vee;"),width="100%",class="btn-success"),title="previous arguments",style="padding-right: 2px;",width=1),
              column(actionButton("unupFDOptionOUP",HTML("&Wedge;"),width="100%",class="btn-success"),title="next arguments",style="padding-left: 2px;",width=1),
              column(actionButton("axesFDOptionOUP","Axes",width="100%",class="btn-success"),title="for s and x",width=2),
              column(actionButton("plotFDOptionOUP","Plot",width="100%",class="btn-success"),title="refresh plot",width=2),
              column(actionButton("otherFDOptionOUP",HTML("&lessgtr;"),width="100%",class="btn-success"),title="other plot",style="padding-right: 2px;",width=1)
            ),
            # Plot
            wellPanel(class="wellPlotOUP",style="margin: 0 auto; height: 402px; width: 580px;",plotlyOutput("plotlyFDOptionOUP"),copyPlot()),
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
              column(numericInput("skipFDEnvelopeOUP",label="skip",value="",step="any",width="100%"),title="divide s:By into smaller intervals",width=2),
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
              column(actionButton("infoFDEnvelopeOUP","Info",width="100%",class="btn-primary"),title="information about Option Envelope",width=2),
              column(actionButton("clearFDEnvelopeOUP",HTML("_"),width="100%",class="btn-info"),title="clear and save arguments",style="padding-right: 2px;",width=1),
              column(actionButton("saveFDEnvelopeOUP",HTML("&equiv;"),width="100%",class="btn-info"),title="save arguments",style="padding-left: 2px;",width=1),
              column(actionButton("undnFDEnvelopeOUP",HTML("&Vee;"),width="100%",class="btn-success"),title="previous arguments",style="padding-right: 2px;",width=1),
              column(actionButton("unupFDEnvelopeOUP",HTML("&Wedge;"),width="100%",class="btn-success"),title="next arguments",style="padding-left: 2px;",width=1),
              column(actionButton("axesFDEnvelopeOUP","Axes",width="100%",class="btn-success"),title="for s and x",width=2),
              column(actionButton("plotFDEnvelopeOUP","Plot",width="100%",class="btn-success"),title="refresh plot",width=2),
              column(actionButton("otherFDEnvelopeOUP",HTML("&lessgtr;"),width="100%",class="btn-success"),title="other plot",style="padding-right: 2px;",width=1)
            ),
            # Plot
            wellPanel(class="wellPlotOUP",style="margin: 0 auto; height: 402px; width: 580px;",plotlyOutput("plotlyFDEnvelopeOUP"),copyPlot()),
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
              column(actionButton("infoFDDecisionOUP","Info",width="100%",class="btn-primary"),title="information about Decision Threshold",width=2),
              column(actionButton("clearFDDecisionOUP",HTML("_"),width="100%",class="btn-info"),title="clear and save arguments",style="padding-right: 2px;",width=1),
              column(actionButton("saveFDDecisionOUP",HTML("&equiv;"),width="100%",class="btn-info"),title="save arguments",style="padding-left: 2px;",width=1),
              column(actionButton("undnFDDecisionOUP",HTML("&Vee;"),width="100%",class="btn-success"),title="previous arguments",style="padding-right: 2px;",width=1),
              column(actionButton("unupFDDecisionOUP",HTML("&Wedge;"),width="100%",class="btn-success"),title="next arguments",style="padding-left: 2px;",width=1),
              column(actionButton("axesFDDecisionOUP","Axes",width="100%",class="btn-success"),title="for s and x",width=2),
              column(actionButton("plotFDDecisionOUP","Plot",width="100%",class="btn-success"),title="refresh plot",width=2)
            ),
            # Plot
            wellPanel(class="wellPlotOUP",style="margin: 0 auto; height: 402px; width: 580px;",plotlyOutput("plotlyFDDecisionOUP"),copyPlot()),
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
        add_busy_spinner(spin="fulfilling-bouncing-circle",color="rgb(0,86,136)",timeout=1000,position=c("top-right"),margins=c(450,350),height="128px",width="128px"),
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
              column(wellPanel(class="wellTableOUP",style="padding: 0px; width=100%;",uiOutput("descrMLDataOUP"),copyTable()),width=6)
            ),
            # buttons, begin and end dates
            fixedRow(
              column(actionButton("resetMLDataOUP","Reset",width="100%",class="btn-success"),title="reset begin and end",style="padding-top: 32px;",width=2),
              column(numericInput("begMLDataOUP",label="begin",value="",step="any",width="100%"),title="time to begin plot",width=2),
              column(numericInput("endMLDataOUP",label="end",value="",step="any",width="100%"),title="time to end plot",width=2),
              column(width=2),
              column(actionButton("plotMLDataOUP","Plot",width="100%",class="btn-success"),title="refresh plot",style="padding-top: 32px;",width=2),
              column(actionButton("infoMLDataOUP","Info",width="100%",class="btn-primary"),title="information about Data",style="padding-top: 32px;",width=2)
            ),
            # plot
            wellPanel(class="wellPlotOUP",style="height: 402px;",plotlyOutput("plotlyMLDataOUP"),copyPlot()),
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
              column(wellPanel(class="wellTableOUP",style="padding: 0px; width=100%;",uiOutput("lnLMLLikelihoodOUP"),copyTable()),width=6)
            ),
            # buttons, begin and end dates
            fixedRow(
              column(actionButton("resetMLLikelihoodOUP","Reset",width="100%",class="btn-success"),title="reset begin and end",style="padding-top: 32px;",width=2),
              column(numericInput("begMLLikelihoodOUP",label="begin",value="",step="any",width="100%"),title="time to begin plot",width=2),
              column(numericInput("endMLLikelihoodOUP",label="end",value="",step="any",width="100%"),title="time to end plot",width=2),
              column(width=2),
              column(actionButton("plotMLLikelihoodOUP","Plot",width="100%",class="btn-success"),title="calculate and plot",style="padding-top: 32px;",width=2),
              column(actionButton("infoMLLikelihoodOUP","Info",width="100%",class="btn-primary"),title="information about Log Likelihood",style="padding-top: 32px;",width=2)
            ),
            # plot
            wellPanel(class="wellPlotOUP",style="height: 402px;",plotlyOutput("plotlyMLLikelihoodOUP"),copyPlot()),
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
            # parameters, likelihoods and such
            fixedRow(
              column(wellPanel(class="wellTableOUP",style="padding: 0px; width: 100%;",uiOutput("paramMLEstimatesOUP"),copyTable()),width=12)
            ),
            # restrictions and buttons
            fixedRow(
              column(actionButton("resetMLEstimatesOUP","Reset",width="100%",class="btn-success"),title="reset rhor, mur and sigmar",style="padding-top: 32px;",width=2),
              column(numericInput("rhorMLEstimatesOUP",label="rhor",value="",step="any",width="100%"),title="constant for rate",width=2),
              column(numericInput("murMLEstimatesOUP",label="mur",value="",step="any",width="100%"),title="constant for location",width=2),
              column(numericInput("sigmarMLEstimatesOUP",label="sigmar",value="",step="any",width="100%"),title="constant for scale",width=2),
              column(actionButton("plotMLEstimatesOUP","Go",width="100%",class="btn-success"),title="calculate",style="padding-top: 32px;",width=2),
              column(actionButton("infoMLEstimatesOUP","Info",width="100%",class="btn-primary"),title="information about Estimates",style="padding-top: 32px;",width=2)
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
              column(wellPanel(class="wellTableOUP",style="padding: 0px; width: 100%;",uiOutput("paramMLGoodnessOUP"),copyTable()),width=12)
            ),
            # buttons
            fixedRow(
              column(width=2),
              column(width=2),
              column(width=2),
              column(width=2),
              column(actionButton("plotMLGoodnessOUP","Go",width="100%",class="btn-success"),title="calculate",style="padding-top: 32px;",width=2),
              column(actionButton("infoMLGoodnessOUP","Info",width="100%",class="btn-primary"),title="information about Goodness-of-Fit",style="padding-top: 32px;",width=2)
            ),
            # table
            fixedRow(
              column(width=3),
              column(wellPanel(class="wellTableOUP",style="margin-top: 26px; padding: 6px 0px 18px 0px; width=100%;",uiOutput("goodsMLGoodnessOUP"),copyTable()),width=6)
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
              column(wellPanel(class="wellTableOUP",style="padding: 0px; width: 100%;",uiOutput("paramMLRatioOUP"),copyTable()),width=12)
            ),
            # restrictions and buttons
            fixedRow(
              column(width=2),
              column(width=2),
              column(width=2),
              column(width=2),
              column(actionButton("plotMLRatioOUP","Go",width="100%",class="btn-success"),title="calculate",style="padding-top: 37px;",width=2),
              column(actionButton("infoMLRatioOUP","Info",width="100%",class="btn-primary"),title="information about Likelihood Ratio Test",style="padding-top: 37px;",width=2)
            ),
            # table
            fixedRow(
              column(width=3),
              column(wellPanel(class="wellTableOUP",style="margin-top: 26px; padding: 6px 0px 18px 0px; width=100%;",uiOutput("ratioMLRatioOUP"),copyTable()),width=6)
            ),
            value="MLRatioOUP"
          ),
          id="navMLOUP",widths=c(3,9)
        ),
        value="tabMLOUP"
        #end list ----
      ),
      nav_panel("Monte Carlo",
        tags$head(HTML('<html lang="en"> <link rel="icon" href="favicon.png" type="image/png" sizes="16x16">'),
                  tags$link(rel="stylesheet",type="text/css",href="styles.css")),
        add_busy_spinner(spin="breeding-rhombus",color="rgb(125,0,25)",timeout=1000,position=c("top-right"),margins=c(450,350),height="128px",width="128px"),
        navset_pill_list(
          # Forward Paths ----
          nav_panel("Forward Paths",
            # User input
            fixedRow(style="height: 60px;",
              column(actionButton("infoMCForwardOUP","Info",width="100%",class="btn-primary"),title="information about Forward Paths",style="padding-top: 32px;",width=2),
              column(numericInput("xMCForwardOUP",label="x",value="",step="any",width="100%"),title="fixed initial state",width=2),
              column(width=2),
              column(width=2),
              column(width=2),
              column(numericInput("tByMCForwardOUP",label="t:By",value="",step="any",width="100%"),title="time increment",width=2)
            ),
            fixedRow(style="height: 60px;",
              column(numericInput("rhoMCForwardOUP",label="rho",value="",step="any",width="100%"),title="rate of convergence",width=2),
              column(numericInput("muMCForwardOUP",label="mu",value="",step="any",width="100%"),title="location",width=2),
              column(numericInput("sigmaMCForwardOUP",label="sigma",value="",step="any",width="100%"),title="scale",width=2),
              column(width=2),
              column(numericInput("kMCForwardOUP",label="k",value="",step="any",width="100%"),title="threshold",width=2),
              column(numericInput("tToMCForwardOUP",label="t:To",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            fixedRow(style="height: 68px;",
              column(actionButton("resetMCForwardOUP","Reset",width="100%",class="btn-success"),title="reset first and last",style="padding-top: 32px;",width=2),
              column(numericInput("firstMCForwardOUP",label="first",value="",step="any",width="100%"),title="first path on plot",width=2),
              column(numericInput("lastMCForwardOUP",label="last",value="",step="any",width="100%"),title="last path on plot",width=2),
              column(numericInput("pathsMCForwardOUP",label="paths",value="",step="any",width="100%"),title="number of paths",width=2),
              column(numericInput("skipMCForwardOUP",label="skip",value="",step="any",width="100%"),title="divide t:By into smaller intervals",width=2),
              column(numericInput("tFromMCForwardOUP",label="t:From",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            # User action
            fixedRow(
              column(actionButton("clearMCForwardOUP",HTML("_"),width="100%",class="btn-info"),title="clear and save arguments",style="padding-right: 2px;",width=1),
              column(actionButton("saveMCForwardOUP",HTML("&equiv;"),width="100%",class="btn-info"),title="save arguments",style="padding-left: 2px;",width=1),
              column(actionButton("undnMCForwardOUP",HTML("&Vee;"),width="100%",class="btn-success"),title="previous arguments",style="padding-right: 2px;",width=1),
              column(actionButton("unupMCForwardOUP",HTML("&Wedge;"),width="100%",class="btn-success"),title="next arguments",style="padding-left: 2px;",width=1),
              column(actionButton("syncMCForwardOUP","Sync",width="100%",class="btn-success"),title="states and times",width=2),
              column(actionButton("axesMCForwardOUP","Axes",width="100%",class="btn-success"),title="for y and t",width=2),
              column(actionButton("plotMCForwardOUP","Plot",width="100%",class="btn-success"),title="refresh plot",width=2),
              column(actionButton("leftMCForwardOUP","<",width="100%",class="btn-success"),title="previous plot",style="padding-right: 2px;",width=1),
              column(actionButton("rghtMCForwardOUP",">",width="100%",class="btn-success"),title="next plot",style="padding-left: 2px;",width=1)
            ),
            # Plot
            wellPanel(class="wellPlotOUP",style="margin: 0 auto; height: 402px; width: 580px;",plotlyOutput("plotlyMCForwardOUP"),copyPlot()),
            value="MCForwardOUP"
          ),
          # Backward Paths ----
          nav_panel("Backward Paths",
            # User input
            fixedRow(style="height: 60px;",
              column(actionButton("infoMCBackwardOUP","Info",width="100%",class="btn-primary"),title="information about Backward Paths",style="padding-top: 32px;",width=2),
              column(numericInput("yMCBackwardOUP",label="y",value="",step="any",width="100%"),title="fixed terminal state",width=2),
              column(width=2),
              column(width=2),
              column(width=2),
              column(numericInput("sByMCBackwardOUP",label="s:By",value="",step="any",width="100%"),title="time increment",width=2)
            ),
            fixedRow(style="height: 60px;",
              column(numericInput("rhoMCBackwardOUP",label="rho",value="",step="any",width="100%"),title="rate of convergence",width=2),
              column(numericInput("muMCBackwardOUP",label="mu",value="",step="any",width="100%"),title="location",width=2),
              column(numericInput("sigmaMCBackwardOUP",label="sigma",value="",step="any",width="100%"),title="scale",width=2),
              column(width=2),
              column(width=2),
              column(numericInput("sToMCBackwardOUP",label="s:To",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            fixedRow(style="height: 68px;",
              column(actionButton("resetMCBackwardOUP","Reset",width="100%",class="btn-success"),title="reset first and last",style="padding-top: 32px;",width=2),
              column(numericInput("firstMCBackwardOUP",label="first",value="",step="any",width="100%"),title="first path on plot",width=2),
              column(numericInput("lastMCBackwardOUP",label="last",value="",step="any",width="100%"),title="last path on plot",width=2),
              column(numericInput("pathsMCBackwardOUP",label="paths",value="",step="any",width="100%"),title="number of paths",width=2),
              column(numericInput("skipMCBackwardOUP",label="skip",value="",step="any",width="100%"),title="divide s:By into smaller intervals",width=2),
              column(numericInput("sFromMCBackwardOUP",label="s:From",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            # User action
            fixedRow(
              column(actionButton("clearMCBackwardOUP",HTML("_"),width="100%",class="btn-info"),title="clear and save arguments",style="padding-right: 2px;",width=1),
              column(actionButton("saveMCBackwardOUP",HTML("&equiv;"),width="100%",class="btn-info"),title="save arguments",style="padding-left: 2px;",width=1),
              column(actionButton("undnMCBackwardOUP",HTML("&Vee;"),width="100%",class="btn-success"),title="previous arguments",style="padding-right: 2px;",width=1),
              column(actionButton("unupMCBackwardOUP",HTML("&Wedge;"),width="100%",class="btn-success"),title="next arguments",style="padding-left: 2px;",width=1),
              column(actionButton("syncMCBackwardOUP","Sync",width="100%",class="btn-success"),title="states and times",width=2),
              column(actionButton("axesMCBackwardOUP","Axes",width="100%",class="btn-success"),title="for x and s",width=2),
              column(actionButton("plotMCBackwardOUP","Plot",width="100%",class="btn-success"),title="refresh plot",width=2),
              column(actionButton("leftMCBackwardOUP","<",width="100%",class="btn-success"),title="previous plot",style="padding-right: 2px;",width=1),
              column(actionButton("rghtMCBackwardOUP",">",width="100%",class="btn-success"),title="next plot",style="padding-left: 2px;",width=1)
            ),
            # Plot
            wellPanel(class="wellPlotOUP",style="margin: 0 auto; height: 402px; width: 580px;",plotlyOutput("plotlyMCBackwardOUP"),copyPlot()),
            value="MCBackwardOUP"
          ),
          # Bounded Paths ----
          nav_panel("Bounded Paths",
            # User input
            fixedRow(style="height: 60px;",
              column(actionButton("infoMCBoundedOUP","Info",width="100%",class="btn-primary"),title="information about Bounded Paths",style="padding-top: 32px;",width=2),
              column(numericInput("xMCBoundedOUP",label="x",value="",step="any",width="100%"),title="fixed initial state",width=2),
              column(width=2),
              column(width=2),
              column(width=2),
              column(numericInput("tByMCBoundedOUP",label="t:By",value="",step="any",width="100%"),title="time increment",width=2)
            ),
            fixedRow(style="height: 60px;",
              column(numericInput("rhoMCBoundedOUP",label="rho",value="",step="any",width="100%"),title="rate of convergence",width=2),
              column(numericInput("muMCBoundedOUP",label="mu",value="",step="any",width="100%"),title="location",width=2),
              column(numericInput("sigmaMCBoundedOUP",label="sigma",value="",step="any",width="100%"),title="scale",width=2),
              column(width=2),
              column(numericInput("kMCBoundedOUP",label="k",value="",step="any",width="100%"),title="threshold",width=2),
              column(numericInput("tToMCBoundedOUP",label="t:To",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            fixedRow(style="height: 68px;",
              column(actionButton("resetMCBoundedOUP","Reset",width="100%",class="btn-success"),title="reset first and last",style="padding-top: 32px;",width=2),
              column(numericInput("firstMCBoundedOUP",label="first",value="",step="any",width="100%"),title="first path on plot",width=2),
              column(numericInput("lastMCBoundedOUP",label="last",value="",step="any",width="100%"),title="last path on plot",width=2),
              column(numericInput("pathsMCBoundedOUP",label="paths",value="",step="any",width="100%"),title="number of paths",width=2),
              column(numericInput("skipMCBoundedOUP",label="skip",value="",step="any",width="100%"),title="divide t:By into smaller intervals",width=2),
              column(numericInput("tFromMCBoundedOUP",label="t:From",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            # User action
            fixedRow(
              column(actionButton("clearMCBoundedOUP",HTML("_"),width="100%",class="btn-info"),title="clear and  save arguments",style="padding-right: 2px;",width=1),
              column(actionButton("saveMCBoundedOUP",HTML("&equiv;"),width="100%",class="btn-info"),title="save arguments",style="padding-left: 2px;",width=1),
              column(actionButton("undnMCBoundedOUP",HTML("&Vee;"),width="100%",class="btn-success"),title="previous arguments",style="padding-right: 2px;",width=1),
              column(actionButton("unupMCBoundedOUP",HTML("&Wedge;"),width="100%",class="btn-success"),title="next arguments",style="padding-left: 2px;",width=1),
              column(actionButton("syncMCBoundedOUP","Sync",width="100%",class="btn-success"),title="states and times",width=2),
              column(actionButton("axesMCBoundedOUP","Axes",width="100%",class="btn-success"),title="for t",width=2),
              column(actionButton("plotMCBoundedOUP","Plot",width="100%",class="btn-success"),title="refresh plot",width=2),
              column(actionButton("leftMCBoundedOUP","<",width="100%",class="btn-success"),title="previous plot",style="padding-right: 2px;",width=1),
              column(actionButton("rghtMCBoundedOUP",">",width="100%",class="btn-success"),title="next plot",style="padding-left: 2px;",width=1)
            ),
            # Plot
            wellPanel(class="wellPlotOUP",style="margin: 0 auto; height: 402px; width: 580px;",plotlyOutput("plotlyMCBoundedOUP"),copyPlot()),
            value="MCBoundedOUP"
          ),
          # Mean ----
          nav_panel("Mean",
            # User input
            fixedRow(style="height: 60px;",
              column(actionButton("infoMCMeanOUP","Info",width="100%",class="btn-primary"),title="information about Mean",style="padding-top: 32px;",width=2),
              column(numericInput("xMCMeanOUP",label="x",value="",step="any",width="100%"),title="fixed initial state",width=2),
              column(numericInput("yFromMCMeanOUP",label="y:From",value="",step="any",width="100%"),title="stochastic states",width=2),
              column(numericInput("yToMCMeanOUP",label="y:To",value="",step="any",width="100%"),title="stochastic states",width=2),
              column(numericInput("yByMCMeanOUP",label="y:By",value="",step="any",width="100%"),title="state increment",width=2),
              column(numericInput("tByMCMeanOUP",label="t:By",value="",step="any",width="100%"),title="time increment",width=2)
            ),
            fixedRow(style="height: 60px;",
              column(numericInput("rhoMCMeanOUP",label="rho",value="",step="any",width="100%"),title="rate of convergence",width=2),
              column(numericInput("muMCMeanOUP",label="mu",value="",step="any",width="100%"),title="location",width=2),
              column(numericInput("sigmaMCMeanOUP",label="sigma",value="",step="any",width="100%"),title="scale",width=2),
              column(width=2),
              column(width=2),
              column(numericInput("tToMCMeanOUP",label="t:To",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            fixedRow(style="height: 68px;",
              column(width=2),
              column(width=2),
              column(numericInput("pmaxMCMeanOUP",label="p max",value="",step="any",width="100%"),title="maximum for heat map of paths",width=2),
              column(numericInput("pathsMCMeanOUP",label="paths",value="",step="any",width="100%"),title="number of paths",width=2),
              column(numericInput("skipMCMeanOUP",label="skip",value="",step="any",width="100%"),title="divide t:By into smaller intervals",width=2),
              column(numericInput("tFromMCMeanOUP",label="t:From",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            # User action
            fixedRow(
              column(actionButton("clearMCMeanOUP",HTML("_"),width="100%",class="btn-info"),title="clear and save arguments",style="padding-right: 2px;",width=1),
              column(actionButton("saveMCMeanOUP",HTML("&equiv;"),width="100%",class="btn-info"),title="save arguments",style="padding-left: 2px;",width=1),
              column(actionButton("undnMCMeanOUP",HTML("&Vee;"),width="100%",class="btn-success"),title="previous arguments",style="padding-right: 2px;",width=1),
              column(actionButton("unupMCMeanOUP",HTML("&Wedge;"),width="100%",class="btn-success"),title="next arguments",style="padding-left: 2px;",width=1),
              column(actionButton("syncMCMeanOUP","Sync",width="100%",class="btn-success"),title="states and times",width=2),
              column(actionButton("axesMCMeanOUP","Axes",width="100%",class="btn-success"),title="for t, y and p",width=2),
              column(actionButton("plotMCMeanOUP","Plot",width="100%",class="btn-success"),title="refresh plot",width=2),
              column(actionButton("otherMCMeanOUP",HTML("&lessgtr;"),width="100%",class="btn-success"),title="other plot",style="padding-right: 2px;",width=1)
            ),
            # Plot
            wellPanel(class="wellPlotOUP",style="margin: 0 auto; height: 402px; width: 580px;",plotlyOutput("plotlyMCMeanOUP"),copyPlot()),
            value="MCMeanOUP"
          ),
          # Variance ----
          nav_panel("Variance",
            # User input
            fixedRow(style="height: 60px;",
              column(actionButton("infoMCVarianceOUP","Info",width="100%",class="btn-primary"),title="information about Variance",style="padding-top: 32px;",width=2),
              column(numericInput("xMCVarianceOUP",label="x",value="",step="any",width="100%"),title="fixed initial state",width=2),
              column(numericInput("yFromMCVarianceOUP",label="y:From",value="",step="any",width="100%"),title="stochastic states",width=2),
              column(numericInput("yToMCVarianceOUP",label="y:To",value="",step="any",width="100%"),title="stochastic states",width=2),
              column(numericInput("yByMCVarianceOUP",label="y:By",value="",step="any",width="100%"),title="state increment",width=2),
              column(numericInput("tByMCVarianceOUP",label="t:By",value="",step="any",width="100%"),title="time increment",width=2)
            ),
            fixedRow(style="height: 60px;",
              column(numericInput("rhoMCVarianceOUP",label="rho",value="",step="any",width="100%"),title="rate of convergence",width=2),
              column(numericInput("muMCVarianceOUP",label="mu",value="",step="any",width="100%"),title="location",width=2),
              column(numericInput("sigmaMCVarianceOUP",label="sigma",value="",step="any",width="100%"),title="scale",width=2),
              column(width=2),
              column(width=2),
              column(numericInput("tToMCVarianceOUP",label="t:To",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            fixedRow(style="height: 68px;",
              column(width=2),
              column(width=2),
              column(numericInput("pmaxMCVarianceOUP",label="p max",value="",step="any",width="100%"),title="maximum for heat map of paths",width=2),
              column(numericInput("pathsMCVarianceOUP",label="paths",value="",step="any",width="100%"),title="number of paths",width=2),
              column(numericInput("skipMCVarianceOUP",label="skip",value="",step="any",width="100%"),title="divide t:By into smaller intervals",width=2),
              column(numericInput("tFromMCVarianceOUP",label="t:From",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            # User action
            fixedRow(
              column(actionButton("clearMCVarianceOUP",HTML("_"),width="100%",class="btn-info"),title="clear and save arguments",style="padding-right: 2px;",width=1),
              column(actionButton("saveMCVarianceOUP",HTML("&equiv;"),width="100%",class="btn-info"),title="save arguments",style="padding-left: 2px;",width=1),
              column(actionButton("undnMCVarianceOUP",HTML("&Vee;"),width="100%",class="btn-success"),title="previous arguments",style="padding-right: 2px;",width=1),
              column(actionButton("unupMCVarianceOUP",HTML("&Wedge;"),width="100%",class="btn-success"),title="next arguments arguments",style="padding-left: 2px;",width=1),
              column(actionButton("syncMCVarianceOUP","Sync",width="100%",class="btn-success"),title="states and times",width=2),
              column(actionButton("axesMCVarianceOUP","Axes",width="100%",class="btn-success"),title="for t, y and p",width=2),
              column(actionButton("plotMCVarianceOUP","Plot",width="100%",class="btn-success"),title="refresh plot",width=2),
              column(actionButton("otherMCVarianceOUP",HTML("&lessgtr;"),width="100%",class="btn-success"),title="other plot",style="padding-right: 2px;",width=1)
            ),
            # Plot
            wellPanel(class="wellPlotOUP",style="margin: 0 auto; height: 402px; width: 580px;",plotlyOutput("plotlyMCVarianceOUP"),copyPlot()),
            value="MCVarianceOUP"
          ),
          # Transition Density ----
          nav_panel("Transition Density",
            # User input
            fixedRow(style="height: 60px;",
              column(actionButton("infoMCDensityOUP","Info",width="100%",class="btn-primary"),title="information about Transition Density",style="padding-top: 32px;",width=2),
              column(numericInput("xMCDensityOUP",label="x",value="",step="any",width="100%"),title="fixed initial state",width=2),
              column(numericInput("yFromMCDensityOUP",label="y:From",value="",step="any",width="100%"),title="stochastic states",width=2),
              column(numericInput("yToMCDensityOUP",label="y:To",value="",step="any",width="100%"),title="stochastic states",width=2),
              column(numericInput("yByMCDensityOUP",label="y:By",value="",step="any",width="100%"),title="state increment",width=2),
              column(numericInput("tByMCDensityOUP",label="t:By",value="",step="any",width="100%"),title="time increment",width=2)
            ),
            fixedRow(style="height: 60px;",
              column(numericInput("rhoMCDensityOUP",label="rho",value="",step="any",width="100%"),title="rate of convergence",width=2),
              column(numericInput("muMCDensityOUP",label="mu",value="",step="any",width="100%"),title="location",width=2),
              column(numericInput("sigmaMCDensityOUP",label="sigma",value="",step="any",width="100%"),title="scale",width=2),
              column(width=2),
              column(width=2),
              column(numericInput("tToMCDensityOUP",label="t:To",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            fixedRow(style="height: 68px;",
              column(width=2),
              column(width=2),
              column(numericInput("pmaxMCDensityOUP",label="p max",value="",step="any",width="100%"),title="maximum for density and heat map of paths",width=2),
              column(numericInput("pathsMCDensityOUP",label="paths",value="",step="any",width="100%"),title="number of paths",width=2),
              column(numericInput("skipMCDensityOUP",label="skip",value="",step="any",width="100%"),title="divide t:By into smaller intervals",width=2),
              column(numericInput("tFromMCDensityOUP",label="t:From",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            # User action
            fixedRow(
              column(actionButton("clearMCDensityOUP",HTML("_"),width="100%",class="btn-info"),title="clear and save arguments",style="padding-right: 2px;",width=1),
              column(actionButton("saveMCDensityOUP",HTML("&equiv;"),width="100%",class="btn-info"),title="save arguments",style="padding-left: 2px;",width=1),
              column(actionButton("undnMCDensityOUP",HTML("&Vee;"),width="100%",class="btn-success"),title="previous arguments",style="padding-right: 2px;",width=1),
              column(actionButton("unupMCDensityOUP",HTML("&Wedge;"),width="100%",class="btn-success"),title="next arguments",style="padding-left: 2px;",width=1),
              column(actionButton("syncMCDensityOUP","Sync",width="100%",class="btn-success"),title="states and times",width=2),
              column(actionButton("axesMCDensityOUP","Axes",width="100%",class="btn-success"),title="for t, y and p",width=2),
              column(actionButton("plotMCDensityOUP","Plot",width="100%",class="btn-success"),title="refresh plot",width=2),
              column(actionButton("otherMCDensityOUP",HTML("&lessgtr;"),width="100%",class="btn-success"),title="other plot",style="padding-right: 2px;",width=1)
            ),
            # Plot
            wellPanel(class="wellPlotOUP",style="margin: 0 auto; height: 402px; width: 580px;",plotlyOutput("plotlyMCDensityOUP"),copyPlot()),
            value="MCDensityOUP"
          ),
          # Transition Probability ----
          nav_panel("Transition Probability",
            # User input
            fixedRow(style="height: 60px;",
              column(actionButton("infoMCProbabilityOUP","Info",width="100%",class="btn-primary"),title="information about Transition Probability",style="padding-top: 32px;",width=2),
              column(numericInput("xMCProbabilityOUP",label="x",value="",step="any",width="100%"),title="fixed initial state",width=2),
              column(numericInput("yFromMCProbabilityOUP",label="y:From",value="",step="any",width="100%"),title="stochastic states",width=2),
              column(numericInput("yToMCProbabilityOUP",label="y:To",value="",step="any",width="100%"),title="stochastic states",width=2),
              column(numericInput("yByMCProbabilityOUP",label="y:By",value="",step="any",width="100%"),title="state increment",width=2),
              column(numericInput("tByMCProbabilityOUP",label="t:By",value="",step="any",width="100%"),title="time increment",width=2)
            ),
            fixedRow(style="height: 60px;",
              column(numericInput("rhoMCProbabilityOUP",label="rho",value="",step="any",width="100%"),title="rate of convergence",width=2),
              column(numericInput("muMCProbabilityOUP",label="mu",value="",step="any",width="100%"),title="location",width=2),
              column(numericInput("sigmaMCProbabilityOUP",label="sigma",value="",step="any",width="100%"),title="scale",width=2),
              column(width=2),
              column(numericInput("psiMCProbabilityOUP",label="psi",value="",step="any",width="100%"),title="-inf to y or y to inf",width=2),
              column(numericInput("tToMCProbabilityOUP",label="t:To",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            fixedRow(style="height: 68px;",
              column(width=2),
              column(width=2),
              column(numericInput("pmaxMCProbabilityOUP",label="p max",value="",step="any",width="100%"),title="maximum for heat map of paths",width=2),
              column(numericInput("pathsMCProbabilityOUP",label="paths",value="",step="any",width="100%"),title="number of paths",width=2),
              column(numericInput("skipMCProbabilityOUP",label="skip",value="",step="any",width="100%"),title="divide t:By into smaller intervals",width=2),
              column(numericInput("tFromMCProbabilityOUP",label="t:From",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            # User action
            fixedRow(
              column(actionButton("clearMCProbabilityOUP",HTML("_"),width="100%",class="btn-info"),title="clear and save arguments",style="padding-right: 2px;",width=1),
              column(actionButton("saveMCProbabilityOUP",HTML("&equiv;"),width="100%",class="btn-info"),title="save arguments",style="padding-left: 2px;",width=1),
              column(actionButton("undnMCProbabilityOUP",HTML("&Vee;"),width="100%",class="btn-success"),title="previous arguments",style="padding-right: 2px;",width=1),
              column(actionButton("unupMCProbabilityOUP",HTML("&Wedge;"),width="100%",class="btn-success"),title="next arguments",style="padding-left: 2px;",width=1),
              column(actionButton("syncMCProbabilityOUP","Sync",width="100%",class="btn-success"),title="states and times",width=2),
              column(actionButton("axesMCProbabilityOUP","Axes",width="100%",class="btn-success"),title="for t, y and p",width=2),
              column(actionButton("plotMCProbabilityOUP","Plot",width="100%",class="btn-success"),title="refresh plot",width=2),
              column(actionButton("otherMCProbabilityOUP",HTML("&lessgtr;"),width="100%",class="btn-success"),title="other plot",style="padding-right: 2px;",width=1)
            ),
            # Plot
            wellPanel(class="wellPlotOUP",style="margin: 0 auto; height: 402px; width: 580px;",plotlyOutput("plotlyMCProbabilityOUP"),copyPlot()),
            value="MCProbabilityOUP"
          ),
          # Double Integral ----
          nav_panel("Double Integral",
            # User input
            fixedRow(style="height: 60px;",
              column(actionButton("infoMCDoubleOUP","Info",width="100%",class="btn-primary"),title="information about Double Integral",style="padding-top: 32px;",width=2),
              column(numericInput("xMCDoubleOUP",label="x",value="",step="any",width="100%"),title="fixed initial state",width=2),
              column(numericInput("yFromMCDoubleOUP",label="y:From",value="",step="any",width="100%"),title="stochastic states",width=2),
              column(numericInput("yToMCDoubleOUP",label="y:To",value="",step="any",width="100%"),title="stochastic states",width=2),
              column(numericInput("yByMCDoubleOUP",label="y:By",value="",step="any",width="100%"),title="state increment",width=2),
              column(numericInput("tByMCDoubleOUP",label="t:By",value="",step="any",width="100%"),title="time increment",width=2)
            ),
            fixedRow(style="height: 60px;",
              column(numericInput("rhoMCDoubleOUP",label="rho",value="",step="any",width="100%"),title="rate of convergence",width=2),
              column(numericInput("muMCDoubleOUP",label="mu",value="",step="any",width="100%"),title="location",width=2),
              column(numericInput("sigmaMCDoubleOUP",label="sigma",value="",step="any",width="100%"),title="scale",width=2),
              column(width=2),
              column(numericInput("psiMCDoubleOUP",label="psi",value="",step="any",width="100%"),title="-inf to y or y to inf",width=2),
              column(numericInput("tToMCDoubleOUP",label="t:To",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            fixedRow(style="height: 68px;",
              column(width=2),
              column(width=2),
              column(numericInput("pmaxMCDoubleOUP",label="p max",value="",step="any",width="100%"),title="maximum for heat map of paths",width=2),
              column(numericInput("pathsMCDoubleOUP",label="paths",value="",step="any",width="100%"),title="number of paths",width=2),
              column(numericInput("skipMCDoubleOUP",label="skip",value="",step="any",width="100%"),title="divide t:By into smaller intervals",width=2),
              column(numericInput("tFromMCDoubleOUP",label="t:From",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            # User action
            fixedRow(
              column(actionButton("clearMCDoubleOUP",HTML("_"),width="100%",class="btn-info"),title="clear and save arguments",style="padding-right: 2px;",width=1),
              column(actionButton("saveMCDoubleOUP",HTML("&equiv;"),width="100%",class="btn-info"),title="save arguments",style="padding-left: 2px;",width=1),
              column(actionButton("undnMCDoubleOUP",HTML("&Vee;"),width="100%",class="btn-success"),title="previous arguments",style="padding-right: 2px;",width=1),
              column(actionButton("unupMCDoubleOUP",HTML("&Wedge;"),width="100%",class="btn-success"),title="next arguments",style="padding-left: 2px;",width=1),
              column(actionButton("syncMCDoubleOUP","Sync",width="100%",class="btn-success"),title="states and times",width=2),
              column(actionButton("axesMCDoubleOUP","Axes",width="100%",class="btn-success"),title="for t, y and p",width=2),
              column(actionButton("plotMCDoubleOUP","Plot",width="100%",class="btn-success"),title="refresh plot",width=2),
              column(actionButton("otherMCDoubleOUP",HTML("&lessgtr;"),width="100%",class="btn-success"),title="other plot",style="padding-right: 2px;",width=1)
            ),
            # Plot
            wellPanel(class="wellPlotOUP",style="margin: 0 auto; height: 402px; width: 580px;",plotlyOutput("plotlyMCDoubleOUP"),copyPlot()),
            value="MCDoubleOUP"
          ),
          # Option ----
          nav_panel("Option",
            # User input
            fixedRow(style="height: 60px;",
              column(actionButton("infoMCOptionOUP","Info",width="100%",class="btn-primary"),title="information about Option",style="padding-top: 32px;",width=2),
              column(numericInput("yMCOptionOUP",label="y",value="",step="any",width="100%"),title="fixed terminal state",width=2),
              column(numericInput("xFromMCOptionOUP",label="x:From",value="",step="any",width="100%"),title="stochastic states",width=2),
              column(numericInput("xToMCOptionOUP",label="x:To",value="",step="any",width="100%"),title="stochastic states",width=2),
              column(numericInput("xByMCOptionOUP",label="x:By",value="",step="any",width="100%"),title="state increment",width=2),
              column(numericInput("sByMCOptionOUP",label="s:By",value="",step="any",width="100%"),title="time increment",width=2)
            ),
            fixedRow(style="height: 60px;",
              column(numericInput("rhoMCOptionOUP",label="rho",value="",step="any",width="100%"),title="rate of convergence",width=2),
              column(numericInput("muMCOptionOUP",label="mu",value="",step="any",width="100%"),title="location",width=2),
              column(numericInput("sigmaMCOptionOUP",label="sigma",value="",step="any",width="100%"),title="scale",width=2),
              column(numericInput("rMCOptionOUP",label="r",value="",step="any",width="100%"),title="discount rate of convergence",width=2),
              column(numericInput("phiMCOptionOUP",label="phi",value="",step="any",width="100%"),title="-inf to y or y to inf",width=2),
              column(numericInput("sToMCOptionOUP",label="s:To",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            fixedRow(style="height: 68px;",
              column(width=2),
              column(width=2),
              column(numericInput("pmaxMCOptionOUP",label="p max",value="",step="any",width="100%"),title="maximum for heat map of paths",width=2),
              column(numericInput("pathsMCOptionOUP",label="paths",value="",step="any",width="100%"),title="number of paths",width=2),
              column(numericInput("skipMCOptionOUP",label="skip",value="",step="any",width="100%"),title="divide s:By into smaller intervals",width=2),
              column(numericInput("sFromMCOptionOUP",label="s:From",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            # User action
            fixedRow(
              column(actionButton("clearMCOptionOUP",HTML("_"),width="100%",class="btn-info"),title="clear and save arguments",style="padding-right: 2px;",width=1),
              column(actionButton("saveMCOptionOUP",HTML("&equiv;"),width="100%",class="btn-info"),title="save arguments",style="padding-left: 2px;",width=1),
              column(actionButton("undnMCOptionOUP",HTML("&Vee;"),width="100%",class="btn-success"),title="previous arguments",style="padding-right: 2px;",width=1),
              column(actionButton("unupMCOptionOUP",HTML("&Wedge;"),width="100%",class="btn-success"),title="next arguments",style="padding-left: 2px;",width=1),
              column(actionButton("syncMCOptionOUP","Sync",width="100%",class="btn-success"),title="states and times",width=2),
              column(actionButton("axesMCOptionOUP","Axes",width="100%",class="btn-success"),title="for s and x",width=2),
              column(actionButton("plotMCOptionOUP","Plot",width="100%",class="btn-success"),title="refresh plot",width=2),
              column(actionButton("otherMCOptionOUP",HTML("&lessgtr;"),width="100%",class="btn-success"),title="other plot",style="padding-right: 2px;",width=1)
            ),
            # Plot
            wellPanel(class="wellPlotOUP",style="margin: 0 auto; height: 402px; width: 580px;",plotlyOutput("plotlyMCOptionOUP"),copyPlot()),
            value="MCOptionOUP"
          ),
          # Visiting Time Mode Median Mean ----
          nav_item(tags$span(HTML("&ensp;&nbsp;Visiting Time..."),style="color: var(--bs-nav-link-color); font-weight: bold;")),
          nav_panel(HTML("&emsp;Mode, Median and Mean"),
            # User input
            fixedRow(style="height: 60px;",
              column(actionButton("infoMCVTModeMedianMeanOUP","Info",width="100%",class="btn-primary"),title="information about Visiting Time Density",style="padding-top: 32px;",width=2),
              column(numericInput("xMCVTModeMedianMeanOUP",label="x",value="",step="any",width="100%"),title="fixed initial state",width=2),
              column(width=2),
              column(width=2),
              column(width=2),
              column(numericInput("tByMCVTModeMedianMeanOUP",label="t:By",value="",step="any",width="100%"),title="time increment",width=2)
            ),
            fixedRow(style="height: 60px;",
              column(numericInput("rhoMCVTModeMedianMeanOUP",label="rho",value="",step="any",width="100%"),title="rate of convergence",width=2),
              column(numericInput("muMCVTModeMedianMeanOUP",label="mu",value="",step="any",width="100%"),title="location",width=2),
              column(numericInput("sigmaMCVTModeMedianMeanOUP",label="sigma",value="",step="any",width="100%"),title="scale",width=2),
              column(width=2),
              column(numericInput("kMCVTModeMedianMeanOUP",label="k",value="",step="any",width="100%"),title="threshold",width=2),
              column(numericInput("tToMCVTModeMedianMeanOUP",label="t:To",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            fixedRow(style="height: 68px;",
              column(width=2),
              column(numericInput("ptmaxMCVTModeMedianMeanOUP",label="pv max",value="",step="any",width="100%"),title="maximum density",width=2),
              column(width=2),
              column(numericInput("pathsMCVTModeMedianMeanOUP",label="paths",value="",step="any",width="100%"),title="number of paths",width=2),
              column(numericInput("skipMCVTModeMedianMeanOUP",label="skip",value="",step="any",width="100%"),title="divide t:By into smaller intervals",width=2),
              column(numericInput("tFromMCVTModeMedianMeanOUP",label="t:From",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            # User action
            fixedRow(
              column(actionButton("clearMCVTModeMedianMeanOUP",HTML("_"),width="100%",class="btn-info"),title="clear and save arguments",style="padding-right: 2px;",width=1),
              column(actionButton("saveMCVTModeMedianMeanOUP",HTML("&equiv;"),width="100%",class="btn-info"),title="save arguments",style="padding-left: 2px;",width=1),
              column(actionButton("undnMCVTModeMedianMeanOUP",HTML("&Vee;"),width="100%",class="btn-success"),title="previous arguments",style="padding-right: 2px;",width=1),
              column(actionButton("unupMCVTModeMedianMeanOUP",HTML("&Wedge;"),width="100%",class="btn-success"),title="next arguments",style="padding-left: 2px;",width=1),
              column(actionButton("syncMCVTModeMedianMeanOUP","Sync",width="100%",class="btn-success"),title="states and thresholds",width=2),
              column(actionButton("axesMCVTModeMedianMeanOUP","Axes",width="100%",class="btn-success"),title="for t, x and pv",width=2),
              column(actionButton("plotMCVTModeMedianMeanOUP","Plot",width="100%",class="btn-success"),title="refresh plot",width=2),
              column(actionButton("otherMCVTModeMedianMeanOUP",HTML("&lessgtr;"),width="100%",class="btn-success"),title="other plot",style="padding-right: 2px;",width=1)
            ),
            # Plot
            wellPanel(class="wellPlotOUP",style="margin: 0 auto; height: 402px; width: 580px;",plotlyOutput("plotlyMCVTModeMedianMeanOUP"),copyPlot()),
            value="MCVTModeMedianMeanOUP"
          ),
          # Visiting Time Percentiles ----
          nav_panel(HTML("&emsp;Percentiles"),
            # User input
            fixedRow(style="height: 60px;",
              column(actionButton("infoMCVTPercentilesOUP","Info",width="100%",class="btn-primary"),title="information about Visiting Time Density",style="padding-top: 32px;",width=2),
              column(numericInput("xMCVTPercentilesOUP",label="x",value="",step="any",width="100%"),title="fixed initial state",width=2),
              column(width=2),
              column(width=2),
              column(width=2),
              column(numericInput("tByMCVTPercentilesOUP",label="t:By",value="",step="any",width="100%"),title="time increment",width=2)
            ),
            fixedRow(style="height: 60px;",
              column(numericInput("rhoMCVTPercentilesOUP",label="rho",value="",step="any",width="100%"),title="rate of convergence",width=2),
              column(numericInput("muMCVTPercentilesOUP",label="mu",value="",step="any",width="100%"),title="location",width=2),
              column(numericInput("sigmaMCVTPercentilesOUP",label="sigma",value="",step="any",width="100%"),title="scale",width=2),
              column(width=2),
              column(numericInput("kMCVTPercentilesOUP",label="k",value="",step="any",width="100%"),title="threshold",width=2),
              column(numericInput("tToMCVTPercentilesOUP",label="t:To",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            fixedRow(style="height: 68px;",
              column(numericInput("PpctMCVTPercentilesOUP",label="P%",value="",step="any",width="100%"),title="passage time probability",width=2),
              column(numericInput("ptmaxMCVTPercentilesOUP",label="pv max",value="",step="any",width="100%"),title="maximum density",width=2),
              column(width=2),
              column(numericInput("pathsMCVTPercentilesOUP",label="paths",value="",step="any",width="100%"),title="number of paths",width=2),
              column(numericInput("skipMCVTPercentilesOUP",label="skip",value="",step="any",width="100%"),title="divide t:By into smaller intervals",width=2),
              column(numericInput("tFromMCVTPercentilesOUP",label="t:From",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            # User action
            fixedRow(
              column(actionButton("clearMCVTPercentilesOUP",HTML("_"),width="100%",class="btn-info"),title="clear and save arguments",style="padding-right: 2px;",width=1),
              column(actionButton("saveMCVTPercentilesOUP",HTML("&equiv;"),width="100%",class="btn-info"),title="save arguments",style="padding-left: 2px;",width=1),
              column(actionButton("undnMCVTPercentilesOUP",HTML("&Vee;"),width="100%",class="btn-success"),title="previous arguments",style="padding-right: 2px;",width=1),
              column(actionButton("unupMCVTPercentilesOUP",HTML("&Wedge;"),width="100%",class="btn-success"),title="next arguments",style="padding-left: 2px;",width=1),
              column(actionButton("syncMCVTPercentilesOUP","Sync",width="100%",class="btn-success"),title="states and thresholds",width=2),
              column(actionButton("axesMCVTPercentilesOUP","Axes",width="100%",class="btn-success"),title="for t, x and pv",width=2),
              column(actionButton("plotMCVTPercentilesOUP","Plot",width="100%",class="btn-success"),title="refresh plot",width=2),
              column(actionButton("otherMCVTPercentilesOUP",HTML("&lessgtr;"),width="100%",class="btn-success"),title="other plot",style="padding-right: 2px;",width=1)
            ),
            # Plot
            wellPanel(class="wellPlotOUP",style="margin: 0 auto; height: 402px; width: 580px;",plotlyOutput("plotlyMCVTPercentilesOUP"),copyPlot()),
            value="MCVTPercentilesOUP"
          ),
          # Visiting Time Density ----
          nav_panel(HTML("&emsp;Density"),
            # User input
            fixedRow(style="height: 60px;",
              column(actionButton("infoMCVTDensityOUP","Info",width="100%",class="btn-primary"),title="information about Visiting Time Density",style="padding-top: 32px;",width=2),
              column(numericInput("xMCVTDensityOUP",label="x",value="",step="any",width="100%"),title="fixed initial state",width=2),
              column(actionButton("resetMCVTDensityOUP","Reset",width="100%",class="btn-success"),title="reset begin and end",style="padding-top: 32px;",width=2),
              column(numericInput("begMCVTDensityOUP",label="begin",value="",step="any",width="100%"),title="state to begin heat map",width=2),
              column(numericInput("endMCVTDensityOUP",label="end",value="",step="any",width="100%"),title="state to end heat map",width=2),
              column(numericInput("tByMCVTDensityOUP",label="t:By",value="",step="any",width="100%"),title="time increment",width=2)
            ),
            fixedRow(style="height: 60px;",
              column(numericInput("rhoMCVTDensityOUP",label="rho",value="",step="any",width="100%"),title="rate of convergence",width=2),
              column(numericInput("muMCVTDensityOUP",label="mu",value="",step="any",width="100%"),title="location",width=2),
              column(numericInput("sigmaMCVTDensityOUP",label="sigma",value="",step="any",width="100%"),title="scale",width=2),
              column(width=2),
              column(numericInput("kMCVTDensityOUP",label="k",value="",step="any",width="100%"),title="threshold",width=2),
              column(numericInput("tToMCVTDensityOUP",label="t:To",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            fixedRow(style="height: 68px;",
              column(width=2),
              column(numericInput("ptmaxMCVTDensityOUP",label="pv max",value="",step="any",width="100%"),title="maximum density",width=2),
              column(numericInput("pmaxMCVTDensityOUP",label="p max",value="",step="any",width="100%"),title="maximum for heat map of paths",width=2),
              column(numericInput("pathsMCVTDensityOUP",label="paths",value="",step="any",width="100%"),title="number of paths",width=2),
              column(numericInput("skipMCVTDensityOUP",label="skip",value="",step="any",width="100%"),title="divide t:By into smaller intervals",width=2),
              column(numericInput("tFromMCVTDensityOUP",label="t:From",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            # User action
            fixedRow(
              column(actionButton("clearMCVTDensityOUP",HTML("_"),width="100%",class="btn-info"),title="clear and save arguments",style="padding-right: 2px;",width=1),
              column(actionButton("saveMCVTDensityOUP",HTML("&equiv;"),width="100%",class="btn-info"),title="save arguments",style="padding-left: 2px;",width=1),
              column(actionButton("undnMCVTDensityOUP",HTML("&Vee;"),width="100%",class="btn-success"),title="previous arguments",style="padding-right: 2px;",width=1),
              column(actionButton("unupMCVTDensityOUP",HTML("&Wedge;"),width="100%",class="btn-success"),title="next arguments",style="padding-left: 2px;",width=1),
              column(actionButton("syncMCVTDensityOUP","Sync",width="100%",class="btn-success"),title="states and thresholds",width=2),
              column(actionButton("axesMCVTDensityOUP","Axes",width="100%",class="btn-success"),title="for t, x and pv",width=2),
              column(actionButton("plotMCVTDensityOUP","Plot",width="100%",class="btn-success"),title="refresh plot",width=2),
              column(actionButton("otherMCVTDensityOUP",HTML("&lessgtr;"),width="100%",class="btn-success"),title="other plot",style="padding-right: 2px;",width=1)
            ),
            # Plot
            wellPanel(class="wellPlotOUP",style="margin: 0 auto; height: 402px; width: 580px;",plotlyOutput("plotlyMCVTDensityOUP"),copyPlot()),
            value="MCVTDensityOUP"
          ),
          # Visiting Time Probability ----
          nav_panel(HTML("&emsp;Probability"),
            # User input
            fixedRow(style="height: 60px;",
              column(actionButton("infoMCVTProbabilityOUP","Info",width="100%",class="btn-primary"),title="information about Visiting Time Density",style="padding-top: 32px;",width=2),
              column(numericInput("xMCVTProbabilityOUP",label="x",value="",step="any",width="100%"),title="fixed initial state",width=2),
              column(actionButton("resetMCVTProbabilityOUP","Reset",width="100%",class="btn-success"),title="reset begin and end",style="padding-top: 32px;",width=2),
              column(numericInput("begMCVTProbabilityOUP",label="begin",value="",step="any",width="100%"),title="state to begin heat map",width=2),
              column(numericInput("endMCVTProbabilityOUP",label="end",value="",step="any",width="100%"),title="state to end heat map",width=2),
              column(numericInput("tByMCVTProbabilityOUP",label="t:By",value="",step="any",width="100%"),title="time increment",width=2)
            ),
            fixedRow(style="height: 60px;",
              column(numericInput("rhoMCVTProbabilityOUP",label="rho",value="",step="any",width="100%"),title="rate of convergence",width=2),
              column(numericInput("muMCVTProbabilityOUP",label="mu",value="",step="any",width="100%"),title="location",width=2),
              column(numericInput("sigmaMCVTProbabilityOUP",label="sigma",value="",step="any",width="100%"),title="scale",width=2),
              column(width=2),
              column(numericInput("kMCVTProbabilityOUP",label="k",value="",step="any",width="100%"),title="threshold",width=2),
              column(numericInput("tToMCVTProbabilityOUP",label="t:To",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            fixedRow(style="height: 68px;",
              column(width=2),
              column(width=2),
              column(numericInput("pmaxMCVTProbabilityOUP",label="p max",value="",step="any",width="100%"),title="maximum for heat map of paths",width=2),
              column(numericInput("pathsMCVTProbabilityOUP",label="paths",value="",step="any",width="100%"),title="number of paths",width=2),
              column(numericInput("skipMCVTProbabilityOUP",label="skip",value="",step="any",width="100%"),title="divide t:By into smaller intervals",width=2),
              column(numericInput("tFromMCVTProbabilityOUP",label="t:From",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            # User action
            fixedRow(
              column(actionButton("clearMCVTProbabilityOUP",HTML("_"),width="100%",class="btn-info"),title="clear and save arguments",style="padding-right: 2px;",width=1),
              column(actionButton("saveMCVTProbabilityOUP",HTML("&equiv;"),width="100%",class="btn-info"),title="save arguments",style="padding-left: 2px;",width=1),
              column(actionButton("undnMCVTProbabilityOUP",HTML("&Vee;"),width="100%",class="btn-success"),title="previous arguments",style="padding-right: 2px;",width=1),
              column(actionButton("unupMCVTProbabilityOUP",HTML("&Wedge;"),width="100%",class="btn-success"),title="next arguments",style="padding-left: 2px;",width=1),
              column(actionButton("syncMCVTProbabilityOUP","Sync",width="100%",class="btn-success"),title="states and thresholds",width=2),
              column(actionButton("axesMCVTProbabilityOUP","Axes",width="100%",class="btn-success"),title="for t, x and pv",width=2),
              column(actionButton("plotMCVTProbabilityOUP","Plot",width="100%",class="btn-success"),title="refresh plot",width=2),
              column(actionButton("otherMCVTProbabilityOUP",HTML("&lessgtr;"),width="100%",class="btn-success"),title="other plot",style="padding-right: 2px;",width=1)
            ),
            # Plot
            wellPanel(class="wellPlotOUP",style="margin: 0 auto; height: 402px; width: 580px;",plotlyOutput("plotlyMCVTProbabilityOUP"),copyPlot()),
            value="MCVTProbabilityOUP"
          ),
          # First Passage Time Mode Median Mean ----
          nav_item(tags$span(HTML("&ensp;&nbsp;First Passage Time..."),style="color: var(--bs-nav-link-color); font-weight: bold;")),
          nav_panel(HTML("&emsp;Mode, Median and Mean"),
            # User input
            fixedRow(style="height: 60px;",
              column(actionButton("infoMCFPTModeMedianMeanOUP","Info",width="100%",class="btn-primary"),title="information about Visiting Time Density",style="padding-top: 32px;",width=2),
              column(numericInput("xMCFPTModeMedianMeanOUP",label="x",value="",step="any",width="100%"),title="fixed initial state",width=2),
              column(width=2),
              column(width=2),
              column(width=2),
              column(numericInput("tByMCFPTModeMedianMeanOUP",label="t:By",value="",step="any",width="100%"),title="time increment",width=2)
            ),
            fixedRow(style="height: 60px;",
              column(numericInput("rhoMCFPTModeMedianMeanOUP",label="rho",value="",step="any",width="100%"),title="rate of convergence",width=2),
              column(numericInput("muMCFPTModeMedianMeanOUP",label="mu",value="",step="any",width="100%"),title="location",width=2),
              column(numericInput("sigmaMCFPTModeMedianMeanOUP",label="sigma",value="",step="any",width="100%"),title="scale",width=2),
              column(width=2),
              column(numericInput("kMCFPTModeMedianMeanOUP",label="k",value="",step="any",width="100%"),title="threshold",width=2),
              column(numericInput("tToMCFPTModeMedianMeanOUP",label="t:To",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            fixedRow(style="height: 68px;",
              column(width=2),
              column(numericInput("ptmaxMCFPTModeMedianMeanOUP",label="pf max",value="",step="any",width="100%"),title="maximum density",width=2),
              column(width=2),
              column(numericInput("pathsMCFPTModeMedianMeanOUP",label="paths",value="",step="any",width="100%"),title="number of paths",width=2),
              column(numericInput("skipMCFPTModeMedianMeanOUP",label="skip",value="",step="any",width="100%"),title="divide t:By into smaller intervals",width=2),
              column(numericInput("tFromMCFPTModeMedianMeanOUP",label="t:From",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            # User action
            fixedRow(
              column(actionButton("clearMCFPTModeMedianMeanOUP",HTML("_"),width="100%",class="btn-info"),title="clear and save arguments",style="padding-right: 2px;",width=1),
              column(actionButton("saveMCFPTModeMedianMeanOUP",HTML("&equiv;"),width="100%",class="btn-info"),title="save arguments",style="padding-left: 2px;",width=1),
              column(actionButton("undnMCFPTModeMedianMeanOUP",HTML("&Vee;"),width="100%",class="btn-success"),title="previous arguments",style="padding-right: 2px;",width=1),
              column(actionButton("unupMCFPTModeMedianMeanOUP",HTML("&Wedge;"),width="100%",class="btn-success"),title="next arguments",style="padding-left: 2px;",width=1),
              column(actionButton("syncMCFPTModeMedianMeanOUP","Sync",width="100%",class="btn-success"),title="states and thresholds",width=2),
              column(actionButton("axesMCFPTModeMedianMeanOUP","Axes",width="100%",class="btn-success"),title="for t, x and pv",width=2),
              column(actionButton("plotMCFPTModeMedianMeanOUP","Plot",width="100%",class="btn-success"),title="refresh plot",width=2),
              column(actionButton("otherMCFPTModeMedianMeanOUP",HTML("&lessgtr;"),width="100%",class="btn-success"),title="other plot",style="padding-right: 2px;",width=1)
            ),
            # Plot
            wellPanel(class="wellPlotOUP",style="margin: 0 auto; height: 402px; width: 580px;",plotlyOutput("plotlyMCFPTModeMedianMeanOUP"),copyPlot()),
            value="MCFPTModeMedianMeanOUP"
          ),
          # First Passage Time Percentiles ----
          nav_panel(HTML("&emsp;Percentiles"),
            # User input
            fixedRow(style="height: 60px;",
              column(actionButton("infoMCFPTPercentilesOUP","Info",width="100%",class="btn-primary"),title="information about Visiting Time Density",style="padding-top: 32px;",width=2),
              column(numericInput("xMCFPTPercentilesOUP",label="x",value="",step="any",width="100%"),title="fixed initial state",width=2),
              column(width=2),
              column(width=2),
              column(width=2),
              column(numericInput("tByMCFPTPercentilesOUP",label="t:By",value="",step="any",width="100%"),title="time increment",width=2)
            ),
            fixedRow(style="height: 60px;",
              column(numericInput("rhoMCFPTPercentilesOUP",label="rho",value="",step="any",width="100%"),title="rate of convergence",width=2),
              column(numericInput("muMCFPTPercentilesOUP",label="mu",value="",step="any",width="100%"),title="location",width=2),
              column(numericInput("sigmaMCFPTPercentilesOUP",label="sigma",value="",step="any",width="100%"),title="scale",width=2),
              column(width=2),
              column(numericInput("kMCFPTPercentilesOUP",label="k",value="",step="any",width="100%"),title="threshold",width=2),
              column(numericInput("tToMCFPTPercentilesOUP",label="t:To",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            fixedRow(style="height: 68px;",
              column(numericInput("PpctMCFPTPercentilesOUP",label="P%",value="",step="any",width="100%"),title="passage time probability",width=2),
              column(numericInput("ptmaxMCFPTPercentilesOUP",label="pf max",value="",step="any",width="100%"),title="maximum density",width=2),
              column(width=2),
              column(numericInput("pathsMCFPTPercentilesOUP",label="paths",value="",step="any",width="100%"),title="number of paths",width=2),
              column(numericInput("skipMCFPTPercentilesOUP",label="skip",value="",step="any",width="100%"),title="divide t:By into smaller intervals",width=2),
              column(numericInput("tFromMCFPTPercentilesOUP",label="t:From",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            # User action
            fixedRow(
              column(actionButton("clearMCFPTPercentilesOUP",HTML("_"),width="100%",class="btn-info"),title="clear and save arguments",style="padding-right: 2px;",width=1),
              column(actionButton("saveMCFPTPercentilesOUP",HTML("&equiv;"),width="100%",class="btn-info"),title="save arguments",style="padding-left: 2px;",width=1),
              column(actionButton("undnMCFPTPercentilesOUP",HTML("&Vee;"),width="100%",class="btn-success"),title="previous arguments",style="padding-right: 2px;",width=1),
              column(actionButton("unupMCFPTPercentilesOUP",HTML("&Wedge;"),width="100%",class="btn-success"),title="next arguments",style="padding-left: 2px;",width=1),
              column(actionButton("syncMCFPTPercentilesOUP","Sync",width="100%",class="btn-success"),title="states and thresholds",width=2),
              column(actionButton("axesMCFPTPercentilesOUP","Axes",width="100%",class="btn-success"),title="for t, x and pv",width=2),
              column(actionButton("plotMCFPTPercentilesOUP","Plot",width="100%",class="btn-success"),title="refresh plot",width=2),
              column(actionButton("otherMCFPTPercentilesOUP",HTML("&lessgtr;"),width="100%",class="btn-success"),title="other plot",style="padding-right: 2px;",width=1)
            ),
            # Plot
            wellPanel(class="wellPlotOUP",style="margin: 0 auto; height: 402px; width: 580px;",plotlyOutput("plotlyMCFPTPercentilesOUP"),copyPlot()),
            value="MCFPTPercentilesOUP"
          ),
          # First Passage Time Density ----
          nav_panel(HTML("&emsp;Density"),
            # User input
            fixedRow(style="height: 60px;",
              column(actionButton("infoMCFPTDensityOUP","Info",width="100%",class="btn-primary"),title="information about Visiting Time Density",style="padding-top: 32px;",width=2),
              column(numericInput("xMCFPTDensityOUP",label="x",value="",step="any",width="100%"),title="fixed initial state",width=2),
              column(actionButton("resetMCFPTDensityOUP","Reset",width="100%",class="btn-success"),title="reset begin and end",style="padding-top: 32px;",width=2),
              column(numericInput("begMCFPTDensityOUP",label="begin",value="",step="any",width="100%"),title="state to begin heat map",width=2),
              column(numericInput("endMCFPTDensityOUP",label="end",value="",step="any",width="100%"),title="state to end heat map",width=2),
              column(numericInput("tByMCFPTDensityOUP",label="t:By",value="",step="any",width="100%"),title="time increment",width=2)
            ),
            fixedRow(style="height: 60px;",
              column(numericInput("rhoMCFPTDensityOUP",label="rho",value="",step="any",width="100%"),title="rate of convergence",width=2),
              column(numericInput("muMCFPTDensityOUP",label="mu",value="",step="any",width="100%"),title="location",width=2),
              column(numericInput("sigmaMCFPTDensityOUP",label="sigma",value="",step="any",width="100%"),title="scale",width=2),
              column(width=2),
              column(numericInput("kMCFPTDensityOUP",label="k",value="",step="any",width="100%"),title="threshold",width=2),
              column(numericInput("tToMCFPTDensityOUP",label="t:To",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            fixedRow(style="height: 68px;",
              column(width=2),
              column(numericInput("ptmaxMCFPTDensityOUP",label="pf max",value="",step="any",width="100%"),title="maximum density",width=2),
              column(numericInput("pmaxMCFPTDensityOUP",label="p max",value="",step="any",width="100%"),title="maximum for heat map of paths",width=2),
              column(numericInput("pathsMCFPTDensityOUP",label="paths",value="",step="any",width="100%"),title="number of paths",width=2),
              column(numericInput("skipMCFPTDensityOUP",label="skip",value="",step="any",width="100%"),title="divide t:By into smaller intervals",width=2),
              column(numericInput("tFromMCFPTDensityOUP",label="t:From",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            # User action
            fixedRow(
              column(actionButton("clearMCFPTDensityOUP",HTML("_"),width="100%",class="btn-info"),title="clear and save arguments",style="padding-right: 2px;",width=1),
              column(actionButton("saveMCFPTDensityOUP",HTML("&equiv;"),width="100%",class="btn-info"),title="save arguments",style="padding-left: 2px;",width=1),
              column(actionButton("undnMCFPTDensityOUP",HTML("&Vee;"),width="100%",class="btn-success"),title="previous arguments",style="padding-right: 2px;",width=1),
              column(actionButton("unupMCFPTDensityOUP",HTML("&Wedge;"),width="100%",class="btn-success"),title="next arguments",style="padding-left: 2px;",width=1),
              column(actionButton("syncMCFPTDensityOUP","Sync",width="100%",class="btn-success"),title="states and thresholds",width=2),
              column(actionButton("axesMCFPTDensityOUP","Axes",width="100%",class="btn-success"),title="for t, x and pv",width=2),
              column(actionButton("plotMCFPTDensityOUP","Plot",width="100%",class="btn-success"),title="refresh plot",width=2),
              column(actionButton("otherMCFPTDensityOUP",HTML("&lessgtr;"),width="100%",class="btn-success"),title="other plot",style="padding-right: 2px;",width=1)
            ),
            # Plot
            wellPanel(class="wellPlotOUP",style="margin: 0 auto; height: 402px; width: 580px;",plotlyOutput("plotlyMCFPTDensityOUP"),copyPlot()),
            value="MCFPTDensityOUP"
          ),
          # First Passage Time Probability ----
          nav_panel(HTML("&emsp;Probability"),
            # User input
            fixedRow(style="height: 60px;",
              column(actionButton("infoMCFPTProbabilityOUP","Info",width="100%",class="btn-primary"),title="information about Visiting Time Density",style="padding-top: 32px;",width=2),
              column(numericInput("xMCFPTProbabilityOUP",label="x",value="",step="any",width="100%"),title="fixed initial state",width=2),
              column(actionButton("resetMCFPTProbabilityOUP","Reset",width="100%",class="btn-success"),title="reset begin and end",style="padding-top: 32px;",width=2),
              column(numericInput("begMCFPTProbabilityOUP",label="begin",value="",step="any",width="100%"),title="state to begin heat map",width=2),
              column(numericInput("endMCFPTProbabilityOUP",label="end",value="",step="any",width="100%"),title="state to end heat map",width=2),
              column(numericInput("tByMCFPTProbabilityOUP",label="t:By",value="",step="any",width="100%"),title="time increment",width=2)
            ),
            fixedRow(style="height: 60px;",
              column(numericInput("rhoMCFPTProbabilityOUP",label="rho",value="",step="any",width="100%"),title="rate of convergence",width=2),
              column(numericInput("muMCFPTProbabilityOUP",label="mu",value="",step="any",width="100%"),title="location",width=2),
              column(numericInput("sigmaMCFPTProbabilityOUP",label="sigma",value="",step="any",width="100%"),title="scale",width=2),
              column(width=2),
              column(numericInput("kMCFPTProbabilityOUP",label="k",value="",step="any",width="100%"),title="threshold",width=2),
              column(numericInput("tToMCFPTProbabilityOUP",label="t:To",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            fixedRow(style="height: 68px;",
              column(width=2),
              column(width=2),
              column(numericInput("pmaxMCFPTProbabilityOUP",label="p max",value="",step="any",width="100%"),title="maximum for heat map of paths",width=2),
              column(numericInput("pathsMCFPTProbabilityOUP",label="paths",value="",step="any",width="100%"),title="number of paths",width=2),
              column(numericInput("skipMCFPTProbabilityOUP",label="skip",value="",step="any",width="100%"),title="divide t:By into smaller intervals",width=2),
              column(numericInput("tFromMCFPTProbabilityOUP",label="t:From",value="",step="any",width="100%"),title="variable times",width=2)
            ),
            # User action
            fixedRow(
              column(actionButton("clearMCFPTProbabilityOUP",HTML("_"),width="100%",class="btn-info"),title="clear and save arguments",style="padding-right: 2px;",width=1),
              column(actionButton("saveMCFPTProbabilityOUP",HTML("&equiv;"),width="100%",class="btn-info"),title="save arguments",style="padding-left: 2px;",width=1),
              column(actionButton("undnMCFPTProbabilityOUP",HTML("&Vee;"),width="100%",class="btn-success"),title="previous arguments",style="padding-right: 2px;",width=1),
              column(actionButton("unupMCFPTProbabilityOUP",HTML("&Wedge;"),width="100%",class="btn-success"),title="next arguments",style="padding-left: 2px;",width=1),
              column(actionButton("syncMCFPTProbabilityOUP","Sync",width="100%",class="btn-success"),title="states and thresholds",width=2),
              column(actionButton("axesMCFPTProbabilityOUP","Axes",width="100%",class="btn-success"),title="for t, x and pv",width=2),
              column(actionButton("plotMCFPTProbabilityOUP","Plot",width="100%",class="btn-success"),title="refresh plot",width=2),
              column(actionButton("otherMCFPTProbabilityOUP",HTML("&lessgtr;"),width="100%",class="btn-success"),title="other plot",style="padding-right: 2px;",width=1)
            ),
            # Plot
            wellPanel(class="wellPlotOUP",style="margin: 0 auto; height: 402px; width: 580px;",plotlyOutput("plotlyMCFPTProbabilityOUP"),copyPlot()),
            value="MCFPTProbabilityOUP"
          ),
          id="navMCOUP",widths=c(3,9)
        ),
        value="tabMCOUP"
        #end list ----
      ),
    ),
    nav_menu("Help",
      nav_panel("What's on the Menu?",
        add_busy_spinner(spin="fingerprint",color="rgb(180,180,0)",position=c("top-right"),margins=c(450,350),height="128px",width="128px"),
        value="tabOnTheMenuOUP"
      ),
      nav_item(
        a(href="https://greghertzler.github.io/GregsOUPShiny/OUP_Shiny.html","Tutorials",target="_blank")
      ),
      nav_item(
        a(href="https://greghertzler.github.io/GregsOUPR6/OUP_Help.html","Reference",target="_blank")
      ),
      nav_panel("About",
        add_busy_spinner(spin="fingerprint",color="rgb(180,180,0)",position=c("top-right"),margins=c(450,350),height="128px",width="128px"),
        value="tabAboutOUP"
      ),
      nav_panel("License",
        add_busy_spinner(spin="fingerprint",color="rgb(180,180,0)",position=c("top-right"),margins=c(450,350),height="128px",width="128px"),
        value="tabLicenseOUP"
      ),
      nav_item(
        a(href="https://github.com/greghertzler/GregsOUPShiny/issues/","Bug Reports",target="_blank")
      )
    ),
    nav_item(input_dark_mode(id="darkmodeswitch")),
    id="navBar",window_title="ROAR"
  )
)
