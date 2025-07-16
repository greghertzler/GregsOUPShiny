# GregsOUPShiny
Real Options for Adoption and Resilience:  Ornstein-Uhlenbeck Process:  Shiny app

GregsOUPR6 is an R app written with R6 objects, to run locally from the console.<br>
GregsOUPShiny is a Shiny app powered by GregsOUPR6, to deploy on a server.<br>
GregsOUP is both, to run the R and Shiny apps locally.<br>
GregsOUPExcel contains Excel workbooks with user-defined functions and VBA code for application development.

Description:
Analytical, Finite Difference, Maximum Likelihood and Monte Carlo methods--a complete set of functions for maximum likelihood estimation and the calculation of probabilities, option prices, decision thresholds, visiting times, first passage times and more--everything for a real options analysis.

Versions:
Excel Version: 1.4.5.0 (Stochastic Process.Modules.Help.Bugs)<br>
R Version: 1.3.5.0 (Stochastic Process.Modules.Help.Bugs)

Code:
Excel user-defined functions, Visual Basic for Applications code, R6 objects and a Shiny App.

Help:
Ribbon Help--context-sensitive help for Excel workbooks, the R app and a website.

Deploy Shiny App using RStudio and shinyapps.io<br>
1. Create shinyapps.io account on their website.<br>
2. Follow the instructions for Tokens and Secrets to link with RStudio.<br>
3. In RStudio, from a blank console, install GregsOUPR6 package in your default library,<br>
    devtools::install_github("greghertzler/GregsOUPR6",force=TRUE)<br>
4. In GitHub Desktop, clone the repo "greghertzler/GregsOUPShiny"<br>
5. In RStudio, go to the Session menu Set Working Directory and choose GregsOUPShiny<br>
6. In the upper right of the screen "RunApp" to make sure it works locally.<br>
7. Also in the upper right, "Publish" to push it to shinyapps.io<br>
8. Repeat 6 and 7 until it works.

I failed to publish GregsOUP with both R6 and Shiny in one package.  That's why they are split.

The roxygen markdown gets converted to html and put in the /html directory when GregsOUPR6 is built.  The /data directory of GregsOUPR6 is accessible in shinyapps.io, but the /html directory seems to disappear.  The /html directory and files describing the data were copied from GregsOUPR6 to the /www directory in GregsOUPShiny and the directory was set to htmlpath <- paste(sep="",getwd(),"/www/html/")
 at the top of the server.  The absolute path seems to be required by the read_html function that is used to put the html files onto a modal dialog.  Thus the html files are accessible in shinyapps.io.
