Correlation gadget on Quality of Govenment data - Social Policy data
=============================

[See the application in process](http://glimmer.rstudio.com/muuankarski/QogSocPol)

This repository has the source code for running similar application as this on your local computer.

## Steps to follow

1. start a new project based on version control in RStudio
2. copy-paste the url <`https://github.com/muuankarski/QoqSocPol`> of this repository and give the project name of your liking
3. create a data folder and run `create_data.R` script (requites RCurl-package)
3. Install the latest deveplopment version of shiny
`if (!require("devtools"))
  install.packages("devtools")
devtools::install_github("shiny", "rstudio")`
4. load shiny `library(shiny)`
5. fire the app `runApp()`