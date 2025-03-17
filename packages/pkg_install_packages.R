{
  # Basic packages ----
  message("Upload ", "Basic packages")
  if(!require(devtools)){
    install.packages("devtools")
    library(devtools)
  }
  if(!require(pak)){#Another Approach to Package Installation
    install.packages("pak")
    library(pak)
  }
  if(!require(remotes)){
    install.packages("remotes")
    library(remotes)
  }
  #----
  if(!require(rlist)){
    install.packages("rlist")
    library(rlist)
  }
  if(!require(WriteXLS)){
    install.packages("WriteXLS")
    library(WriteXLS)
  }
  if(!require(tidyverse)){
    install.packages("tidyverse")
    library(tidyverse)
  }
  if(!require(magrittr)){
    install.packages("magrittr")
    library(magrittr)
  }
  if(!require(reshape2)){
    install.packages("reshape2")
    library(reshape2)
  }
  {
    if(!require(furrr)){
      install.packages("furrr")
      library(furrr)
    }
    furrr::furrr_options(stdout=TRUE
                         , conditions="condition"
                         , globals=TRUE
                         , packages=NULL
                         , seed=FALSE
                         , scheduling=2L
                         , chunk_size=NULL
                         , prefix=NULL)
    
    plan(multisession, workers=(parallel::detectCores()-1))
  }
  if(!require(progress)){
    install.packages("progress")
    library(progress)
  }
  if(!require(logr)){
    install.packages(file.path(path_packages, "common_1.1.3.tar.gz"), repos=NULL, type="source")
    install.packages(file.path(path_packages, "logr_1.3.8.tar.gz"), repos=NULL, type="source")
    library(logr)
  }
  if(!require(feather)){
    install.packages("feather")
    library(feather)
  }
  
  # Parameters Optimization ----
  message("Upload ", "Parameters Optimization packages")
  if(!require(rBayesianOptimization)){
    install.packages("rBayesianOptimization")
    library(rBayesianOptimization)
  }
  # if(!require(ParBayesianOptimization)){
  #   install.packages("ParBayesianOptimization")
  #   library(ParBayesianOptimization)
  # }
  if(!require(GA)){
    install.packages("GA")
    library(GA)
  }
  if(!require(microbenchmark)){
    install.packages("microbenchmark")
    library(microbenchmark)
  }
  if(!require(DEoptim)){
    install.packages("DEoptim")
    library(DEoptim)
  }
  if(!require(GenSA)){
    install.packages("GenSA")
    library(GenSA)
  }
  if(!require(nloptr)){
    install.packages("nloptr")
    library(nloptr)
  }
  
  # Artificial Intelligence ----
  message("Upload ", "Artificial Intelligence packages")
  if(!require(gemini.R)){
    install.packages("gemini.R")
    library(gemini.R)
  }
  if(!require(askgpt)){
    install.packages("askgpt")
    library(askgpt)
  }
  if(!require(gptstudio)){
    # usethis::edit_r_environ()
    install.packages("gptstudio")
    library(gptstudio)
  }
  if(!require(gpttools)){
    # Enable repository from jameshwade
    options(repos=c(
      jameshwade="https://jameshwade.r-universe.dev",
      CRAN="https://cloud.r-project.org"
    ))
    # Download and install gpttools in R
    install.packages("gpttools")
    library(gpttools)
  }
  # if(!require(transformers)){
  #   # https://rpubs.com/eR_ic/transfoRmers
  #   remotes::install_github("huggingface/transformers")
  #   library(transformers)
  # }
  # if(!require(huggingfaceR)){
  #   # https://github.com/farach/huggingfaceR
  #   devtools::install_github("farach/huggingfaceR")
  #   library(huggingfaceR)
  # }
  
  # Python Interfaces ----
  message("Upload ", "Python Interfaces packages")
  if(!require(reticulate)){
    install.packages("reticulate")
    library(reticulate)
  }
  
  # Quant packages ----
  message("Upload ", "Quant packages")
  if(!require(quantmod)){
    install.packages("quantmod")
    library(quantmod)
  }
  if(!require(tidyquant)){
    install.packages("tidyquant")
    library(tidyquant)
  }
  if(!require(TTR)){
    install.packages("TTR")
    library(TTR)
  }
  if(!require(PerformanceAnalytics)){
    install.packages("PerformanceAnalytics")
    library(PerformanceAnalytics)
  }
  if(!require(ParBayesianOptimization)){
    install.packages("ParBayesianOptimization")
    library(ParBayesianOptimization)
  }
  
  # Time Series models ----
  message("Upload ", "Time Series models packages")
  if(!require(tidymodels)){
    install.packages("tidymodels")
    library(tidymodels)
  }
  if(!require(modeltime)){
    install.packages("modeltime")
    library(modeltime)
  }
  if(!require(modeltime.ensemble)){
    install.packages("modeltime.ensemble")
    library(modeltime.ensemble)
  }
  if(!require(fable)){
    install.packages("fable")
    library(fable)
  }
  if(!require(glmnet)){
    install.packages("glmnet")
    library(glmnet)
  }
  if(!require(parsnip)){
    install.packages("parsnip")
    library(parsnip)
  }
  if(!require(earth)){
    install.packages("earth")
    library(earth)
  }
  if(!require(kernlab)){
    install.packages("kernlab")
    library(kernlab)
  }
  if(!require(ranger)){
    install.packages("ranger")
    library(ranger)
  }
  if(!require(xgboost)){
    install.packages("xgboost")
    library(xgboost)
  }
  if(!require(lightgbm)){
    install.packages("lightgbm")
    library(lightgbm)
  }
  if(!require(finetune)){
    install.packages("finetune")
    library(finetune)
  }
  if(!require(tsibble)){
    install.packages("tsibble")
    library(tsibble)
  }
  if(!require(timetk)){
    install.packages("timetk")
    library(timetk)
  }
  if(!require(fmdates)){
    install.packages(file.path(path_packages, "fmdates_0.1.4.tar.gz"), repos=NULL, type="source")
    library(fmdates)
  }
  if(!require(rdbnomics)){
    install.packages("rdbnomics")
    library(rdbnomics)
  }
  if(!require(tune)){
    install.packages("tune")
    library(tune)
  }
  
  # Graph packages ----
  message("Upload ", "Graph packages")
  if(!require(plotly)){
    install.packages("plotly")
    library(plotly)
  }
  
  # Table packages ----
  message("Upload ", "Table packages")
  if(!require(DT)){
    install.packages("DT")
    library(DT)
  }
  if(!require(formattable)){
    install.packages("formattable")
    library(formattable)
  }
  
  # Shiny packages ----
  message("Upload ", "Shiny packages")
  if(!require(shiny)){
    install.packages("shiny")
    library(shiny)
  }
  if(!require(shinydashboard)){
    install.packages("shinydashboard")
    library(shinydashboard)
  }
  if(!require(shinydashboardPlus)){
    install.packages("shinydashboardPlus")
    library(shinydashboardPlus)
  }
  if(!require(shinyWidgets)){
    install.packages("shinyWidgets")
    library(shinyWidgets)
  }
  # https://datastorm-open.github.io/shinymanager/
  if(!require(shinymanager)){
    install.packages("shinymanager")
    library(shinymanager)
  }
  if(!require(shiny.fluent)){
    install.packages("shiny.fluent")
    library(shiny.fluent)
  }
  if(!require(shiny.react)){
    install.packages("shiny.react")
    library(shiny.react)
  }
  if(!require(shinyjs)){
    install.packages("shinyjs")
    library(shinyjs)
  }
  if(!require(jsonlite)){
    install.packages("jsonlite")
    library(jsonlite)
  }
  if(!require(httr)){
    install.packages("httr")
    library(httr)
  }
  if(!require(htmlwidgets)){
    install.packages("htmlwidgets")
    library(htmlwidgets)
  }
  if(!require(htmltools)){
    install.packages("htmltools")
    library(htmltools)
  }
  if(!require(cachem)){
    install.packages("cachem")
    library(cachem)
  }
  if(!require(arrow)){
    install.packages("arrow")
    library(arrow)
  }
  
  print("ALL PACKAGES UPLOADED!!!")
}
