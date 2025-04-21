library(mlrMBO)
library(mlr)
library(ParamHelpers)
library(parallelMap)
library(randomForest)

################################################################################

# Configura learner e controllo MBO
ctrl<-makeMBOControl()
ctrl<-setMBOControlTermination(ctrl, iters=50) # Aumentato numero iterazioni
ctrl<-setMBOControlInfill(ctrl, crit=makeMBOInfillCritCB()) # Confidence Bound per esplorazione/sfruttamento

# Modello di regressione: Random Forest per ridurre il rumore e migliorare la robustezza
lrn<-makeLearner("regr.randomForest", predict.type="se")

# Parallelizzazione compatibile Windows
parallelStartSocket(parallel::detectCores() - 1)

# Carica i pacchetti anche nei worker paralleli
packages<-c("tidyverse", "magrittr", "tidyquant", "tidymodels")
lapply(packages, parallelLibrary)

# Esporta funzioni e oggetti usati nella funzione obiettivo
parallelExport(
  "calculate_omega_ratio"
  ,"fun_composite_score"
  ,"fun_ichimoku"
  ,"optimize_SMA"
  ,"optimize_EMA"
  ,"optimize_MACD"
  ,"optimize_RSI"
  ,"optimize_STOCH"
  ,"optimize_BBands"
  ,"FX_Data"
)
################################################################################

#️ Definizione funzione obiettivo (minimizziamo il -score)
fun_obj_mlrMBO<-makeSingleObjectiveFunction(
  name="SMA_Optimization"
  ,fn=function(x){
    library(tidyverse); library(magrittr); library(tidyquant); library(tidymodels)
    
    # Aggiunta di un controllo per verificare i dati
    stopifnot(exists("FX_Data"))
    
    result<-optimize_SMA(x[["short_sma"]], x[["long_sma"]]
                         , forex_data=FX_Data%>%slice_tail(n=(1000))
    )
    return(result$Score * -1)  # Minimizzazione
  }
  ,par.set=makeParamSet(
    makeIntegerParam("short_sma", lower=short_SMA[1], upper=short_SMA[2]),
    makeIntegerParam("long_sma", lower=long_SMA[1], upper=long_SMA[2])
  )
  ,minimize=TRUE
)


# Esegui l’ottimizzazione
res<-mbo(fun=fun_obj_mlrMBO, learner=lrn, control=ctrl)

# Ferma i cluster
parallelStop()

# 🔍 Risultati
cat("Parametri ottimali:\n")
print(res$x)
res$x$short_sma
res$x$long_sma
cat("Score massimo:\n")
print(res$y * -1)  # Inversione segno per ottenere il massimo score
