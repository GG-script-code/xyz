# Installa il pacchetto rdbnomics se non è già installato
if (!requireNamespace("rdbnomics", quietly = TRUE)) {
  install.packages("rdbnomics")
}
library(rdbnomics)
library(dplyr)
library(purrr)

DBnomics_providers<-rdb_providers()
DBnomics_datasets<-rdb_datasets()

dataset_tibble<-tibble(Provider=as.character(NA)
                       ,code=as.character(NA)
                       ,name=as.character(NA))

for(prov in names(DBnomics_datasets)){
  # prov<-names(DBnomics_datasets)[1]
  dataset_tibble<-bind_rows(dataset_tibble
                            ,DBnomics_datasets[[prov]]%>%
                              as_tibble())%>%
    mutate(Provider=replace_na(Provider,prov))%>%
    na.omit()%>%
    distinct()
  
}
dataset_tibble$Provider%>%unique()
dataset_tibble<-dataset_tibble%>%
  rename("name_dataset"="name")%>%
  left_join(DBnomics_providers%>%
              as_tibble()%>%
              rename("name_provider"="name")
            , by=join_by("Provider"=="code"))

WriteXLS::WriteXLS(dataset_tibble, "DBnomics_datasets.xlsx")
saveRDS(dataset_tibble, "DBnomics_datasets.Rds")

# Funzione per scaricare i dati economici principali con controlli di errore
scarica_dati_economici <- function() {
  # Definisci i codici degli indicatori economici e le aree geografiche
  indicatori <- list(
    CPI = "OECD/MEI/CPALTT01",
    GDP = "OECD/MEI/B1_GE",
    UNEMP = "OECD/MEI/UNR",
    PPI = "OECD/MEI/PPI",
    TRADE_BALANCE = "OECD/MEI/TRADE_BALANCE",
    RETAIL_SALES = "OECD/MEI/RS",
    CONSUMER_CONFIDENCE = "OECD/MEI/CSCICP",
    INTEREST_RATE = "OECD/MEI/IR",
    LABOR_FORCE_PARTICIPATION = "OECD/MEI/LFPRT"
  )
  paesi <- c("USA", "CAN", "EMU", "CHE", "GBR")
  
  # Funzione per ottenere i dati con gestione degli errori
  get_data <- function(indicatore, paese) {
    codice <- paste0(indicatore, "/", paese)
    tryCatch({
      rdb(ids = codice)
    }, error = function(e) {
      message(paste("Errore nel recupero dei dati per", indicatore, paese, ": ", e$message))
      NULL
    })
  }
  
  # Usa map per scaricare i dati e assegnarli correttamente
  dati <- map_dfr(names(indicatori), function(indicatore) {
    map_dfr(paesi, function(paese) {
      data <- get_data(indicatori[[indicatore]], paese)
      if (!is.null(data)) {
        data %>%
          mutate(Indicatore = indicatore, Paese = paese)
      } else {
        NULL
      }
    })
  })
  
  # Restituisci i dati
  return(dati)
}

# Scarica e visualizza i dati economici
dati_economici <- scarica_dati_economici()
print(dati_economici)
