library(httr)
library(jsonlite)
library(data.table)

# 📌 Funzione per scaricare dati Forex da Dukascopy API
get_dukascopy_quotes <- function(symbol = "CADCHF", timeframe = "H1", 
                                 start_date = "2024-02-01", end_date = "2024-02-07") {
  
  # Converte le date in timestamp Unix (millisecondi)
  start_timestamp <- as.numeric(as.POSIXct(start_date, tz = "UTC")) * 1000
  end_timestamp <- as.numeric(as.POSIXct(end_date, tz = "UTC")) * 1000
  
  # Costruisce l'URL dell'API
  base_url <- "https://freeserv.dukascopy.com/2.0/?path=api/quotes"
  url <- paste0(base_url, 
                "&symbols=", symbol, 
                "&timeframe=", timeframe, 
                "&from=", start_timestamp, 
                "&to=", end_timestamp)
  
  # Effettua la richiesta API
  response <- GET(url)
  
  # Verifica se la richiesta ha avuto successo
  if (http_status(response)$category != "Success") {
    stop("❌ Errore nel download dei dati da Dukascopy API")
  }
  
  # Converte la risposta JSON in lista R
  data <- fromJSON(content(response, as = "text"))
  
  # Verifica se ci sono dati validi
  if (length(data$quotes) == 0) {
    stop("⚠️ Nessun dato ricevuto dalle API per il periodo selezionato")
  }
  
  # Converte i dati in data.table
  df <- data.table(data$quotes)
  
  # Converte il timestamp
  df[, time := as.POSIXct(time / 1000, origin = "1970-01-01", tz = "UTC")]
  
  return(df)
}

# 📌 **Esegui la funzione per ottenere dati Forex per un range di date**
cadchf_data <- get_dukascopy_quotes(symbol = "CADCHF", timeframe = "M5", 
                                    start_date = "2024-02-01", end_date = "2024-02-07")

# Mostra i primi dati
head(cadchf_data)
