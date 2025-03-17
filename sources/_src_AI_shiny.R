{
  if(!require(shiny)){
    install.packages("shiny")
    library(shiny)
  }
  
  if(!require(shinychat)){
    install.packages("shinychat")
    library(shinychat)
  }
  # usethis::edit_r_environ()
  if(!require(ellmer)){
    install.packages("ellmer")
    library(ellmer)
  }
}

#	Spiegazione del codice
#	chat_ollama() : Crea una connessione al modello Llama 3 in esecuzione su Ollama.
#	chat_ui("chat") : Usa shinychat per creare una chat interattiva in R.
#	chat$stream_async(user_text) : Esegue richieste asincrone, permettendo risposte in streaming.
#	chat_append("chat", stream) : Aggiorna l'interfaccia con la risposta del modello.


# Creare un oggetto chat con Llama 3 utilizzando Ollama
chat <- chat_ollama(
  model = "phi",  
  system_prompt = "Devi essere un assistente esperto di forex e trading."
)

# Definizione dell'UI con shinychat
ui <- bslib::page_fluid(
  chat_ui("chat") # Interfaccia chat interattiva
)

# Definizione del server
server <- function(input, output, session) {
  observeEvent(input$chat_user_input, {
    user_text <- input$chat_user_input
    if (nchar(user_text) > 0) {
      stream <- chat$stream_async(user_text) # Chiamata asincrona al modello
      chat_append("chat", stream) # Mostra il risultato nella chat
    }
  })
}

# Avviare l'app Shiny
shinyApp(ui, server)