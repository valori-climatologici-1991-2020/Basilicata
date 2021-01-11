#9gennaio 2021: Il file di input e' sensori.csv. Questo file e' stato ottenuto dal
#codice html della pagina: "http://www.centrofunzionalebasilicata.it/it/scaricaDati.php#"
#Per ogni stazione e' riportato il nome e il rispettivo codice. 

#Il problema e' che ogni stazione ha un proprio codice (codice sensore) che identifica la pioggia cumulata e la temperatura media.
#Quindi: una stazione e' identificata non solo dal codice stazione ma anche dal codice sensore. Per scrivere
#le stringhe da inviare al server della Basilicata abbiamo bisogno di inviare: il codice stazione, il codice sensore (che varia al variare delle stazioni)e il tipo di elaborazione richiesta (temperatura minima giornaliera, precipitazione cumulata dalle 0 alle 24)

#Per accedere a questi codici "sensori" e' necessario selezionare nel menu a tendina il nome della stazione:
#a quel punto nella pagina html si avranno disponibili i codici che identificano i vari parametri da scaricare (che nella pagina della basilicata sono chiamati sensori)

#Avendo a disposizione il codice della stazione e il codice del sensore e' possibile
#scaricare i dati senza passare per l'interfaccia web della Basilicata.

#Questo programma genera un file in cui per ogni stazione sono disponibili:
#codice stazione
#nome stazione
#codice sensore (ovvero pioggia, temperatura) e il nome che lo identifica
rm(list=objects())
library("tidyverse")
library("RSelenium")
library("rvest")

read_delim("sensori.csv",delim=";",col_names = TRUE)->sensori

myurl<-"http://www.centrofunzionalebasilicata.it/it/scaricaDati.php#"

rsDriver(port =4566L,version = "3.141.59",verbose=TRUE, browser = c("firefox"),check=F)->rD
remDr <- rD[["client"]]
remDr$open()

remDr$navigate(myurl)

purrr::map_dfr(1:nrow(sensori),.f=function(riga){
  
  Sys.sleep(8)
  
  sensori$nomeStaz[riga]->nomeStaz
  sensori$CodeStaz[riga]->codeStaz
  
  remDr$findElement(using = "xpath",value=glue::glue("/*//option[@value='{codeStaz}']"))->scegliStazione
  scegliStazione$clickElement()
  
  Sys.sleep(4)
  
  remDr$findElement(using = "xpath",value="//*[@id='scegli_rilevamento']")->scegliSensore
  #scegliSensore$sendKeysToElement(list("Temperatura Acqua",key="enter"))
  
  #sorgente della pagina corrispondente alla stazione selezionata
  remDr$getPageSource()->sorgentePagina
  
  xml2::read_html(sorgentePagina[[1]]) %>%
    rvest::html_nodes(xpath="/html/body/div[3]/section/form/table/tbody/tr[2]/td[2]/select/*")->opzioniSensori
  
  rvest::html_attr(opzioniSensori,name = "value")->codiciSensori
  rvest::html_text(opzioniSensori)->nomiSensori
  
  
  tibble(codiciSensori,nomiSensori,nomeStaz,sensori$CodeStaz[riga])
  
  
})->finale

write_delim(finale,file="sensori_basilicata.csv",delim=";",col_names=TRUE)
