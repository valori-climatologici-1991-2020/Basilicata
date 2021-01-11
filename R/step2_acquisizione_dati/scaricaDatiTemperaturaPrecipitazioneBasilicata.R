rm(list=objects())
library("tidyverse")
library("tidylog")

annoI<-1991
annoF<-2020

seq.Date(from=as.Date(glue::glue("{annoI}-12-31")),
         to=as.Date(glue::glue("{annoF}-12-31")),by="year")->calendario

#La Basilicata mette a disposizione sia la precipitazione tra le 0 e le 24 e la precipitazione dalle 9 alle 9
#A priori non sappiamo se al sensore "Pioggia cumulata" corrisponda solo la precipitazione dalle 0 alle 24,
#la precipitazione cumulata dalle 9 alle 9 oppure entrambe.
#Prima scarichiamo la precipitazione dalle 0 alle 24 (parametro Prec). Infine facciamo rigirare il programma
#con il parametro Prec99: se il programma vede che ci sono gia' file che iniziano con "Prec" e che riportano il
#codice della stazione di cui si vogliono scaricare i dati, il programma passa alla stazione successiva. 
#Ovvero: se ho i dati dalle 0 alle 24 non riscarico i dati dalle 9 alle 9!
PARAMETRO<-c("Tmax","Tmin","Prec","Prec99")[4]

if(grepl("^Tm.+",PARAMETRO)){
  ifelse(PARAMETRO=="Tmax","ElabDailyMaximum","ElabDailyMinimum")->elabora
}else{
  ifelse(PARAMETRO=="Prec","ElabDailyIncrements","ElabDailyIncrements99")->elabora
}


#questo file "sensori_basilicata.csv" e' stato acquisito mediante il programma: selenium_acquisisciListaSensori.R 
read_delim(file="sensori_basilicata.csv",delim=";",col_names = TRUE)->sensori


if(grepl("^Tm.+",PARAMETRO)){
  #scarica Temperatura massima
  sensori %>%
    filter(nomiSensori=="Temperatura Aria")->sensoriTemp
}else{
  #scarica Pioggia Cumulata
  sensori %>%
    filter(nomiSensori=="Pioggia Cumulata")->sensoriTemp
}

#da qui in poi si chiama tutto sensoriTemp..ma parliamo anche di pioggia

purrr::walk(1:nrow(sensoriTemp),.f=function(riga){
  
  #se per Pioggia Cumulata ho gia' scaricati i dati dalle 0 alle 24, non voglio
  #riscaricare (e perdere tempo con) i dati dalle 9 alle 9.
  sensoriTemp[riga,]$codeStaz->codice
  list.files(pattern=glue::glue("^P.+_{codice}.+csv"))->ffile
  if(length(ffile)) return()
  
  DA_SCARICARE<-TRUE

  purrr::walk(calendario,.f=function(startDate){
    
    #DA_SCARICARE e' FALSE per la Pioggia Cumulata nel caso non esista il cumulato
    #24 - 24 o 9 -9. Nonpossiamo sapere a priori quale cumulato sia disponibile
    if(!DA_SCARICARE) return()
    
    print(glue::glue("Provo: {sensoriTemp$nomeStaz[riga]} Data Inizio {startDate}"))

    lubridate::leap_year(lubridate::year(startDate))->leapYear
    
    ifelse(leapYear,366,365)->interval
    
    id<-sensoriTemp$codeStaz[riga]
    sensNum<-sensoriTemp$codiciSensori[riga]
  
    if(grepl("^Tm.+",PARAMETRO)){
      #nell'url compare startDate ma in realta' deve essere indicata la data finale dello scarico
      glue::glue("http://www.centrofunzionalebasilicata.it/it/scaricaDati.php?action=download&id={id}&sensNum={sensNum}&exmethod={elabora}_Date&startDate={startDate}&interval={interval}#")->urlDati
    }else{
      #nell'url compare startDate ma in realta' deve essere indicata la data finale dello scarico
      glue::glue("http://www.centrofunzionalebasilicata.it/it/scaricaDati.php?action=download&id={id}&sensNum={sensNum}&exmethod={elabora}&startDate={startDate}&interval={interval}#")->urlDati
    }
    

    readr::read_file(urlDati)->datiRaw
    
    #Per la Pioggia Cumulata non possiamo sapere se esiste il cumulato 24-24 o 9 -9
    #Tocca fare entrambe le richieste. Se si ottiene come output una pagina html
    #allora il sensore non esiste e si passa alla stazione successiva
    if(grepl("html",datiRaw)){
      DA_SCARICARE<<-FALSE
      return()
    }
        
    str_split(datiRaw,"\n")->dati
    
    unlist(dati)->dati
    
    Sys.sleep(8)
    
    if(length(dati)<5) return()
    
    length(dati)->len
    as_tibble(dati[2:len])->daScrivere
    
    readr::write_delim(daScrivere,file=glue::glue("{PARAMETRO}_{id}_{sensNum}_{startDate}.csv"),delim=";")

  })
           
})           