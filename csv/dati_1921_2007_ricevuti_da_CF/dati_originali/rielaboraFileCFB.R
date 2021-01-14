#Revisione gennaio 2021
#
#crea file in formato HisCentral partendo dati dati del centro funzionale
#I codici: (L),(9) non sono documentati. Confrontando i valori dei file con i dati negli Annali dal
#1988 al 1999 disponibili sul sito di Arpa Basilicata questi due simboli sembrano corrispondere
#comunque a dati validi
#Il codice (A) di sicuro corrisponde a valori di precipitazione cumulata. Si è trovato
#riscontro di un solo valore negli Annali pdf relativo a un dato con (A)
#I codici (G),(C),(X),(Y) non sono documentati e non si è riusciti a trovare riscontro
#negli annali: vengono presi come validi
#Il codice (B) corrisponde al dato incerto e viene comunque preso come valido
#Il codice (I) sembra dovuto solo a un errato allineamento dei dati nel file (si veda Cogliandrino 1999 Precipitazione)

#TEMPERATURA: 
#(T) trovato riscontro a Sciffra 1989. Dovrebbe corrispondere a un dato valido, non viene eliminato
#(Q) non si è trovato riscontro per questo codice, ma sembrerebbe corrispondere agli 0.0 per indicare dato mancante. Viene annullato
#(R) potrebbe essere dato ricostruito
#(S) e (C) nessun riscontro
rm(list=objects())
library("stringr")
library("tidyr")
library("magrittr")
library("dplyr")
library("readr")
library("purrr")
library("lubridate")
library("guido")
library("visdat")

PARAMETRO<-c("Precipitation","Tmax","Tmin")[1]

annoI<-1921
annoF<-2007
creaCalendario(annoI,annoF)->calendario

read_delim("anagraficaBasilicata.csv",col_names = TRUE,delim=";")->ana
list.files(pattern=paste0("^.+",PARAMETRO,".csv"))->ffile

purrr::map(ffile,.f=function(nome.file){
  
  unlist(str_split(nome.file,"-"))[1]->nomeStazione

  
  #cerca codice in anagrafica
  grep(paste0("^",nomeStazione,"$"),ana$SiteName)->indexAna
  
  if(length(indexAna)!=1){
    
    sink(paste0("_logNomeSTazioneNonTrovato",PARAMETRO,".txt"),append=TRUE)
    print(nomeStazione)
    sink()
    
    return()
  }#
  
  #acquisiamo idstaz
  ana[indexAna,]$SiteID->idstaz
  
  read_lines(file=nome.file)->textFile

  
  #eliminiamo le righe di testo vuote
  grep("^$",textFile)->indexEmpty
  if(!length(indexEmpty)){
    stop(paste0("Non trovata nessuna riga vuota nel file: ",nome.file," Verificare"))
  }
  
  #cerchiamo le righe che contengono anno
  textFile[-indexEmpty]->textFile
  #troviamo le righe che contengono l'anno
  grep("^[[:space:]]+Anno:[[:space:]]+[[:digit:]]{4}",textFile)->indexAnno
  if(!length(indexAnno)){
    stop(paste0("Non trovata nessuna riga con l'anno nel file: ",nome.file," Verificare"))    
  }
  
  textFile[indexAnno]->anni
  unlist(lapply(anni,FUN=function(strYear){
    as.integer(unlist(str_split(strYear,":"))[2])
  }))->anni
    
  stopifnot(all(anni %in% seq(1900,2020)))

  #estraiamo i dati
  lapply(1:length(indexAnno),FUN=function(ii){
    
    indexAnno[ii]->riga
    anni[ii]->annoAttuale
    print(paste0("Nome stazione: ",nomeStazione," Elaboro anno: ",annoAttuale))
    
    textFile[riga+1]->testoMese
    if(!length(grep("Mese",testoMese))){
      print(textFile[riga])
      stop(paste0("Non trovata riga mese nel file: ",nome.file," Verificare"))    
    } #errore, riga+1 non contiene mese
    textFile[seq(riga+2,riga+13)]->dati
    str_replace_all(dati,"\\) ","\\);")->dati2
    #tolgo il mese iniziale
    str_replace_all(dati2,"^[[:space:]]*([[:digit:]]{1,2})[[:space:]]*","\\1;")->dati3
    str_split(dati3,";")->dati4
    mesi<-seq(1,12)
    vector(mode="list",length=12)->matriceValoriAnnuali
    lapply(mesi,FUN=function(mm){
      
      dati4[[mm]]->singoloMese
      
      if(mm %in% c(1,3,5,7,8,10,12)){
        meseLen<-32
      }else if(mm %in% c(4,6,9,11)){
        meseLen<-31        
      }else{
        
        if(leap_year(annoAttuale)){
          meseLen<-30
        }else{
          meseLen<-29
        } #if per febbraio
        
      }#fine if su mm
      
      if(length(singoloMese)!=meseLen){
        print(textFile[riga])
        print(singoloMese)
        stop(paste0("Errore riga con dati mese nel file: ",nome.file," Verificare"))    
      }
      
      if(as.integer(singoloMese[1])!=mm){
        print(textFile[riga])
        print(singoloMese)
        stop(paste0("Valore del mese non valido: ",nome.file," Verificare"))            
      }
      
      singoloMese[-1]->singoloMese

      #(M) dato mancante. Tutto il resto lo prendiamo come buono (dato valido, dato incerto e dato provvisorio)
      #(Q) 0.0 indicanti dato mancante nella  temperatura
      #(A) probabilmente valore cumulato
      grep("\\( *[MAQ] *\\)",singoloMese)->indexNA
      singoloMese[indexNA]<-NA
      
      grep("\\( *[01P9LBITRSC] *\\)",singoloMese)->indexVALIDO
      if(length(indexVALIDO)) str_replace(singoloMese[indexVALIDO],"\\( *[01P9LBITRSC] *\\)","")->singoloMese[indexVALIDO]
      tryCatch({
        as.double(singoloMese)->z
        print("**********")
        print(z)
        print(singoloMese)
        print("**********")
        z
      },error=function(e){
        sink("_logErroreConversione.txt",append=TRUE)
        print(paste0(idstaz,"\n"))
        print(paste0(nome.file,"\n"))        
        print(singoloMese)
        sink()
        if(annoAttuale >=1988 && annoAttuale<=1999) stop("mi fermo")
      })->valoriNumerici
      
      length(valoriNumerici)->numeroElementi
      if(numeroElementi<31) c(valoriNumerici,rep(NA,31-numeroElementi))->valoriNumerici
      
      #if(!is.null(fermo) && annoAttuale>=1988) browser()
      matriceValoriAnnuali[[mm]]<<-valoriNumerici
      
    }) #fine lapply su mm
    names(matriceValoriAnnuali)<-mesi
    as_data_frame(matriceValoriAnnuali)->myDF
    myDF %<>%mutate(yy=annoAttuale,dd=1:31) %>% gather(key=mm,value=DataValue,-yy,-dd) %>% select(yy,mm,dd,everything())
    myDF %<>% mutate(mm=str_pad(mm,pad="0",width=2,side="left"),dd=str_pad(dd,pad="0",width=2,side="left"))
    myDF %<>% mutate(time=as.Date(paste0(yy,"-",mm,"-",dd)) ) %>% filter(!is.na(time)) %>% mutate(time=as.character(paste0(time," 00:00:00"))) 
    
  })->listaMatrici #fine lapply su indexAnno


  do.call("rbind",listaMatrici)->finale
  
  finale %>% 
    select(yy,mm,dd,DataValue) %>%
    mutate(yy=as.integer(yy),mm=as.integer(mm),dd=as.integer(dd))->finale
  
  names(finale)[4]<-idstaz

  
  finale[!duplicated(finale[,c("yy","mm","dd")]),]
  
})->listaOut

purrr::compact(listaOut)->listaOut

if(!length(listaOut)) stop(glue::glue("Nessun dato per {PARAMETRO}"))

purrr::reduce(.x=listaOut,.f=left_join,.init=calendario)->daScrivere

write_delim(daScrivere,file=glue::glue("{PARAMETRO}_{annoI}_{annoF}_basilicata_datiInviatiDaCF.csv"),delim=",",col_names = TRUE)
