rm(list=objects())
library("tidyverse")

annoI<-1991
annoF<-2020

PARAMETRO<-c("Prec","Tmax","Tmin")[3]

creaCalendario<-function(annoI,annoF){
  
  if(missing(annoI)) stop("Anno inizio mancante")
  if(missing(annoF)) stop("Anno fine mancante")
  
  as.integer(annoI)->annoI
  as.integer(annoF)->annoF
  
  stopifnot(annoI<=annoF)
  
  seq.Date(from=as.Date(glue::glue("{annoI}-01-01")),to=as.Date(glue::glue("{annoF}-12-31")),by="day")->yymmdd
  
  tibble(yymmdd=yymmdd) %>%
    tidyr::separate(yymmdd,into=c("yy","mm","dd"),sep="-") %>%
    dplyr::mutate(yy=as.integer(yy),mm=as.integer(mm),dd=as.integer(dd))
  
}

creaCalendario(annoI,annoF)->calendario


list.files(pattern=glue::glue("{PARAMETRO}_.+csv$"))->ffile
purrr::map(ffile,.f=~(unlist(str_split(.,pattern="_"))))->listaOut
unique(purrr::map_chr(listaOut,2))->codiciStaz

purrr::map_dfc(1:length(codiciStaz),.f=function(ii){
  
  codiciStaz[ii]->codice
  
  list.files(pattern=glue::glue("{PARAMETRO}_{codice}.+csv$"))->ffile
  
  purrr::map_dfr(ffile,.f=function(nomeFile){
    
    message(glue::glue("Leggo {nomeFile}"))
    readr::read_file(nomeFile)->stringa

    unlist(str_split(stringa,pattern="\n"))->listaDati
    listaDati[grepl("[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}",listaDati)]->listaDati
  
    purrr::map_dfr(listaDati,.f=function(ll){
      
      unlist(str_split(str_remove_all(ll,"\""),";"))->valori
      lubridate::ymd(as.Date(str_remove(valori[1],pattern=" .+$"),format="%d/%m/%Y"))->yymmdd
      as.numeric(str_replace(valori[2],",","."))->value
      tibble(yy=as.integer(lubridate::year(yymmdd)),mm=as.integer(lubridate::month(yymmdd)),dd=as.integer(lubridate::day(yymmdd)),val=value)->df
      names(df)[4]<-codice
      
      df
      
    })->datiStazione
    

    datiStazione
  
  })->finale
  
  
  #la Tmin contiene date duplicate!!!!
  finale[!duplicated(finale[,c("yy","mm","dd")]),]->finale
  
  left_join(calendario,finale,by=c("yy"="yy","mm"="mm","dd"="dd"))->finale
  
  if(ii==1){
    return(finale)
  }else{
    return(finale[,c(4)])
  }

})->dfFinale  

#purrr::reduce(listaOut,.f=left_join,i.init=calendario)->dfFinale

dfFinale %>%
  arrange(yy,mm,dd)->daScrivere

write_delim(daScrivere,glue::glue("{PARAMETRO}_{annoI}_{annoF}_basilicata_centroFunzionale.csv"),delim=",",col_names = TRUE)