library(data.table)
library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(shinydashboard)
library(tidyr)
library(zoo)

if (file.exists('/Users/mross09/Documents/covid/covid-shiny')) {
  setwd('/Users/mross09/Documents/covid/covid-shiny')
} else {
  setwd('C:/Users/marco/Dropbox/covid/covid-shiny')
}

nazione.files <- '../COVID-19/dati-andamento-nazionale/'
regioni.files <- '../COVID-19/dati-regioni/'
province.files <- '../COVID-19/dati-province/'

popolazione <- fread("./data/popolazione.csv")

dati.nazione <- do.call('bind_rows', lapply(list.files(nazione.files, pattern = "dpc-covid19-ita-andamento-nazionale-202.*"), function(x) fread(paste(nazione.files, x, sep = ""))))
dati.nazione <- dati.nazione %>% mutate(data = as.Date(substring(data,1,10)))

dati.regione <- do.call('bind_rows', lapply(list.files(regioni.files, pattern = "dpc-covid19-ita-regioni-202.*"), function(x) fread(paste(regioni.files, x, sep = ""))))
dati.regione <- dati.regione %>% mutate(data = as.Date(substring(data,1,10)))
dati.regione <- merge(group_by(popolazione, denominazione_regione) %>% summarise(popolazione = sum(popolazione)),
                      dati.regione, by = "denominazione_regione")

dati.province <- do.call('bind_rows', lapply(list.files(province.files, pattern = "dpc-covid19-ita-province-202.*"), function(x) fread(paste(province.files, x, sep = ""))))
dati.province <- dati.province %>% mutate(data = as.Date(substring(data,1,10)))
dati.province <- merge(select(popolazione, "denominazione_provincia", "popolazione") %>% unique(),
                       dati.province, by = "denominazione_provincia")
#dati.province[sigla_provincia=="FC"]$denominazione_provincia <- "Forlì-Cesena"

dati.nazione <- dati.nazione %>% rename(totale_casi_prc = totale_positivi_test_molecolare)
dati.nazione <- dati.nazione %>% rename(totale_casi_ag = totale_positivi_test_antigenico_rapido)
dati.regione <- dati.regione %>% rename(totale_casi_prc = totale_positivi_test_molecolare)
dati.regione <- dati.regione %>% rename(totale_casi_ag = totale_positivi_test_antigenico_rapido)

dati.nazione <- dati.nazione %>% rename(tamponi_prc = tamponi_test_molecolare)
dati.nazione <- dati.nazione %>% rename(tamponi_ag = tamponi_test_antigenico_rapido)
dati.regione <- dati.regione %>% rename(tamponi_prc = tamponi_test_molecolare)
dati.regione <- dati.regione %>% rename(tamponi_ag = tamponi_test_antigenico_rapido)

dati.nazione <- dati.nazione %>% mutate(totale_casi_prc = ifelse(is.na(totale_casi_prc), totale_casi, totale_casi_prc))
dati.nazione <- dati.nazione %>% mutate(tamponi_prc = ifelse(is.na(tamponi_prc), tamponi, tamponi_prc))
dati.regione <- dati.regione %>% mutate(totale_casi_prc = ifelse(is.na(totale_casi_prc), totale_casi, totale_casi_prc))
dati.regione <- dati.regione %>% mutate(tamponi_prc = ifelse(is.na(tamponi_prc), tamponi, tamponi_prc))


create.variables <- function(df) {
  df %>%
    arrange(data) %>%
    mutate(deceduti_rapporto = deceduti/totale_casi,
           deceduti_uno_ogni = totale_casi/deceduti,
           totale_casi_rapporto  = totale_casi/tamponi,
           totale_casi_prc_rapporto  = totale_casi_prc/tamponi_prc,
           totale_casi_ag_rapporto  = totale_casi_ag/tamponi_ag,
           ricoverati_con_sintomi_rapporto  = ricoverati_con_sintomi/totale_positivi,
           terapia_intensiva_rapporto  = terapia_intensiva/totale_positivi,
           totale_ospedalizzati_rapporto  = totale_ospedalizzati/totale_positivi,
           isolamento_domiciliare_rapporto  = isolamento_domiciliare/totale_positivi,
           terapia_intensiva_uno_ogni  = totale_positivi/terapia_intensiva,
           gio_totale_casi = totale_casi-lag(totale_casi),
           gio_totale_casi_prc = totale_casi_prc-lag(totale_casi_prc),
           gio_totale_casi_ag = totale_casi_ag-lag(totale_casi_ag),
           gio_tamponi = tamponi-lag(tamponi),
           gio_tamponi_prc = tamponi_prc-lag(tamponi_prc),
           gio_tamponi_ag = tamponi_ag-lag(tamponi_ag),
           gio_totale_ospedalizzati = totale_ospedalizzati-lag(totale_ospedalizzati),
           gio_terapia_intensiva = terapia_intensiva-lag(terapia_intensiva),
           gio_ricoverati_con_sintomi = ricoverati_con_sintomi-lag(ricoverati_con_sintomi),
           gio_deceduti = deceduti-lag(deceduti),
           gio_dimessi_guariti = dimessi_guariti-lag(dimessi_guariti),
           gio_isolamento_domiciliare = isolamento_domiciliare-lag(isolamento_domiciliare),
           gio_totale_positivi = totale_positivi-lag(totale_positivi),
           gio_deceduti_rapporto = gio_deceduti/gio_totale_casi,
           gio_totale_casi_rapporto  = ifelse(gio_totale_casi/gio_tamponi>0,gio_totale_casi/gio_tamponi,NA),
           gio_totale_casi_prc_rapporto  = ifelse(gio_totale_casi_prc/gio_tamponi_prc>0,gio_totale_casi_prc/gio_tamponi_prc,NA),
           gio_totale_casi_ag_rapporto  = ifelse(gio_totale_casi_ag/gio_tamponi_ag>0,gio_totale_casi_ag/gio_tamponi_ag,NA),
           gio_totale_ospedalizzati_rapporto  = gio_totale_ospedalizzati/gio_totale_casi,
           set_totale_casi = rollmean(gio_totale_casi,7,align='right',fill=NA),
           set_totale_casi_prc = rollmean(gio_totale_casi_prc,7,align='right',fill=NA),
           set_totale_casi_ag = rollmean(gio_totale_casi_ag,7,align='right',fill=NA),
           set_tamponi = rollmean(gio_tamponi,7,align='right',fill=NA),
           set_totale_ospedalizzati = rollmean(gio_totale_ospedalizzati,7,align='right',fill=NA),
           set_terapia_intensiva = rollmean(gio_terapia_intensiva,7,align='right',fill=NA),
           set_ricoverati_con_sintomi = rollmean(gio_ricoverati_con_sintomi,7,align='right',fill=NA),
           set_deceduti = rollmean(gio_deceduti,7,align='right',fill=NA),
           set_dimessi_guariti = rollmean(gio_dimessi_guariti,7,align='right',fill=NA),
           set_isolamento_domiciliare = rollmean(gio_isolamento_domiciliare,7,align='right',fill=NA),
           set_totale_positivi = rollmean(gio_totale_positivi,7,align='right',fill=NA),
           set_deceduti_rapporto = rollmean(gio_deceduti_rapporto,7,align='right',fill=NA),
           set_totale_casi_rapporto  = rollmean(gio_totale_casi_rapporto,7,align='right',fill=NA, na.rm = T),
           set_totale_casi_prc_rapporto  = rollmean(gio_totale_casi_prc_rapporto,7,align='right',fill=NA, na.rm = T),
           set_totale_casi_ag_rapporto  = rollmean(gio_totale_casi_ag_rapporto,7,align='right',fill=NA, na.rm = T),
           set_totale_ospedalizzati_rapporto  = set_totale_ospedalizzati/set_totale_casi
    )
}


# enrich dati nazionali
dati.nazione.plus <- create.variables(dati.nazione)


# enrich dati regionali
dati.regione.plus <- create.variables(
  dati.regione %>%
    group_by(denominazione_regione)
) %>%
  ungroup()


# enrich dati provinciali
dati.province.plus <- dati.province %>%
  group_by(denominazione_provincia)  %>%
  arrange(data) %>%
  mutate(
    gio_totale_casi = totale_casi-lag(totale_casi),
    set_totale_casi = rollmean(gio_totale_casi,7,align='right',fill=NA)
  ) %>%
  ungroup() %>%
  mutate(
    uno_ogni = popolazione/totale_casi,
    contagio = totale_casi/popolazione
  )

daily.vars <- sort(names(dati.regione.plus)[grep("gio_",names(dati.regione.plus))])
weekly.vars <- sort(names(dati.regione.plus)[grep("set_",names(dati.regione.plus))])
overall.vars <- sort(c("totale_casi","totale_casi_prc","totale_casi_ag","tamponi","tamponi_prc","tamponi_ag","totale_ospedalizzati","terapia_intensiva","ricoverati_con_sintomi","deceduti","dimessi_guariti","isolamento_domiciliare","totale_positivi","deceduti_rapporto","deceduti_uno_ogni","terapia_intensiva_rapporto","terapia_intensiva_uno_ogni","ricoverati_con_sintomi_rapporto","totale_ospedalizzati_rapporto","isolamento_domiciliare_rapporto","totale_casi_rapporto","totale_casi_prc_rapporto","totale_casi_ag_rapporto"))
