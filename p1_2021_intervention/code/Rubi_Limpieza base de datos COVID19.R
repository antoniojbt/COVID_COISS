library(lubridate)
library(stringr)
library(data.table)
library(dplyr)
library(readr)
library(ggplot2)
library(scales)
library(quantmod)
#install.packages("quantmod")
library(tidyverse)
library(RColorBrewer)
#install.packages("hrbrthemes")
library(hrbrthemes)
library(viridis)
#install.packages("plotly")
library(plotly)
library(tidyverse)
library(htmlwidgets)
library(haven)

#Cargar base de datos
COISS_EXCL <- read_sav("COISS_EXCLUSION_CDMX_EDOMEX.sav")

colnames(COISS_EXCL)

#Seleccionar columnas que nos interesan
fraccion<-COISS_EXCL[,c(5,6,10:13,18:21,23,26:35,41,45)]  

#Seleccionar solo casos positivos a COVID-19 "Clasificación final menor a 4)
Poblacion <- subset(fraccion, clasificacion_final <4)

#Cambiar clase a Entidad que notificó
Poblacion$entidad_um <- as.numeric(Poblacion$entidad_um)
class(Poblacion$entidad_um)

#Cambiar clase a fecha de ingreso
Poblacion$fecha_ingreso<- as.Date(Poblacion$fecha_ingreso)
class(Poblacion$fecha_ingreso)

#Seleccionar solo t0 y t1
Poblacion <- filter(Poblacion, tiempos != 2)

#Cambiar fecha de defunción por vivo o muerto
Poblacion$fecha_def <- as.numeric(as.Date(Poblacion$fecha_def))
Poblacion$fecha_def [which(is.na(Poblacion$fecha_def))] <- "VIVO"
Poblacion$fecha_def [Poblacion$fecha_def != "VIVO"] <- "DEFUNCION"

#Bloque Edad
Poblacion_RRR <- Poblacion %>%
  mutate( GRUPO_EDAD = case_when( edad <  20  ~ "0-19",
                                 edad >=  20  & edad < 45 ~ "20-44",
                                 edad >=  45  & edad < 65 ~ "45-64",
                                 edad >=  65  ~ "65")) 

#Bloque Estrategia
Poblacion_RRR <- Poblacion_RRR %>%
  mutate(  ESTRATEGIA = case_when( entidad_um == 6 ~ "COISS",
                                   entidad_um == 12 ~ "COISS",
                                   entidad_um == 13 ~ "COISS",
                                   entidad_um == 14 ~ "COISS",
                                   entidad_um == 17 ~ "COISS",
                                   entidad_um == 18 ~ "COISS",
                                   entidad_um == 19 ~ "COISS",
                                   entidad_um == 21 ~ "COISS",
                                   entidad_um == 25 ~ "COISS",
                                   entidad_um == 27 ~ "COISS",
                                   entidad_um == 29 ~ "COISS",
                                   entidad_um == 30 ~ "COISS",
                                   entidad_um == 1 ~ "NO COISS",
                                   entidad_um == 2 ~ "NO COISS",
                                   entidad_um == 3 ~ "NO COISS",
                                   entidad_um == 4 ~ "NO COISS",
                                   entidad_um == 5 ~ "NO COISS",
                                   entidad_um == 7 ~ "NO COISS",
                                   entidad_um == 8 ~ "NO COISS",
                                   entidad_um == 10 ~ "NO COISS",
                                   entidad_um == 11 ~ "NO COISS",
                                   entidad_um == 16 ~ "NO COISS",
                                   entidad_um == 20 ~ "NO COISS",
                                   entidad_um == 22 ~ "NO COISS",
                                   entidad_um == 23 ~ "NO COISS",
                                   entidad_um == 24 ~ "NO COISS",
                                   entidad_um == 26 ~ "NO COISS",
                                   entidad_um == 28 ~ "NO COISS",
                                   entidad_um == 31 ~ "NO COISS",
                                   entidad_um == 32 ~ "NO COISS")) 

#Bloque Diabetes
Poblacion_RRR <- Poblacion_RRR %>%
  mutate( DIABETES = case_when( diabetes == 1  ~ "Con",
                                diabetes == 2  ~ "Sin",
                                diabetes == 98 ~ "Sin")) 


#Bloque EPOC
Poblacion_RRR <- Poblacion_RRR %>%
  mutate( EPOC = case_when( epoc == 1  ~ "Con",
                                epoc == 2  ~ "Sin",
                                epoc == 98 ~ "Sin")) 

#Bloque INMUNOSUPRIMIDO
Poblacion_RRR <- Poblacion_RRR %>%
  mutate( INMUNOSUP = case_when( inmusupr == 1  ~ "Con",
                            inmusupr == 2  ~ "Sin",
                            inmusupr == 98 ~ "Sin")) 

#Bloque OTRA COMORBILIDAD
Poblacion_RRR <- Poblacion_RRR %>%
  mutate( OTRA_COM = case_when( otra_com == 1  ~ "Con",
                            otra_com == 2  ~ "Sin",
                            otra_com == 98 ~ "Sin")) 

#Bloque OBESIDAD
Poblacion_RRR <- Poblacion_RRR %>%
  mutate( OBESIDAD = case_when( obesidad == 1  ~ "Con",
                            obesidad == 2  ~ "Sin",
                            obesidad == 98 ~ "Sin")) 

#Bloque HTA
Poblacion_RRR <- Poblacion_RRR %>%
  mutate( HIPERTENSION = case_when( hipertension == 1  ~ "Con",
                            hipertension == 2  ~ "Sin",
                            hipertension == 98 ~ "Sin")) 

#Bloque CARDIOVASCULAR
Poblacion_RRR <- Poblacion_RRR %>%
  mutate( CARDIOVAS = case_when( cardiovascular == 1  ~ "Con",
                            cardiovascular == 2  ~ "Sin",
                            cardiovascular == 98 ~ "Sin")) 

#Bloque CKD
Poblacion_RRR <- Poblacion_RRR %>%
  mutate( INS_RENAL = case_when( renal_cronica == 1  ~ "Con",
                            renal_cronica == 2  ~ "Sin",
                            renal_cronica == 98 ~ "Sin")) 

#Bloque TABAQUISMO
Poblacion_RRR <- Poblacion_RRR %>%
  mutate( TABAQUISMO = case_when( tabaquismo == 1  ~ "Con",
                            tabaquismo == 2  ~ "Sin",
                            tabaquismo == 98 ~ "Sin")) 

#Bloque ASMA
Poblacion_RRR <- Poblacion_RRR %>%
  mutate( ASMA = case_when( asma == 1  ~ "Con",
                            asma == 2  ~ "Sin",
                            asma == 98 ~ "Sin")) 

#Bloque SIN_COMORBILIDAD
Poblacion_RRR <- Poblacion_RRR %>%
  mutate( COMORBILIDADES = case_when( DIABETES == "Sin" & EPOC == "Sin" &
                                          ASMA == "Sin" & INMUNOSUP ==  "Sin" &
                                          HIPERTENSION == "Sin" & OTRA_COM ==  "Sin" &
                                          CARDIOVAS == "Sin" & OBESIDAD == "Sin" &
                                          INS_RENAL == "Sin" & TABAQUISMO == "Sin"
                                        ~ "Sin Comorbilidades")) 

Poblacion_RRR$COMORBILIDADES [which(is.na(Poblacion_RRR$COMORBILIDADES))] <- "Con Comorbilidades"

write_csv(Poblacion_RRR, "Poblacion_COVID19_RRR.csv")
