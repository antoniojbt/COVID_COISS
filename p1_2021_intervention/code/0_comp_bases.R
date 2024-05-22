############
# COISS paper 1
# L. Bonifaz
# April 2024
############


############
# TO DOs:
# Bases de la DGE tienen diferente fecha de inicio
# Base ya procesada tiene fechas que no cubren T2


# Fuentes:
# https://www.gob.mx/salud/documentos/datos-abiertos-152127
# total 538M
# -rw-r--r-- 1 antoniob 132M May  7 15:20 COVID19MEXICO2021.zip
# -rw-r--r-- 1 antoniob  93M May  7 15:20 COVID19MEXICO2022.zip
# -rw-r--r-- 1 antoniob  59M May  7 15:19 COVID19MEXICO2020.zip
# -rw-r--r-- 1 antoniob  18M May  7 15:18 datos_abiertos_covid19_26.12.2023.zip
# -rw-r--r-- 1 antoniob  19M May  7 15:14 datos_abiertos_covid19_07.05.2024.zip

# Rubi:
# ll
# total 486M
# -rw-r--r-- 1 antoniob 361M May  6 13:56 COISS_EXCLUSION_CDMX_EDOMEX.sav
# -rw-r--r-- 1 antoniob  33M Apr 19 09:17 Poblacion_COVID19_RRR.csv
# -rw-r--r-- 1 antoniob  43M Apr 17 22:47 BaseCoiss_23mayo.txt
# -rw-r--r-- 1 antoniob  51M Apr 15 13:08 BaseCoiss_23mayo.sav
############


############
project_loc <- '/Users/antoniob/Documents/work/science/projects/projects/ongoing/laura_bonifaz/COVID_COISS/p1_2021_intervention/results/'
getwd()
setwd(project_loc)
############


############
# Import libraries
library(data.table)
library(haven)
library(episcout)
library(ggthemes)
library(cowplot)
library(tidyverse)
############


############
####
# COISS_EXCLUSION_CDMX_EDOMEX <- '../data/processed/Rubi/COISS_EXCLUSION_CDMX_EDOMEX.sav'
# BaseCoiss_23mayo <- '../data/processed/Rubi/BaseCoiss_23mayo.sav'

# COISS_EXCLUSION_CDMX_EDOMEX <- read_sav(COISS_EXCLUSION_CDMX_EDOMEX)
# BaseCoiss_23mayo <- read_sav(BaseCoiss_23mayo)


# epi_head_and_tail(data_rubiro)
# str(data_rubiro)
# dplyr::glimpse(data_rubiro)
# dim(data_rubiro)
# colnames(data_rubiro)
# length(unique(as.character(data_rubiro$id_registro)))
# summary(as.Date(data_rubiro$fecha_ingreso))
# summary(as.Date(data_rubiro$ingreso))
# # Ultima fecha es antes de T2
# summary(as.Date(data_rubiro$fecha_def))
# # Ultima fecha es antes que el cierre de T2
####


####
# Databases DGE:
# COVID19MEXICO2020 <- '../data/raw/datos_abiertos_covid19/COVID19MEXICO2020.zip'
COVID19MEXICO2021 <- '../data/raw/datos_abiertos_covid19/COVID19MEXICO2021.zip'
COVID19MEXICO2022 <- '../data/raw/datos_abiertos_covid19/COVID19MEXICO2022.zip'
# COVID19MEXICO26.12.2023 <- '../data/raw/datos_abiertos_covid19/datos_abiertos_covid19_26.12.2023.zip'
# COVID19MEXICO07.05.2024 <- '../data/raw/datos_abiertos_covid19/datos_abiertos_covid19_07.05.2024.zip'

# COVID19MEXICO2020 <- episcout::epi_read(COVID19MEXICO2020)
COVID19MEXICO2021 <- episcout::epi_read(COVID19MEXICO2021)
COVID19MEXICO2022 <- episcout::epi_read(COVID19MEXICO2022)
# COVID19MEXICO26.12.2023 <- episcout::epi_read(COVID19MEXICO26.12.2023)
# COVID19MEXICO07.05.2024 <- episcout::epi_read(COVID19MEXICO07.05.2024)

data_bases_dge <- list(#COVID19MEXICO2020,
                       COVID19MEXICO2021,
                       COVID19MEXICO2022 #,
                       #COVID19MEXICO26.12.2023,
                       #COVID19MEXICO07.05.2024
                       )

lapply(data_bases_dge, dim)
lapply(data_bases_dge, str)
lapply(data_bases_dge, epi_head_and_tail)

# count_IDs <- function(vec) {length(unique(vec))}
# lapply(data_bases_dge, count_IDs)
# # cat 23_04_2024.csv | cut -d ',' -f 2 | sort | uniq | wc -l
# # 1296718
# # (dsci) antoniob@antonios-MacBook-Pro: ~/Documents/work/science/projects/projects/ongoing/laura_bonifaz/estatregia_COVID_COISS/data/raw/datos_abiertos_covid19 ->
# #     wc -l 23_04_2024.csv
# # 1296718 23_04_2024.csv
# 
# fechas_ingreso <- list(as.Date(COVID19MEXICO2020$FECHA_INGRESO),
#                        as.Date(COVID19MEXICO2021$FECHA_INGRESO),
#                        as.Date(COVID19MEXICO2022$FECHA_INGRESO),
#                        as.Date(COVID19MEXICO26.12.2023$FECHA_INGRESO),
#                        as.Date(COVID19MEXICO07.05.2024$FECHA_INGRESO)
#                        )
# lapply(fechas_ingreso, summary)
# 
# 
# fechas_def <- list(as.Date(COVID19MEXICO2020$FECHA_DEF),
#                        as.Date(COVID19MEXICO2021$FECHA_DEF),
#                        as.Date(COVID19MEXICO2022$FECHA_DEF),
#                        as.Date(COVID19MEXICO26.12.2023$FECHA_DEF),
#                        as.Date(COVID19MEXICO07.05.2024$FECHA_DEF)
#                        )
# char_to_date <- function(vec) {
#     vec <- data.table::as.IDate(vec, format = "%Y-%m-%d")
#     }
# lapply(fechas_def, summary)

####
############


############
# Join databases
identical(as.character(colnames(COVID19MEXICO2021)),
          as.character(colnames(COVID19MEXICO2022))
          )

data_f <- rbind(COVID19MEXICO2021, COVID19MEXICO2022)
dim(data_f)
str(data_f)
############



############
# The end:
# Save objects, to eg .RData file:
folder <- '../data/processed'
script <- '0_comp_bases'
infile_prefix <- 'COVID19MEXICO_2021_2022'
suffix <- 'rdata.gzip'
outfile <- sprintf(fmt = '%s/%s_%s.%s', folder, script, infile_prefix, suffix)
outfile

# Check and remove objects that are not necessary to save:
object_sizes <- sapply(ls(), function(x) object.size(get(x)))
object_sizes <- as.matrix(rev(sort(object_sizes))[1:10])
object_sizes
objects_to_save <- (c('data_f', 'infile_prefix', 'outfile'))

# Save:
save(list = objects_to_save,
     file = outfile,
     compress = 'gzip'
     )

# Remove/clean up session:
all_objects <- ls()
all_objects
rm_list <- which(!all_objects %in% objects_to_save)
all_objects[rm_list]
rm(list = all_objects[rm_list])
ls() # Anything defined after all_objects and objects_to_save will still be here

sessionInfo()
# q()

# Next: run the script for xxx
############
