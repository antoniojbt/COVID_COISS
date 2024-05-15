############
# COISS paper 1
# L. Bonifaz
# April 2024
############


############
project_loc <- '/Users/antoniob/Documents/work/science/projects/projects/ongoing/laura_bonifaz/COVID_COISS/p1_2021_intervention/results/'
getwd()
setwd(project_loc)
############


############
# Import libraries
library(data.table)
library(episcout)
library(ggthemes)
library(cowplot)
library(tidyverse)
############


############
# Load a previous R session, data and objects:
infile <- '../data/processed/1_COISS_setup.rdata.gzip'
load(infile, verbose = TRUE)

data_f <- data_f # just to get rid of RStudio warnings
dim(data_f)
str(data_f)
epi_head_and_tail(data_f)
colnames(data_f)


# For saving/naming outputs, should already come with the RData file though:
infile_prefix <- infile_prefix
infile_prefix
# Otherwise manually:
# infile_prefix <- 'COVID19MEXICO2021'
############


############
# Do manually if sub-setting
# Re-run code below as is but with subset of COVID-only rows:
# Use "CLASIFICACION_FINAL" codes 1, 2, 3:
data_f_COVID <- data_f[which(as.integer(data_f$CLASIFICACION_FINAL) < 4), ]
dim(data_f)
dim(data_f_COVID)

table(data_f$CLASIFICACION_FINAL)
table(data_f_COVID$CLASIFICACION_FINAL)

# Remove objects, keep objects names so that code can be re-run as several DBs:
data_f <- data_f_COVID
rm(list = c('data_f_COVID'))

# Check this manually:
file_n <- 'COVID-only_COISS-only'
# infile_prefix <- NULL
infile_prefix <- sprintf('%s_%s', infile_prefix, file_n)
infile_prefix

# Do manually, rename outdir if sub-setting
# Create a folder for results:
dir()
if (!dir.exists(infile_prefix)) {
    dir.create(infile_prefix)
}
dir()
############


############
# New variables

# ###
# Analysis dates, based on epidemic waves:
# T0_start	30 July 2021
# T0_end	02 August 2021
# T1_start	03 September 2021
# T1_end	08 September 2021
# T2_start	19 December 2021
# T2_end	28 February 2022
# Figure 2

T0_start <- '2021-07-30'
T0_end <- '2021-08-02'
T1_start <- '2021-09-03'
T1_end <- '2021-09-08'
T2_start <- '2021-12-19'
T2_end <- '2022-02-28'

data_f$time_cuts <- NULL
length(which(as.Date(data_f$FECHA_INGRESO) >= as.Date(T0_start)))
length(which(data_f$FECHA_INGRESO < T0_start))

summary(data_f$FECHA_INGRESO)
# Looks like for 2021 database, once COVID-only cases are kept, the last date is before the T2 cut-off.

summary(data_f$FECHA_DEF)
summary(data_f$FECHA_ACTUALIZACION)
summary(data_f$FECHA_SINTOMAS)

data_f$time_cuts <- ifelse(data_f$FECHA_INGRESO >= T0_start & data_f$FECHA_INGRESO <= T0_end, 'T0',
                           ifelse(data_f$FECHA_INGRESO >= T1_start & data_f$FECHA_INGRESO <= T1_end, 'T1',
                                  ifelse(data_f$FECHA_INGRESO >= T2_start & data_f$FECHA_INGRESO <= T2_end, 'T2', #NA)))
                                         ifelse(data_f$FECHA_INGRESO < T0_start, 'pre-T0',
                                                ifelse(data_f$FECHA_INGRESO > T0_end & data_f$FECHA_INGRESO < T1_start, 'gap_T0_T1',
                                                       ifelse(data_f$FECHA_INGRESO > T1_end & data_f$FECHA_INGRESO < T2_start, 'gap_T1_T2',
                                                              ifelse(data_f$FECHA_INGRESO > T2_end, 'post-T2',
                                                                     NA
                                                                     )))))))

data_f$time_cuts
data_f$time_cuts <- factor(data_f$time_cuts,
                           levels = c('pre-T0', 'T0',
                                      'gap_T0_T1', 'T1',
                                      'gap_T1_T2', 'T2',
                                      'post-T2'),
                           # labels = levels,
                           ordered = TRUE
                           )
summary(data_f$time_cuts)

# Plot:
i <- 'time_cuts'
file_n <- 'plots_bar'
suffix <- 'pdf'
infile_prefix
outfile <- sprintf(fmt = '%s/%s_%s.%s', infile_prefix, file_n, i, suffix)
outfile
plot_list <- list()
plot_1 <- epi_plot_bar(data_f, var_x = 'time_cuts')
plot_list[[i]] <- plot_1
my_plot_grid <- epi_plots_to_grid(plot_list = plot_list)
epi_plot_cow_save(file_name = outfile, plot_grid = my_plot_grid)
###


###
# Intervention variable
# entidad_um "Identifica la entidad donde se ubica la unidad medica que brindó la atención."
dplyr::glimpse(data_f$ENTIDAD_UM)
summary(data_f$ENTIDAD_UM)

# Labels:
interv_labels <- '../data/raw/diccionario_datos_covid19/COISS_states.csv'
interv_labels <- epi_read(interv_labels)
epi_head_and_tail(interv_labels, cols = 4)

data_interv <- data_f %>% left_join(interv_labels[, c(2, 4)],
                                    by = c('ENTIDAD_UM'),
                                    )
summary(as.factor(data_interv$intervention))
data_interv$intervention <- factor(data_interv$intervention,
                                   levels = c('non-COISS', 'COISS', 'other'),
                                   ordered = TRUE
                                   )
summary(data_interv$intervention)
str(data_interv)

# Clean up:
data_f <- data_interv
rm(list = c('data_interv'))
ls()
###

###
# Plot:
i <- 'intervention'
file_n <- 'plots_bar'
suffix <- 'pdf'
infile_prefix
outfile <- sprintf(fmt = '%s/%s_%s.%s', infile_prefix, file_n, i, suffix)
outfile
plot_list <- list()
plot_1 <- epi_plot_bar(data_f, var_x = 'intervention')
plot_list[[i]] <- plot_1
my_plot_grid <- epi_plots_to_grid(plot_list = plot_list)
epi_plot_cow_save(file_name = outfile, plot_grid = my_plot_grid)
###
############


############
# Exclude 'others' (CDMX, EdoMex)
epi_head_and_tail(data_f)
colnames(data_f)
summary(data_f$intervention)

data_f_COISS <- data_f[which(!as.character(data_f$intervention) == 'other'), ]
dim(data_f)
dim(data_f_COISS)
summary(data_f$intervention)
summary(data_f$intervention)[1] + summary(data_f$intervention)[2]
summary(data_f_COISS$intervention)

# Remove objects, keep objects names so that code can be re-run as several DBs:
data_f <- data_f_COISS
rm(list = c('data_f_COISS'))
############



############
# The end:
# Save objects, to eg .RData file:
folder <- '../data/processed'
script <- '2_df_subset'
infile_prefix
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
