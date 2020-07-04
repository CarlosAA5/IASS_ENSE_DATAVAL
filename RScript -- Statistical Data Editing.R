# Load packages
devtools::install_github('david-salgado/fastReadfwf')
library(data.table)
library(validate)
library(ggplot2)
library(stringr)
library(dplyr)

# PARA FEBRERO DE 2018
# Change for user's path
path <- 'C:\\Users\\X541\\Documents\\TFG_MATES\\EMPRESAS\\Duda 1'

# Load data sets
dataHistory <- readRDS(file.path(path, 'dataHistory.rds'))
dataHistory_MM022018.dt <- rbindlist(
  lapply(names(dataHistory), function(fecha){
  
  DT <- dataHistory[[fecha]][
    , mes := fecha]
  return(DT)
})
)

null_c11.dt <- dataHistory_MM022018.dt[, list(hist_c11 = median(c11)), by = 'ID']
null_c121.dt <- dataHistory_MM022018.dt[, list(hist_c121 = median(c121)), by = 'ID']
null_c122.dt <- dataHistory_MM022018.dt[, list(hist_c122 = median(c122)), by = 'ID']

MM022018 <- readRDS(file.path(path, 'dataMM022018.rds'))
MM022018[
  , cond_factor := (factor > quantile(factor, prob = 0.95))][
  , hist_c11 := null_c11.dt$hist_c11][
  , hist_c121 := null_c121.dt$hist_c121][
  , hist_c122 := null_c122.dt$hist_c122]

sd_cn_c10 <- quantile(dataHistory_MM022018.dt[, sd(log(1 + b1)), by = 'ID']$V1, prob = 0.10)
sd_ratio_c10 <- quantile(dataHistory_MM022018.dt[, sd(log(1 + b1 / (c11 + c121 + c122))), by = 'ID']$V1, prob = 0.10)
sd_ex_c10 <- quantile(dataHistory_MM022018.dt[, sd(log(1 + b1)) , by = 'ID']$V1, prob = 0.10)

FL_MM022020.dt <- readRDS(file.path(path, 'dataMM022020_Intervalos.dt'))[
  , b1_anterior := dataHistory_MM022018.dt[mes == 'MM012018', b1]]


#Se deben crear en formato .yaml los controles siguiendo el siguiente esquema para cada regla: 
#rules:
#-
#expr:
#name:
#label:
#description:

# Load edit rules from yaml file
validator <- validator(.file = file.path(path, 'EditsIASS_corr.yaml'))


editNames <- names(validator)
varXedits.matrix <- variables(validator, as = 'matrix')

# Apply rules to data set
cf <- confront(MM022018, validator, ref = FL_MM022020.dt)

# Flag por unidad y edit
flag.matrix <- values(cf)


npass <- colSums(flag.matrix)
nfail <- dim(flag.matrix)[1] - npass
nNA<-apply(flag.matrix, 1, function(x){sum(is.na(x))})
rel.pass <- npass / dim(flag.matrix)[1]
rel.fail <- nfail / dim(flag.matrix)[1]
rel.NA <- nNA / dim(flag.matrix)[1]

# Esta información se obtiene directamente con
relPass_edit.df <- as.data.frame(aggregate(cf))
relPass_edit.df$editNames <- row.names(relPass_edit.df)

ggplot(relPass_edit.df, aes(x = editNames, y = rel.pass)) +
  geom_col() +
  labs(x = 'Edit', y = '', title = 'Tasa de unidades que satisface cada edit') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(hjust = 0.5))

ggplot(relPass_edit.df, aes(x = editNames, y = rel.fail)) +
  geom_col() +
  labs(x = 'Edit', y = '', title = 'Tasa de unidades fallidas de cada edit') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(hjust = 0.5))

ggplot(relPass_edit.df, aes(x = editNames, y = rel.NA)) +
  geom_col() +
  labs(x = 'Edit', y = '', title = 'Tasa de unidades desinformadas de cada edit') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(hjust = 0.5))

# La misma información por unidad estad�???stica:
relPass_record.df <- as.data.frame(aggregate(cf, by = 'record'))
(relPass_record.df$ID <- str_pad(as.character(1:50), 2, 'left', '0'))

ggplot(relPass_record.df, aes(x = ID, y = rel.pass)) +
  geom_col() +
  labs(x = 'Unidad', y = '', title = 'Tasa de edits satisfechos por cada unidad') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(hjust = 0.5))

ggplot(relPass_record.df, aes(x = ID, y = rel.fail)) +
  geom_col() +
  labs(x = 'Unidad', y = '', title = 'Tasa de edits fallidos por cada registro') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(hjust = 0.5))

ggplot(relPass_record.df, aes(x = ID, y = rel.NA)) +
  geom_col() +
  labs(x = 'Unidad', y = '', title = 'Tasa de edits desinformados por cada registro') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(hjust = 0.5))

