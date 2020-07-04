# Load packages
devtools::install_github('david-salgado/fastReadfwf')
library(data.table)
library(validate)
library(ggplot2)
library(stringr) 
library(dplyr)

# Change for user's path
path <- "C:\\Users\\X541\\Documents\\TFG_MATES\\SALUD_ADULTO"

# Load data sets
dataFile_adulto <- file.path(path, 'MICRODAT.CA.txt')
schemaFileName <- file.path(path, 'stENSE2017Adulto_Schema.xlsx') 
stSchema_adulto <- fastReadfwf::xlsxToSchema(schemaFileName, sheetname = 'stSchema', lang = 'en')
data_adulto.dt <- fastReadfwf::fread_fwf(dataFile_adulto, stSchema_adulto, outFormat = 'data.table')
data_adult<-setDF(data_adulto.dt)

# Integrity check
#fastReadfwf::validateValues(data_adulto.dt, stSchema_adulto)

#Se deben crear en formato .yaml los controles siguiendo el siguiente esquema para cada regla: 
#rules:
  #-
  #expr:
  #name:
  #label:
  #description:
  
# Load edit rules from yaml file
validate <- validator(.file = file.path(path, 'EditsSalud4SEG2.yaml'))

editNames <- names(validate)
varXedits.matrix <- variables(validate, as = 'matrix')

# Apply rules to data set
cf <- confront(data_adult, validate, ref = data_adulto.dt)

# Flag por unidad y edit
flag.matrix <- values(cf)


npass <- colSums(flag.matrix)
nfail <- dim(flag.matrix)[1] - npass
nNA <- apply(flag.matrix, 1, function(x){sum(is.na(x))})
rel.pass <- npass / dim(flag.matrix)[1]
rel.fail <- nfail / dim(flag.matrix)[1]
rel.NA <- nNA / dim(flag.matrix)[1]

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
  labs(x = 'Edit', y = '', title = 'Tasa de unidades fallidas en cada edit') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(hjust = 0.5))

ggplot(relPass_edit.df, aes(x = editNames, y = rel.NA)) +
  geom_col() +
  labs(x = 'Edit', y = '', title = 'Tasa de unidades desinformadas en cada edit') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(hjust = 0.5))

# La misma informaciÃ³n por unidad estadÃ???stica:
relPass_record.df <- as.data.frame(aggregate(cf, by = 'record'))
(relPass_record.df$IDENTHOGAR <- str_pad(as.character(1:23089), 2, 'left', '0'))

ggplot(relPass_record.df, aes(x = IDENTHOGAR, y = rel.pass)) +
  geom_col() +
  labs(x = 'Unidad', y = '', title = 'Tasa de edits satisfechos por cada unidad') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(hjust = 0.1))

ggplot(relPass_record.df, aes(x = IDENTHOGAR, y = rel.fail)) +
  geom_col() +
  labs(x = 'Unidad', y = '', title = 'Tasa de edits fallidos por cada registro') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(hjust = 0.5))

ggplot(relPass_record.df, aes(x = IIDENTHOGAR, y = rel.NA)) +
  geom_col() +
  labs(x = 'Unidad', y = '', title = 'Tasa de edits desinformados por cada registro') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(hjust = 0.5))

