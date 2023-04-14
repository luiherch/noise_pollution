# Preprocessing
library(dplyr)
library(ggplot2)
library(tidyr)

stations <- read.csv(file = 'estaciones_utf.csv', header = TRUE, sep = ';', dec = '.', encoding = 'UTF-8')
colnames(stations)[1] ='NMT'
colnames(stations)[2] ='Names'

data_2022 <- read.csv(file = '2022.csv', header = TRUE, sep = ';', dec = ',',stringsAsFactors=F)
data_2021 <- read.csv(file = '2021.csv', header = TRUE, sep = ';', dec = ',')
data_2020 <- read.csv(file = '2020.csv', header = TRUE, sep = ';', dec = ',')

df <- rbind.data.frame(data_2020, data_2021, data_2022)
# Replace commas with '.' for numeric
df <- data.frame(lapply(df, function(x) {gsub(",", ".", x)}))
Sys.setlocale('LC_ALL', 'es_ES.utf8')
df['Fecha'] <- lapply(df, function(x) paste("01", x, sep='-'))
df <- df %>%
  mutate(Fecha=as.Date(Fecha, format = '%d-%b-%y'))

# Coerce chars to numeric
df[, 4:ncol(df)] <- lapply(4:ncol(df), function(x) as.numeric(df[[x]]))
df[, 2] <- lapply(2, function(x) as.numeric(df[[x]]))

df <- left_join(df, stations, by='NMT')

save(df, file = "data_df.RData")
save(stations, file = 'stations.RData')



