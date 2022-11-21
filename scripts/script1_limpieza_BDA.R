library(ggplot2)
library(tidyverse)

setwd("~/GitHub/DataZonRiesgos/")
dir <- "~/GitHub/DataZonRiesgos/data/tab/BDACR_2020"

# Se definen los nombres a corregir (deben estar en orden para sustituirlos adecuadamente
# También se añade "\n" para realizar un quiebre de línea

nombrecr <- c("Social","Trabajo","Sin nexo","Familiar","Viaje","Intrahospitalario","Centro \npenitenciario","Viaje","Transmisión \ncomunitaria","Transmisión \ncomunitaria","Transmisión \ncomunitaria","Hogares de \nlarga estancia","Sin nexo", "Sin nexo","Laboral","Familiar","NA") # 
nombres <- c("Sin nexo","Transmisión\ncomunitaria","Social","Familiar","Trabajo","Centro \npenitenciario","Hogares de\nlarga estancia","Viaje","Intrahospitalario")

hosp <- c("NO","SI","Si","No","si","N/D")
hosp_s <- c("No","Si","Si","No","Si","N/D")

clas <- c("AUTOCTONO","IMPORTADO","autoctono","AUTÓCTONO")
clas_s <- c("Autóctono","Importado","Autóctono","Autóctono")

fall <- c("NO","SI","no","No")
fall_s <- c("No","Si","No","No")

data_bsanon <- read.csv2("data/tab/BASE ANONIMIZADA CASOS COVID 2020.csv",encoding = "UTF-8",sep = ",") # se lee la data
for (i in 1:3) { # Se separan los distritos de su código, para después unir con la capa de distrito
  a <- str_split_fixed(data_bsanon[,7+i],":",2) # Se realize un split a la columna 7+i por ":" y se divide en dos
  a <- as.data.frame(a) # Se convierte el resultado a dataframe
  ifelse(exists("b"),assign("b",bind_cols(b,a)),assign("b",a)) # si ya existe el string "b" se asigna b al bind de b y a. 
  # si no existe se asigna "b"  al data frame "a"
}
colnames(b) <- c("provcod","prov","cantcod","cant","discod","dist") # se cambian los nombres de columna  
data_bsanonfix <- bind_cols(b,data_bsanon[,c(-8,-9,-10)]) # se excluyen columnas que no interesan
for (i in seq(1:25)) {# quitan los espacios en blanco en TODAS las columnas 
  data_bsanonfix[,i] <- str_trim(data_bsanonfix[,i],side = "both") # específicamente los espacios al final y al inicio
}
data_bsanonfix[125489,25] <- "NA" # Se asigna NA a un dato específico 
unicr <- unique(data_bsanonfix$TIPO.DE.NEXO) # Se extran los valores unicos de esa columna

for (i in seq(1,length(nombrecr))) { # Busca y sustituye todos los carácteres de unicr por los de nombrecr
  data_bsanonfix$TIPO.DE.NEXO <- str_replace_all(data_bsanonfix$TIPO.DE.NEXO, # columna donde se busca
                                                 unicr[i],nombrecr[i]) # patrón de búsqueda y valor a poner
}
for (i in seq(1,length(hosp))) { # Busca y sustituye todos los carácteres de unicr por los de nombrecr
  data_bsanonfix$HOSPITALIZADO <- str_replace_all(data_bsanonfix$HOSPITALIZADO, # columna donde se busca
                                                 hosp[i],hosp_s[i]) # patrón de búsqueda y valor a poner
}
for (i in seq(1,length(clas))) { # Busca y sustituye todos los carácteres de unicr por los de nombrecr
  data_bsanonfix$CLASIFICACION.FINAL <- str_replace_all(data_bsanonfix$CLASIFICACION.FINAL, # columna donde se busca
                                                  clas[i],clas_s[i]) # patrón de búsqueda y valor a poner
}
for (i in seq(1,length(clas))) { # Busca y sustituye todos los carácteres de unicr por los de nombrecr
  data_bsanonfix$FALLECIDO <- str_replace_all(data_bsanonfix$FALLECIDO, # columna donde se busca
                                                        fall[i],fall_s[i]) # patrón de búsqueda y valor a poner
}

# Se remueven las columnas que no interesan

data_bsanonfix <- data_bsanonfix[,c(-13,-14,-15,-16,-17)] 

if (file.exists(dir)) {
  print("La tabla se encuentra en la carpeta data, carpeta tab. Se llama 'BDACR_2020'")
}else{
  write.csv(data_bsanonfix,"~/GitHub/DataZonRiesgos/data/tab/BDACR_2020.csv")
}
# Base con las enfermedades en formato ancho y otros arreglos que facilitan el análisis
 
