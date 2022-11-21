#####################################################
# Describir las principales fuentes de contagio en Cartago (trabajo, social, etc.)
data_bsanoncartago <- filter(data_bsanonfix, cant == "Cartago")
cart <- data_bsanoncartago %>% count(TIPO.DE.NEXO,sort = TRUE)
cart$prov <- rep("Cartago",dim(cart)[1])
cart$porc <- ((cart$n)/sum(cart$n))*100
# Describir las principales fuentes de contagio en San José (trabajo, social, etc.)
data_bsanonsanjose <- filter(data_bsanonfix, cant == "San Jose")
sanjo <- data_bsanonsanjose %>% count(TIPO.DE.NEXO,sort = TRUE)
sanjo$prov <- rep("San José",dim(sanjo)[1])
sanjo$porc <- ((sanjo$n)/sum(sanjo$n))*100
#####################################################
# Describir las principales fuentes de contagio en Costa Rica (trabajo, social, etc.)
data_bsanon_cr <- data_bsanonfix
data_bsanon_cr %>% count(TIPO.DE.NEXO,sort = TRUE)
cr <- data_bsanon_cr %>% count(TIPO.DE.NEXO,sort = TRUE)
cr$prov <- rep("Costa Rica",dim(cr)[1])
cr$porc <- ((cr$n)/sum(cr$n))*100
##############################
#z <- bind_rows(sanjo,cart,cr) #Unión de tabla de San Jose y Cartago Y Costa Rica.
z <- bind_rows(cart,cr) #Unión de tabla de Costa Rica y Cartago.
z$porc <- round(z$porc,2)
z <- z[-20,]
unique(z$TIPO.DE.NEXO)
z2 <- z[c(-1,-10,-19),] # Remoción de registros "Sin nexo"

rem <- c("a","b","data_bsanon")
rm(list = rem)

