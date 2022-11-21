
##################################### TIPOS DE NEXO CARTAGO Y COSTA RICA

p <- ggplot(z, aes(TIPO.DE.NEXO, porc, fill = prov)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_brewer(palette = "Set1") +
  ylab("Porcentaje del total") +
  xlab("Tipo de nexo") +
  geom_text(data = z, aes(x = TIPO.DE.NEXO, group=prov, y = porc + 2.5, 
                          label = format(porc, nsmall = 0, digits=1, scientific = FALSE)), 
            color="black", position=position_dodge(.9), hjust=.5) +
  
  theme_classic(base_size = 17)

p + labs(title = "Tipos de nexo en el año 2020", fill = "Unidad",caption = paste0("Total Cartago:     ",
                                                                                  as.character(sum(cart$n)),
                                                                                  "\n                  ",
                                                                                  "Total Costa Rica: ",
                                                                                  as.character(sum(cr$n))))

rem <- c("data_bsanonsanjose","data_bsanon_cr","cr","z","z2")
rm(list = rem)

#####################################
# PARA EL CONGLOMERADO TC
#####################################

df_TC <- data_bsanonfix %>% filter(CODIGO.DE.ASOCIACIÓN == "TC")

#####################################################
# Describir las principales fuentes de contagio en Cartago (trabajo, social, etc.)
data_bsanoncartago <- filter(df_TC, cant == "Cartago")
cart <- data_bsanoncartago %>% count(TIPO.DE.NEXO,sort = TRUE)
cart$prov <- rep("Cartago",dim(cart)[1])
cart$porc <- ((cart$n)/sum(cart$n))*100
# Describir las principales fuentes de contagio en San José (trabajo, social, etc.)
data_bsanonsanjose <- filter(df_TC, cant == "San Jose")
sanjo <- data_bsanonsanjose %>% count(TIPO.DE.NEXO,sort = TRUE)
sanjo$prov <- rep("San José",dim(sanjo)[1])
sanjo$porc <- ((sanjo$n)/sum(sanjo$n))*100
#####################################################
# Describir las principales fuentes de contagio en Costa Rica (trabajo, social, etc.)
data_bsanon_cr <- df_TC
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

########################



p <- ggplot(z, aes(TIPO.DE.NEXO, porc, fill = prov)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_brewer(palette = "Set2") +
  ylab("Porcentaje del total") +
  xlab("Tipo de nexo") +
  geom_text(data = z, aes(x = TIPO.DE.NEXO, group=prov, y = porc + 2.5, 
                          label = format(porc, nsmall = 0, digits=1, scientific = FALSE)), 
            color="black", position=position_dodge(.9), hjust=.5) +
  
  theme_classic(base_size = 17)

p + labs(title = "Tipos de nexo en el conglomerado TC", fill = "Unidad",caption = paste0("Total Cartago:     ",
                                                                                         as.character(sum(cart$n)),
                                                                                         "\n                  ",
                                                                                         "Total Costa Rica: ",
                                                                                         as.character(sum(cr$n))))

paste0("Considerar que en Costa Rica el  ",)
rem <- c("data_bsanonsanjose","data_bsanon_cr","cr","z","z2")




