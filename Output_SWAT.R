#***************************************************************************************************#
#                      ANALISIS DE DATOS DE SALIDA DEL MODELO HIDROLOGICO SWAT    
#                                (Soil & Water Assessment Tool)             

#***************************************************************************************************#

# Configurar carpeta de trabajo 
setwd("D:/Qswat_cachi/Jonatan_tesis/Scenarios/Simulación_R") 
rm(list=ls())
system("SWAT_64rel.exe")

library(pacman)# install paquetes
p_load(data.table, ggplot2, reshape2, dplyr)

# Crear tabla para datos de subcuenca:TxtInOut
datos <- read.table("output (2075-2099).sub", skip =9, header = FALSE) #copiar output.sub de scenarios
datos = as.data.table(datos)

# Dividir 11 datos(# nro de subcuencas) de la tabla :TablesOut (formato access) EN QSWAT 
datos1 <- tail(datos,11)

# Dividir los datos de la columna de la tabla y crear una nueva tabla #TablesOut
datos2 <- datos1[, list(V2, V5, V8, V9, V11, V12, V13, V20)]
datos2<- cbind(datos2,SW=c(280.531,336.873,307.485,376.342,531.681, 353.524, 
                           251.447,465.375,
                           437.673,313.228,275.585))

# Sumar colmumnas especificas
#datos2$bwf <- rowSums(datos2[, c (5,6,8)],na.rm = F)
#datos2$gwf <- rowSums(datos2[, c (3,4)],na.rm = F)
datos2$bwf <- apply(datos2[ , c (5,6,8)], 1, sum)# apply(DF, 1(fila), 2(columna), mean)
datos2$gwf <- apply(datos2[ , c (3,9)], 1, sum)

dat3 <- datos2[ ,11]
dat3$a <- apply(datos2[ ,10:11], 1, sum)#fhggfd
dat3$b <- dat3[ , 1]/dat3[ ,2]
datos2$gwc <- cbind(dat3$b)# datos2$cwf = apply (datos2[ , 9:10], 1, function(x) datos2[ , 10]/sum(x))

# Agregar valores de columna
datos3<- cbind(datos2,Surname=c("San Pedro de Cachi","Vinchos","Pongora","Yucaes","Paccha", "Huatatas", 
                                "chillico","Chicllarazo (aguas arriba)",
                                "Apacheta","Huanta","Chicllarazo (aguas abajo)"))
#mean <- apply(datos2[ ,1:11], 2, mean)
# Establecer el nombre de la columna
setnames(datos3, old = c("Surname", "V5","V8","V9","SW","V11", "V12","V13","V20","bwf","gwf","gwc"), 
         new = c("Subcuencas", "PRECIP", "ET", "V9","SW", "SURQ","GW","WYLD","LAT","BWF","GWF","GWC"))

# Eliminar columna
tab <- select(datos3, -V2,-V9,-GWC)

# Cambiar la tabla a una forma más larga desde una forma más ancha
tab1 <- melt(tab, id.vars = "Subcuencas", variable.name = "Parámetros", value.name = "Q_mmaño")

# Guardar
write.csv(tab,"D:/Qswat_cachi/Jonatan_tesis/Scenarios/Simulacion_Python/GWFBWF2075-2099.csv", quote = F)

#library(dplyr)
tab2 <- tab1 %>% 
  mutate_if(is.numeric, round,0)

# Gráfico de barras replanteadas

# Plot 1
ggplot(data = tab2, mapping = aes(x = Subcuencas, y = Q_mmaño, fill = Parámetros)) + 
  geom_bar(stat = "identity") + ggtitle("Comparación de diferentes parámetros a nivel de subcuencas")
#windows()
ggplot(data = tab2, mapping = aes(x = Subcuencas, y = Q_mmaño, fill = Parámetros)) +
  geom_bar(stat = "identity") + ggtitle("Comparación de diferentes parámetros a mivel de subcuencas")+
  labs(title = "",
       x = "Subcuencas",
       y = "mm/año")

# Plot 2
ggplot(data = tab2, mapping = aes(x = Subcuencas, y = Q_mmaño, fill = Parámetros))+
  geom_bar(stat = "identity")+coord_flip()+
  labs(y='(mm/año)')+ theme(axis.text.y=element_text(size = 8)) +
  geom_text(aes(label = paste(Q_mmaño, "")),
            position = position_stack(vjust = 0.5), size=2.6) 
#scale_fill_manual(values=c("red", "blue", "green", "yellow", "gray70"))

ggplot(data = tab2, mapping = aes(x = Subcuencas, y = Q_mmaño, colour = Parámetros, shape = Parámetros)) + 
  geom_point() + geom_line() + geom_smooth(method = lm, se = FALSE, fullrange = TRUE) 

#scatterplot
ggplot(data = tab2, mapping = aes(x = Subcuencas, y = Q_mmaño, colour = Parámetros, shape = Parámetros)) + 
  geom_point() + geom_line() + geom_smooth(method = lm, se = FALSE, fullrange = TRUE) 

#boxplot
ggplot(data = tab2, mapping = aes(x = Parámetros, y = Q_mmaño)) + geom_boxplot()

ggplot(data = tab2, mapping = aes(x = Parámetros, y = Q_mmaño)) +
  geom_boxplot(alpha = 0) +
  geom_jitter(alpha = 0.4, color = "red")+
  labs(title = "Comparación de diferentes parámetros anualmente (2075 - 2099)") 


ggplot(data = tab2,
       mapping = aes(x = Subcuencas, y = Q_mmaño, color = Parámetros)) +
  geom_boxplot(fill="gray") +
  facet_wrap(vars(Parámetros)) +
  theme_bw()


#boxplot2
ggplot(data = tab2, mapping = aes(x = Subcuencas, y = Q_mmaño, color = Parámetros)) +
  geom_boxplot(fill="gray") +
  facet_wrap(vars(Parámetros)) +
  labs(title = "Output_SWAT",
       x = "subcuencas",
       y = "mm/año") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "grey20", size = 12, angle = 90, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(colour = "grey20", size = 12),
        strip.text = element_text(face = "italic"),
        text = element_text(size = 16))

main_plot <- 
  ggplot(tab2, aes(group=Parámetros)) + 
  geom_point(aes(x=Subcuencas, y=Q_mmaño, colour=Parámetros), alpha=0.6) + 
  facet_wrap(~Parámetros) +  ## plot each prize category separately 
  theme_bw() +
  labs(y="Age of prize winner", x="Year of award") + 
  geom_smooth(aes(x=Subcuencas, y=Q_mmaño), method = "loess") + ## add a smoothed line
  scale_y_continuous(limits=c(0,1000)) + 
  scale_colour_discrete(breaks=c("Female","Male")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.title = element_blank()) 

main_plot

get_inset <- function(df){
  p <- ggplot(data=df %>% 
                group_by(Parámetros, Subcuencas) %>% 
                slice(1),
              aes(x=Subcuencas, fill=Parámetros)) +
    geom_bar() + 
    scale_x_discrete( drop=FALSE) + 
    scale_fill_manual(values = c("#00BF7D", "#A3A500", "#F8766D","#00B0F6","#E76BF3","#636363","#00B0F6","#E76BF3","#636363")) + 
    guides(fill=FALSE) +
    theme_bw(base_size=9) +  ## makes everything smaller
    theme(panel.background = element_rect(fill="white"),  ## white plot background 
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_text(size=rel(0.7)), ## tiny axis text
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_blank())
  return(p)
}
inset_plot <- get_inset(tab2) 

## Add it as an inset
main_plot +
  annotation_custom(grob=ggplotGrob(inset_plot), 
                    ymin = 500, ymax=1000, xmin='Pongora', xmax='Yucaes')
