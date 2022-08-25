#***************************************************************************************************#
#                      ANALISIS DE DATOS DE SALIDA DEL MODELO HIDROLOGICO SWAT    
#                                (Soil & Water Assessment Tool)             

#***************************************************************************************************#

library(pacman)# cargar el paquete
pacman::p_load(ggplot2,dplyr,lubridate,cowplot,ggthemes)

# ubicación de la data
data <- read.csv(url('https://raw.githubusercontent.com/jonatanmoreno777/Output-SWAT-hydrological-model/main/Data/Data_SWAT.csv'))
class(data)

tab1 <- melt(data, id=c("Decadas","Subcuencas"), variable.name = "Parámetros", value.name = "Q_mmaño")

# boxplot
main_plot <- 
  ggplot(tab1, aes(group=Parámetros)) + 
  geom_point(aes(x=Decadas, y=Q_mmaño, colour=Parámetros), alpha=0.6) + 
  facet_wrap(~Parámetros) +  ## trazar cada categoría de parámetros por separado
  theme_bw() +
  labs(y="Promedio anual (mm/año)", x="Décadas") + 
  geom_boxplot(aes(x=Decadas, y=Q_mmaño), method = "loess") + ## add a geom_smooth
  geom_line(aes(x=Decadas, y=Q_mmaño, colour=Parámetros), alpha=0.6)+
  geom_smooth(aes(x=Decadas, y=Q_mmaño), method = "loess")+
  scale_y_continuous(limits=c(0,1000)) + 
  scale_colour_discrete(breaks=c("Female","Male")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.title = element_blank()) 

main_plot

# crear plot
p1 <- ggplot(tab1, aes(width = 0.9, fill=Parámetros, y=Q_mmaño, x=Subcuencas)) +
  geom_bar(position="stack", stat="identity", color="black")+
  geom_hline(yintercept=0, color = 'white', size=2)+
  # Adding y ticks
  #mainplot <- mainplot + scale_y_continuous(breaks = round(seq(-250, max(100), by = 50),1))
  # Adding line to differentiate -ve and +ve y axis
  #mainplot <- mainplot + geom_hline(yintercept=0, color = 'white', size=2)
  theme(axis.text.x = element_text(angle = 90))+
  #theme(text = element_text(size=12))+
  xlab('Subcuencas') + ylab('mm/año')+
  scale_fill_manual(name="Parámetros",
                    values = c("PRECIP" = "#7162fd", 
                               "ET" = "#ffffb3", 
                               "SW" = "#bebada", 
                               "SURQ" = "#fb8072", 
                               "GW" = "#80b1d3", 
                               "WYLD" = "#62d4fd",
                               "LAT" = "#fdb462",
                               "BWF" = "#62f8fd",
                               "GWF" = "#62fd90"),
                    labels=c("PRECIP" = "Precipitación", 
                             "ET" = "Evapotranspiración Real", 
                             "SW" = "Humedad del suelo", 
                             "SURQ" = "Escorrentía superficial", 
                             "GW" = "Flujo de agua subterránea", 
                             "WYLD" = "Rendimiento hídrico",
                             "LAT" = "Flujo lateral",
                             "BWF" = "Flujos de agua azul",
                             "GWF" = "Flujos de agua verde"
                    ),
                    breaks = c("PRECIP", "ET", "SW" , "SURQ" , "GW" , "WYLD" ,"LAT","BWF","GWF"))+
  ggtitle('Output_SWAT')+
  #library(cowplot)
  theme_cowplot(8)+
  #library(ggthemes)
  coord_flip() +theme_few(8)

# vemos la lista de plantillas disponibles
ls("package:ggthemes")[grepl("theme_", ls("package:ggthemes"))]

p1 + coord_flip() +theme_foundation(11)

