
library(sf)
library(ggplot2)
library(ggspatial)
library(raster)

Peru  <- getData('GADM', country='Peru', level=1) %>% st_as_sf()

Valores= c (379.4, 
            1083.5,
            405.8,
            1382.7,
            616.2,
            1341.0,
            994.5,
            1205.5,
            347.6,
            721.0,
            850.8,
            1246.0,
            1778.1,
            1197.3,
            9485.4,
            8575.0,
            883.5,
            141.1,
            174.9,
            254.1,
            1856.8,
            1172.7,
            813.4,
            329.3,
            224.9,
            496.2
)
PeruPer= cbind(Peru, Valores)


col=c("#DACBB1", "#D6642D", "#F7BB5A", "#C88864", "#BC782F", 
      "#FABA82", "#BAA599", "#F1995A", "#C99C58", "#BE683F",
      "#BDA074", "#F99944", "#D59533", "#EDB695", "#E5C48E", "#F7823A", "#F1874D", 
      "#D19861", "#B2814E", "#E6A759",
      "#D6842E", "#CD6F31", "#DA875B", "#89D79E", "#949956", 
      "#74A6A5"
)


Peru$NAME_1
A=  ggplot()+
  geom_sf(data = PeruPer, aes(fill=Valores), color= "black", size= 0.1)+
  geom_sf_label(data = Peru, aes(label=Peru$NAME_1), size=1.5)+
  scale_fill_gradientn(colours = col,
                       breaks = c(0,2000,4000,8000,10000,12000,14000),
                       labels = c("[0 - 1999] ","[2000 - 3999]",
                                  "[4000 - 7999]", "[8000 - 9999]", 
                                  "[10000 - 11999]", "[12000 - 13999]",
                                  "[14000 - 15000]"))+
  theme(legend.position = c(0.2, 0.2),
        axis.text.x  = element_text(face="bold", color="black", size=10,
                                    family="serif"),
        axis.text.y  = element_text(angle = 90,face="bold", color="black",
                                    family="serif",size=10),
        axis.title = element_text(face="bold", color="black"),
        legend.background = element_rect(fill = "#f4f3ee"),
        legend.text=element_text(size=9, family="serif"),
        legend.title = element_text(size=9, family="serif"),
        legend.key.size = unit(0.2, "cm"), #alto de cuadrados de referencia
        legend.key.width = unit(0.5,"cm"), #ancho de cuadrados de referencia 
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 0.5))+
  labs(title = '', fill = 'Densidad \n(miles)',  x = 'Longitud', y = 'Latitud')+
  guides(fill = guide_legend(title.position = "top",direction = "vertical"))+
  annotation_north_arrow(location="tl",which_north="true",style=north_arrow_fancy_orienteering ())+
  annotation_scale(location = "br",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")


library(tidyverse)
summ <- PeruPer %>%
  mutate(NAME_1= fct_reorder(NAME_1, Valores, .desc = TRUE))

B = ggplot(data = summ, aes(x=NAME_1, y=Valores, fill=NAME_1))+
  scale_fill_manual(values = col)+
  geom_col()+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        legend.position = c(0.75, 0.55),
        legend.text =element_text(size=8, family="serif"),
        legend.title = element_text(size=8, family="serif"),
        axis.title.x = element_text(color="black", size=11,family="serif",face="bold"),
        axis.title.y = element_text(color="black", size=11,family="serif",face="bold"),
        axis.text.y  = element_text(color="black", size=10, family="serif",face="bold"),
        axis.text.x  = element_text(color="black", size=10, family="serif",face="bold", hjust=1),
        legend.key.size = unit(0.3, "cm"), #alto de cuadrados de referencia
        legend.key.width = unit(0.6,"cm"), #ancho de cuadrados de referencia 
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 11))+
  
  geom_text(aes(label=round(Valores,1), y = 3000), position = position_dodge(0.90), 
            size=2,family="serif",face="bold")+
  labs(x= "Departamentos",
       y= "Densidad Poblacional (miles)",fill = 'Densidad \n(miles)')+
  guides(fill= guide_legend(nrow = 26, ncol=1))+
  coord_flip()


library(cowplot)
Final=ggdraw() +
  coord_equal(xlim = c(0, 29), ylim = c(0, 21), expand = FALSE) +
  draw_plot(A , width = 19, height = 19,x = -3, y = 1)+
  draw_plot(B, width = 16, height = 18,x = 13, y = 1)+
  
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect( color = "black", fill = NA, size = 1))+
  annotate(geom = "text", x = 8, y = 3, hjust = 0, vjust = 1, angle=45,
           label = "Gorky Florez Castillo            Gorky Florez Castillo        Gorky Florez Castillo",
           size = 7, family="serif", color = "grey20",
           alpha=0.2)
Final
ggsave(plot=Final,"Mapa/Mapa de Suelo3.png",units = "cm",width = 29, #alto
       height = 21, #ancho
       dpi=1200)















