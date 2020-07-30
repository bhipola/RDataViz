library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(purrr)
library(forcats)
library(RColorBrewer)
library(haven)
library(dummies)
library(ggridges)
library(grid)
library(extrafont)
library(ggthemes)
library(stringr)
library(gridExtra)

cis_abril<-read_sav("C:/Users/belen/OneDrive/Documentos/Cursos y artículos/CIS/ABRIL/3279.sav")  #to download these files visit CIS website (http://www.cis.es/cis/opencm/ES/1_encuestas/estudios/ver.jsp?estudio=14505)

cis_marzo<-read_sav("C:/Users/belen/OneDrive/Documentos/Cursos y artículos/CIS/MARZO/3277.sav")

escideol_decomp<-cis_marzo %>% group_by(ESCIDEOL)%>% summarise(n=n())
escideol_indecisos<- cis_marzo %>% filter(ESCIDEOL %in% c(98,99))
escideol_1_10<-cis_marzo %>% filter(ESCIDEOL %in% 1:10)

# ridgeplot MARZO

sub<-cis_marzo %>% filter(ESCIDEOLPAR_1 %in% 1:10 & ESCIDEOLPAR_2 %in% 1:10 & ESCIDEOLPAR_3 %in% 1:10 & ESCIDEOLPAR_4 %in% 1:10) %>%
  filter(ESCIDEOLPAR_5 %in% 1:10 & ESCIDEOLPAR_6 %in% 1:10 & ESCIDEOLPAR_7 %in% 1:10 & ESCIDEOL %in% 1:10) %>%
  select(ESCIDEOL,ESCIDEOLPAR_1,ESCIDEOLPAR_2,ESCIDEOLPAR_3,ESCIDEOLPAR_4,ESCIDEOLPAR_5,ESCIDEOLPAR_6,ESCIDEOLPAR_7)%>% 
  lapply(as.integer) %>% as.data.frame()
names(sub)<-c('Encuestado', 'PSOE','PP','VOX','Podemos','IU','Ciudadanos','Más País')
dim(sub)
sub_gathered<-gather(sub, key='partido', value='valor', -c(Encuestado))
#loadfonts()
#names(windowsFonts())
sub_gathered_2<-sub_gathered %>% filter(partido != 'Más País' & partido != 'IU')

ridgeplot<-ggplot(sub_gathered_2, aes(x=valor, y=as.factor(Encuestado), fill=stat(x)))+
  geom_density_ridges_gradient(scale=3) +
  scale_fill_viridis_c(direction=-1, name='Escala ideológica') +
  scale_x_continuous(breaks = 1:10)+
  facet_wrap(partido~., ncol=3)+
  labs(x="Percepción de la posición del partido",
       y="Ubicación ideológica del encuestado",
       title="Percepción de la posición del partido según la ideología del encuestado ")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text(size=11),
        legend.position=c(0.85,0.25),
        legend.direction='vertical',
        plot.title = element_text(size=13),
        legend.title = element_text(size=10),
        legend.text=element_blank())

ridgeplot
grid.text("10 - Extrema \nderecha", x = 0.86, y = 0.35,gp=gpar(fontsize=9,col='grey34'))  
grid.text("1 - Extrema \nizquierda", x = 0.86, y = 0.21, gp=gpar(fontsize=9,col='grey34'))  
grid.text("Fuente: Barómetro marzo 2020 (CIS)", x = 0.84, y = 0.015,gp=gpar(fontsize=9,col='grey56', fontface="italic"))  
ridgeplot<-grid.grab()
ridgeplot<-ggplotify::as.ggplot(ridgeplot)


ggsave("C:/Users/belen/OneDrive/Documentos/Cursos y artículos/CIS/ABRIL/ridgeplot_marzo.png",ridgeplot, dpi=350, width=7.34, height =5.67)


###### COVID ABRIL ########

cis_covid<- cis_abril %>% select(ESCIDEOL, INTENCIONG, P3,P7,P12,P14, P16)%>%
  lapply(as.integer) %>% as.data.frame() %>%
  filter(ESCIDEOL %in% 1:10 )%>%
  mutate(P3=ifelse(P3==8,'NS',
                   ifelse(P3==9, 'NC',P3)),
         P7=ifelse(P7==8,'NS',
                   ifelse(P7==9, 'NC',P7)),
         P12=ifelse(P12==8,'NS',
                   ifelse(P12==9, 'NC',P12)),
         P14=ifelse(P14==8,'NS',
                   ifelse(P14==9, 'NC',P14)),
         P16=ifelse(P16==8,'NS',
                   ifelse(P16==9, 'NC',P16))) %>%
  mutate(INTENCIONG=ifelse(INTENCIONG==2, 'PSOE',ifelse(INTENCIONG==1, 'PP', 
                                                        ifelse(INTENCIONG==18,'VOX',
                                                               ifelse(INTENCIONG==3, 'Podemos',
                                                                      ifelse(INTENCIONG==4, 'Ciudadanos',
                                                                             ifelse(INTENCIONG==5, 'IU',
                                                                                    ifelse(INTENCIONG==8, 'ERC',
                                                                                           ifelse(INTENCIONG==98,'NS',
                                                                                                  ifelse(INTENCIONG==97, 'No votaría',
                                                                                                         ifelse(INTENCIONG==99, 'NC','Otro')))))))))))


cis_covid$ESCIDEOL<-factor(x=cis_covid$ESCIDEOL,
                            levels = c('1','2','3','4','5','6','7','8','9','10'))
cis_covid$P3<-factor(x=cis_covid$P3,
                     levels = c('1','2','3','4','5', 'NS','NC'),
                     labels=c('Muy necesarias','Necesarias', 'Regular', 'Poco necesarias','Nada necesarias','NS','NC'))
cis_covid$P7<-factor(x=cis_covid$P7,
                     levels = c('1','2','3','4','5', 'NS','NC'),
                     labels=c('Mucha confianza','Bastante confianza', 'Regular','Poca confianza','Ninguna confianza','NS','NC'))
cis_covid$P12<-factor(x=cis_covid$P12,
                     levels = c('1','2','3','4','5', 'NS','NC'),
                     labels = c('Mucho mejor','Algo mejor','Prácticamente igual','Algo peor','Mucho peor','NS','NC'))
cis_covid$P14<-factor(x=cis_covid$P14,
                     levels = c('1','2','NS','NC'),
                     labels = c('Cree que ahora hay que apoyar al Gobierno y dejar las críticas para otro momento',
                                'Cree que deben continuar haciendo todas las críticas que consideren oportunas',
                                'NS','NC'))
cis_covid$P16<-factor(x=cis_covid$P16,
                     levels = c('1','2','3', 'NS','NC'),
                     labels = c('Es mejor que se intenten grandes acuerdos',
                                'Es mejor que cada partido plantee sus propias alternativas y puntos de vista',
                                'Depende de las circunstancias',
                                'NS',
                                'NC'))


cis_covid %>% group_by(INTENCIONG)%>% summarise(n=n()) %>% mutate(ptg=n*100/sum(n))
cis_covid %>% group_by(ESCIDEOL)%>% summarise(n=n()) %>% mutate(ptg=n*100/sum(n))

cis_p3<-cis_covid %>% group_by(ESCIDEOL,P3) %>% summarise(n=n()) %>% mutate(ptg=n/sum(n))

cis_p7<-cis_covid %>% group_by(ESCIDEOL,P7) %>% summarise(n=n()) %>% mutate(ptg=n/sum(n))

caption<-textGrob('Fuente: Barómetro especial de abril 2020 (CIS)',gp=gpar(fontsize=9, fontface="italic", col='gray43'))

pal<-brewer.pal(9,'YlGnBu')
pal_sub<-c(pal[3:7],'bisque2','lightsalmon3')

p3<-ggplot(cis_p3, aes(x=ESCIDEOL,y=ptg*100, fill=P3))+
  geom_bar(stat='identity')+
  #coord_flip()+
  theme_fivethirtyeight()+
  scale_fill_manual(values=pal_sub)+
  ggtitle('Respecto a las medidas que se han adoptado en España
para combatir el COVID-19, ¿cree Ud. que son muy necesarias,
necesarias, poco necesarias o nada necesarias?')+
  theme(plot.title = element_text(size=14),
        legend.title = element_blank(),
        axis.title = element_text(size=12),
        legend.position='right',
        legend.direction='vertical')+
  labs(y='Porcentaje de encuestados', x='Escala ideológica')+
  coord_cartesian(ylim=c(0,100),xlim=c(0,10),clip = "off")+
  annotation_custom(caption,xmin = 11.7,xmax=11.7, ymax=-19, ymin=-19)


p7<-ggplot(cis_p7, aes(x=ESCIDEOL,y=ptg*100, fill=P7))+
  geom_bar(stat='identity')+
 # coord_flip()+
  theme_fivethirtyeight()+
  scale_fill_manual(values=pal_sub)+
  ggtitle('La política que está siguiendo el Gobierno actual para luchar
contra el COVID-19 en su conjunto, ¿le merece a Ud. mucha
confianza, bastante confianza, poca confianza o ninguna
confianza?')+
  labs(y='Porcentaje de encuestados', x='Escala ideológica')+
  theme(plot.title = element_text(size=14),
        legend.title = element_blank(),
        axis.title = element_text(size=12),
        legend.position='right',
        legend.direction='vertical')+
  coord_cartesian(ylim=c(0,100),xlim=c(0,10),clip = "off")+
  annotation_custom(caption,xmin = 12.2,xmax=12.2, ymax=-19, ymin=-19)


ggsave("C:/Users/belen/OneDrive/Documentos/Cursos y artículos/CIS/ABRIL/p7_4.png",p7, dpi=350, width=6.76, height =5.79 )

ggsave("C:/Users/belen/OneDrive/Documentos/Cursos y artículos/CIS/ABRIL/p3_4.png",p3, dpi=350, width=6.76, height =5.79 )

cis_p7 %>% filter(ESCIDEOL==1)
cis_p7 %>% filter(ESCIDEOL==10)
# 
# ggplot(cis_covid, aes(x=ESCIDEOL, fill=P12))+
#   geom_bar(position = 'fill')+
#   coord_flip()+
#   theme_fivethirtyeight()+
#   ggtitle('Si en estos momentos tuviéramos en España un Gobierno
# presidido por Pablo Casado (PP), ¿cree Ud. que la lucha contra
# el COVID-19 se estaría haciendo mucho mejor, algo mejor,
# prácticamente igual, algo peor o mucho peor que la que se está
# haciendo con el Gobierno presidido por Pedro Sánchez (PSOE)?')+
#   theme(plot.title = element_text(size=14))
# 
# 
# ggplot(cis_covid, aes(x=ESCIDEOL, fill=P14))+
#   geom_bar(position = 'fill')+
#   coord_flip()+
#   theme_fivethirtyeight()+
#   ggtitle('En circunstancias como las actuales, ¿cree Ud. que los
# partidos y líderes de la oposición tienen que colaborar y apoyar
# al Gobierno en todo lo posible, dejando sus críticas o
# discrepancias para otros momentos, o que deben continuar
# criticando y oponiéndose al actual Gobierno en todo lo que
# consideren?')+
#   theme(plot.title = element_text(size=14))
# 
# 
# 
# ggplot(cis_covid, aes(x=ESCIDEOL, fill=P16))+
#   geom_bar(position = 'fill')+
#   coord_flip()+
#   theme_fivethirtyeight()+
#   ggtitle('¿Cree Ud. que cuando termine la crisis del COVID-19
# debería hacerse un esfuerzo especial para intentar llegar a
# grandes acuerdos ante la crisis económica y laboral, o cree que
# lo mejor es que cada partido plantee sus propias alternativas y
# puntos de vista?')+
#   theme(plot.title = element_text(size=14))
# 

#-----------------------------------------------------------------------------------------#
### -------------------------------------INGLÉS------------------------------------------ ###
#-----------------------------------------------------------------------------------------#

cis_abril<-read_sav("C:/Users/belen/OneDrive/Documentos/Cursos y artículos/CIS/ABRIL/3279.sav")

cis_marzo<-read_sav("C:/Users/belen/OneDrive/Documentos/Cursos y artículos/CIS/MARZO/3277.sav")

escideol_decomp<-cis_marzo %>% group_by(ESCIDEOL)%>% summarise(n=n())
escideol_indecisos<- cis_marzo %>% filter(ESCIDEOL %in% c(98,99))
escideol_1_10<-cis_marzo %>% filter(ESCIDEOL %in% 1:10)

# ridgeplot MARZO

sub<-cis_marzo %>% filter(ESCIDEOLPAR_1 %in% 1:10 & ESCIDEOLPAR_2 %in% 1:10 & ESCIDEOLPAR_3 %in% 1:10 & ESCIDEOLPAR_4 %in% 1:10) %>%
  filter(ESCIDEOLPAR_5 %in% 1:10 & ESCIDEOLPAR_6 %in% 1:10 & ESCIDEOLPAR_7 %in% 1:10 & ESCIDEOL %in% 1:10) %>%
  select(ESCIDEOL,ESCIDEOLPAR_1,ESCIDEOLPAR_2,ESCIDEOLPAR_3,ESCIDEOLPAR_4,ESCIDEOLPAR_5,ESCIDEOLPAR_6,ESCIDEOLPAR_7)%>% 
  lapply(as.integer) %>% as.data.frame()
names(sub)<-c('Encuestado', 'PSOE','PP','VOX','Podemos','IU','Ciudadanos','Más País')
dim(sub)
sub_gathered<-gather(sub, key='partido', value='valor', -c(Encuestado))
#loadfonts()
#names(windowsFonts())
sub_gathered_2<-sub_gathered %>% filter(partido != 'Más País' & partido != 'IU')

ridgeplot_eng<-ggplot(sub_gathered_2, aes(x=valor, y=as.factor(Encuestado), fill=stat(x)))+
  geom_density_ridges_gradient(scale=3) +
  scale_fill_viridis_c(direction=-1, name='Ideological scale') +
  scale_x_continuous(breaks = 1:10)+
  facet_wrap(partido~., ncol=3)+
  labs(x="Party's placement perception",
       y="Respondent's ideological self-placement ",
       title="Perception of the party's placement according to respondent's ideology")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text(size=11),
        legend.position=c(0.85,0.25),
        legend.direction='vertical',
        plot.title = element_text(size=13),
        legend.title = element_text(size=10),
        legend.text=element_blank())

ridgeplot_eng
grid.text("10 - Extreme \nright", x = 0.86, y = 0.35,gp=gpar(fontsize=9,col='grey34'))  
grid.text("1 - Extreme \nleft", x = 0.86, y = 0.21, gp=gpar(fontsize=9,col='grey34'))  
grid.text("Source: 'Barómetro marzo 2020' (CIS)", x = 0.84, y = 0.015,gp=gpar(fontsize=9,col='grey56', fontface="italic"))  
ridgeplot<-grid.grab()
ridgeplot_eng<-ggplotify::as.ggplot(ridgeplot)


ggsave("C:/Users/belen/OneDrive/Documentos/Cursos y artículos/CIS/ABRIL/ridgeplot_marzo_eng.png",ridgeplot_eng, dpi=350, width=7.34, height =5.67)


###### COVID ABRIL ########

cis_covid<- cis_abril %>% select(ESCIDEOL, INTENCIONG, P3,P7,P12,P14, P16)%>%
  lapply(as.integer) %>% as.data.frame() %>%
  filter(ESCIDEOL %in% 1:10 )%>%
  mutate(P3=ifelse(P3==8,'NS',
                   ifelse(P3==9, 'NC',P3)),
         P7=ifelse(P7==8,'NS',
                   ifelse(P7==9, 'NC',P7)),
         P12=ifelse(P12==8,'NS',
                    ifelse(P12==9, 'NC',P12)),
         P14=ifelse(P14==8,'NS',
                    ifelse(P14==9, 'NC',P14)),
         P16=ifelse(P16==8,'NS',
                    ifelse(P16==9, 'NC',P16))) %>%
  mutate(INTENCIONG=ifelse(INTENCIONG==2, 'PSOE',ifelse(INTENCIONG==1, 'PP', 
                                                        ifelse(INTENCIONG==18,'VOX',
                                                               ifelse(INTENCIONG==3, 'Podemos',
                                                                      ifelse(INTENCIONG==4, 'Ciudadanos',
                                                                             ifelse(INTENCIONG==5, 'IU',
                                                                                    ifelse(INTENCIONG==8, 'ERC',
                                                                                           ifelse(INTENCIONG==98,'NS',
                                                                                                  ifelse(INTENCIONG==97, 'No votaría',
                                                                                                         ifelse(INTENCIONG==99, 'NC','Otro')))))))))))


cis_covid$ESCIDEOL<-factor(x=cis_covid$ESCIDEOL,
                           levels = c('1','2','3','4','5','6','7','8','9','10'))
cis_covid$P3<-factor(x=cis_covid$P3,
                     levels = c('1','2','3','4','5', 'NS','NC'),
                     labels=c('Very necessary','Necessary', 'Average', 'Not very necessary','Not necessary at all','Does not know','Does not answer'))
cis_covid$P7<-factor(x=cis_covid$P7,
                     levels = c('1','2','3','4','5', 'NS','NC'),
                     labels=c('Very trustworthy','Quite trustworthy', 'Average','Not very trustworthy','Not trustworthy at all','Does not know','Does not answer'))


cis_covid %>% group_by(INTENCIONG)%>% summarise(n=n()) %>% mutate(ptg=n*100/sum(n))
cis_covid %>% group_by(ESCIDEOL)%>% summarise(n=n()) %>% mutate(ptg=n*100/sum(n))

cis_p3<-cis_covid %>% group_by(ESCIDEOL,P3) %>% summarise(n=n()) %>% mutate(ptg=n/sum(n))

cis_p7<-cis_covid %>% group_by(ESCIDEOL,P7) %>% summarise(n=n()) %>% mutate(ptg=n/sum(n))

caption<-textGrob("Source: 'Barómetro especial de abril 2020' (CIS)",gp=gpar(fontsize=9, fontface="italic", col='gray43'))

p3_eng<-ggplot(cis_p3, aes(x=ESCIDEOL,y=ptg*100, fill=P3))+
  geom_bar(stat='identity')+
  #coord_flip()+
  theme_fivethirtyeight()+
  scale_fill_manual(values=pal_sub)+
  ggtitle('Regarding the measures that have been taken in Spain
to fight COVID-19, do you consider they are very necessary,
necessary, not very necessary, or not neccesary at all?')+
  theme(plot.title = element_text(size=14),
        legend.title = element_blank(),
        axis.title = element_text(size=12),
        legend.position='right',
        legend.direction='vertical')+
  labs(y='Percentage of respondents', x='Ideological scale')+
  coord_cartesian(ylim=c(0,100),xlim=c(0,10),clip = "off")+
  annotation_custom(caption,xmin = 11.7,xmax=11.7, ymax=-18.5, ymin=-18.5)


p7_eng<-ggplot(cis_p7, aes(x=ESCIDEOL,y=ptg*100, fill=P7))+
  geom_bar(stat='identity')+
  # coord_flip()+
  theme_fivethirtyeight()+
  scale_fill_manual(values=pal_sub)+
  ggtitle("Does the Government's policy to fight COVID-19 as a whole
seem to you very trustworthy, quite trustworthy, 
not very trustworthy or not trustworthy at all?")+
  labs(y='Percentage of respondents', x='Ideological scale')+
  theme(plot.title = element_text(size=14),
        legend.title = element_blank(),
        axis.title = element_text(size=12),
        legend.position='right',
        legend.direction='vertical')+
  coord_cartesian(ylim=c(0,100),xlim=c(0,10),clip = "off")+
  annotation_custom(caption,xmin = 12.2,xmax=12.2, ymax=-18.5, ymin=-18.5)


ggsave("C:/Users/belen/OneDrive/Documentos/Cursos y artículos/CIS/ABRIL/p7_4_eng.png",p7_eng, dpi=350, width=6.76, height =5.79 )

ggsave("C:/Users/belen/OneDrive/Documentos/Cursos y artículos/CIS/ABRIL/p3_4_eng.png",p3_eng, dpi=350, width=6.76, height =5.79 )

