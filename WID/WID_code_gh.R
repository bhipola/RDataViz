library(tidyverse)
library(extrafont)

wid_1p<-readxl::read_excel("top1percent/WID_Data_11042021-193956.xlsx")

head(wid_1p)
wid_1p_gathered<-gather(wid_1p, key="country", value="participation", -c('Percentile','Year'))%>%
  select(2:4)

wid_1p_averages<-wid_1p_gathered%>%
  group_by(Year)%>%
  mutate(participation=mean(participation, na.rm=TRUE),country="OECD Average")%>% 
  select(Year, country,participation)%>%
  distinct()

labels<-wid_1p_total%>%
  filter(country %in% c("USA","Spain","OECD Average"), Year==2019)

#fonttable()

ggplot(wid_1p_averages, mapping=aes(x=Year, y=participation, color=country, label=country))+
  geom_line(size=0.8)+
  geom_line(data=filter(wid_1p_gathered,country %in% c("USA","Spain")),
            size=0.8)+
  ggrepel::geom_label_repel(data=labels, inherit.aes = TRUE)+
  scale_color_brewer(palette = "Set2")+
  theme(legend.position = "none",
        axis.text = element_text(family="Palatino Linotype",size=10, color="gray25"),
        axis.title = element_text(family="Palatino Linotype",size=12, color="gray25"),
        axis.ticks = element_blank(),
        plot.title = element_text(family="Palatino Linotype",size=14, color="gray15",hjust = 0.5),
        plot.subtitle = ggtext::element_markdown(family="Palatino Linotype",size=10, color="gray30", hjust=0.5),
        panel.background = element_blank(),
        panel.grid=element_line(color="gray95"),
        plot.caption=element_text(family="Palatino Linotype",size=8, color="gray50"))+
  labs(x="Year",
       y="Share",
       title="Pre-tax National Income Share Held by Top 1% (1980-2019)",
       subtitle="Pre-tax national includes labor and capital income and does not account for tax or  
       transfer operations. In 2019, USA's top 1% held <span style='color:#8DA0CB;'>**18.7%** </span>of pre-tax national income,  
       Spain's top 1% held <span style='color:#fc8d62;'>**12.2%**</span> while the OECD average was <span style='color:#7acab1;'>**12.7%**</span>.",
       caption="Source: World Inequality Lab Database (WID)")+
  scale_y_continuous(limits = c(0,0.2))
