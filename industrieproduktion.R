# SETUP

install.packages("tidyverse")
install.packages("scales")
install.packages("lubridate")
install.packages("ggthemes")
install.packages("esquisse")
install.packages("gganimate")
install.packages("gifski")

library("tidyverse")
library("scales")
library("lubridate")
library(ggthemes)
library(gganimate)
library(esquisse)
library("gifski")


# DATA





AT <- filter(data, geo == "AT" & TIME_PERIOD == "2001-01-01")

# GRAPH Vergleich

graf2 <- data %>% 
  filter(geo=="AT" |geo== "DE") %>% 
  ggplot(., aes(x=TIME_PERIOD, y=OBS_VALUE, colour=geo))+
  geom_line(linewidth=2)+
  labs(title = "Industrieproduktionsindex: Österreich vs Deutschland", subtitle = "Datenquelle: EUROSTAT | 2015 = 100 | Saison- und kalenderbereinigte Daten",
     x="Jahr (monatliche Updates)", y="Produktionsindex") +
 scale_color_hue(labels = c("AT - Österreich", "DE - Deutschland"))+
 labs(color = "Staaten:")+
 # transition_reveal(TIME_PERIOD)+
  theme_fivethirtyeight()

print(graf2)

#anim

animate(graf2, duration = 15, fps = 20, width = 1200, height = 627, renderer = gifski_renderer())+
anim_save("Downloads/output21.gif")

animate(graf2,100,fps = 20,duration = 30, width = 950, height = 750, renderer = gifski_renderer())





# GRAPH Facet

data_facet <- filter(data, geo == "BE" | geo == "BG" | geo == "DK" | geo == "DE"
                  | geo == "EE" | geo == "FI" | geo == "FR" | geo == "EL" | geo == "IE"
                  | geo == "IT" | geo == "HR" | geo == "LV" | geo == "LT"
                  | geo == "LU" | geo == "MT" | geo == "NL" | geo == "AT"
                  | geo == "PL" | geo == "PT" | geo == "RO" | geo == "SE"
                  | geo == "SK" | geo == "SI" | geo == "ES" | geo == "CZ"
                  | geo == "HU" | geo == "CY")

data_facet %>% 
  filter(!(geo %in% "UK")) %>%
  
  ggplot(., aes(x=TIME_PERIOD, y=OBS_VALUE, color=geo, fill()))+
  
  geom_line()+
  geom_vline(xintercept = as.numeric(as.Date("2015-01-01")),
             color="red", lwd=0.3, linetype="dashed")+
  theme(legend.text = element_text(colour="gray", size=10, face="bold"))+
  labs(color = "Staaten:")+
  
  scale_color_hue(labels = c("AT - Österreich", "BE - Belgien", "BG - Bulgarien", "CY - Zypern", "CZ - Tschechien", "DE - Deutschland", "DK - Dänemark", "EE - Estland", "EL - Griechenland", "ES - Spanien", "FI - Finnland", "FR - Frankreich", "HR - Kroatien", "HU - Ungarn", "IE - Irland", "IT - Italien", "LT - Litauen", "LU - Luxemburg", "LV - Lettland", "MT - Malta", "NL - Niederlande", "PL - Polen", "PT - Portugal", "RO - Rumänien", "SE - Schweden", "SI - Slowenien", "SK - Slowakei", "UK - Vereinigtes Königreich"))+
  
  
  
  labs(title = "Industrieproduktionsindex nach Land", subtitle = "Datenquelle: EUROSTAT | 2015 = 100 | Saison- und kalenderbereinigte Daten",
       x="Jahr (monatliche Updates)", y="Produktionsindex", alt="dsdasda") +
  
  annotate(geom = "rect", xmin = ymd('2008-01-01'), xmax = ymd('2009-12-01'), ymin = -Inf, ymax = Inf,
           fill = "blue", colour = NA, alpha = 0.2) +
  annotate(geom = "rect", xmin = ymd('2019-11-01'), xmax = ymd('2021-12-01'), ymin = -Inf, ymax = Inf,
           fill = "red", colour = NA, alpha = 0.2) +
  
  
  
  theme_fivethirtyeight()+
  
  facet_wrap(~ geo)
