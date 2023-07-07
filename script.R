library(tidyverse)
library(jsonlite)
library(cbsodataR)
library(lubridate)
library(DBI)
library(base64enc)
library(RSQLite)


#Alle identifiers met de term prijs
#toc_filter <- cbs_get_toc() %>%
#  filter(grepl('prijs|Prijs', Title))
#metadata <- cbs_get_meta("83131NED")

data <- cbs_get_data("83131NED") %>%
  select(Bestedingscategorieen, Perioden, CPIAfgeleid_2) %>% #selecteer benodigde variabelen
  rename(cat = Bestedingscategorieen,  CPI = CPIAfgeleid_2) %>%
  filter(
    cat %in% c('CPI011520','CPI072220', 'T001112  '), #filter plantaaridge olie bezine en algemeen
    !grepl('JJ', Perioden)#uitfilteren van jaren in periode
  ) %>% 
  mutate( #namen van categorieen veranderen
    cat = case_when(
      cat == 'CPI011520' ~ 'plant.olie',
      cat == 'CPI072220' ~ 'bezine',
      cat == 'T001112  ' ~ 'algemeen'
    ),
    Perioden = gsub("MM","-",as.character(Perioden))  #notatie tijdsvariabele veranderen
  )

#functie voor kwartaal op kartaal mutaties
func_quart_dif <- function(
    data, 
    y_var = 'CPI', 
    time_var = 'Perioden',
    cat_var = 'cat', 
    date_format = "%Y-%m", 
    time_range = c(1996.1, 2023.2) #parameter om tijdsreek aan te passen
) {
  
  
  
  #Note: Ik ben er vanuit gegaan dat kwartaal op kwartaal betekent de mutatie t.o.v. het voorgaande kwartaal.
  #Ik ben me ervan bewust dat inflatie meestal t.o.v. hetzelfde kwartaal in het voorgaande jaar wordt berekend. 
  #indien het laatste gewenst is kan simpelweg de optie "k=4" aan de lag() functie worden toegevoegd. 
  data %>%
    mutate(
      date = parse_date_time(!!sym(time_var), !!date_format), #variabele omzetten naar datumtype
      kwartaal = quarter(date, type = "year.quarter") #kwartaal variabele maken van datum type
    ) %>%
    group_by(kwartaal, !!sym(cat_var)) %>% #groepeer maanden die tot hetzelfde kwartaal behoren en binnen dezelfde categorie
    summarise(CPI = mean(!!sym(y_var), na.rm = T)) %>% #neem gemiddelde van maanden binnen kwartaal
    group_by(!!sym(cat_var)) %>%
    mutate(
      y_var_lag = lag(!!sym(y_var), order_by = !!sym(cat_var), k = 1), #kwartaal op kwartaal mutatie berekenen.
      mutatie = (!!sym(y_var) - y_var_lag)/y_var_lag
    ) %>%
    filter(
      kwartaal >= time_range[1], #filter op basis van input parameter
      kwartaal <= time_range[2], #filter op basis van input parameter
      !is.na(mutatie)
    ) %>% 
    select(-y_var_lag)
}




#PLOTS
#################################################################################################

#grafieken vanaf 2000

#dataset vanaf 2000
data_full <- func_quart_dif(
  data = data, 
  time_range = c(2000.1, 2023.2)
)

df_plot <- data.frame(id = 1:6, plot = rep(NA, 6)) #leeg dataframe om alle plots in op te slaan


#plot beide categorien sinds 2000
plot_change_full <- data_full %>%
  ggplot(aes(y = mutatie, x = kwartaal, colour = cat)) +
  geom_line(size = 1.2, alpha = 0.8)

ggsave(
  'plot_change_full.pdf',
  plot_change_full ,
  device = cairo_pdf, 
  height = 4*2,
  width = 6*2,
  dpi = 300
) #plot opslaan als PDF

df_plot[1,] <- c(
  'plot_change_full',base64encode(
    serialize(
      plot_change_full , NULL
    )
  )
) #plot coderen en opslaan in dataframe


#plot benzine sinds 2000
plot_change_full_ben <- data_full %>%
  filter(cat != 'plant.olie') %>%
  ggplot(aes(y = mutatie, x = kwartaal, colour = cat)) +
  geom_line(size = 1.2, alpha = 0.8)

ggsave(
  'plot_change_full_ben.pdf',
  plot_change_full_ben,
  device = cairo_pdf,
  height = 4*2, width = 6*2,
  dpi = 300
)

df_plot[2,] <- c(
  'plot_change_full_ben',base64encode(
    serialize(
      plot_change_full_ben , NULL
    )
  )
)

#plot plant.olie sinds 2000
plot_change_full_plant <- data_full %>%
  filter(cat != 'bezine') %>%
  ggplot(aes(y = mutatie, x = kwartaal, colour = cat)) +
  geom_line(size = 1.2, alpha = 0.8)

ggsave(
  'plot_change_full_plant.pdf',
  plot_change_full_plant,
  device = cairo_pdf,
  height = 4*2, width = 6*2,
  dpi = 300
)

df_plot[3,] <- c(
  'plot_change_full_plant',
  base64encode(
    serialize(
      plot_change_full_plant , NULL
    )
  )
)


#plot CPI beide categorien sinds 2000
plot_level_full <- data_full %>%
  ggplot(aes(y = CPI, x = kwartaal, colour = cat)) +
  geom_line(size = 1.2, alpha = 0.8)

ggsave(
  'plot_level_full.pdf',
  plot_level_full,
  device = cairo_pdf,
  height = 4*2,
  width = 6*2,
  dpi = 300
)

df_plot[4,] <- c(
  'plot_level_full',base64encode(
    serialize(
      plot_level_full , NULL
    )
  )
)


#evenstudie oorlog Oekraine

#dataset vanaf 2018
data_2018 <- func_quart_dif(
  data = data, 
  time_range = c(2018.1, 2023.2)
)

#plot beide categorien sinds 2018
plot_change_event <- data_2018 %>%
  ggplot(aes(y = mutatie, x = kwartaal, colour = cat)) +
  geom_line(size = 1.2, alpha = 0.8) +
  geom_vline(xintercept = 2022.1, linetype="dotted", color = "black", size=2) +
  geom_text(aes(x=2022.03, label="begin oorlog Oekraine", y=-0.07), colour="black", angle=90, size = 4) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=1)

ggsave(
  'plot_change_event.pdf',
  plot_change_event,
  device = cairo_pdf,
  height = 4*2,
  width = 6*2,
  dpi = 300
)

df_plot[5,] <- c(
  'plot_change_event',base64encode(
    serialize(
      plot_change_event , NULL
    )
  )
)

#plot CPI beide categorien sinds 2018
plot_level_event <- data_2018 %>%
  ggplot(aes(y = CPI, x = kwartaal, colour = cat)) +
  geom_line(size = 1.2, alpha = 0.8) +
  geom_vline(xintercept = 2022.1, linetype="dotted", color = "black", size=2) +
  geom_text(aes(x=2022.03, label="begin oorlog Oekraine", y=-0.07), colour="black", angle=90, size = 4)

ggsave(
  'plot_level_event.pdf',
  plot_level_event ,
  device = cairo_pdf,
  height = 4*2,
  width = 6*2,
  dpi = 300
)

df_plot[6,] <- c(
  'plot_level_event',base64encode(
    serialize(
      plot_level_event , NULL
    )
  )
)

#opslaan in database
#########################################################################################################

db <- dbConnect(SQLite(), "db.sqlite") #SQLite database openen

dbWriteTable(db, "df_plot", df_plot, overwrite = T) #dataframe met plots opslaan
dbWriteTable(db, "data_full", df_plot, overwrite = T) #dataframe met data opslaan
dbWriteTable(db, "data_2018", df_plot, overwrite = T) #dataframe met data opslaan

retrieve_table <- dbReadTable(db, "df_plot") #terughalen plot tabel
retrieve_plot <- unserialize(base64decode(retrieve_table[1,2])) #oncoderen van plot uit plottabel