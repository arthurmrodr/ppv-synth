library(ipeadatar)
library(tidyverse)
library(purrr)
library(dplyr)

# Documentação séries

list_series <- ipeadatar::available_series()

##### Baixando séries relevantes

gini <- 
  ipeadata("PGINI") %>% 
  filter(uname == "States") # Gini
gini <- gini %>% mutate(ano = substr(date, 1, 4)) %>% mutate(id = paste(tcode, "_", ano, sep = "")) %>% rename(gini = value)

rendad_pc <-
  ipeadata("MRDPC") %>% 
  filter(uname == "States") # Renda per capita  
rendad_pc <- rendad_pc %>% mutate(ano = substr(date, 1, 4)) %>% mutate(id = paste(tcode, "_", ano, sep = "")) %>% rename(rendad_pc = value)

desemp <-
  ipeadata("Dese_Pnad") %>% 
  filter(uname == "States") # Taxa de desemprego 
desemp <- desemp %>% mutate(ano = substr(date, 1, 4)) %>% mutate(id = paste(tcode, "_", ano, sep = "")) %>% rename(desemp = value)

pop_tot <- ipeadata("ESTIMA_PO") %>% 
  filter(uname == "States") # População total
pop_tot <- pop_tot %>% mutate(ano = substr(date, 1, 4)) %>% mutate(id = paste(tcode, "_", ano, sep = "")) %>% rename(pop_tot = value)

homic <- ipeadata("THOMIC") %>% 
  filter(uname == "States") # Taxa de homicídio
homic <- homic %>% mutate(ano = substr(date, 1, 4)) %>% mutate(id = paste(tcode, "_", ano, sep = "")) %>% rename(homic = value)

pop_urb <- ipeadata("POPUR") %>% 
  filter(uname == "States") # População urbana
pop_urb <- pop_urb %>% mutate(ano = substr(date, 1, 4)) %>% mutate(id = paste(tcode, "_", ano, sep = "")) %>% rename(popurb = value)

freqesc <- ipeadata("SGR1517") %>% 
  filter(uname == "States") # Frequência Escolar (15 a 17 anos)
freqesc <- freqesc %>% mutate(ano = substr(date, 1, 4)) %>% mutate(id = paste(tcode, "_", ano, sep = "")) %>% rename(freqesc = value)

pop1519m <- ipeadata("POP15_19M") %>% 
  filter(uname == "States") # População masculina 15 a 19 anos
pop1519m <- pop1519m %>% mutate(ano = substr(date, 1, 4)) %>% mutate(id = paste(tcode, "_", ano, sep = "")) %>% rename(pop1519m = value)

pop2024m <- ipeadata("POP20_24M") %>% 
  filter(uname == "States") # População masculina 20 a 24 anos
pop2024m <- pop2024m %>% mutate(ano = substr(date, 1, 4)) %>% mutate(id = paste(tcode, "_", ano, sep = "")) %>% rename(pop2024m = value)

# Tratando bases de dados

df_join <- reduce(list(desemp, gini, rendad_pc, pop_tot, homic, pop_urb, freqesc, pop1519m, pop2024m),
                  ~.x %>%
                    full_join(.y, by="id"))

df_join <- df_join %>% mutate(ano = substr(id, 4, 7))

# Criando arquivo RDS

write_rds(df_join, "data/db_ufs.RDS")
