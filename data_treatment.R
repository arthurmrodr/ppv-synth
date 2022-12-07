library(ipeadatar)
library(tidyverse)
library(purrr)
library(dplyr)

# Documentação séries

list_series <- ipeadatar::available_series()

# Baixando séries relevantes

gini <- 
  ipeadata("PGINI") %>% 
  filter(uname == "States")
gini <- gini %>% mutate(ano = substr(date, 1, 4)) %>% mutate(id = paste(tcode, "_", ano, sep = "")) %>% rename(gini = value)

rendad_pc <-
  ipeadata("MRDPC") %>% 
  filter(uname == "States")  
rendad_pc <- rendad_pc %>% mutate(ano = substr(date, 1, 4)) %>% mutate(id = paste(tcode, "_", ano, sep = "")) %>% rename(rendad_pc = value)

desemp <-
  ipeadata("Dese_Pnad") %>% 
  filter(uname == "States")  
desemp <- desemp %>% mutate(ano = substr(date, 1, 4)) %>% mutate(id = paste(tcode, "_", ano, sep = "")) %>% rename(desemp = value)

pop_tot <- ipeadata("ESTIMA_PO") %>% 
  filter(uname == "States")
pop_tot <- pop_tot %>% mutate(ano = substr(date, 1, 4)) %>% mutate(id = paste(tcode, "_", ano, sep = "")) %>% rename(pop_tot = value)

homic <- ipeadata("THOMIC") %>% 
  filter(uname == "States")
homic <- homic %>% mutate(ano = substr(date, 1, 4)) %>% mutate(id = paste(tcode, "_", ano, sep = "")) %>% rename(homic = value)


# Tratando bases de dados

df_join <- reduce(list(desemp, gini, rendad_pc, pop_tot, homic),
                  ~.x %>%
                    left_join(.y, by="id"))


