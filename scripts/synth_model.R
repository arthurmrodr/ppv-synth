library(Synth)
library(caret)
library(tidyselect)
library(plm)
library(tidyverse)


# Reading dataframe generated in "data_treatment.R"

df_join <- read_rds("data/db_ufs.RDS")

# Data preparation

df_join$ano <- as.numeric(df_join$ano)
df_join <- df_join %>% filter(ano >= 1992 & ano <= 2014)
df_select <- df_join %>% select(id, tcode, ano, desemp, gini, rendad_pc, pop_tot, homic, popurb, freqesc, pop1519m, pop2024m)
df_select <- df_select %>% mutate(tcode = substr(id, 1, 2))




df_select <- df_select %>%
  group_by(tcode) %>%
  mutate(homic = ifelse(is.na(homic), (dplyr::lag(homic) + dplyr::lead(homic)) / 2, homic)) # Caso não exista dado de homicídio, considera-se a média do ano anterior e seguinte

df_select <- df_select %>% mutate(pct_urb = popurb / pop_tot,
                                  pct_jov = (pop1519m + pop2024m) / pop_tot,
                                  log_rpc = log(rendad_pc)) # Gerando as variáveis necessárias


df_select$tcode <- as.numeric(df_select$tcode)

df_select <- as.data.frame(df_select)


# Rodando Controle Sintético


dataprep_out <- dataprep(foo = df_select,
                         predictors = c("desemp", "gini", "log_rpc", "pct_urb", "pct_jov", "freqesc", "homic"),
                         predictors.op = "mean",
                         special.predictors = list(
                           list("desemp", 1992:2006, c("mean")),
                           list("gini", 1992:2006, c("mean")),
                           list("log_rpc", 1992:2006, c("mean")),
                           list("pct_urb", 1992:2006, c("mean")),
                           list("pct_jov", 1992:2006, c("mean")),
                           list("freqesc", 1992:2006, c("mean")),
                           list("homic", c(2006), c("mean"))
                         ),
                         time.predictors.prior = c(1992:2006),
                         dependent = "homic",
                         unit.variable = "tcode",
                         time.variable = "ano",
                         treatment.identifier = 26,
                         controls.identifier = c(#11, 12, 13, 14, 15, 16, 17,
                                                 21, 22, 23, 24, 25, 27, 28, 29#,
                                                 #31, 32, 33, 35,
                                                 #41, 42, 43,
                                                 #50, 51, 52, 53),
                         ),
                         time.optimize.ssr = c(1992:2007),
                         time.plot = c(1992:2014))

synth_out = dataprep_out %>% synth()

synth_out %>% path.plot(dataprep.res = dataprep_out, tr.intake = 2007)


synth_control = dataprep_out$Y0plot %*% synth_out$solution.w


print(synth.tables   <- synth.tab(
  dataprep.res = dataprep_out,
  synth.res    = synth_out)
)



gaps.plot(synth_out, dataprep_out)


placebos <- generate.placebos(dataprep_out, synth_out, Sigf.ipop = 3)



#df_complete <- df_join# %>% complete(tcode, ano = 1992:2014)

#df_complete$homic <- as.numeric(df_complete$homic)
#df_complete <- make.pbalanced(df_complete, balance.type = "fill")

#df_train <- df_complete %>% select(homic, ano, tcode)
#df_train <- df_train %>% pivot_longer(df_train, cols = c(homic), names_to = c("tcode", "ano"), values_to = "homic")

#model <- train(df_complete, method = "randomForest")

#weights <- synth(df_complete, weights = c(12, 27, 16, 13, 29, 23, 53, 32,
#                                          52, 21, 51, 50, 31, 15, 25, 41,
#                                          22, 24, 43, 11, 14, 42, 28,
#                                          17))
#print()