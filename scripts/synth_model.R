library(Synth)
library(tidyselect)
library(tidyverse)


# Reading dataframe generated in "data_treatment.R"

df_join <- read_rds("data/db_ufs2.RDS")

##### Data preparation ####

df_join$ano <- as.numeric(df_join$ano)
df_join <- df_join %>% filter(ano >= 1992 & ano <= 2014)
df_select <- df_join %>% select(id, tcode, ano, desemp, gini, rendad_pc, pop_tot, homic, popurb, freqesc, pop1519m, pop2024m, tsuicid, desp_gov)
df_select <- df_select %>% mutate(tcode = substr(id, 1, 2))


df_select <- df_select %>% mutate(pct_urb = popurb / pop_tot,
                                  pct_jov = (pop1519m + pop2024m) / pop_tot,
                                  log_rpc = log(rendad_pc),
                                  desp_pc = desp_gov / pop_tot) # Gerando as variáveis necessárias


df_select$tcode <- as.numeric(df_select$tcode)

df_select <- as.data.frame(df_select)

df_standardized <- df_select %>% mutate_at(vars(desemp, gini, rendad_pc, homic, freqesc, pct_jov, pct_urb, log_rpc, tsuicid, desp_pc), scale)

df_standardized <- as.data.frame(df_standardized)

#### Synthetic Control ####

# Sem variável de homicídio

dataprep_m1 <- dataprep(foo = df_select,
                         predictors = c("desemp", "gini", "log_rpc", "pct_urb", "pct_jov"),
                         predictors.op = "mean",
                         special.predictors = list(
                           list("desemp", 1992:2006, c("mean")),
                           list("gini", 1992:2006, c("mean")),
                           list("log_rpc", 1992:2006, c("mean")),
                           list("pct_urb", 1992:2006, c("mean")),
                           list("pct_jov", 1992:2006, c("mean"))
                       ),
                         time.predictors.prior = c(1992:2006),
                         dependent = "homic",
                         unit.variable = "tcode",
                         time.variable = "ano",
                         treatment.identifier = 26,
                         controls.identifier = c(11, 12, 13, 14, 15, 16, 17,
                                                 21, 22, 23, 24, 25, 27, 28, 29,
                                                 31, 32, 33, 35,
                                                 41, 42, 43,
                                                 50, 51, 52, 53),
                         time.optimize.ssr = c(1992:2007),
                         time.plot = c(1992:2014)) # data preparation

synth_m1 = dataprep_m1 %>% synth(neg = T, normalize = F) # synth control

synth_m1 %>% path.plot(dataprep.res = dataprep_m1, tr.intake = 2007) # plot


synth_control = dataprep_m1$Y0plot %*% synth_out$solution.w # 


print(synth.tables   <- synth.tab( # weights
  dataprep.res = dataprep_out,
  synth.res    = synth_out)
)



gaps.plot(synth_m1, dataprep_m1) #gaps plot


placebos <- generate.placebos(dataprep_m1, synth_m1, Sigf.ipop = 3) # placebo exercise


# Com variável de homicídio para 2006

dataprep_m2 <- dataprep(foo = df_select,
                        predictors = c("desemp", "gini", "log_rpc", "pct_urb", "pct_jov", "freqesc", "homic", "tsuicid", "desp_pc"),
                        predictors.op = "mean",
                        special.predictors = list(
                          list("desemp", 1992:2006, c("mean")),
                          list("gini", 1992:2006, c("mean")),
                          list("rendad_pc", 1992:2006, c("mean")),
                          list("pct_urb", 1992:2006, c("mean")),
                          list("pct_jov", 1992:2006, c("mean")),
                          list("freqesc", 1992:2006, c("mean")),
                          list("homic", c(2006), c("mean")),
                          list("tsuicid", 1992:2006, c("mean")),
                          list("desp_pc", 1994:2006, c("mean"))
                        ),
                        time.predictors.prior = c(1992:2006),
                        dependent = "homic",
                        unit.variable = "tcode",
                        time.variable = "ano",
                        treatment.identifier = 26,
                        controls.identifier = c(11, 12, 13, 14, 15, 16, 17,
                                                21, 22, 23, 24, 25, 27, 28, 29,
                                                31, 32, 33, 35,
                                                41, 42, 43,
                                                50, 51, 52, 53),
                        time.optimize.ssr = c(1992:2007),
                        time.plot = c(1992:2014))


synth_m2 = dataprep_m2 %>% synth(neg = T, normalize = F) # synth control

synth_m2 %>% path.plot(dataprep.res = dataprep_m2, tr.intake = 2007) # plot


synth_control = dataprep_m2$Y0plot %*% synth_out$solution.w # 


print(synth.tables   <- synth.tab( # weights
  dataprep.res = dataprep_m2,
  synth.res    = synth_m2)
)



gaps.plot(synth_m2, dataprep_m2) #gaps plot


placebos <- generate.placebos(dataprep_m2, synth_m2, Sigf.ipop = 3) # placebo exercise

# Padronizando variáveis

dataprep_m3 <- dataprep(foo = df_standardized,
                        predictors = c("desemp", "gini", "rendad_pc", "pct_urb", "pct_jov", "freqesc", "homic", "tsuicid", "desp_pc"),
                        predictors.op = "mean",
                        special.predictors = list(
                          list("desemp", 1992:2006, c("mean")),
                          list("gini", 1992:2006, c("mean")),
                          list("rendad_pc", 1992:2006, c("mean")),
                          list("pct_urb", 1992:2006, c("mean")),
                          list("pct_jov", 1992:2006, c("mean")),
                          list("freqesc", 1992:2006, c("mean")),
                          list("homic", c(2006), c("mean")),
                          list("tsuicid", 1992:2006, c("mean")),
                          list("desp_pc", 1994:2006, c("mean"))
                        ),
                        time.predictors.prior = c(1992:2006),
                        dependent = "homic",
                        unit.variable = "tcode",
                        time.variable = "ano",
                        treatment.identifier = 26,
                        controls.identifier = c(11, 12, 13, 14, 15, 16, 17,
                                                21, 22, 23, 24, 25, 27, 28, 29,
                                                31, 32, 33, 35,
                                                41, 42, 43,
                                                50, 51, 52, 53),
                        time.optimize.ssr = c(1992:2007),
                        time.plot = c(1992:2014))

synth_m2 = dataprep_m2 %>% synth(neg = T, normalize = F) # synth control

synth_m2 %>% path.plot(dataprep.res = dataprep_m2, tr.intake = 2007) # plot


synth_control = dataprep_m2$Y0plot %*% synth_out$solution.w # 


print(synth.tables   <- synth.tab( # weights
  dataprep.res = dataprep_m2,
  synth.res    = synth_m2)
)



gaps.plot(synth_m2, dataprep_m2) #gaps plot


placebos <- generate.placebos(dataprep_m2, synth_m2, Sigf.ipop = 3) # placebo exercise

#### TWFE ####



# Homicídio média

# Apenas nordeste

