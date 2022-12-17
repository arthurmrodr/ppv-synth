library(Synth)
library(tidyselect)
library(tidyverse)
library(plm)
library(fixest)
library(did)
library(stargazer)
library(SCtools)
library(table1)


# Reading dataframe generated in "data_treatment.R"

df_join <- read_rds("data/db_ufs2.RDS")

##### Data preparation ####

df_join$ano <- as.numeric(df_join$ano)
df_join <- df_join %>% filter(ano >= 1992 & ano <= 2014)

#Base para controle sintético
df_select <- df_join %>% select(id, tcode, ano, desemp, gini, rendad_pc, pop_tot, homic, popurb, freqesc, pop1519m, pop2024m, tsuicid, desp_gov)
df_select <- df_select %>% mutate(tcode = substr(id, 1, 2))


df_select <- df_select %>% mutate(pct_urb = popurb / pop_tot,
                                  pct_jov = (pop1519m + pop2024m) / pop_tot,
                                  log_rpc = log(rendad_pc),
                                  desp_pc = desp_gov / pop_tot) # Gerando as variáveis necessárias


df_select$tcode <- as.numeric(df_select$tcode)

df_select <- as.data.frame(df_select)

df_select <- df_select %>% mutate(tratamento = ifelse(
  (ano >= 2003 & tcode == 31) |
    (ano >= 2007 & tcode == 26) |
    (ano >= 2007 & tcode == 33) |
    (ano >= 2011 & tcode == 32) |
    (ano >= 2011 & tcode == 25),
  1, 0
))

#Base para controle sintético padronizado

df_standardized <- df_select %>% mutate_at(vars(desemp, gini, rendad_pc, homic, freqesc, pct_jov, pct_urb, log_rpc, tsuicid, desp_pc), scale)

df_standardized <- as.data.frame(df_standardized)

#Base para Callaway e Sant'Anna


df_cs <- df_select %>%  group_by(tcode) %>% mutate(anos_pra_tratar = ano - ano[which(tratamento == 1)[1]])
df_cs$anos_pra_tratar <- df_cs$anos_pra_tratar %>% replace_na(0)
df_cs <- df_cs %>% group_by(tcode) %>% mutate(ano_tratamento = ano[which(tratamento == 1)[1]])
df_cs$ano_tratamento <- df_cs$ano_tratamento %>% replace_na(0)

#### Tabela Descritiva ####

table1::label(df_select$desemp) <- "Taxa de Desemprego (%)"
table1::label(df_select$gini) <- "Coeficiente de Gini"
table1::label(df_select$rendad_pc) <- "Renda média domiciliar per capita"
table1::label(df_select$homic) <- "Taxa de Homicídio (por 100.000 hab.)"
table1::label(df_select$freqesc) <- "Frequência Escolar - 15 a 17 anos (%)"
table1::label(df_select$tsuicid) <- "Taxa de Suicídio (por 100.000 hab.)"
table1::label(df_select$desp_pc) <- "Despesa segurança do gov. est. per capita"
table1::label(df_select$pct_urb) <- "População em área urbana (%)"
table1::label(df_select$pct_jov) <- "População masculina entre 15 e 24 anos (%)"


table1(~desemp+gini+rendad_pc+
         homic+freqesc+tsuicid+desp_pc+pct_urb+pct_jov, data = df_select)

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


synth_control1 = dataprep_m1$Y0plot %*% synth_out$solution.w # 


print(synth.tables   <- synth.tab( # weights
  dataprep.res = dataprep_out,
  synth.res    = synth_out)
)



gaps.plot(synth_m1, dataprep_m1) #gaps plot


placebos <- generate.placebos(dataprep_m1, synth_m1, Sigf.ipop = 3) # placebo exercise

plot_placebos(placebos)

mspe.plot(placebos, discard.extreme = TRUE, mspe.limit = 1, plot.hist = TRUE)

# Com variável de homicídio para 2006

dataprep_m2 <- dataprep(foo = df_select,
                        predictors = c("desemp", "gini", "log_rpc", "pct_urb", "pct_jov"),
                        predictors.op = "mean",
                        special.predictors = list(
                          list("desemp", 1992:2006, c("mean")),
                          list("gini", 1992:2006, c("mean")),
                          list("log_rpc", 1992:2006, c("mean")),
                          list("pct_urb", 1992:2006, c("mean")),
                          list("pct_jov", 1992:2006, c("mean")),
                          list("homic", c(2006), c("mean"))
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


synth_control2 = dataprep_m2$Y0plot %*% synth_out$solution.w # 


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

synth_m3 = dataprep_m3 %>% synth(neg = T, normalize = F) # synth control

synth_m3 %>% path.plot(dataprep.res = dataprep_m3, tr.intake = 2007) # plot


synth_control3 = dataprep_m3$Y0plot %*% synth_out$solution.w # 


print(synth.tables   <- synth.tab( # weights
  dataprep.res = dataprep_m3,
  synth.res    = synth_m3)
)



gaps.plot(synth_m3, dataprep_m3) #gaps plot


placebos <- generate.placebos(dataprep_m2, synth_m2, Sigf.ipop = 3) # placebo exercise

#### TWFE ####

df_twfe <- pdata.frame(df_select, index = c("tcode", "ano"))

# Regressão normal só com tratamento

reg_plain <- plm(homic ~ tratamento, data = df_twfe, model = "within", effect = "twoway")
summary(reg_plain)

# Regressão normal com controles

reg_controls <- plm(homic ~ tratamento  + gini + desemp + log_rpc  , data = df_twfe)
summary(reg_controls)
stargazer(reg_controls, type = "latex")
#event study sem controles

twfe_1 <- fixest::feols(homic ~
                    # The time-treatment interaction terms
                    i(anos_pra_tratar, tratamento, ref=-1)
                  # State and year fixed effects
                  | tcode + ano, data=df_twfe)

coefplot(twfe_1)
esttable(twfe_1)
confint(twfe_1)

iplot(twfe_1, 
      xlab = 'Anos para tratamento',
      main = '')

#### Callaway and Sant'Anna ####

cs_homic <- att_gt(yname = "homic",
                     tname = "ano",
                     idname = "tcode",
                     gname = "ano_tratamento",
                     data = df_cs,
                     xformla = ~1,
                     control_group = "notyettreated")

summary(cs_homic)

es_cs <- aggte(cs_homic, type = "dynamic", na.rm = T)

summary(es_cs)

ggdid(es_cs)
