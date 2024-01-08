# pacotes -------------------------------------------------------------------------------------
library(tidyverse)
library(afex)
library(lsmeans)
library(emmeans)

# lendo a base e ajustando os nomes -----------------------------------------------------------
df <-
  read_delim(
    "dataset.csv",
    delim = ";",
    escape_double = FALSE,
    col_types = cols(`GLP-1` = col_number()),
    trim_ws = TRUE
  )

# colocando os nomes das variaveis em minusculo
df <- janitor::clean_names(df)

# selecionando só tempo BEFORE RYGB --------------------------------------------------------------
before_rygb <-
  df |>
  filter(cond == "BEFORE")

# selecionando só tempo AFTER RYGB --------------------------------------------------------------
after_rygb <-
  df |>
  filter(cond == "AFTER")

# Glicemic ------------------------------------------------------------------------------------
# inf-y ---------------------------------------------------------------------------------------
fit = afex::aov_ez(
  id = "sujeito",
  dv =  "in_fy",
  within = c("tempo"),
  data = before_rygb
)

summary(fit)

# MULTIPLAS COMPARAÇOES -----------------------------------------------------------------------
comp = lsmeans::lsmeans(fit,
                        specs = c("tempo"))
comp

contrast(object = comp,
         method="pairwise")

# ANOVA ---------------------------------------------------------------------------------------
fit = afex::aov_ez(
  id = "sujeito",
  dv =  "in_fy",
  within = c("tempo"),
  data = after_rygb
)

summary(fit)

# MULTIPLAS COMPARAÇOES -----------------------------------------------------------------------
comp = lsmeans::lsmeans(fit,
                        specs = c("tempo"))
comp

contrast(object = comp,
         method="pairwise")

# il_1beta ------------------------------------------------------------------------------------
# ANOVA ---------------------------------------------------------------------------------------
fit = afex::aov_ez(
  id = "sujeito",
  dv =  "il_1beta",
  within = c("tempo"),
  data = before_rygb
)

summary(fit)

# MULTIPLAS COMPARAÇOES -----------------------------------------------------------------------
comp = lsmeans::lsmeans(fit,
                        specs = c("tempo"))
comp

contrast(object = comp,
         method="pairwise")
# ANOVA ---------------------------------------------------------------------------------------
fit = afex::aov_ez(
  id = "sujeito",
  dv =  "il_1beta",
  within = c("tempo"),
  data = after_rygb
)

summary(fit)

# MULTIPLAS COMPARAÇOES -----------------------------------------------------------------------
comp = lsmeans::lsmeans(fit,
                        specs = c("tempo"))
comp

contrast(object = comp,
         method="pairwise")

# il_1ra ------------------------------------------------------------------------------------
# ANOVA ---------------------------------------------------------------------------------------
fit = afex::aov_ez(
  id = "sujeito",
  dv =  "il_1ra",
  within = c("tempo"),
  data = before_rygb
)

summary(fit)

# MULTIPLAS COMPARAÇOES -----------------------------------------------------------------------
comp = lsmeans::lsmeans(fit,
                        specs = c("tempo"))
comp

contrast(object = comp,
         method="pairwise")
# ANOVA ---------------------------------------------------------------------------------------
fit = afex::aov_ez(
  id = "sujeito",
  dv =  "il_1ra",
  within = c("tempo"),
  data = after_rygb
)

summary(fit)

# MULTIPLAS COMPARAÇOES -----------------------------------------------------------------------
comp = lsmeans::lsmeans(fit,
                        specs = c("tempo"))
comp

contrast(object = comp,
         method="pairwise")

# tnf_alfa ------------------------------------------------------------------------------------
# ANOVA ---------------------------------------------------------------------------------------
fit = afex::aov_ez(
  id = "sujeito",
  dv =  "tnf_alfa",
  within = c("tempo"),
  data = before_rygb
)

summary(fit)

# MULTIPLAS COMPARAÇOES -----------------------------------------------------------------------
comp = lsmeans::lsmeans(fit,
                        specs = c("tempo"))
comp

contrast(object = comp,
         method="pairwise")

# ANOVA ---------------------------------------------------------------------------------------
fit = afex::aov_ez(
  id = "sujeito",
  dv =  "tnf_alfa",
  within = c("tempo"),
  data = after_rygb
)

summary(fit)

# MULTIPLAS COMPARAÇOES -----------------------------------------------------------------------
comp = lsmeans::lsmeans(fit,
                        specs = c("tempo"))
comp

contrast(object = comp,
         method="pairwise")

# il_mcp1 ------------------------------------------------------------------------------------
# ANOVA ---------------------------------------------------------------------------------------
fit = afex::aov_ez(
  id = "sujeito",
  dv =  "il_mcp1",
  within = c("tempo"),
  data = before_rygb
)

summary(fit)

# MULTIPLAS COMPARAÇOES -----------------------------------------------------------------------
comp = lsmeans::lsmeans(fit,
                        specs = c("tempo"))
comp

contrast(object = comp,
         method="pairwise")

# ANOVA ---------------------------------------------------------------------------------------
fit = afex::aov_ez(
  id = "sujeito",
  dv =  "il_mcp1",
  within = c("tempo"),
  data = after_rygb
)

summary(fit)

# MULTIPLAS COMPARAÇOES -----------------------------------------------------------------------
comp = lsmeans::lsmeans(fit,
                        specs = c("tempo"))
comp

contrast(object = comp,
         method="pairwise")

# il_6 ------------------------------------------------------------------------------------
# ANOVA ---------------------------------------------------------------------------------------
fit = afex::aov_ez(
  id = "sujeito",
  dv =  "il_6",
  within = c("tempo"),
  data = before_rygb
)

summary(fit)

# MULTIPLAS COMPARAÇOES -----------------------------------------------------------------------
comp = lsmeans::lsmeans(fit,
                        specs = c("tempo"))
comp

contrast(object = comp,
         method="pairwise")

# ANOVA ---------------------------------------------------------------------------------------
fit = afex::aov_ez(
  id = "sujeito",
  dv =  "il_6",
  within = c("tempo"),
  data = after_rygb
)

summary(fit)

# MULTIPLAS COMPARAÇOES -----------------------------------------------------------------------
comp = lsmeans::lsmeans(fit,
                        specs = c("tempo"))
comp

contrast(object = comp,
         method="pairwise")


# il_8 ------------------------------------------------------------------------------------
# ANOVA ---------------------------------------------------------------------------------------
fit = afex::aov_ez(
  id = "sujeito",
  dv =  "il_8",
  within = c("tempo"),
  data = before_rygb
)

summary(fit)

# MULTIPLAS COMPARAÇOES -----------------------------------------------------------------------
comp = lsmeans::lsmeans(fit,
                        specs = c("tempo"))
comp

contrast(object = comp,
         method="pairwise")

# ANOVA ---------------------------------------------------------------------------------------
fit = afex::aov_ez(
  id = "sujeito",
  dv =  "il_8",
  within = c("tempo"),
  data = after_rygb
)

summary(fit)

# MULTIPLAS COMPARAÇOES -----------------------------------------------------------------------
comp = lsmeans::lsmeans(fit,
                        specs = c("tempo"))
comp

contrast(object = comp,
         method="pairwise")

# il_17a ------------------------------------------------------------------------------------
# ANOVA ---------------------------------------------------------------------------------------
fit = afex::aov_ez(
  id = "sujeito",
  dv =  "il_17a",
  within = c("tempo"),
  data = before_rygb
)

summary(fit)

# MULTIPLAS COMPARAÇOES -----------------------------------------------------------------------
comp = lsmeans::lsmeans(fit,
                        specs = c("tempo"))
comp

contrast(object = comp,
         method="pairwise")

# ANOVA ---------------------------------------------------------------------------------------
fit = afex::aov_ez(
  id = "sujeito",
  dv =  "il_17a",
  within = c("tempo"),
  data = after_rygb
)

summary(fit)

# MULTIPLAS COMPARAÇOES -----------------------------------------------------------------------
comp = lsmeans::lsmeans(fit,
                        specs = c("tempo"))
comp

contrast(object = comp,
         method="pairwise")
# il_4 ------------------------------------------------------------------------------------
# ANOVA ---------------------------------------------------------------------------------------
fit = afex::aov_ez(
  id = "sujeito",
  dv =  "il_4",
  within = c("tempo"),
  data = before_rygb
)

summary(fit)

# MULTIPLAS COMPARAÇOES -----------------------------------------------------------------------
comp = lsmeans::lsmeans(fit,
                        specs = c("tempo"))
comp

contrast(object = comp,
         method="pairwise")

# ANOVA ---------------------------------------------------------------------------------------
fit = afex::aov_ez(
  id = "sujeito",
  dv =  "il_4",
  within = c("tempo"),
  data = after_rygb
)

summary(fit)

# MULTIPLAS COMPARAÇOES -----------------------------------------------------------------------
comp = lsmeans::lsmeans(fit,
                        specs = c("tempo"))
comp

contrast(object = comp,
         method="pairwise")
# il_10 ------------------------------------------------------------------------------------
# ANOVA ---------------------------------------------------------------------------------------
fit = afex::aov_ez(
  id = "sujeito",
  dv =  "il_10",
  within = c("tempo"),
  data = before_rygb
)

summary(fit)

# MULTIPLAS COMPARAÇOES -----------------------------------------------------------------------
comp = lsmeans::lsmeans(fit,
                        specs = c("tempo"))
comp

contrast(object = comp,
         method="pairwise")

# ANOVA ---------------------------------------------------------------------------------------
fit = afex::aov_ez(
  id = "sujeito",
  dv =  "il_10",
  within = c("tempo"),
  data = after_rygb
)

summary(fit)

# MULTIPLAS COMPARAÇOES -----------------------------------------------------------------------
comp = lsmeans::lsmeans(fit,
                        specs = c("tempo"))
comp

contrast(object = comp,
         method="pairwise")

