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
# ANOVA ---------------------------------------------------------------------------------------
fit = afex::aov_ez(
  id = "sujeito",
  dv =  "glicemia",
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
  dv =  "glicemia",
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

# Insulin ------------------------------------------------------------------------------------
# ANOVA ---------------------------------------------------------------------------------------
fit = afex::aov_ez(
  id = "sujeito",
  dv =  "insulina",
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
  dv =  "insulina",
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

# GLP-1 ------------------------------------------------------------------------------------
# ANOVA ---------------------------------------------------------------------------------------
fit = afex::aov_ez(
  id = "sujeito",
  dv =  "glp_1",
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
  dv =  "glp_1",
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

# PYY ------------------------------------------------------------------------------------
# ANOVA ---------------------------------------------------------------------------------------
fit = afex::aov_ez(
  id = "sujeito",
  dv =  "pyy",
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
  dv =  "pyy",
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

# PP ------------------------------------------------------------------------------------
# ANOVA ---------------------------------------------------------------------------------------
fit = afex::aov_ez(
  id = "sujeito",
  dv =  "pp",
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
  dv =  "pp",
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

# GIP ------------------------------------------------------------------------------------
# ANOVA ---------------------------------------------------------------------------------------
fit = afex::aov_ez(
  id = "sujeito",
  dv =  "gip",
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
  dv =  "gip",
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


# Ghrelin ------------------------------------------------------------------------------------
# ANOVA ---------------------------------------------------------------------------------------
fit = afex::aov_ez(
  id = "sujeito",
  dv =  "grelina",
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
  dv =  "grelina",
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
