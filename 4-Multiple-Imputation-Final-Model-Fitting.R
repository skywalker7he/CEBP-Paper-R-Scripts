dat_mi <- dat_da_final %>%
  select(
    Chronic_opiate_use,
    Age,
    Sex,
    Race,
    Cancer_type,
    Dose_delay,
    Dose_reduction,
    Cramping
  )

imp_init <- mice(
  dat_mi,
  maxit = 0,
  printFlag = FALSE
)

predM <- imp_init$predictorMatrix
meth <- imp_init$method

imp <- mice(
  dat_mi,
  m = 20,
  maxit = 50,
  predictorMatrix = predM,
  method = meth,
  seed = 20260904,
  printFlag = FALSE
)

fit_dose_delay <- with(
  imp,
  glm(
    Dose_delay ~ Chronic_opiate_use + Age,
    family = binomial(link = "logit")
  )
)

pool_dose_delay <- pool(fit_dose_delay)

results_dose_delay <- summary(
  pool_dose_delay,
  conf.int = TRUE,
  exponentiate = TRUE
)

results_dose_delay

fit_dose_reduction <- with(
  imp,
  glm(
    Dose_reduction ~ Chronic_opiate_use,
    family = binomial(link = "logit")
  )
)

pool_dose_reduction <- pool(fit_dose_reduction)

results_dose_reduction <- summary(
  pool_dose_reduction,
  conf.int = TRUE,
  exponentiate = TRUE
)

results_dose_reduction

fit_cramping <- with(
  imp,
  glm(
    Cramping ~ Chronic_opiate_use,
    family = binomial(link = "logit")
  )
)

pool_cramping <- pool(fit_cramping)

results_cramping <- summary(
  pool_cramping,
  conf.int = TRUE,
  exponentiate = TRUE
)

results_cramping
