# Do a power analysis by creating fake data and fitting a lot of models.
#
# ARGUMENTS:
# param_table : a data frame with slope, intercept, sigma and n : the parameters of the model.
#
# RETURN VALUE:
# A list with all the power value.
#
power_analysis <- function(param_table) {
  
  # On simule de nouvelles données
  param_table <- param_table %>%
    group_by(slope, intercept, sigma, n) %>%
    expand(reps = 1:n) %>%
    ungroup()
  
  param_table <- param_table %>%
    crossing(sim = 1:100)
  
  param_table <- param_table %>%
    mutate(fumeurs = runif(n(), 0, 100)) %>%
    mutate(cancer = rnorm(n(), intercept + slope * fumeurs, sigma))
  
  # On entraîne plein de modèles sur nos "nouvelles données".
  
  fits <- param_table %>%
    group_by(slope, intercept, sigma, n, sim) %>%
    nest()
  
  # Generate coef
  fits <- fits %>%
    mutate(mod = map(data, ~lm(cancer ~ fumeurs, data = .))) %>%
    mutate(coefs = map(mod, ~tidy(.)))
  
  # Gets slope
  fits <- fits %>%
    unnest(coefs) %>%
    ungroup() %>%
    filter(term == "fumeurs")
  
  # On calcule le power
  pow <- fits %>%
    group_by(n) %>%
    summarise(power = 1 - sum(p.value > 0.05) / n()) %>%
    ungroup()
  
  return(pow)
}
