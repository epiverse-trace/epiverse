# load a single Ebola serial interval (SI) from {epiparameter} database
ebola_si <- epiparameter_db(
  disease = "Ebola",
  epi_name = "serial interval",
  single_epiparameter = TRUE
)

# check the Ebola SI selected is Gamma, this may change in future versions of
# {epiparameter} with new entries in the database. The tests will have to be
# updated if the distribution selected from the database changes.
family(ebola_si)

# get parameters from Ebola SI
dist_params <- get_parameters(ebola_si)

# load Ebola dataset from {cfr}
data("ebola1976", package = "cfr")

test_that("cfr_static is works with base and <epiparameter>", {
  # calculate cfr using Ebola SI parameters with base density
  cfr_base_density <- cfr_static(
    data = ebola1976,
    delay_density = function(x) {
      dgamma(x, shape = dist_params[["shape"]], scale = dist_params[["scale"]])
    }
  )

  # calculate cfr using Ebola SI parameters with <epiparameter> density
  cfr_ep_density <- cfr_static(
    data = ebola1976,
    delay_density = function(x) {
      density(ebola_si, at = x)
    }
  )

  # calculate cfr using Ebola SI parameters with <epiparameter> as.function
  ebola_si_func <- as.function(ebola_si, func_type = "density")
  cfr_ep_func_density <- cfr_static(
    data = ebola1976,
    delay_density = ebola_si_func
  )

  # check each density function produces the same cfr
  expect_identical(cfr_base_density, cfr_ep_density)
  expect_identical(cfr_base_density, cfr_ep_func_density)
})
