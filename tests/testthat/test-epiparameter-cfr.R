# load a single Ebola onset-to-death delay distribution from {epiparameter}
# database
ebola_onset_to_death <- epiparameter::epiparameter_db(
  disease = "Ebola",
  epi_name = "onset-to-death",
  single_epiparameter = TRUE
)

# check the Ebola onset-to-death selected is Gamma, this may change in future
# versions of {epiparameter} with new entries in the database. The tests will
# have to be updated if the distribution selected from the database changes
family(ebola_onset_to_death)

# get parameters from Ebola onset-to-death
dist_params <- epiparameter::get_parameters(ebola_onset_to_death)

# load Ebola dataset from {cfr}
data("ebola1976", package = "cfr")

test_that("cfr_static is works with base and <epiparameter>", {
  # calculate cfr using Ebola onset-to-death parameters with base density
  cfr_base_density <- cfr::cfr_static(
    data = ebola1976,
    delay_density = function(x) {
      dgamma(x, shape = dist_params[["shape"]], scale = dist_params[["scale"]])
    }
  )

  # calculate cfr using Ebola onset-to-death parameters with <epiparameter>
  # density
  cfr_ep_density <- cfr::cfr_static(
    data = ebola1976,
    delay_density = function(x) {
      density(ebola_onset_to_death, at = x)
    }
  )

  # calculate cfr using Ebola onset-to-death parameters with <epiparameter>
  # as.function
  ebola_onset_to_death_func <- as.function(
    ebola_onset_to_death,
    func_type = "density"
  )
  cfr_ep_func_density <- cfr::cfr_static(
    data = ebola1976,
    delay_density = ebola_onset_to_death_func
  )

  # check each density function produces the same cfr
  expect_identical(cfr_base_density, cfr_ep_density)
  expect_identical(cfr_base_density, cfr_ep_func_density)
})
