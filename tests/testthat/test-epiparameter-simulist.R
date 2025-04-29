ll_colnames <- c(
  "id", "case_name", "case_type", "sex", "age", "date_onset", "date_reporting",
  "date_admission", "outcome", "date_outcome", "date_first_contact",
  "date_last_contact", "ct_value"
)
ll_col_class <-  c(
  "integer", "character", "character", "character", "integer", "Date", "Date",
  "Date", "character", "Date", "Date", "Date", "numeric"
)
contacts_colnames <- c(
  "from", "to", "age", "sex", "date_first_contact", "date_last_contact",
  "was_case", "status"
)
contacts_col_class <- c(
  "character", "character", "integer", "character", "Date", "Date", "character",
  "character"
)

# seed for checking dimensions of simulated data
set.seed(1)

test_that("sim_linelist works with <epiparameter> contact distribution", {
  contact_distribution <- epiparameter::epiparameter(
    disease = "COVID-19",
    pathogen = "SARS-CoV-2",
    epi_name = "contact distribution",
    prob_distribution = create_prob_distribution(
      prob_distribution = "pois",
      prob_distribution_params = c(lambda = 2)
    )
  )

  expect_no_condition(
    ll <- simulist::sim_linelist(
      contact_distribution = contact_distribution
    )
  )

  # not using snapshot regression testing until {simulist} v1.0.0
  # due to output breaking changes
  expect_s3_class(ll, class = "data.frame")
  expect_identical(dim(ll), c(158L, 13L))
  expect_identical(colnames(ll), ll_colnames)
  expect_identical(
    vapply(ll, FUN = class, FUN.VALUE = character(1), USE.NAMES = FALSE),
    ll_col_class
  )
})

test_that("sim_contacts works with <epiparameter> contact distribution", {
  contact_distribution <- epiparameter::epiparameter(
    disease = "COVID-19",
    pathogen = "SARS-CoV-2",
    epi_name = "contact distribution",
    prob_distribution = create_prob_distribution(
      prob_distribution = "pois",
      prob_distribution_params = c(lambda = 2)
    )
  )

  expect_no_condition(
    contacts <- simulist::sim_contacts(
      contact_distribution = contact_distribution
    )
  )

  # not using snapshot regression testing until {simulist} v1.0.0
  # due to output breaking changes
  expect_s3_class(contacts, class = "data.frame")
  expect_identical(dim(contacts), c(158L, 8L))
  expect_identical(colnames(contacts), contacts_colnames)
  expect_identical(
    vapply(contacts, FUN = class, FUN.VALUE = character(1), USE.NAMES = FALSE),
    contacts_col_class
  )
})

test_that("sim_linelist works with <epiparameter> onset-to-hospitalisation", {
  onset_to_hosp <- epiparameter::epiparameter(
    disease = "COVID-19",
    pathogen = "SARS-CoV-2",
    epi_name = "onset-to-hospitalisation",
    prob_distribution = create_prob_distribution(
      prob_distribution = "gamma",
      prob_distribution_params = c(shape = 2, scale = 2)
    )
  )

  expect_no_condition(
    ll <- simulist::sim_linelist(
      onset_to_hosp = onset_to_hosp
    )
  )

  # not using snapshot regression testing until {simulist} v1.0.0
  # due to output breaking changes
  expect_s3_class(ll, class = "data.frame")
  expect_identical(dim(ll), c(176L, 13L))
  expect_identical(colnames(ll), ll_colnames)
  expect_identical(
    vapply(ll, FUN = class, FUN.VALUE = character(1), USE.NAMES = FALSE),
    ll_col_class
  )
})

test_that("sim_linelist errors with <epiparameter> onset-to-reporting", {
  reporting_delay <- epiparameter::epiparameter(
    disease = "COVID-19",
    pathogen = "SARS-CoV-2",
    epi_name = "reporting delay",
    prob_distribution = create_prob_distribution(
      prob_distribution = "gamma",
      prob_distribution_params = c(shape = 2, scale = 2)
    )
  )

  # currently ({simulist} v0.5.0) reporting_delay is the only delay
  # distribution argument in sim_*() that does not accept <epiparameter>
  # objects as input
  expect_error(
    simulist::sim_linelist(reporting_delay = reporting_delay),
    regexp = "(Assertion)*(failed: Must be a function, not 'epiparameter')"
  )
})
