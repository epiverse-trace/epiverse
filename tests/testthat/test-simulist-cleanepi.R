set.seed(1)
ll <- simulist::sim_linelist()

test_that("convert_to_numeric corrects prop_int_as_word", {
  # create messy data with 50% of integers converted to words
  messy_ll <- simulist::messy_linelist(
    linelist = ll,
    prop_missing = 0,
    prop_spelling_mistakes = 0,
    inconsistent_sex = FALSE,
    numeric_as_char = FALSE,
    date_as_char = FALSE,
    prop_int_as_word = 0.5,
    prop_duplicate_row = 0
  )

  # convert columns with numbers as words into numbers as numeric
  clean_ll <- cleanepi::convert_to_numeric(
    data = messy_ll,
    target_columns = c("id", "age")
  )

  # the below is not TRUE because
  # 1. `clean_ll` has an attribute used to store the report from the performed
  # cleaning operation
  # 2. the converted "id" and "age" columns are numeric not integer
  expect_false(identical(ll, clean_ll))

  # check whether report is created as expected
  report <- attr(clean_ll, "report")
  expect_identical(names(report), "converted_into_numeric")
  expect_identical(report$converted_into_numeric, c("id", "age"))

  # convert the 2 converted numeric columns into integer
  clean_ll[, c("id", "age")] <- apply(
    clean_ll[, c("id", "age")],
    MARGIN = 2,
    FUN = as.integer
  )

  # remove report to check identical line list <data.frame>
  attr(clean_ll, "report") <- NULL

  expect_identical(ll, clean_ll)
})

test_that("find_duplicates corrects prop_duplicate_row", {
  messy_ll <- simulist::messy_linelist(
    linelist = ll,
    prop_missing = 0,
    prop_spelling_mistakes = 0,
    inconsistent_sex = FALSE,
    numeric_as_char = FALSE,
    date_as_char = FALSE,
    prop_int_as_word = 0,
    prop_duplicate_row = 0.2
  )

  clean_ll <- cleanepi::find_duplicates(data = messy_ll)

  # check report is created as expected
  report <- attr(clean_ll, "report")
  expect_identical(names(report), "found_duplicates")
  expect_identical(
    names(report$found_duplicates),
    c("duplicated_rows", "duplicates_checked_from")
  )

  clean_ll <- cleanepi::remove_duplicates(data = messy_ll)

  report <- attr(clean_ll, "report")
  expect_identical(names(report), c("found_duplicates", "removed_duplicates"))
  expect_identical(
    names(report$found_duplicates),
    c("duplicated_rows", "duplicates_checked_from")
  )


  # remove report to check identical line list <data.frame>
  attr(clean_ll, "report") <- NULL

  expect_identical(ll, clean_ll)
})

test_that("replace_missing_values corrects missing data", {
  # create messy data with 50% of cells missing (i.e. replaced with "NA")
  expect_warning(
    messy_ll <- simulist::messy_linelist(
      linelist = ll,
      prop_missing = 0.5,
      missing_value = "NA",
      prop_spelling_mistakes = 0,
      inconsistent_sex = FALSE,
      numeric_as_char = FALSE,
      date_as_char = FALSE,
      prop_int_as_word = 0,
      prop_duplicate_row = 0
    ),
    regexp = "(The linelist columns:)*(are being coerced to character)"
  )

  # convert "NA" values to NA
  clean_ll <- cleanepi::replace_missing_values(data = messy_ll)

  # check whether report is created as expected
  report <- attr(clean_ll, "report")
  expect_identical(names(report), "missing_values_replaced_at")
  expect_identical(
    report$missing_values_replaced_at,
    c("id", "case_name", "case_type", "sex", "age", "date_onset",
      "date_reporting", "date_admission", "outcome", "date_outcome",
      "date_first_contact", "date_last_contact", "ct_value")
  )

  # the index of the "NA" values in the messy data should be a subset of the
  # NA indices in the cleaned data (subset and not equal because there are NA
  # values already in the messy data, e.g. $date_admission)
  expect_true(
    all(which(messy_ll == "NA") %in% which(is.na(clean_ll)))
  )

  # all of the "NA"s are replaced in cleaned data
  expect_false(any(clean_ll == "NA", na.rm = TRUE))
})

test_that("replace_missing_values corrects multiple missing data values", {
  # create messy data with 50% of cells missing
  expect_warning(
    messy_ll <- simulist::messy_linelist(
      linelist = ll,
      prop_missing = 0.5,
      missing_value = c("N/A", "missing", "not available"),
      prop_spelling_mistakes = 0,
      inconsistent_sex = FALSE,
      numeric_as_char = FALSE,
      date_as_char = FALSE,
      prop_int_as_word = 0,
      prop_duplicate_row = 0
    ),
    regexp = "(The linelist columns:)*(are being coerced to character)"
  )

  # convert "NA" values to NA
  clean_ll <- cleanepi::replace_missing_values(data = messy_ll)

  # check whether report is created as expected
  report <- attr(clean_ll, "report")
  expect_identical(names(report), "missing_values_replaced_at")
  expect_identical(
    report$missing_values_replaced_at,
    c("id", "case_name", "case_type", "sex", "age", "date_onset",
      "date_reporting", "date_admission", "outcome", "date_outcome",
      "date_first_contact", "date_last_contact", "ct_value")
  )

  # the index of the missing values in the messy data should be a subset of the
  # NA indices in the cleaned data (subset and not equal because there are NA
  # values already in the messy data, e.g. $date_admission)
  expect_true(
    all(
      which(
        messy_ll == "N/A" | messy_ll == "missing" | messy_ll == "not available"
      ) %in% which(is.na(clean_ll))
    )
  )

  # all of the missing values are replaced in cleaned data
  expect_false(any(clean_ll == "N/A", na.rm = TRUE))
  expect_false(any(clean_ll == "missing", na.rm = TRUE))
  expect_false(any(clean_ll == "not available", na.rm = TRUE))
})
