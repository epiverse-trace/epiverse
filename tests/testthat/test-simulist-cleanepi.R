set.seed(1)
ll <- sim_linelist()

test_that("convert_to_numeric corrects prop_int_as_word", {
  messy_ll <- messy_linelist(
    linelist = ll,
    prop_missing = 0,
    prop_spelling_mistakes = 0,
    inconsistent_sex = FALSE,
    numeric_as_char = FALSE,
    date_as_char = FALSE,
    prop_int_as_word = 0.5,
    prop_duplicate_row = 0
  )

  clean_ll <- cleanepi::convert_to_numeric(
    data = messy_ll,
    target_columns = c("id", "age")
  )

  expect_false(identical(ll, clean_ll))
  clean_ll[, c("id", "age")] <- apply(clean_ll[, c("id", "age")], MARGIN = 2, FUN = as.integer)

  # check report is created as expected
  report <- attr(clean_ll, "report")
  expect_identical(names(report), "converted_into_numeric")
  expect_identical(report$converted_into_numeric, "id, age")

  # remove report to check identical line list <data.frame>
  attr(clean_ll, "report") <- NULL

  expect_identical(ll, clean_ll)
})

test_that("find_duplicates corrects prop_duplicate_row", {
  messy_ll <- messy_linelist(
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
  expect_identical(names(report), c("duplicated_rows", "duplicates_checked_from"))

  clean_ll <- cleanepi::remove_duplicates(data = messy_ll)

  report <- attr(clean_ll, "report")
  expect_identical(names(report), c("duplicated_rows", "duplicates_checked_from", "removed_duplicates"))


  # remove appended column in cleaning
  clean_ll$row_id <- NULL

  # remove report to check identical line list <data.frame>
  attr(clean_ll, "report") <- NULL

  expect_identical(ll, clean_ll)
})

