test_that("metadata is present after calling func", {
  data(df_rosters)
  metadata_cols <- c("color", "sort_order", "cost")
  df_rosters <- add_roster_metadata(df_rosters)
  expect_equal(metadata_cols %in% colnames(df_rosters), c(TRUE, TRUE, TRUE))
})
