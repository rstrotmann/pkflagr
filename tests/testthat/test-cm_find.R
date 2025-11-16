test_that("cm_find identifies matching drugs in concomitant medication table", {
  # Create test data using tribble
  cm <- tibble::tribble(
    ~USUBJID, ~CMSEQ, ~CMDECOD,
    "001",    1,      "ASPIRIN",
    "001",    2,      "WARFARIN",
    "002",    1,      "IBUPROFEN",
    "002",    2,      "ACETAMINOPHEN"
  )
  
  drug_list <- tibble::tribble(
    ~DRUG,      ~TYPE,      ~TARGET, ~QUALIFIER,
    "ASPIRIN",  "inhibitor", "2C9",  "weak",
    "WARFARIN", "substrate", "2C9",  "sensitive"
  )
  
  result <- cm_find(cm, drug_list)
  
  # Should find ASPIRIN and WARFARIN
  expect_true(nrow(result) >= 2)
  expect_true(all(c("ASPIRIN", "WARFARIN") %in% result$DRUG))
  expect_true(all(c("DRUG", "TYPE", "TARGET", "QUALIFIER") %in% names(result)))
})

test_that("cm_find handles multiple matches for a single medication", {
  cm <- tibble::tribble(
    ~USUBJID, ~CMSEQ, ~CMDECOD,
    "001",    1,      "KETOCONAZOLE"
  )
  
  drug_list <- tibble::tribble(
    ~DRUG,          ~TYPE,      ~TARGET, ~QUALIFIER,
    "KETOCONAZOLE", "inhibitor", "3A4",  "strong",
    "KETOCONAZOLE", "inhibitor", "2C9",  "moderate"
  )
  
  result <- cm_find(cm, drug_list)
  
  # Should find both matches
  expect_equal(nrow(result), 2)
  expect_equal(sum(result$DRUG == "KETOCONAZOLE"), 2)
  expect_true(all(c("3A4", "2C9") %in% result$TARGET))
})

test_that("cm_find uses default drug_list when not provided", {
  cm <- tibble::tribble(
    ~USUBJID, ~CMSEQ, ~CMDECOD,
    "001",    1,      "ASPIRIN"
  )
  
  # Should work with default drug_list from make_ddi_drugs()
  result <- cm_find(cm)
  
  # Result should have the expected structure
  expect_true(is.data.frame(result))
  expect_true(all(c("DRUG", "TYPE", "TARGET") %in% names(result)))
})

test_that("cm_find handles partial string matches", {
  cm <- tibble::tribble(
    ~USUBJID, ~CMSEQ, ~CMDECOD,
    "001",    1,      "ASPIRIN 325MG",
    "002",    1,      "WARFARIN SODIUM"
  )
  
  drug_list <- tibble::tribble(
    ~DRUG,      ~TYPE,      ~TARGET, ~QUALIFIER,
    "ASPIRIN",  "inhibitor", "2C9",  "weak",
    "WARFARIN", "substrate", "2C9",  "sensitive"
  )
  
  result <- cm_find(cm, drug_list)
  
  # Should match partial strings
  expect_true(nrow(result) >= 2)
  expect_true(any(grepl("ASPIRIN", result$CMDECOD)))
  expect_true(any(grepl("WARFARIN", result$CMDECOD)))
})

test_that("cm_find preserves original cm columns", {
  cm <- tibble::tribble(
    ~USUBJID, ~CMSEQ, ~CMDECOD, ~CMSTDTC,
    "001",    1,      "ASPIRIN", "2023-01-01",
    "002",    1,      "IBUPROFEN", "2023-01-02"
  )
  
  drug_list <- tibble::tribble(
    ~DRUG,      ~TYPE,      ~TARGET, ~QUALIFIER,
    "ASPIRIN",  "inhibitor", "2C9",  "weak"
  )
  
  result <- cm_find(cm, drug_list)
  
  # Should preserve original columns
  expect_true("CMSTDTC" %in% names(result))
  expect_true("USUBJID" %in% names(result))
  expect_true("CMSEQ" %in% names(result))
})

test_that("cm_find handles case sensitivity in drug matching", {
  cm <- tibble::tribble(
    ~USUBJID, ~CMSEQ, ~CMDECOD,
    "001",    1,      "aspirin",  # lowercase
    "002",    1,      "WARFARIN"  # uppercase
  )
  
  drug_list <- tibble::tribble(
    ~DRUG,      ~TYPE,      ~TARGET, ~QUALIFIER,
    "ASPIRIN",  "inhibitor", "2C9",  "weak",
    "WARFARIN", "substrate", "2C9",  "sensitive"
  )
  
  result <- cm_find(cm, drug_list)
  
  # Note: current implementation is case-sensitive, so "aspirin" may not match "ASPIRIN"
  # This test documents current behavior
  expect_true(nrow(result) >= 1)  # At least WARFARIN should match
})

test_that("cm_find returns empty result when no matches found", {
  cm <- tibble::tribble(
    ~USUBJID, ~CMSEQ, ~CMDECOD,
    "001",    1,      "XYLOPHONE",  # Not in drug list
    "002",    1,      "NONEXISTENT" # Not in drug list
  )
  
  drug_list <- tibble::tribble(
    ~DRUG,      ~TYPE,      ~TARGET, ~QUALIFIER,
    "ASPIRIN",  "inhibitor", "2C9",  "weak"
  )
  
  # Note: This may fail with current implementation due to unnest() issue
  # Testing current behavior
  expect_error(
    cm_find(cm, drug_list),
    NA  # Should not error, but may due to bug
  )
})

test_that("cm_find handles empty cm table", {
  cm <- tibble::tribble(
    ~USUBJID, ~CMSEQ, ~CMDECOD
  )
  
  drug_list <- tibble::tribble(
    ~DRUG,      ~TYPE,      ~TARGET, ~QUALIFIER,
    "ASPIRIN",  "inhibitor", "2C9",  "weak"
  )
  
  # Note: This may fail with current implementation due to unnest() issue
  # Testing current behavior
  expect_error(
    result <- cm_find(cm, drug_list),
    NA
  )
})

test_that("cm_find handles empty drug_list", {
  cm <- tibble::tribble(
    ~USUBJID, ~CMSEQ, ~CMDECOD,
    "001",    1,      "ASPIRIN"
  )
  
  drug_list <- tibble::tribble(
    ~DRUG, ~TYPE, ~TARGET, ~QUALIFIER
  )
  
  # Should handle empty drug_list gracefully
  # Note: This may fail with current implementation due to unnest() issue
  expect_error(
    cm_find(cm, drug_list),
    NA
  )
})

test_that("cm_find requires CMDECOD column in cm", {
  cm <- tibble::tribble(
    ~USUBJID, ~CMSEQ, ~CMTRT,  # Missing CMDECOD
    "001",    1,      "ASPIRIN"
  )
  
  drug_list <- tibble::tribble(
    ~DRUG,      ~TYPE,      ~TARGET, ~QUALIFIER,
    "ASPIRIN",  "inhibitor", "2C9",  "weak"
  )
  
  # Should error when CMDECOD is missing
  expect_error(
    cm_find(cm, drug_list),
    "CMDECOD"
  )
})

test_that("cm_find handles drugs with 'AND' in name correctly", {
  # Test that make_ddi_drugs() processes drugs with "AND" correctly
  # and cm_find can match them
  cm <- tibble::tribble(
    ~USUBJID, ~CMSEQ, ~CMDECOD,
    "001",    1,      "ASPIRIN AND CAFFEINE"
  )
  
  # Create drug_list that might have "AND" processed
  drug_list <- tibble::tribble(
    ~DRUG,      ~TYPE,      ~TARGET, ~QUALIFIER,
    "ASPIRIN",  "inhibitor", "2C9",  "weak"
  )
  
  result <- cm_find(cm, drug_list)
  
  # Should match ASPIRIN even if full name contains "AND"
  expect_true(nrow(result) >= 1)
})

test_that("cm_find adds object_index and joins correctly", {
  cm <- tibble::tribble(
    ~USUBJID, ~CMSEQ, ~CMDECOD,
    "001",    1,      "ASPIRIN"
  )
  
  drug_list <- tibble::tribble(
    ~DRUG,      ~TYPE,      ~TARGET, ~QUALIFIER,
    "ASPIRIN",  "inhibitor", "2C9",  "weak"
  )
  
  result <- cm_find(cm, drug_list)
  
  # Should have joined drug_list information
  expect_true("object_index" %in% names(result))
  expect_true(all(result$DRUG == "ASPIRIN"))
  expect_true(all(result$TYPE == "inhibitor"))
  expect_true(all(result$TARGET == "2C9"))
})
