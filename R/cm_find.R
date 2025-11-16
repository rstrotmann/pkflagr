#' Make list of DDI perpetrators or substrates
#'
#' @param drug_list The list of drugs, defaults to a built-in data set provided
#'   by FDA. Must contain columns DRUG, TYPE, and TARGET.
#' @param type Type of DDI perpetrator or substrate, as character vector.
#'   Valid values are "inhibitor", "substrate", and "inducer".
#' @param target Type of DDI target, as character vector. Valid values are
#'   "2D6", "1A2", "OAT1", "3A4", "2C9", "P-gp", "2C19", "OATP1B1",
#'   "OATP1B3", "OAT3", "2B6", "MATE1", "MATE2-K", "2C8", "BCRP".
#'
#' @returns A data frame containing the filtered drug list with columns:
#'   \itemize{
#'     \item DRUG: The canonical drug name (uppercase, without numbers and commas)
#'     \item TYPE: The type of DDI (inhibitor, substrate, or inducer)
#'     \item TARGET: The target of the DDI
#'     \item QUALIFIER: The intensity qualifier (if present in input)
#'   }
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @export
#'
#' @examples
#' make_ddi_drugs(type = "inhibitor", target = "2B6")
make_ddi_drugs <- function(
    drug_list = NULL,
    type = c("inhibitor", "substrate", "inducer"),
    target = c("2D6", "1A2", "OAT1", "3A4", "2C9", "P-gp", "2C19","OATP1B1",
               "OATP1B3", "OAT3", "2B6", "MATE1", "MATE2-K", "2C8", "BCRP")) {

  # Input validation
  if (is.null(drug_list)) {
    drug_list = fda_clinical_ddi_drug_list
  }

  required_cols <- c("DRUG", "TYPE", "TARGET")
  missing_cols <- setdiff(required_cols, names(drug_list))
  if (length(missing_cols) > 0) {
    stop("drug_list is missing required columns: ",
         paste(missing_cols, collapse = ", "))
  }

  # Validate type and target parameters
  valid_types <- c("inhibitor", "substrate", "inducer")
  invalid_types <- setdiff(type, valid_types)
  if (length(invalid_types) > 0) {
    stop("Invalid type values: ", paste(invalid_types, collapse = ", "))
  }

  valid_targets <- c("2D6", "1A2", "OAT1", "3A4", "2C9", "P-gp", "2C19",
                    "OATP1B1", "OATP1B3", "OAT3", "2B6", "MATE1", "MATE2-K",
                    "2C8", "BCRP")
  invalid_targets <- setdiff(target, valid_targets)
  if (length(invalid_targets) > 0) {
    stop("Invalid target values: ", paste(invalid_targets, collapse = ", "))
  }

  result <- drug_list %>%
    filter(.data$TYPE %in% type) %>%
    filter(.data$TARGET %in% target) %>%
    mutate(DRUG = toupper(.data$DRUG)) %>%
    mutate(DRUG = str_remove_all(as.character(lapply(
      str_split(.data$DRUG, " AND "),
      function(x) x[1])),
      "[1-9,]"))

  if (nrow(result) == 0) {
    warning("No drugs found matching the specified criteria")
  }

  return(result)
}


#' Identify concomitant medication with potential DDI effects
#'
#' This function identifies concomitant medications that match drugs in a
#' drug-drug interaction (DDI) list. String matching is performed
#' case-insensitively, so "aspirin" will match "ASPIRIN". The function returns
#' all matching rows with DDI information added.
#'
#' @param cm Concomitant medication table as data frame. Must contain a column
#'   named `CMDECOD` with medication names. May contain other columns (e.g.,
#'   `USUBJID`, `CMSEQ`) which will be preserved in the output.
#' @param drug_list A data frame with the drugs to be identified, with the columns
#'   DRUG, TYPE, QUALIFIER and TARGET. If NULL, uses the default FDA drug list
#'   via `make_ddi_drugs()`.
#' @returns The concomitant medication table, filtered by the drugs that are in
#'   the drug_list, with added columns DRUG (the canonical DDI drug name), TYPE
#'   (the nature of the DDI object), QUALIFIER (the DDI intensity qualifier),
#'   and TARGET (the DDI target). Returns an empty data frame with the correct
#'   column structure if no matches are found. String matching is
#'   case-insensitive.
#'
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @seealso [make_ddi_drugs()] for creating custom drug lists
#'
#' @examples
#' # Example 1: Basic usage with custom drug list
#' cm <- data.frame(
#'   USUBJID = c("001", "002"),
#'   CMSEQ = c(1, 1),
#'   CMDECOD = c("ASPIRIN", "WARFARIN")
#' )
#' drug_list <- data.frame(
#'   DRUG = c("ASPIRIN", "WARFARIN"),
#'   TYPE = c("inhibitor", "substrate"),
#'   TARGET = c("2C9", "2C9"),
#'   QUALIFIER = c("weak", "sensitive")
#' )
#' cm_find(cm, drug_list)
#'
#' # Example 2: Case-insensitive matching
#' cm <- data.frame(
#'   USUBJID = "001",
#'   CMSEQ = 1,
#'   CMDECOD = "aspirin"  # lowercase
#' )
#' drug_list <- data.frame(
#'   DRUG = "ASPIRIN",  # uppercase
#'   TYPE = "inhibitor",
#'   TARGET = "2C9",
#'   QUALIFIER = "weak"
#' )
#' cm_find(cm, drug_list)  # Will match despite case difference
#'
#' # Example 3: Multiple matches for a single medication
#' cm <- data.frame(
#'   USUBJID = "001",
#'   CMSEQ = 1,
#'   CMDECOD = "KETOCONAZOLE"
#' )
#' drug_list <- data.frame(
#'   DRUG = c("KETOCONAZOLE", "KETOCONAZOLE"),
#'   TYPE = c("inhibitor", "inhibitor"),
#'   TARGET = c("3A4", "2C9"),
#'   QUALIFIER = c("strong", "moderate")
#' )
#' cm_find(cm, drug_list)  # Returns 2 rows (one for each target)
#'
#' # Example 4: No matches found (returns empty data frame)
#' cm <- data.frame(
#'   USUBJID = "001",
#'   CMSEQ = 1,
#'   CMDECOD = "XYLOPHONE"  # Not in drug list
#' )
#' drug_list <- data.frame(
#'   DRUG = "ASPIRIN",
#'   TYPE = "inhibitor",
#'   TARGET = "2C9",
#'   QUALIFIER = "weak"
#' )
#' result <- cm_find(cm, drug_list)  # Returns empty data frame, no error
#' nrow(result)  # 0
#'
#' # Example 5: Using default FDA drug list
#' cm <- data.frame(
#'   USUBJID = "001",
#'   CMSEQ = 1,
#'   CMDECOD = "ASPIRIN"
#' )
#' cm_find(cm)  # Uses make_ddi_drugs() default
#'
#' @export
cm_find <- function(
  cm,
  drug_list = NULL
  ) {
  # Input validation for cm
  if (!is.data.frame(cm)) {
    stop("cm must be a data frame")
  }
  
  if (!"CMDECOD" %in% names(cm)) {
    stop("cm must contain a column named 'CMDECOD'")
  }
  
  # Handle empty cm table - crossing() will naturally return empty result
  # but we validate structure first
  
  # Input validation and default for drug_list
  if(is.null(drug_list)) {
    drug_list <- make_ddi_drugs()
  }
  
  # Validate drug_list structure
  if (!is.data.frame(drug_list)) {
    stop("drug_list must be a data frame")
  }
  
  required_drug_cols <- c("DRUG", "TYPE", "TARGET")
  missing_cols <- setdiff(required_drug_cols, names(drug_list))
  if (length(missing_cols) > 0) {
    stop("drug_list is missing required columns: ",
         paste(missing_cols, collapse = ", "))
  }
  
  # Handle empty drug_list - crossing() will naturally return empty result
  # No need for special handling, but we validate structure first

  drug_list <- drug_list %>%
    mutate(object_index = row_number())

  # Create cross join of all cm rows with all drug_list rows
  # Then filter to only matches where drug name is found in medication name
  # Matching is case-insensitive (both converted to uppercase for comparison)
  # crossing() already includes all columns from both cm and drug_list
  cm %>%
    crossing(drug_list) %>%
    filter(str_detect(toupper(.data$CMDECOD), toupper(.data$DRUG)))
}


#' Flag use of concomitant medication.
#'
#' @inheritParams cm_find
#' @export
cm_flag <- function(
    cm,
    drug_list = NULL) {
  temp <- cm_find(cm, drug_list) %>%
    distinct(.data$USUBJID, .data$CMSEQ, .data$DRUG, .data$TYPE,
             .data$QUALIFIER, .data$TARGET)

  out <- cm %>%
    left_join(temp, by = c("USUBJID", "CMSEQ")) %>%
    mutate(DDI_FLAG = !is.na(.data$DRUG))
}





