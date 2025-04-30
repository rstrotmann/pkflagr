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
#' @export
#'
#' @examples
#' make_ddi_drugs(type = "inhibitor", target = "2B6")
make_ddi_drugs <- function(
    drug_list = fdi_clinical_ddi_drug_list,
    type = c("inhibitor", "substrate", "inducer"),
    target = c("2D6", "1A2", "OAT1", "3A4", "2C9", "P-gp", "2C19","OATP1B1",
               "OATP1B3", "OAT3", "2B6", "MATE1", "MATE2-K", "2C8", "BCRP")) {
  
  # Input validation
  if (is.null(drug_list)) {
    stop("drug_list cannot be NULL")
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
    filter(TYPE %in% type) %>%
    filter(TARGET %in% target) %>%
    mutate(DRUG = toupper(DRUG)) %>%
    mutate(DRUG = str_remove_all(as.character(lapply(
      str_split(DRUG, " AND "),
      function(x) x[1])),
      "[1-9,]"))
  
  if (nrow(result) == 0) {
    warning("No drugs found matching the specified criteria")
  }
  
  return(result)
}


#' Title
#'
#' @param cm Concomitant medication table as data frame.
#' @param drug_list A data frame with the drugs to be identified, with the columns
#'   DRUG, TYPE, QUALIFIER and TARGET.
#' @returns The concomitant medication table, filtered by the drugs that are in
#'   the drug_list, with added columns DRUG (the canonical DDI drug name), TYPE
#'   (the nature of the DDI object), QUALIFIER (the DDI intensity qualifier),
#'   and TARGET (the DDI target).
#' @import purrr
#' @import stringr
#' @seealso [make_ddi_drugs()]
#' @export
cm_find <- function(
  cm,
  drug_list = NULL
  ) {
  if(is.null(drug_list)) {
    drug_list <- make_ddi_drugs()
  }

  drug_list <- drug_list %>%
    mutate(object_index = row_number())

  temp <- data.frame(
    name = cm$CMDECOD,
    m = purrr::map(drug_list$DRUG, function(x) {str_detect(cm$CMDECOD, x)})
  )
  colnames(temp) <- NULL

  cm %>%
    mutate(object_index = apply(
      temp, 1, function(x) {
        which(x[-1] == TRUE)}
      )) %>%
    unnest(object_index) %>%
    left_join(drug_list, by = "object_index")
}

