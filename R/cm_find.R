#' Make list of DDI perpetrators or substrates
#'
#' @param drug_list The list of drugs, defaults to a built-in data set provided
#'   by FDA.
#' @param type Type of DDI perpetrator or substrate, as character.
#' @param target Type of DDI target, as character.
#'
#' @returns Data frame
#' @export
#'
#' @examples
#' make_ddi_drugs(type = "inhibitor", target = "2B6")
make_ddi_drugs <- function(
    drug_list = fdi_clinical_ddi_drug_list,
    type = c("inhibitor", "substrate", "inducer"),
    target = c("2D6", "1A2", "OAT1", "3A4", "2C9", "P-gp", "2C19","OATP1B1",
               "OATP1B3", "OAT3", "2B6", "MATE1", "MATE2-K", "2C8", "BCRP")) {
  return(
    drug_list %>%
      filter(TYPE %in% type) %>%
      filter(TARGET %in% target) %>%
      mutate(DRUG = toupper(DRUG)) %>%
      mutate(DRUG = str_remove_all(as.character(lapply(
        str_split(DRUG, " AND "),
        function(x) x[1])),
        "[1-9,]"))
  )
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

