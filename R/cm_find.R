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
#' @param type
#' @param target
#' @param drug_list
#'
#' @returns
#' @import purrr
#' @import stringr
#' @export
#'
#' @examples
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

