#' Title
#'
#' @param cm
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
  type = "inhibitor",
  target = c("2D6", "1A2", "OAT1", "3A4", "2C9", "P-gp", "2C19","OATP1B1",
             "OATP1B3", "OAT3", "2B6", "MATE1", "MATE2-K", "2C8", "BCRP"),
  drug_list = fdi_clinical_ddi_drug_list
  ) {
  # validate input

  objects <- drug_list %>%
    filter(type == type) %>%
    filter(target %in% target) %>%
    mutate(drug = toupper(drug)) %>%
    mutate(drug = str_remove_all(as.character(lapply(
      str_split(objects$drug, " AND "),
      function(x) x[1])),
      "[1-9,]"))

  # patterns <- unique(str_remove_all(as.character(unique(lapply(
  #   str_split(objects$drug, " AND "),
  #   function(x) x[1]))),
  #   "[1-9,]"))

  patterns <- unique(objects$drug)

  temp <- data.frame(
    name = cm$CMDECOD,
    m = purrr::map(patterns, function(x) {str_detect(cm$CMDECOD, x)})
  )
  # colnames(temp) = c("drug", patterns)

  # apply(temp, 1, function(x) {
  #   which(x[-1] == TRUE)
  # })

  cm %>%
    mutate(drug_index = apply(
      temp, 1, function(x) {
        which(x[-1] == TRUE)}
      ))


  # fcm <- function(cmdecod) {
  #   i <- str_detect(cmdecod, patterns)
  #   return(patterns[i])
  # }
  #
  # fcm_v <- Vectorize(fcm)
  #
  # temp <- cm %>%
  #   mutate(d = fcm_v(CMDECOD))
}
