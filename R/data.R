#' FDA Clinical Drug-Drug Interaction List
#'
#' A dataset containing information about drugs that interact with various
#' cytochrome P450 enzymes and transporter systems, as provided by the FDA.
#' This dataset is used to identify potential drug-drug interactions (DDIs)
#' in clinical studies.
#'
#' @format A tibble with 4 columns:
#' \describe{
#'   \item{DRUG}{Character. The name of the drug or substance.}
#'   \item{TYPE}{Character. The type of interaction: "inhibitor", "substrate", or "inducer".}
#'   \item{QUALIFIER}{Character. The intensity of the interaction: "strong", "moderate", "weak", or "sensitive".}
#'   \item{TARGET}{Character. The enzyme or transporter system affected by the drug:
#'     \itemize{
#'       \item CYP enzymes: "1A2", "2B6", "2C8", "2C9", "2C19", "2D6", "3A4"
#'       \item Transporters: "OATP1B1", "OATP1B3", "OAT1", "OAT3", "P-gp", "BCRP", "MATE1", "MATE2-K"
#'     }
#'   }
#' }
#'
#' @source U.S. Food and Drug Administration (FDA) Drug Development and Drug Interactions:
#' Table of Substrates, Inhibitors and Inducers
#' \url{https://www.fda.gov/drugs/drug-interactions-labeling/drug-development-and-drug-interactions-table-substrates-inhibitors-and-inducers}
"fdi_clinical_ddi_drug_list"
