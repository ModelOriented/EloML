
#' PCA for models
#'
#'
#' @param list_epp_score list of data frames with epp scores, list of outputs from calculate_epp_score
#' @param transformation_formula formula passed to data.table::dcast function how transform data.
#' If we want PCA on models we use transformation_formula = as.formula("DATASET~MODEL"), otherwise e use transformation_formula = as.formula("MODEL~DATASET")
#' @param value.var colnames with epp score
#' @param ... other arguments pass to PCA function
#' @importFrom data.table rbindlist setDT dcast
#' @importFrom FactoMineR PCA
#' @importFrom stats as.formula
#' @export



calculate_pca <- function(list_epp_score, transformation_formula = as.formula("DATASET~MODEL"), value.var = "EPP", ...){
  if(is.null(names(list_epp_score))){
    names(list_epp_score) <- 1:length(list_epp_score)

  }

  rbind_epp_score <- rbindlist(list_epp_score, idcol = "DATASET")

  rbind_epp_score_wide <- setDT(rbind_epp_score)
  rbind_epp_score_wide <- dcast(rbind_epp_score_wide, transformation_formula, value.var = value.var)

  epp_score_pca <- PCA(rbind_epp_score_wide[,-1], ...)

}


