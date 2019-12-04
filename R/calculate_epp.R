#' @importFrom dplyr filter left_join mutate rename group_by summarise
#' @importFrom data.table setDT dcast
#' @noRd

calculate_wins_one_model <- function(results,value_compare_with, model_base, split_compare_with, compare_function, compare_in_split =TRUE, aggregate = TRUE ){
  setDT(results)
  if(aggregate){
    if(!compare_in_split){
      results[,.(wins = sum(compare_function(value_compare_with,score)), match = .N),by = model][,`:=`(winner = model_base, loser = model)][, `:=`(model=NULL)][winner!= loser]

    }else{
      results[split == split_compare_with][,.(wins = sum(compare_function(value_compare_with, score)), match = .N),by = model][,`:=`(winner = model_base, loser = model)][, `:=`(model=NULL)][winner!= loser]

    }
  }
  else{
    if(!compare_in_split){
      results[,`:=`(wins = compare_function(value_compare_with, score), match = 1)][,`:=`(winner = model_base, loser = model)][, `:=`(model=NULL)][winner!= loser]

    }else{
      results[split == split_compare_with][,`:=`(wins = compare_function(value_compare_with, score), match = 1)][,`:=`(winner = model_base, loser = model)][, `:=`(model=NULL)][winner!= loser]

    }
  }

}




#' @importFrom dplyr filter left_join mutate rename group_by summarise
#' @importFrom data.table setDT dcast
#' @noRd
calculate_wins_all_model <- function(results, list_models, compare_in_split, compare_function, aggregate = TRUE){

  results_list <- list()
  for(i in 1:nrow(results)){
    row_sel <- i
    value_compare_with <- results[['score']][row_sel]
    model_base <- results[['model']][row_sel]
    split_compare_with <- results[['split']][row_sel]


    results_v2 <- calculate_wins_one_model(results = results, value_compare_with = value_compare_with, split_compare_with = split_compare_with, model_base = model_base, compare_function = compare_function, compare_in_split = compare_in_split, aggregate = aggregate)
    results_list[[length(results_list)+1]] <- results_v2
  }
  results_list_rbind <- rbindlist(results_list)
  if (aggregate){


    results_list_rbind_summary <- results_list_rbind[,.(match = sum(match), wins = sum(wins)),by = .(winner, loser)][,players := paste(winner, loser)]
    return(results_list_rbind_summary)
  }
  else{
    results_list_rbind_summary <- results_list_rbind[,players := paste(winner, loser)]
    return(results_list_rbind_summary)
  }

}

#' @importFrom stats coefficients
#' @noRd
create_summary_model <- function(model_epp){

  vector_coeff_model <- as.vector(coefficients(model_epp))

  names(vector_coeff_model) <- gsub("^players", "", rownames(coefficients(model_epp)))


  # Now we remove Intercept
  vector_coeff_model <- vector_coeff_model[-1]

  result <- data.frame(model = names(vector_coeff_model),
                       epp = vector_coeff_model)
  result
}


#' Calulate actual results for every pair of models
#'
#'
#' @param results raw results
#' @param decreasing_metric if used metric is decreasing
#' @param compare_in_split if compare models and parameters only in fhe same fold
#' @param aggregate if results should be aggregated for every pair of models and hyperparameters. Otherwise output will have many rows with binary response (acccording to amount of splitsin crossvalidation) for every pair of models
#' @export


calculate_actual_wins <- function(results, decreasing_metric = TRUE, compare_in_split, aggregate = TRUE){
  ### define comparison of metrics
  if(decreasing_metric){
    is_metric1_better <- function(metric1, metric2){
      as.numeric(metric1>metric2)

    }
  }else{

    is_metric1_better <- function(metric1, metric2){
      as.numeric(metric1<metric2)

    }

  }

  unique_model <- unique(results$model)

  summary_results <-  calculate_wins_all_model(results = results,
                                               list_models = unique_model,
                                               compare_in_split = compare_in_split,
                                               compare_function = is_metric1_better,
                                               aggregate = aggregate)
  summary_results <- as.data.frame(summary_results)


  summary_results$players <- as.factor(summary_results$players)
  return(summary_results)
}

#' @title Preparing matrix of contrasts
#'
#' @param actual_score A data frame created with function calculate_actual_wins.
#' @noRd
#' @importFrom Matrix Matrix

prepare_model_matrix <- function(actual_score){
  num_level <- nlevels(actual_score$players)
  model_matrix <- matrix(0,nrow = nlevels(actual_score$players), ncol = nlevels(actual_score$winner))
  colnames(model_matrix) <- levels(actual_score$winner)
  rownames(model_matrix) <- levels(actual_score$players)

  model_winner <- gsub(" .*", "", rownames(model_matrix))
  model_loser <- gsub(".+? ", "", rownames(model_matrix))

  for(col in colnames(model_matrix)) {
    model_matrix[col == model_winner, col] <- 1
    model_matrix[col == model_loser, col] <- -1
  }

  model_matrix <- Matrix(model_matrix, sparse = TRUE)
}


#' @title Calulate EPP score for all models
#'
#' @param results data frame with results for one dataset. Data should be in the following format.
#' First 3 columns should correspond to: model, split, score. See more in 'details' section.
#' @param decreasing_metric Logical. If TRUE used metric is considered as decreasing, that means a model with higher score value is considered as better model.
#' If FALSE used metric will be considered as increasing.
#' @param compare_in_split Logical. If TRUE compares models only in the same fold. If FALSE compares models across folds.
#' @param keep_data Logical. If FALSE only EPP scores will be returned. If TRUE original data frame with new 'epp' column will be returned.
#' @param reference Model that should be a reference level for EPP scores. It should be a name of one of the models from
#' 'results' data frame. If NULL, none of the models will be chosen.
#'
#' @details Format of the data frame passed via results parameter.
#' First column should correspond to a model. Dofferent settings of hyperparameters of the same model should have different values in this column.
#' Second column corresponds to indexes of splits. As EPP is based on Elo rating system, power of model is assessed by comparing its results
#' with other models on multiple data splits. Therefore, each model should be evaluated on multiple train-test splits.
#' Indexes of splits should be in this column. And should match across models.
#' Third column contains score used to evaluate models. It can be both decreasing or increasing metric,
#' just remember to set the \code{decreasing_metric} parameter accordingly.
#' The following columns can be of any kind.
#'
#' @examples
#' library(EloML)
#' data(auc_scores)
#' calculate_epp(auc_scores)
#'
#' @export
#' @importFrom glmnet glmnet
calculate_epp <- function(results, decreasing_metric = TRUE, compare_in_split = TRUE, keep_data = FALSE, reference = NULL){
  # some cleaning to make unified naming
  models_results <- results[, 1:3]
  colnames(models_results) <- c("model", "split", "score")
  models_results[, "model"] <- factor(models_results[, "model"])

  actual_score <- calculate_actual_wins(results = models_results, decreasing_metric = decreasing_metric, compare_in_split=compare_in_split)

  glm_model_matrix_sparse <- prepare_model_matrix(actual_score)

  actual_score$loses <- actual_score$match - actual_score$wins

  model_epp <- glmnet(x = glm_model_matrix_sparse[,-1], y = as.matrix(actual_score[,c('loses', 'wins')]),
                                family = 'binomial', lambda = 0, standardize = FALSE)



  res <- create_summary_model(model_epp)
  rownames(res) <- NULL

  if(!is.null(reference)){
    reference_level <- res[which(res[,"model"] == reference) ,"epp"][1]

    res[,"epp"] <- res[,"epp"] - reference_level
  }
  if(keep_data == TRUE) {
    tmp <- merge(res, models_results, by = "model")
    res <- merge(tmp, results, by.x = c("model", "split", "score"), by.y = colnames(results)[1:3])
    colnames(res)[1:3] <- colnames(results)[1:3]
  }

  res
}
