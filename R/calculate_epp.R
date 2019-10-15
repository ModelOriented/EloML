#' @importFrom dplyr filter left_join mutate rename group_by summarise
#' @importFrom data.table setDT dcast
#' @noRd

calculate_wins_one_model <- function(results,value_compare_with, model_base, split_compare_with, compare_function, compare_in_split =TRUE ){
  if(!compare_in_split){
    results[,.(win = sum(compare_function(score,value_compare_with)), match = .N),by = model][,`:=`(winner = model_base, loser = model)][, `:=`(model=NULL)][winner!= loser]

  }else{
    results[split == split_compare_with][,.(win = sum(compare_function(score,value_compare_with)), match = .N),by = model][,`:=`(winner = model_base, loser = model)][, `:=`(model=NULL)][winner!= loser]

  }
 }




#' @importFrom dplyr filter left_join mutate rename group_by summarise
#' @importFrom data.table setDT dcast
#' @noRd
calculate_wins_all_model <- function(results, list_models, compare_in_split, compare_function){
  # # some tricks to get rid of notes about 'ins'no visible binding' notes
  # model <- compare_with <- score.x <- score.y <- winner <- loser <- n <- win <- NULL
  #
  # list_models <- data.frame(compare_with = list_models)
  #
  # tmp <- base::merge(results,  list_models, by=NULL)
  # tmp <- filter(tmp, model  != compare_with)
  #
  # if(compare_in_split){
  #   tmp_joined <- left_join(tmp, results, by = c("compare_with" = "model", "split"="split"))
  # }else{
  #   tmp_joined <- left_join(tmp, results, by = c("compare_with" = "model"))
  # }
  #
  # tmp_joined <- mutate(tmp_joined, win = compare_function(score.x, score.y))
  # tmp_joined <- rename(tmp_joined, winner = model, loser = compare_with)
  # tmp_joined <- group_by(tmp_joined, winner, loser )
  # tmp_joined <- summarise(tmp_joined, match = n(), wins = sum(win))
  # tmp_joined$players <- factor(paste(tmp_joined$winner, tmp_joined$loser))
  # tmp_joined

  results_list <- list()
  for(i in 1:nrow(results)){
    row_sel <- i
    value_compare_with <- results[['score']][row_sel]
    model_base <- results[['model']][row_sel]
    split_compare_with <- results[['split']][row_sel]


    results_v2 <- calculate_wins_one_model(results = results, value_compare_with = value_compare_with, split_compare_with = split_compare_with, model_base = model_base, compare_function = compare_function, compare_in_split = compare_in_split)
    results_list[[length(results_list)+1]] <- results_v2
  }

  results_list_rbind <- rbindlist(results_list)

  results_list_rbind_summary <- results_list_rbind[,.(match = sum(match), win = sum(win)),by = .(winner, loser)][,players := paste(winner, loser)]
  return(results_list_rbind_summary)
}

#' @importFrom stats coefficients
#' @noRd
create_summary_model <- function(model_epp){

  vector_coeff_model <- coefficients(model_epp)
  names(vector_coeff_model) <- gsub("^players", "", names(vector_coeff_model))
  #model with NA coefficients gain 0 coefficitent
  vector_coeff_model[is.na(vector_coeff_model)] <- 0
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
#'
#' @export


calculate_actual_wins <- function(results, decreasing_metric = TRUE, compare_in_split){
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
                                               compare_function = is_metric1_better)

  as.data.frame(summary_results)
}

#' @title Preparing matrix of contrasts
#'
#' @param actual_score A data frame created with function calculate_actual_wins.
#' @noRd
prepare_contrasts <- function(actual_score){
   num_level <- nlevels(actual_score$players)
   contrasts <- matrix(0,nrow = nlevels(actual_score$players), ncol = nlevels(actual_score$winner))
   colnames(contrasts) <- levels(actual_score$winner)
   rownames(contrasts) <- levels(actual_score$players)

   model_winner <- gsub(" .*", "", rownames(contrasts))
   model_loser <- gsub(".+? ", "", rownames(contrasts))

   for(col in colnames(contrasts)) {
       contrasts[col == model_winner, col] <- 1
       contrasts[col == model_loser, col] <- -1
   }

  contrasts
}


#' @title Calulate EPP score for all models
#'
#' @param results data frame with results for one dataset. Data should be in the following format.
#' First 3 columns should correspond to: model, split, score. See more in 'details' section.
#' @param decreasing_metric Logical. If TRUE used metric is considered as decreasing, that means a model with higher score value is considered as better model.
#' If FALSE used metric will be considered as increasing.
#' @param compare_in_split Logical. If TRUE compares models only in the same fold. If FALSE compares models across folds.
#' @param keep_data Logical. If FALSE only EPP scores will be returned. If TRUE original data frame with new 'epp' column will be returned.
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
#' @importFrom stats glm
calculate_epp <- function(results, decreasing_metric = TRUE, compare_in_split = TRUE, keep_data = FALSE){
  # some cleaning to make unified naming
  models_results <- results[, 1:3]
  colnames(models_results) <- c("model", "split", "score")
  models_results[, "model"] <- factor(models_results[, "model"])

  actual_score <- calculate_actual_wins(results = models_results, decreasing_metric = decreasing_metric, compare_in_split=compare_in_split)

  contrasts <- prepare_contrasts(actual_score)
  model_epp <- glm(cbind(wins, match - wins) ~ players, data = actual_score, family = "binomial",
                   contrasts = list(players = contrasts))

  res <- create_summary_model(model_epp)
  rownames(res) <- NULL
  if(keep_data == TRUE) {
    tmp <- merge(res, results, by.x = "model", by.y = colnames(results)[1])
    res <- cbind(tmp$epp, results)
    colnames(res)[1] <- "epp"
  }

  res
}







