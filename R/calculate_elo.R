#' @importFrom data.table setDT dcast
#' @noRd

calculate_wins_one_player <- function(results,value_compare_with, player_base, round_compare_with, compare_function, compare_in_round =TRUE, aggregate = TRUE ){
  loser <- winner <- loses <- wins <- player <- score <- `.` <- `.N` <- `:=` <- NULL
  setDT(results)
  if(aggregate){
    if(!compare_in_round){
      results <- results[,.(wins = sum(compare_function(value_compare_with, score)),
                            loses = sum(compare_function(score, value_compare_with)),
                            match = .N),
                         by = player][,`:=`(winner = player_base, loser = player)][, `:=`(player=NULL)][winner!= loser]
      results <- results[wins + loses == 0, `:=`(wins = match/2,loses = match/2) ]



    }else{
      results <- results[round == round_compare_with][,.(wins = sum(compare_function(value_compare_with, score)),
                                                         loses = sum(compare_function(score, value_compare_with)),
                                                         match = .N),
                                                      by = player][,`:=`(winner = player_base, loser = player)][, `:=`(player=NULL)][winner!= loser]
      results <- results[wins + loses == 0, `:=`(wins = match/2,loses = match/2) ]

    }
  }
  else{
    if(!compare_in_round){
      results[,`:=`(wins = compare_function(value_compare_with, score),
                    loses = sum(compare_function(score, value_compare_with)), match = 1)][,`:=`(winner = player_base, loser = player)][, `:=`(player=NULL)][winner!= loser]

    }else{
      results[round == round_compare_with][,`:=`(wins = compare_function(value_compare_with, score),
                                                 loses = sum(compare_function(score, value_compare_with)), match = 1)][,`:=`(winner = player_base, loser = player)][, `:=`(player=NULL)][winner!= loser]

    }
  }

}




#' @importFrom data.table setDT dcast rbindlist
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @noRd
calculate_wins_all_player <- function(results, list_players, compare_in_round, compare_function, aggregate = TRUE){
  loser <- winner <- loses <- wins <- players <- score <- `.` <- `:=` <- NULL
  results_list <- list()

  pb <- txtProgressBar(min = 1, max = nrow(results), style = 3)
  for(i in 1:nrow(results)){
    row_sel <- i
    value_compare_with <- results[['score']][row_sel]
    player_base <- results[['player']][row_sel]
    round_compare_with <- results[['round']][row_sel]


    results_v2 <- calculate_wins_one_player(results = results, value_compare_with = value_compare_with, round_compare_with = round_compare_with, player_base = player_base, compare_function = compare_function, compare_in_round = compare_in_round, aggregate = aggregate)
    results_list[[length(results_list)+1]] <- results_v2
    setTxtProgressBar(pb, i)
  }
  close(pb)
  results_list_rbind <- rbindlist(results_list)
  if (aggregate){


    results_list_rbind_summary <- results_list_rbind[,.(match = sum(match), loses = sum(loses), wins = sum(wins)),by = .(winner, loser)][,players := paste(winner, loser)]
    return(results_list_rbind_summary)
  }
  else{
    results_list_rbind_summary <- results_list_rbind[,players := paste(winner, loser)]
    return(results_list_rbind_summary)
  }

}

#' @title Extract Coefficients From Glmnet Model
#'
#' @importFrom stats coefficients deviance
#' @noRd

create_summary_model_glmnet <- function(model_epp, player_names,  reference){

    residual_deviance <- list(value = deviance(model_epp),
                            df = df.residual(model_epp))
    vector_coeff_model <- as.vector(coefficients(model_epp))
    intercept <- vector_coeff_model[1]
    epp_summary <- data.frame(player = player_names,
                         epp = vector_coeff_model[-1] - intercept)
    rownames(epp_summary) <- NULL
    epp_summary[nrow(epp_summary),2] <- 0

    results <- list(epp = epp_summary,
                    residual_deviance = residual_deviance)

    if(!is.null(reference)){
      reference_level <- results[['epp']][which(results[['epp']][,"player"] == reference) ,"epp"][1]

      results[['epp']][, 'epp'] <- results[["epp"]][,'epp'] - reference_level
    }
    results
}

#' @title Extract Coefficients From glm Model
#'
#' @importFrom stats coefficients deviance
#' @noRd

  create_summary_model_glm <- function(model_epp, player_names, reference){
    residual_deviance <- list(value = deviance(model_epp),
                              df = df.residual(model_epp))

    reference_glmmodel <- player_names[length(player_names)]
    epp_summary <- data.frame(coefficients(summary(model_epp)))
    colnames(epp_summary) <- c('epp', 'std_epp', 'z_statistic', 'p_value')
    intercept <- epp_summary[1,1]
    rownames(epp_summary)[1] <- reference_glmmodel
    epp_summary[,'player'] <- rownames(epp_summary)
    epp_summary[,1] <- epp_summary[,1]-intercept
    epp_summary[,'conf_lower'] <- epp_summary[,1] - 1.96 * epp_summary[,2]
    epp_summary[,'conf_upper'] <- epp_summary[,1] + 1.96 * epp_summary[,2]
    epp_summary <- epp_summary[,c('player', 'epp', 'std_epp', 'conf_lower',   'conf_upper', 'p_value')]
    rownames(epp_summary) <- NULL

    covariance_epp <- vcov(model_epp)
    rownames(covariance_epp)[1] <- reference_glmmodel
    colnames(covariance_epp)[1] <- reference_glmmodel
    covariance_epp <- covariance_epp[-nrow(covariance_epp),]
    covariance_epp <- covariance_epp[,-ncol(covariance_epp)]

    results <- list(epp = epp_summary[,1:2],
                    epp_summary = epp_summary,
                    covariance_epp = covariance_epp,
                    residual_deviance = residual_deviance)


  if(!is.null(reference)){
    reference_level <- results[['epp']][which(results[['epp']][,"player"] == reference) ,"epp"][1]

    results[['epp']][, 'epp'] <- results[["epp"]][,'epp'] - reference_level
      results[['epp_summary']][, 'epp'] <- results[["epp_summary"]][,'epp'] - reference_level
      results[['epp_summary']][, 'conf.lower'] <- results[["epp_summary"]][,'conf_lower'] - reference_level
      results[['epp_summary']][, 'conf.upper'] <- results[["epp_summary"]][,'conf_upper'] - reference_level

  }

  results
}


#' @title Actual Results for Every Pair of Players
#'
#' @description Calculate number of wins and loses for every pair of Players.
#'
#' @details Naming convention, such as Player, Rounds, etc. comes from [Gosiewska et al. (2020)](https://arxiv.org/abs/2006.02293).
#'
#'
#' @param results raw results
#' @param decreasing_metric Logical. If Score is decreasing metrics.
#' @param compare_in_round Logical. If Players should be compared only in the same Round. Setting to FALSE increase the number of Matches, but Score values of Players will be compared between different Rounds.
#' @param aggregate Logical. If results should be aggregated for every pair of Players. Otherwise, output will have many rows with binary response (according to amount of Rounds in the Tournament) for every pair of Players.
#'
#' @return data.frame
#'
#' @export


calculate_actual_wins <- function(results, decreasing_metric = TRUE, compare_in_round = TRUE, aggregate = TRUE){
  colnames(results) <- c("player", "round", "score")
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

  unique_player <- unique(results$player)

  summary_results <-  calculate_wins_all_player(results = results,
                                               list_players = unique_player,
                                               compare_in_round = compare_in_round,
                                               compare_function = is_metric1_better,
                                               aggregate = aggregate)
  summary_results <- as.data.frame(summary_results)


  summary_results$players <- as.factor(summary_results$players)
  return(summary_results)
}

#' @title Preparing Matrix of Contrasts
#'
#' @param actual_score A data frame created with function calculate_actual_wins.
#' @noRd
#' @importFrom Matrix Matrix
#' @importFrom  stats binomial glm

prepare_model_matrix <- function(actual_score){
  num_level <- nlevels(actual_score$players)
  model_matrix <- matrix(0,nrow = nlevels(actual_score$players), ncol = nlevels(actual_score$winner))
  colnames(model_matrix) <- unique(actual_score$winner)
  rownames(model_matrix) <- unique(actual_score$players)

  player_winner <- gsub(" .*", "", rownames(model_matrix))
  player_loser <- gsub(".+? ", "", rownames(model_matrix))

  for(col in colnames(model_matrix)) {
    model_matrix[col == player_winner, col] <- 1
    model_matrix[col == player_loser, col] <- -1
  }

  model_matrix
}



fit_glm_model <- function(glm_model_matrix, actual_score){

  model_data <- cbind(actual_score[,c("wins","loses")], as.data.frame(glm_model_matrix))

  model_data$wins <- floor(model_data$wins)
  model_data$loses <- floor(model_data$loses)
  model_epp <- glm(cbind(wins, loses)~.,
                    data = model_data,
                    family = binomial)

}


fit_glmnet_model <- function(glm_model_matrix_sparse, actual_score){

  model_epp <- bigGlm(x = glm_model_matrix_sparse,
                      y = as.matrix(actual_score[,c('loses', 'wins')]),
                      family = 'binomial',
                      standardize = FALSE,
                      intercept=TRUE,
                      trace.it = 1)

}


#' @title Calculate EPP Meta-Scores for All Players
#'
#' @param results Data frame. Results for one Round Data should be in the following format.
#' First 3 columns should correspond to: Player, Round, Score. See more in 'details' section.
#' @param decreasing_metric Logical. If TRUE used Score is considered as decreasing, that means a Player with higher Score value is considered as winner.
#' If FALSE used Score will be considered as increasing.
#' @param compare_in_round Logical. If TRUE compares Players only in the same fold. If FALSE compares Players across folds.
#' @param keep_columns Logical. If TRUE original data frame with new 'epp' column will be returned.
#' @param keep_model Logical. If TRUE logistic regression model to compute EPP will be returned.
#' @param reference Character. Name of the Player that should be a reference level for EPP Meta-scores. It should be a name of one of the Players from
#' 'results' data frame. If NULL, none of the Players will be chosen.
#' @param keep_data If all the meta-data should be kept in result.
#' @param estimation Method of estimating EPP coefficients, 'glm' or 'glmnet'.
#'
#' @details Format of the data frame passed via results parameter.
#' First column should correspond to a Player.
#' Second column corresponds to indexes of Rounds As EPP is based on Elo rating system, power of Player is assessed by comparing its results
#' with other Players on multiple Rounds. Therefore, each Player should be evaluated on multiple Rounds.
#' Indexes of Rounds should be in this column. And should match across Players.
#' Third column contains Score used to evaluate players. It can be both decreasing or increasing metric.
#' just remember to set the \code{decreasing_metric} parameter accordingly.
#' The following columns can be of any kind.
#'
#' Naming convention, such as Player, Rounds, etc. comes from [Gosiewska et al. (2020)](https://arxiv.org/abs/2006.02293).
#'
#' @return epp_results
#'
#' @examples
#' library(EloML)
#' data(auc_scores)
#' calculate_epp(auc_scores[1:400,])
#'
#' @export
#' @importFrom glmnet glmnet cv.glmnet bigGlm
calculate_epp <- function(results, decreasing_metric = TRUE, compare_in_round = TRUE,
                          keep_columns = FALSE, keep_model = FALSE,
                          reference = NULL, keep_data = TRUE, estimation = "glmnet"){
  # some cleaning to make unified naming
  players_results <- results[, 1:3]
  colnames(players_results) <- c("player", "round", "score")
  players_results <- players_results[order(players_results[["player"]], players_results[["round"]]),]
  players_results[, "player"] <- factor(players_results[["player"]])
  actual_score <- calculate_actual_wins(results = players_results,
                                        decreasing_metric = decreasing_metric,
                                        compare_in_round=compare_in_round)
  glm_model_matrix <- prepare_model_matrix(actual_score)

  if(estimation == "glm"){
    model_epp <- fit_glm_model(glm_model_matrix, actual_score)
    epp_list <- create_summary_model_glm(model_epp, player_names = colnames(glm_model_matrix),  reference)
    # epp_list[nrow(epp_list),2] <- 0
  }else if(estimation == "glmnet"){
    glm_model_matrix_sparse <- Matrix(glm_model_matrix, sparse = TRUE)
    glm_model_matrix <- Matrix(glm_model_matrix, sparse = TRUE)
    model_epp <- fit_glmnet_model(glm_model_matrix_sparse, actual_score)
    epp_list <- create_summary_model_glmnet(model_epp, player_names = colnames(glm_model_matrix_sparse), reference)

  }



  if(keep_columns == TRUE) {
    tmp <- merge(epp_list[['epp']], players_results, by = "player")
    epp_list[['epp']] <- merge(tmp, results, by.x = c("player", "round", "score"), by.y = colnames(results)[1:3])
    # colnames(epp_list[['epp']])[1:3] <- colnames(results)[1:3]
  }


  browser()
  if(keep_data == TRUE){
    res <- c(epp_list, list(actual_score = actual_score))
  } else {
    res <- c(epp_list, list(actual_score=NULL))
  }
  class(res) <- c("epp_results", "list")
  if(keep_model == TRUE){
    res[["model"]] <- model_epp
  } else {
    res[["model"]] <- NULL
  }
  res[["estimation"]] <- estimation

  res
}

#' @title Printing EPP results
#'
#' @param x epp_results. The result of a function \code{\link{calculate_epp}}.
#' @param ... other parameters
#'
#' @return No return value, prints the structure of the object
#'
#' @export

print.epp_results <- function(x, ...){
  cat("Head of Players EPP: \n")
  print(head(x$epp))
  cat("Type of estimation: ", x$estimation, "\n")
}
