#' @title Plot EPP Meta-Score Ranking
#'
#' @description EPP Meta-Score values vs Aggregated values of Scores.
#'
#' @param epp epp_results. The result of a function \code{\link{calculate_epp}}.
#' @param aggregation Function. Aggregation function, for example mean or median.
#' @param confidence_intervals Logical. If confidence intervals should be plotted. Only for 'glm' estimation.
#' @param show_player_names Logical. If Players' names should be shown.
#'
#' @return gg
#'
#' @import ggplot2
#' @importFrom ggrepel geom_text_repel
#'
#' @details Naming convention, such as Player, Rounds, etc. comes from [Gosiewska et al. (2020)](https://arxiv.org/abs/2006.02293).
#'
#' @examples
#' library(EloML)
#' data(auc_scores)
#' epp <- calculate_epp(
#'   auc_scores[1:60, ],
#'   keep_columns=TRUE,
#'   keep_model = TRUE,
#'   estimation ="glm"
#'   )
#' plot_epp_ranking(epp, confidence_intervals = TRUE, show_player_names = TRUE)
#'
#' @export
plot_epp_ranking <- function(epp, aggregation = mean, confidence_intervals = FALSE, show_player_names = FALSE){
  if(ncol(epp[["epp"]]) == 2) stop("Use `keep_columns == TRUE` parameter in function `calculate_epp()`")
  if(!("model" %in% names(epp)) & confidence_intervals)  stop("Use `keep_model == TRUE` parameter in function `calculate_epp()`")
  if((epp[["estimation"]] != "glm") & confidence_intervals)  stop("Use `estimation == 'glm'` parameter in function `calculate_epp()`")

  player_name <- colnames(epp[["epp"]])[1]
  score_name <- colnames(epp[["epp"]])[3]
  plot_df <- epp[["epp"]][, c(player_name, score_name, "epp")]
  plot_df <- merge(
    aggregate(as.formula(paste(score_name, "~", player_name)), data = plot_df, FUN = aggregation),
    aggregate(as.formula(paste("epp", "~", player_name)), data = plot_df, FUN = aggregation)
    )
  if(confidence_intervals){
    conf <- summary(epp[["model"]])$coefficients[,2]
    plot_df[["sd"]] = conf[unlist(plot_df[player_name])]
    plot_df[is.na(plot_df$sd), "sd"] <- conf["(Intercept)"]
    possible_ci <- geom_errorbarh(aes(xmin = epp-1.96 *sd, xmax=epp+1.96 *sd))
  } else{
    possible_ci <- NULL
  }
  if (show_player_names){
    possible_player_names <- geom_text_repel(aes(label = get(player_name)), color = "#4AC4B6", max.overlaps = 100)
  }else{
    possible_player_names <- NULL
  }
  ggplot(plot_df, aes(x = epp, y = get(score_name))) +
    possible_ci +
    geom_point(aes(x = epp, y = get(score_name)), size = 3) +
    possible_player_names +
    theme_bw() +
    ylab("Aggregated Score") +
    xlab("EPP")
  }
