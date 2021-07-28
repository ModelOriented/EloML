#' @title Plot EPP ranking
#'
#' @param epp object of class epp_results.
#' @param aggregation aggregation function.
#' @param confidence_intervals whether confidence intervals should be plotted. Only for "glmnet" estimation.
#' @param show_player_names whether player names should be shown
#' @import ggplot2
#' @importFrom ggrepel geom_text_repel
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
    aggregate(as.formula(paste(score_name, "~", player_name)), data = plot_df, FUN = mean),
    aggregate(as.formula(paste("epp", "~", player_name)), data = plot_df, FUN = mean)
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
