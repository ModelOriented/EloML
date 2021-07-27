#' @title Plot EPP ranking
#'
#' @param epp object of class epp_results.
#' @param aggregation aggregation function.
#' @param confidence_intervals whether confidence intervals should be plotted. Only for "glmnet" estimation.
#' @import ggplot2
#' @importFrom ggrepel geom_text_repel
#'
#' @example
#' library(EloML)
#' data(auc_scores)
#' epp <- calculate_epp(
#'   auc_scores[1:400,],
#'   keep_columns=TRUE,
#'   keep_model = TRUE,
#'   estimation ="glm"
#'   )
#'
#' @export
plot_epp_ranking <- function(epp, aggregation = mean, confidence_intervals = FALSE){
  if(ncol(epp[["epp"]]) == 2) stop("Use `keep_columns == TRUE` parameter in function `calculate_epp()`")
  if(keep_model & confidence_intervals)  stop("Use `keep_model == TRUE` parameter in function `calculate_epp()`")
  if((epp[["estimation"]] != "glmnet") & confidence_intervals)  stop("Use `estimation == 'glmnet'` parameter in function `calculate_epp()`")

  player_name <- colnames(epp[["epp"]])[1]
  score_name <- colnames(epp[["epp"]])[3]
  plot_df <- epp[["epp"]][, c(player_name, score_name, "epp")]
  plot_df <- merge(aggregate(as.formula(paste(score_name, "~", player_name)), data = plot_df, FUN = mean),
  aggregate(as.formula(paste("epp", "~", player_name)), data = plot_df, FUN = mean))
  if(confidence_intervals){
    # conf <- summary(epp[["model"]])$coefficients[,2]
    # conf_df <- plot_df[,c("epp", "Mean", "model")] %>%
    #   unique() %>%
    #   mutate(sd = conf[model])
    # conf_df[is.na(conf_df$sd), "sd"] <- conf["(Intercept)"]
  }
  ggplot(plot_df, aes(x = epp, y = get(score_name))) +
    # geom_errorbarh(aes(xmin = epp-1.96 *sd, xmax=epp+1.96 *sd)) +
    geom_point(aes(x = epp, y = get(score_name)), size = 3) +
    geom_text_repel(aes(label = get(player_name)), color = "#4AC4B6", max.overlaps = 100) +
    theme_bw() +
    ylab("Aggregated Score") +
    xlab("EPP")
  }
