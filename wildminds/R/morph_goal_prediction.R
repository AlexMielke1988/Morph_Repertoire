#' Give R a prediction column and a target column and it will build a Naive Bayes classifier to predict the latter
#'
#'
#' @param predictor vector with the predictor (e.g., gesture actions)
#' @param target vector with the outcome variable
#' @param out how many folds should the prediction be repeated on
#'
#' @return accuracy of the naive bayes and random
#'
#' @importFrom dplyr mutate arrange desc as_tibble select all_of na_if left_join bind_rows bind_cols rename mutate_all
#' @importFrom stringr str_detect str_split str_replace_all str_to_title str_to_lower str_replace
#' @importFrom magrittr %>%
#' @importFrom tidyr replace_na gather separate unite
#' @importFrom rsample vfold_cv training testing
#' @importFrom parsnip naive_Bayes set_engine set_mode
#' @importFrom workflows workflow add_model add_formula
#' @importFrom recipes step_naomit
#'
#' @export
#'


morph_goal_prediction <- function(predictor, target, out = 10) {

  pred.data <- cbind(predictor = predictor,
                     target = target) %>%
    data.frame() %>%
    drop_na() %>%
    filter(!(target %in% c('Unknown', 'Other', 'Unclear'))) %>%
    filter(!(predictor %in% c('Unknown', 'Other', 'Unclear'))) %>%
    mutate(target = as.factor(target)) %>%
    mutate(predictor = as.factor(predictor))

  pred.data.kfold <- vfold_cv(pred.data, v = out)

  nb_model <-
    naive_Bayes(Laplace = 0.01) %>%
    set_engine("naivebayes") %>%
    set_mode("classification")

  nb_wflow <-
    workflow() %>%
    add_model(nb_model) %>%
    add_formula(target ~ predictor) %>%
    step_naomit()

  pred.accuracy.nb <- lapply(pred.data.kfold$splits, function(x) {
    x.train <- training(x)
    x.test <- testing(x)
    x.fit <- fit(nb_wflow, x.train)
    return(
      predict(x.fit, x.test) %>%
        bind_cols(target = x.test$target) %>%
        accuracy(target, .pred_class)
    )
  })

  pred.accuracy.random <-
    lapply(pred.data.kfold$splits, function(x) {
      x.train <- training(x)
      x.train$predictor <- sample(x.train$predictor)
      x.test <- testing(x)
      x.fit <- fit(nb_wflow, x.train)
      return(
        predict(x.fit, x.test) %>%
          bind_cols(target = x.test$target) %>%
          accuracy(target, .pred_class)
      )
    })

  pred.accuracy.nb <- bind_rows(pred.accuracy.nb)
  pred.accuracy.random <- bind_rows(pred.accuracy.random)

  x.plot <- fit(nb_wflow, pred.data) %>%
    predict(pred.data) %>%
    bind_cols(target = pred.data$target) %>%
    conf_mat(target, .pred_class) %>%
    autoplot(type = "heatmap") + theme(axis.text.x = element_text(angle = 45, vjust = 0.6))

  return(
    list(
      result = data.frame(
        accuracy.nb = mean(pred.accuracy.nb$.estimate),
        accuracy.random = mean(pred.accuracy.random$.estimate)
      ),
      plot = x.plot))
}
