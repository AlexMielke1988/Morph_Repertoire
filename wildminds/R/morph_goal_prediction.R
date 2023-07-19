#' Give R a prediction column and a target column and it will build a Naive Bayes classifier to predict the latter
#'
#'
#' @param predictor vector with the predictor (e.g., gesture actions)
#' @param target vector with the outcome variable
#' @param out how many folds should the prediction be repeated on
#' @param upsample should the dataset be upsample to make all targets equally likely in the training set
#'
#' @return accuracy of the naive bayes and random
#'
#' @importFrom dplyr mutate arrange desc as_tibble select all_of na_if left_join bind_rows bind_cols rename mutate_all
#' @importFrom stringr str_detect str_split str_replace_all str_to_title str_to_lower
#' @importFrom magrittr %>%
#' @importFrom tidyr replace_na gather separate unite
#' @importFrom rsample vfold_cv training testing
#' @importFrom parsnip naive_Bayes set_engine set_mode
#' @importFrom workflows workflowadd_model add_formula
#' @importFrom recipes step_naomit
#' @importFrom yardstick
#'
#' @export
#'


morph_goal_prediction <- function(predictor, target, out = 10, upsample = T) {

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
    naive_Bayes(Laplace = 0.1, engine = 'naivebayes') %>%
    set_mode("classification")

  nb_wflow <-
    workflow() %>%
    add_model(nb_model) %>%
    add_formula(target ~ predictor) %>%
    step_naomit()

  pred.accuracy.nb <-
    lapply(pred.data.kfold$splits, function(x) {
      x.train <- training(x)
      x.test <- testing(x)

      if (upsample){
        # Create recipe to preprocess data
        rec <- recipe(target ~ ., data = x.train) %>%
          themis::step_upsample(target, over_ratio = 1, skip = FALSE)

        # Fit the recipe to the training data
        x.train <- prep(rec) %>% juice() %>% data.frame()
      }


      x.fit <- fit(nb_wflow, x.train)
      return(list(
        accuracy = predict(x.fit, x.test) %>%
          bind_cols(target = x.test$target) %>%
          accuracy(target, .pred_class),
        by_goal = predict(x.fit, x.test) %>%
          bind_cols(target = x.test$target) %>%
          mutate(correct = .pred_class == target) %>%
          group_by(target) %>%
          summarise(mean = mean(correct)) %>%
          ungroup(),
        by_element =
          predict(x.fit, x.test) %>%
          bind_cols(target = x.test$target) %>%
          bind_cols(element = x.test$predictor) %>%
          mutate(correct = .pred_class == target) %>%
          group_by(element) %>%
          summarise(mean = mean(correct)) %>%
          ungroup()
      )
      )
    })

  pred.accuracy.random <-
    lapply(pred.data.kfold$splits, function(x) {
      x.train <- training(x)
      x.train$predictor <- sample(x.train$predictor)
      x.test <- testing(x)

      if(upsample){
        max.outcome <- (x.train$target %>% table() %>% max()) + 1
        add_rows <- sapply(unique(x.train$target), function(x){
          which.outcome <- which(x == x.train$target)
          return(sample(x = rep(which.outcome, 2),
                        size = (max.outcome - length(which.outcome)),
                        replace = TRUE))
        }) %>% unlist()
        add_input <- x.train[add_rows,] %>%
          data.frame()

        x.train <- rbind(x.train, add_input)
      }

      x.fit <- fit(nb_wflow, x.train)
      return(list(
        accuracy = predict(x.fit, x.test) %>%
          bind_cols(target = x.test$target) %>%
          accuracy(target, .pred_class),
        by_goal = predict(x.fit, x.test) %>%
          bind_cols(target = x.test$target) %>%
          mutate(correct = .pred_class == target) %>%
          group_by(target) %>%
          summarise(mean = mean(correct)) %>%
          ungroup(),
        by_element =
          predict(x.fit, x.test) %>%
          bind_cols(target = x.test$target) %>%
          bind_cols(element = x.test$predictor) %>%
          mutate(correct = .pred_class == target) %>%
          group_by(element) %>%
          summarise(mean = mean(correct)) %>%
          ungroup()
      )
      )
    })

  accuracy.nb <- bind_rows(purrr::transpose(pred.accuracy.nb)$accuracy)
  accuracy.random <- bind_rows(purrr::transpose(pred.accuracy.random)$accuracy)

  by_target.nb <-
    bind_rows(purrr::transpose(pred.accuracy.nb)$by_goal) %>%
    group_by(target) %>%
    summarise(mean = mean(mean)) %>%
    ungroup()

  by_element.nb <-
    bind_rows(purrr::transpose(pred.accuracy.nb)$by_element) %>%
    group_by(element) %>%
    summarise(mean = mean(mean)) %>%
    ungroup()

  if(upsample){
    max.outcome <- (pred.data$target %>% table() %>% max()) + 1
    add_rows <- sapply(unique(pred.data$target), function(x){
      which.outcome <- which(x == pred.data$target)
      return(sample(x = rep(which.outcome, 2),
                    size = (max.outcome - length(which.outcome)),
                    replace = TRUE))
    }) %>% unlist()
    add_input <- pred.data[add_rows,] %>%
      data.frame()

    pred.data <- rbind(pred.data, add_input)
  }

  x.plot <- fit(nb_wflow, pred.data) %>%
    predict(pred.data) %>%
    bind_cols(target = pred.data$target) %>%
    conf_mat(target, .pred_class) %>%
    autoplot(type = "heatmap") +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.6))

  return(
    list(
      result = data.frame(
        accuracy.nb = mean(accuracy.nb$.estimate),
        accuracy.random = mean(accuracy.random$.estimate)
      ),
      plot = x.plot,
      by_target = by_target.nb,
      by_element = by_element.nb
      ))
}
