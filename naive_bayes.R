naive_bayes_train <- function(train_data, target_name, prior_prob=NULL){
  if (!(target_name %in% colnames(train_data))){
    stop("No column with that name found")
  }
  if (is.null(prior_prob)){
    prior_prob <- table(train_data[target_name]) / nrow(train_data)
  }
  class_labels <- unique(train_data[[target_name]])
  numeric_data <- sapply(train_data[colnames(train_data)!=target_name], is.numeric)
  discrete_data <- !numeric_data
  mean_cd <- list()
  sd_cd <- list()
  meta_dd <- list()
  for (cl in class_labels){
    cl_cd <- train_data[train_data[target_name] == cl, names(numeric_data[numeric_data]), drop=FALSE]
    mean_cd[[as.character(cl)]] <- apply(cl_cd, 2, mean)
    sd_cd[[as.character(cl)]] <- apply(cl_cd, 2, sd)
    cl_dd <- train_data[train_data[target_name] == cl, names(discrete_data[discrete_data]), drop=FALSE]
    meta_dd[[as.character(cl)]] <- list(count = nrow(cl_dd), table = lapply(cl_dd, table))
  }
  return(list(numeric_data=names(numeric_data[numeric_data]),
              discrete_data=names(discrete_data[discrete_data]),
              prior_prob=prior_prob, class_labels=class_labels,
              mean=mean_cd, sd=sd_cd, meta_dd=meta_dd,
              unique=lapply(lapply(train_data[names(discrete_data[discrete_data])], unique), length))
  )
}

naive_bayes_predict <- function(test_data, nb_model, alpha=0){
  numeric_data <- nb_model$numeric_data
  discrete_data <- nb_model$discrete_data
  prior_prob <- nb_model$prior_prob
  class_labels <- nb_model$class_labels
  meta_dd <- nb_model$meta_dd
  class_probability <- function(x, label, prior_prob) {
    result <- log(prior_prob)
    for (i in numeric_data) {
      mean <- nb_model$mean[[as.character(label)]][i]
      sd <- nb_model$sd[[as.character(label)]][i]
      result <- result + dnorm(x[[i]], mean, sd, log=TRUE)
    }
    for (i in discrete_data) {
      w <- meta_dd[[as.character(label)]]$table[[i]][x[[i]]]
      if (is.null(w)) w <- 0
      W <- meta_dd[[as.character(label)]]$count
      result <- result + log(
        (w + alpha)
          /
          (W + alpha*nb_model$unique[[i]])
      )
    }
    return(result)
  }
  predictions <- sapply(1:nrow(test_data),
                        function(i) {
                          probabilities <-  sapply(class_labels,
                                                  function(label) class_probability(test_data[i, ], label, prior_prob[[as.character(label)]])
                          )
                          class_labels[which.max(probabilities)]
                        }
  )
  return(predictions)
}