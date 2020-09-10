# Set the directory of the folder

#Load data from Step 1
load("./Data/datamods1.RData")

# Set seed to ensure that the results are reproducible
set.seed(1234)

# Required packages
req.packages <-
  c(
    "caret",
    "dplyr",
    "doParallel",
    "leaps",
    "tidyverse",
    "scales",
    "microbenchmark",
    "relgam",
    "bigstep"
    
  )
new.packages <-
  req.packages[!(req.packages %in% installed.packages()[, "Package"])]
if (length(new.packages))
  install.packages(new.packages)
invisible(lapply(req.packages, library, character.only = TRUE))

# 1. Creation of the partition of test and train. Last 365 days as --------


year_day <- max(data_mods$date) - 365
pos_year <- which(data_mods$date == year_day)

training.samples <- seq(1:(pos_year - 1))
train.data <- data_mods[training.samples,-1]
test.data <- data_mods[-training.samples,-1]


# 2. Preprocess of the sets --------------------------------------------------

# Transformations and standardization
preProcValues <-
  preProcess(train.data, method = c("YeoJohnson", "scale", "center"))
train.data.pp <- predict(preProcValues, train.data)
test.data.pp <- predict(preProcValues, test.data)

# Remove dependencies for modellable
comboInfo_mods <- findLinearCombos(train.data.pp)
train.data.pp <- train.data.pp[,-comboInfo_mods$remove]
test.data.pp <- test.data.pp[,-comboInfo_mods$remove]

#Definition of the sample
non_sample <-
  seq(2, dim(data_nomods)[2]) # In this case, we are running the whole set

# Create list for the variables
#names.vars <- colnames(data_nomods[,-1])
vars.ls <- sapply(as.character(non_sample), function(x)
  NULL)

# Create list for the models
name.models.ls <- paste0('mod.', seq(1:17))
models.ls <- sapply(name.models.ls, function(x)
  NULL)

# Create list for the models inside each variable
vars.ls <- lapply(vars.ls, function(x) {
  x = models.ls
})

# Results data frame per model
results.model  <- data.frame(non_sample)
results.model$RMSE <- NA
results.model$Rsquared <- NA
results.model$MAE <- NA


# 3. Development of the models -----------------------------------------------
# Model 16: Reluctant Generalized Additive Models with filter ----------------------------------------------------
#Load function for relgam fixed
load("./Data/function_relgam.RData")

# Function model 16
fn_model16 <- function(i, nomod_n, mod_n, data_nomods, non_sample) {
  set.seed(1234)
  # Create risk factor matrix
  fr_m <-
    data_nomods[, non_sample[i], drop = FALSE] # extract one of non-modellability
  fr_m.train.base <- fr_m[training.samples, , drop = FALSE]
  fr_m.test.base <- fr_m[-training.samples, , drop = FALSE]
  
  # Change to character
  nomod_n <-
    data.frame(lapply(nomod_n, as.character), stringsAsFactors = FALSE)
  mod_n <-
    data.frame(lapply(mod_n, as.character), stringsAsFactors = FALSE)
  
  #Subset of currency
  cur.nomod <-
    as.character(nomod_n[nomod_n$fr == names(fr_m)[1], match('currency', names(nomod_n))])
  cur.subset <- as.character(mod_n[mod_n$currency == cur.nomod, ]$fr)
  
  # Keep running only if there is more than one other "non-modellable' risk factor with the same currency.
  if (length(cur.subset) != 0) {
    train.data.type.pp <-
      train.data.pp[, names(train.data.pp) %in% cur.subset]
    test.data.type.pp <-
      test.data.pp[, names(test.data.pp) %in% cur.subset]
    
    #Create matrix, the first column is the "non-modellable" risk factor and the other the set of "modellable"
    fr_m.train <- cbind(fr_m.train.base, train.data.type.pp)
    fr_m.test <- cbind(fr_m.test.base, test.data.type.pp)
    
    x = as.matrix(fr_m.train[, -1])
    y = as.matrix(fr_m.train[, 1])
    
    #Definition of the model

    # Parallelization
    cores <- floor(0.8 * detectCores())
    cl <- makeCluster(cores)
    
    registerDoParallel(cl)
    
    model.16 <-  cv.rgam.ed(
      x,
      y,
      gamma = 0.6,
      nfolds = 5,
      init_nz = 1:ncol(x),
      parallel = TRUE
    )
    stopCluster(cl)
    
    
    # Results collection for the model for each non-modellable variable
    results.ls <- list()
    
    #Keeo coefficients from the resulting model
    results.ls[['var_coef']] <-
      model.16$glmfit[["full_glmfit"]][["beta"]][, which(model.16$glmfit$lambda ==
                                                           model.16$lambda.min)] %>% as.matrix() %>% as.data.frame() %>%
      rownames_to_column()
    # Keep best tune parameters
    results.ls[['best_tune_parameters']] <- model.16$lambda.min
    #Keep linear variables
    results.ls[['var_linear']] <-
      model.16$glmfit$linfeat[[which(model.16$glmfit$lambda == model.16$lambda.min)]]
    # Kepp non-linear variables
    results.ls[['var_nonlinear']] <-
      model.16$glmfit$nonlinfeat[[which(model.16$glmfit$lambda == model.16$lambda.min)]]
    # Keep selected variables
    results.ls[['var_select']] <- cur.subset
    
    # Make predictions
    pred.ls <- list()
    predictions <-
      model.16 %>% predict(as.matrix(fr_m.test[, -1]), s = "lambda.min")
    res.pred <-
      postResample(pred = predictions, obs = fr_m.test[, 1])
    
    pred.ls[['predictions']] <- predictions
    pred.ls[['predictions.res']] <- res.pred
    
  } else {
    results.ls <- NA
    pred.ls <- NA
  }
  # List to keep results
  resultsfn <-
    list(
      iteration = i,
      fr = names(fr_m),
      results.ls = results.ls,
      pred.ls = pred.ls
    )
  return(resultsfn)
}



# Paralellization 
# Run on a sample of variables to test the configuration of the packages
############################### TEST ###################################
start.t <- Sys.time()
cores <- floor(0.8 * detectCores())
#cores <- detectCores()

cl <- makeCluster(cores)
registerDoParallel(cl)
results_fn_m16.0 <-
  foreach(
    i = 1:3,
    .packages = c('dplyr', 'caret', 'tidyverse', 'doParallel', 'relgam'),
    .errorhandling = 'pass'
  ) %dopar%
  {
    cat(paste("Iteration", i, "\n"),
        file = "m16log0.txt",
        append = TRUE)
    fn_model16(i, nomod_n, mod_n, data_nomods, non_sample)
  }
stopCluster(cl)

end.t <- Sys.time()
time_elapsed.fn.16.0 <- end.t - start.t

# Check to see that the model runs without any problem
# If there is an error uncomment the next line
# results_fn_m16.0[[1]]$message

results_fn_m16.0[[1]]$iteration
results_fn_m16.0[[2]]$iteration
results_fn_m16.0[[3]]$iteration

results_fn_m16.0[[1]]$pred.ls$predictions.res
results_fn_m16.0[[2]]$pred.ls$predictions.res
results_fn_m16.0[[3]]$pred.ls$predictions.res

# Check that results are saved in the correct directory
save.image("./Results/Model16_0.RData")

############################### RUN AFTER TEST ###################################
start.t <- Sys.time()
cores <- floor(0.8 * detectCores())
#cores <- detectCores()

cl <- makeCluster(cores)
registerDoParallel(cl)
results_fn_m16 <-
  foreach(
    i = 1:length(non_sample),
    .packages = c('dplyr', 'caret', 'tidyverse', 'doParallel', 'relgam'),
    .errorhandling = 'pass'
  ) %dopar%
  {
    cat(paste("Iteration", i, "\n"),
        file = "m16log.txt",
        append = TRUE)
    fn_model16(i, nomod_n, mod_n, data_nomods, non_sample)
  }
stopCluster(cl)

end.t <- Sys.time()
time_elapsed.16 <- end.t - start.t

# Save check of the results of the model
save.image("./Results/Model16.RData")

# Model 4: Subset of curves by currency----------------------------------------------------------
# Function model 4 
fn_model4 <- function(i, nomod_n, mod_n, data_nomods, non_sample) {
  set.seed(1234)
  
  # Create risk factor matrix
  fr_m <-
    data_nomods[, non_sample[i], drop = FALSE] # extract one of non-modellability
  fr_m.train.base <- fr_m[training.samples, , drop = FALSE]
  fr_m.test.base <- fr_m[-training.samples, , drop = FALSE]
  
  #Change to character
  nomod_n <-
    data.frame(lapply(nomod_n, as.character), stringsAsFactors = FALSE)
  mod_n <-
    data.frame(lapply(mod_n, as.character), stringsAsFactors = FALSE)
  
  # Subset based on currency
  cur.nomod <-
    as.character(nomod_n[nomod_n$fr == names(fr_m)[1], match('currency', names(nomod_n))])
  cur.subset <- as.character(mod_n[mod_n$currency == cur.nomod, ]$fr)
  
  # If there exists a set of other "non-modellable" risk factors with that currency 
  if (length(cur.subset) != 0) {
    train.data.type.pp <-
      train.data.pp[, names(train.data.pp) %in% cur.subset]
    test.data.type.pp <-
      test.data.pp[, names(test.data.pp) %in% cur.subset]
    
    #Create matrix, the first column is the "non-modellable" risk factor and the others, the set of "modellable" risk factors
    fr_m.train <- cbind(fr_m.train.base, train.data.type.pp)
    fr_m.test <- cbind(fr_m.test.base, test.data.type.pp)
    
    #Definition of the model
    # Definition of th training control to search for the optimal hyperparameters
    train.control <- trainControl(method = "cv", number = 10)
    
    # Parallelization
    cores <- floor(0.8 * detectCores())
    cl <- makeCluster(cores)
    
    registerDoParallel(cl)
    
    model.4 <- train(
      x = fr_m.train[, -1],
      y = fr_m.train[, 1],
      data = fr_m.train,
      method = "leapSeq",
      trControl = train.control
    )
    stopCluster(cl)
    
    
    # Results collection for the model for each non-modellable variable
    results.ls <- list()
    
    # Keep the coefficients of the selected model
    results.ls[['var_coef']] <-
      coef(model.4$finalModel, as.numeric(model.4$bestTune)) %>%  as.data.frame() %>%
      rownames_to_column()
    # Keep best hyperparameters
    results.ls[['best_tune_parameters']] <- model.4$bestTune
    
    
    # Make predictions
    pred.ls <- list()
    predictions <- model.4 %>% predict(fr_m.test)
    
    # Obtain the metrics for measuring performance
    res.pred <-
      postResample(pred = predictions, obs = fr_m.test[, 1])
    
    pred.ls[['predictions']] <- predictions
    pred.ls[['predictions.res']] <- res.pred
    
  } else {
    results.ls <- NA
    pred.ls <- NA
  }
  #List of results
  resultsfn <-
    list(
      iteration = i,
      fr = names(fr_m),
      results.ls = results.ls,
      pred.ls = pred.ls
    )
  return(resultsfn)
}


# Paralellization ---------------------------------------------------------
# Run on a sample of variables to test the configuration of the packages
############################### TEST ###################################
start.t <- Sys.time()
cores <- floor(0.8 * detectCores())
#cores <- detectCores()

cl <- makeCluster(cores)
registerDoParallel(cl)
results_fn_m4.0 <-
  foreach(
    i = 1:3,
    .packages = c('dplyr', 'caret', 'tidyverse', 'doParallel'),
    .errorhandling = 'pass'
  ) %dopar%
  {
    cat(paste("Iteration", i, "\n"),
        file = "m4log0.txt",
        append = TRUE)
    fn_model4(i, nomod_n, mod_n, data_nomods, non_sample)
  }
stopCluster(cl)

end.t <- Sys.time()
time_elapsed.fn.4.0 <- end.t - start.t

# Check to see that the model runs without any problem
# If there is an error uncomment the next line
# results_fn_m4.0[[1]]$message

results_fn_m4.0[[1]]$iteration
results_fn_m4.0[[2]]$iteration
results_fn_m4.0[[3]]$iteration

results_fn_m4.0[[1]]$pred.ls$predictions.res
results_fn_m4.0[[2]]$pred.ls$predictions.res
results_fn_m4.0[[3]]$pred.ls$predictions.res

# Check save
save.image("./Results/Model4_0.RData")
############################### RUN AFTER TEST ###################################
start.t <- Sys.time()
cores <- floor(0.8 * detectCores())
#cores <- detectCores()

cl <- makeCluster(cores)
registerDoParallel(cl)
results_fn_m4 <-
  foreach(
    i = 1:length(non_sample),
    .packages = c('dplyr', 'caret', 'tidyverse', 'doParallel'),
    .errorhandling = 'pass'
  ) %dopar%
  {
    cat(paste("Iteration", i, "\n"),
        file = "m4log.txt",
        append = TRUE)
    fn_model4(i, nomod_n, mod_n, data_nomods, non_sample)
  }
stopCluster(cl)

end.t <- Sys.time()
time_elapsed.4<- end.t - start.t

# Save results
save.image("./Results/Model4.RData")


# Function model 3 -------------------------------------------------------
fn_model3 <- function(i, nomod_n, mod_n, data_nomods, non_sample) {
  set.seed(1234)
  
  # Create matrix of risk factors
  fr_m <-
    data_nomods[, non_sample[i], drop = FALSE] # extract one of non-modellability
  fr_m.train.base <- fr_m[training.samples, , drop = FALSE]
  fr_m.test.base <- fr_m[-training.samples, , drop = FALSE]
  
  nomod_n <-
    data.frame(lapply(nomod_n, as.character), stringsAsFactors = FALSE)
  mod_n <-
    data.frame(lapply(mod_n, as.character), stringsAsFactors = FALSE)
  
  # Filter based on the types of curves 
  type.nomod <-
    as.character(nomod_n[nomod_n$fr == names(fr_m)[1], match('type', names(nomod_n))])
  type.subset <- as.character(mod_n[mod_n$type == type.nomod, ]$fr)
  
  if (length(cur.subset) != 0) {
    train.data.type.pp <-
      train.data.pp[, names(train.data.pp) %in% type.subset]
    test.data.type.pp <-
      test.data.pp[, names(test.data.pp) %in% type.subset]
    
    #Create matrix, the first column is the "non-modellable" risk factor and the others, the set of "modellable" risk factors
    fr_m.train <- cbind(fr_m.train.base, train.data.type.pp)
    fr_m.test <- cbind(fr_m.test.base, test.data.type.pp)
    
    
    # Definition of the model
    # Definition of the training method for hyperparameter search
    train.control <- trainControl(method = "cv", number = 10)
    
    # Parallelization
    cores <- floor(0.8 * detectCores())
    cl <- makeCluster(cores)
    
    registerDoParallel(cl)
    
    model.3 <- train(
      x = fr_m.train[, -1],
      y = fr_m.train[, 1],
      data = fr_m.train,
      method = "leapSeq",
      trControl = train.control
    )
    stopCluster(cl)
    
    
    # Results collection for the model for each non-modellable variable
    results.ls <- list()
    
    results.ls[['var_coef']] <-
      coef(model.3$finalModel, as.numeric(model.3$bestTune)) %>%  as.data.frame() %>%
      rownames_to_column()
    results.ls[['best_tune_parameters']] <- model.3$bestTune
    
    
    # Make predictions
    pred.ls <- list()
    predictions <- model.3 %>% predict(fr_m.test)
    
    res.pred <-
      postResample(pred = predictions, obs = fr_m.test[, 1])
    
    pred.ls[['predictions']] <- predictions
    pred.ls[['predictions.res']] <- res.pred
    
  } else {
    results.ls <- NA
    pred.ls <- NA
  }
  resultsfn <-
    list(
      iteration = i,
      fr = names(fr_m),
      results.ls = results.ls,
      pred.ls = pred.ls
    )
  return(resultsfn)
}



# Paralellization ---------------------------------------------------------
# Run on a sample of variables to test the configuration of the packages
############################### TEST ###################################
start.t <- Sys.time()
cores <- floor(0.8 * detectCores())
#cores <- detectCores()

cl <- makeCluster(cores)
registerDoParallel(cl)
results_fn_m3.0 <-
  foreach(
    i = 1:3,
    .packages = c('dplyr', 'caret', 'tidyverse', 'doParallel'),
    .errorhandling = 'pass'
  ) %dopar%
  {
    cat(paste("Iteration", i, "\n"),
        file = "m3log0.txt",
        append = TRUE)
    fn_model3(i, nomod_n, mod_n, data_nomods, non_sample)
  }
stopCluster(cl)

end.t <- Sys.time()
time_elapsed.fn.3.0 <- end.t - start.t

# Check to see that the model runs without any problem
# If there is an error uncomment the next line
# results_fn_m3.0[[1]]$message

results_fn_m3.0[[1]]$iteration
results_fn_m3.0[[2]]$iteration
results_fn_m3.0[[3]]$iteration

results_fn_m3.0[[1]]$pred.ls$predictions.res
results_fn_m3.0[[2]]$pred.ls$predictions.res
results_fn_m3.0[[3]]$pred.ls$predictions.res

# Check save
save.image("./Results/Model3_0.RData")
############################### RUN AFTER TEST ###################################
start.t <- Sys.time()
cores <- floor(0.8 * detectCores())
#cores <- detectCores()

cl <- makeCluster(cores)
registerDoParallel(cl)
results_fn_m3 <-
  foreach(
    i = 1:length(non_sample),
    .packages = c('dplyr', 'caret', 'tidyverse', 'doParallel'),
    .errorhandling = 'pass'
  ) %dopar%
  {
    cat(paste("Iteration", i, "\n"),
        file = "m3log.txt",
        append = TRUE)
    fn_model3(i, nomod_n, mod_n, data_nomods, non_sample)
  }
stopCluster(cl)

end.t <- Sys.time()
time_elapsed.3 <- end.t - start.t

save.image("./Results/Model3.RData")

# Model 7: wrapper stepwise for big data ----------------------------------------------------------
# Function model 7 --------------------------------------------------------

fn_model7 <- function(i, nomod_n, mod_n, data_nomods, non_sample) {
  set.seed(1234)
  
  #problem with i 2705
  # Create factor risk
  fr_m <- data_nomods[, non_sample[i], drop = FALSE]
  fr_m.train.base <- fr_m[training.samples, , drop = FALSE]
  fr_m.test.base <- fr_m[-training.samples, , drop = FALSE]
  
  #Create matrix, the first column is the "non-modellable" risk factor and the others, the set of "modellable" risk factors
  fr_m.train <- cbind(fr_m.train.base, train.data.pp)
  fr_m.test <- cbind(fr_m.test.base, test.data.pp)
  
  # For big data is recommended to reduce the variables that are not correlated  and then perform BIC
  y <- fr_m.train[, 1]
  X <- fr_m.train[, -1]
  
  # We need to prepare the data first and convert it to a big class object
  data <- prepare_data(y, X)
  
  # Explore the best combination of the criteria
  options <- c('maic', 'mbic')
  combinations <-
    data.frame(lapply(expand.grid(options, options, options), as.character), stringsAsFactors =
                 FALSE)
  df.results <-
    setNames(
      data.frame(matrix(
        ncol = 6, nrow = dim(combinations)[1]
      )),
      c(
        'Combination',
        'R2',
        'ADJR2',
        'NVAR',
        'NVAR.P0.05',
        'P.VALUE.F'
      )
    )
  list.results <-
    vector(mode = "list", length = dim(combinations)[1])
  
  for (j in 1:dim(combinations)[1]) {
    data %>%
      reduce_matrix(minpv = 0.01) %>% #Reducing number of variables
      fast_forward(crit = combinations[j, 1]) %>% # Add variables to a model as long as they reduce the given criterion.
      multi_backward(crit = combinations[j, 2]) %>% #Remove the worst variables from a model as long as they
      #reduce the given criterion (backward  elimination). Tested with other but results look the same
      stepwise(crit = combinations[j, 3]) ->  resultsm #Stepwise
    
    # If the first threashold is too small and it is reducing the variables to zero, then new threashold
    if (is_empty(resultsm$candidates)) {
      data %>%
        reduce_matrix(minpv = 0.05) %>% #Reducing number of variables
        fast_forward(crit = combinations[j, 1]) %>% # Add variables to a model as long as they reduce the given criterion.
        multi_backward(crit = combinations[j, 2]) %>% #Remove the worst variables from a model as long as they
        #reduce the given criterion (backward  elimination). Tested with other but results look the same
        stepwise(crit = combinations[j, 3]) ->  resultsm #Stepwise
    }
    
    #If just with the reduction the model is done
    if (is_empty(resultsm$model)) {
      vars_selected_7 <- names(fr_m.train[, -1])[resultsm$candidates]
    } else{
      #The direct output of the model can not be extract because there could be repeated variables taking into account given that the function does not take them
      vars_selected_7 <- unique(resultsm$model)
    }
    
    # Definition of the formula of the model
    f <-
      formula(paste0(
        names(fr_m.train)[1],
        '~',
        paste(vars_selected_7, collapse = " + ")
      ))
    
    # Paralellization
    cores <- floor(0.8 * detectCores())
    #cores <- detectCores()
    
    cl <- makeCluster(cores)
    registerDoParallel(cl)
    resultsm.7 <- train(f,
                        data = fr_m.train,
                        method = "lm")
    stopCluster(cl)
    
    # Significance of the varaibles of the models
    if (length(summary(resultsm.7)$coef[-1, 4] <= .05) > 1) {
      df.results[j, 5] <-
        dim(data.frame(summary(resultsm.7)$coef[-1, ][summary(resultsm.7)$coef[-1, 4] <= .05, 4]))[1]
    } else if (summary(resultsm.7)$coef[1, 4] >= .05) {
      df.results[j, 5] <- 0
    } else {
      df.results[j, 5] <- 1
    }
    
    # F-test
    f <- summary(resultsm.7)$fstatistic
    df.results[j, 6] <-
      pf(f["value"], f["numdf"], f["dendf"], lower = FALSE)
    df.results[j, 4] <- dim(summary(resultsm.7)$coef)[1] - 1
    df.results[j, 3] <- summary(resultsm.7)$adj.r.squared
    df.results[j, 2] <- summary(resultsm.7)$r.squared
    df.results[j, 1] <-
      apply(combinations[j, ], 1, paste, collapse = ' ')
    list.results[[j]] <- resultsm.7
  }
  
  #All significant
  df.results$Significant <-
    ifelse(df.results$NVAR == df.results$NVAR.P0.05 &
             df.results$NVAR != 0,
           1,
           0)
  #p value correspondant to F test
  df.F.results <- df.results[df.results$P.VALUE.F <= .05, ]
  
  #If there is more than one in df.F.results, then the max adjusted R is check
  if (dim(df.F.results)[1] > 1) {
    df.max.results <-
      df.results[df.results$ADJR2 == max(df.results$ADJR2) &
                   df.results$P.VALUE.F <= .05, ]
    
    #If there is more than one in df.max.results
    if (dim(df.max.results)[1] > 1) {
      if (dim(df.max.results[which(df.max.results$Significant == 1), ])[1] > 1) {
        comb <- df.max.results[which(df.max.results$Significant == 1)[1], 1]
      } else if (dim(df.max.results[df.max.results$Significant == 1, ])[1] ==
                 0) {
        comb <- df.max.results[which.max(df.max.results$NVAR.P0.05), 1]
      } else{
        comb <-
          df.max.results[df.max.results[df.max.results$Significant == 1, ], 1]
      }
      #If there is more than one in df.max.results
    } else if (dim(df.max.results)[1] == 1) {
      comb <- df.max.results[1, 1]
      #If there is none df.max.results
    } else {
      comb <- df.results[runif(1, seq(1:8)), 1]
    }
    
    
    #If there is one in df.F.results
  } else if (dim(df.F.results)[1] == 1) {
    comb <- df.F.results[1, 1]
    #If there is none in df.F.results just gets anything
  } else{
    comb <- df.results[runif(1, seq(1:8)), 1]
  }
  
  names(list.results) <- df.results$Combination
  best.comb <- strsplit(comb, '\\s+')
  best.model <- list.results[names(list.results) == comb]
  
  #Final model
  vars_selected <- colnames(best.model[[1]]$finalModel$model)
  
  if (length(vars_selected) != 1) {
    # Results collection for the model for each non-modellable variable
    results.ls <- list()
    
    results.ls[['var_coef']] <-
      coef(best.model[[1]]$finalModel,
           as.numeric(best.model[[1]]$bestTune)) %>%  as.data.frame() %>%
      rownames_to_column()
    #results.ls[['tune_parameters']] <- model.1$results
    results.ls[['best_tune_parameters']] <-
      best.model[[1]]$bestTune
    
    # Make predictions
    pred.ls <- list()
    
    # Make predictions
    predictions <- best.model[[1]] %>% predict(fr_m.test)
    
    res.pred <-
      postResample(pred = predictions, obs = fr_m.test[, 1])
    
    pred.ls[['predictions']] <- predictions
    pred.ls[['predictions.res']] <- res.pred
    
  } else {
    results.ls <- NA
    pred.ls <- NA
  }
  #List of the results
  resultsfn <-
    list(
      iteration = i,
      fr = names(fr_m),
      results.ls = results.ls,
      pred.ls = pred.ls
    )
  return(resultsfn)
}

# Paralellization ---------------------------------------------------------
# Run on a sample of variables to test the configuration of the packages
############################### TEST ###################################
start.t <- Sys.time()
cores <- floor(0.8 * detectCores())
#cores <- detectCores()

cl <- makeCluster(cores)
registerDoParallel(cl)
results_fn_m7.0 <-
  foreach(
    i = 1:3,
    .packages = c('bigstep', 'dplyr', 'caret', 'tidyverse', 'doParallel'),
    .errorhandling = 'pass'
  ) %dopar%
  {
    cat(paste("Iteration", i, "\n"),
        file = "m7log0.txt",
        append = TRUE)
    fn_model7(i, nomod_n, mod_n, data_nomods, non_sample)
  }
stopCluster(cl)

end.t <- Sys.time()
time_elapsed.fn.7.0 <- end.t - start.t

# Check to see that the model runs without any problem
# If there is an error uncomment the next line
# results_fn_m7.0[[1]]$message

results_fn_m7.0[[1]]$iteration
results_fn_m7.0[[2]]$iteration
results_fn_m7.0[[3]]$iteration

results_fn_m7.0[[1]]$pred.ls$predictions.res
results_fn_m7.0[[2]]$pred.ls$predictions.res
results_fn_m7.0[[3]]$pred.ls$predictions.res

# Check save
save.image("./Results/Model7_0.RData")

############################### RUN AFTER TEST ###################################
start.t <- Sys.time()
cores <- floor(0.8 * detectCores())
#cores <- detectCores()

cl <- makeCluster(cores)
registerDoParallel(cl)
results_fn_m7 <-
  foreach(
    i = 1:length(non_sample),
    .packages = c('bigstep', 'dplyr', 'caret', 'tidyverse', 'doParallel'),
    .errorhandling = 'pass'
  ) %dopar%
  {
    cat(paste("Iteration", i, "\n"),
        file = "m7log.txt",
        append = TRUE)
    fn_model7(i, nomod_n, mod_n, data_nomods, non_sample)
  }
stopCluster(cl)

end.t <- Sys.time()
time_elapsed.7 <- end.t - start.t

save.image("./Results/Model7.RData")


# Option 6: Subset of curves by term ----------------------------------------------------------
# Function model 6 --------------------------------------------------------

fn_model6 <- function(i, nomod_n, mod_n, data_nomods, non_sample) {
  set.seed(1234)
  
  # Extract one RF from the "non-modellable" matrix
  fr_m <-
    data_nomods[, non_sample[i], drop = FALSE] # extract one of non-modellability
  # Subset based on term
  term.nomod <-
    as.character(nomod_n[nomod_n$fr == names(fr_m)[1], match('term', names(nomod_n))])
  term.subset <- as.character(mod_n[mod_n$term == term.nomod, ]$fr)
  
  if (length(term.subset) != 0) {
    # Create factor risk matrix
    fr_m.train.base <- fr_m[training.samples, , drop = FALSE]
    fr_m.test.base <- fr_m[-training.samples, , drop = FALSE]
    
    nomod_n <-
      data.frame(lapply(nomod_n, as.character), stringsAsFactors = FALSE)
    mod_n <-
      data.frame(lapply(mod_n, as.character), stringsAsFactors = FALSE)
    
    
    train.data.type.pp <-
      train.data.pp[, names(train.data.pp) %in% term.subset]
    test.data.type.pp <-
      test.data.pp[, names(test.data.pp) %in% term.subset]
    
    #Create matrix, the first column is the "non-modellable" risk factor and the others, the set of "modellable" risk factors
    fr_m.train <- cbind(fr_m.train.base, train.data.type.pp)
    fr_m.test <- cbind(fr_m.test.base, test.data.type.pp)
    
    train.control <- trainControl(method = "cv", number = 10)
    
    # Parallelization
    cores <- floor(0.8 * detectCores())
    cl <- makeCluster(cores)
    
    registerDoParallel(cl)
    
    model.6 <- train(
      x = fr_m.train[, -1],
      y = fr_m.train[, 1],
      data = fr_m.train,
      method = "leapSeq",
      trControl = train.control
    )
    stopCluster(cl)
    
    
    # Results collection for the model for each non-modellable variable
    results.ls <- list()
    
    results.ls[['var_coef']] <-
      coef(model.6$finalModel, as.numeric(model.6$bestTune)) %>%  as.data.frame() %>%
      rownames_to_column()
    #results.ls[['tune_parameters']] <- model.1$results
    results.ls[['best_tune_parameters']] <- model.6$bestTune
    
    
    # Make predictions
    pred.ls <- list()
    predictions <- model.6 %>% predict(fr_m.test)
    
    res.pred <-
      postResample(pred = predictions, obs = fr_m.test[, 1])
    
    pred.ls[['predictions']] <- predictions
    pred.ls[['predictions.res']] <- res.pred
    
    
  } else {
    results.ls <- NA
    pred.ls <- NA
  }
  resultsfn <-
    list(
      iteration = i,
      fr = names(fr_m),
      results.ls = results.ls,
      pred.ls = pred.ls
    )
  return(resultsfn)
}

# Paralellization ---------------------------------------------------------
# Run on a sample of variables to test the configuration of the packages
############################### TEST ###################################
start.t <- Sys.time()
cores <- floor(0.8 * detectCores())
#cores <- detectCores()

cl <- makeCluster(cores)
registerDoParallel(cl)
results_fn_m6.0 <-
  foreach(
    i = 1:3,
    .packages = c('dplyr', 'caret', 'tidyverse', 'doParallel'),
    .errorhandling = 'pass'
  ) %dopar%
  {
    cat(paste("Iteration", i, "\n"),
        file = "m6log0.txt",
        append = TRUE)
    fn_model6(i, nomod_n, mod_n, data_nomods, non_sample)
  }
stopCluster(cl)

end.t <- Sys.time()
time_elapsed.fn.6.0 <- end.t - start.t

# Check to see that the model runs without any problem
# If there is an error uncomment the next line
# results_fn_m6.0[[1]]$message
results_fn_m6.0[[1]]$iteration
results_fn_m6.0[[2]]$iteration
results_fn_m6.0[[3]]$iteration

results_fn_m6.0[[1]]$pred.ls$predictions.res
results_fn_m6.0[[2]]$pred.ls$predictions.res
results_fn_m6.0[[3]]$pred.ls$predictions.res

# Check save
save.image("./Results/Model6_0.RData")

############################### RUN AFTER TEST ###################################
start.t <- Sys.time()
cores <- floor(0.8 * detectCores())

cl <- makeCluster(cores)
registerDoParallel(cl)
results_fn_m6 <-
  foreach(
    i = 1:500,
    .packages = c('dplyr', 'caret', 'tidyverse', 'doParallel'),
    .errorhandling = 'pass'
  ) %dopar%
  {
    cat(paste("Iteration", i, "\n"),
        file = "m6log.txt",
        append = TRUE)
    fn_model6(i, nomod_n, mod_n, data_nomods, non_sample)
  }
stopCluster(cl)

end.t <- Sys.time()
time_elapsed.6 <- end.t - start.t


# 4. Compilation of results --------------------------------------------------
# Function to build just one list with all the results

fn_dataframe <- function(list, data.results, list.results, model){
  for (i in 1:length(list)){
    it <- list[[i]]$iteration
    if (!is.na(list[[i]]$pred.ls)){
      data.results[it,2] <- list[[i]]$pred.ls$predictions.res[[1]]
      data.results[it,3] <- list[[i]]$pred.ls$predictions.res[[2]]
      data.results[it,4] <- list[[i]]$pred.ls$predictions.res[[3]]
    } else{
      data.results[it,2] <- NA 
      data.results[it,3] <- NA 
      data.results[it,4] <- NA 
    }
    #List vars
    list.results[[it]][[model]][['results']] <- list[[i]]$results.ls
    list.results[[it]][[model]][['predictions']] <- list[[i]]$pred.ls
  }
  dataframe <- list(data.results=data.results,list.results=list.results)
  return(dataframe)
}

# For each model run the function to create the vars.ls list
re_vars.ls <- vars.ls

# Model 3
# Empty data frame to export the results for each variable
results.model.3 <- results.model
resultsdata <-fn_dataframe(results_fn_m3, results.model.3, re_vars.ls, model=as.numeric(3))
results.model.3 <-resultsdata[[1]]
re_vars.ls <-resultsdata[[2]]


# Model 4
# Empty data frame to export the results for each variable
results.model.4 <- results.model
resultsdata <-fn_dataframe(results_fn_m4, results.model.4, re_vars.ls, model=as.numeric(4))
results.model.4 <-resultsdata[[1]]
re_vars.ls <-resultsdata[[2]]

# Model 5
# Empty data frame to export the results for each variable
results.model.5 <- results.model
resultsdata <-fn_dataframe(results_fn_m5, results.model.5, re_vars.ls, model=as.numeric(5))
results.model.5 <-resultsdata[[1]]
re_vars.ls <-resultsdata[[2]]

# Model 6
# Empty data frame to export the results for each variable
results.model.6 <- results.model
resultsdata <-fn_dataframe(results_fn_m6, results.model.6, re_vars.ls, model=as.numeric(6))
results.model.6 <-resultsdata[[1]]
re_vars.ls <-resultsdata[[2]]

# Model 7
# Empty data frame to export the results for each variable
results.model.7 <- results.model
resultsdata <-fn_dataframe(results_fn_m7, results.model.7, re_vars.ls, model=as.numeric(7))
results.model.7 <-resultsdata[[1]]
re_vars.ls <-resultsdata[[2]]

# Model 16
# Empty data frame to export the results for each variable
results.model.16 <- results.model
resultsdata <-fn_dataframe(results_fn_m16, results.model.16, re_vars.ls, model=as.numeric(16))
results.model.16 <-resultsdata[[1]]
re_vars.ls <-resultsdata[[2]]

vars.ls <- re_vars.ls

# Clean enviorment for next step-------------------------------------------------------
# Remove innecesary data sets to retrieved just the four needed for the next step
rm(list = ls()[!ls() %in% c('data_nomods', 'non_sample', 'results.model.3', 'time_elapsed.3',
                            'results.model.4', 'time_elapsed.4',
                            'results.model.5', 'time_elapsed.5',
                            'results.model.6', 'time_elapsed.6',
                            'results.model.7', 'time_elapsed.7',
                            'results.model.16', 'time_elapsed.16','vars.ls','nomod_n', 'mod_n')])
save.image("./Results/Results3.RData")


