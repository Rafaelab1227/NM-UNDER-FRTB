# Set the directory of the folder

#Load data from Step 1
load("./Data/datamods1.RData")

# Set seed to ensure that the results are reproducible
set.seed(1234)

# Required packages -------------------------------------------------------
req.packages <-
  c(
    "caret",
    "dplyr",
    "doParallel",
    "relgam",
    "spls",
    "bigstep",
    "tidyverse",
    "scales",
    "leaps"
  )
new.packages <-
  req.packages[!(req.packages %in% installed.packages()[, "Package"])]
if (length(new.packages))
  install.packages(new.packages)
invisible(lapply(req.packages, library, character.only = TRUE))

# Partition -------------------------------------------------------

# Creation of the partition of test and train. Simple 80% to take into account the behavior over time.
training.samples <- seq(1:floor(nrow(data_mods) * 0.8))
train.data <- data_mods[training.samples,-1]
test.data <- data_mods[-training.samples,-1]


# Preprocess of the sets -------------------------------------------------------

# Transformations and standardization
preProcValues <-
  preProcess(train.data, method = c("YeoJohnson", "scale", "center"))
train.data.pp <- predict(preProcValues, train.data)
test.data.pp <- predict(preProcValues, test.data)

# Remove dependencies for "modellable" risk factor
comboInfo_mods <- findLinearCombos(train.data.pp)
train.data.pp <- train.data.pp[,-comboInfo_mods$remove]
test.data.pp <- test.data.pp[,-comboInfo_mods$remove]

# Extract sample of 100 "non-modellable" variables to test the methods
non_sample <-
  sample(x = seq(2, dim(data_nomods)[2]),
         size = 100,
         replace = FALSE)

#If all the data is used, uncomment the next line
#non_sample <- seq(2,dim(nomod_n)[1])

# Create list for the variables
vars.ls <- sapply(as.character(non_sample), function(x)
  NULL)

# Create list for the models
#Define number of models that will be tested.
nmodels <- 17
name.models.ls <- paste0('mod.', seq(1:nmodels))
models.ls <- sapply(name.models.ls, function(x)
  NULL)

# Create list for the models inside each ""non-modellable" risk factor.
vars.ls <- lapply(vars.ls, function(x) {
  x = models.ls
})

# Results data frame per model
results.model  <- data.frame(non_sample)
results.model$RMSE <- NA
results.model$Rsquared <- NA
results.model$MAE <- NA

# Threshold high correlation

train.data.cor = cor(train.data.pp)
test.data.cor = cor(test.data.pp)

train.data.hc = findCorrelation(train.data.cor, cutoff = 0.75)

train.data.hc = sort(train.data.hc)
train.data.red.hc = train.data.pp[, -c(train.data.hc)]
test.data.red.hc = test.data.pp[, -c(train.data.hc)]


# Model 1: filter by high correlation ---------------------------------------
# Record system time to measure the computational time
start.t <- Sys.time()

#Create data frame for results for Model 1
results.model.1 <- results.model

for (i in 1:length(non_sample)) {
  # Extract one RF from the "non-modellable" matrix
  fr_m <- data_nomods[, non_sample[i], drop = FALSE]
  # Separate into train and test partitions the "modellable matrix"
  fr_m.train.base <- fr_m[training.samples, , drop = FALSE]
  fr_m.test.base <- fr_m[-training.samples, , drop = FALSE]
  #Create matrix, the first column is the "non-modellable" risk factor and the other the set of "modellable"
  fr_m.train <- cbind(fr_m.train.base, train.data.red.hc)
  fr_m.test <- cbind(fr_m.test.base, test.data.red.hc)
  
  # Parallelization
  # Use just the 80% of the total cores of the computer to avoid problems
  cores <- floor(0.8 * detectCores())
  cl <- makeCluster(cores)
  
  registerDoParallel(cl)
  
  # Definition of the model
  # Definition of the training method for hyperparameter search
  train.control <- trainControl(method = "cv", number = 10)
  
  model.1 <- train(
    x = fr_m.train[, -1],
    y = fr_m.train[, 1],
    data = fr_m.train,
    method = "leapSeq",
    tuneGrid = data.frame(nvmax = seq(1, 25, 1)),
    trControl = train.control
  )
  
  
  
  # Results collection for the model for each "non-modellable" variable
  results.ls <- list()
  
  # Keep coefficients of the resulting model
  results.ls[['var_coef']] <-
    coef(model.1$finalModel, as.numeric(model.1$bestTune)) %>%  as.data.frame() %>%
    rownames_to_column()
  
  #We could add other results as well
  #results.ls[['tune_parameters']] <- model.1$results
  
  #Tune parameters
  results.ls[['best_tune_parameters']] <- model.1$bestTune
  
  
  # Variables that were selected
  names_vars_mod <- results.ls[['var_coef']][[1]]
  
  # Create formula to perform a linear regression
  f <-
    formula(paste0(
      names(fr_m.train)[1],
      '~',
      paste(names_vars_mod[2:length(names_vars_mod)], collapse = " + ")
    ))
  
  # Linear regression
  model.1.1 <- train(f,
                     data = fr_m.train,
                     method = "lm")
  
  stopCluster(cl)
  
  # Make predictions
  pred.ls <- list()
  predictions <- model.1.1 %>% predict(fr_m.test)
  
  # Obtain performance metrics
  res.pred <- postResample(pred = predictions, obs = fr_m.test[, 1])
  
  #Insert in the list the results from the prediction
  pred.ls[['predictions']] <- predictions
  pred.ls[['predictions.res']] <- res.pred
  
  # Assign prediction and results lists to the model list
  
  # Assignation of the results of the model to the corresponding item
  vars.ls[[i]][[1]][['results']] <- results.ls
  
  # Assignation of the results of the predictions to the corresponding item
  vars.ls[[i]][[1]][['predictions']] <- pred.ls
  
  # Assign results for the model for the "non-modellable"factor
  results.model.1[i, 2] <- res.pred[[1]]
  results.model.1[i, 3] <- res.pred[[2]]
  results.model.1[i, 4] <- res.pred[[3]]
  
  # Print iteration to control the running
  print(i)
}

# Record time of the system to measure time
end.t <- Sys.time()
# Record computational time
time_elapsed.1 <- end.t - start.t


# Model 2: correlation filter for independent and then dependent variables----------------------------------------------------
# Record system time to measure the computational time
start.t <- Sys.time()

#Create data frame for results for Model 2
results.model.2 <- results.model

for (i in 1:length(non_sample)) {
  # Create partition for each "non-modellable" risk factor
  fr_m <- data_nomods[, non_sample[i], drop = FALSE]
  fr_m.train.base <- fr_m[training.samples, , drop = FALSE]
  fr_m.test.base <- fr_m[-training.samples, , drop = FALSE]
  
  #Create correlation matrix with independent variables
  cor <- cor(fr_m.train.base, train.data.red.hc)
  cor_names <- colnames(cor)[which(cor >= 0.50)]
  
  if (length(cor_names) >= 1) {
    train.data.red.hc_cor <- train.data.red.hc[which(cor >= 0.50)]
    test.data.red.hc_cor <- test.data.red.hc[which(cor >= 0.50)]
    
    # Create factor risk matrix
    fr_m.train <- cbind(fr_m.train.base, train.data.red.hc_cor)
    fr_m.test <- cbind(fr_m.test.base, test.data.red.hc_cor)
    
    # Parallelization
    cores <- floor(0.8 * detectCores())
    cl <- makeCluster(cores)
    
    registerDoParallel(cl)
    
    # Definition of the model
    f <- formula(paste0(names(fr_m.train)[1], '~', '.'))
    
    model.2 <- train(f,
                     data = fr_m.train,
                     method = "lm")
    
    stopCluster(cl)
    
    # Results collection for the model for each "non-modellable" variable
    results.ls <- list()
    
    # Keep coefficients of the resulting model
    results.ls[['var_coef']] <-
      coef(model.2$finalModel, as.numeric(model.2$bestTune)) %>%  as.data.frame() %>%
      rownames_to_column()
    results.ls[['best_tune_parameters']] <- model.2$bestTune
    
    # Make predictions
    pred.ls <- list()
    predictions <- model.2 %>% predict(fr_m.test)
    
    res.pred <-
      postResample(pred = predictions, obs = fr_m.test[, 1])
    
    pred.ls[['predictions']] <- predictions
    pred.ls[['predictions.res']] <- res.pred
    
    # Assign prediction and result list to the model list
    
    # Assignation of the results of the model to the correspondant item
    vars.ls[[i]][[2]][['results']] <- results.ls
    
    # Assignation of the results of the predictions to the correspondant item
    vars.ls[[i]][[2]][['predictions']] <- pred.ls
    
    # Assign results for the model for the variables
    results.model.2[i, 2] <- res.pred[[1]]
    results.model.2[i, 3] <- res.pred[[2]]
    results.model.2[i, 4] <- res.pred[[3]]
    
    
  } else {
    # Assign results for the model for the variables
    vars.ls[[i]][[2]][['results']] <- NA
    vars.ls[[i]][[2]][['predictions']] <- NA
    results.model.2[i, 2] <- NA
    results.model.2[i, 3] <- NA
    results.model.2[i, 4] <- NA
    
  }
  
  print(i)
}

end.t <- Sys.time()
time_elapsed.2 <- end.t - start.t

# Model 3: Subset of curves by type----------------------------------------------------------
# Record system time to measure the computational time

start.t <- Sys.time()

#Create data frame for results for Model 3
results.model.3 <- results.model

for (i in 1:length(non_sample)) {
  # Create factor risk matrix
  fr_m <-
    data_nomods[, non_sample[i], drop = FALSE] # extract one of non-modellability
  fr_m.train.base <- fr_m[training.samples, , drop = FALSE]
  fr_m.test.base <- fr_m[-training.samples, , drop = FALSE]
  
  nomod_n <-
    data.frame(lapply(nomod_n, as.character), stringsAsFactors = FALSE)
  mod_n <-
    data.frame(lapply(mod_n, as.character), stringsAsFactors = FALSE)
  
  # Types of curves filter
  type.nomod <-
    as.character(nomod_n[nomod_n$fr == names(fr_m)[1], match('type', names(nomod_n))])
  type.subset <- as.character(mod_n[mod_n$type == type.nomod, ]$fr)
  
  train.data.type.pp <-
    train.data.pp[, names(train.data.pp) %in% type.subset]
  test.data.type.pp <-
    test.data.pp[, names(test.data.pp) %in% type.subset]
  
  #Create matrix, the first column is the "non-modellable" risk factor and the other the set of "modellable"
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
  
  # Keep coefficients of the resulting model
  results.ls[['var_coef']] <-
    coef(model.3$finalModel, as.numeric(model.3$bestTune)) %>%  as.data.frame() %>%
    rownames_to_column()
  results.ls[['best_tune_parameters']] <- model.3$bestTune
  
  
  # Make predictions
  pred.ls <- list()
  predictions <- model.3 %>% predict(fr_m.test)
  
  res.pred <- postResample(pred = predictions, obs = fr_m.test[, 1])
  
  pred.ls[['predictions']] <- predictions
  pred.ls[['predictions.res']] <- res.pred
  
  # Assign prediction and result list to the model list
  
  # Assignation of the results of the model to the correspondant item
  vars.ls[[i]][[3]][['results']] <- list(NULL)
  vars.ls[[i]][[3]][['results']] <- results.ls
  
  # Assignation of the results of the predictions to the correspondant item
  vars.ls[[i]][[3]][['predictions']] <- list(NULL)
  vars.ls[[i]][[3]][['predictions']] <- pred.ls
  
  # Assign results for the model for the variables
  results.model.3[i, 2] <- res.pred[[1]]
  results.model.3[i, 3] <- res.pred[[2]]
  results.model.3[i, 4] <- res.pred[[3]]
  print(i)
}

end.t <- Sys.time()
time_elapsed.3 <- end.t - start.t

# Model 4: Subset of curves by currency----------------------------------------------------------
# Record system time to measure the computational time

start.t <- Sys.time()

#Create data frame for results for Model 4
results.model.4 <- results.model

for (i in 1:length(non_sample)) {
  # Create factor risk matrix
  fr_m <-
    data_nomods[, non_sample[i], drop = FALSE] # extract one of non-modellability
  fr_m.train.base <- fr_m[training.samples, , drop = FALSE]
  fr_m.test.base <- fr_m[-training.samples, , drop = FALSE]
  
  nomod_n <-
    data.frame(lapply(nomod_n, as.character), stringsAsFactors = FALSE)
  mod_n <-
    data.frame(lapply(mod_n, as.character), stringsAsFactors = FALSE)
  
  cur.nomod <-
    as.character(nomod_n[nomod_n$fr == names(fr_m)[1], match('currency', names(nomod_n))])
  cur.subset <- as.character(mod_n[mod_n$currency == cur.nomod, ]$fr)
  
  if (length(cur.subset) != 0) {
    train.data.type.pp <-
      train.data.pp[, names(train.data.pp) %in% cur.subset]
    test.data.type.pp <-
      test.data.pp[, names(test.data.pp) %in% cur.subset]
    
    #Create matrix, the first column is the "non-modellable" risk factor and the other the set of "modellable"
    fr_m.train <- cbind(fr_m.train.base, train.data.type.pp)
    fr_m.test <- cbind(fr_m.test.base, test.data.type.pp)
    
    # Definition of the model
    # Definition of the training method for hyperparameter search
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
    
    # Keep coefficients of the resulting model
    results.ls[['var_coef']] <-
      coef(model.4$finalModel, as.numeric(model.4$bestTune)) %>%  as.data.frame() %>%
      rownames_to_column()
    results.ls[['best_tune_parameters']] <- model.4$bestTune
    
    
    # Make predictions
    pred.ls <- list()
    predictions <- model.4 %>% predict(fr_m.test)
    
    res.pred <-
      postResample(pred = predictions, obs = fr_m.test[, 1])
    
    pred.ls[['predictions']] <- predictions
    pred.ls[['predictions.res']] <- res.pred
    
    # Assign prediction and result list to the model list
    
    # Assignation of the results of the model to the correspondant item
    vars.ls[[i]][[4]][['results']] <- list(NULL)
    vars.ls[[i]][[4]][['results']] <- results.ls
    
    # Assignation of the results of the predictions to the correspondant item
    vars.ls[[i]][[4]][['predictions']] <- list(NULL)
    vars.ls[[i]][[4]][['predictions']] <- list(pred.ls)[[1]]
    
    # Assign results for the model for the variables
    results.model.4[i, 2] <- res.pred[[1]]
    results.model.4[i, 3] <- res.pred[[2]]
    results.model.4[i, 4] <- res.pred[[3]]
    
    
  } else {
    vars.ls[[i]][[4]][['results']] <- NA
    vars.ls[[i]][[4]][['predictions']] <- NA
    results.model.4[i, 2] <- NA
    results.model.4[i, 3] <- NA
    results.model.4[i, 4] <- NA
  }
  
  print(i)
}

end.t <- Sys.time()
time_elapsed.4 <- end.t - start.t

# Model 5: Subset of curves by name ----------------------------------------------------------
# Record system time to measure the computational time

start.t <- Sys.time()

#Create data frame for results for Model 5
results.model.5 <- results.model


for (i in 1:length(non_sample)) {
  # Create factor risk matrix
  fr_m <-
    data_nomods[, non_sample[i], drop = FALSE] # extract one of non-modellability
  fr_m.train.base <- fr_m[training.samples, , drop = FALSE]
  fr_m.test.base <- fr_m[-training.samples, , drop = FALSE]
  
  nomod_n <-
    data.frame(lapply(nomod_n, as.character), stringsAsFactors = FALSE)
  mod_n <-
    data.frame(lapply(mod_n, as.character), stringsAsFactors = FALSE)
  
  curve_name.nomod <-
    as.character(nomod_n[nomod_n$fr == names(fr_m)[1], match('curve_name', names(nomod_n))])
  curve_name.subset <-
    as.character(mod_n[mod_n$curve_name == curve_name.nomod, ]$fr)
  
  if (length(curve_name.subset) != 0) {
    train.data.type.pp <-
      train.data.pp[, names(train.data.pp) %in% curve_name.subset]
    test.data.type.pp <-
      test.data.pp[, names(test.data.pp) %in% curve_name.subset]
    
    #Create matrix, the first column is the "non-modellable" risk factor and the other the set of "modellable"
    fr_m.train <- cbind(fr_m.train.base, train.data.type.pp)
    fr_m.test <- cbind(fr_m.test.base, test.data.type.pp)
    
    # Definition of the model
    # Definition of the training method for hyperparameter search
    train.control <- trainControl(method = "cv", number = 10)
    
    if (length(curve_name.subset) > 3) {
      # Parallelization
      cores <- floor(0.8 * detectCores())
      cl <- makeCluster(cores)
      
      registerDoParallel(cl)
      
      model.5 <- train(
        x = fr_m.train[, -1],
        y = fr_m.train[, 1],
        data = fr_m.train,
        method = "leapSeq",
        trControl = train.control
      )
      
      stopCluster(cl)
      
    } else{
      cores <- floor(0.8 * detectCores())
      cl <- makeCluster(cores)
      
      registerDoParallel(cl)
      
      f <- formula(paste0(names(fr_m.train)[1], '~', '.'))
      
      model.5 <- train(f,
                       data = fr_m.train,
                       method = "lm")
      stopCluster(cl)
    }
    
    
    # Results collection for the model for each non-modellable variable
    results.ls <- list()
    
    # Keep coefficients of the resulting model
    results.ls[['var_coef']] <-
      coef(model.5$finalModel, as.numeric(model.5$bestTune)) %>%  as.data.frame() %>%
      rownames_to_column()
    results.ls[['best_tune_parameters']] <- model.5$bestTune
    
    
    # Make predictions
    pred.ls <- list()
    predictions <- model.5 %>% predict(fr_m.test)
    
    res.pred <-
      postResample(pred = predictions, obs = fr_m.test[, 1])
    
    pred.ls[['predictions']] <- predictions
    pred.ls[['predictions.res']] <- res.pred
    
    # Assign prediction and result list to the model list
    
    # Assignation of the results of the model to the correspondant item
    vars.ls[[i]][[5]][['results']] <- list(NULL)
    vars.ls[[i]][[5]][['results']] <- results.ls
    
    # Assignation of the results of the predictions to the correspondant item
    vars.ls[[i]][[5]][['predictions']] <- list(NULL)
    vars.ls[[i]][[5]][['predictions']] <- pred.ls
    
    # Assign results for the model for the variables
    results.model.5[i, 2] <- res.pred[[1]]
    results.model.5[i, 3] <- res.pred[[2]]
    results.model.5[i, 4] <- res.pred[[3]]
    
    
  } else {
    vars.ls[[i]][[5]][['results']] <- NA
    vars.ls[[i]][[5]][['predictions']] <- NA
    results.model.5[i, 2] <- NA
    results.model.5[i, 3] <- NA
    results.model.5[i, 4] <- NA
  }
  print(i)
}

end.t <- Sys.time()
time_elapsed.5 <- end.t - start.t


# Model 6: Subset of curves by term ----------------------------------------------------------
# Record system time to measure the computational time

start.t <- Sys.time()

#Create data frame for results for Model 6
results.model.6 <- results.model

for (i in 1:length(non_sample)) {
  fr_m <-
    data_nomods[, non_sample[i], drop = FALSE] # extract one of non-modellability
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
    
    #Create matrix, the first column is the "non-modellable" risk factor and the other the set of "modellable"
    fr_m.train <- cbind(fr_m.train.base, train.data.type.pp)
    fr_m.test <- cbind(fr_m.test.base, test.data.type.pp)
    
    # Definition of the model
    # Definition of the training method for hyperparameter search
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
    
    # Keep coefficients of the resulting model
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
    
    # Assign prediction and result list to the model list
    
    # Assignation of the results of the model to the correspondant item
    vars.ls[[i]][[6]][['results']] <- list(NULL)
    vars.ls[[i]][[6]][['results']] <- results.ls
    
    # Assignation of the results of the predictions to the correspondant item
    vars.ls[[i]][[6]][['predictions']] <- list(NULL)
    vars.ls[[i]][[6]][['predictions']] <- pred.ls
    
    # Assign results for the model for the variables
    results.model.6[i, 2] <- res.pred[[1]]
    results.model.6[i, 3] <- res.pred[[2]]
    results.model.6[i, 4] <- res.pred[[3]]
    
    
  } else {
    vars.ls[[i]][[6]][['results']] <- NA
    vars.ls[[i]][[6]][['predictions']] <- NA
    results.model.6[i, 2] <- NA
    results.model.6[i, 3] <- NA
    results.model.6[i, 4] <- NA
  }
  print(i)
}

end.t <- Sys.time()
time_elapsed.6 <- end.t - start.t

# Model 7: wrapper stepwise for big data ----------------------------------------------------------
# Record system time to measure the computational time

start.t <- Sys.time()

#Create data frame for results for Model 7
results.model.7 <- results.model

for (i in 1:length(non_sample)) {
  # Create factor risk
  fr_m <- data_nomods[, non_sample[i], drop = FALSE]
  fr_m.train.base <- fr_m[training.samples, , drop = FALSE]
  fr_m.test.base <- fr_m[-training.samples, , drop = FALSE]
  
  #Create matrix, the first column is the "non-modellable" risk factor and the other the set of "modellable"
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
    # Definition of the formula of the model with the selected variables
    f <-
      formula(paste0(
        names(fr_m.train)[1],
        '~',
        paste(vars_selected_7, collapse = " + ")
      ))
    
    # Paralellization
    cores <- floor(0.8 * detectCores())
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
    
    # Keep coefficients of the resulting model
    results.ls[['var_coef']] <-
      coef(best.model[[1]]$finalModel,
           as.numeric(best.model[[1]]$bestTune)) %>%  as.data.frame() %>%
      rownames_to_column()
    #results.ls[['tune_parameters']] <- model.1$results
    # Keep best hyperparameters for the model
    results.ls[['best_tune_parameters']] <- best.model[[1]]$bestTune
    
    # Make predictions
    pred.ls <- list()
    
    # Make predictions
    predictions <- best.model[[1]] %>% predict(fr_m.test)
    
    res.pred <-
      postResample(pred = predictions, obs = fr_m.test[, 1])
    
    pred.ls[['predictions']] <- predictions
    pred.ls[['predictions.res']] <- res.pred
    
    # Assign prediction and result list to the model list
    
    # Assignation of the results of the model to the correspondant item
    vars.ls[[i]][[7]][['results']] <- results.ls
    
    # Assignation of the results of the predictions to the correspondant item
    vars.ls[[i]][[7]][['predictions']] <- pred.ls
    
    # Assign results for the model for the variables
    results.model.7[i, 2] <- res.pred[[1]]
    results.model.7[i, 3] <- res.pred[[2]]
    results.model.7[i, 4] <- res.pred[[3]]
  } else {
    vars.ls[[i]][[7]][['results']] <- NA
    vars.ls[[i]][[7]][['predictions']] <- NA
    results.model.7[i, 2] <- NA
    results.model.7[i, 3] <- NA
    results.model.7[i, 4] <- NA
  }
  
  print(i)
}
end.t <- Sys.time()
time_elapsed.7 <- end.t - start.t

# Model 8: Ridge -------------------------------------------------------------------
# Record system time to measure the computational time
start.t <- Sys.time()

#Create data frame for results for Model 8
results.model.8 <- results.model
lambda <- 10 ^ seq(-3, 3, length = 100)

for (i in 1:length(non_sample)) {
  # Create factor risk matrix
  fr_m <-
    data_nomods[, non_sample[i], drop = FALSE] # extract one of non-modellability
  fr_m.train.base <- fr_m[training.samples, , drop = FALSE]
  fr_m.test.base <- fr_m[-training.samples, , drop = FALSE]
  
  #Create matrix, the first column is the "non-modellable" risk factor and the other the set of "modellable"
  fr_m.train <- cbind(fr_m.train.base, train.data.pp)
  fr_m.test <- cbind(fr_m.test.base, test.data.pp)
  
  #Definition of the model
  # Parallelization
  cores <- floor(0.8 * detectCores())
  cl <- makeCluster(cores)
  
  registerDoParallel(cl)
  
  model.8 <- train(
    x = fr_m.train[, -1],
    y = fr_m.train[, 1],
    data = fr_m.train,
    method = "glmnet",
    trControl = trainControl("cv", number = 10),
    tuneGrid = expand.grid(alpha = 0, lambda = lambda)
  )
  stopCluster(cl)
  
  # Results collection for the model for each non-modellable variable
  results.ls <- list()
  
  # Keep coefficients of the resulting model
  results.ls[['var_coef']] <-
    coef(model.8$finalModel, as.numeric(model.8$bestTune$lambda)) %>% as.matrix() %>% as.data.frame() %>%
    rownames_to_column()
  # Keep best hyperparameters for the model
   results.ls[['best_tune_parameters']] <- model.8$bestTune
  
  
  # Make predictions
  pred.ls <- list()
  predictions <- model.8 %>% predict(fr_m.test)
  
  res.pred <- postResample(pred = predictions, obs = fr_m.test[, 1])
  
  pred.ls[['predictions']] <- predictions
  pred.ls[['predictions.res']] <- res.pred
  
  # Assign prediction and result list to the model list
  
  # Assignation of the results of the model to the correspondant item
  vars.ls[[i]][[8]][['results']] <- results.ls
  
  # Assignation of the results of the predictions to the correspondant item
  vars.ls[[i]][[8]][['predictions']] <- pred.ls
  
  # Assign results for the model for the variables
  results.model.8[i, 2] <- res.pred[[1]]
  results.model.8[i, 3] <- res.pred[[2]]
  results.model.8[i, 4] <- res.pred[[3]]
  print(i)
}

# Record of system time to measure the computational time from the begining to the end of the algorithm
end.t <- Sys.time()
time_elapsed.8 <- end.t - start.t

# Model 9: Lasso -------------------------------------------------------------------
# Record system time to measure the computational time

start.t <- Sys.time()

#Create data frame for results for Model 9
results.model.9 <- results.model

for (i in 1:length(non_sample)) {
  # Create factor risk matrix
  fr_m <-
    data_nomods[, non_sample[i], drop = FALSE] # extract one of non-modellability
  fr_m.train.base <- fr_m[training.samples, , drop = FALSE]
  fr_m.test.base <- fr_m[-training.samples, , drop = FALSE]
  
  #Create matrix, the first column is the "non-modellable" risk factor and the other the set of "modellable"
  fr_m.train <- cbind(fr_m.train.base, train.data.pp)
  fr_m.test <- cbind(fr_m.test.base, test.data.pp)
  
  #Definition of the model
  #Definition of the grid search of lambda
  lambda <- 10 ^ seq(-3, 3, length = 100)
  
  cores <- floor(0.8 * detectCores())
  cl <- makeCluster(cores)
  
  registerDoParallel(cl)
  
  model.9 <- train(
    x = fr_m.train[, -1],
    y = fr_m.train[, 1],
    data = fr_m.train,
    method = "glmnet",
    trControl = trainControl("cv", number = 10),
    tuneGrid = expand.grid(alpha = 1, lambda = lambda)
  )
  stopCluster(cl)
  
  # Results collection for the model for each non-modellable variable
  results.ls <- list()
  
  # Keep coefficients of the resulting model
  results.ls[['var_coef']] <-
    coef(model.9$finalModel, as.numeric(model.9$bestTune$lambda)) %>% as.matrix() %>% as.data.frame() %>%
    rownames_to_column()
  # Keep best hyperparameters for the model
  results.ls[['best_tune_parameters']] <- model.9$bestTune
  
  
  # Make predictions
  pred.ls <- list()
  predictions <- model.9 %>% predict(fr_m.test)
  
  res.pred <- postResample(pred = predictions, obs = fr_m.test[, 1])
  
  pred.ls[['predictions']] <- predictions
  pred.ls[['predictions.res']] <- res.pred
  
  # Assign prediction and result list to the model list
  
  # Assignation of the results of the model to the correspondant item
  vars.ls[[i]][[9]][['results']] <- results.ls
  
  # Assignation of the results of the predictions to the correspondant item
  vars.ls[[i]][[9]][['predictions']] <- pred.ls
  
  # Assign results for the model for the variables
  results.model.9[i, 2] <- res.pred[[1]]
  results.model.9[i, 3] <- res.pred[[2]]
  results.model.9[i, 4] <- res.pred[[3]]
  print(i)
}

end.t <- Sys.time()
time_elapsed.9 <- end.t - start.t

# Model 10: Elastic net -------------------------------------------------------------------
# Record system time to measure the computational time
start.t <- Sys.time()

#Create data frame for results for Model 10
results.model.10 <- results.model

for (i in 1:length(non_sample)) {
  # Create factor risk matrix
  fr_m <-
    data_nomods[, non_sample[i], drop = FALSE] # extract one of non-modellability
  fr_m.train.base <- fr_m[training.samples, , drop = FALSE]
  fr_m.test.base <- fr_m[-training.samples, , drop = FALSE]
  
  #Create matrix, the first column is the "non-modellable" risk factor and the other the set of "modellable"
  fr_m.train <- cbind(fr_m.train.base, train.data.pp)
  fr_m.test <- cbind(fr_m.test.base, test.data.pp)
  
  #Definition of the model
  cores <- floor(0.8 * detectCores())
  cl <- makeCluster(cores)
  
  registerDoParallel(cl)
  
  model.10 <- train(
    x = fr_m.train[, -1],
    y = fr_m.train[, 1],
    data = fr_m.train,
    method = "glmnet",
    trControl = trainControl("cv", number = 10)
  )
  stopCluster(cl)
  
  # Results collection for the model for each non-modellable variable
  results.ls <- list()
  
  results.ls[['var_coef']] <-
    coef(model.10$finalModel, as.numeric(model.10$bestTune$lambda)) %>% as.matrix() %>% as.data.frame() %>%
    rownames_to_column()
  results.ls[['best_tune_parameters']] <- model.10$bestTune
  
  
  # Make predictions
  pred.ls <- list()
  predictions <- model.10 %>% predict(fr_m.test)
  
  res.pred <- postResample(pred = predictions, obs = fr_m.test[, 1])
  
  pred.ls[['predictions']] <- predictions
  pred.ls[['predictions.res']] <- res.pred
  
  # Assign prediction and result list to the model list
  
  # Assignation of the results of the model to the correspondant item
  vars.ls[[i]][[10]][['results']] <- results.ls
  
  # Assignation of the results of the predictions to the correspondant item
  vars.ls[[i]][[10]][['predictions']] <- pred.ls
  
  # Assign results for the model for the variables
  results.model.10[i, 2] <- res.pred[[1]]
  results.model.10[i, 3] <- res.pred[[2]]
  results.model.10[i, 4] <- res.pred[[3]]
  print(i)
}

end.t <- Sys.time()
time_elapsed.10 <- end.t - start.t

# Model 11: Reluctant Generalized Additive Models ----------------------------------------------------
load("C:/Users/Rafaela Becerra/Desktop/UC3M/TFM/TFM/function_relgam.RData")

# Record system time to measure the computational time
start.t <- Sys.time()

#Create data frame for results for Model 11
results.model.11 <- results.model

for (i in 1:length(non_sample)) {
  # Create factor risk matrix
  fr_m <-
    data_nomods[, non_sample[i], drop = FALSE] # extract one of non-modellability
  fr_m.train.base <- fr_m[training.samples, , drop = FALSE]
  fr_m.test.base <- fr_m[-training.samples, , drop = FALSE]
  
  #Create matrix, the first column is the "non-modellable" risk factor and the other the set of "modellable"
  fr_m.train <- cbind(fr_m.train.base, train.data.pp)
  fr_m.test <- cbind(fr_m.test.base, test.data.pp)
  
  x = as.matrix(fr_m.train[, -1])
  y = as.matrix(fr_m.train[, 1])
  
  cores <- floor(0.8 * detectCores())
  cl <- makeCluster(cores)
  
  registerDoParallel(cl)
  model.11 <-  cv.rgam.ed(
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
  
  # Keep coefficients of the resulting model
  results.ls[['var_coef']] <-
    model.11$glmfit[["full_glmfit"]][["beta"]][, which(model.11$glmfit$lambda ==
                                                         model.11$lambda.min)] %>% as.matrix() %>% as.data.frame() %>%
    rownames_to_column()
  results.ls[['best_tune_parameters']] <- model.11$lambda.min
  results.ls[['var_linear']] <-
    model.11$glmfit$linfeat[[which(model.11$glmfit$lambda == model.11$lambda.min)]]
  results.ls[['var_nonlinear']] <-
    model.11$glmfit$nonlinfeat[[which(model.11$glmfit$lambda == model.11$lambda.min)]]
  
  # Make predictions
  pred.ls <- list()
  predictions <-
    model.11 %>% predict(as.matrix(fr_m.test[, -1]), s = "lambda.min")
  res.pred <- postResample(pred = predictions, obs = fr_m.test[, 1])
  
  pred.ls[['predictions']] <- predictions
  pred.ls[['predictions.res']] <- res.pred
  
  # Assign prediction and result list to the model list
  
  # Assignation of the results of the model to the correspondant item
  vars.ls[[i]][[11]][['results']] <- results.ls
  
  # Assignation of the results of the predictions to the correspondant item
  vars.ls[[i]][[11]][['predictions']] <- pred.ls
  
  # Assign results for the model for the variables
  results.model.11[i, 2] <- res.pred[[1]]
  results.model.11[i, 3] <- res.pred[[2]]
  results.model.11[i, 4] <- res.pred[[3]]
  print(i)
}

end.t <- Sys.time()
time_elapsed.11 <- end.t - start.t

# Model 12: PCR -------------------------------------------------------------------
# Record system time to measure the computational time

start.t <- Sys.time()

#Create data frame for results for Model 12
results.model.12 <- results.model

for (i in 1:length(non_sample)) {
  # Create factor risk matrix
  fr_m <-
    data_nomods[, non_sample[i], drop = FALSE] # extract one of non-modellability
  fr_m.train.base <- fr_m[training.samples, , drop = FALSE]
  fr_m.test.base <- fr_m[-training.samples, , drop = FALSE]
  
  #Create matrix, the first column is the "non-modellable" risk factor and the other the set of "modellable"
  fr_m.train <- cbind(fr_m.train.base, train.data.pp)
  fr_m.test <- cbind(fr_m.test.base, test.data.pp)
  
  #Definition of the model
  cores <- floor(0.8 * detectCores())
  cl <- makeCluster(cores)
  
  registerDoParallel(cl)
  
  model.12 <- train(
    x = fr_m.train[, -1],
    y = fr_m.train[, 1],
    method = "pcr",
    trControl = trainControl("cv", number = 10),
    tuneLength = 20
  )
  
  
  stopCluster(cl)
  
  # Results collection for the model for each non-modellable variable
  results.ls <- list()
  
  # Keep coefficients of the resulting model
  results.ls[['var_coef']] <-
    coef(model.12$finalModel, as.numeric(model.12$bestTune)) %>% as.matrix() %>% as.data.frame() %>%
    rownames_to_column()
  results.ls[['best_tune_parameters']] <- model.12$bestTune
  
  
  # Make predictions
  pred.ls <- list()
  predictions <-
    model.12 %>% predict(fr_m.test, ncomp = model.12$bestTune)
  
  res.pred <- postResample(pred = predictions, obs = fr_m.test[, 1])
  
  pred.ls[['predictions']] <- predictions
  pred.ls[['predictions.res']] <- res.pred
  
  # Assign prediction and result list to the model list
  
  # Assignation of the results of the model to the correspondant item
  vars.ls[[i]][[12]][['results']] <- results.ls
  
  # Assignation of the results of the predictions to the correspondant item
  vars.ls[[i]][[12]][['predictions']] <- pred.ls
  
  # Assign results for the model for the variables
  results.model.12[i, 2] <- res.pred[[1]]
  results.model.12[i, 3] <- res.pred[[2]]
  results.model.12[i, 4] <- res.pred[[3]]
  print(i)
}

end.t <- Sys.time()
time_elapsed.12 <- end.t - start.t

# Model 13: PLS -------------------------------------------------------------------
# Record system time to measure the computational time

start.t <- Sys.time()

#Create data frame for results for Model 13
results.model.13 <- results.model

for (i in 1:length(non_sample)) {
  # Create factor risk matrix
  fr_m <-
    data_nomods[, non_sample[i], drop = FALSE] # extract one of non-modellability
  fr_m.train.base <- fr_m[training.samples, , drop = FALSE]
  fr_m.test.base <- fr_m[-training.samples, , drop = FALSE]
  
  #Create matrix, the first column is the "non-modellable" risk factor and the other the set of "modellable"
  fr_m.train <- cbind(fr_m.train.base, train.data.pp)
  fr_m.test <- cbind(fr_m.test.base, test.data.pp)
  
  #Definition of the model
  cores <- floor(0.8 * detectCores())
  cl <- makeCluster(cores)
  
  registerDoParallel(cl)
  
  model.13 <- train(
    x = fr_m.train[, -1],
    y = fr_m.train[, 1],
    method = "pls",
    trControl = trainControl("cv", number = 10),
    tuneLength = 20
  )
  
  
  stopCluster(cl)
  
  # Results collection for the model for each non-modellable variable
  results.ls <- list()
  
  # Keep coefficients of the resulting model
  results.ls[['var_coef']] <-
    coef(model.13$finalModel, as.numeric(model.13$bestTune)) %>% as.matrix() %>% as.data.frame() %>%
    rownames_to_column()
  results.ls[['best_tune_parameters']] <- model.13$bestTune
  
  
  # Make predictions
  pred.ls <- list()
  predictions <-
    model.13 %>% predict(fr_m.test, ncomp = model.13$bestTune)
  
  res.pred <- postResample(pred = predictions, obs = fr_m.test[, 1])
  
  pred.ls[['predictions']] <- predictions
  pred.ls[['predictions.res']] <- res.pred
  
  # Assign prediction and result list to the model list
  
  # Assignation of the results of the model to the correspondant item
  vars.ls[[i]][[13]][['results']] <- results.ls
  
  # Assignation of the results of the predictions to the correspondant item
  vars.ls[[i]][[13]][['predictions']] <- pred.ls
  
  # Assign results for the model for the variables
  results.model.13[i, 2] <- res.pred[[1]]
  results.model.13[i, 3] <- res.pred[[2]]
  results.model.13[i, 4] <- res.pred[[3]]
  print(i)
}

end.t <- Sys.time()
time_elapsed.13 <- end.t - start.t

# Model 14: ICR -------------------------------------------------------------------
# Record system time to measure the computational time

start.t <- Sys.time()

#Create data frame for results for Model 14
results.model.14 <- results.model

for (i in 1:length(non_sample)) {
  # Create factor risk matrix
  fr_m <-
    data_nomods[, non_sample[i], drop = FALSE] # extract one of non-modellability
  fr_m.train.base <- fr_m[training.samples, , drop = FALSE]
  fr_m.test.base <- fr_m[-training.samples, , drop = FALSE]
  
  #Create matrix, the first column is the "non-modellable" risk factor and the other the set of "modellable"
  fr_m.train <- cbind(fr_m.train.base, train.data.pp)
  fr_m.test <- cbind(fr_m.test.base, test.data.pp)
  
  #Definitin of the model
  cores <- floor(0.8 * detectCores())
  cl <- makeCluster(cores)
  
  registerDoParallel(cl)
  
  model.14 <- train(
    x = fr_m.train[, -1],
    y = fr_m.train[, 1],
    method = "icr",
    trControl = trainControl("cv", number = 10),
    tuneLength = 10
  )
  
  
  stopCluster(cl)
  
  # Results collection for the model for each non-modellable variable
  results.ls <- list()
  
  # Keep coefficients of the resulting model
  results.ls[['var_coef']] <-
    model.14$finalModel$model$coefficients %>% as.matrix()
  results.ls[['best_tune_parameters']] <- model.14$bestTune
  
  
  # Make predictions
  pred.ls <- list()
  predictions <-
    model.14 %>% predict(fr_m.test, ncomp = model.14$bestTune)
  
  res.pred <- postResample(pred = predictions, obs = fr_m.test[, 1])
  
  pred.ls[['predictions']] <- predictions
  pred.ls[['predictions.res']] <- res.pred
  
  # Assign prediction and result list to the model list
  
  # Assignation of the results of the model to the correspondant item
  vars.ls[[i]][[14]][['results']] <- results.ls
  
  # Assignation of the results of the predictions to the correspondant item
  vars.ls[[i]][[14]][['predictions']] <- pred.ls
  
  # Assign results for the model for the variables
  results.model.14[i, 2] <- res.pred[[1]]
  results.model.14[i, 3] <- res.pred[[2]]
  results.model.14[i, 4] <- res.pred[[3]]
  print(i)
}

end.t <- Sys.time()
time_elapsed.14 <- end.t - start.t

# Model 15: SPLS -------------------------------------------------------------------
# Record system time to measure the computational time

start.t <- Sys.time()

#Create data frame for results for Model 15
results.model.15 <- results.model

spls.grid <- expand.grid(eta = seq(0.1, 0.9, 0.1),
                         K = 5,
                         kappa = 0.5)


for (i in 1:length(non_sample)) {
  # Create factor risk matrix
  fr_m <-
    data_nomods[, non_sample[i], drop = FALSE] # extract one of non-modellability
  fr_m.train.base <- fr_m[training.samples, , drop = FALSE]
  fr_m.test.base <- fr_m[-training.samples, , drop = FALSE]
  
  #Create matrix, the first column is the "non-modellable" risk factor and the other the set of "modellable"
  fr_m.train <- cbind(fr_m.train.base, train.data.pp)
  fr_m.test <- cbind(fr_m.test.base, test.data.pp)
  
  #Definition of the model
  cores <- floor(0.8 * detectCores())
  cl <- makeCluster(cores)
  
  registerDoParallel(cl)
  
  model.15 <- train(
    x = fr_m.train[, -1],
    y = fr_m.train[, 1],
    method = "spls",
    trControl = trainControl("cv", number = 10),
    tuneGrid = spls.grid
  )
  
  
  stopCluster(cl)
  
  # Results collection for the model for each non-modellable variable
  results.ls <- list()
  
  # Keep coefficients of the resulting model
  results.ls[['var_coef']] <-
    coef(model.15$finalModel, as.numeric(model.15$bestTune)) %>% as.matrix() %>% as.data.frame() %>%
    rownames_to_column()
  results.ls[['best_tune_parameters']] <- model.15$bestTune
  
  
  # Make predictions
  pred.ls <- list()
  predictions <-
    model.15 %>% predict(fr_m.test, ncomp = model.15$bestTune)
  
  res.pred <- postResample(pred = predictions, obs = fr_m.test[, 1])
  
  pred.ls[['predictions']] <- predictions
  pred.ls[['predictions.res']] <- res.pred
  
  # Assign prediction and result list to the model list
  
  # Assignation of the results of the model to the correspondant item
  vars.ls[[i]][[15]][['results']] <- results.ls
  
  # Assignation of the results of the predictions to the correspondant item
  vars.ls[[i]][[15]][['predictions']] <- pred.ls
  
  # Assign results for the model for the variables
  results.model.15[i, 2] <- res.pred[[1]]
  results.model.15[i, 3] <- res.pred[[2]]
  results.model.15[i, 4] <- res.pred[[3]]
  print(i)
}

end.t <- Sys.time()
time_elapsed.15 <- end.t - start.t

# Model 16: Reluctant Generalized Additive Models with filter ----------------------------------------------------

load("C:/Users/Rafaela Becerra/Desktop/UC3M/TFM/TFM/function_relgam.RData")
# Record system time to measure the computational time
start.t <- Sys.time()

#Create data frame for results for Model 16
results.model.16 <- results.model

for (i in 1:length(non_sample)) {
  # Create factor risk matrix
  fr_m <-
    data_nomods[, non_sample[i], drop = FALSE] # extract one of non-modellability
  fr_m.train.base <- fr_m[training.samples, , drop = FALSE]
  fr_m.test.base <- fr_m[-training.samples, , drop = FALSE]
  
  nomod_n <-
    data.frame(lapply(nomod_n, as.character), stringsAsFactors = FALSE)
  mod_n <-
    data.frame(lapply(mod_n, as.character), stringsAsFactors = FALSE)
  
  cur.nomod <-
    as.character(nomod_n[nomod_n$fr == names(fr_m)[1], match('currency', names(nomod_n))])
  cur.subset <- as.character(mod_n[mod_n$currency == cur.nomod, ]$fr)
  
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
    
    # Definition of the model
    # Definition of the training method for hyperparameter search
    train.control <- trainControl(method = "cv", number = 10)
    
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
    
    # Keep coefficients of the resulting model
    results.ls[['var_coef']] <-
      model.16$glmfit[["full_glmfit"]][["beta"]][, which(model.16$glmfit$lambda ==
                                                           model.16$lambda.min)] %>% as.matrix() %>% as.data.frame() %>%
      rownames_to_column()
    results.ls[['best_tune_parameters']] <- model.16$lambda.min
    results.ls[['var_linear']] <-
      model.16$glmfit$linfeat[[which(model.16$glmfit$lambda == model.16$lambda.min)]]
    results.ls[['var_nonlinear']] <-
      model.16$glmfit$nonlinfeat[[which(model.16$glmfit$lambda == model.16$lambda.min)]]
    results.ls[['var_select']] <- cur.subset
    
    # Make predictions
    pred.ls <- list()
    predictions <-
      model.16 %>% predict(as.matrix(fr_m.test[, -1]), s = "lambda.min")
    res.pred <- postResample(pred = predictions, obs = fr_m.test[, 1])
    
    pred.ls[['predictions']] <- predictions
    pred.ls[['predictions.res']] <- res.pred
    
    # Assign prediction and result list to the model list
    
    # Assignation of the results of the model to the correspondant item
    vars.ls[[i]][[16]][['results']] <- list(NULL)
    vars.ls[[i]][[16]][['results']] <- results.ls
    
    # Assignation of the results of the predictions to the correspondant item
    vars.ls[[i]][[16]][['predictions']] <- list(NULL)
    vars.ls[[i]][[16]][['predictions']] <- pred.ls
    
    # Assign results for the model for the variables
    results.model.16[i, 2] <- res.pred[[1]]
    results.model.16[i, 3] <- res.pred[[2]]
    results.model.16[i, 4] <- res.pred[[3]]
    
    
  } else {
    vars.ls[[i]][[16]][['results']] <- NA
    vars.ls[[i]][[16]][['predictions']] <- NA
    results.model.16[i, 2] <- NA
    results.model.16[i, 3] <- NA
    results.model.16[i, 4] <- NA
  }
  
  print(i)
  
}


end.t <- Sys.time()
time_elapsed.16 <- end.t - start.t


# Model 17: XGBoost with filter ----------------------------------------------------
# Record system time to measure the computational time

start.t <- Sys.time()

#Create data frame for results for Model 17
results.model.17 <- results.model


for (i in 1:length(non_sample)) {
  # Create factor risk matrix
  fr_m <-
    data_nomods[, non_sample[i], drop = FALSE] # extract one of non-modellability
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
    
    #Create matrix, the first column is the "non-modellable" risk factor and the other the set of "modellable"
    fr_m.train <- cbind(fr_m.train.base, train.data.type.pp)
    fr_m.test <- cbind(fr_m.test.base, test.data.type.pp)
    
    x = as.matrix(fr_m.train[, -1])
    y = as.matrix(fr_m.train[, 1])
    
    # Definition of the model
    # Definition of the training method for hyperparameter search
    train.control = trainControl(
      method = "cv",
      number = 5,
      allowParallel = TRUE,
      returnData = FALSE
    )
    
    #Definition of the grid of hyperparameters for the search
    param.grid <- expand.grid(
      nrounds = 200,
      max_depth = c(5, 10),
      colsample_bytree = seq(0.75, 0.9, length.out = 3),
      eta = 0.1,
      gamma = 0,
      min_child_weight = 1,
      subsample = 0.5
    )
    # Parallelization
    cores <- floor(0.8 * detectCores())
    cl <- makeCluster(cores)
    
    registerDoParallel(cl)
    
    model.17 <- train(
      x = fr_m.train[, -1],
      y = fr_m.train[, 1],
      method = "xgbTree",
      trControl = train.control,
      tuneGrid = param.grid
    )
    
    stopCluster(cl)
    
    # Results collection for the model for each non-modellable variable
    results.ls <- list()
    
    # Keep coefficients of the resulting model
    results.ls[['var_coef']] <- varImp(model.17)
    results.ls[['best_tune_parameters']] <- model.17$bestTune
    # Keep importance of variables of the model
    results.ls[['var_coef_xg']] <-
      xgb.importance(
        feature_names = model.17$finalModel$feature_names,
        model = model.17$finalModel
      )
    
    # Make predictions
    pred.ls <- list()
    predictions <- model.17 %>% predict(fr_m.test)
    
    res.pred <-
      postResample(pred = predictions, obs = fr_m.test[, 1])
    
    # Assign prediction and result list to the model list
    
    pred.ls[['predictions']] <- predictions
    pred.ls[['predictions.res']] <- res.pred
    
    # Assign prediction and result list to the model list
    
    # Assignation of the results of the model to the correspondant item
    vars.ls[[i]][[17]][['results']] <- list(NULL)
    vars.ls[[i]][[17]][['results']] <- results.ls
    
    # Assignation of the results of the predictions to the correspondant item
    vars.ls[[i]][[17]][['predictions']] <- list(NULL)
    vars.ls[[i]][[17]][['predictions']] <- pred.ls
    
    # Assign results for the model for the variables
    results.model.17[i, 2] <- res.pred[[1]]
    results.model.17[i, 3] <- res.pred[[2]]
    results.model.17[i, 4] <- res.pred[[3]]
    
    
  } else {
    vars.ls[[i]][[17]][['results']] <- NA
    vars.ls[[i]][[17]][['predictions']] <- NA
    results.model.17[i, 2] <- NA
    results.model.17[i, 3] <- NA
    results.model.17[i, 4] <- NA
  }
  
  print(i)
  
}
# Record of system time to measure the computational time from the begining to the end of the algorithm
end.t <- Sys.time()
time_elapsed.17 <- end.t - start.t

# Remove innecesary data frames for the next step-------------------------------------------------------
list_include <- c(paste0("results.model.", seq(1,17)),paste0("time_elapsed.", seq(1,17)), 'data_nomods', 'vars.ls', 'non_sample','nomod_n') 
rm(list = ls()[!ls() %in% list_include])

save.image("./Results/Results1.RData")
