library(mlr)

fitMLR <- function(func, data, method = "dt", niter = 10){

    # Extract info from formula
  vars <- all.vars(func)
  resp <-as.character(func[[2]])
  predictors <- all.vars(func[[3]])

  # Reduce dataset
  data.mlr <- data[vars]

  # Define task
  tsk = makeRegrTask(id = paste("Resp", resp, sep = ""), data = data.mlr,
                     target = resp)

  # Define learner
  if (method == "dt") {
    lrn = makeLearner("regr.rpart", cp=0.0001)
  }

  if (method == "lm") {
    lrn = makeLearner("regr.lm")
  }

  # Train model
  #  mod <- train(lrn, tsk)

  # Define an do resampling
  rdesc = makeResampleDesc(method = "Bootstrap", iters = niter)
  r = resample(learner = lrn, task = tsk, resampling = rdesc, models = TRUE, show.info = FALSE)

  # Calculate results for every resampled model
  # Make partial predicitons for all predictors
  pred <- list()
  ames <- list()
  for (i in 1:niter){
    pred[[i]] <- generatePartialPredictionData(r$models[[i]], tsk, predictors)
    ames[[i]] <- list()

    # Calculate AMEs for all predictors
    for (predictor in predictors){
      ames[[i]][[predictor]] <- list()

      # Handle factors
      if (is.factor(data[, predictor]) == TRUE){
        cats <- levels(data[, predictor])
        refcat <- cats[1]
        refcat.pred <- pred[[i]]$data[which(pred[[i]]$data[, predictor] == refcat), resp]
        ames[[i]][[predictor]][["type"]] <- "factor"
        ames[[i]][[predictor]][["levels"]] <- cats
        ames[[i]][[predictor]][["reference"]] <- refcat
        ames[[i]][[predictor]][["predictions"]] <-vector(mode = "numeric", length(3))
        ames[[i]][[predictor]][["predictions"]][1] <- refcat.pred
        ames[[i]][[predictor]][["effects"]] <-vector(mode = "numeric", length(3))
        ames[[i]][[predictor]][["effects"]][1] <- NA

        # Loop over categories of factor
        ncat <- 2
        for (cat in cats[2:length(cats)]){
          cat.pred <- pred[[i]]$data[which(pred[[i]]$data[, predictor] == cat), resp]
          ame <- cat.pred - refcat.pred
          ames[[i]][[predictor]][["predictions"]][ncat] <- cat.pred
          ames[[i]][[predictor]][["effects"]][ncat] <- ame
          ncat <- ncat + 1
        } # end category loop

      } # end factor handling

      # Handle numeric predictors
      if (is.numeric(data[, predictor])){
        dat <- pred[[i]]$data[!is.na(pred[[i]]$data[, predictor]),c(resp, predictor)]
        dat <- dat[order(dat[predictor]), ]
        ame <- mean(diff(dat[, resp]))
        ames[[i]][[predictor]][["type"]] <- "numeric"
        ames[[i]][[predictor]][["values"]] <- dat[, predictor]
        ames[[i]][[predictor]][["predictions"]] <- dat[, resp]
        ames[[i]][[predictor]][["effect"]] <-ame
      } # end numeric handling

    } # end predictor loop
  } # end resample iterations

  # Aggregate resampled results
  # ... for each predictor
  results <- list()
  for (predictor in predictors){

    # Handle factors
    if (is.factor(data[, predictor]) == TRUE){
      results[[predictor]] <- list()
      cats <- ames[[1]][[predictor]][["levels"]]
      results[[predictor]][[cats[1]]] <- "reference"

      ncat <- 2
      for (cat in cats[2:length(cats)]){
        effect.vector <- vector(mode = "numeric", length = niter)

        for (i in 1:niter){
          effect.vector[i] <- ames[[i]][[predictor]][["effects"]][ncat]
        }
        res.mean <- round(mean(effect.vector), 3)
        res.se <- round(sd(effect.vector), 3)
        res.error <- round((qnorm(0.975) * res.se), 3)
        res.p <- round(2*pt(-abs(res.mean/res.se), df=niter-1), 3) # 2*pnorm(-abs(mean/se))
        results[[predictor]][[cat]] <- c(ame = res.mean, se = res.se,
                                         p.value = res.p, ci.lb = res.mean - res.error,
                                         ci.ub = res.mean + res.error)
        ncat <- ncat + 1
      }
    }

    # Handle numeric predictors
    if (is.numeric(data[, predictor])){
      effect.vector <- vector(mode = "numeric", length = niter)
      for (i in 1:niter){
        effect.vector[i] <- ames[[i]][[predictor]][["effect"]]
      }
      res.mean <- round(mean(effect.vector), 3)
      res.se <- round(sd(effect.vector), 3)
      res.error <- round((qnorm(0.975) * res.se), 3)
      res.p <- round(2*pt(-abs(res.mean/res.se), df=niter-1), 3) # 2*pnorm(-abs(mean/se))
      results[[predictor]] <- c(ame = res.mean, se = res.se,
                                p.value = res.p, ci.lb = res.mean - res.error,
                                ci.ub = res.mean + res.error)
    }
  }
  print(results)

}


fitMLR(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species, iris)
fitMLR(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species, iris, method = "lm")


