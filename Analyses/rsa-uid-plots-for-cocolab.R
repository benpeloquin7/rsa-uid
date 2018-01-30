library(doParallel)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(tidyr)


THETA <- 0.1
getModelFile <- function(fPath) {
  readChar(fPath, file.info(fPath)$size)
}

create_test <- function(fPath, run_fn, test_fn) {
  m <- getModelFile(fPath)
  d <- run_fn(m)
  return(function() test_fn(d))
}

listener_run_fn_ <- function(modelFile, alpha=1, lambda=10, modelName='L1', theta=THETA) {
  rData <- data.frame(alpha=alpha, lambda=lambda, modelName=modelName, theta=theta)
  rwebppl::webppl(modelFile, data=rData, data_var='rData')
}

speaker_run_fn_ <- function(modelFile, alpha_=5, lambda_=1, modelName_='S3', theta_=THETA, speakerInput_='a'){
  # Run Settings
  # ------------
  rData <- data.frame(alpha=alpha_, lambda=lambda_, modelName=modelName_, theta=theta_, speakerInput=speakerInput_)
  rwebppl::webppl(modelFile, data=rData, data_var='rData')
}

createRunFn <- function(modelStr) {
  return(function(data, dataName='rData') { 
    rwebppl::webppl(modelStr, data=data, data_var=dataName)
  })
}

createRData_ <- function(modelName, alpha, lambda, theta, n) {
  data.frame(modelName=modelName,
             alpha=alpha,
             lambda=lambda,
             theta=theta,
             n=n,
             stringsAsFactors=FALSE)
}

# Pipeline
# ---------
# (1) run experiments in parallel
# (2) run experiments in parallel

runExperimentFn_ <- function(runFn, modelName, alpha, lambda, theta, n) {
  return(function(expNum) {
    rData <- createRData_(modelName, alpha, lambda, theta, n)
    df <- runFn(rData) %>%
      mutate(modelName=modelName,
             alpha=alpha,
             lambda=lambda,
             theta=theta,
             n=n,
             expNum=expNum)
    df
  })
}

createLM_ <- function(dfCorpus) {
  annotateUtterance <- function(x) {
    x <- gsub(' ', '', x)
    x <- paste0('^', x)
    x <- paste0(x, '$')
    return(x)
  }
  stream <- dfCorpus %>%
    mutate(annotatedUtterance=annotateUtterance(currUtterance)) %>%
    select(annotatedUtterance) %>%
    unlist %>%
    paste0(collapse='') %>%
    gsub(' ', '', ., fixed=TRUE)
  ng <- ngram::ngram(stream, n=2, sep='')  # bigram model
  dfNgramRaw <- data.frame(ngram::get.phrasetable(ng))
  dfNgramRaw$first <- sapply(dfNgramRaw$ngrams, function(x) {strsplit(x, '')}[[1]][1])
  dfNgramRaw$second <- sapply(dfNgramRaw$ngrams, function(x) {strsplit(x, '')}[[1]][3])
  dfNgramRaw <- dfNgramRaw %>%
    mutate(first=ifelse(first==' ', '*', first),
           second=ifelse(second==' ', '*', second))
  dfNgramRaw$theta <- unique(dfCorpus$theta)
  dfNgramRaw$alpha <- unique(dfCorpus$alpha)
  dfNgramRaw$lambda <- unique(dfCorpus$lambda)
  dfNgramRaw$expNum <- unique(dfCorpus$expNum)
  dfNgramRaw
}

run_corpus_analysis_ <- function(df_lm) {
  df_lm %>%
    group_by(expNum, second) %>%
    mutate(secondTotalProp=sum(prop),       # p(a|X) + p(a|^)
           lik=prop/secondTotalProp) %>%    # p(X|a) = p(X a) / p(a|X) + p(a|^)
    ungroup %>%
    group_by(expNum) %>%
    mutate(postProb=log2(secondTotalProp/sum(prop))) %>%  # p(a)
    ungroup %>%
    select(expNum, first, second, lik, postProb) %>%
    filter(second %in% c(letters[1:5], '*'), first=='X') %>%
    group_by(second) %>%
    summarise(
      avgPostProb=mean(postProb),
      xmin=quantile(postProb, probs=c(0.05)),
      xmax=quantile(postProb, probs=c(0.95)),
      avgLikelihood=mean(lik),
      ymin=quantile(lik, probs=c(0.05)),
      ymax=quantile(lik, probs=c(0.95)),
      n=n())
}

process_sims_ <- function(sims) {
  lSimsExp1 <- sims %>%
    group_by(expNum) %>%
    do(vals=data.frame(.)) %>%
    lapply(function(x) {(x)})
  
  lD <- lapply(lSimsExp1$vals, function(x) {createLM_(x)})
  df <- do.call(rbind, lD)
  df
}



# Put in a function somewhere
# no_cores <- detectCores() - 1
# cl <- makeCluster(no_cores, type='FORK')
# registerDoParallel(cl)
# 
# simsExp1 <- foreach(i=seq(1, nIters), .packages=c('dplyr', 'rwebppl'), .combine=rbind) %dopar% runExp1(i)
# stopCluster(cl)

# corpus_run_fn
# -------------
# Pipeline
# (1) run simulations
# (2) create lm for each simluation
# (3) run lm analysis per simulation - utterance surprisal X likelihood of marked
corpus_run_fn_ <- function(modelFile, modelName='S3', alpha=5, lambda=1, theta=0.2, nUtterances=1000){
  runFn <- createRunFn(modelFile)
  runExp_ <- runExperimentFn_(runFn, modelName, alpha, lambda, theta, nUtterances)
  # Register cores
  no_cores <- detectCores() - 1
  cl <- makeCluster(no_cores, type='FORK')
  registerDoParallel(cl)
  # Run sims
  ptm <- proc.time()
  nSims <- 100
  cat("Running", nSims, "simulations each with", nUtterances, "samples.")
  sims <- foreach(i=seq(1, nSims), .packages=c('dplyr', 'rwebppl'), .combine=rbind) %dopar% runExp_(i)
  stopCluster(cl)
  etm <- proc.time() - ptm
  cat("runtime: ", etm[3] / 60)
  # Create LMs
  df_lm <- process_sims_(sims)
  # LM analysis
  df_summary <- run_corpus_analysis_(df_lm)
  df_summary
}