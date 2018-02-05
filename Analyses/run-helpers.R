library(doParallel)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(tidyr)

# Globals
THETA <- 0.1

# getModelFile()
# ==============
# Return a model string from a .wppl file
#
# Parameters
# ----------
# modelFile: str
#  File path str
# 
# Returns
# -------
# str
#    Model str.
#
getModelFile <- function(modelFile) {
  readChar(modelFile, file.info(modelFile)$size)
}

# create_test()
# =============
# Create a new test.
#
# Parameters
# ----------
# modelFile: str
#   File path str
# runFn: function
#   Function that specifies run parameters (e.g. speaker_run_fn)
# testFn: function
#   Function that specifies boolean returning test.
#
# Returns
# -------
# function
#    New boolean returning test function.
#
create_test <- function(modelFile, run_fn, test_fn) {
  m <- getModelFile(modelFile)
  d <- run_fn(m)
  return(function() test_fn(d))
}

# listener_run_fn()
# =================
# Run a listener model.
#
# Parameters
# ----------
# modelFile: str
#   File path str
# modelName: str
#   Listener model to run (specifies recursion level e.g. "L1").
# alpha: int
#   Speaker rationality.
# lambda: int
#   Speaker rationality in listener recursion.
# theta: float
#   Noise level (prob of a deletion).
# speakerInput: str
#   Speaker target referent (one of 'a', 'b', 'c', 'd')
#
# Returns
# -------
# data.frame
#    Data.frame output from a call rwebppl file.
#
listener_run_fn <- function(modelFile, modelName='L1', alpha=1, lambda=10, theta=THETA) {
  modelStr <- getModelFile(modelFile)
  rData <- data.frame(alpha=alpha, lambda=lambda, modelName=modelName, theta=theta)
  rwebppl::webppl(modelStr, data=rData, data_var='rData')
}

# speaker_run_fn()
# ================
# Run a speaker model.
#
# Parameters
# ----------
# modelFile: str
#   File path str.
# modelName: str
#   Speaker model to run (specifies recursion level e.g. "S1").
# alpha: int
#   Speaker rationality.
# lambda: int
#   Speaker rationality in listener recursion.
# theta: float
#   Noise level (prob of a deletion).
# speakerInput: str
#   Speaker target referent (one of 'a', 'b', 'c', 'd')
#
# Returns
# -------
# data.frame
#    Data.frame output from a call rwebppl file.
#
speaker_run_fn <- function(modelFile, modelName='S3', alpha=5, lambda=1, theta=THETA, speakerInput='a'){
  # Run Settings
  # ------------
  modelStr <- getModelFile(modelFile)
  rData <- data.frame(alpha=alpha, lambda=lambda, modelName=modelName, theta=theta, speakerInput=speakerInput)
  rwebppl::webppl(modelStr, data=rData, data_var='rData')
}

# newRData()
# ==========
# Create a new data frame to pass to wppl models.
#
# Parameters
# ----------
# modelName: str
#  E.g. 'S5'
# alpha: float
#   Speaker model rationality parameter.
# lambda: float
#   Listener model rationality parameter.
# theta: float
#   Noise prob.
# n: int
#   Number of simulations.
#
# Returns
# -------
# data.frame
#    Data frame with data to pass to wppl model.
#
newRData <- function(modelName, alpha, lambda, theta, n) {
  data.frame(modelName=modelName,
             alpha=alpha,
             lambda=lambda,
             theta=theta,
             n=n,
             stringsAsFactors=FALSE)
}
# rData <- newRData('S1', 1, 1, 0.5, 10)

# createRunFn()
# =============
# Create a new run function for corpus experiment.
#
# Parameters
# ----------
# modelStr: str
#  Model str from call to getModelFile()
#
# Returns
# -------
# function
#    Run function.
#
createRunFn <- function(modelStr) {
  return(function(data, dataName='rData') { 
    rwebppl::webppl(modelStr, data=data, data_var=dataName)
  })
}

# runExperimentFn()
# =================
# Run a single isntance of a corpus experiment.
#
# Parameters
# ----------
# runFn: function
#   Run function.
# modelName: str
#  E.g. 'S5'
# alpha: float
#   Speaker model rationality parameter.
# lambda: float
#   Listener model rationality parameter.
# theta: float
#   Noise prob.
# n: int
#   Number of simulations.
#
# Returns
# -------
# function
#    Function that runs a single experiment data.frame.
#
runExperimentFn <- function(runFn, modelName, alpha, lambda, theta, n) {
  return(function(expNum) {
    rData <- newRData(modelName, alpha, lambda, theta, n)
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

# createLM()
# ==========
# Create a new language model.

# Parameters
# ----------
# dfCorpus: data.frame
#   Data.frame output of a call to a runExperiment funciton.
#
# Returns
# -------
# data.frame
#    Data.frame language model.
#
createLM <- function(dfCorpus) {
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

# process_sims()
# ==============
# Process multiple corpus simulations.
#
# Parameters
# ----------
# sims: data.frame
#   Data.frame containing simulations (output of runExperimentFn)
#
# Returns
# -------
# data.frame
#    Data.frame containing multiple instantiations of bigram
#    LM anlayses.
#
process_sims <- function(sims) {
  df_sims <- sims %>%
    group_by(expNum) %>%
    do(vals=data.frame(.)) %>%
    lapply(function(x) {(x)})
  
  lD <- lapply(df_sims$vals, function(x) {createLM(x)})
  df <- do.call(rbind, lD)
  df
}

# run_corpus_analysis()
# =====================
# Process a series of corpus experiments.
# Compute likelihood of marked utterance by surprisal
# with 95% CIs.
#
# Parameters
# ----------
# sims: data.frame
#   Data.frame containing simulations (output of runExperimentFn)
# ci: float
#   Desired (two-tailed) confidenced level.
#
# Returns
# -------
# data.frame
#    Data.frame containing multiple instantiations of bigram
#    LM anlayses.
#
run_corpus_analysis <- function(df_lm, ci=0.975) {
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
      xmin=quantile(postProb, probs=c(1-ci)),
      xmax=quantile(postProb, probs=c(ci)),
      avgLikelihood=mean(lik),
      ymin=quantile(lik, probs=c(1-ci)),
      ymax=quantile(lik, probs=c(ci)),
      n=n())
}

# run_corpus_analysis()
# =====================
# Process multiple corpus simulations.
#
# Parameters
# ----------
# sims: data.frame
#   Data.frame containing simulations (output of runExperimentFn)
#
# Returns
# -------
# data.frame
#    Data.frame containing multiple instantiations of bigram
#    LM anlayses.
#
corpus_run_fn <- function(modelFile, modelName, alpha, lambda, theta, nUtterances){
  modelStr <- getModelFile(modelFile)
  runFn <- createRunFn(modelStr)
  runExp_ <- runExperimentFn(runFn, modelName, alpha, lambda, theta, nUtterances)
  
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
  df_lm <- process_sims(sims)
  
  # LM analysis
  df_summary <- run_corpus_analysis(df_lm)
  df_summary
}