library(batchtools)

makeExperimentRegistry(file.dir = "real", packages = c("mlr", "stabm"))

load("rins_halves.RData")
load("simmats_halves.RData")


###################################################################################
#### Definition of problems

for (i in seq_along(rins)) {
  addProblem(name = names(rins)[i], seed = i,
    data = list(rins = rins[[i]], sim.mats = sim.mats[[i]])
  )
}


####################################################################################
### Definition of algorithms

algoSVM = function(job, data, instance, fw.method) {
  source("resampling.R")

  ps = makeParamSet(
    makeNumericParam("sigma", lower = -15, upper = 15, trafo = function(x) 2^x),
    makeNumericParam("C", lower = -15, upper = 15, trafo = function(x) 2^x)
  )

  result = createAndResampleLearner(data = data, name = "classif.ksvm",
    ps = ps, fw.method = fw.method)
  return(result)
}


algoLasso = function(job, data, instance, fw.method) {
  source("resampling.R")

  ps = makeParamSet(
    makeDiscreteParam("alpha", values = 1),
    makeNumericParam("s", lower = -15, upper = 15, trafo = function(x) 2^x)
  )

  result = createAndResampleLearner(data = data, name = "classif.glmnet",
    ps = ps, fw.method = fw.method)
  return(result)
}


algoRF = function(job, data, instance, fw.method) {
  source("resampling.R")

  p = getTaskNFeats(data$rins$task.train)
  pars.fw = list()

  if (fw.method != "none") {
    fw.perc = runif(1)
    pars.fw$fw.perc = fw.perc
    p = round(p * fw.perc)
    p = max(p, 1)
  }

  ps = makeParamSet(
    makeDiscreteParam("num.trees", values = c(1, 2, 5, 10, 20, 50) * 100),
    makeNumericParam("min.node.size", lower = 0, upper = 1,
      trafo = function(x) {
        n = getTaskSize(data$rins$task.train)
        r = round((n * 0.2) ^ x)
        max(r, 1)
      }),
    makeIntegerParam("mtry", lower = 1, upper = p),
    makeDiscreteParam("importance", values = "impurity")
  )

  pars = as.list(generateDesign(1, ps, trafo = TRUE))

  classes = sapply(pars, class)
  for (i in which(classes == "factor")) {
    pars[[i]] = varhandle::unfactor(pars[[i]])
  }

  lrn = makeLearner("classif.ranger")
  lrn = setHyperPars(lrn, par.vals = pars)

  if (fw.method != "none") {
    pars = c(pars, pars.fw)
    lrn = makeFilterWrapper(lrn, fw.method = fw.method, fw.perc = pars.fw$fw.perc)
  }

  res = resampling(lrn = lrn, data = data)
  result = c(res, list(pars = pars))

  return(result)
}


algoBoosting = function(job, data, instance, fw.method) {
  source("resampling.R")

  ps = makeParamSet(
    makeNumericParam("mstop", lower = 0, upper = 15, trafo = function(x) round(2^x))
  )

  result = createAndResampleLearner(data = data, name = "classif.glmboost",
    ps = ps, fw.method = fw.method)
  return(result)
}


algoKNN = function(job, data, instance, fw.method) {
  source("resampling.R")

  ps = makeParamSet(
    makeDiscreteParam("kernel", values = "rectangular"),
    makeIntegerParam("k", lower = 1L, upper = 20L)
  )

  result = createAndResampleLearner(data = data, name = "classif.kknn",
    ps = ps, fw.method = fw.method)
  return(result)
}


algoRidge = function(job, data, instance, fw.method) {
  source("resampling.R")

  ps = makeParamSet(
    makeDiscreteParam("alpha", values = 0),
    makeNumericParam("s", lower = -15, upper = 15, trafo = function(x) 2^x)
  )

  result = createAndResampleLearner(data = data, name = "classif.glmnet",
    ps = ps, fw.method = fw.method)
  return(result)
}



addAlgorithm(name = "SVM", fun = algoSVM)
addAlgorithm(name = "Lasso", fun = algoLasso)
addAlgorithm(name = "RF", fun = algoRF)
addAlgorithm(name = "Boosting", fun = algoBoosting)
addAlgorithm(name = "KNN", fun = algoKNN)
addAlgorithm(name = "Ridge", fun = algoRidge)



#####################################################################################
### Definition of designs

design.algos = data.table(
  fw.method = c("praznik_JMI", "praznik_MIM", "ranger_impurity", "none")
)

algo.designs = lapply(1:6, function(i) design.algos)
names(algo.designs) = c("SVM", "Lasso", "RF", "Boosting", "KNN", "Ridge")


#################################################################################
### Add Experiments

addExperiments(algo.designs = algo.designs, repls = 200)


#################################################################################
# Submit all jobs and wait for them to be done

# submitJobs()

#################################################################################
### Collect results

res = reduceResultsList(findDone(), fun = function(job, res) {
  data.table(
    prob.name = job$prob.name,
    classif = job$algo.name,
    filter = job$pars$algo.pars$fw.method,
    repl = job$repl,
    stability.train.value = list(res$stability.train$values),
    stability.train.time = list(res$stability.train$times)
  )
})

result = dplyr::bind_rows(res)
save(result, file = "real_results.RData")

