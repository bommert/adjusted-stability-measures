library(OpenML)
library(mlr)
library(data.table)
library(Matrix)
library(BBmisc)

###########################################################################
##################### Datasets ############################################
###########################################################################

ids = c(851, 41163, 1484, 1458)

datasets = lapply(ids, getOMLDataSet)
datasets = lapply(datasets, convertOMLDataSetToMlr)
names(datasets) = c("tecator", "dilbert", "lsvt", "arcene")

data.subset.classes = function(task, classes) {
  target = getTaskData(task, target.extra = TRUE)$target
  keep = target %in% classes
  data = getTaskData(task)
  data = data[keep, ]
  data[[getTaskTargetNames(task)]] = droplevels(data[[getTaskTargetNames(task)]])
  ret = makeClassifTask(id = getTaskId(task), data = data, target = getTaskTargetNames(task))
  return(ret)
}

datasets$dilbert = data.subset.classes(datasets$dilbert, c("1", "3"))

datasets = lapply(datasets, removeConstantFeatures)

# rename classes
renameClasses = function(task, old.class.names, new.class.names) {
  dat = getTaskData(task)
  levs = levels(dat[[getTaskTargetNames(task)]])

  if (!identical(levs, old.class.names)) {
    if (identical(levs, old.class.names[2:1])) {
      new.class.names = new.class.names[2:1]
    } else {
      warning("Old class names did not match!")
      return(task)
    }
  }

  levels(dat[[getTaskTargetNames(task)]]) = new.class.names
  pos = new.class.names[which(levs == task$task.desc$positive)]
  task2 = makeClassifTask(id = getTaskId(task), data = dat,
    target = getTaskTargetNames(task), positive = pos)
  return(task2)
}

datasets$tecator = renameClasses(datasets$tecator, c("P", "N"), c("Positive", "Negative"))
datasets$lsvt = renameClasses(datasets$lsvt, c("1", "2"), c("Acceptable", "Unacceptable"))
datasets$arcene = renameClasses(datasets$arcene, c("1", "2"), c("Cancer", "Control"))


save(datasets, file = "datasets.RData")





set.seed(2019)

iters = 10
rdesc.cv = makeResampleDesc("CV", iters = iters, stratify = TRUE)
rdesc.tt = makeResampleDesc("Holdout", split = 0.5, stratify = TRUE)

makeRins = function(task) {
  rin = makeResampleInstance(rdesc.tt, task)

  task.train = subsetTask(task, rin$train.inds[[1]])
  task.train = removeConstantFeatures(task.train)
  feats.train = getTaskFeatureNames(task.train)

  task.test = subsetTask(task, rin$test.inds[[1]])
  feats.test = getTaskFeatureNames(task.test)
  feats.diff = setdiff(feats.test, feats.train)
  if (length(feats.diff) > 0) {
    task.test = subsetTask(task.test, features = feats.train)
  }

  rin.train = makeResampleInstance(rdesc.cv, task.train)
  rin.test = makeResampleInstance(rdesc.cv, task.test)

  return(list(task.train = task.train, task.test = task.test,
    rin.train = rin.train, rin.test = rin.test))
}

rins = lapply(datasets, makeRins)

save(rins, file = "rins_halves.RData")



# list of sparse similarity matrices with threshold = 0.9
simMats = function(task, threshold = 0.9) {
  trafoIndex = function(index, nr) {
    i1 = ceiling(index / nr)
    i2 = index - (i1 - 1) * nr
    return(c(i1, i2))
  }

  dat = getTaskData(task, target.extra = TRUE)$data
  cc = abs(cor(dat))

  gt = which(cc >= threshold)
  gt.mat = convertListOfRowsToDataFrame(lapply(gt, trafoIndex, nr = nrow(cc)))
  w = which(gt.mat[, 1] >= gt.mat[, 2])

  sparse.mat = sparseMatrix(gt.mat[w, 1], gt.mat[w, 2], x = cc[gt[w]], symmetric = TRUE)

  colnames(sparse.mat) = rownames(sparse.mat) = getTaskFeatureNames(task)
  return(sparse.mat)
}

sim.mats = lapply(rins, function(r) {
  sim.mat.train = simMats(r$task.train)
  sim.mat.test = simMats(r$task.test)
  return(list(sim.mat.train = sim.mat.train, sim.mat.test = sim.mat.test))
})

save(sim.mats, file = "simmats_halves.RData")
