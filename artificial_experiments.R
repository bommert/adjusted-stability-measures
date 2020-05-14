library(batchtools)

makeExperimentRegistry(file.dir = "artificial", packages = "stabm")


###################################################################################
#### Definition of problems

combs = lapply(0:7, function(i) combn(7, i, simplify = FALSE))
combs = Reduce(c, combs)

sim.mat = mat = matrix(0.1, ncol = 7, nrow = 7)
sim.mat[1:3, 1:3] = 0.95
sim.mat[4:5, 4:5] = 0.95
sim.mat[6:7, 6:7] = 0.95
diag(sim.mat) = 1

addProblem(name = "blocks", seed = 2,
  data = list(data = combs, p = 7, sim.mat = sim.mat),
  fun = function(job, data, i, j) data$data[c(i,j)]
)


####################################################################################
### Definition of algorithm

algo = function(job, data, instance) {
  lsm = listStabilityMeasures()
  values = numeric(nrow(lsm))

  for (i in seq_len(nrow(lsm))) {
    fun = get(lsm$Name[i])
    if (lsm$Adjusted[i]) {
      if (lsm$Corrected[i]) {
        values[i] = fun(features = instance, sim.mat = data$sim.mat, correction.for.chance = "exact")
      } else {
        values[i] = fun(features = instance, sim.mat = data$sim.mat)
      }
    } else {
      values[i] = fun(features = instance, p = data$p)
    }
  }

  values = as.list(values)
  names(values) = lsm$Name

  result = list(values = values, features = instance)
}

addAlgorithm(name = "stability", fun = algo)


#####################################################################################
### Definition of designs

design.probs = expand.grid(
  i = seq_along(combs),
  j = seq_along(combs)
)


#################################################################################
### Add Experiments

addExperiments(prob.designs = list(blocks = design.probs))


###############################################################################
### Submit jobs and wait for all jobs to be done

# submitJobs()


#################################################################################
### Collect results

res = reduceResultsList(findDone(), fun = function(job, res) {
  data.table(
    prob.name = job$prob.name,
    i = job$prob.pars$i,
    j = job$prob.pars$j,
    features = list(res$features),
    value = list(res$values)
  )
})

result = dplyr::bind_rows(res)
result = unwrap(result, cols = "value")


save(result, file = "artificial_results.RData")
