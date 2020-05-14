library(data.table)
library(ggplot2)

load("real_results.RData")

replaceNames = function(name) {
  if (name == "IntersectionCount") "SMA-Count"
  else if (name == "IntersectionGreedy") "SMA-Greedy"
  else if (name == "IntersectionMBM") "SMA-MBM"
  else if (name == "IntersectionMean") "SMA-Mean"
  else if (name == "Unadjusted") "SMU"
  else if (name == "Yu") "SMY"
  else if (name == "Zucknick") "SMZ"
  else NA_character_
}

result2 = result
result2$prob.name = factor(result2$prob.name, levels = c("tecator", "dilbert", "lsvt", "arcene"))

result3 = batchtools::unwrap(result2, sep = ".")
result3 = result3[, c("prob.name", "classif", "filter", "repl",
  "stability.train.value.stabilityIntersectionCount",
  "stability.train.value.stabilityIntersectionGreedy",
  "stability.train.value.stabilityIntersectionMBM",
  "stability.train.value.stabilityIntersectionMean",
  "stability.train.value.stabilityUnadjusted",
  "stability.train.value.stabilityYu",
  "stability.train.value.stabilityZucknick",
  "stability.train.time.stabilityIntersectionCount",
  "stability.train.time.stabilityIntersectionGreedy",
  "stability.train.time.stabilityIntersectionMBM",
  "stability.train.time.stabilityIntersectionMean",
  "stability.train.time.stabilityUnadjusted",
  "stability.train.time.stabilityYu",
  "stability.train.time.stabilityZucknick"), with = FALSE]

result4 = melt(result3, id.vars = c("prob.name", "classif", "filter", "repl"), variable.factor = FALSE)


#######################################################################
### correlations

res = split(result3, result3$prob.name)
res = lapply(res, function(r) {
  ret = r[, grep("value", colnames(r)), with = FALSE]
  tmp = sapply(strsplit(colnames(ret), ".stability"), function(x) x[2])
  colnames(ret) = sapply(tmp, replaceNames)
  return(ret)
})


cors = lapply(res, cor, use = "pairwise.complete.obs")

mean.cors = Reduce("+", cors) / length(cors)
o = hclust(as.dist(1 - mean.cors), method = "average")$order
cn = colnames(cors[[1]])
levs = cn[o]


plot_mat = function(dat, names, title = "") {
  colnames(dat) = rownames(dat) = names
  plt.data = cbind(measure = names, as.data.frame(dat))
  plt.data = reshape2::melt(plt.data, id.vars = "measure")
  plt.data$measure = factor(plt.data$measure, levels = names)
  plt.data$variable = factor(plt.data$variable, levels = names)

  gg = ggplot(plt.data, aes(measure, variable)) +
    geom_tile(aes(fill = value), colour = "white") +
    scale_fill_gradient(low = "white", high = "darkred", limits = c(0, 1), name = "Similarity") +
    theme_grey() +
    labs(x = element_blank(), y = element_blank()) +
    scale_x_discrete(expand = c(0, 0), labels = names) +
    scale_y_discrete(expand = c(0, 0), labels = names) +
    theme(axis.ticks = element_blank()) +
    coord_equal(ratio = 1) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
      axis.text = element_text(size = 12),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 12))

  if (title != "") {
    gg = gg +
      ggtitle(title) +
      theme(title = element_text(size = 13))
  } else {
    gg = gg + geom_text(mapping = aes(label = sprintf("%.4f", round(value, 4))),
      color = "white", size = 4.25)
  }

  return(gg)
}


pdf("cors.pdf", height = 6, width = 7)
plot_mat(mean.cors[o, o], names = levs, title = "")
dev.off()


#################################################################################
### run times

result.time = result4[grep("time", result4$variable), ]
result.time$variable = sapply(strsplit(result.time$variable, ".stability"), function(x) x[2])
result.time$variable = sapply(result.time$variable, replaceNames)
levs = c("SMY", "SMA-Count", "SMA-Mean", "SMA-Greedy", "SMA-MBM", "SMZ", "SMU")
result.time$variable = factor(result.time$variable, levels = levs)

rename_n_p = function(name) {
  if (name == "tecator") ret = "ID: 851,\nn = 240, p = 124"
  else if (name == "dilbert") ret = "ID: 41163,\nn = 4095, p = 2700"
  else if (name == "lsvt") ret = "ID: 1484,\nn = 126, p = 307"
  else ret = "ID: 1458,\nn = 200, p = 9961"
  return(ret)
}

result.time[, name.n.p := sapply(prob.name, rename_n_p)]
result.time$name.n.p = factor(result.time$name.n.p,
  levels = c("ID: 851,\nn = 240, p = 124", "ID: 41163,\nn = 4095, p = 2700",
    "ID: 1484,\nn = 126, p = 307", "ID: 1458,\nn = 200, p = 9961"))

result.time.adjusted = result.time[variable %in% c("SMY", "SMA-Greedy", "SMA-MBM", "SMA-Count", "SMA-Mean"), ]


gg.rt = ggplot(data = result.time.adjusted, mapping = aes(x = variable, y = value)) +
  geom_boxplot() +
  facet_wrap("name.n.p", scales = "free_y", ncol = 4) +
  xlab("Stability Measure") +
  ylab("Run Time (in Seconds)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))


pdf("runtimes.pdf", height = 3, width = 7)
print(gg.rt)
dev.off()
