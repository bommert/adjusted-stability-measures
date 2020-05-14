library(data.table)
library(ggplot2)

load("artificial_results.RData")

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


result = result[, c("i", "j", "stabilityIntersectionCount",
  "stabilityIntersectionGreedy", "stabilityIntersectionMBM", "stabilityIntersectionMean",
  "stabilityUnadjusted", "stabilityYu", "stabilityZucknick"), with = FALSE]

result2 = melt(result, id.vars = c("i", "j"), variable.factor = FALSE)


##############################################################################################
### scatter plots

result.value = result2[grep("stability", result2$variable), ]
result.value$variable = sapply(strsplit(result.value$variable, "stability"), function(x) x[2])
result.value$variable = sapply(result.value$variable, replaceNames)

result.value2 = result.value
colnames(result.value2) = c("i", "j", "variable2", "value2")
result.value.m = merge(result.value, result.value2, by = c("i", "j"), allow.cartesian = TRUE)
result.value.m2 = result.value.m[, .N, by = c("variable", "value", "variable2", "value2")]

levs = c("SMY", "SMA-Count", "SMA-Mean", "SMA-Greedy", "SMA-MBM", "SMZ", "SMU")
result.value.m2$variable = factor(result.value.m2$variable, levels = levs)
result.value.m2$variable2 = factor(result.value.m2$variable2, levels = levs)

gg.scatter = ggplot(data = result.value.m2, mapping = aes(x = value, y = value2)) +
  geom_abline(intercept = 0, slope = 1, color = "red", size = 1.1) +
  geom_point() +
  facet_grid(variable2 ~ variable, scales = "free") +
  theme_bw() +
  xlab("Stability Value") + ylab("Stability Value") +
  theme(axis.text.x = element_text(size = 7),
    strip.text = element_text(size = 11),
    axis.title = element_text(size = 13)) +
  scale_x_continuous(labels = function(x) sprintf("%.1f", x))

pdf("scatter.pdf", height = 8, width = 8)
print(gg.scatter)
dev.off()


#############################################################################################
### visualization of similarity matrix

sim.mat = matrix(0.1, ncol = 7, nrow = 7)
sim.mat[1:3, 1:3] = 0.95
sim.mat[4:5, 4:5] = 0.95
sim.mat[6:7, 6:7] = 0.95
diag(sim.mat) = 1

m = melt(sim.mat)
m = cbind(m, similar = ifelse(m$value >= 0.9, "Yes", "No"))
m$value = round(m$value, 3)
leg.title = expression(Similarity >= 0.9)

gg.mat = ggplot(data = m, mapping = aes(x = Var1, y = Var2, label = value)) +
  geom_tile(mapping = aes(fill = similar), color = "white") +
  geom_text(size = 4.5) +
  theme_void() +
  labs(x = element_blank(), y = element_blank()) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(axis.text = element_blank(), plot.margin = grid::unit(c(0, 3, 3, 3), "mm")) +
  scale_discrete_manual(values = c("grey85", "grey70"), aesthetics = "fill") +
  theme(legend.position = "bottom", legend.title.align = 1,
    legend.text = element_text(size = 14), legend.title = element_text(size = 14)) +
  guides(fill = guide_legend(title = leg.title))

pdf("simmat.pdf", height = 4, width = 4)
print(gg.mat)
dev.off()



