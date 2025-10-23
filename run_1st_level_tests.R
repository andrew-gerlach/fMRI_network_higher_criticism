# ARoG: temporary script for running 1st level tests
#       assumes data and fc exist in workspace

k = dim(fc)[2]
K = k * (k - 1) / 2

first_level_results = data.frame(node1=numeric(K),
                                 node2=numeric(K),
                                 direction=character(K),
                                 test_statistic=numeric(K),
                                 p=numeric(K))

idx = 0
for(i in 1 : (k - 1)) {
  for(j in (i + 1) : k) {
    idx = idx + 1
    first_level_results$node1[idx] = i
    first_level_results$node2[idx] = j
    mod = t.test(fc[data$group == 0, i, j], fc[data$group == 1, i, j])
    first_level_results$p[idx] = mod$p.value
  }
}