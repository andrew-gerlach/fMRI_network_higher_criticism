# Note, run this from the testing folder

### Data generation
n = 60
nt = 5
net_def = c(rep("A", 25), rep("B", 15), rep("C", 10))
mu = c(-1, 0.3, 0.5, 0, 0, 0)
tau = c(0.15, 0.4, 0.25, 0, 0, 0)
seed_in = 123

m = length(unique(net_def))
M = m * (m + 1) / 2

tmp_in = fCOuNT_GEN_MEM_TEST_DATA(n, nt, net_def, mu, tau, seed_in)

### Data checks
data = tmp_in$data
get_fc = function(net1, net2) {
  fc_mean = rep(NA, n * nt)
  if(net1 == net2) {
    idx = which(net_def == net1)
    for(i in 1 : n) {
      for(t in 1 : nt) {
        tmp = tmp_in$fc[(i - 1) * nt + t, idx, idx]
        fc_mean[(i - 1) * nt + t] = mean(tmp[upper.tri(tmp)])
      }
    }
  } else {
    idx1 = which(net_def == net1)
    idx2 = which(net_def == net2)
    for(i in 1 : n) {
      for(t in 1 : nt) {
        tmp = tmp_in$fc[(i - 1) * nt + t, idx1, idx2]
        fc_mean[(i - 1) * nt + t] = mean(tmp)
      }
    }
  }
  return(fc_mean)
}
data$fcAA = get_fc("A", "A")
data$fcAB = get_fc("A", "B")
data$fcAC = get_fc("A", "C")
data$fcBB = get_fc("B", "B")
data$fcBC = get_fc("B", "C")
data$fcCC = get_fc("C", "C")
data %>% ggplot(aes(factor(t), fcAA, fill=group)) + geom_boxplot()

### Continuous variable
tmp_out = fCOuNT(data = tmp_in$data,
                 test_type = "mlr",
                 form = "fc ~ x + t + (1 | subj)",
                 var = "x",
                 net_def = tmp_in$net_def, 
                 fc = tmp_in$fc,
                 nsim = 1E6,
                 seed = 135)
results = data.frame(test=rep("x", M * 2)) %>%
  cbind(tmp_out$second_level_results)

### Categorical variable
tmp_out = fCOuNT(data = tmp_in$data,
                 test_type = "mlr",
                 form = "fc ~ group + t + (1 | subj)",
                 var = "group",
                 net_def = tmp_in$net_def, 
                 fc = tmp_in$fc,
                 nsim = 1E6,
                 seed = 135)
results = data.frame(test=rep("g", M * 2)) %>%
  cbind(tmp_out$second_level_results) %>%
  rbind(results, .)

### Time variable
tmp_out = fCOuNT(data = tmp_in$data,
                 test_type = "mlr",
                 form = "fc ~ x + t + (1 | subj)",
                 var = "t",
                 net_def = tmp_in$net_def, 
                 fc = tmp_in$fc,
                 nsim = 1E6,
                 seed = 135)
results = data.frame(test=rep("t", M * 2)) %>%
  cbind(tmp_out$second_level_results) %>%
  rbind(results, .)

### Continuous variable
tmp_out = fCOuNT(data = tmp_in$data,
                 test_type = "mlr",
                 form = "fc ~ x * t + (1 | subj)",
                 var = "interaction",
                 net_def = tmp_in$net_def, 
                 fc = tmp_in$fc,
                 nsim = 1E6,
                 seed = 135)
results = data.frame(test=rep("x_int", M * 2)) %>%
  cbind(tmp_out$second_level_results) %>%
  rbind(results, .)

### Categorical variable
tmp_out = fCOuNT(data = tmp_in$data,
                 test_type = "mlr",
                 form = "fc ~ group * t + (1 | subj)",
                 var = "interaction",
                 net_def = tmp_in$net_def, 
                 fc = tmp_in$fc,
                 nsim = 1E6,
                 seed = 135)
results = data.frame(test=rep("g_int", M * 2)) %>%
  cbind(tmp_out$second_level_results) %>%
  rbind(results, .)