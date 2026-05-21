# Note, run this from the testing folder

### Data generation
n = 90
net_def = c(rep("A", 25), rep("B", 15), rep("C", 10))
mu = c(-1, 0.3, 0.5, 0, 0, 0)
tau = c(0.15, 0.4, 0.25, 0, 0, 0)
seed_in = 123

m = length(unique(net_def))
M = m * (m + 1) / 2

tmp_in = fCOuNT_GEN_TEST_DATA(n, net_def, mu, tau, seed_in)

### One-sample t-test
tmp_out = fCOuNT(data = tmp_in$data,
                 test_type = "t.one",
                 net_def = tmp_in$net_def, 
                 fc = tmp_in$fc,
                 seed = 135)
results = data.frame(test=rep("t.one", M * 2)) %>%
  cbind(tmp_out$second_level_results)

### Two-sample t-test
tmp_out = fCOuNT(data = tmp_in$data,
                 test_type = "t.two",
                 form = "fc ~ group2", 
                 net_def = tmp_in$net_def, 
                 fc = tmp_in$fc,
                 seed = 135)
results = data.frame(test=rep("t.two", M * 2)) %>%
  cbind(tmp_out$second_level_results) %>%
  rbind(results, .)

### ANOVA
tmp_out = fCOuNT(data = tmp_in$data,
                 test_type = "anova",
                 form = "fc ~ group3", 
                 net_def = tmp_in$net_def, 
                 fc = tmp_in$fc,
                 seed = 135)
results = data.frame(test=rep("anova", M * 2)) %>%
  cbind(tmp_out$second_level_results) %>%
  rbind(results, .)

### Linear regression
tmp_out = fCOuNT(data = tmp_in$data,
                 test_type = "regression",
                 form = "fc ~ x", 
                 net_def = tmp_in$net_def, 
                 fc = tmp_in$fc,
                 seed = 135)
results = data.frame(test=rep("regression", M * 2)) %>%
  cbind(tmp_out$second_level_results) %>%
  rbind(results, .)

# results_benchmark = results
# save(results_benchmark, file="benchmark_50node_3net_01.rdata")
load("benchmark_50node_3net_01.rdata")
if(!identical(results, results_benchmark)) {
  stop("Error! Benchmark: 50node, 3net, 01 failed!!!")
}
