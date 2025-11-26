net_def = c(rep("A", 25), rep("B", 15), rep("C", 10))
tmp = XXX_generate_test_data(100, 50, net_def, c(1, 0, 0, 0, 0, 0), c(0.5, 0, 0, 0, 0, 0), 123)
data = tmp$data
fc = tmp$fc

saveRDS(tmp, "~/Fellowship/projects/hc/fMRI_network_higher_criticism/testing/testdata_ttest_3network.RDS")
