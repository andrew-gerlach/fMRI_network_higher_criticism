# Note, run this from the testing folder
tmp = fCOuNT_generate_test_data(100, 50, net_def, c(0.2, 0.2, 0.5, 0, 0, 0), c(0.1, 0.3, 0.1, 0, 0, 0), 123)
tmp$net_def = c(rep("A", 25), rep("B", 15), rep("C", 10))

saveRDS(tmp, "testdata_50node_3net_ttwo.RDS")
