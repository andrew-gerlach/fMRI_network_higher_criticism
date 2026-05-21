# Note, run this from the testing folder
fCOuNT_generate_test_data(n=90,
                          k=50,
                          net_def=c(rep("A", 25), rep("B", 15), rep("C", 10)),
                          test_type="anova",
                          mu=c(0.2, 0.2, 0.4, 0, 0, 0),
                          tau=c(0.2, 0.5, 0.2, 0, 0, 0),
                          seed=13) %>%
  saveRDS("testdata_50node_3net_anova.RDS")
