# Note, run this from the testing folder
tmp = fCOuNT_GEN_TEST_DATA(n=90,
                           k=50,
                           net_def=c(rep("A", 25), rep("B", 15), rep("C", 10)),
                           mu=c(0.2, 0.2, 0.5, 0, 0, 0),
                           tau=c(0.1, 0.3, 0.1, 0, 0, 0),
                           seed=123)
# saveRDS(tmp, "testdata_50node_3net_ttwo.RDS")

data = tmp$data
fc = tmp$fc
net_def = tmp$net_def
rm(tmp)

tmp = fCOuNT(data = data, test_type, form, var, controls, net_def, net_def_col, fc, fc_col_name, fc_obj_name, k1, emp, nsim, qc_plot, results_plot, mcc, font_size, label_height, seed)