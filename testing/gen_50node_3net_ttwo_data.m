% Note, run this from the testing folder
net_def = categorical([repmat("A", 1, 25), repmat("B", 1, 15), repmat("C", 1, 10) ]);
[~, ~] = fCOuNT_generate_test_data(100, 50, net_def, [0.3, 0.2, 0.5, 0, 0, 0], [0.1, 0.3, 0.1, 0, 0, 0], "ttwo", 311, "50node_3net");
