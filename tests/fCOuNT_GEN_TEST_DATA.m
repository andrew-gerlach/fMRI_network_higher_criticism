function [data, fc] = fCOuNT_GEN_TEST_DATA(n, k, net_def, mu, tau, seed, fn_label)
% Function for generating test data
% In:
%   n         - number of subjects
%   k         - number of nodes in parcellation
%   net_def   - network definition for nodes (length k)
%   mu        - effect strength (1 per network pair)
%   tau       - effect sparsity (1 per network pair)
%   seed      - seed for replicability
%   fn_label  - filename to save data to
%
% Out:
%   data - table with subject and group
%   fc   - connectivity matrices (n x k x k)

    % set seed for replicability
    rng(seed);

    % data table with:
    subj  = (1:n)';
    x = [normrnd(0, 1, [2 * n / 3, 1]); normrnd(1, 1, [n / 3, 1])];
    group2 = categorical([zeros(n / 2, 1); ones(n / 2, 1)]);
    group3 = categorical([zeros(n / 3, 1); ones(n / 3, 1); 2 * ones(n / 3, 1)]);
    age = normrnd(45, 8, [n, 1]);
    sex = categorical(repmat(["F"; "M"], n / 2, 1));
    site = categorical(repmat(["A"; "B"; "C"], n / 3, 1));
    fc_fn = "testdata/subj_" + compose('%03d', subj) + "/subj_" + compose('%03d', subj) + "_" + fn_label + ".mat";
    data  = table(subj, x, group2, group3, age, sex, site, fc_fn);

    % number of unique entries in upper triangle
    K = k * (k - 1) / 2;

    % initialize connectivity matrices
    fc = NaN(n, k, k);

    % generate random symmetric matrices
    for i = 1 : n

        % initialize subject matrix
        mat = zeros(k, k);

        % random values for upper triangle
        ut = triu(true(k), 1);
        mat(ut) = randn(K, 1);

        % mirror to lower triangle
        mat = mat + mat';

        % set diagonal to one
        mat(1:k+1:end) = 1;

        % store in stacked array
        fc(i, :, :) = mat;

    end

    % extract networks
    networks = unique(net_def);
    m = numel(networks);                % number of networks
    M = m * (m + 1) / 2;                % number of network pairs (not directly used)

    net_pair = 0;

    % loop over network pairs
    for i = 1 : m
        for j = i : m

            net_pair = net_pair + 1;

            if mu(net_pair) == 0
                % skip if no signal in this network pair
                continue;
            else
                % determine number of node pairs in network pair
                ni = sum(net_def == networks(i));
                nj = sum(net_def == networks(j));

                if i == j
                    K_net = ni * (nj - 1) / 2;
                else
                    K_net = ni * nj;
                end
            end

            % inject sparse/weak signal
            n_inject = round(tau(net_pair) * K_net);

            for l = 1:n_inject

                % random indices from the two networks
                idx1 = randsample(find(net_def == networks(i)), 1, false);
                idx2 = randsample(find(net_def == networks(j)), 1, false);

                % add signal to group 1 but not group 0
                fc(:, idx1, idx2) = fc(:, idx1, idx2) + data.x * mu(net_pair);

                % enforce symmetry
                fc(:, idx2, idx1) = fc(:, idx1, idx2);

            end

        end
    end

    % save matrices
    for i = 1 : n
        fc_matrix = squeeze(fc(i, :, :));
        mkdir(fileparts(data.fc_fn{i}));
        save(data.fc_fn{i}, "fc_matrix")
    end

    % save data table
    data_fn = "testdata/testdata_" + fn_label + ".csv";
    writetable(data, data_fn)

end
