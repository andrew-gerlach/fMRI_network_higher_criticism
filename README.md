# fMRI Connectome Omnibus Network Testing

This packages implements a two level omnibus approach called Higher Criticism
for use in making connectome-basd inferences at the network level. This approach
utilizes the rich data contained in functional connectivity matrices with first-
level mass univariate testing. The second-level testing organizes these matrices
along network boundaries and asks the fundamental question "in this network
pair, is there evidence for a signal?" Higher criticism is optimal for detecting
rare and weak effects, making it ideal for investigating brain associations of
behavior. Further, this method is flexible to the questions it can answer,
requiring only that the first level tests result in a typically distributed
p-value under null conditions. Essentially, many-nodes pairs within a network-
pair are treated as observations within that network-pair. This allows us to
investigate network signatures without averaging over large swaths of cortex,
which is known to often wipe out the subtle effects present in connectome data.

# Github installation

```r
# Ensure devtools is loaded
library(devtools)

# Install (once)
devtools::install_github("andrew-gerlach/fCOuNT")

# Load (every session)
library(fCOuNT)
```

# Usage

The fCOuNT routine is designed as wrapper function to control most standard use
cases. The minimum input for the routine is...

1. Study data (`data`): data frame containing all relevant variables *except*
connectivity data. Note, the fCOuNT routine can read data directly from file if
a filename is provided, but we strongly recommend against this to ensure
variable types are correctly formatted (e.g., factors).
2. Functional connectivity matrices: these can be input as 3D array (`fc`, subject x
node x node) or read directly from file if one column of the study data contains
the full paths to the connectivity matrix files (`fc_col_name`). There is
currently support for reading connectivity data from csv, R data types, or .m
files. If reading from .m files, the name of the matrix object in matlab must
also be provided (`fc_obj_name`).
3. Network definitions (`net_def`): node-wise network definitions can be input as a vector
of labels or read directly from file. If the file contains more than one column
then the column with the network definitions must be provided (`net_def_col`)
4. Information about the first-level statistical test of interest:
  - Type of statistical test(`test_type`): Currently, the package has built-in support for
    t-tests (`t.one`, `t.two`), ANOVA (`anova`), and linear regression (`lr`). Please
    request modules to be added for desired statistical tests directly from the developer.  
    There is also a `custom` option that allows the user to provide a routine
    (`custom_fun`) for performing the first level tests. This routine must accept the
    following arguments: `fc_vec` (subject-wise vector of single entry from FC matrix),
    `data`, `form` (see below), and `var_idx` (index of the variable interest in the output).
    `form` and `var_idx` do not necessarily need to be used by the routine if this information
    is hard-coded into `custom_fun`. Additional input variables cannot be passed to this
    routine. The output must be a list consisting of `test_statistic`, `p_low`, and `p_high`.
    For tests without a direction (e.g., ANOVA), only values for `p_low` should be entered and
    `p_high` should be set to NA. For tests with a direction, **ONE-SIDED** p-values should be
    calculated in each direction to allow for directional inference.
  - Formula for the statistical test (`form`): Required for all statistical tests except
    one-sample t-tests and custom tests. The formula **MUST** contain a variable called
    `fc` as either an independent or the dependent variable.
  - Variable of interest (`var`): The variable of interest for inference should should
    be supplied. If this is not present, it will be assumed that the first
    variable in the formula is the variable of interest (if FC is the outcome)
    or FC is the variable of interest if `fc` is included as an independent variable.

Example usage with pre-loaded FC matrices and network definitions:

```r
results = fCOuNT(data = data,     # study data frame
  fc = fc,                        # 3D FC matrix
  net_def = net_def,              # vector of node-wise network assignments
  test_type = "t.two",            # two group t-test
  form = "fc ~ group",            # formula for t-test, group must be column of data
  var = "group")                  # variable of interest for inference
```

Example usage with FC matrices read from matlab files and network defintions
read from Excel spreadsheet:

```r
results = fCOuNT(data = data,     # study data frame
  fc_col_name = "fc_file"         # column in data with path to subject's FC file
  fc_obj_name = "conn_matrix"     # name of matlab object containing FC matrix when loaded from .m file
  net_def = "path/net_defs.xlsx"  # path to file containing node-wise network assignemnts
  net_def_col = "yeo7_net"        # name of column in net_def file containing network assignment
  test_type = "lr",               # linear regression
  form = "fc ~ age + sex + x"     # regression formula; age, sex, and x contained in data
  var = "x")                      # variable of interest for inference
```

# Outputs

The `fCOuNT` routine contains 4 objects in the output:
1. First-level test results (`first_level_results`): a data-frame containing
the results of the mass univariate first level tests. This is intended primarily
for verification and debugging purposes. Highly recommend running the first-level
tests by hand for a few entries of the FC matrix and verifying the output is as
expected.
2. Second-level test results (`second_level_results`): a data-frame containing
the primary analysis of network-pair-wise inference. The table will consist of
`K * (K + 1)` entries for `K` networks. There is one entry in each direction
(low and high) for each network-pair, including intranetwork.
3. Quality control plots (`qc_plots`): a list of plots that can be used to check
the underlying behavior and assumption of HC. This list will be `K * (K + 1) / 2`,
one for each network-pair. Each entry will contain a list of 4 plots: a histogram
of the first-level p-values for the low direction, a plot of the HC statistic for
the low direction, a histogram of the first-level p-values for the high direction,
and a plot of the HC statistic for the high direction. The histograms are important
for ensuring that the underlying assumption of approximately uniform distribution
of p-values is present (shown in red). The HC plots show the critical value in red.
4. Chord diagram of results (`results_plots`): a visualization of the second-level
results. Significant low network pairs are shown with blue links, significant high
network pairs are shown with red links, and non-directional pairs are shown with
purple links.

# Package improvements

Current plans to improve the fCOuNT package are:

1. Automated network definitions for common parcellations: Currently network
definitions are defined by the user. This allows for a maximum flexibility in
selecting both the nodal parcellation that was used to generate the FC matrices
and the network parcellation that is used to define the node network assignments.
However, we will add automated network assignments for Schaefer and Shen
nodal parcellations and Yeo 7 and 17 network parcellations. If you would like
other nodal and network parcellations added, please let me know.
2. Documentation: I'm currently working on a vignette to make this package as
accessible as possible for users of all backgrounds. Please let me know what
could use more explanation!

I welcome any and all feedback and will be constantly working to improve this
package throughout 2026. Please let me know how I can make fCOuNT work for you!

Andrew Gerlach
Assistant Professor of Psychiatry and Bioengineering
University of Pittsburgh
gerlachar@upmc.edu
