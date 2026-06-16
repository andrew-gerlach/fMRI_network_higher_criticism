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
devtools::install_github("username/fCOuNT")

# Load (every session)
library(fCOuNT)
```

# Usage

The fCOuNT routine is desgined as wrapper function to control most standard use
cases. The minimum input for the routine is...

1. Study data (`dat`): data frame containing all relevant variables *except*
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
4. Information about the statistical test of interest:
  - Type of statistical test(`test_type`): Currently, the package supports
    t-tests (`t.one`, `t.two`), ANOVA (`anova`), ANCOVA (`ancova`), and linear
    regression (`lr`). The option to define custom statistical tests
    will be added in the future; in the meantime please request modules to be
    added for desired statistical tests directly from the developer.
  - Formula for the statistical test (`form`): Required for all statistical tests except
    one-sample t -tests. The formula **MUST** contain a variable called `fc` as
    either an independent or the dependent variable.
  - Variable of interest (`var`): The variable of interest for inference should should
    be supplied. If this is not present, it will be assumed that the first
    variable in the formula is the variable of interest (if FC is the outcome)
    or FC is the variable of interest if `fc` is included as an independent variable.

# Package improvements

Current plans to improve the fCOuNT package are:

1. Automated network definitions for common parcellations: Currently network
definitions are defined by the user. This allows for a maximum flexibility in
selecting both the nodal parcellation that was used to generate the FC matrices
and the network parcellation that is used to define the node network assignments.
However, we will add automated network assignments for Schaefer and Shen
nodal parcellations and Yeo 7 and 17 network parcellations. If you would like
other nodal and network parcellations added, please let me know.
2. Parallelization improvements: Currently, only first-level tests are able to
take advantage of parallelization and only with unix-based systems (e.g. macOS).
The p-value calculation for the higher criticism statistic will also be
parallelized and parallel capabilities will be added for Windows.
3. Flexible first level tests: Currently, only the predefined first level tests
(t-tests, ANOVA, and linear regression) can be used, but future extension will
allow for a user-defined module to implement other tests. Additionally, if you'd
like a specific test type added to the built-in options, please let me know.
4. Documentation: I'm currently working on a vignette to make this package as
accessible as possible for users of all backgrounds. Please let me know what
could use more explanation!

I welcome any and all feedback and will be constantly working to improve this
package throughout 2026. Please let me know how I can make fCOuNT work for you!

Andrew Gerlach
Assistant Professor of Psychiatry and Bioengineering
University of Pittsburgh
gerlachar@upmc.edu
