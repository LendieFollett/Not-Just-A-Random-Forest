---
title: "Not Just a Random Forest: Using Bayesian Additive Regression Trees to Estimate Willingness to Pay to Protect Critical Habitat"
contributors:
  - Lendie Follett
  - Brian Vander Naald
---

## Overview

The code in this replication package constructs the empirical analysis and simulation study using R. 


The simulation study is computationally intensive and uses parallel processing. On a modern laptop with approximately 10 CPU cores and 16 GB RAM, the simulation requires approximately 3–7 days to complete depending on processor speed and available cores.

Users wishing to verify functionality rather than reproduce all results exactly may reduce the number of simulation replications inside `predict_simulated_data.R`.

## Data Availability and Provenance Statements


### Statement about Rights

- [x] I certify that the author(s) of the manuscript have legitimate access to and permission to use the data used in this manuscript. 
- [x] I certify that the author(s) of the manuscript have documented permission to redistribute/publish the data contained within this replication package. Appropriate permission are documented in the LICENSE.txt file file.

### License for Data

The survey dataset included in this replication package was collected by the authors and is made available under the Creative Commons Attribution 4.0 International (CC-BY 4.0) license.

Users are free to share and adapt the data provided appropriate attribution is given. See LICENSE.txt for details.

### Summary of Availability

- [x] All data **are** publicly available.
- [ ] Some data **cannot be made** publicly available.
- [ ] **No data can be made** publicly available.

### Accessing Data

Datafile:  `raw/BART_restricted.csv`

### Software Requirements

- All computations are done using the R language. 

- [x] The replication package contains one or more programs to install all dependencies and set up the necessary directory structure. 


- R 4.5.1 (2025-06-13)-- "Great Square Root"
  - the file "`src/env_setup.R`" will install all dependencies (latest version), and should be run once prior to running other programs.

### Controlled Randomness

Randomness is introduced in generating the simulated datasets (covariates and error terms), train/test splits, and cross validation splits. Random seed is set at line 58, 248, 358, of program predict_simulated_data.R.


#### Summary time to reproduce

Approximate time needed to reproduce the simulation study on a standard desktop machine:

- [ ] <10 minutes
- [ ] 10-60 minutes
- [ ] 1-2 hours
- [ ] 2-8 hours
- [ ] 8-24 hours
- [ ] 1-3 days
- [x] 3-14 days
- [ ] > 14 days

Approximate time needed to reproduce the empirical study on a standard desktop machine:

- [ ] <10 minutes
- [x] 10-60 minutes
- [ ] 1-2 hours
- [ ] 2-8 hours
- [ ] 8-24 hours
- [ ] 1-3 days
- [ ] 3-14 days
- [ ] > 14 days


#### Summary of required storage space

Approximate storage space needed:

- [ ] < 25 MBytes
- [ ] 25 MB - 250 MB
- [x] 250 MB - 2 GB
- [ ] 2 GB - 25 GB
- [ ] 25 GB - 250 GB
- [ ] > 250 GB

- [ ] Not feasible to run on a desktop machine, as described below.

#### Computational Details

The code was last run on a 10-core Apple Silicon (M4) MacBook Air running macOS 26.4.1 with 16 GB of memory and approximately 133 GB of available disk space.

### Details on various programs

- `src/functions.R` is necessary to run the estimation algorithms for the empirical application and the simulation study (but is sourced automatically in the respective files)
- `src/Simulation Study Code/predict_simulated_data.R` runs the simulation study
- `src/Simulation Study Code/results_simulated_data.R` produces the tables and plots for the simulation study
- `src/empirical_application.R` produces the plots for the empirical study
- `src/Illustrative Plots Code/figures_for_latex.R` and `src/Illustrative Plots Code/logit_vs_tree_plot.R` produce plots created for explanatory purposes (outside of simulation study or empirical application)

### (Optional, but recommended) License for Code

> INSTRUCTIONS: Most journal repositories provide for a default license, but do not impose a specific license. Authors should actively select a license. This should be provided in a LICENSE.txt file, separately from the README, possibly combined with the license for any data provided. Some code may be subject to inherited license requirements, i.e., the original code author may allow for redistribution only if the code is licensed under specific rules - authors should check with their sources. For instance, some code authors require that their article describing the econometrics of the package be cited. Licensing can be complex. Some non-legal guidance may be found [here](https://social-science-data-editors.github.io/guidance/Licensing_guidance.html).

The code is licensed under a MIT/BSD/GPL [choose one!] license. See LICENSE.txt file for details.

## Instructions to Replicators

**To replicate Simulation Study:**

- Run `src/env_setup.R` once on a new system to set up the working environment. 
- Run `src/Simulation Study Code/predict_simulationed_data.R` to run all steps in sequence. Note that this file sources the `src/functions.R` file.
- Run `src/Simulation Study Code/results_simulated_data.R` to produce plots and tables.

**To replicate Empirical Study:**

- Run `src/env_setup.R` once on a new system to set up the working environment. 
- Download the data files referenced above. Each should be stored in the prepared subdirectories of `raw/`, in the format that you download them in. 
- Edit `src/empirical_application.R` to adjust the default path. Note that this file sources the `src/functions.R` file.
- Run `src/empirical_application` to run all steps in sequence.



## List of tables and programs


> INSTRUCTIONS: Your programs should clearly identify the tables and figures as they appear in the manuscript, by number. Sometimes, this may be obvious, e.g. a program called "`table1.do`" generates a file called `table1.png`. Sometimes, mnemonics are used, and a mapping is necessary. In all circumstances, provide a list of tables and figures, identifying the program (and possibly the line number) where a figure is created.
>
> NOTE: If the public repository is incomplete, because not all data can be provided, as described in the data section, then the list of tables should clearly indicate which tables, figures, and in-text numbers can be reproduced with the public material provided.

The provided code reproduces:

- [x] All numbers provided in text in the paper
- [x] All tables and figures in the paper
- [ ] Selected tables and figures in the paper, as explained and justified below.


| Figure/Table #    | Program                  | Line Number | Output file                      | Note                            |
|-------------------|--------------------------|-------------|----------------------------------|---------------------------------|
| Table 1           | 02_analysis/table1.do    |             | summarystats.csv                 ||
| Table 2           | 02_analysis/table2and3.do| 15          | table2.csv                       ||
| Table 3           | 02_analysis/table2and3.do| 145         | table3.csv                       ||
| Figure 1          | src/Illustrative Plots Code/logit_vs_tree_plot.R        | |  tree.pdf                               |       |
| Figure 2          | src/Illustrative Plots Code/logit_vs_tree_plot.R      | | tree_vs_lr.pdf                     ||
| Figure 3          | src/empirical_application.R      |  | integral_mcmc1.pdf             |     |
| Figure 4          |  src/empirical_application.R    |             | integral_mcmc.pdf          |     |
| Figure 5          | src/results_simulated_data.R     |             | cor_sym_7.pdf,cor_sym_15.pdf            |     |
| Figure 6          | src/results_simulated_data.R     |             | RMSE_sym_7.pdf,RMSE_sym_15.pdf           |     |
| Figure 7          | src/results_simulated_data.R     |             | bias_sym_7.pdf,bias_sym_15.pdf           |     |
| Figure 8          | BAR CHART?!    |             | percent_yes_responses.pdf            |     |
| Figure 9          | src/empirical_application.R  |             | bart_probit_compare.pdf            |     |
| Figure 10          | src/empirical_application.R  |             | bird_wtp_both.pdf          |     |

