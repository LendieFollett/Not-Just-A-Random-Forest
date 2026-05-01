---
title: Not Just a Random Forest: Using Bayesian Additive Regression Trees to Estimate Willingness to Pay to Protect Critical Habitat
contributors:
  - Lendie Follett
  - Brian Vander Naald
---

## Overview

The code in this replication package constructs the empirical analysis and simulation study using R. 


The simulation study code (results_simulated_data.R), which uses parallelization techniques, takes several days to run. The empirical study takes less than an hour to run.

## Data Availability and Provenance Statements


### Statement about Rights

- [x] I certify that the author(s) of the manuscript have legitimate access to and permission to use the data used in this manuscript. 
- [x] I certify that the author(s) of the manuscript have documented permission to redistribute/publish the data contained within this replication package. Appropriate permission are documented in the LICENSE.txt file file.

### (Optional, but recommended) License for Data

> INSTRUCTIONS: Most data repositories provide for a default license, but do not impose a specific license. Authors should actively select a license. This should be provided in a LICENSE.txt file, separately from the README, possibly combined with the license for any code. Some data may be subject to inherited license requirements, i.e., the data provider may allow for redistribution only if the data is licensed under specific rules - authors should check with their data providers. For instance, a data use license might require that users - the current author, but also any subsequent users - cite the data provider. Licensing can be complex. Some non-legal guidance may be found [here](https://social-science-data-editors.github.io/guidance/Licensing_guidance.html). For multiple licenses within a data package, the `LICENSE.txt` file might contain the concatenation of all the licenses that apply (for instance, a custom license for one file, plus a CC-BY license for another file).
>
> INSTRUCTIONS: In many cases, it is not up to the creator of the replication package to simply define a license, a license may be *sticky* and be defined by the original data creator. Journals may also require that the license to be part of a certain family of licenses. Always check the journal's  submission guidelines for any such requirements.

*Example:* The data are licensed under a Creative Commons/CC-BY-NC license. See LICENSE.txt for details.


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
  - `tidyr` (0.8.3)
  - `rdrobust` (0.99.4)
  - the file "`src/env_setup.R`" will install all dependencies (latest version), and should be run once prior to running other programs.

### Controlled Randomness

Randomness is introduced in generating the simulated datasets (covariates and error terms), train/test splits, and cross validation splits. Random seed is set at line 58, 248, 358, of program predict_simulated_data.R.

### Memory, Runtime, Storage Requirements

> INSTRUCTIONS: Memory and compute-time requirements may also be relevant or even critical. Some example text follows. It may be useful to break this out by Table/Figure/section of processing. For instance, some estimation routines might run for weeks, but data prep and creating figures might only take a few minutes. You should also describe how much storage is required in addition to the space visible in the typical repository, for instance, because data will be unzipped, data downloaded, or temporary files written.

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

## Description of programs/code

> INSTRUCTIONS: Give a high-level overview of the program files and their purpose. Remove redundant/ obsolete files from the Replication archive.

- Programs in `programs/01_dataprep` will extract and reformat all datasets referenced above. The file `programs/01_dataprep/main.do` will run them all.
- Programs in `programs/02_analysis` generate all tables and figures in the main body of the article. The program `programs/02_analysis/main.do` will run them all. Each program called from `main.do` identifies the table or figure it creates (e.g., `05_table5.do`).  Output files are called appropriate names (`table5.tex`, `figure12.png`) and should be easy to correlate with the manuscript.
- Programs in `programs/03_appendix` will generate all tables and figures  in the online appendix. The program `programs/03_appendix/main-appendix.do` will run them all. 
- Ado files have been stored in `programs/ado` and the `main.do` files set the ADO directories appropriately. 
- The program `programs/00_setup.do` will populate the `programs/ado` directory with updated ado packages, but for purposes of exact reproduction, this is not needed. The file `programs/00_setup.log` identifies the versions as they were last updated.
- The program `programs/config.do` contains parameters used by all programs, including a random seed. Note that the random seed is set once for each of the two sequences (in `02_analysis` and `03_appendix`). If running in any order other than the one outlined below, your results may differ.

### (Optional, but recommended) License for Code

> INSTRUCTIONS: Most journal repositories provide for a default license, but do not impose a specific license. Authors should actively select a license. This should be provided in a LICENSE.txt file, separately from the README, possibly combined with the license for any data provided. Some code may be subject to inherited license requirements, i.e., the original code author may allow for redistribution only if the code is licensed under specific rules - authors should check with their sources. For instance, some code authors require that their article describing the econometrics of the package be cited. Licensing can be complex. Some non-legal guidance may be found [here](https://social-science-data-editors.github.io/guidance/Licensing_guidance.html).

The code is licensed under a MIT/BSD/GPL [choose one!] license. See LICENSE.txt file for details.

## Instructions to Replicators

> INSTRUCTIONS: The first two sections ensure that the data and software necessary to conduct the replication have been collected. This section then describes a human-readable instruction to conduct the replication. This may be simple, or may involve many complicated steps. It should be a simple list, no excess prose. Strict linear sequence. If more than 4-5 manual steps, please wrap a main program/Makefile around them, in logical sequences. Examples follow.

- Run `src/env_setup.R` once on a new system to set up the working environment. 
- Download the data files referenced above. Each should be stored in the prepared subdirectories of `raw/`, in the format that you download them in. 
- Edit `src/empirical_application.R` to adjust the default path. Note that this file sources the `src/functions.R` file.
- Run `src/empirical_application` to run all steps in sequence.

### Details on various programs

- `src/functions.R` is necessary to run the estimation algorithms for the empirical application and the simulation study (but is sourced automatically in the respective files)
- `src/Simulation Study Code/predict_simulated_data.R` runs the simulation study
- `src/Simulation Study Code/results_simulated_data.R` produces the tables and plots for the simulation study
- `src/empirical_application.R` produces the plots for the empirical study
- `src/Illustrative Plots Code/figures_for_latex.R` and `src/Illustrative Plots Code/logit_vs_tree_plot.R` produce plots created for explanatory purposes (outside of simulation study or empirical application)

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

