# extdyn: A Fast Computational Method for Extinction-Date Estimation with Mixed-Certainty Sightings

## Overview

This repository provides R code and example data for implementing a new approach to extinction-date estimation when some sightings in a species’ record are uncertain. The approach resamples observational data according to sighting reliability scores and processes that resampled record with any existing extinction-date estimator (EDE). This makes it easy to incorporate ambiguous (non-physical) observations in a transparent, probabilistic way, rather than arbitrarily including or excluding them.

The method is described in the paper:
“A fast re-sampling method for using reliability ratings of sightings with extinction-date estimators,” by Barry W. Brook, Jessie C. Buettel, and Ivan Jarić. Published in *Ecology*, Volume **100**, Issue 9, 2019, e02787. DOI: https://doi.org/10.1002/ecy.2787.

All the datasets and R scripts needed to replicate the results in that paper are included. The code is modular, enabling straightforward adaptation and extension for new EDEs or further custom analyses.

## Background

Estimating the date a species went extinct (or the probability that it still persists) typically uses the pattern of sighting records. Many existing methods assume those sightings are correct observations of the focal species. This assumption can be problematic because a fraction of sightings will come from unverifiable or anecdotal sources. Our resampling approach addresses that issue by assigning each sighting a reliability probability. We then re-sample many thousands of times from the full record, each time deciding whether to retain or reject a sighting based on its probability of being true. The resulting sample sets are passed to an EDE—such as the well-known Solow (1993) or Roberts and Solow’s OLE—yielding a distribution of possible extinction times. This distribution provides straightforward ways to compute median or mean extinction dates, confidence intervals, and the probability that the species persists after a given date.

## Repository Contents

The repository has three main folders: examples, src, and the root-level README. It also contains data files embedded in the scripts, which can be easily modified.

1. examples/  
   Contains example scripts, such as ede_examples.R, that illustrate how to load different species’ sighting data, assign reliability scores, and run the extinction-date estimators.

2. src/  
   Houses the core R functions that implement the method, along with several well-known EDEs:
   - ede_functions.r defines the main resampling approach (bbj.2018) and other extinction-date estimators (e.g. Solow 1993, OLE).  
   - ede_method.r shows how to tie everything together: loading data, combining multiple sightings within a single year probabilistically, resampling according to assigned reliabilities, running various EDE methods in parallel, and visualizing outputs.  
   - ede_sensitivity_functions.r and ede_sensitivity_method.r provide scripts that generate simulated sighting records under varying reliability assumptions and detectability scenarios. These scripts test how the method performs under controlled conditions (where the “true” extinction date is known).

3. README.md (this file)  
   Describes the purpose of the repository, lists dependencies, and explains how to run the code and replicate the published results.

## Dependencies and Setup

All scripts assume a working installation of R (≥3.5) and a standard set of packages for parallel computing and other tasks. Packages used include parallel, sExtinct, spatExtinct (for some advanced Bayesian methods), and base R libraries like stats and graphics. You can install missing packages via install.packages("packagename").

## Running the Examples and Replicating Results

To reproduce the results in the paper and run the included examples, open R in the root directory of this repository and source the relevant scripts. A typical workflow involves:

1. Loading the function definitions with:
   source("src/ede_functions.r")
   source("src/ede_examples.r")

2. Choosing a species sighting record (e.g., “IW” for the Ivory-billed Woodpecker) and a set of reliability values (e.g., “B” for a baseline set). Example usage is in src/ede_method.r. That file shows how to:
   - Combine repeated sightings in a single year into one probability.  
   - Call the resampling routine (bbj.2018) with thousands of iterations.  
   - Obtain the extinction date distribution and compute confidence bounds.

3. Looking at examples/ede_examples.R for demonstration data on a range of species, including known “Lazarus” taxa and species considered definitively extinct.  

4. Exploring the sensitivity scripts in src/ede_sensitivity_functions.r and src/ede_sensitivity_method.r to see how the method behaves on simulated sighting records with a known “true” extinction date. This allows you to evaluate estimation bias and coverage.

## Modifying and Extending

The code is modular. You can add your own extinction-date estimators or adjust existing ones in ede_functions.r. You can also extend or change the reliability scoring approach. For instance, you might group sightings into classes (e.g., physical, expert, photographic, anecdotal) or assign a different probability to each individual sighting. Then you can specify those probabilities in the data loading function, run a large resampling, and produce an updated extinction-date inference.

## Data

Datasets used to replicate the paper’s empirical examples are written as R lists. They capture the species name or code, the years of sightings, and a reliability vector for each sighting. These are found in the examples/ folder (ede_examples.R) and partially within the code blocks in src/ede_method.r. For real species, reliability assignments came from published expert ratings or from meta-analyses. For simulations, the reliability is assigned randomly or systematically to explore method sensitivity.

## Reference

Brook, B. W., Buettel, J. C., & Jarić, I. 2019. A fast re-sampling method for using reliability ratings of sightings with extinction-date estimators. *Ecology* **100**(9): e02787. https://doi.org/10.1002/ecy.2787

Additional citations used or referenced in the scripts (Solow 1993, Roberts and Solow 2003, etc.) can be found in the paper.

## License

Unless otherwise stated, this code is made available for academic and research purposes. You are free to modify and distribute it. If you use it in published work, please cite the above paper. (You can, of course, add an explicit license file if you prefer a standard open-source license.)

## Contact

The authors welcome questions, suggestions, and feedback. Please feel free to open issues on GitHub or reach out directly to Barry Brook (barry.brook(at)utas.edu.au) or the coauthors.

