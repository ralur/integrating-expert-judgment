# Integrating Expert Judgment and Algorithmic Decision Making

This repository contains code and data to reproduce the experiments in our paper "Integrating Expert Judgment and Algorithmic Decision Making: An Indistinguishability Approach" (https://arxiv.org/abs/2410.08783).

## Dependencies

### R Dependencies
- boot
- tidyverse
- mltools
- future
- future.apply
- glue
- gridExtra
- Cairo

### Python Dependencies

Defined in `requirements.txt`. Install with `pip install -r requirements.txt`.

## Repository Structure

#### ./code
Contains the scripts to reproduce the experiments in the paper.

- mc_indistinguishability.ipynb: notebook for reproducing results in Sections 5.2  and 5.3. Depends on LSBoost.py and helper_functions.py, which contain the implementation of the level set boosting algorithm proposed in [1]. For full documentation, see https://github.com/Declancharrison/Level-Set-Boosting. 
    - As of 10/2024, there is a known issue using the parallel implementation in this package with MacOS. For additional detail, see "Issues" in https://github.com/Declancharrison/Level-Set-Boosting. 
- figures.R: reproduces the remaining figures in the paper. Depends on outputs from the mc_indistinguishability.ipynb notebook (see below).

#### ./data

Contains both input and output data for each experiment.
- gib_gbs_clean_inputs_normalized.csv contains the input data for the case study of ER triage decisions
- gib_gbs_clean_inputs_normalized_train.csv, gib_gbs_clean_inputs_normalized_test.csv contain the train/test split (generated in figures.R)
- by_group_figure_data_uncorrected.csv, by_group_figure_data_corrected.csv contain the data used to produce Figure 1 (generated in figures.R)
- by_gbs_figure_data.csv contains the data used to produce Figure 2 (generated by figures.R)
- gib_gbs_clean_inputs_normalized_test.csv contains the data needed to produce Figure 4 (generated by mc_indistinguishability.ipynb)

#### ./figures

Contains the figures in the paper. Running mc_indistinguishability.ipynb and figures.R will overwrite the contents of this folder.

## References

[1] Globus-Harris, I., Harrison, D., Kearns, M., Roth, A., & Sorrell, J. (2023). Multicalibration as Boosting for Regression. Fortieth International Conference on Machine Learning.

If you find this repository useful, please cite our work as:

```
@misc{2410.08783,
Author = {Rohan Alur and Loren Laine and Darrick K. Li and Dennis Shung and Manish Raghavan and Devavrat Shah},
Title = {Integrating Expert Judgment and Algorithmic Decision Making: An Indistinguishability Framework},
Year = {2024},
Eprint = {arXiv:2410.08783},
}
```
