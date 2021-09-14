# Indefinite pronouns optimize the simplicity/informativeness trade-off

This folder contains (i) code and (ii) appendix to the paper 'Indefinite pronouns optimize the simplicity/informativeness trade-off' by [Milica Denić](https://sites.google.com/view/milicadenic/), [Shane Steinert-Threlkeld](https://www.shane.st/) and [Jakub Szymanik](https://jakubszymanik.com/).

The appendix file is [Negative_indefinites_40_lang_data.pdf](https://github.com/milicaden/indefinite-pronouns-simplicity-informativeness/blob/main/Negative_indefinites_40_lang_data.pdf).

In the remainder, we describe the code for Experiments 1 and 2. Experiments 1-i and 2-i, Experiments 1-ii and 2-ii, and Experiments 1-iii and 2-iii work similarly, with the relevant files in their respective folders.

# Scripts and language files

Python and R scripts are in the folder *src*. CSV files needed for scripts to run and generated by them are in the folder *data*.

- [Beekhuizen_priors.R](https://github.com/milicaden/indefinite-pronouns-simplicity-informativeness/blob/main/Experiment_1_and_2/src/Beekhuizen_priors.R) 
  - Description: It extracts the prior probability distribution over flavors from the annotated corpus from [Beekhuizen et al.'s (2017)](http://www.cs.toronto.edu/~barend/data/beekhuizen_watson_stevenson_2017.pdf) study downloaded from [here](https://github.com/dnrb/indefinite-pronouns/tree/master/data), and stores it in [*Beekhuizen_priors.csv*](https://github.com/milicaden/indefinite-pronouns-simplicity-informativeness/blob/main/Experiment_1_and_2/data/Beekhuizen_priors.csv) file.
  - Dependencies: [*beekhuizen_full_set.csv*](https://github.com/milicaden/indefinite-pronouns-simplicity-informativeness/blob/main/Experiment_1_and_2/data/beekhuizen_full_set.csv)

- [Min-desc-length-algo.R](https://github.com/milicaden/indefinite-pronouns-simplicity-informativeness/blob/main/Experiment_1_and_2/src/Min-desc-length-algo.R) 
  - Description: It generates the minimum-length feature-based descriptions of all logically possible indefinite pronouns (in terms of which combination of flavors they can express) and stores them in [*minimum-desc-indef.csv*](https://github.com/milicaden/indefinite-pronouns-simplicity-informativeness/blob/main/Experiment_1_and_2/data/minimum-desc-indef.csv) file.


- [Indefinites_functions.R](https://github.com/milicaden/indefinite-pronouns-simplicity-informativeness/blob/main/Experiment_1_and_2/src/Indefinites_functions.R) 
  - Description: Definitions of a series of useful functions for Experiments 1 and 2.
  - Dependencies: [*Beekhuizen_priors.csv*](https://github.com/milicaden/indefinite-pronouns-simplicity-informativeness/blob/main/Experiment_1_and_2/data/Beekhuizen_priors.csv)

- [Exp1_languages_generation.R](https://github.com/milicaden/indefinite-pronouns-simplicity-informativeness/blob/main/Experiment_1_and_2/src/Exp1_languages_generation.R) 
  - Description: It imports the data file with Haspelmath's 40 natural languages, generates 10000 aritificial languages used in Experiment 1. It stores the languages of Experiment 1 in [*languages_exp1.csv*](https://github.com/milicaden/indefinite-pronouns-simplicity-informativeness/blob/main/Experiment_1_and_2/data/languages_exp1.csv). 
  - Dependencies: [*Indefinites_functions.R*](https://github.com/milicaden/indefinite-pronouns-simplicity-informativeness/blob/main/Experiment_1_and_2/src/Indefinites_functions.R), [*languages_real_40_updated.csv*](https://github.com/milicaden/indefinite-pronouns-simplicity-informativeness/blob/main/Experiment_1_and_2/data/languages_real_40_updated.csv)

- [Exp1_languages_matching.R](https://github.com/milicaden/indefinite-pronouns-simplicity-informativeness/blob/main/Experiment_1_and_2/src/Exp1_languages_matching.R) 
  - Description: It imports the data file with languages of Experiment 1, and performs overlap and coverage matching and stores the matched languages in [*Exp1_languages_matched001_timeout.csv*](https://github.com/milicaden/indefinite-pronouns-simplicity-informativeness/blob/main/Experiment_1_and_2/data/Exp1_languages_matched001_timeout.csv).
  - Dependencies: [*Indefinites_functions.R*](https://github.com/milicaden/indefinite-pronouns-simplicity-informativeness/blob/main/Experiment_1_and_2/src/Indefinites_functions.R), [*languages_exp1.csv*](https://github.com/milicaden/indefinite-pronouns-simplicity-informativeness/blob/main/Experiment_1_and_2/data/languages_exp1.csv)

- [Exp1_languages_cost_complexity.R](https://github.com/milicaden/indefinite-pronouns-simplicity-informativeness/blob/main/Experiment_1_and_2/src/Exp1_languages_cost_complexity.R) 
  - Description: It computes communicative cost and complexity of languages of Experiment 1 matched for overlap and coverage and stores them into [*all_complexity_cost_exp1.csv*](https://github.com/milicaden/indefinite-pronouns-simplicity-informativeness/blob/main/Experiment_1_and_2/data/all_complexity_cost_exp1.csv). 
  - Dependencies: [*Indefinites_functions.R*](https://github.com/milicaden/indefinite-pronouns-simplicity-informativeness/blob/main/Experiment_1_and_2/src/Indefinites_functions.R), [*languages_exp1.csv*](https://github.com/milicaden/indefinite-pronouns-simplicity-informativeness/blob/main/Experiment_1_and_2/data/languages_exp1.csv), [*Exp1_languages_matched001_timeout.csv*](https://github.com/milicaden/indefinite-pronouns-simplicity-informativeness/blob/main/Experiment_1_and_2/data/Exp1_languages_matched001_timeout.csv), [*minimum-desc-indef.csv*](https://github.com/milicaden/indefinite-pronouns-simplicity-informativeness/blob/main/Experiment_1_and_2/data/minimum-desc-indef.csv)


- [Exp2_languages.R](https://github.com/milicaden/indefinite-pronouns-simplicity-informativeness/blob/main/Experiment_1_and_2/src/Exp2_languages.R) 
  - Description: It generates 10 000 artificial languages used in Experiment 1 (5000 Haspel-ok and 5000 Not Haspel-ok languages), computes communicative cost and complexity of these languages and stores them into [*all_complexity_cost_exp2.csv*](https://github.com/milicaden/indefinite-pronouns-simplicity-informativeness/blob/main/Experiment_1_and_2/data/all_complexity_cost_exp2.csv). Finally, it performs synonymy matching and stores the matched languages in [*syn_matched_exp2.csv*](https://github.com/milicaden/indefinite-pronouns-simplicity-informativeness/blob/main/Experiment_1_and_2/data/syn_matched_exp2.csv).
  - Dependencies: [*Indefinites_functions.R*](https://github.com/milicaden/indefinite-pronouns-simplicity-informativeness/blob/main/Experiment_1_and_2/src/Indefinites_functions.R), [*minimum-desc-indef.csv*](https://github.com/milicaden/indefinite-pronouns-simplicity-informativeness/blob/main/Experiment_1_and_2/data/minimum-desc-indef.csv)
 
- [Indef_gen_alg.py](https://github.com/milicaden/indefinite-pronouns-simplicity-informativeness/blob/main/Experiment_1_and_2/src/Indef_gen_alg.py)
  - Description: It runs an evolutionary algorithm selecting for Pareto optimal languages for 100 generations. It stores the complexity and communicative cost measures of the final generation in  [*finalgencostcom.csv*](https://github.com/milicaden/indefinite-pronouns-simplicity-informativeness/blob/main/Experiment_1_and_2/data/finalgencostcom.csv). Finally, it selects dominant languages in terms of complexity and communicative cost from the final generation and the languages used in Experiment 1 and stores them in [*pareto_dominant.csv*](https://github.com/milicaden/indefinite-pronouns-simplicity-informativeness/blob/main/Experiment_1_and_2/data/pareto_dominant.csv).
  - Dependencies: [*all_complexity_cost_exp1.csv*](https://github.com/milicaden/indefinite-pronouns-simplicity-informativeness/blob/main/Experiment_1_and_2/data/all_complexity_cost_exp1.csv), [*minimum-desc-indef.csv*](https://github.com/milicaden/indefinite-pronouns-simplicity-informativeness/blob/main/Experiment_1_and_2/data/minimum-desc-indef.csv), [*allitems.csv*](https://github.com/milicaden/indefinite-pronouns-simplicity-informativeness/blob/main/Experiment_1_and_2/data/allitems.csv),  (a file with all logically possible items), [*Beekhuizen_priors.csv*](https://github.com/milicaden/indefinite-pronouns-simplicity-informativeness/blob/main/Experiment_1_and_2//data/Beekhuizen_priors.csv)

- [Exp1_and_2_pareto.R](https://github.com/milicaden/indefinite-pronouns-simplicity-informativeness/blob/main/Experiment_1_and_2/src/Exp1_and_2_pareto.R) 
  - Description: It imports the data file with dominant languages in terms of complexity and communicative cost and based on them estimates the Pareto frontier for indefinite pronouns. It plots languages of Experiment 1 and languages of Experiment 2 with respect to the frontier. It computes minimum Euclidian distances of languages of Experiment 1 and 2 from the Pareto frontier, and stores them in [*natural_distances_pareto.csv*](https://github.com/milicaden/indefinite-pronouns-simplicity-informativeness/blob/main/Experiment_1_and_2/data/natural_distances_pareto.csv), [*artificial_distances_pareto.csv*](https://github.com/milicaden/indefinite-pronouns-simplicity-informativeness/blob/main/Experiment_1_and_2/data/artificial_distances_pareto.csv), [*Haspok_distances_pareto.csv*](https://github.com/milicaden/indefinite-pronouns-simplicity-informativeness/blob/main/Experiment_1_and_2/data/Haspok_distances_pareto.csv), [*Haspnotok_distances_pareto.csv*](https://github.com/milicaden/indefinite-pronouns-simplicity-informativeness/blob/main/Experiment_1_and_2/data/Haspnotok_distances_pareto.csv). Finally, it establishes that (i) natural languages are closer to the frontier than artificial languages; and (ii) that languages which satisfy Haspelmath's universals are closer to the frontier than languages which do not satisfy them.
  - Dependencies: [*Indefinites_functions.R*](https://github.com/milicaden/indefinite-pronouns-simplicity-informativeness/blob/main/Experiment_1_and_2/src/Indefinites_functions.R), [*syn_matched_exp1.csv*](https://github.com/milicaden/indefinite-pronouns-simplicity-informativeness/blob/main/Experiment_1_and_2/data/syn_matched_exp1.csv), [*syn_matched_exp2.csv*](https://github.com/milicaden/indefinite-pronouns-simplicity-informativeness/blob/main/Experiment_1_and_2/data/syn_matched_exp2.csv), [*pareto_dominant.csv*](https://github.com/milicaden/indefinite-pronouns-simplicity-informativeness/blob/main/Experiment_1_and_2/data/pareto_dominant.csv)
