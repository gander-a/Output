This repository includes the files and scripts used to create the plots featuring in my Master's thesis "Text analysis using colexification networks".
Furthermore, it includes a R Markdown script showing how the text similarity is computed using an example (see SCRIPTS/EXAMPLE)

DATASETS includes the datasets needed for the text similarity analysis (Google Ngram data) and the word similarity datasets used for calibration and validation.
FILES includes temporary results for the computation of the colexification networks and the text similarity metric.
PLOTS includes the figures shown in the results chapter of the thesis.
RESULTS includes the result files needed to generate the figures. The results were computed using the Vienna Scientific Cluster distributed computer system.
SCRIPTS includes all the scripts needed to generate the figures.
	COMPUTATIONS includes helper scripts needed to run the text similarity example file.
	EVALUATION includes the scripts generating the figures.
	EXAMPLE includes an R Markdown script containing an example computing the similarity between two text (both text files are in the folder as well).
	NETWORKS includes scripts to modify the colexification networks and scripts needed to construct the word similarity metric on top of the colexification networks.
SIMILARITIES includes the adjacency matrices including the word similarity values of the colexification network corresponding to different beta parameter (dampening factor) values.
TABLES includes output tables from the word similarity calibration and validation experiments.