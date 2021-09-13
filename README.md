# Bayesian Single Case Classification Model

This repository serves as a central location for R scripts and documentation for publications and review of a Bayesian classification model intended for use in single-case neuropsychological decision-making. As projects are made for validation studies, elaborations, extensions, and modification of the model, these materials will be updated and stored here as a single location.

## Simulation-based Validation of Model with Sample Means, Standard Deviations, and Intercorrelation Matrices

|Resource Available|Link|
|---|---|
|R Code|[Here](https://github.com/w-goette/Single-Case-Classification/tree/main/R%20Script%20Files)|
|Supplementary Materials|[Here](https://github.com/w-goette/Single-Case-Classification/tree/main/Markdown%20Files)|
|Introduction to the Model|[Here](https://github.com/w-goette/Single-Case-Classification/blob/main/Modeling%20Details.md)|
|What can we learn from a simulation study?|TBD|
|Final Publication|TBD|

The materials referenced above correspond to the manuscript currently under-review at the _Journal of the International Neuropsychological Society_. Materials can be accessed either by navigating this github page or by clicking the link for the desired material in the table above. This study serves as an initial validation of the model by examining three specific criteria: parameter recovery, classification/diagnosis accuracy, and quantitative posterior inference. The study is purely a simulation study, so the shared R script files allow for full transparency and reproducibility of the simulation results. Supplementary materials for this study include RMarkdown documents describing additional explorations of the simulation-based results and a simple tutorial for using the model in R. The rendered RMarkdown files (.html, in the "Supplementary Materials" folder) are recommended for viewing, but we also provide the raw Markdown files (.rmd, in the "R Script Files" folder) should any reader be interested in these.

Note that a seed was specified for the study to aid in complete reproducibility; however, we caution that results may still vary minimally as the shared scripts do not include initial testing of the models. We attempted to ensure that seeds were not set or at least that a new R session was opened before running the simulation to obtain the repored results; however, this is subject to human error. For clarity, the potential concern is that a seed will produce the same sequence of random numbers, but we are highlighting that the sequence of such randomly generated numbers may have been further along because of initial code checks and testing before we started the simulation reported in the final results than if someone were to have simply started the simulation script as is. This should not be a signifcant source of discrepancy, but to maintain full discrepancy, we have also made available the .RData file used to report results (found in "R Script Files" folder). This should aid in any discrepancy testing if needed.
