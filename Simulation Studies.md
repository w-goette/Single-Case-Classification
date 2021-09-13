# What Can a Simulation Study Tell Us About Clinical Practice

It is anticipated that results from the model validation may be met with skepticism from some readers. We acknowledge several points of empirical skepticism in the manuscript ourselves as there are several applications that are theoretically possible but have not yet been tested, and we also acknowledge the significant limitation of having used purely simulated data in this study. At the same time, however, we do believe that the simulation study is able to tell us something about the model's performance in clinical settings. Part of that stems from our theoretical stance in building the model, but most of it comes from the steps taken to make a reasonable simulation of clinical data. Regardless, simulation studies serve an important role in model building, and there were specific assumptions that had to be tested in simulation since they would otherwise be impossible to test. The aim of this document is thus to highlight what is gained from a simulation study and why it was needed in the first place.

## Simulation-Unique Opportunities

One of the primary interests of the model is to present a way that simultaneously returns a Bayes' rule-consistent post-test probability for a battery of tests across multiple reference samples while also returning quick, easily-understanable summary information about a client's cognitive performance. Neuropsychologists are intimately familiar with the normal distribution and standardized test metrics, making the z-score an appealing metric to provide clinicians. Z-scores simultaneously convey information about relative rank and probability of observations, and they are readily interpretted by neuropsychologists. The problem with true z-scores, however, is that they require knowledge of the population's mean and standard deviation. There are relatively straight-forward sample-based approximations for the z-score that account for the fact that the population mean and standard deviation must be estimated from sample statistics, but extending these methods to multivariate cases or at least rapidly doing this multiple times for testing batteries across reference samples is not always readily feasible. The proposed model thus does this work to estimate the population parameters and pass the uncertainty of these estimates to the z-score computations as well.

Since the model tries to compute the population parameters, a simulation study is the only way that we can test how accurately the model makes this estimation. Using real clinical data, we can't be certain that we know the population parameters since we only assume or create scores from the assumption of certain distributional forms (e.g., that standard scores have a mean of 100 and sd of 15). In a simulation study, we simulate data using the "true" population parameters, meaning that we know with certainty how well the model does in approximating these values since we know the values to begin with. This analysis is commonly called parameter recovery in simulation studies since the model is having to "recover" the parameters. We examined this directly with the z-scores, though we could technically have also extended this to the population means and standard deviations. Of particular interest to us was validating that the 95% credible intervals were indeed appropriate (i.e., that they covered the true population values 95% of the time) and that the z-scores were not unreasonably biased. This latter point is important as the model would perform extremely poorly if its z-score estimates were systematically biased toward or away from impairment since this would bias any inference built from them.

The other thing that required simulation in order to answer is the diagnostic accuracy element. Neuropsychology currently lacks highly accurate, gold-standard diagnostic methods for most conditions. While we are confident in our diagnostic accuracy and can make strenuous diagnostic methods (e.g., specific diagnostic criteria + consensus diagnosis + stable diagnosis over X number of visits), diagnostic accuracy studies in neuropsychology are often circular: the tests used to make diagnoses are interpreted in the same or similar ways as clinicians who made those diagnoses and then used to predict the diagnoses again. Bayesian methods have been recommended for diagnostic accuracy studies in cases where the true diagnosis is not knowable because the uncertainty about the true diagnosis can be built into the modeling. In the case of a simulation study, we know exactly what the true diagnosis is because that is part of the simulation process. As a result, we know the exact accuracy of the model's performance since we do not need to worry about the model being perhaps more accurate than the clinical diagnoses (i.e., there being artifically low classification accuracy because the reference diagnoses were wrong rather than the model's predictions being wrong). 

The issue of circularity, however, is not fully resolved by simulation studies. It is possible to produce arbitrarily good diagnostic accuracy in a simulation study by simply simulating favorable conditions. For example, if the model makes certain assumptions about how the data are generated, then simulating the data in that way will introduce artificial favorable classification rates. Just like with clinical diagnostic accuracy studies, simulation studies introduce circularity when the simulation conditions are built around the model. As highlighted in the next section, we took several steps to ensure that realistic simulation conditions were being created, but we are still making every effort to be transparent about the steps taken and the assumptions made at every step of the study so that readers can arrive at their own decisions regarding the utility of this simulation study and our success in avoiding favorably biased results.

## Simulation-Specific Strengths

One of the major concerns with simulation-based research is that the data end up being unrealistically idealized. Model building is already an act of simplification: complexities of the data generation process are reduced through distributional assumptions and reductions of covariates to only those that explain appreciable amounts of variance. Simulation is also, inherently, simplified since they are built on mathematically idealized formulas that approximate real data. No sample produces perfectly normal distributions, but some do produce some that are close enough. Simulated datasets have the potential come too close to that idealized normal distribution (or whatever distribution is being simulated). For that reason, we tried to avoid fitting unrealistically large sample sizes so that the simulated reference samples still had reasonable noise to feel realistic. The n = 1000 conditions were added only at the request of a reviewer who, rightfully, recommended its addition to see whether there is a loss of return on information as sample sizes become very large (plus there are some large, multisite studies like NACC and ADNI where such sample sizes are not unreasonable).

Another concern with simulation-based research comes from the historical literature on simulations studies of factor analytic mehtods. It was relatively common to create conditions for a simulation study based on the "true" factor model from which the "true" correlation matrix can be created and sampled from. The issue, however, is that these correlation matices correspond to the true factor structure and are then fit on the true factor model. This is unrealistic because researchers never know whether the model they fit is the "true" model or not, so several methods were created for simulating noisy correlation matrices where the true model is never going to be fit. These methods more closely align to what is observable in the real-world, and these methods were taken for generating our correlation matrices for exactly this reason.

Finally, we wanted to also make sure that our determination of classification accuracy was rooted in an appropriate comparison to alternative methods. Reporting just the diagnostic accuracy of our model could be misleading as it could either (a) appear much worse than might be expected by a reader or (b) appear favorable but actually be worse than another alternative method. Toward this end, we wanted to include other diagnostic/classification methods that would be appropriate for the data. Since all the data were simulated from a multivariate normal distribution, linear and quadratic discriminant function analyses were a good parametric comparison point, and then since 3 diagnostic classes were examined, a multinomial logistic regression was used as a generalized linear model alternative. Classification accuracy of the model is thus compared to these established, well-defined mathematical models.

## Simulation Robustness

Perhaps the most significant assumption of the model and simulation steps is that of multivariate normality. Neuropsychological testing can be heavily skewed, particularly in impaired samples. Since a primary goal of the model is diagnosis, the presence of non-normal distribution in some of these diagnoses would directly violate the assumptions of the models and potentially undermine its performance. The simulation study did not examine the effects of such skewing or non-normality, so little can be said at this time as to the actual impact of such violations. It is worth noting that the model syntax can be adjusted to account for truncated distributions (e.g., in cases where scores cannot exceed specific ranges) or alternative parametric shapes (e.g., skew normal, log-normal, binomial, etc). These were not tested as they require either case-specific modification of the model code or additional data that might not be readily reported in research. Since this first simulation tests a flexible general model, these technical aspects were ignored.

Despite such ignoring of the technical implications of violating multivariate normality, we do ultimately believe that this is a reasonable starting assumption for many settings and needs. Gaussian distributions, to which the standard normal distribution belongs, are well-defined in nature when the generative process has a central tendency to which random variation is added. Such random variation occurs in two ways for neuropsychological data: measurement error and cognitive ability variation. Since these two factors introduce random variation away from a central tendency, it is reasonable to expect Gaussian distributions to emerge commonly in neuropsychological settings. Despite these conditions, non-normality might arise from two potential sources. One possibility that was already noted is that of truncated distributions wherein the range of scores fails results in skewing from scores piling at the boundaries. Another possibility is that the examined distribution is actually a mixture of multiple distributions. An example of this is actually visiable in the other Markdown file on this github page ([Modeling Details](https://github.com/w-goette/Single-Case-Classification/blob/main/Modeling%20Details.md)). Since this model is fitting a mixture of distributions, this may result in identification genuinely multivariate normal distributions within these sub-populations despite the population distribution potentially appearing to be strongy non-normal.