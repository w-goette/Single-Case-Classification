# Mathematical Documentation of the Single Case Bayesian Latent Class Model

While several documents, including the corresponding manuscript, all aim to present the model's logic in a common-language manner, there are likely to be certain readers who are interested in the mathematical model undergirding the described model. A benfit of Stan is that models are fairly easily understood from the code itself, but this requires that one is familiar with the Stan language. The aim, therefore, of this supplemental file is to review the mathematical notation and details of the model itself.

Conceptually, the model is a latent profile analysis fit via a finite mixture model. Unlike standard latent profile analyses, the aim is not to determine the number of latent classes or the mixing ratio of the mixtures; instead, the goal is to identify what sub-population in the mixture a certain random observation is most likely to belong. The consequence of this is that informative priors are utilized to ensure identifiability and performance of the model. The informative priors are both hard priors and soft priors. Hard priors come in the form of explicit specification of the latent classes. If the clinician examines 6 populations of interests, then there will be 6 latent classes estimated. There is no way of empirically reducing or expanding the number of latent classes outside of whatever is determined *a priori* to be of interest. The soft priors take the form of more standard Bayesian priors -- indeed these are canonical priors for the population means and covariance matrices. The distinction between "hard" and "soft" priors is arbitrary but is meant to reflect a distinction in Bayesian methods. While some proper priors may be highly regularizing and cause non-real effects to shrink toward zero, there is not a (non-controversial) Bayesian prior that gives no probability to an effect. The "hard" priors thus reflect a strong clinical belief that only those diagnoses that could credibly be present are examined and the probability of all other diagnoses are zero. This is a particularly strong assumption that reduces an arbitrarily large number of possible latent classes/mixtures to a probability of zero; however, the assumption is unavoidable since there is inadequate data to overwhelm the priors since *n* = 1 in these cases.

## Sampling Notation

For convenience, all of the notation in this document will be kept consistent. Treating mixtures as a population of populations, *P* indexes the total number of composite populations, *p*. Similarly, *T* indexes all tests with *t* reflecting a single case of this set. Other notation is standard statistical notation (e.g., &mu; for the population mean) or will be described as needed (e.g., that &eta; is the shape parameter of the LKJ prior).

The priors for the model can be written as follows (note: the formulae may not be visible in dark mode):

<img src="https://latex.codecogs.com/png.latex?\sigma_{p,&space;t}^{2}&space;\sim&space;\chi&space;^2(\nu_p)" title="\sigma_{p, t}^{2} \sim \chi ^2(\nu_p)" />
<img src="https://latex.codecogs.com/png.latex?\mu_{p,&space;t}&space;\sim&space;N(0,&space;1)" title="\mu_{p, t} \sim N(0, 1)" />
<img src="https://latex.codecogs.com/png.latex?\Sigma_p&space;\sim&space;LKJ(\eta_p)" title="\Sigma_p \sim LKJ(\eta_p)" />

These priors are not complicated. As noted earlier, these are the canonical priors for the population mean when variance and mean are unknown; however, unlike standard extensions to the multivariate normal case via the Wishart distribution, Stan recommends decomposing the covariance matrix into the vector of standard deviations (&Sqrt;&sigma;<sup>2</sup>) and a prior over the correlation matrix (&Sigma;).

Briefly, &sigma;<sup>2</sup> is a *P* x *T* matrix where each row, *P*, corresponds to a unique sub-population while each column, *T*, refers to a different test. Each cell in this matrix is therefore the estimated population variance of that test, *t*, in that population, *p*. This variance term is given a &chi;<sup>2</sup> distribution with degrees of freedom, &nu;, based on the sample size of the reference sample derived from that population.

The population mean, &mu;, is also a *P* x *T* matrix where rows correspond to sub-populations and columns to tests. The standard normal distribution is specified in order to utilize a non-centered parameterization of the model, which is more stable and efficient in the Stan sampler. As shown in the Data and Parameter transformation section, these standard normal estimates are later scaled by the data to the original metric, but this non-centered prior parameterization is more stable overall for the model.

Finally, the correlation matrix, &Sigma;, is a *T* x *T* symmetric matrix with diagonal values of 1. A unique correlation matrix can be provided for each sub-population, or sub-populations can share matrices. The shape parameter of the LKJ prior is defined by &eta;, which can also be unique or shared across all populations. Technically, the model prior is on the Cholesky decomposition of the correlation matrix, which is again a more stable and efficient parameterization for Stan's sampler. As the Cholesky decomposition is used, all correlation matrices must be positive definite.

The likelihood of the model is given by the sum of log-probability density function of the multivariate normal distribution and pre-test probabilties as follows:

<img src="https://latex.codecogs.com/png.latex?p_Y(y&space;|&space;\theta&space;,&space;\mu,&space;\Sigma)=\sum_{p&space;=&space;1}^{P}\theta_{p}\cdot&space;MVN(\mu_{p,t},&space;\Sigma_{p})" title="p_Y(y | \theta , \mu, \Sigma)=\sum_{p = 1}^{P}\theta_{p}\cdot MVN(\mu_{p,t}, \Sigma_{p})" />

In this modeling, &theta; corresponds to the pre-test probabilities for membership in each *p* while the log-probability density function of the multivariate normal distribution quantifies the likelihood of the data *y* given the parameter estimates for &mu; and &Sigma; for the population and vector of tests, *t*.

## Data/Parameter Transformations

There are two transformations to the raw data passed to Stan and then four transformations of estimated parameters. The first raw data transformation is to obtain the degrees of freedom, &nu;, for each reference sample. This is done by taking the *N*<sub>p</sub> corresponding to the sample size of each reference sample and subtracting 1. The second transformation to the raw data is to convert the given correlation matrix into a Cholesky decomposition as discussed earlier.

The parameter transformations begin with the estimation of &sigma;<sub>p</sub> from the variance estimate. In order to do this, the standard deviations of the reference sample are computed as the root of the sample-based approximation of the population variance divided by the estimated population variance:

<img src="https://latex.codecogs.com/png.latex?\sigma_{p,&space;t}&space;=&space;\sqrt{\frac{\nu_p*s_{p,&space;t}^{2}}{\sigma_{p,&space;t}^2}}" title="\sigma_{p, t} = \sqrt{\frac{\nu_p*s_{p, t}^{2}}{\sigma_{p, t}^2}}" />.

The estimated population standard deviation for each test, *t*, in each popultaion, *p*, is then converted to the standard error of the mean by dividing it by the root of the degrees of freedom:

<img src="https://latex.codecogs.com/png.latex?\sigma_{\mu_{p,&space;t}}&space;=&space;\frac{\sigma_{p,t}}{\sqrt{\nu_p}}" title="\sigma_{\mu_{p, t}} = \frac{\sigma_{p,t}}{\sqrt{\nu_p}}" />.

This information can then be used to rescale the non-centered standard normal distribution of &mu; as follows:

<img src="https://latex.codecogs.com/png.latex?\mu^T_{p,&space;t}&space;=&space;(\mu_{p,&space;t}\cdot&space;\sigma_{\mu_{p,&space;t}})&plus;\bar{x}_{p,&space;t}" title="\mu^T_{p, t} = (\mu_{p, t}\cdot \sigma_{\mu_{p, t}})+\bar{x}_{p, t}" />,

where the standard error of the mean is multiplied by the standard normal deviate and then relocated by the reference sample means.

With the rescaled mean and standard deviation parameters prepared, it is then possible to compute the *z*-score in the standard fashion:

<img src="https://latex.codecogs.com/png.latex?z_{p,&space;t}&space;=&space;\frac{x_{p,&space;t}-\mu_{p,&space;t}^T}{\sigma_{p,&space;t}}" title="z_{p, t} = \frac{x_{p, t}-\mu_{p, t}^T}{\sigma_{p, t}}" />.

There are two additional modeling notes to make regarding the model's specification. The first is that the sum of the pre-test probabilities by the multivariate normal LPDF is first indexed in a vector of length *P*. This the log sum of exponentials of this vector is then taken to define the likelihood of the model. This step is needed to marginalize over the discrete latent classes as Stan does not handle discrete latent variables. The other modeling note to make is that a softmax function is used over that same vector in order to obtain post-test probabilities that always sum to 1.
