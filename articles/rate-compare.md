# Unstratified and Stratified Miettinen and Nurminen Test

## Overview

Binary outcome is a commonly used endpoint in clinical trials. This page
illustrates how to conduct the unstratified or stratified analysis with
the Miettinen and Nurminen (M&N) method (Miettinen and Nurminen 1985)
for risk difference analysis in R. The following statistics can be
calculated with the function
[`rate_compare()`](https://merck.github.io/metalite.ae/reference/rate_compare.md):

- Estimated risk difference.
- Test statistic.
- Confidence interval for the risk difference.
- p-value for risk difference.

## Statistical methods

### Unstratified analysis of M&N method

Assume the data includes two independent binomial samples with binary
response variables to be analyzed/summarized and the data collected in a
clinical design without stratification. Also this approach is applicable
to the case when the data are collected using a stratified clinical
design and the statistician would like to ignore stratification by
pooling the data over strata assuming two independent binomial samples.
Assume $P_{i}$ is the proportion of success responses in the test
($i = 1$) or control ($i = 0$) group.

#### Confidence interval

The confidence interval is based on the M&N method and given by the
roots for $PD = P_{1} - P_{0}$ of the equation:

$$\chi_{\alpha}^{2} = \frac{\left( {\widehat{p}}_{1} - {\widehat{p}}_{0} - PD \right)^{2}}{\widetilde{V}}$$,

where ${\widehat{p}}_{1}$ and ${\widehat{p}}_{0}$ are the observed
values of $P_{1}$ and $P_{0}$, respectively;

- $\chi_{\alpha}^{2}$ = the upper cut point of size $\alpha$ from the
  central chi-square distribution with 1 degree of freedom
  ($\chi_{\alpha}^{2} = 3.84$ for $95$% confidence interval);

- $PD$ = the difference between two population proportions
  ($PD = P_{1} - P_{0}$);

$$\widetilde{V} = \lbrack\frac{{\widetilde{p}}_{1}\left( 1 - {\widetilde{p}}_{1} \right)}{n_{1}} + \frac{{\widetilde{p}}_{0}\left( 1 - {\widetilde{p}}_{0} \right)}{n_{0}}\rbrack\frac{n_{1} + n_{0}}{n_{1} + n_{0} - 1}$$;

- $n_{1}$ and $n_{0}$ are the sample sizes for the test and control
  group, respectively;

- ${\widetilde{p}}_{1}$ = maximum likelihood estimate of proportion on
  test computed as ${\widetilde{p}}_{0} + PD$;

- ${\widetilde{p}}_{0}$ = maximum likelihood estimate of proportion on
  control under the constraint
  ${\widetilde{p}}_{1} - {\widetilde{p}}_{0} = PD$.

As stated above the 2-sided $100(1 - \alpha)$% CI is given by the roots
for $PD = P_{1} - P_{0}$. The bisection algorithm is used in the
function to obtain the two roots (confidence interval) for $PD$.

#### p-value and Z-statistic

The Z-statistic is computed as:

$$Z_{\text{diff}} = \frac{{\widehat{p}}_{1} - {\widehat{p}}_{0} + S_{0}}{\sqrt{\widetilde{V}}}$$
where ${\widehat{p}}_{1}$ and ${\widehat{p}}_{0}$ are the observed
values for $P_{1}$ and $P_{0}$ respectively, $S_{0}$ is pre-specified
proportion difference under the null;

- ${\widetilde{p}}_{1}$ = maximum likelihood estimate of proportion on
  test computed as ${\widetilde{p}}_{0} + S_{0}$;

- ${\widetilde{p}}_{0}$ = maximum likelihood estimate of proportion on
  control under the constraint
  ${\widetilde{p}}_{1} - {\widetilde{p}}_{0} = S_{0}$.

- For non-inferiority or one-sided equivalence hypothesis with
  $S_{0} > 0$, the p-value,
  $\Pr\left( Z \geq Z_{\text{diff}}\,|\, H_{0} \right)$, is computed
  based on $Z_{\text{diff}}$ using the standard normal distribution.

- For non-inferiority or one-sided equivalence hypothesis with
  $S_{0} < 0$, the p-value,
  $\Pr\left( Z \leq Z_{\text{diff}}\,|\, H_{0} \right)$, is computed
  based on $Z_{\text{diff}}$ using the standard normal distribution.

- For two-sided superiority test, the p-value
  $\Pr\left( \chi_{\text{diff}}^{2} \leq \chi_{1}^{2}\,|\, H_{0} \right)$,
  is computed based on $\chi_{\text{diff}}^{2}$ using the chi-square
  distribution with 1 degree of freedom, where
  $\chi_{\text{diff}}^{2} = Z_{\text{diff}}^{2}$.

### Stratified analysis of M&N method

Assume the data includes two treatment groups, test and control, and
collected based on a stratified design. Within each stratum there are
two independent binomial samples with binary response variables to be
analyzed/summarized. The parameter of interest is the difference between
the population proportions of the test and the control groups. The
analysis and summaries need to be performed while adjusting for the
stratifying variables.

#### Confidence interval

The confidence interval is based on the M&N method and given by the
roots for $PD = P_{1} - P_{0}$ of the equation:

$$\chi_{\alpha}^{2} = \frac{\left( {\widehat{p}}_{1}^{*} - {\widehat{p}}_{0}^{*} - PD \right)^{2}}{\sum\limits_{i = 1}^{I}\left( W_{i}/\sum\limits_{k = 1}^{K}W_{k} \right)^{2}{\widetilde{V}}_{i}}$$,

where
${\widehat{p}}_{s}^{*} = \sum_{i = 1}^{I}\left( W_{i}/\sum_{k = 1}^{K}W_{k} \right){\widehat{p}}_{si}$
for $s = 0,1$;

$${\widetilde{V}}_{i} = \lbrack\frac{{\widetilde{p}}_{1i}\left( 1 - {\widetilde{p}}_{1i} \right)}{n_{1i}} + \frac{{\widetilde{p}}_{0i}\left( 1 - {\widetilde{p}}_{0i} \right)}{n_{0i}}\rbrack\frac{n_{1i} + n_{0i}}{n_{1i} + n_{0i} - 1}$$;

- $W_{i}$ is the weight for the $i$-th strata;
- \$I = K = \$ number of strata, $i = k =$ strata;
- $n_{1i}$ and $n_{0i}$ are the sample sizes in $i$-th strata for the
  test and control group, respectively;
- ${\widehat{p}}_{1i}$ and ${\widehat{p}}_{0i}$ = observed proportion in
  $i$-th strata for the test and control, respectively;
- ${\widetilde{p}}_{0i}$ and ${\widetilde{p}}_{1i}$ are MLE for $P_{0i}$
  and $P_{1i}$, respectively, computed under the constraint
  ${\widetilde{p}}_{1i} = {\widetilde{p}}_{0i} + PD$.

Similarly as for unstratified analysis,the 2-sided $100(1 - \alpha)$% CI
is given by the roots for $PD = P_{1} - P_{0}$, and the bisection
algorithm is used in the function to obtain the two roots (confidence
interval) for $PD$.

#### p-value and Z-statistic

The Z-statistic is computed as:

$$Z_{\text{diff}} = \frac{{\widehat{p}}_{1}^{*} - {\widehat{p}}_{0}^{*} + S_{0}}{\sqrt{\sum\limits_{i = 1}^{I}\left( W_{i}/\sum\limits_{k = 1}^{K}W_{k} \right)^{2}{\widetilde{V}}_{i}}}$$
where $S_{0}$ is pre-specified proportion difference under the null;

- ${\widetilde{p}}_{0i}$ and ${\widetilde{p}}_{1i}$ are MLE for $P_{0i}$
  and $P_{1i}$, respectively, computed under the constraint
  ${\widetilde{p}}_{1i} = {\widetilde{p}}_{0i} + S_{0}$.

The p-value can be calculated as stated above.

## Example

### Load package

``` r
library(metalite.ae)
```

### Data simulation

We simulated a dataset with 2 treatment group for binary output. If
stratum is used, we considered 4 stratum.

``` r
ana <- data.frame(
  treatment = c(rep(0, 100), rep(1, 100)),
  response  = c(rep(0, 80), rep(1, 20), rep(0, 40), rep(1, 60)),
  stratum   = c(rep(1:4, 12), 1, 3, 3, 1, rep(1:4, 12), rep(1:4, 25))
)

head(ana)
#>   treatment response stratum
#> 1         0        0       1
#> 2         0        0       2
#> 3         0        0       3
#> 4         0        0       4
#> 5         0        0       1
#> 6         0        0       2
```

### Unstratified analysis

The function computes the risk difference, Z-statistic, p-value given
the type of test, and two-sided $100(1 - \alpha)$% confidence interval
of difference between two rates.

``` r
rate_compare(response ~ treatment, data = ana)
#>   est  z_score            p    lower     upper
#> 1 0.4 5.759051 4.229411e-09 0.269662 0.5165743
```

### Stratified analysis

The sample size weighting is often used in the clinical trial. Below is
the function to conduct stratified MN analysis with sample size weights.

We also support weight in `"equal"` and `"cmh"`. More details can be
found in the
[`rate_compare()`](https://merck.github.io/metalite.ae/reference/rate_compare.md)
documentation.

``` r
rate_compare(
  formula = response ~ treatment, strata = stratum, data = ana,
  weight = "ss"
)
#>         est  z_score            p     lower     upper
#> 1 0.3998397 5.712797 5.556727e-09 0.2684383 0.5172779
```

## References

Miettinen, Olli, and Markku Nurminen. 1985. “Comparative Analysis of Two
Rates.” *Statistics in Medicine* 4 (2): 213–26.
