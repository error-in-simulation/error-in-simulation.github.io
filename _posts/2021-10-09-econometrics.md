---
layout: post
title: "Project 1: Econometrics"
subtitle: "Part 1/n"
date: 2021-10-09 23:45:13 -0400
background:
---

In this post, I replicated Angrist and Pischke's (relatively famous?) case study on the effects of lowering the minimum legal drinking age. 

Code and post was originally written in R (seen in appendix) and RMarkdown. 

# Introduction

In 2011, Carpenter and Dobkins compiled a literature review in the
*Journal of Economic Perspectives* regarding the role of the minimum
legal drinking age (MLDA) in the USA and it’s impact among youths and
young adults. The minimum legal drinking age is commonly framed as a
dichotomous discussion - detractors often consider the 21-year MLDA as
too high in the qualified sense of what constitutes an adult right, that
culturally similar countries have a lower MLDA of 18, and most
importantly the effectiveness of these kinds of laws when it comes to
reducing adverse societal impacts (Carpenter and Dobkins, 2009).
Beginning in the 70s, several states experimented with lowering the
MLDA, which offers an opportunity to study the policy effects of
government intervention. In their paper, the authors evaluate these
effects in part by analyzing the changes in mortality rates that are
linked to alcholic consumption. Our focus in this paper is the
replication of the differences-in-differences method employed for such
analysis.

# Data

The original dataset analyzed by Carpenter and Dobkins contained
information on state, year, mortality rates (in 100 000s), cause of
mortality and a \`legal drinking ratio’. The last variable represents
the proportion of 18-20 year olds that are legally able to drink at a
specific state and time, and so accounts for the fact that not every
state that chose to lower the MLDA uniformly lowered to 18 years old. We
turn our attention to mortality rates specific to motor vehicle
accidents, as that cause of death is likely the most compelling from a
causal point of view. This dataset was sourced from the National Vital
Statistics System and the Fatality Accident Reporting System.
Unfortunately, attempts to recover this particular dataset failed. As a
result, an alternative dataset was used to approximate the results, the
same one that Angrist and Pischke (2014) chose as a case study for the
same topic in their textbook[1]. All 51 states[2] were used, over the
period of 1970-1983, for a total of 714 data points. The key difference
between the datasets are the time periods chosen (the original dataset
is from 1975-1993).

# Econometric Model

The model in the paper is a panel regression of the following form:

*Y*<sub>*s**t*</sub> = *β*MLDA<sub>*s**t*</sub> + *θ*<sub>*s*</sub> + *μ*<sub>*t*</sub> + *ψ*<sub>*s**t*</sub> + *ε*<sub>*i**s**t*</sub>

The model chosen to replicate the results *in general*[3] is that of the
difference in differences design:

$$Y\_{st} = \\alpha +  \\delta\_{DD} \\text{MLDA}\_{st} + \\sum\_{k=\\text{Alaska}}^{\\text{Wyoming}}\\beta\_k\\text{State}\_{ks} + \\sum\_{i=1970}^{1983} \\lambda\_{i}\\text{Year}\_{it} + \\psi\_{st} + \\varepsilon\_{st} $$

The above model can be interpreted in the following manner: *Y* is the
mortality rate, *α* is the intercept term, *δ* is the parameter for MLDA
(the proportion of 18-20 year olds that are legally able to drink),
*β*<sub>*k*</sub> are the dummy variables that captures the state
effects, *λ*<sub>*i*</sub> are the dummy variables that captures the
time effects, and *ψ* is the interaction between state and year. In the
paper, a form of RDD was also carried out to provide alternative
estimates:

*y* = *β*<sub>0</sub> + *β*<sub>1</sub>MLDA + *β*<sub>2</sub>Birthday + *f*(age) + *ε*
Where *f*( ⋅ ) is a quadratic function. This regression model was
included for the sake of completion *and not analysis*.

# Results

We consider four kinds of models based on the DID design described
earlier: the OLS model without *ψ*, the OLS model with *ψ*, the weighted
model without *ψ*, and the weighted model with *ψ*. Clustered standard
errors by state population were chosen to be included. We present the
findings of our estimates in the table below:

All models find MLDA to be significant. We find that on average, a lower
MLDA resulted in an increase of 6-7 motor vehicle accident fatalities
per 100 000. Including the interaction term modestly increases standard
errors, while considering a weighted model substantially decreases
standard errors. With a different dataset, we would not be able to
recover the same parameter and SE as the paper. However, we can happily
report that the point estimates and standard errors matches the case
study results, which is really the intended goal (as the dataset
corresponds to the case study).

# Discussion and Limitations

One important aspect of econometric analysis we have yet to address is
that of checking model assumptions. Indeed, it seems out of order that
we have actually conducted the analysis first prior to checking the
assumption of the DID model - particularly the common trend assumption.
But by including *ψ*, the state-specific linear time trend effect, we
are capturing any sharp deviations in the trend of a treated group, even
if trends are not parallel. Since there is not much difference between
the models with and without *ψ*, we conclude our findings are not
spurious. Another potential issue is the difference in how all states
chose to implement changes to their MLDA[4]. This forces us to use a
more complex model than the parsimonious binary model. As a result, it
may be more favourable to consider an RDD model instead - far less model
coefficients to estimate.

# Conclusion

Our findings, while not matching the exact estimates in the paper, leads
to the same conclusion that a lower MLDA is linked to higher fatalities.
As the identification strategy is through the difference in differences
method, we feel justified in saying we recovered the *causal effect*. As
to whether or not we *ought* to increase the MLDA to decrease fatalities
is outside the scope of what the original paper strives to answer - as a
result, we make no attempt either. Our findings do match the results in
the case study, which lends a lot of credence to the analysis.

# References

1.  Carpenter, Christopher, and Carlos Dobkin. “The minimum legal
    drinking age and morbidity in the United States.” Review of
    Economics and Statistics 99.1 (2017): 95-104.

2.  Carpenter, Christopher, and Carlos Dobkin. “The effect of alcohol
    consumption on mortality: regression discontinuity evidence from the
    minimum drinking age.” American Economic Journal: Applied Economics
    1.1 (2009): 164-82.

3.  Angrist, Joshua D., and Jörn-Steffen Pischke. Mastering’metrics: The
    path from cause to effect. Princeton University Press, 2014.

# Appendix

Below are the codes used to generate the results in the paper.

``` r
library(multiwayvcov)

load(file = "deaths.rda") #DID

dtypes <- c("all" = "All deaths",
            "MVA" = "Motor vehicle accidents",
            "suicide" = "Suicide",
            "internal" = "All internal causes")
            
deaths = mutate(deaths, year_fct =  factor(year))
data = filter(deaths, year <= 1983, agegr == "18-20 yrs", dtype == "MVA")

mod = lm(mrate ~ 0 + legal + state + year_fct, data = data)
vcov_firm <- cluster.vcov(mod, data$state)
coeftest(mod, vcov_firm)

model2 = lm(mrate ~ 0 + legal + year_fct + state + state:year, data = data)
coeftest(model2, vcov.= vcovHC(model2, type = "HC3"))
vcov_firm <- cluster.vcov(model2, data$state)
coeftest(model2, vcov_firm)

model3 = lm(mrate ~ 0 + legal + state + year_fct, data = data, weights = pop)
vcov_firm <- cluster.vcov(model3, data$state)
coeftest(model3, vcov_firm)



model4 = lm(mrate ~ 0 + legal + year_fct + state + state:year, data = data, weights = pop)
coeftest(model4, vcov.= vcovHC(model4, type = "HC3"))
vcov_firm <- cluster.vcov(model4, data$state)
coeftest(model4, vcov_firm)
```

[1] Probably, the primary data source is the same (National Vital
Statistics)

[2] Includes the district of Columbia

[3] We actually consider 4 variants to this model in the Analysis
portion

[4] An unfortunate side effect is that it makes graphical depictions of
the data trends very difficult (if not straight up impossible) to
implement



P.S. If you are a student looking to peruse my code for your own academic projects, I'd advise that Measure of Software Similarity are quite effective at checking for plagiarism. On the other hand, this code is truly not that difficult to replicate - use at your own risk, IMO.
