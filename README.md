# CompositeReliability
Determine the Composite Reliability of a Naturalistic, Unbalanced Dataset

The reliability of assessment tools is a crucial aspect of monitoring student performance in various educational settings. It ensures that the assessment outcomes accurately reflect a student's true level of performance. However, when assessments are combined, determining composite reliability can be challenging, especially for naturalistic and unbalanced datasets. This package is designed to estimate composite reliability using multivariate generalizability theory and enhance the analysis of assessment data. The package allows for the inclusion of weight per assessment type and produces extensive G- and D-study results with graphical interpretations, and options to find the set of weights that minimize the standard error of measurement (SEM).

```R
library("CompositeReliability") 
```

## G-Study
The "GStudy" function, available in the "CompositeReliability" package, is designed to assess the reliability coefficient and the standard error of measurement (SEM) for each assessment type. This function utilizes the harmonic mean of the number of assessments per type as a measure of effective assessment quantity. By providing a dataset as input, users can obtain reliable estimates of the reliability coefficient and SEM for each assessment type. The desired number of decimal places in the output (nrDigitsOutput) can be added as input. NB: The dataset "mydata" is included in the package as example.

```R
GStudy(mydata,nrDigitsOutput=4,optimizeSEM=TRUE)
```

The output presents a table with descriptive statistics for each Type included in the dataset.

To view the observed variances, covariances, and error scores, the dataset, along with a vector specifying the number of assessments per assessment type, can be added as input.

```R
varcov <- calculateVarCov(mydata, n=c("A"=3.1579, "B"=3.0000, "C"=1.4286))
varcov$S_p
varcov$S_iINp
varcov$S_delta
```

## D-Study
The number of assessments per Type has a notable impact on the reliability estimation. To investigate the relationship between the number of assessments and reliability, the D-study functionality within the program provides an analysis of the reliability coefficient and standard error of measurement (SEM) for varying numbers of assessments per type.

```R
plots <- DStudy(mydata, maxNrAssessments = 60)
plots$plotRel
plots$plotSEM
```

## Composite reliability
The estimation of composite reliability, encompassing the integration of student results from assessments of various types, is performed utilizing multivariate generalizability theory. Leveraging the capabilities of the R package, researchers can obtain a comprehensive analysis that includes the determination of weights optimizing the standard error of measurement (SEM) and the subsequent presentation of the composite reliability value.
NB: make sure that the sum of weights included in vector 'w' is equal to 1.

```R
compRel <- computeCompositeReliability(mydata, n=c("A"=10, "B"=5, "C"=2), w=c("A"=1/3,"B"=1/3, "C"=1/3), optimizeSEM=TRUE)
compRel$reliability
compRel$SEM
compRel$weights
```

And it is possible to compute the maximum value of the composite reliability coefficient of the dataset, given the number of assessments per type.

```R
compMaxRel <- computeMaxCompositeReliability(mydata, n=c("A"=10, "B"=5, "C"=2))
compMaxRel$reliability
compMaxRel$SEM
compMaxRel$weights
```
