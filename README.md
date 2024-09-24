# compositeReliabilityInNestedDesigns
Optimizing the Composite Reliability in Multivariate Nested Designs

The reliability of assessment tools is a crucial aspect of monitoring student performance in various educational settings. It ensures that the assessment outcomes accurately reflect a student's true level of performance. However, when assessments are combined, determining composite reliability can be challenging, especially for naturalistic and unbalanced datasets in nested design as is often the case for Workplace-Based Assessments. This package is designed to estimate composite reliability in nested designs using multivariate generalizability theory and enhance the analysis of assessment data. The package allows for the inclusion of weight per assessment type and produces extensive G- and D-study results with graphical interpretations, and options to find the set of weights that maximizes the composite reliability or minimizes the standard error of measurement (SEM).

```R
library("compositeReliabilityInNestedDesigns") 
```

## G-Study
The "GStudy" function, available in the "compositeReliabilityInNestedDesigns" package, is designed to assess the reliability coefficient and the standard error of measurement (SEM) for each assessment type. This function utilizes the harmonic mean of the number of assessments per type as a measure of effective assessment quantity. By providing a dataset as input, users can obtain reliable estimates of the reliability coefficient and SEM for each assessment type. The desired number of decimal places in the output (nrDigitsOutput) can be added as input. NB: The dataset "mydata" is included in the package as example.

```R
GStudy(mydata,nrDigitsOutput=4)
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

## Assumptions and Requirements
Generalizability theory, as a fundamental framework, typically assumes local independence of occasions, implying that each data point is independent of others. However, this assumption may not hold in educational settings, where assessments can affect subsequent performances due to feedback provided to students. Despite this violation, such conditions are common in programmatic, workplace-based assessment settings. We accept this violation as the primary objective in such a setting is to differentiate between overall student performances over an extended time period.

When combining different assessment tools, each focusing on specific aspects in the education, to evaluate the performance of the student using multivariate generalizability theory, the students should be graded on the same rating scale using the same assessment standard throughout all assessments.

The data set utilized in the compositeReliabilityInNestedDesigns package must adhere to specific criteria. It should be an R dataframe with three columns labeled “ID”, “Type”, and “Score”. The “Score” column must contain numeric data. To estimate the composite reliability of multiple assessment tools, each student must receive a grade for each assessment tool at least once. When including the desired number of assessments per tool, each tool must be included and receiving a numeric value. Furthermore, the sum of weights assigned to the assessment tools must be equal to one, ensuring a proper weighting scheme for the composite reliability calculation.
