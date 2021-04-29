# R_IRT_Analysis_Pipeline
A script for running a 2PL dichotomous item response theory analysis on the openly available LSAT data from ltm package in R

The script will let you output all parameter estimates for a 2PL or 3PL model and the following figures:

**1. A scree-plot to check unidimensionality**

![scree-plot](https://github.com/mbrucato/R_IRT_Analysis_Pipeline/blob/main/IRT_Tutorial_ScreePlot.png?raw=true)

**2. Q3 statistic matrix to check for local dependence:**

Example from this tutorial with no locally dependent items:
![Q3 matrix](https://github.com/mbrucato/R_IRT_Analysis_Pipeline/blob/main/IRT_Tutorial_Q3matrix.png?raw=true)


Example from another dataset with locally dependent items highlighted in blue and red:
![Q3 matrix 2](https://github.com/mbrucato/R_IRT_Analysis_Pipeline/blob/main/Q3matrix_example.png?raw=true)

**3. Item Characteristic Curves:**

Example from this tutorial:

![ICC plot](https://github.com/mbrucato/R_IRT_Analysis_Pipeline/blob/main/IRT_Tutorial_ICCplot.png?raw=true)

Example from another dataset with variety of discriminability values:
![ICC plot 2](https://github.com/mbrucato/R_IRT_Analysis_Pipeline/blob/main/ICCplot_example.png?raw=true)

**4. Test/Item Characteristic Curves:**


![ITF plot](https://github.com/mbrucato/R_IRT_Analysis_Pipeline/blob/main/IRT_Tutorial_TIF.png?raw=true)

![IIF plot](https://github.com/mbrucato/R_IRT_Analysis_Pipeline/blob/main/IRT_Tutorial_IIFs.png?raw=true)
