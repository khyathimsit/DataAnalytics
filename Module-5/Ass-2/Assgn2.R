#Question1

>  model1 = lm(machinedata$PRP ~ machinedata$MYCT + machinedata$MMIN + machinedata$MMAX + machinedata$CACH + machinedata$CHMIN + machinedata$CHMAX + machinedata$X)
> summary(model1)

Call:
lm(formula = machinedata$PRP ~ machinedata$MYCT + machinedata$MMIN + 
    machinedata$MMAX + machinedata$CACH + machinedata$CHMIN + 
    machinedata$CHMAX + machinedata$X)

Residuals:
    Min      1Q  Median      3Q     Max 
-208.57  -24.09    6.30   27.27  365.11 

Coefficients:
                    Estimate Std. Error t value Pr(>|t|)    
(Intercept)       -5.500e+01  1.090e+01  -5.047 1.00e-06 ***
machinedata$MYCT   5.064e-02  1.734e-02   2.921  0.00389 ** 
machinedata$MMIN   1.491e-02  1.832e-03   8.136 4.26e-14 ***
machinedata$MMAX   5.170e-03  6.517e-04   7.933 1.49e-13 ***
machinedata$CACH   8.415e-01  1.549e-01   5.433 1.60e-07 ***
machinedata$CHMIN -5.320e-01  8.502e-01  -0.626  0.53225    
machinedata$CHMAX  1.643e+00  2.251e-01   7.298 6.71e-12 ***
machinedata$X     -1.131e-02  7.005e-02  -0.161  0.87190    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 59.1 on 200 degrees of freedom
Multiple R-squared:   0.87,     Adjusted R-squared:  0.8654 
F-statistic: 191.2 on 7 and 200 DF,  p-value: < 2.2e-16


#Question2

machinedata$MYCT
machinedata$MMIN 
machinedata$MMAX
machinedata$CACH
machinedata$CHMAX

#Question3

machinetrain = machinedata
machinetrain$VendorName = NULL
machinetrain$ModelName = NULL
cor(machinetrain)
                X       MYCT        MMIN        MMAX       CACH       CHMIN
X      1.00000000 -0.1070108 -0.02839829  0.06260811  0.1134489  0.08857411
MYCT  -0.10701078  1.0000000 -0.33707144 -0.37959189 -0.3404142 -0.30073367
MMIN  -0.02839829 -0.3370714  1.00000000  0.75782678  0.6027875  0.52666495
MMAX   0.06260811 -0.3795919  0.75782678  1.00000000  0.6006801  0.56859376
CACH   0.11344889 -0.3404142  0.60278750  0.60068010  1.0000000  0.58812766
CHMIN  0.08857411 -0.3007337  0.52666495  0.56859376  0.5881277  1.00000000
CHMAX  0.03212341 -0.2556289  0.29387722  0.56238751  0.4235497  0.54176232
PRP    0.02875070 -0.3065714  0.79831047  0.86557615  0.7046424  0.60884107
ERP    0.04311519 -0.2878055  0.82311293  0.90417970  0.6874281  0.61009375
            CHMAX        PRP         ERP
X      0.03212341  0.0287507  0.04311519
MYCT  -0.25562893 -0.3065714 -0.28780554
MMIN   0.29387722  0.7983105  0.82311293
MMAX   0.56238751  0.8655762  0.90417970
CACH   0.42354969  0.7046424  0.68742812
CHMIN  0.54176232  0.6088411  0.61009375
CHMAX  1.00000000  0.6213091  0.60628122
PRP    0.62130911  1.0000000  0.96642300
ERP    0.60628122  0.9664230  1.00000000

#Question4

MMAX

#Question5

> model2 = lm(machinetrain$PRP ~ machinetrain$MMAX + machinetrain$MYCT, data = machinetrain)
> summary(model2)

Call:
lm(formula = machinetrain$PRP ~ machinetrain$MMAX + machinetrain$MYCT, 
    data = machinetrain)

Residuals:
    Min      1Q  Median      3Q     Max 
-230.90  -34.33    3.92   27.68  421.33 

Coefficients:
                    Estimate Std. Error t value Pr(>|t|)    
(Intercept)       -3.998e+01  1.067e+01  -3.748 0.000232 ***
machinetrain$MMAX  1.200e-02  5.178e-04  23.180  < 2e-16 ***
machinetrain$MYCT  1.587e-02  2.332e-02   0.680 0.496965    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 80.97 on 205 degrees of freedom
Multiple R-squared:  0.7498,    Adjusted R-squared:  0.7473 
F-statistic: 307.2 on 2 and 205 DF,  p-value: < 2.2e-16

coeffcient of MMAX = 0.12

#Question6

R2 value = 0.7498

#Question7

newmodel2 = step(model2)

Start:  AIC=1830.92
machinetrain$PRP ~ machinetrain$MMAX + machinetrain$MYCT

                    Df Sum of Sq     RSS    AIC
- machinetrain$MYCT  1      3036 1347094 1829.4
<none>                           1344058 1830.9
- machinetrain$MMAX  1   3522743 4866801 2096.6

Step:  AIC=1829.39
machinetrain$PRP ~ machinetrain$MMAX

                    Df Sum of Sq     RSS    AIC
<none>                           1347094 1829.4
- machinetrain$MMAX  1   4024568 5371662 2115.1
> summary(newmodel2)

Call:
lm(formula = machinetrain$PRP ~ machinetrain$MMAX, data = machinetrain)

Residuals:
    Min      1Q  Median      3Q     Max 
-230.65  -35.72    4.01   29.63  425.55 

Coefficients:
                    Estimate Std. Error t value Pr(>|t|)    
(Intercept)       -3.516e+01  7.965e+00  -4.414 1.63e-05 ***
machinetrain$MMAX  1.187e-02  4.784e-04  24.808  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 80.87 on 206 degrees of freedom
Multiple R-squared:  0.7492,    Adjusted R-squared:  0.748 
F-statistic: 615.4 on 1 and 206 DF,  p-value: < 2.2e-16

R2value = 0.7492

#Question8

set.seed(6)
sample = sample.int(n = nrow(machinetrain), size = floor(.75*nrow(machinetrain)), replace = F)
train = machinetrain[sample, ]
test = machinetrain[-sample, ]
testmodel = lm(PRP ~ MMAX + MYCT, data = test)
prediction =predict(testmodel, newdata = train)
SSE = sum((prediction - train$PRP)^2)
SST = sum((mean(train$PRP) - train$PRP)^2) 
R2 = 1 - SSE/SST
R2

0.6386738


















