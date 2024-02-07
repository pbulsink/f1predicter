Crash rate globally? 0.0555 Failure rate globally? 0.0756 Finish rate globally? 0.8689 Quali Position (New) = 18 Finish Position (New) = 18

For both - each race could be 1/10 of average toward truth? e.g. driver race 1 - no crash. New crash rate for driver = mean(c(rep(.0555, 9), 0)) = .04995 next race: mean(c(rep(.04995, 9), 0)) = .044955 next race crash: mean(c(rep(.044955, 9), 1)) = .1404595

or use an adj-mean (weighted to most recent races?) weighted.mean(c(1,0,0,1,0,0,1,0,0,0), w = sqrt(10:1)) fill with global rate \< 20

Win Model Performance (from \>=2018):

| Type            | Accuracy | PPV    | NPV    | Sens | Spec   | AUC     | LogLoss | Time    |
|-----------------|----------|--------|--------|------|--------|---------|---------|---------|
| Null            | 0.95     | 0      | .95    |      |        |         |         | 0       |
| Random          |          |        |        |      |        |         |         | 0       |
| glmnet Binomial | 0.964    | 0.7333 | 0.9711 | 0.44 | 0.9916 | 0.9416  | 0.1147  | 27 s    |
| xgboost         | 0.9523?  |        |        |      |        | 0.9478? |         | 1250 s? |
| svm             |          |        |        |      |        |         |         | \>24 h? |

Podium Model Performance (from \>=2018):

| Type            | Accuracy | PPV    | NPV    | Sens   | Spec   | AUC    | LogLoss | Time |
|-----------------|----------|--------|--------|--------|--------|--------|---------|------|
| Null            |          |        |        |        |        |        |         | 0    |
| Random          |          |        |        |        |        |        |         | 0    |
| glmnet Binomial | 0.892    | 0.6567 | 0.9284 | 0.5867 | 0.9459 | 0.9226 | 0.2399  | 44 s |
| xgboost         |          |        |        |        |        |        |         |      |
| svm             |          |        |        |        |        |        |         |      |

: Top 10 Model Performance (from \>=2018):

| Type            | Accuracy | PPV    | NPV    | Sens   | Spec   | AUC    | LogLoss | Time |
|-----------------|----------|--------|--------|--------|--------|--------|---------|------|
| Null            |          |        |        |        |        |        |         | 0    |
| Random          |          |        |        |        |        |        |         | 0    |
| glmnet Binomial | 0.758    | 0.7699 | 0.7471 | 0.7360 | 0.7800 | 0.8329 | 0.511   | 42   |
| xgboost         |          |        |        |        |        |        |         |      |
| svm             |          |        |        |        |        |        |         |      |

: Finish Model Performance (from \>=2018):

| Type            | Accuracy | PPV    | NPV | Sens | Spec | AUC    | LogLoss | Time |
|-----------------|----------|--------|-----|------|------|--------|---------|------|
| Null            |          |        |     |      |      |        |         | 0    |
| Random          |          |        |     |      |      |        |         | 0    |
| glmnet Binomial | 0.8320   | 0.8320 | NA  | 1    | 0    | 0.5865 | 0.4466  | 42 s |
| xgboost         |          |        |     |      |      |        |         |      |
| svm             |          |        |     |      |      |        |         |      |

Position Models:

| Type                     | RMSE   | R2     | Time |
|--------------------------|--------|--------|------|
| Null                     |        |        | 0    |
| Random                   |        |        | 0    |
| glmnet lin_reg (\>=2018) | 4.7118 | 0.3437 | 19 s |
| glmnet lin_reg           | 4.4636 | 0.401  | 101s |
| xgboost                  |        |        |      |
| svm                      |        |        |      |
