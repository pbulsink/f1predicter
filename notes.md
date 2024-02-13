Crash rate globally? 0.0555 Failure rate globally? 0.0756 Finish rate globally? 0.8689 Quali Position (New) = 18 Finish Position (New) = 18

For both - each race could be 1/10 of average toward truth? e.g. driver race 1 - no crash. New crash rate for driver = mean(c(rep(.0555, 9), 0)) = .04995 next race: mean(c(rep(.04995, 9), 0)) = .044955 next race crash: mean(c(rep(.044955, 9), 1)) = .1404595

or use an adj-mean (weighted to most recent races?) weighted.mean(c(1,0,0,1,0,0,1,0,0,0), w = sqrt(10:1)) fill with global rate \< 20

Win Model Performance (from from 2018 and on):

| Type            | Accuracy | PPV    | NPV    | Sens | Spec   | AUC     | LogLoss | Time    |
|-----------------|----------|--------|--------|------|--------|---------|---------|---------|
| Null            | 0.95     | 0      | .95    |      |        |         |         | 0       |
| Random          |          |        |        |      |        |         |         | 0       |
| glmnet Binomial | 0.964    | 0.7333 | 0.9711 | 0.44 | 0.9916 | 0.9416  | 0.1147  | 27 s    |
| xgboost         | 0.9523?  |        |        |      |        | 0.9478? |         | 1250 s? |
| svm             |          |        |        |      |        |         |         | \>24 h? |

Podium Model Performance (from from 2018 and on):

| Type            | Accuracy | PPV    | NPV    | Sens   | Spec   | AUC    | LogLoss | Time |
|-----------------|----------|--------|--------|--------|--------|--------|---------|------|
| Null            |          |        |        |        |        |        |         | 0    |
| Random          |          |        |        |        |        |        |         | 0    |
| glmnet Binomial | 0.892    | 0.6567 | 0.9284 | 0.5867 | 0.9459 | 0.9226 | 0.2399  | 44 s |
| xgboost         |          |        |        |        |        |        |         |      |
| svm             |          |        |        |        |        |        |         |      |

Top 10 Model Performance (from from 2018 and on):

| Type            | Accuracy | PPV    | NPV    | Sens   | Spec   | AUC    | LogLoss | Time |
|-----------------|----------|--------|--------|--------|--------|--------|---------|------|
| Null            |          |        |        |        |        |        |         | 0    |
| Random          |          |        |        |        |        |        |         | 0    |
| glmnet Binomial | 0.758    | 0.7699 | 0.7471 | 0.7360 | 0.7800 | 0.8329 | 0.511   | 42   |
| xgboost         |          |        |        |        |        |        |         |      |
| svm             |          |        |        |        |        |        |         |      |

Finish Model Performance (from 2018 and on):

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

Model train results glm from 2018 and on
| Model                       | LL     | PPV    | sens   | spec   | npv    | ACC    | AUC    | Kappa  | mcc    | mae    | rmse   | rsq    | Time   |
|-----------------------------|--------|--------|--------|--------|--------|--------|--------|--------|--------|--------|--------|--------|--------|
| Quali Pole Model (early)    | 0.1187 | 0.2500 | 0.0435 | 0.9931 | 0.9518 | 0.9457 | 0.9382 | 0.0602 | --     | --     | --     | --     | 41   s |
| Quali Pos Model (early)     | 2.6083 | --     | --     | --     | --     | 0.1174 | 0.7336 | 0.0709 | 0.0713 | --     | --     | --     | 317  s |
| Quali Pos Reg Model (early) | --     | --     | --     | --     | --     | --     | --     | --     | --     | 2.9376 | 3.8398 | 0.5576 | 22.5 s |   
| Quali Pole Model (late)     | 0.1186 | 0.5000 | 0.1739 | 0.9908 | 0.9580 | 0.9500 | 0.9374 | 0.2384 | --     | --     | --     | --     | 54   s |
| Quali Pos Model (late)      | 2.6161 | --     | --     | --     | --     | 0.1500 | 0.7634 | 0.1053 | 0.1058 | --     | --     | --     | 380  s |
| Quali Pos Reg Model (late)  | --     | --     | --     | --     | --     | --     | --     | --     | --     | 2.5366 | 3.3560 | 0.6619 | 23   s |

Model train results xgb from 2018 and on
| Model                       | LL     | PPV    | sens   | spec   | npv    | ACC    | AUC    | Kappa  | mcc    | mae    | rmse   | rsq    | Time   |
|-----------------------------|--------|--------|--------|--------|--------|--------|--------|--------|--------|--------|--------|--------|--------|
| Quali Pole Model (early)    | 0.1594 | 0.2143 | 0.1304 | 0.9748 | 0.9552 | 0.9326 | 0.8857 | 0.1292 | --     | --     | --     | --     | 1936 s |
| Quali Pos Model (early)     | 2.6676 | --     | --     | --     | --     | 0.1543 | 0.7624 | 0.1098 | 0.1103 | --     | --     | --     | 15918s |
| Quali Pos Reg Model (early) | --     | --     | --     | --     | --     | --     | --     | --     | --     | 2.8156 | 3.5920 | 0.6146 | 2927 s |
| Quali Pole Model (late)     | 0.1585 | 0.2353 | 0.1739 | 0.9703 | 0.9571 | 0.9304 | 0.9278 | 0.1645 | --     | --     | --     | --     | 2007 s |
| Quali Pos Model (late)      | 2.5719 | --     | --     | --     | --     | 0.1500 | 0.7480 | 0.1053 | 0.1055 | --     | --     | --     | 16341s |
| Quali Pos Reg Model (late)  | --     | --     | --     | --     | --     | --     | --     | --     | --     | 2.9903 | 3.8697 | 0.5502 | 3031 s |
