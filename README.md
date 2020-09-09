Introduction

The objective for this project is to perform optimization, tree-based modeling and prediction algorithms on the Ames Housing dataset. The dataset is randomly sampled into training (75%) and test (25%) data. We used generalized linear regression and random foresting to evaluate the relation between sale price and other dependent factors. Upon testing the correlation, we observed a high multicollinearity between dependent variables. For eliminating redundancy in our regression equation, we used the Lasso and Ridge optimization techniques to deal with the variables with high Variable Inflation Factor (more than 10). K-Fold Cross Validation is also used to optimize the random forest model. Using the regression hyper tuning parameters like RMSE, MAE, AIC and BIC, we have selected the best fit model to predict the final sale price. 

Deeper Analysis is provided in the word document and presentation.
