# Predicting-the-diagnosis-of-Coronary-Artery-Disease-using-Statistical-Models
Heart disease is the leading cause of death in the United States, causing about 1 in 4 deaths. The most common type of heart disease is coronary artery disease (CAD), which can lead to heart attack. 
It is one of the leading causes of morbidity and mortality among the population of the world. 
These diseases take an economic toll, as well, costing our health care system $214 billion per year and causing $138 billion in lost productivity on the job.
A heart disease prediction system can assist medical professionals in predicting heart disease based on the clinical data of patients. 
Early prediction of a possible heart disease will lead to better decision making for a clinical treatment.
I have taken a heart disease dataset from UCI Machine Learning Repository: https://archive.ics.uci.edu/ml/datasets/heart+disease .
The dataset comprises 303 samples and each of them has 14 characteristics. There are 13 predictors and one target variable.
EDA typically detects errors, finds appropriate data, checks assumptions, and determines the correlation among the explanatory variables. It enhances the insight into a given dataset and identifies anomalies.
We split the 13 characteristics into two categories, 6 of them as numerical and rest 7 as categorical.
1. Age
2. Gender
3. Chest Pain Type (cp)
4. Resting Blood Pressure in mm Hg (trestbps)
5. Cholesterol (chol)
6. Fasting Blood Sugar (fbs)
7. Resting electrocardiographic results (restecg)
8. Max Heart Rate (thalach)
9. Exercise Angina (exang)
10. Old Peak
11. Slope
12. Number of Major Vessels (ca)
13. Thal (thalassemia)
14. Target (Diagnosis) [Response Variable]

The statistical model building is done in two parts - Variabel Selection and Model Building
1. Part-1 : Variable selection using methods like Best Subset Algorith using Mallow's Cp, Stepwise Regression, Ridge Regression and Lasso. The AIC (Akaike’s Information Criteria) is used to compare these models. 
Two models are selected. Model-1 has all 13 predicting variables and Model-2 has onnly 9 of these 13 variables. 
2. Part-2 : Model development with Logistic Regression, Decision Tree and k-nearest neighbors. Confusion matrix are created for both models applying all three methods. Mean error and type II errors are calculated.
It is concluded that Logistic regression applied to Model-1 (Full Model with all 13 variables) generate the best model with least mean and type II error. 

The R file contains the code used for the analysis. Word file has a detailes report and Presentation is also attached which has a breif summary of the analysis. 
