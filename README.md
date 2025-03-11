
# Mapping Obesity: Leveraging Machine Learning to Predict Lifestyle Impact


## Problem
The prevalence of obesity has reached alarming levels globally, with significant health implications, such as cardiovascular diseases, diabetes, and other chronic conditions.1,2 In countries such as Mexico, Peru, and Colombia, the burden of obesity is particularly concerning, necessitating effective interventions for prevention and management.3 Traditional methods for estimating obesity levels rely on subjective assessments and may lack accuracy. Therefore, there is a pressing need for robust predictive models that can accurately estimate obesity levels based on individual characteristics, including eating habits and physical condition.4,5 To comprehend the multifaceted nature of obesity and its determinants, we analyzed existing research and literature on obesity and its determinants enabling us to identify trends, knowledge gaps, and potential avenues for exploration in our predictive modeling efforts. By leveraging machine learning techniques, we aimed to develop predictive models that can effectively estimate obesity levels in individuals in Mexico, Peru, and Colombia based on demographic characteristics and lifestyle behaviors. 

### Data
The Estimation of Obesity Levels Based On Eating Habits and Physical Condition dataset obtained from the UC Irvine Machine Learning Repository was utilized for the present analysis.6  The dataset consists of 2111 entries and 17 features, encompassing a range of variables including age, weight, height, eating habits, and physical condition indicators. Underweight and normal obesity categories were collapsed and overweight and obese categories were collapsed to create a binary outcome. To handle imputed data for categorical predictor variables, the original dataset was manipulated to create three new datasets. One dataset involved rounding, where imputed values were rounded to the nearest whole number. In the second dataset, imputed values were entirely removed. The third dataset comprised randomly selected data sampled from the rounded dataset. 

### Approach
We employed supervised machine learning techniques and algorithms in order to analyze the dataset. Preprocessing techniques and methods were utilized, including handling missing values, encoding categorical variables, and scaling numerical features. The dataset was split into training (80%) and test data (20%). The inclusion of different features in each model was based on regularization/shrinkage using 5 fold-CV. Classification algorithms were explored, including logistic regression, and decision trees, given that our outcome is categorical. Models were trained on the training dataset. 
For logistic regression, a threshold of 0.5 was set for binary predictions. This threshold is set as a part of the hyperparameter tuning process. In the case of decision trees, the size of the tree was determined by plotting deviance against the size of the tree. These algorithms were selected apriori based on data structure and aim of the present analysis. 

### Evaluation
The performance of each trained model will be evaluated on the testing dataset. Performance metrics such as testing and training errors, confusion matrix (accuracy, sensitivity, and specificity), and area under the receiver operating characteristic (ROC-AUC) will be calculated. Based on this criteria the best performing model will be selected. These metrics collectively provide a comprehensive assessment of each model's performance, enabling us to determine which model performs more effectively in our study.
In our analysis, we evaluated the models using key performance metrics, including the area under the ROC curve (ROC-AUC) and confusion matrix. Additionally, we will compare training and testing errors to assess the models' generalization ability and identify potential overfitting or underfitting issues. To ensure the models' stability and robustness, we will employ 5-fold cross-validation to estimate misclassification errors. This approach will allow us to evaluate how well each model generalizes to unseen data and provide a more reliable estimation of their predictive performance. 
Training and testing errors will assess the models' generalization ability and identify potential overfitting or underfitting issues. To ensure the models' stability and robustness, we will employ 5-fold cross-validation to estimate misclassification errors. This approach will allow us to evaluate how well each model generalizes to unseen data and provide a more reliable estimation of their predictive performance. 

---
## Project Update & Results

### Changes	
In the original project proposal, we utilized one dataset (the full dataset), but given the imputed values we created three new datasets. Additionally, we had planned to perform multinomial logistic regression given that our outcome variable contained seven categories. However, for feasibility and the examination of the AUCROC we opted to create a binary outcome variable and employ logistic regression. 

### Results
Using 5-fold Lasso regularization, each dataset had features selected, resulting in different combinations of 10 out of the 16 predictors. Across all three dataset types, logistic regression consistently achieved higher AUC values compared to the decision tree algorithm. Specifically, logistic regression yielded AUC values of 0.9049 (full dataset), 0.9194 (rounded subset), and 0.7844 (removed subset). The decision tree algorithm demonstrated competitive AUC values, slightly lower than logistic regression. The rounded subset consistently outperformed the full dataset and removed subset, indicating its contribution to better discrimination ability. While the full dataset demonstrated adequate performance, logistic regression achieved an AUC of 0.9049, slightly lower than the rounded subset.
Confusion matrix analysis findings provided insights into the logistic regression model's accuracy across datasets. In the full dataset, the model achieved an accuracy of 87.94%, with high sensitivity (94.94%) and moderate specificity (67.29%). Similarly, the rounded subset exhibited robust sensitivity (95.59%) but lower specificity (69.23%). Additionally, the removed subset showed accuracy at 73.8%, considering both true positive and true negative rates. These findings complemented the AUC values, offering a comprehensive understanding of the model's predictive capabilities across various scenarios. Misclassification errors were also calculated to assess the model's performance, with lower misclassification errors indicating better classification performance, along with higher sensitivity and specificity values, which are reflected in a ROC curve that approaches the upper-left corner.

---
## Next Steps
While the manipulation of imputed data provided valuable insights, using a more comprehensive dataset with complete and well-defined data would likely improve the accuracy and robustness of the models. Exploring ensemble methods or neural networks as next steps can enhance the project's predictive accuracy, given obesity’s multifaceted nature. These methods offer stronger performance compared to individual models and effectively capture non-linear relationships. Additionally, retaining the outcome variable in its initial seven categories rather than collapsing it into binary groups may provide a more interpretable representation of the data.

---
## References
Koliaki C, Dalamaga M, Liatis S. Update on the Obesity Epidemic: After the Sudden Rise, Is the Upward Trajectory Beginning to Flatten? [published correction appears in Curr Obes Rep. 2023 Oct 17;:]. Curr Obes Rep. 2023;12(4):514-527. doi:10.1007/s13679-023-00527-y
Powell-Wiley TM, Poirier P, Burke LE, et al. Obesity and Cardiovascular Disease: A Scientific Statement From the American Heart Association. Circulation. 2021;143(21):e984-e1010. doi:10.1161/CIR.0000000000000973
Shamah-Levy T, Cuevas-Nasu L, Gaona-Pineda EB, Valenzuela-Bravo DG, Méndez Gómez-Humarán I, Ávila-Arcos MA. Childhood obesity in Mexico: Influencing factors and prevention strategies. Front Public Health. 2022;10:949893. Published 2022 Aug 18. doi:10.3389/fpubh.2022.949893
An R, Shen J, Xiao Y. Applications of Artificial Intelligence to Obesity Research: Scoping Review of Methodologies. J Med Internet Res. 2022;24(12):e40589. Published 2022 Dec 7. doi:10.2196/40589
Gozukara Bag HG, Yagin FH, Gormez Y, et al. Estimation of Obesity Levels through the Proposed Predictive Approach Based on Physical Activity and Nutritional Habits. Diagnostics (Basel). 2023;13(18):2949. Published 2023 Sep 14. doi:10.3390/diagnostics13182949
Palechor FM, Manotas AH. Dataset for estimation of obesity levels based on eating habits and physical condition in individuals from Colombia, Peru and Mexico. Data Brief. 2019;25:104344. Published 2019 Aug 2. doi:10.1016/j.dib.2019.104344

