# Telco-project
## Customer churn prediction

This thesis explores the concept of churn and its surrounding factors within the telecommunications sector. Churn refers to the loss of customers from a company or service over a given period.

The first part of the study addresses churn in the context of Customer Relationship Management (CRM) and examines how data analysis can help companies predict which customers are more likely to switch providers (Customer Churn Prediction). This allows for strategies to retain customers (Churn Management). Key factors leading up to churn, such as customer satisfaction, experience, and care, are discussed as they play essential roles in reducing churn rates and increasing retention rates.

The second part of the study presents statistical methods for predicting churn, along with various performance metrics to evaluate model effectiveness. Additionally, the issue of class imbalance, common in real-world datasets, is addressed with a solution involving oversampling techniques.

Finally, the third part applies the theoretical insights to a case study involving a telecommunications company in the United States. The initial section conducts an exploratory data analysis to extract insights about customer characteristics, subscription plans, usage patterns, and spending habits. These aspects help identify behavioral trends, enabling the company to detect potential churn. In the second section, the classification models introduced earlier are applied to both balanced and imbalanced datasets, also considering optimal threshold selection. The goal is to determine which model performs best in each context based on performance metrics.

The analysis of performance metrics across various contexts suggests that the careful selection of an optimal threshold has a more significant impact on model performance than data balancing alone. In the case study, among the models tested, Random Forest and Support Vector Machine yielded the best results for customer churn prediction.

Future research could explore alternative balancing techniques to assess their impact on model performance, such as Synthetic Minority Oversampling (SMOTE) and Borderline SMOTE. These methods, as described in section 2.2.1, go beyond simple random oversampling by using more advanced techniques for oversampling the minority class.

While this study focused on supervised learning models, there is potential for applying unsupervised learning models, especially clustering techniques, which may offer further improvements in churn prediction performance.
