# Comparative Engagement

Analysis for paper: Motivations and Views on State Support for Refugees: Distinguishing between Politically and Socially Engaged

The corresponding data can be accessed at: <https://doi.org/10.5281/zenodo.13143527>

Below is an overview of the codebase:

## Data cleaning & wrangling

**datacleaning.R**

-   Load "engagement survey" dataset (engagement.dta)

-   Data wrangling

    -   **Combine motivation questions**: The political and engaged persons' motivation questions were coded as separate questions and are combined into single questions by coalescing them.

    -   **Select and rename demographic and engagement variables**: The dataset is filtered to include key demographic and engagement variables, which are then renamed for consistency.

    -   **Standardize demographic variables**: Various demographic variables (e.g., age, gender, employment status, education level, income, and political party) are standardized for consistency.

    -   **Label the dataset**: The dataset is labeled for identification during analysis.

-   **Load and process private accommodation dataset (private_accommodation.csv)**

    -   **Select and rename variables**: Relevant demographic, motivation, engagement, and state engagement variables are selected and renamed for consistency with the engagement dataset.

    -   **Standardize variables**: Demographic and motivation variables are standardized to ensure consistency between datasets.

    -   **Filter and label the dataset**: The dataset is filtered to include only those who are engaged and hosted, and labeled for identification during analysis.

-   **Merge datasets**: The engagement and private accommodation datasets are merged into a single dataset (`compeng`).

-   **Categorize engagement**: Respondents are categorized based on their engagement and hosting status.

## Modeling

**Modeldevelopment.R**

-   **Standardizing Numeric Variables**: Key numeric variables (e.g., age, democracy, religious, giveback, helpothers, learnsth, improvecoexistence, helpsociety, helprefugees) are standardized.

```{=html}
<!-- -->
```
-   **Converting Characters to Factors**: Educational attainment is converted to numeric factors and standardized.

-   **Gender Encoding**: Gender is encoded as a binary variable.

### Question 1: Influence of Motivation on Engagement Type

1.  **Data Splitting**: The dataset is split into training (90%) and test (10%) sets for evaluation.

2.  **Logistic Regression Models**:

    -   **Training Data Model**: A logistic regression model is trained using the training dataset to predict political engagement based on various motivations and demographic factors.

    -   **Full Data Model**: A logistic regression model is trained using the complete standardized dataset.

3.  **Model Summary and Interpretation**:

    -   The model coefficients are examined to interpret the influence of demographic and motivational factors.

    -   Point estimates for key variables (e.g., democratic and religious motivations) are calculated and interpreted.

4.  **Model Validation**:

    -   **ROC Curve**: The model's performance is evaluated using ROC curve analysis on the test dataset.

    -   **Brier Score**: The Brier score is computed to assess the model's accuracy.

    -   **Cross-Validation**: Leave-one-out cross-validation (LOOCV) is performed to validate the model.

5.  **Robustness Checks**: Additional models are developed excluding respondents who hosted refugees to check the robustness of the findings.

### Question 2: Influence of Engagement Type on Perspectives on State Support

1.  **Data Preparation**: The dataset is filtered and prepared to include key variables for the analysis.

2.  **Linear Regression Model**:

    -   A linear regression model is developed to examine the influence of engagement type on respondents' perspectives on state support.

3.  **Robustness Checks**: Additional models are developed excluding respondents who hosted refugees to verify the robustness of the findings.

## Visualization

figures.R

The figures file contains the code to reproduce tables of demographic variables and visualizations of regression models.
