# project-7
Group 7 project for DSCI445 @ CSU

# Group Members
Austin Lackey, Ethan Powers and Danny Laposata

# Breast Cancer Data
Features are computed from a digitized image of a fine needle aspirate (FNA) of a breast mass. They describe characteristics of the cell nuclei present in the image.

# What we did

The goal of our analysis is to predict whether a cell is benign or malignant based on the `32 variables`.
A cell is considered benign if it is not cancerous and malignant if it is cancerous.
Normally in most machine learning models, we do our best to train the model to reduce the overall error rate.
While this is an important goal, our group was more concerned with the `Type-II error rate`.
By reducing the `Type-II error rate`, we can ensure that we are not making the mistake of classifying a malignant cell as benign.
This is important in the world of Oncology because if a malignant cell is classified as benign, it could lead to a patient not receiving the proper treatment.
Whereas if a benign cell is classified as malignant, the patient may be alarmed, but a false alarm is better than a missed diagnosis.

In order to achieve our goal, we conducted the following steps:

1. Data Cleaning
2. Explatory Data Analysis
3. Classification Analysis
4. Regression Analysis
5. Overall Analysis Summary


# Directions

## Install Packages
### Ensure the neccesary packages are installed, see the list below...

* tidyverse
* GGally
* knitr
* boot
* caret
* randomForest
* e1071
* class

*Note:* If you do not have one of these packages installed. use the command `install.packages("<package-name>")` to install it.


# Paper
### Run/Knit the *"paper.rmd"* file and a **"paper.pdf"** should export.
### Keep in mind this will take many minutes to export.

# Presentation
### Run/Knit the *"powerpoint.rmd"* file and a **"powerpoint.pptx"** powerpoint should export.
### Keep in mind this will take many minutes to export.

# Works Cited

- Breast Cancer Wisconsin (Diagnostic) Data Set:
    - https://www.kaggle.com/datasets/uciml/breast-cancer-wisconsin-data?datasetId=180&sortBy=voteCount
- Definition of Features (Variables):
    - https://www.causeweb.org/usproc/sites/default/files/usclap/2017-2/Evaluating_Benign_and_Malignant_Breast_Cancer_Cells_from_Fine-Needle_Aspirates.pdf