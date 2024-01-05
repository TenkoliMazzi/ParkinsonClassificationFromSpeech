

# Machine Learning Analysis on Vocal Features for Parkinson's Disease

This repository contains a machine learning analysis script focusing on vocal features to detect Parkinson's disease in a case-control experimental setup.

## Installation

To run this project, you'll need to install the required R packages. You can do this by executing the following commands:
```R
install.packages("devtools")
library("devtools")
install_github("kassambara/factoextra")
install.packages("scatterplot3d")
install.packages("e1071")
```
## Parkinson's Disease (PD) Speech Dataset

The PD database comprises both training and test files, with the training data collected from 20 individuals diagnosed with Parkinson's disease (PWP) and 20 healthy individuals at the Department of Neurology in Cerrahpasa Faculty of Medicine, Istanbul University. The dataset includes multiple types of sound recordings, such as sustained vowels, numbers, words, and short sentences, extracted from 26 voice samples of each subject. Additionally, 26 linear and time–frequency-based features are extracted from each voice sample.
For additional details, please refer to the following reference:

> Erdogdu Sakar, B., Isenkul, M., Sakar, C.O., Sertbas, A., Gurgen, F., Delil, S., Apaydin, H., Kursun,
O., 'Collection and Analysis of a Parkinson Speech Dataset with Multiple Types of Sound
Recordings', IEEE Journal of Biomedical and Health Informatics, vol. 17(4), pp. 828-834, 2013

### Training Data File
Each subject in the training dataset has 26 voice samples, including sustained vowels, numbers, words, and short sentences. The voice samples are organized in the following order:

- 1-3: Sustained vowel (aaa...)
- 4-13: Numbers from 1 to 10
- 14-17: Short sentences
- 18-26: Words

### Test Data File
In the test dataset, 28 PD patients were asked to say sustained vowels 'a' and 'o' three times each, resulting in a total of 168 recordings. The voice samples in the test data file are given in the following order:

- 1-3: Sustained vowel (aaa...)
- 4-6: Sustained vowel (ooo...)

### Features
- 2-27: Features
  - Features 1-5: Jitter (local), Jitter (local, absolute), Jitter (rap), Jitter (ppq5), Jitter (ddp)
  - Features 6-11: Shimmer (local), Shimmer (local, dB), Shimmer (apq3), Shimmer (apq5), Shimmer (apq11), Shimmer (dda)
  - Features 12-14: AC, NTH, HTN
  - Features 15-19: Median pitch, Mean pitch, Standard deviation, Minimum pitch, Maximum pitch
  - Features 20-23: Number of pulses, Number of periods, Mean period, Standard deviation of period
  - Features 24-26: Fraction of locally unvoiced frames, Number of voice breaks, Degree of voice breaks

## Data Preprocessing

The script performs various data preprocessing steps, including handling missing values, normalizing data, and identifying and removing overcorrelated features.
The correlation treshold is varied in order to achieve the best prediction results.
## Principal Component Analysis (PCA)
Principal Component Analysis is used to visualize and reduce the dimensionality of the dataset. The script generates PCA plots before and after removing overcorrelated features.

## Support Vector Machine (SVM)
The script applies SVM with both linear and radial kernels for classification. 
The SVM prediction is applied to the PCA projected datapoints. 
The results are visualized with scatter plots and decision boundaries.
<!-- Centered Image -->
<div class="row">
  <div class="column">
    <img src="https://github.com/TenkoliMazzi/ParkinsonClassificationFromSpeech/blob/main/images/SVMRadialTrain.png" alt="SVM Plot Train" width="45%" height="45%">
  <div class="column">
   <img src="https://github.com/TenkoliMazzi/ParkinsonClassificationFromSpeech/blob/main/images/SVMRadialTest.png" alt="SVM Plot Test" width="45%" height="45%">
  </div>  
</div>

Feel free to adapt the script to your specific dataset and requirements. If you encounter any issues or have questions, please don't hesitate to reach out.

Happy coding!
