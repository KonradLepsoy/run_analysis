Below is the list of the columns of the dataset. Column 1 identifies the subject, column 2 describes the activity, and the rest of the columns is the mean of the column, where the column by itself is described at the very bottom.

 [1] "Subject"                  
 [2] "ActivityLabel"            
 [3] "tBodyAcc-mean-X"          
 [4] "tBodyAcc-mean-Y"          
 [5] "tBodyAcc-mean-Z"          
 [6] "tGravityAcc-mean-X"       
 [7] "tGravityAcc-mean-Y"       
 [8] "tGravityAcc-mean-Z"       
 [9] "tBodyAccJerk-mean-X"      
[10] "tBodyAccJerk-mean-Y"      
[11] "tBodyAccJerk-mean-Z"      
[12] "tBodyGyro-mean-X"         
[13] "tBodyGyro-mean-Y"         
[14] "tBodyGyro-mean-Z"         
[15] "tBodyGyroJerk-mean-X"     
[16] "tBodyGyroJerk-mean-Y"     
[17] "tBodyGyroJerk-mean-Z"     
[18] "tBodyAccMag-mean"         
[19] "tGravityAccMag-mean"      
[20] "tBodyAccJerkMag-mean"     
[21] "tBodyGyroMag-mean"        
[22] "tBodyGyroJerkMag-mean"    
[23] "fBodyAcc-mean-X"          
[24] "fBodyAcc-mean-Y"          
[25] "fBodyAcc-mean-Z"          
[26] "fBodyAccJerk-mean-X"      
[27] "fBodyAccJerk-mean-Y"      
[28] "fBodyAccJerk-mean-Z"      
[29] "fBodyGyro-mean-X"         
[30] "fBodyGyro-mean-Y"         
[31] "fBodyGyro-mean-Z"         
[32] "fBodyAccMag-mean"         
[33] "fBodyBodyAccJerkMag-mean" 
[34] "fBodyBodyGyroMag-mean"    
[35] "fBodyBodyGyroJerkMag-mean"
[36] "tBodyAcc-std-X"           
[37] "tBodyAcc-std-Y"           
[38] "tBodyAcc-std-Z"           
[39] "tGravityAcc-std-X"        
[40] "tGravityAcc-std-Y"        
[41] "tGravityAcc-std-Z"        
[42] "tBodyAccJerk-std-X"       
[43] "tBodyAccJerk-std-Y"       
[44] "tBodyAccJerk-std-Z"       
[45] "tBodyGyro-std-X"          
[46] "tBodyGyro-std-Y"          
[47] "tBodyGyro-std-Z"          
[48] "tBodyGyroJerk-std-X"      
[49] "tBodyGyroJerk-std-Y"      
[50] "tBodyGyroJerk-std-Z"      
[51] "tBodyAccMag-std"          
[52] "tGravityAccMag-std"       
[53] "tBodyAccJerkMag-std"      
[54] "tBodyGyroMag-std"         
[55] "tBodyGyroJerkMag-std"     
[56] "fBodyAcc-std-X"           
[57] "fBodyAcc-std-Y"           
[58] "fBodyAcc-std-Z"           
[59] "fBodyAccJerk-std-X"       
[60] "fBodyAccJerk-std-Y"       
[61] "fBodyAccJerk-std-Z"       
[62] "fBodyGyro-std-X"          
[63] "fBodyGyro-std-Y"          
[64] "fBodyGyro-std-Z"          
[65] "fBodyAccMag-std"          
[66] "fBodyBodyAccJerkMag-std"  
[67] "fBodyBodyGyroMag-std"     
[68] "fBodyBodyGyroJerkMag-std" 


Feature Selection 
=================

The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 

Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 

Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals). 

These signals were used to estimate variables of the feature vector for each pattern:  
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.

tBodyAcc-XYZ
tGravityAcc-XYZ
tBodyAccJerk-XYZ
tBodyGyro-XYZ
tBodyGyroJerk-XYZ
tBodyAccMag
tGravityAccMag
tBodyAccJerkMag
tBodyGyroMag
tBodyGyroJerkMag
fBodyAcc-XYZ
fBodyAccJerk-XYZ
fBodyGyro-XYZ
fBodyAccMag
fBodyAccJerkMag
fBodyGyroMag
fBodyGyroJerkMag

The set of variables that were estimated from these signals are: 

mean(): Mean value
std(): Standard deviation
mad(): Median absolute deviation 
max(): Largest value in array
min(): Smallest value in array
sma(): Signal magnitude area
energy(): Energy measure. Sum of the squares divided by the number of values. 
iqr(): Interquartile range 
entropy(): Signal entropy
arCoeff(): Autorregresion coefficients with Burg order equal to 4
correlation(): correlation coefficient between two signals
maxInds(): index of the frequency component with largest magnitude
meanFreq(): Weighted average of the frequency components to obtain a mean frequency
skewness(): skewness of the frequency domain signal 
kurtosis(): kurtosis of the frequency domain signal 
bandsEnergy(): Energy of a frequency interval within the 64 bins of the FFT of each window.
angle(): Angle between to vectors.

Additional vectors obtained by averaging the signals in a signal window sample. These are used on the angle() variable:

gravityMean
tBodyAccMean
tBodyAccJerkMean
tBodyGyroMean
tBodyGyroJerkMean

The complete list of variables of each feature vector is available in 'features.txt'