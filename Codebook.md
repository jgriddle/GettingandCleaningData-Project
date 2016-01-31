This dataset represents a set of observations recorded over a range of Activity types per 
 subject for a group of 30 subjects. Each observation in the dataset include Subject number,
 Activity type and the mean and standard deviation a set of variables that were estimated 
 from the following kinematic measured signals: 

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

NOTE: the variable names follow this protocol:
      "Body" would reference a coordinate system centered at subject center of mass
      '-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.
      mean(): Mean value
      std(): Standard deviation
      mag: magnitude (sqrt(x^2 + y^2 +z^2))
      All variables 03 to 68 are normalized 0 to 1
      
Units: Acceleration terms (normally length/sec^2) are referenced to gravity (in terms
        of G's). Jerk is (length/sec^3) and gyro units are angular-velocity/sec 
  
_________________________________________________________________________________
    |                                 |
Col |  Name                           |  Other information not mentioned above
____|_________________________________|__________________________________________
01  |  Subject                        | 1 - 30
02  |  Activity                       | WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING
03  |  tBodyAcc-mean()-X              | 
04  |  tBodyAcc-mean()-Y              |
05  |  tBodyAcc-mean()-Z              |
06  |  tBodyAcc-std()-X               |
07  |  tBodyAcc-std()-Y               |
08  |  tBodyAcc-std()-Z               |
09  |  tGravityAcc-mean()-X           |
10  |  tGravityAcc-mean()-Y           |
11  |  tGravityAcc-mean()-Z           |
12  |  tGravityAcc-std()-X            |
13  |  tGravityAcc-std()-Y            |
14  |  tGravityAcc-std()-Z            |
15  |  tBodyAccJerk-mean()-X          |
16  |  tBodyAccJerk-mean()-Y          |
17  |  tBodyAccJerk-mean()-Z          |
18  |  tBodyAccJerk-std()-X           |
19  |  tBodyAccJerk-std()-Y           |
20  |  tBodyAccJerk-std()-Z           |
21  |  tBodyGyro-mean()-X             |
22  |  tBodyGyro-mean()-Y             |
23  |  tBodyGyro-mean()-Z             |
24  |  tBodyGyro-std()-X              |
25  |  tBodyGyro-std()-Y              |
26  |  tBodyGyro-std()-Z              |
27  |  tBodyGyroJerk-mean()-X         |
28  |  tBodyGyroJerk-mean()-Y         |
29  |  tBodyGyroJerk-mean()-Z         |
30  |  tBodyGyroJerk-std()-X          |
31  |  tBodyGyroJerk-std()-Y          |
32  |  tBodyGyroJerk-std()-Z          |
33  |  tBodyAccMag-mean()             |
34  |  tBodyAccMag-std()              |
35  |  tGravityAccMag-mean()          |
36  |  tGravityAccMag-std()           |
37  |  tBodyAccJerkMag-mean()         |
38  |  tBodyAccJerkMag-std()          |
39  |  tBodyGyroMag-mean()            |
40  |  tBodyGyroMag-std()             | 
41  |  tBodyGyroJerkMag-mean()        |  
42  |  tBodyGyroJerkMag-std()         |
43  |  fBodyAcc-mean()-X              |
44  |  fBodyAcc-mean()-Y              |
45  |  fBodyAcc-mean()-Z              |
46  |  fBodyAcc-std()-X               |
47  |  fBodyAcc-std()-Y               |
48  |  fBodyAcc-std()-Z               |
49  |  fBodyAccJerk-mean()-X          |
50  |  fBodyAccJerk-mean()-Y          |
51  |  fBodyAccJerk-mean()-Z          |
52  |  fBodyAccJerk-std()-X           |     
53  |  fBodyAccJerk-std()-Y           |
54  |  fBodyAccJerk-std()-Z           |   
55  |  fBodyGyro-mean()-X             |   
56  |  fBodyGyro-mean()-Y             |  
57  |  fBodyGyro-mean()-Z             |
58  |  fBodyGyro-std()-X              |
59  |  fBodyGyro-std()-Y              |
60  |  fBodyGyro-std()-Z              |
61  |  fBodyAccMag-mean()             |
62  |  fBodyAccMag-std()              |
63  |  fBodyBodyAccJerkMag-mean()     |   
64  |  fBodyBodyAccJerkMag-std()      |
65  |  fBodyBodyGyroMag-mean()        |
66  |  fBodyBodyGyroMag-std()         | 
67  |  fBodyBodyGyroJerkMag-mean()    |
68  |  fBodyBodyGyroJerkMag-std()     |