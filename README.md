# Application: Entropy Based Clustering

##  Solution

## Mining similar tasks 
Organizational perspectives of business process mining consist of organizing and classifying the organization in terms of missions, roles as well as the interactions between the performers of the tasks. However, standard clustering approaches are not always proper business
processes as they are known for their complex, flexible nature. Therefore, we opt for entropy based clustering algorithm for extracting tasks. The algorithm is applied on event logs.

## Input / Output

### Input file

The input file has  the following schema: `(Resource, Activity1, Activity2, Activity 3)`
Plus the simularity threashold

#### Example :

 ```
 Mike  Verification  validation  
 Sara  information entry  
 John  Verification  impression 
 ```
 The similarity threashold :
 
  ```
  \alpha = 0.7
 ```
### Output  
 ```
 Sara
 John, Mike

 ```
 ### Remarks





