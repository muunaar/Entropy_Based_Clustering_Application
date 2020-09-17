# Application: Entropy Based Clustering

This repository holds the code source of the implementation of an algorithm for entropy based clustering of individuals performing tasks. 
The project is built upon two major functional libraries for Scala [Cats](https://typelevel.org/cats/) and [Cats Effects](https://typelevel.org/cats-effect/). Also, It uses [scala-parser-combinators](https://github.com/scala/scala-parser-combinators) for parsing file rows to identify resources and their related tasks, [ScalaTest](https://www.scalatest.org) for the part related to unit testing.


## Input / Output

The solution proposed uses clustering based on entropy based for classifying the individuals according to the similar tasks performed. 
The start of the program consists of reading a log file that contains information about individuals and the tasks they are involved in. The similarity threshold used for clustering is also entered by the user. 


### Input 

The input file has the following schema: `(Resource Activity1,Activity2,Activity3)`
 
The similarity threshold should be between 0.7 and 0.9 for optimal clustering results.

## Example :

 ```
 Mike	Verification,Validation  
 Sara	Information,Entry  
 John	Verification,Impression 
 ```
 The similarity threshold :
 
  ```
  threshold = 0.8
 ```
 
### Results

 ```
 Sara
 John, Mike

 ```
 The results presented here illustrate the clusters we have two clusters, the first that consists of a single node cluster and the second cluster with two resources John and Mike.
 
### How to use the project 
``` 
- clone the project
- run
- Enter similarity threshold
- Enter filename (data.dat)
```

### License

This content of this project itself is not licensed under any licenses. 


