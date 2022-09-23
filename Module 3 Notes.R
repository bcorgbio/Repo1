#9/19

#model = mathematical explanations of a process/system
#R notation: y~x (y is predicated by x)
  #y~x+a+b+c...
    #y is predicted by x plus these variables
      #all variables (categories) have same slope, different means/y-intercepts
      #assessing the difference in mean between variables/categories
  #y~x*a
    #y has a different response depending on the variable
      #variables (categories) have different slopes
      #assessing the difference in slope, i.e. the response of y to x (for each variable/category)
#linear model: lm(y~x)

#AIC -> gives probability that a specific model could have produced the data
  #aka finding the simplest  model that has a good fit to the data
  #think of the inverse of the P-value
  #AIC = 2k-2ln(L)
    #k = number of model parameters
      #accounts for additional degrees of freedom
    #L = maximum value of the likelihood function of the model

