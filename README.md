# Robust-Optimization-using-Machine-Learning-for-building-Uncertainty-Sets

## Implementation of Research Paper
Paper - https://arxiv.org/abs/1407.1097

## Creation of Uncertainty Sets
Created Uncertainty Sets from predictive modeling and the past data. These uncertainty sets capture the set of uncertain future conditions, which is useful for 'Robust Optimization'.

## Methodologies
Method 1 - Conditional quantile regression method estimates the 95th and 5th percentiles of the output of a new feature vector and creates the uncertainty set to be the values between the two estimates.

Method 2 - Involves adding a ‘wiggle room’ to the ordinary least squares loss for capturing the class of good models that have a low least squares loss.

All these methods result in uncertainty sets, which can be used to solve problems of optimization which are robust to any uncertainty in the future.

## Summary
Built uncertainty sets for demand forecasting and solved the optimization problem of bike rebalancing within stations.

## File Description
hubway_data_preparation file contains the analysis of Hubway data. The data is prepared in a generalized format which is given as inputs for invoking the different methods of constructing uncertainty sets (decribed in methods_script file).

methods_script file describes the different methods for constructing the sets. This script has methods which take inputs in a standardized format. Different dataset files can be feeded here to calculate the values of uncertainty sets.
