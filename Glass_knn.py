# -*- coding: utf-8 -*-
"""
Created on Fri May  3 12:14:43 2019

@author: Admin
"""

import numpy as np
import matplotlib.pyplot as plt
import pandas as pd

# Importing the dataset
dataset = pd.read_csv('glass.csv')
dataset.info() 
X = dataset.iloc[:, 0:9].values
y = dataset.iloc[:, 9].values

# Splitting the dataset into the Training set and Test set
from sklearn.model_selection import train_test_split
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size = 0.25, random_state = 0)

# Feature Scaling
from sklearn.preprocessing import StandardScaler
sc = StandardScaler()
X_train = sc.fit_transform(X_train)
X_test = sc.transform(X_test)


# Fitting the classifier to the training set
from sklearn.neighbors import KNeighborsClassifier

classifier = KNeighborsClassifier(n_neighbors = 5, metric= 'minkowski',p=2)
classifier.fit(X_train,y_train)

# Predicting test set results
y_pred = classifier.predict(X_test)

# Making confusion matrix
from sklearn.metrics import confusion_matrix
cm=confusion_matrix(y_test,y_pred)

from sklearn.metrics import accuracy_score
accuracy_score(y_test,y_pred)

