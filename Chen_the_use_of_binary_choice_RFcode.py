import numpy as np
#for terminal: 
#pip3 install scikit-learn
import sklearn
from sklearn.ensemble import RandomForestClassifier

n = 72 # number of products
T = 1000 # size of the training set

offered_set_binary = np.random.randint(2, size = (T,n)) #training assortments
choices = np.random.randint(n, size = T) #randomly generate choices

for i in range(T):
  if offered_set_binary[i][choices[i]] == 0:
      choices[i] = -1 #no purchase if not in assortment



classifier = RandomForestClassifier(n_estimators = 1000, 
                  max_features = 'sqrt', #paper has 'auto' - throws error
                 random_state = 50, min_samples_split = 50)
classifier.fit(offered_set_binary,choices) #fit data by random forest
  
new_set_binary = np.random.randint(2, size = n) #testing assortment
rf_test = classifier.predict_proba([new_set_binary])
rf_test = rf_test.flatten()
  #remove the products not in the assortment
predict_prob = np.append(1,new_set_binary)*rf_test
predict_prob = predict_prob/np.sum(predict_prob)
  
