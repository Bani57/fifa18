from keras.models import Sequential
from keras.layers import Dense
import numpy as np
from sklearn.model_selection import train_test_split
from keras.metrics import categorical_accuracy
import random
from tensorflow import set_random_seed

np.random.seed(0)
random.seed(0)
set_random_seed(0)

# crossing 33-th column
# total 34 attributes for the skills
X = np.loadtxt("Data/dataset_cleaned.csv", delimiter=',', skiprows=1, usecols=[33 + i for i in range(34)])

print(X.shape)
print(X)

# 0 - goalkeeper
# 1 - defense
# 2 - midfielder
# 3 - striker
col_index_to_pos_dict = {0: 3,
                         1: 2,
                         2: 3,
                         3: 2,
                         4: 2,
                         5: 2,
                         6: 2,
                         7: 1,
                         8: 1,
                         9: 1,
                         10: 3,
                         11: 2,
                         12: 3,
                         13: 2,
                         14: 2,
                         15: 2,
                         16: 2,
                         17: 1,
                         18: 1,
                         19: 1,
                         20: 3,
                         21: 3,
                         22: 2,
                         23: 2,
                         24: 2,
                         25: 1,
                         26: 0
                         }

Y = np.loadtxt("Data/dataset_cleaned.csv", delimiter=',', skiprows=1, usecols=[67 + i for i in range(27)],
               dtype="unicode")
print(Y.shape)
print(Y)

evaluateY = np.vectorize(lambda x: 0 if x == "b'NA'" else float(eval(x).decode('utf-8')))
Y = evaluateY(Y)
print(Y.shape)
print(Y)

finalY = np.zeros(Y.shape)
# finalY = np.zeros((Y.shape[0], 4))
for i, argmaxRow in enumerate(np.argmax(Y, axis=1)):
    finalY[i, argmaxRow] = 1
    # finalY[i, col_index_to_pos_dict[argmaxRow]] = 1
print(finalY.shape)
print(finalY)

# Split data
X_train, X_test, Y_train, Y_test = train_test_split(X, finalY, test_size=0.2, random_state=0)

# Model define
model = Sequential()
model.add(Dense(40, input_dim=34, activation='relu'))
model.add(Dense(30, activation='relu'))
model.add(Dense(27, activation='sigmoid'))
# model.add(Dense(4, activation='sigmoid'))

# Model compile
model.compile(loss='categorical_crossentropy', optimizer='adam', metrics=[categorical_accuracy])

# Model fit
model.fit(X_train, Y_train, epochs=10, batch_size=4, validation_split=0.2)

# Evaluate model
scores = model.evaluate(X_test, Y_test)
print("\n%s: %.2f%%" % (model.metrics_names[1], scores[1] * 100))
