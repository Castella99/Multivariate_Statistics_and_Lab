# %%
import pandas as pd
import numpy as np

from SearchCV import GridSearchCV_test

from sklearn.preprocessing import StandardScaler

from sklearn.decomposition import PCA, FastICA
from sklearn.manifold import TSNE

from sklearn.svm import SVC
from sklearn.linear_model import LogisticRegression
from sklearn.neighbors import KNeighborsClassifier
from sklearn.ensemble import RandomForestClassifier
from sklearn.naive_bayes import GaussianNB

# %% [markdown]
# 데이터 불러오기

# %%
df = pd.read_csv("CancerDataRm_normalized.csv")
df.drop("id", axis=1, inplace=True)
df.loc[df['diagnosis']=="M", "diagnosis"] = 1
df.loc[df['diagnosis']=="B", "diagnosis"] = 0
df

# %% [markdown]
# 데이터 분해

# %%
dataset = df.to_numpy()
X = dataset[:, 1:]
y = dataset[:, 0].astype('int')

# %%
print("Data class distribution ", np.unique(y, return_counts=True)[1], '\n')

# %% [markdown]
# 서포트 벡터 머신 분류

# %%
print("Classification by SVM\n")
params = {
        'C' : np.logspace(-3, 3, 7),
        'gamma' : np.logspace(-3, 3, 7),
        'kernel' : ["linear", "rbf", "poly"]
}
CV = GridSearchCV_test(SVC(random_state=42), dataset, StandardScaler(), 5, params)
CV.fit()
CV.predict()

# %% [markdown]
# 로지스틱 회귀 분류

# %%
print("Classification by Logistic Regression\n")
params = {
        'C' : np.logspace(-3, 3, 7),
        'solver' : ['lbfgs', 'sag', 'saga', 'liblinear', 'newton-cg']
}
CV = GridSearchCV_test(LogisticRegression(random_state=42), dataset, StandardScaler(), 5, params)
CV.fit()
CV.predict()

# %% [markdown]
# K-이웃 알고리즘 분류

# %%
print("Classification by K-Nearst Neighbors\n")
params = {
        'n_neighbors' : [3, 5, 7, 9, 11]
}
CV = GridSearchCV_test(KNeighborsClassifier(), dataset, StandardScaler(), 5, params)
CV.fit()
CV.predict()

# %%
print("Classification by Random Forest\n")
params = {
        'criterion' : ['gini', 'entropy', 'log_loss'],
        'max_features' : ['sqrt', 'log2', None],
}
CV = GridSearchCV_test(RandomForestClassifier(random_state=42), dataset, StandardScaler(), 5, params)
CV.fit()
CV.predict()

# %% [markdown]
# 가우시안 나이브 베이즈 분류

# %%
print("Classification by Gaussian Naive Bayes\n")
params = {
    "priors" : [None],
    "var_smoothing" : [1e-9, 1e-10, 1e-8]
}
CV = GridSearchCV_test(GaussianNB(), dataset, StandardScaler(), 5, params)
CV.fit()
CV.predict()
