# %%
import pandas as pd
import numpy as np

from SearchCV import GridSearchCV_test, nested_k_fold

from sklearn.preprocessing import StandardScaler

from sklearn.model_selection import StratifiedKFold

from sklearn.svm import SVC
from sklearn.linear_model import LogisticRegression
from sklearn.neighbors import KNeighborsClassifier
from sklearn.ensemble import RandomForestClassifier
from sklearn.naive_bayes import GaussianNB

# 데이터 분해

print("Using the dataset with outliers removed for classification\n")

df = pd.read_csv("CancerDataRm_normalized.csv")
df.drop("id", axis=1, inplace=True)
df.loc[df['diagnosis']=="M", "diagnosis"] = 1
df.loc[df['diagnosis']=="B", "diagnosis"] = 0
# %%
dataset = df.to_numpy()
X = dataset[:, 1:]
y = dataset[:, 0].astype('int')

# %%
print("Data class distribution ", np.unique(y, return_counts=True)[1], '\n')
scaler = StandardScaler()

# %% [markdown]
# 서포트 벡터 머신 분류

# %%
print("Classification by SVM\n")
params = {
        'C' : np.logspace(-3, 3, 7),
        'gamma' : np.logspace(-3, 3, 7),
        'kernel' : ["linear", "rbf", "poly"]
}

nested_k_fold(SVC(random_state=42), X, y, scaler, 10, params)

# %% [markdown]
# 로지스틱 회귀 분류

# %%
print("Classification by Logistic Regression\n")
params = {
        'C' : np.logspace(-3, 3, 7),
        'solver' : ['lbfgs', 'sag', 'saga', 'liblinear', 'newton-cg']
}
nested_k_fold(LogisticRegression(random_state=42, n_jobs=-1), X, y, scaler, 10, params)

# %% [markdown]
# K-이웃 알고리즘 분류

# %%
print("Classification by K-Nearst Neighbors\n")
params = {
        'n_neighbors' : [3, 5, 7, 9, 11]
}

nested_k_fold(KNeighborsClassifier(n_jobs=-1), X, y, scaler, 10, params)

# %%
print("Classification by Random Forest\n")
params = {
        'criterion' : ['gini', 'entropy', 'log_loss'],
        'max_features' : ['sqrt', 'log2', None],
}
nested_k_fold(RandomForestClassifier(random_state=42, n_jobs=-1), X, y, scaler, 10, params)

# %% [markdown]
# 가우시안 나이브 베이즈 분류

# %%
print("Classification by Gaussian Naive Bayes\n")
params = {
    "priors" : [None],
    "var_smoothing" : [1e-9, 1e-10, 1e-8]
}
nested_k_fold(GaussianNB(), X, y, scaler, 10, params)

print("Using the dataset with outliers included for classification\n")

df = pd.read_csv("CancerData_normalized_outlier.csv")
df.drop("id", axis=1, inplace=True)
df.loc[df['diagnosis']=="M", "diagnosis"] = 1
df.loc[df['diagnosis']=="B", "diagnosis"] = 0
# %%
dataset = df.to_numpy()
X = dataset[:, 1:]
y = dataset[:, 0].astype('int')

# %%
print("Data class distribution ", np.unique(y, return_counts=True)[1], '\n')
skf = StratifiedKFold(n_splits=10, shuffle=True, random_state=42)
idx_list = list(skf.split(X, y))
scaler = StandardScaler()

# %% [markdown]
# 서포트 벡터 머신 분류

# %%
print("Classification by SVM\n")
params = {
        'C' : np.logspace(-3, 3, 7),
        'gamma' : np.logspace(-3, 3, 7),
        'kernel' : ["linear", "rbf", "poly"]
}

nested_k_fold(SVC(random_state=42), X, y, scaler, 10, params)

# %% [markdown]
# 로지스틱 회귀 분류

# %%
print("Classification by Logistic Regression\n")
params = {
        'C' : np.logspace(-3, 3, 7),
        'solver' : ['lbfgs', 'sag', 'saga', 'liblinear', 'newton-cg']
}
nested_k_fold(LogisticRegression(random_state=42, n_jobs=-1), X, y, scaler, 10, params)

# %% [markdown]
# K-이웃 알고리즘 분류

# %%
print("Classification by K-Nearst Neighbors\n")
params = {
        'n_neighbors' : [3, 5, 7, 9, 11]
}

nested_k_fold(KNeighborsClassifier(n_jobs=-1), X, y, scaler, 10, params)

# %%
print("Classification by Random Forest\n")
params = {
        'criterion' : ['gini', 'entropy', 'log_loss'],
        'max_features' : ['sqrt', 'log2', None],
}
nested_k_fold(RandomForestClassifier(random_state=42, n_jobs=-1), X, y, scaler, 10, params)

# %% [markdown]
# 가우시안 나이브 베이즈 분류

# %%
print("Classification by Gaussian Naive Bayes\n")
params = {
    "priors" : [None],
    "var_smoothing" : [1e-9, 1e-10, 1e-8]
}
nested_k_fold(GaussianNB(), X, y, scaler, 10, params)