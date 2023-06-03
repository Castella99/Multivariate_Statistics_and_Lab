import numpy as np
from itertools import product
import copy

from sklearn.metrics import accuracy_score, f1_score, precision_score, recall_score, roc_auc_score, confusion_matrix

from sklearn.model_selection import StratifiedKFold, train_test_split

class GridSearchCV_test :
    def __init__(self, model, dataset, scaler, fold, params, compressor=None, verbose=0) :
        self.model = model
        self.scaler = scaler
        self.fold = fold
        self.params = params
        self.compressor = compressor
        self.X = dataset[:, 1:]
        self.y = dataset[:, 0].astype('int')
        self.best_model = None
        self.best_scaler = None
        self.best_compressor = None
        self.verbose = verbose
    
    def generate_combinations(self, dct):
        keys = list(dct.keys())
        values = list(dct.values())

        # 리스트와 딕셔너리의 조합 생성
        combinations = []
        for selection in product(*values) :
            combination = dict(zip(keys, selection))
            combinations.append(combination)

        return combinations
    
    def list_mean(self, lst) :
        total = sum(lst)  # 리스트의 모든 요소의 합을 계산
        count = len(lst)  # 리스트의 요소의 개수를 계산
        mean = total / count  # 합을 개수로 나누어 평균 계산
        return mean
    
    def fit(self) :
        skf = StratifiedKFold(n_splits=self.fold, shuffle=True, random_state=42)
        x_train_val, self.x_test, y_train_val, self.y_test = train_test_split(self.X, self.y, test_size=1/(self.fold+1), random_state=42, stratify=self.y)
        idx_list = list(skf.split(x_train_val, y_train_val))
        
        params = self.generate_combinations(self.params)
        
        best_accuracy = 0
        best_f1_score = 0
        best_auroc = 0

        for param in params :
            performance = {'accuracy' : [],
                        'precision' : [],
                        'recall' : [],
                        'f1_score' : [],
                        'auroc' : []}
            
            for j in range(self.fold) :
                model = copy.deepcopy(self.model)
                scaler = copy.deepcopy(self.scaler)
                compressor = copy.deepcopy(self.compressor)
                model.set_params(**param)
                
                x_val = x_train_val[idx_list[j][1]]
                y_val = y_train_val[idx_list[j][1]]
                x_train = x_train_val[idx_list[j][0]]
                y_train = y_train_val[idx_list[j][0]]
                
                x_train = scaler.fit_transform(x_train)
                x_val = scaler.transform(x_val)
                
                if self.compressor != None :
                    x_train = compressor.fit_transform(x_train)
                    x_val = compressor.transform(x_val)
                
                model.fit(x_train, y_train)
                
                y_pred = model.predict(x_val)
                
                if self.verbose == 2 :
                    print("accuracy :", accuracy_score(y_val, y_pred))
                    print("precision :", precision_score(y_val, y_pred, zero_division=0))
                    print("recall :", recall_score(y_val, y_pred, zero_division=0))
                    print("f1_score :", f1_score(y_val, y_pred, zero_division=0))
                    print("auroc :", roc_auc_score(y_val, y_pred))
                    print(confusion_matrix(y_val, y_pred))
                
                performance['accuracy'].append(accuracy_score(y_val, y_pred))
                performance['precision'].append(precision_score(y_val, y_pred, zero_division=0))
                performance['recall'].append(recall_score(y_val, y_pred))
                performance['f1_score'].append(f1_score(y_val, y_pred))
                performance['auroc'].append(roc_auc_score(y_val, y_pred))
            
            if self.verbose >= 1 :
                print(f'\n{self.fold}-Fold Result')
                print('params :', param)
                print("accuracy :", self.list_mean(performance['accuracy']))
                print("precision :", self.list_mean(performance['precision']))
                print("recall :", self.list_mean(performance['recall']))
                print("f1_score :", self.list_mean(performance['f1_score']))
                print("auroc :", self.list_mean(performance['auroc']))
                print()
            
            if self.list_mean(performance['accuracy']) > best_accuracy :
                best_accuracy = self.list_mean(performance['accuracy'])
                best_f1_score = self.list_mean(performance['f1_score'])
                best_auroc= self.list_mean(performance['auroc'])
                self.best_model = model
                self.best_scaler = scaler
                self.best_compressor = compressor
                
            elif self.list_mean(performance['accuracy'] == best_accuracy) and \
                self.list_mean(performance['f1_score'] > best_f1_score) :
                best_f1_score = self.list_mean(performance['f1_score'])
                best_auroc= self.list_mean(performance['auroc'])
                self.best_model = model
                self.best_scaler = scaler
                self.best_compressor = compressor
                
            elif self.list_mean(performance['accuracy'] == best_accuracy) and \
                self.list_mean(performance['f1_score'] == best_f1_score) and \
                self.list_mean(performance['auroc'] > best_auroc):
                best_auroc = self.list_mean(performance['auroc'])
                self.best_model = model
                self.best_scaler = scaler
                self.best_compressor = compressor
            
        print('best estimator :', {key : self.best_model.get_params()[key] for key in param.keys()})
        print('best accuracy :', best_accuracy)
        print('best f1 score :', best_f1_score)
        print('best auroc :', best_auroc)
        print()
    
    def predict(self) :
        print('\nTest best estimator')
        x_test = self.best_scaler.transform(self.x_test)
        
        if self.compressor != None :
            x_test = self.best_compressor.transform(x_test)
            
        y_pred = self.best_model.predict(x_test)
        print("accuracy :", accuracy_score(self.y_test, y_pred))
        print("precision :", precision_score(self.y_test, y_pred, zero_division=0))
        print("recall :", recall_score(self.y_test, y_pred, zero_division=0))
        print("f1_score :", f1_score(self.y_test, y_pred, zero_division=0))
        print("auroc :", roc_auc_score(self.y_test, y_pred))
        print(confusion_matrix(self.y_test, y_pred))