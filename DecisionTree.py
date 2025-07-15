import pandas as pd
from sklearn.feature_extraction import DictVectorizer
from sklearn.tree import DecisionTreeClassifier
from sklearn.model_selection import train_test_split
from sklearn.model_selection import GridSearchCV
from sklearn.ensemble import RandomForestClassifier
import time
import numpy as np
import os

def AC(path, csv_list):
    # Trianing_table = pd.read_csv("training_variables/D_BU_A_Training.csv")
    # Trianing_table = pd.read_csv("training_variables/D_BU_C_Training_new.csv")
    # Trianing_table = pd.read_csv("table_to_try/Net_Value.csv")
    # Trianing_table = pd.read_csv("table_to_try/SOP_num_Items.csv")
    if len(csv_list)==1:
        Trianing_table = pd.read_csv(path+csv_list[0])
    elif len(csv_list)==2:
        table1 = pd.read_csv(path+csv_list[0])
        table1.drop(table1.columns[-1], axis=1, inplace=True)
        table2 = pd.read_csv(path+csv_list[1])
        Trianing_table = pd.concat([table1, table2], axis=1)
    else:
        table1 = pd.read_csv(path+csv_list[0])
        table1.drop(table1.columns[-1], axis=1, inplace=True)
        table2 = pd.read_csv(path+csv_list[1])
        table2.drop(table2.columns[-1], axis=1, inplace=True)
        table3 = pd.read_csv(path+csv_list[2])
        table = pd.concat([table1, table2], axis=1)
        Trianing_table = pd.concat([table, table3], axis=1)

    


    test_all = Trianing_table[Trianing_table["Reseller"].isna()]
    Trianing_table = Trianing_table[~Trianing_table["Reseller"].isna()]

    y = Trianing_table['Reseller']
    labels = list(Trianing_table.columns.values)
    labels.remove("Reseller")
    x = Trianing_table[labels]
    x_dict_list = x.to_dict(orient='records')
    dict_vec = DictVectorizer(sparse=False)
    x = dict_vec.fit_transform(x_dict_list)

    # for feat in feat_indices:
    # y = dta[:, 329, 0]
    # x = dta[:, 0:329, feat].reshape(len(dta), -1)

    # 划分训练集和测试集
    x_train, x_test, y_train, y_test = train_test_split(x, y, test_size=0.1)

    def x_train_oversampling(x_train, y_train, ratio):
        x_reseller = x_train[y_train.values==1, :]
        x_nonreseller = x_train[y_train.values==0, :]
        y_reseller = y_train[y_train.values==1]
        y_nonreseller = y_train[y_train.values==0]
        k = round((ratio/(1-ratio))*len(x_nonreseller)/len(x_reseller))
        new_x_train = np.repeat(x_reseller, k, axis=0)
        new_x_train = np.concatenate([new_x_train, x_nonreseller])
        new_y_train = np.concatenate([np.repeat(y_reseller, k), y_nonreseller])
        return new_x_train, new_y_train

    x_train, y_train = x_train_oversampling(x_train, y_train, ratio=0.2)
    # 决策树分类器
    dec_tree = DecisionTreeClassifier()
    dec_tree.fit(x_train, y_train)

    print("*" * 30 + " 准确率 " + "*" * 30)
    print(dec_tree.score(x_test, y_test))

    def CaculateAcc(reselt, label):
        TP = 0
        TN = 0
        FP = 0
        FN = 0
        for i in range(len(reselt)):
            if(label[i]==1):
                if(reselt[i]==1):
                    TP = TP+1
                else: FN =FN+1
            else:
                if(reselt[i]==1):
                    FP = FP+1
                else: TN =TN+1
        TPR = TP/(TP+FN)
        TNR = TN/(FP+TN)
        BAC = (TPR+TNR)/2
        return BAC

    result = dec_tree.predict(x_test)
    label = np.array(y_test)
    acc = CaculateAcc(result, label)
    print("*" * 30 + " BAC " + "*" * 30)
    print(acc)

    # from sklearn import tree
    # from matplotlib import pyplot as plt
    # tree.plot_tree(dec_tree)
    # plt.show()
    print("finish")
    return acc

def get_log_path_dict(log_path):
    for files in os.walk(log_path):
        return files[2]
        # log_path_dict = dict()
        # for dir_name in dirs:
        #     dir_path = os.path.join(root, dir_name)
        #     log_path = dir_path + "/public.log"
        #     log_path_dict[dir_name] = log_path
        # return log_path_dict

# log_path = "E:/TUM/Lecture/WS2022/Lecture/ML/Programming/CUP/Program/test/All_features/CSVS/new_with_line/"
log_path = "E:/TUM/Lecture/WS2022/Lecture/ML/Programming/CUP/Program/test/All_features/CSVS/new_no_line/"
list_file = get_log_path_dict(log_path)
print(list_file)
list_Name = []
list_BAC = []

# for i in range(len(list_file)):
#     Li_files = []
#     Li_files.append(list_file[i])
#     print(Li_files)
#     acc = AC(log_path,Li_files)
#     list_Name.append(Li_files)
#     list_BAC.append(acc)


# for i in range(len(list_file)):
#     Li_files = []
#     Li_files.append(list_file[i])
#     for j in range(len(list_file)-1-i):
#         Li_files.append(list_file[j+i+1])
#         print(Li_files)
#         acc = AC(log_path,Li_files)
#         list_Name.append(Li_files)
#         list_BAC.append(acc)
#         Li_files.pop()

# for i in range(len(list_file)):
#     Li_files = []
#     Li_files.append(list_file[i])
#     for j in range(len(list_file)-1-i):
#         Li_files.append(list_file[j+i+1])
#         for k in range(len(list_file)-2-i-j):
#             Li_files.append(list_file[j+i+k+2])
#             print(Li_files)
#             acc = AC(log_path,Li_files)
#             list_Name.append(Li_files)
#             list_BAC.append(acc)
#             Li_files.pop()
#         Li_files.pop()


# for i in range(len(list_file)):
#     Li_files = []
#     Li_files.append(list_file[i])
#     for j in range(len(list_file)-1-i):
#         Li_files.append(list_file[j+i+1])
#         for k in range(len(list_file)-2-i-j):
#             Li_files.append(list_file[j+i+k+2])
#             for l in range(len(list_file)-3-i-j-k):
#                 Li_files.append(list_file[j+i+k+3])
#                 print(Li_files)
#                 acc = AC(log_path,Li_files)
#                 list_Name.append(Li_files)
#                 list_BAC.append(acc)
#                 Li_files.pop()
#             Li_files.pop()
#         Li_files.pop()

for i in range(len(list_file)):
    Li_files = []
    Li_files.append(list_file[i])
    for j in range(len(list_file)-1-i):
        Li_files.append(list_file[j+i+1])
        for k in range(len(list_file)-2-i-j):
            Li_files.append(list_file[j+i+k+2])
            for l in range(len(list_file)-3-i-j-k):
                Li_files.append(list_file[j+i+k+3])
                for m in range(len(list_file)-4-i-j-k-l):
                    Li_files.append(list_file[j+i+k+l+4])
                    print(Li_files)
                    acc = AC(log_path,Li_files)
                    list_Name.append(Li_files)
                    list_BAC.append(acc)
                    Li_files.pop()
                Li_files.pop()
            Li_files.pop()
        Li_files.pop()
