import pandas as pd 
import numpy as np
from difflib import SequenceMatcher

"""" the code it's self should be running fine  but be sure to cehck col names."""
df=pd.read_csv(r'C:\Users\buzga\Desktop\School\Reaserch\Vicky\Code\R\Example Code\inspect.csv')
test=df[df["id_econ1"]!=df["id_econ2"]]


def check_lasts(df,idx):
    string_1=df.iloc[idx]["apellido_1"]
    string_2=df.iloc[idx]["apellido"]
    if(type(string_1)==str and type(string_2)==str):
        string_1=string_1.strip().split()
        string_2=string_2.strip().split()
        for i in range(min(len(string_1),len(string_2))):
            if(string_1[i]!=string_2[i]):
                return False
        return True
    else:
        return False


def check_first(df,idx):
    string_1=df.iloc[idx]["nombre_1"]
    string_2=df.iloc[idx]["nombre"]
    if(type(string_1)==str and type(string_2)==str):
        string_1=string_1.strip().split()
        string_2=string_2.strip().split()
        for i in range(min(len(string_1),len(string_2))):
            if(string_1[i]!=string_2[i]):
                return False
        return True
    else:
        return False
def fuzzy_search(search_key, text, strictness):
    lines = text.split("\n")
    for i, line in enumerate(lines):
        
        words = line.split()
        for word in words:
            similarity = SequenceMatcher(None, word, search_key)
            if similarity.ratio() > strictness and similarity.ratio()!=1 :
                return True
    return False
def fuzzy_searches(keys, text, strictness):
    for key in keys:
        if(fuzzy_search(key, text, strictness)):
            return True
    return False
def automate_matches(test):
    test["Python Matches"]=np.zeros(len(test["nombreapellido"]))
    for i in range(len(test["nombreapellido"])):
        if (check_first(test,i) and check_lasts(test,i)):
            test["Python Matches"][i]=1
        else: 
            text=test["nombreapellido_1"].iloc[i]
            keys=test["nombreapellido"].iloc[i].strip().split()
            if(fuzzy_searches(keys,text,.9)):
                test["Python Matches"][i]=1
    return test

## our current goal is to make sure that cases with the matches have the same id econ values. 
def match_ids_econ(matches, data):
    """""
    matches ids of comfirmed ties for the econ dataset 
    args:
        1. mathces- a data set with the validated matches
        2.  data- the orignal data
    """
    d={}
    for i in range(len(matches)):
        if(matches["Autmated Matches"][i] and matches["nombreapellido_1"][i] not in d.keys()):
            d[matches["nombreapellido_2"][i]]=matches["id_econ1"][i]
    for i in range(len(data)):
        if(data["nombreapellido"][i] in d.keys()):
            data["id_eco"][i]=d[data["nombreapellido"][i]]
