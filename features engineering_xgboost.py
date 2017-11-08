# These lines provide a dataframe containing all the paths of the boosted trees. These ones will be used in the next step as predictors in a LASSO Model.

import numpy as np
import pandas as pd
import xgboost as xgb
from sklearn.model_selection import train_test_split, StratifiedKFold, ParameterGrid 
from sklearn.metrics import accuracy_score, roc_auc_score



data=pd.read_csv('/home/user/data.csv', sep=";")
data=data.fillna(-99)
data=pd.get_dummies(data)
X, y =data.drop("Y",axis=1), data.Y

X_train, X_test, y_train, y_test = train_test_split(X, y, random_state=0, test_size=0.50, stratify=y, shuffle=True)
dtrain, dtest = xgb.DMatrix(X_train, y_train), xgb.DMatrix(X_test, y_test)

param = {"objective": "binary:logistic"
 , "learning_rate": 0.35
 , "max_depth": 2
 , "eval_metric": "logloss"
 }
bst = xgb.train(param, dtrain, 50)

bst.dump_model("/home/user/dump_xgb.txt")
data_leaf=pd.DataFrame(bst.predict(dtest, pred_leaf=True))
data_leaf.to_csv('/home/user/data_leaf.csv')


def conta_spazio(stringa):
	count=0
	#print(stringa)
	while stringa[count]=="\t":
		count+=1 
	return count

def riga_nulla(stringa):
	nulla=True
	for i in range(len(stringa)):
		if stringa[i]!="\t":
			nulla=False
	return nulla


def salva_path(tree, leaf, descrizione, traccia):
	path_tree=""
	for i in range(len(traccia)):
		path_tree=path_tree+"{"+descrizione[i]+":"+str(traccia[i])+"}"
	return pd.DataFrame([{"tree":tree, "leaf":int(leaf), "path_tree":path_tree}])
		


#file.close()
file = open("/home/ciro/Immagini/dump_xgb.txt", "r")

res=pd.DataFrame()
a="mario"
tree=0
riga=file.readline() 
while True:
	traccia=[]
	riga=file.readline()
	descrizione=[riga[(riga.find("[")+1):riga.find("]")] ]
	riga=file.readline()
	while True:
		print(riga)
		contaSp=conta_spazio(riga)
		if((contaSp==0) | riga_nulla(riga)):
			break
		if contaSp>len(traccia):
			traccia.append(True) 
		else:
			traccia=traccia[:contaSp]
			descrizione=descrizione[:contaSp]
			traccia[-1]=False 		
		if riga.find(":leaf=")!=-1: 
			leaf=riga[contaSp:riga.find(":")]
			res=pd.concat([res, salva_path(tree, leaf, descrizione, traccia)], axis=0)
		else:
			descrizione.append(riga[(riga.find("[")+1):riga.find("]")])	
		riga=file.readline() 
	tree+=1
	if(riga_nulla(riga)):
		break

file.close()

res
res.to_csv('/home/user/features_engineering.csv')



data_leaf2=data_leaf.copy()

for i in range(res.shape[0]):
	data_leaf2.loc[data_leaf2.iloc[:,res.iloc[i,2]]==res.iloc[i,0], res.iloc[i,2]]= res.iloc[i,1]

data_leaf2
data_leaf2=pd.get_dummies(data_leaf2)

data_leaf2.to_csv('/home/user/data_leaf2.csv')
y_test.to_csv('/home/user/y_leaf2.csv')


##################################################################################################################
# From this moment on we use a LASSO model for a further and more informative feature selection
# R scripts

require(h2o)
h2o.init(nthreads = -1)
data.hex = h2o.importFile(path = "/home/user/data_leaf2.csv", destination_frame = "data.hex")
y.hex=h2o.importFile(path = "/home/user/y_leaf2.csv", destination_frame = "y.hex")

#lo schifo di python inserisce numeri di riga
data.hex=h2o.cbind(y.hex[,2], data.hex[,-c(1, NCOL(data.hex))])

m<- h2o.glm(y = 1, training_frame = data.hex, nfolds=5,  alpha=1, family="binomial", standardize=FALSE, lambda_search=TRUE)  

summary(m)

h2o.coef(m)

out=data.frame( coef=h2o.coef(m)[-1][h2o.coef(m)[-1]!=0], path=colnames(data.hex)[-1][h2o.coef(m)[-1]!=0])
idx=sort.int(out[,1], decreasing=T, index.return=TRUE)$ix
out=out[idx,]

write.csv(out, file="/home/user/output.csv")
