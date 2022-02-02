

from math import sqrt
from numpy import hstack
from numpy import vstack
from numpy import asarray
from sklearn.datasets import make_regression
from sklearn.model_selection import KFold
from sklearn.model_selection import train_test_split
from sklearn.metrics import mean_squared_error
from sklearn.linear_model import LinearRegression
from sklearn.linear_model import ElasticNet
from sklearn.neighbors import KNeighborsRegressor
from sklearn.tree import DecisionTreeRegressor
from sklearn.svm import SVR
from sklearn.ensemble import AdaBoostRegressor
from sklearn.ensemble import BaggingRegressor
from sklearn.ensemble import RandomForestRegressor
from sklearn.ensemble import ExtraTreesRegressor
from sklearn.experimental import enable_hist_gradient_boosting
from sklearn.ensemble import HistGradientBoostingRegressor
from sklearn.ensemble import VotingRegressor
import xgboost as xgb
import holidays    
 


def get_models():
	models = list()
	models.append(LinearRegression())
	models.append(ElasticNet())
	models.append(SVR(gamma='scale'))
	models.append(DecisionTreeRegressor())
	models.append(KNeighborsRegressor())
	models.append(AdaBoostRegressor())
	models.append(BaggingRegressor(n_estimators=20))
	models.append(HistGradientBoostingRegressor())
	models.append(RandomForestRegressor(n_estimators=20))
	models.append(ExtraTreesRegressor(n_estimators=20))
	return models


 
# collect out of fold predictions form k-fold cross validation
def get_out_of_fold_predictions(X, y, models):
	meta_X, meta_y = list(), list()
	# define split of data
	kfold = KFold(n_splits=10, shuffle=True)
	# enumerate splits
	for train_ix, test_ix in kfold.split(X):
		fold_yhats = list()
		# get data
		train_X, test_X = X.iloc[train_ix,:], X.iloc[test_ix,:]
		train_y, test_y = y[train_ix], y[test_ix]
		meta_y.extend(test_y)
		# fit and make predictions with each sub-model
		for model in models:
			model.fit(train_X, train_y)
			yhat = model.predict(test_X)
			# store columns
			fold_yhats.append(yhat.reshape(len(yhat),1))
		# store fold yhats as columns
		meta_X.append(hstack(fold_yhats))
	return vstack(meta_X), asarray(meta_y)
 
# fit all base models on the training dataset
def fit_base_models(X, y, models):
	for model in models:
		model.fit(X, y)
 
# fit a meta model
def fit_meta_model(X, y):
	model = LinearRegression()
	model.fit(X, y)
	return model
 
# evaluate a list of models on a dataset
def evaluate_models(X, y, models):
	for model in models:
		yhat = model.predict(X)
		mse = mean_squared_error(y, yhat)
		print('%s: RMSE %.3f' % (model.__class__.__name__, sqrt(mse)))
 
# make predictions with stacked model
def super_learner_predictions(X, models, meta_model):
	meta_X = list()
	for model in models:
		yhat = model.predict(X)
		meta_X.append(yhat.reshape(len(yhat),1))
	meta_X = hstack(meta_X)
	# predict
	return meta_model.predict(meta_X)

mmpow=glc[glc["loadtype"]==groups[7]].groupby(['date'],as_index=False).sum('power_abs_')['power_abs_']

mmpow = pd.DataFrame(mmpow)

mmpow['date']= glc['date'].unique()


mmpow['date'] = pd.to_datetime(mmpow['date'], format='%Y-%m-%d %H:%M:%S.%f') 


mmpow.set_index('date', inplace=True)        

mmpow2 = mmpow.resample('H').mean()

rdates = pd.Series(glc[glc['loadtype']==7][glc[glc['loadtype']==7]['power_abs_'].isnull()]['date'])

glcgr7red = glcgr7[~glcgr7['date'].isin(rdates)]


glc[glc['loadtype']==1]['power_abs_'].isnull()

glc[glc[glc['loadtype']==1]['power_abs_'].isnull()]['date']


    
glc1h = pd.DataFrame()
grs = pd.DataFrame()
grsges = pd.DataFrame()
d = pd.DataFrame()

 for i in range(0,len(groups)):    

        inp = glc[glc["loadtype"]==groups[i]]
        
        inp['date'] = pd.to_datetime(inp['date'], format='%Y-%m-%d %H:%M:%S.%f') 

        rdates = pd.Series(inp[inp['power_abs_'].isnull()]['date'])

        inpred = inp[~inp['date'].isin(rdates)]
        
        glc2[glc2["loadtype"]==groups[i]] = inpred
    
        
glc3 = glc2[~glc2['power_abs_'].isnull()]

glc3.reset_index(inplace=True,drop=True)

#### save demand input file
glc3.to_feather('path')

#define input dataset for full time series prediction
hoursf = dts.date.dt.hour
daysf = dts.date.dt.dayofweek
monthsf = dts.date.dt.month 
hoursf = hoursf.astype(hours_type)
daysf = daysf.astype(days_type)
monthsf = monthsf.astype(months_type)  
        
hoursf = pd.get_dummies(hoursf)
daysf = pd.get_dummies(daysf)
monthsf = pd.get_dummies(monthsf)
        
inpf = pd.concat([dts.date,hoursf,daysf,monthsf], axis=1)
        
inpfn = pd.merge_asof(inpf,dts[['date','cloud','temp','precipitation','radiation2','skintemp','thermalradiation','u10m','v10m']], on='date', direction='nearest')                                        
at_holidays = holidays.AT()
inpfn['hday'] = [1 if str(val).split()[0] in at_holidays else 0 for val in inpfn['date']]

glc1h = pd.DataFrame()
grs = pd.DataFrame()
grsges = pd.DataFrame()
d = pd.DataFrame()

for j in range(0,10):
  
    for i in range(0,len(groups)):    
       
        inp = glc[glc["loadtype"]==groups[i]]#change to i
        
        inp['date'] = pd.to_datetime(inp['date'], format='%Y-%m-%d %H:%M:%S.%f') 

        rdates = pd.Series(inp[inp['power_abs_'].isnull()]['date'])

        inpred = inp[~inp['date'].isin(rdates)]
    
        inp = inpred
        
        inp = inp.groupby(['date']).sum()
          
        inp2 = inp.resample('H').mean()
    
        date = pd.to_datetime(inp2.index, format='%Y-%m-%d %H:%M:%S.%f')
    
        inp2.reset_index(inplace=True)
    
        hours = date.hour
        days = date.dayofweek
        months = date.month 
                    
        hh = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23]
        dd = [0, 1, 2, 3, 4, 5, 6]
        md = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]
              
        hd = list(hh)

        hours = pd.Series(list(hours))
        
        hours_type = CategoricalDtype(categories=hd, ordered=True)
        hours = hours.astype(hours_type)
    
        dd = list(dd)

        days = pd.Series(list(days))

        days_type = CategoricalDtype(categories=dd, ordered=True)
        days = days.astype(days_type)
    
        md = list(md)

        months = pd.Series(list(months))
        months_type = CategoricalDtype(categories=md, ordered=True)
        months = months.astype(months_type)
  
        hours = pd.get_dummies(hours)
        days = pd.get_dummies(days)
        months = pd.get_dummies(months)
        
        inp2 = pd.concat([inp2,hours,days,months], axis=1) 
        
        inp2 = inp2.drop(['loadtype'], axis = 1) 
    
        inp2 = inp2[inp2['date'] < '2021-01-01']

        inpn = pd.merge_asof(inp2,dts[['date','cloud','temp','precipitation','radiation2','skintemp','thermalradiation','u10m','v10m']], on='date', direction='nearest')
        inpn_train = inpn[inpn['power_abs_'].notnull()]
        
        at_holidays = holidays.AT()
        inpn_train['hday'] = [1 if str(val).split()[0] in at_holidays else 0 for val in inpn_train['date']]

        y = inpn_train[['date','power_abs_']]
        X = inpn_train.drop(['date','power_abs_','Site'], axis = 1) 

        
        Xx, X_valx, yx, y_valx = train_test_split(X, y, test_size=0.05)
        X, X_val, y, y_val = train_test_split(Xx, yx, test_size=0.3)
        
        yx.reset_index(inplace=True,drop=False)
          
        y.reset_index(inplace=True,drop=True)
        y = y.iloc[:,1]
        y_val = y_val.iloc[:,1]
  
        print('Train', X.shape, y.shape, 'Test', X_val.shape, y_val.shape)

        models = get_models()
       
        # get out of fold predictions
        meta_X, meta_y = get_out_of_fold_predictions(X, y, models)
        print('Meta ', meta_X.shape, meta_y.shape)
        # fit base models
        fit_base_models(X, y, models)
        # fit the meta model
        meta_model = fit_meta_model(meta_X, meta_y)
        # evaluate base models
        evaluate_models(X_val, y_val, models)
        # evaluate meta model
        yhat = super_learner_predictions(X_val, models, meta_model)
        yhatx = super_learner_predictions(X_valx, models, meta_model)
        
        mperf = pd.DataFrame(y_valx)
        mperf["pred"] = yhatx
        mperf.reset_index(inplace=True,drop=False)
        
        #save validation set prediction data
        mperf.to_feather('path'+ str(groups[i]) + 'run#' + str(j)+'mperf')
        
        print('Super Learner: RMSE %.3f' % (sqrt(mean_squared_error(y_val, yhat))))
   
        cor=np.corrcoef(y_val,yhat)
        nrmse=np.sqrt(mean_squared_error(y_val, yhat))/np.mean(y_val)
       
        #model quality dataframe
        grs = grs.append(pd.DataFrame({'group' : [groups[i]],'Cor':[cor[1][0]],'NRMSE':[nrmse],'Run#':[j]}))                                                
                                              
        Xf = inpfn.drop(['date'], axis = 1) 
        fpred = super_learner_predictions(Xf, models, meta_model)
        group = groups[i]
        group = pd.Series(group.repeat(len(dts)))
        run = pd.Series(j).repeat(len(dts))
        run.reset_index(inplace=True,drop=True)
        grsges = pd.DataFrame({'date' : dts.date, 'pred' : pd.Series(fpred), 'group' : group,'Run#' : run})
        grsges = pd.merge_asof(grsges,inpn[['date','power_abs_']], on='date', direction='nearest',tolerance=pd.Timedelta("1h"))
        
        #save preditions for every run and demand group 
        grsges.to_feather('path'+ str(groups[i]) + 'run#' + str(j)+'')


# calculate mean demand and save model quality data
grs.reset_index(inplace=True,drop=True)

meanpower=pd.DataFrame(mpow[grs.PiN[grs.PiN.isin(mpow.index)]])
meanpower.reset_index(inplace=True,drop=True)

grs['meanpower']=meanpower

grs.to_feather("glcrfruns13")
