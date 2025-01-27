from tabpfn import TabPFNRegressor
import sklearn

def fit_reg(X, y):
    
    reg = TabPFNRegressor(device = 'auto')
    
    reg.fit(X, y)

    return {
                "fit": rega
           }
           

def fit_cls(X, y):
    
    cls = TabPFNClassifier(device = 'auto')
    
    cls.fit(X, y)

    return {
                "fit": cls
           }           

def pred_reg(model, data):
    
    pred = model.predict(data)

    return {
                "pred": pred
           }
