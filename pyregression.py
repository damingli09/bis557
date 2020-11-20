
# Implements three python functions for BIS557 homework 4
import numpy as np 

def pyridge(X, y, l=1.0, iter=10000, eta=0.01):
    """
    Implements Ridge regression 

    Parameters
    ----------
    X: 
      data matrix exculding the 1's in design matrix, numpy array of shape (n,p)
    y:
      response vector, numpy array of length n
    l:
      regularization parameter, float
    iter:
      iteration number, integer
    eta:
      learning rate, float

    Returns
    -------
    betas:
      fitted coefficients, numpy array of length p
    intercept:
      fitted intercept, float
    """

    assert X.shape[0] == len(y), "Wrong data input, dimension not consistent."
    N = len(y)  # sample size

    betas = np.zeros(X.shape[1])
    intercept = 0

    for _ in range(iter):
      # temporary prediction
      ypred = intercept + X.dot(betas)
          
      # compute gradients       
      dbetas = 2*(-(X.T).dot(y - ypred) + l*betas)/N    
      dintercept = -2*np.sum(y - ypred)/N
        
      # updates    
      betas = betas - eta*dbetas     
      intercept = intercept - eta*dintercept

    return betas, intercept
    
def onlineLR(x_input, y_input, betas, intercept, eta=0.01):
    """
    Online linear regression learner (equivalent to stochastic gradient descent), assuming that during each call the function reads only one sample
    
    Parameters
    ----------
    x_input:
        New streaming input of X
    y_input:
        New streaming input of y
    eta:
        Learning rate, float
    betas:
        Current coefficients
    intercept:
        Current intercept
    
    Returns
    -------
    betas:
        Updated coefficients
    intercept:
        Updated intercept
    """
    
    assert len(betas) == len(x_input), "Wrong data input"
    
    ypred = intercept + x_input.dot(betas)
    
    # compute gradients
    dbetas = -2*(x_input*(y_input - ypred))
    dintercept = -2*(y_input - ypred)
      
    # updates
    betas = betas - eta*dbetas
    intercept = intercept - eta*dintercept
    
    return betas, intercept

def pylasso(X, y, l=1.0, iter=10000, eta=0.01):
    """
    Implements Lasso regression 

    Parameters
    ----------
    X: 
      data matrix exculding the 1's in design matrix, numpy array of shape (n,p)
    y:
      response vector, numpy array of length n
    l:
      regularization parameter, float
    iter:
      iteration number, integer
    eta:
      learning rate, float

    Returns
    -------
    betas:
      fitted coefficients, numpy array of length p
    intercept:
      fitted intercept, float
    """

    assert X.shape[0] == len(y), "Wrong data input, dimension not consistent."
    N = len(y)  # sample size
    p = X.shape[1]  # number of features

    betas = np.zeros(X.shape[1])
    intercept = 0

    for _ in range(iter):
      # temporary prediction
      ypred = intercept + X.dot(betas)
          
      # compute gradients       
      dbetas = np.zeros(p)  
      for i in range(p): 
        if betas[i] > 0: 
            dbetas[i] = (-2*(X[:,i]).dot(y - ypred) + l) /N
        else: 
            dbetas[i] = (-2*(X[:,i]).dot(y - ypred) - l) /N
  
      dintercept = -2*np.sum(y - ypred)/N
        
      # updates    
      betas = betas - eta*dbetas     
      intercept = intercept - eta*dintercept

    return betas, intercept
