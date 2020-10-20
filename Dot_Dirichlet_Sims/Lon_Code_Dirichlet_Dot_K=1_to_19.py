


##Import packages and files
from itertools import compress, product
import numpy as np
import matplotlib.pyplot as plt
import array
import random
import pandas as pd
import itertools
import operator
import importlib
#import functions defining the layered landscape
from LayeredLandscapeFunctions import *
from Landscapes import *
#import networkx as nx
import datetime


def create_dotSpace(N=10,hypers=[1,1,1,1,1,1,1,1,1,1],masked=False, mask=None):
    our_dirichlet = stats.dirichlet(hypers)    
    all_permutations = list(itertools.product([0,1],repeat = N))
    fitnesses = []
    perms = []
    transformed_locs = []
    for perm in all_permutations:
        fitnesses.append(np.dot(perm,our_dirichlet.rvs()[0]))
        if masked:
            if mask:
                perm = transform(perm,mask)
            else:
                perm = transform(perm)
        perms.append("".join([str(i) for i in perm]))
        transformed_locs.append(perm)
    frame = {"Fitness":pd.Series(fitnesses,index=perms), "Location":pd.Series(transformed_locs,index=perms)}
    return pd.DataFrame(frame)


def Make_Lon(DF_Landscape, M, D, Num_Perturbs):

    ##First get a list of all the Maxima locations
    DFMaximas = DF_Landscape[DF_Landscape.Maxima==1].index.values
    df = pd.DataFrame(data=None, index=DF_Landscape[DF_Landscape.Maxima==1].index.values, columns=DF_Landscape[DF_Landscape.Maxima==1].index.values,
                 dtype=None, copy=False)
    df = df.fillna(0) #replace nas with zeros

    #loop through every maxima
    for Maxima in DFMaximas:
        print('new_maxima')
        s_star=Maxima #set currrent s_star
        i=0
        while(i<Num_Perturbs):
            s_prime = Perturbation(s_star, D) #perturb
            s_prime_star = Hill_Climb_First(s_prime,DF_Landscape, M)  #climb new hill, here find first maxima
            #update the transition matrix
            df.at[s_star, s_prime_star] =  df.at[s_star, s_prime_star]+1
            i = i + 1
    return(df)




def run_dirichlet_experiment():
    for K in range(1,11):
        print("starting ", K, "at", datetime.datetime.now())

        N = 10        
        ##First define a landscape, then notice all the maximas (it prinds the number of maxima)

        for I in range(10):
            #pool

            concentration_params = [K,K,K,K,K,K,K,K,K,K]
            
            assert len(concentration_params) == N, "The dirichlet params are not the same count as N"
            
            dirichland = create_dotSpace(N=10,hypers=concentration_params,masked=False, mask=None)
            
            DFWithMaximas = Local_Maxima_Locations(dirichland)
            
            DFWithMaximas.to_csv("N-{}_K={}_I={}_mapping_Dirich_Dot.csv".format(N,K,I))

            LONMatrix = Make_Lon(DFWithMaximas,M = 1,D = 2,Num_Perturbs = 10)

            with open("N-{}_K={}_I={}_dirich.csv".format(N,K,I), "w") as f:
                f.write(LONMatrix.to_csv())
                print('writing')
        print("ended at", datetime.datetime.now())


import multiprocessing as mp

def run_dirichlet_experiment_pool(K):
    print("starting ", K, "at", datetime.datetime.now())

    N = 10        
    ##First define a landscape, then notice all the maximas (it prinds the number of maxima)

    for I in range(10):
        #pool

        concentration_params = [K,K,K,K,K,K,K,K,K,K]
        
        assert len(concentration_params) == N, "The dirichlet params are not the same count as N"
        
        dirichland = create_dotSpace(N=10,hypers=concentration_params,masked=False, mask=None)
        
        DFWithMaximas = Local_Maxima_Locations(dirichland)
        
        DFWithMaximas.to_csv("N-{}_K={}_I={}_mapping_Dirich_Dot.csv".format(N,K,I))

        LONMatrix = Make_Lon(DFWithMaximas,M = 1,D = 2,Num_Perturbs = 75)

        with open("N-{}_K={}_I={}_dirich.csv".format(N,K,I), "w") as f:
            f.write(LONMatrix.to_csv())
            print('writing')
    print("ended at", datetime.datetime.now())

       
##parallelize


pool = mp.Pool(mp.cpu_count())

pool.map(run_dirichlet_experiment_pool, [1,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9])



#run_dirichlet_experiment()