# -*- coding: utf-8 -*-

# pip install biopandas

# from biopandas.pdb import PandasPdb
# ppdb = PandasPdb().fetch_pdb('5keh')
# my_prtn = ppdb.read_pdb('5keh.pdb')

# my_prtn

# print(ppdb.code)
# print(ppdb.header)

# import pandas as pd
# #pd.set_option('display.max_rows', None)
# #pd.set_option('display.max_columns', None)
# #ppdb.df['ATOM'].head(10)
# ppdb.df

import regex as re

file = open('prtn.txt', 'w')
with open('5keh.cif') as f:
  for line in f:
    if re.search('^ATOM', line):
      file.write(line)
file.close()

import pandas as pd

omk_keh = pd.read_csv('prtn.txt', sep='\s+', header=None)

omk_keh

#extracting main chain atoms
main_NCaC = omk_keh[omk_keh[3].isin(['N','C','CA'])]

#extracting only one coordinate set
main_A = main_NCaC[main_NCaC[4].isin(['A','.'])]

#removing extra columns
mainCol_NCaC = main_A.drop([0,2,6,7,8,9,13,14,15,16,17,18,19,20], axis=1, index=None)
#main_NCaC = ppdb.df['ATOM'][ppdb.df['ATOM']['atom_name'].isin(['C','N','CA'])]

#extracting only one coordinate set
#main_A = main_NCaC[main_NCaC['alt_loc'].isin(['A',''])]
#main_A

#remove alt_loc column
#main_A = main_A.drop(labels='alt_loc',axis=1)
mainCol_NCaC

#extracting important columns
#mainCol_NCaC = main_A[['atom_number','atom_name','residue_name','residue_number','x_coord','y_coord','z_coord']]
#mainCol_NCaC

"""__Dihedral angle of 4 atoms__"""

import numpy as np
import math, sympy
from sympy import atan2

def dihedral_angle(X1,X2,X3,X4):
    #3 vectors from 4 points
    V1 = X1-X2
    V2 = X2-X3
    V3 = X3-X4
    
    #normal vectors from cross product of 3 vectors above
    V4 = np.cross(V1,V2)
    V5 = np.cross(V2,V3)
    
    #vectors for orthogonal plane
    nr_vct_cross = np.cross(V4,V2)
    nr_vct_dot = np.dot(V4,V5)
    
    x = nr_vct_dot
    y = np.dot(nr_vct_cross,V5)/np.linalg.norm(V2)
    
    #final dihedral angle calculation
    deg = sympy.atan2(y,x)
    di_an = math.degrees(deg)
    
    return di_an    

# Reference:
# https://mebinfotalk.blogspot.com/2013/06/how-to-calculate-dihedral-angle-using.html
# https://math.stackexchange.com/questions/47059/how-do-i-calculate-a-dihedral-angle-given-cartesian-coordinates

import numpy as np
X1 = np.array([0, 0, 1])
X2 = np.array([0, 0, 0])
X3 = np.array([1, 0, 0])
X4 = np.array([1, 1, 1])

dihedral_angle(X1,X2,X3,X4)

import numpy as np
X1 = np.array([0, 0, 1])
X2 = np.array([0, 0, 0])
X3 = np.array([1, 0, 0])
X4 = np.array([1, -1, 1])

dihedral_angle(X1,X2,X3,X4)

mainCol_NCaC

mainCol_NCaC.dtypes

# #combining residue_name and atom_number to create a unique id
# uniq_id = mainCol_NCaC[mainCol_NCaC(1)] + mainCol_NCaC[mainCol_NCaC(5)]
# #uniq_id = mainCol_NCaC[mainCol_NCaC(1)] + mainCol_NCaC[mainCol_NCaC(5)].map(str)
# mainCol_NCaC[(13)] = uniq_id

# #removing columns combined
# #mainCol_NCaC = mainCol_NCaC.drop(columns=['atom_number','residue_name','residue_number'])
# mainCol_NCaC

mainCol_NCaC.info()

mainCol_NCaC

mainCol_NCaC.iloc[2,4:7]

"""__Phi and Psi angles__"""

def psi(df):
    psi_list = []
        
    a = 0
    for i in range(len(mainCol_NCaC)):
        if np.all(np.array(mainCol_NCaC.iloc[a:a+4, 1]) == np.array(['N','CA','C','N'])):
            X1 = np.array(mainCol_NCaC.iloc[a, 4:7], dtype = "float64")
            X2 = np.array(mainCol_NCaC.iloc[a+1, 4:7], dtype = "float64")
            X3 = np.array(mainCol_NCaC.iloc[a+2, 4:7], dtype = "float64")
            X4 = np.array(mainCol_NCaC.iloc[a+3, 4:7], dtype = "float64")
      
            psi_list.append(dihedral_angle(X1,X2,X3,X4))
            a = a+3
        else:
            psi_list.append('NA')
        
    return psi_list

def phi(df):
    phi_list = []
        
    a = 2
    for i in range(len(mainCol_NCaC)):
        if np.all(np.array(mainCol_NCaC.iloc[a:a+4, 1]) == np.array(['C','N','CA','C'])):
            X1 = np.array(mainCol_NCaC.iloc[a, 4:7], dtype = "float64")
            X2 = np.array(mainCol_NCaC.iloc[a+1, 4:7], dtype = "float64")
            X3 = np.array(mainCol_NCaC.iloc[a+2, 4:7], dtype = "float64")
            X4 = np.array(mainCol_NCaC.iloc[a+3, 4:7], dtype = "float64")
      
            phi_list.append(dihedral_angle(X1,X2,X3,X4))
            a = a+3
        else:
            phi_list.append('NA')
            
    return phi_list

keh_psi = psi(mainCol_NCaC)
keh_phi = phi(mainCol_NCaC)

# # extracting unique ids
# id_list = mainCol_NCaC['Unique_ID'].tolist()

# def unique(list1):
#     unique_list = []
    
#     for x in list1:
#          if x not in unique_list:
#                 unique_list.append(x)
#     return unique_list

# unid_list = unique(id_list)

#removing NAs
import math
import numpy as np
psi_noNA = [x for x in keh_psi if x != 'NA']
phi_noNA = [x for x in keh_phi if x != 'NA']

# #Dataframe with phi and psi angles
# import pandas as pd
# dict_angles = pd.DataFrame()
# dict_angles['psi_angles']=psi_noNA
# dict_angles['phi_angles']=phi_noNA

# ID_uniq = pd.DataFrame()
# ID_uniq['Unique_ID']=unid_list

# final_table = pd.concat([ID_uniq,dict_angles], ignore_index=True, axis=1)

# final_table

"""__Ramachandran Plot__"""

import matplotlib.pyplot as plt

plt.figure(figsize=[7,7],dpi=100)
axes = plt.subplot()
axes.set_title('Ramachandran Plot')
axes.set_xlabel('Phi')
axes.set_ylabel('Psi')
axes.set_xlim(-180,180)
axes.set_ylim(-180,180)
axes.set_xticks([-180, -135, -90, -45, 0, 45, 90, 135, 180])
axes.set_yticks([-180, -135, -90, -45, 0, 45, 90, 135, 180])
plt.axhline(y=0, color='red', lw=1)
plt.axvline(x=0, color='red', lw=1)
axes.scatter(phi_noNA,psi_noNA)
plt.savefig('Ramachandran Plot')
plt.show()

"""__Secondary structures present in the protein '5KEH' include a majority of beta sheets followed by right-handed alpha helix and a few randomly distributed lef-handed alpha helix__"""
