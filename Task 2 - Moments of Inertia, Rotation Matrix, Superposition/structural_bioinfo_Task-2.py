# -*- coding: utf-8 -*-

import regex as re

#opening 1st file
mola=open('first_mol.txt','w')
with open('1zta_ca_1.xyz') as f:
  for line in f:
    if re.search("C", line):
      mola.write(line)
mola.close()

#opening 2nd file
mola=open('sec_mol.txt','w')
with open('1zta_ca_2.xyz') as f:
  for line in f:
    if re.search("C", line):
      mola.write(line)
mola.close()

import pandas as pd

#reading text files and converting to dataframe
mol1 = pd.read_csv('first_mol.txt', sep='\s+', header=None)
mol2 = pd.read_csv('sec_mol.txt', sep='\s+', header=None)

one_df = pd.DataFrame(mol1)
one_df_noC = one_df.iloc[:,1:]

two_df = pd.DataFrame(mol2)
two_df_noC = two_df.iloc[:,1:]

import numpy as np

#converting df to np.array
mol1 = one_df_noC.to_numpy()
mol2 = two_df_noC.to_numpy()

#center of mass of both molecules
x1_coord_sum = one_df_noC.iloc[:,0].sum()/35
y1_coord_sum = one_df_noC.iloc[:,1].sum()/35
z1_coord_sum = one_df_noC.iloc[:,2].sum()/35
com_mol1 = np.array([x1_coord_sum,y1_coord_sum,z1_coord_sum])


x2_coord_sum = two_df_noC.iloc[:,0].sum()/35
y2_coord_sum = two_df_noC.iloc[:,1].sum()/35
z2_coord_sum = two_df_noC.iloc[:,2].sum()/35
com_mol2 = np.array([x2_coord_sum,y2_coord_sum,z2_coord_sum])

com_mol1, com_mol2

#centering coordinates to zero
one_df_noC = one_df_noC - com_mol1
two_df_noC = two_df_noC - com_mol2

np.array(one_df_noC.iloc[0,:])

#Radius of gyration for 1st molecule
import math

rg_1 = np.linalg.norm(mol1 - com_mol1)**2
mol1_rg = math.sqrt(rg_1/35)

#Radius of gyration for 2nd molecule
rg_2 = np.linalg.norm(mol2 - com_mol2)**2
mol2_rg = math.sqrt(rg_2/35)

mol1_rg, mol2_rg

#molecule vectors
import numpy as np

one_v1 = np.array([0.541059, -0.299595, 0.785810])
one_v2 = np.array([0.787317, -0.103191, -0.581439])
one_v3 = np.array ([0.010829, 0.039588, 0.007637])

two_v1 = np.array([0.027761, -0.103066, 0.994287])
two_v2 = np.array([-0.010364, -0.974455, -0.100720])
two_v3 = np.array([0.050197, -0.000385, -0.001441])

miv_one = np.array([one_v1, one_v2, one_v3])
miv_two = np.array([two_v1, two_v2, two_v3])

#skew symmetric matrix
def skew_symm(x):
  sm = np.array([[0,-x[2],x[1]], 
                  [x[2],0,x[0]],
                 [-x[1],x[0],0]])
  return sm

#rotation matrix function
def rotn_mat(v1,v2):
  #scaling vectors
  x = v1/np.linalg.norm(v1)
  y = v2/np.linalg.norm(v2)
  #cross product for orthogonal vector
  or_vc = np.cross(x,y)
  #dot product
  dp = np.dot(x,y)
  #magnitude of orthogonal vector
  or_mag = np.linalg.norm(or_vc)
  #scaling orthogonal vector
  or_vc = or_vc/np.linalg.norm(or_vc)
  #identity matrix 
  eye = np.eye(3)
  #orthogonal outer product
  outer = np.outer(or_vc,or_vc)

  final_r = dp * eye + or_mag * skew_symm(or_vc) + outer * (1-dp)

  return final_r

#https://en.wikipedia.org/wiki/Rotation_matrix#In_three_dimensions

#3 rotation matrices
rm1 = rotn_mat(one_v1,two_v1)
rm2 = rotn_mat(one_v2, two_v2)
rm3 = rotn_mat(one_v3, two_v3)

#function for rotation and generating new coordinates
def rotation(one_df_noC,rm):
  new_coord = []

  for h in range(len(one_df_noC)):
    single_atom = np.array(one_df_noC.iloc[h,:])
    a = np.dot(rm,single_atom)
    new_coord.append([a[0],a[1],a[2]])
  
  return new_coord

#rotations
import pandas as pd
rot_one = rotation(one_df_noC,rm1)
rot_one = pd.DataFrame(rot_one)

rot_two = rotation(rot_one,rm2)
rot_two = pd.DataFrame(rot_two)

rot_three = rotation(rot_one,rm3)
rot_three = pd.DataFrame(rot_three)

#RMSD

def rmsd(df1,df2):
  j = 0
  k = 0
  l = 0

  X_1 = df1.iloc[:,0]
  Y_1 = df1.iloc[:,1]
  Z_1 = df1.iloc[:,2]

  X_2 = df2.iloc[:,0]
  Y_2 = df2.iloc[:,1]
  Z_2 = df2.iloc[:,2]

  for i in range(len(df1)):
    j += (X_1[i] - X_2[i])**2
    k += (Y_1[i] - Y_2[i])**2
    l += (Z_1[i] - Z_2[i])**2
  
  total = j + k + l
  r = np.sqrt(total/len(df1))

  return r

before_rotation_rmsd = rmsd(one_df_noC,two_df_noC)
after_rotation_rmsd = rmsd(rot_three,two_df_noC)

before_rotation_rmsd, after_rotation_rmsd

"""**RMSD** reduced from 18.418 to 5.153 after superposition."""
