#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Oct 27 21:40:39 2019

@author: home
"""



import os
import glob
import pandas as pd

#User Inputs
newwd = ""
os.chdir(newwd)
extension_name = 'csv'
output_name = ""

#Code

all_filenames = [i for i in glob.glob('*.{}'.format(extension_name))]

#combine all files in the list
combined_csv = pd.concat([pd.read_csv(f) for f in all_filenames ])
combined_csv.to_csv( output_name, index=False, encoding='utf-8-sig')
