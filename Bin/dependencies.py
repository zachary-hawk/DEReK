#!/usr/bin/env python3
import os
import re
import numpy as np

        
def get_dependencies(file_path):
    dependencies = []
    with open(file_path, 'r') as file:
        content = file.read()
        pattern = r'use\s+(\w+)'
        matches = re.findall(pattern, content, re.IGNORECASE)
        for dependency in matches:
            if dependency.lower() not in ['iso_fortran_env', 'mpi']:
                dependencies.append(dependency)

    return list(np.unique(dependencies))



def get_dependent_files():
    current_dir = os.getcwd()
    folder_path = current_dir
    file_paths = [os.path.join(folder_path, file) for file in os.listdir(folder_path) if file.endswith('.f90')]
    file_names=[]

    ordered_files=np.empty(len(file_paths),dtype=str)

    for file in file_paths:
        file_names.append(os.path.basename(file)[0:-4])

    counter = 0
    notSorted=True
    i=0
    while notSorted:
        counter+=1
        # get the file name in the first spot
        temp_name=file_names[i]
        oldFilenames = list(file_names)
        path=file_names[i]+'.f90'
        deps=get_dependencies(path)
        max_mod=0

        #print(temp_name,deps)
        #print(file_names)
        
        for j,module in enumerate(file_names):
            if module in deps:
                max_mod = j
        #print("max_mod",max_mod,"counter",i)
        if max_mod>i:        
            file_names[i:max_mod] = file_names[1+i:max_mod+1]
            file_names[max_mod] = temp_name
        else:
                i+=1

        #print(file_names)
        #print('')

        # Check if we move on
        if i==len(file_names):
            notSorted = False

        if counter > 3*len(file_names):
            raise Exception("Circular dependancy detected!!")

        
    #    print(file_paths,get_dependencies(file))
    

       
    return file_names


# Usage example

ordered_files = get_dependent_files()
name_list=[]
for file_path in ordered_files:
    file_name = os.path.basename(file_path+".f90")
    name_list.append(file_name)
print(' '.join(name_list))
