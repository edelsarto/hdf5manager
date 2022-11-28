from h5py._hl import base, dataset
from h5py._hl.datatype import Datatype
from h5py._hl.files import File
import h5py

import time

import numpy as np
from numpy.core.defchararray import index
from numpy.core.fromnumeric import _choose_dispatcher

import ExcelExtract as exceldata

import numbers
#"excel/Prova_BL-34.xlsx"
def Create(h5Name, excelName):#async 

    print("PART1 - SAVE: PARAM SENSORID")
    start = time.time()

    parameterList = exceldata.SaveParameters(excelName)
    #print(parameterList)
    parameterListFiltered = FilterParameterList(parameterList)
    #print(parameterListFiltered)

    sensorIdList = exceldata.SaveSensorID(excelName)
    print("END PART1: ", time.time() - start)
    #C:/Users/Andrea/Desktop/HDF5ManagerGithub/HDF5 File/NEW TEST/
    with h5py.File('C:/Users/e_del/Documents/hdf5manager/HDF5 File/NEW TEST' + h5Name + '.h5', 'w') as hdf:

        #datasetTemp = h5py.Dataset()
        
        #GROUP
        groupList = [h5py.Group(hdf.id)]

        #print("PART2 - SAVE TIMELIST")        #timeList = hdf.create_group("Time")
        #timeList = exceldata.SaveTimeList(excelName)
        #print("END PART2: ", time.time() - start)
        
        print("PART3 - CREAZIONE DATASET")
        for i in range(parameterListFiltered.__len__()):

            groupList.append(hdf.create_group(parameterListFiltered[i]))
            
        #DATASET
        for i in range(sensorIdList.__len__()):
            
            for j in range(groupList.__len__()):

                testParam = str("/" + parameterList[i])
                
                #print("PARAM: ", parameterList[i])
                #print("SENSOR ID: ", sensorIdList[i])
                
                valueAndTagList = []
                tagList = []
                valueTimeList = []
                
                if(groupList[j].name == testParam):

                    valueAndTagList = exceldata.SaveValuesList(excelName, sensorIdList[i])
                    #print(valueAndTagList.pop(0))
                    
                    timeList = exceldata.SaveTimeList(excelName, sensorIdList[i])
                    #print(timeList.pop(0))

                    ind = 0
                    for e in valueAndTagList:
                        #print("LEN CAT LIST ", exceldata.SaveCategoryList(excelName).__len__())
                        if(ind <= exceldata.SaveCategoryList(excelName).__len__() - 1):
                            
                            tagList.append(e) 
                            ind+=1
                            
                        else:
                            
                            for y in range(ind):
                                
                                valueAndTagList.remove(tagList[y])
                                
                            break
                    
                    #print("###### ", len(timeList),"--", len(valueAndTagList) ,"######")
                    
                    for x in range(len(timeList)):
                        
                        valueTimeList.append([timeList[x], valueAndTagList[x]])

                    #print(valueTimeList)

                    print("END PART3: ", time.time() - start)
                    print("TAGS: ",tagList)
                    #print(valueTimeList)
                    print("#########################  ", i ,"  #########################")

                    #DATASET TAG/METADATA
                    #PENSARE ANCHE IN CASO DI LOCATION3
                    datasetTemp = groupList[j].create_dataset(sensorIdList[i], data=valueTimeList )
                    datasetTemp = h5py.Dataset(datasetTemp.id)
                    
                    tagList.pop(0)

                    datasetTemp.attrs.create('PARAMETER', "",None,None)
                    datasetTemp.attrs['PARAMETER'] = tagList[0]

                    datasetTemp.attrs.create('LOCATION1', "",None,None)
                    datasetTemp.attrs['LOCATION1'] = tagList[1]

                    datasetTemp.attrs.create('LOCATION2', "",None,None)
                    datasetTemp.attrs['LOCATION2'] = tagList[2]
                    
                    datasetTemp.attrs.create('LOCATION3', "",None,None)
                    
                    if tagList[3] != None:
                        datasetTemp.attrs['LOCATION3'] = tagList[3]

                    datasetTemp.attrs.create('UNIT', "",None,None)
                    datasetTemp.attrs['UNIT'] = tagList[4]
                    
                    datasetTemp.attrs.create('RANGEMAX', "",None,None)
                    datasetTemp.attrs['RANGEMAX'] = tagList[5]
                    
                    datasetTemp.attrs.create('RANGEMIN', "",None,None)
                    datasetTemp.attrs['RANGEMIN'] = tagList[6]
                    
                    datasetTemp.attrs.create('ERROR', "",None,None)
                    datasetTemp.attrs['ERROR'] = tagList[7]
                    
                    #break
        
    end = time.time()
    print("######################### FINISH #########################")
    print("TERMINATED in ", end - start)
   
    return

def FilterParameterList(parameterArray):

    parameterArrayFiltered = []

    for e in range(parameterArray.__len__()):

        #if(e >= 2):

        if(parameterArray[e] != parameterArray[e-1]):

            parameterArrayFiltered.append(parameterArray[e])     

    return parameterArrayFiltered

######### MAIN ##########
#Create("Prova_BL-34_" + time.strftime("%H_%M_%S"), "excel/Prova_BL-34.xlsx")
#print(FilterParameterList(exceldata.SaveParameters("excel/Prova_BL-34.xlsx")))