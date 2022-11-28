from inspect import Parameter, _void
from h5py._hl import base, dataset
from h5py._hl.datatype import Datatype
from h5py._hl.files import File
from h5py import File, _hl
import h5py

import numpy as np
from numpy.core.defchararray import count, index
from numpy.core.fromnumeric import _choose_dispatcher

import time

# cerca il file fisico in cartella data
def SearchFile():
    return


# apre e legge struttura del file h5
def ReadH5File(hdf):

    item = hdf
    parameterObjList = []
    sensorKeyList = []
    sensorObjList = []

    h5KeyList = list(item.keys())  # tutti i GROUPS-KEY presenti

    for groupKey in h5KeyList:

        parameterObjList.append(item.get(groupKey))  # tutti i GROUPS-OBJ presenti

    for paramObj in parameterObjList:

        paramObj = h5py.Group(paramObj.id)

        sensorKeyList.append(
            list(paramObj.keys())
        )  # tutti i DATASET-KEYS per ogni GROUP
        # (ogni cella contiene la lista di tutti i ds all'interno di un gruppo)

    for paramObj in parameterObjList:

        paramObj = h5py.Group(paramObj.id)

        for datasetGroup in sensorKeyList:

            for datasetSingle in datasetGroup:

                if paramObj.get(datasetSingle) != None:

                    sensorObjList.append(
                        paramObj.get(datasetSingle)
                    )  # tutti i DATSET-OBJ

    return sensorObjList


####################  MAIN  ####################

# ReadH5File()
