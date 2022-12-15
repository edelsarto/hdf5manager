import os
import string
import sys

from asyncio.windows_events import NULL
import asyncio

import time

from webbrowser import get

import h5py

import numpy as np

from PySide6.QtCore import QFile, Qt
from PySide6.QtUiTools import QUiLoader
from PySide6.QtWidgets import (
    QApplication,
    QMainWindow,
    QTableWidget,
    QTableWidgetItem,
    QWidget,
    QWidgetAction,
)

import H5Search as h5search
import H5Create as h5create
import ExcelExtract as ecxcelEx

from modules import *
from widgets import *

import matplotlib.pyplot as plt

import threading

# IMPORT / GUI AND MODULES AND WIDGETS

os.environ["QT_FONT_DPI"] = "96"  # FIX Problem for High DPI and Scale above 100%

# SET AS GLOBAL WIDGET
# Prova_BL-34_14_19_187
# from . resources_rc import *
widgets = None

param_list_item = []
ds_list_item = []
resultsArray = []
listItemsSelected = []
h5file = []

path = None

class AnotherWindow(QWidget):

    table = None

    def __init__(self):

        super().__init__()

    def AddTable(self, row, column, labelText, windowTitle):
        
        #SET WIDGETS VAR
        layout = QVBoxLayout()

        self.label = QLabel(labelText)
        dataList = labelText.split("&")

        global table
        table = QTableWidget(row, column)

        plotbutton = QPushButton("PLOT")
        
        dataArea = QTableWidget(len(dataList), 1)
        
        #SET BUTTON PLOT 
        plotbutton.setObjectName("PLOT")
        plotbutton.setFixedWidth(55)
        plotbutton.clicked.connect(self.ButtonClicked)
        
        #SET DATA AREA
        dataArea.setEditTriggers(QTableWidget.EditTrigger.NoEditTriggers)
        
        spaceArray = []
        for i in range(len(dataList)):
            
            dataArea.setItem(
                i, 0, QTableWidgetItem(str(dataList[i]))
            ) 
            spaceArray.append(" ")
            
        dataArea.setHorizontalHeaderLabels(" ")
        dataArea.setVerticalHeaderLabels(spaceArray)
        
        #SET WINDOW FORMAT
        self.setWindowTitle(windowTitle)
        self.setMinimumWidth(50)
        self.resize(250,500)
        
        #ADD WIDGET CREATED
        layout.addWidget(plotbutton)
        layout.addWidget(table)
        layout.addWidget(dataArea)
        layout.addWidget(self.label)
        
        
        self.setLayout(layout)
        
        return table

    def ButtonClicked(self):

        def PlotTable():
            
            if table.itemAt(0, 0) != None:

                time = []
                for i in range(table.rowCount()):
                    
                    item = float(table.item(i,0).text())
                    
                    if item != None:

                        time.append(float(item))
                
                value = []
                for i in range(table.rowCount()):

                    item = float(table.item(i,2).text())
                    
                    if item != None:

                        value.append(float(item))
                
                valueNegativeUncertainty = []
                for i in range(table.rowCount()):

                    item = float(table.item(i,1).text())
                    
                    if item != None:

                        valueNegativeUncertainty.append(float(item))
                        
                valuePositiveUncertainty = []
                for i in range(table.rowCount()):

                    item = float(table.item(i,3).text())
                    
                    if item != None:

                        valuePositiveUncertainty.append(float(item))
                
                label = self.label.text()
                
                plt.figure(label)

                plt.plot(time, value)
                plt.plot(time, valueNegativeUncertainty)
                plt.plot(time, valuePositiveUncertainty)

                plt.xlabel('Time - s')

                plt.ylabel(label.split("&")[1] + ' - ' + label.split("&")[3])

                plt.show()

        btn = self.sender()
        btnName = btn.objectName()

        if btnName == "PLOT":
            #print("PLOTTING!!!")
            PlotTable()

class MainWindow(QMainWindow):
    def __init__(self):

        # self.nw = AnotherWindow()

        QMainWindow.__init__(self)

        # SET AS GLOBAL WIDGETS

        self.ui = Ui_MainWindow()
        self.ui.setupUi(self)
        self.nw = None

        global widgets
        widgets = self.ui
        
        global path

        global param_list_item
        global ds_list_item
        global resultsArray
        global listItemsSelected
        global h5file
        global newWindow
        newWindow = [AnotherWindow()]

        ######### USE CUSTOM TITLE BAR | USE AS "False" FOR MAC OR LINUX

        Settings.ENABLE_CUSTOM_TITLE_BAR = True

        ######### APP NAME #########

        title = "HDF5 - Database Manager"
        description = "HDF5 Database Manager"

        ######### APPLY TEXTS #########

        self.setWindowTitle(title)
        widgets.titleRightInfo.setText(description)

        ######### TOGGLE MENU #########

        widgets.toggleButton.clicked.connect(lambda: UIFunctions.toggleMenu(self, True))

        ######### SET UI DEFINITIONS #########

        UIFunctions.uiDefinitions(self)

        # QTableWidget PARAMETERS

        widgets.tableWidget.horizontalHeader().setSectionResizeMode(QHeaderView.Stretch)
        widgets.tableWidget_showSearchResult.setEditTriggers(
            QTableWidget.EditTrigger.NoEditTriggers
        )

        ######### BUTTONS CLICK #########

        ######### LEFT MENUS #########

        widgets.btn_home.clicked.connect(self.buttonClick)
        widgets.btn_widgets.clicked.connect(self.buttonClick)
        widgets.btn_create_page.clicked.connect(self.buttonClick)

        widgets.btn_test.clicked.connect(self.buttonClick)

        widgets.btn_search.clicked.connect(self.buttonClick)
        widgets.btn_search.clicked.connect(self.buttonClick)
        widgets.tableWidget_showSearchResult.doubleClicked.connect(self.buttonClick)

        widgets.btn_create.clicked.connect(self.buttonClick)

        # print(widgets.comboBox_2.currentText()) #possibile utilizzo del menu a tendina per i parametri

        ######### EXTRA LEFT BOX #########

        def openCloseLeftBox():
            UIFunctions.toggleLeftBox(self, True)
            widgets.toggleLeftBox.clicked.connect(openCloseLeftBox)
            widgets.extraCloseColumnBtn.clicked.connect(openCloseLeftBox)

        ######### EXTRA RIGHT BOX #########

        def openCloseRightBox():
            UIFunctions.toggleRightBox(self, True)
            widgets.settingsTopBtn.clicked.connect(openCloseRightBox)

        ######### SHOW APP #########

        self.show()

        ######### SET CUSTOM THEME #########

        useCustomTheme = False
        themeFile = "themes\py_dracula_light.qss"

        ######### SET THEME AND HACKS #########

        if useCustomTheme:

            # LOAD AND APPLY STYLE

            UIFunctions.theme(self, themeFile, True)

            # SET HACKS

            AppFunctions.setThemeHack(self)

        ######### SET HOME PAGE AND SELECT MENU #########

        widgets.stackedWidget.setCurrentWidget(widgets.test)
        widgets.btn_test.setStyleSheet(
            UIFunctions.selectMenu(widgets.btn_test.styleSheet())
        )

    ######### BUTTONS CLICK #########

    # Post here your functions for clicked buttons #
    def buttonClick(self):

        ### Take data ###
        def GetData(self):

            path = (
                widgets.lineEdit_url_6.text() + "/"
                + widgets.lineEdit_url_1.text()
            )
            # C:/Users/Andrea/Desktop/HDF5ManagerGithub/HDF5 File/NEW TEST/
            # C:/Users/e_del/Documents/hdf5manager/HDF5 File/NEW TEST/
            
            with h5py.File(
                "C:/Users/Andrea/Desktop/HDF5ManagerGithub/HDF5 File/"
                + widgets.lineEdit_url_6.text() + "/"
                + widgets.lineEdit_url_1.text()
                + ".h5",
                "r",
            ) as hdf:

                h5file = hdf
                ds_list_item = h5search.ReadH5File(h5file)
                FillSearchResultTable(ds_list_item)

        ### Fill search result table with dataset path founded ###
        def FillSearchResultTable(dsList):

            ### FILTRARE IN BASE AI TAG ###
            filteredDsList = []
            filteredDsList.clear()

            tagList = []
            # togliere spazi e tutte le lettere in lowcase
            tagList.append(widgets.lineEdit_url_2.text())  # PARAMETER
            tagList.append(widgets.lineEdit_url_3.text())  # SENSO ID
            tagList.append(widgets.lineEdit_url_4.text())  # LOCATION1
            tagList.append(widgets.lineEdit_url_5.text())  # LOCATION2

            isEmpty = True
            isAbsolute = False

            absolute = widgets.checkBox_2.isChecked()

            for tag in tagList:

                if tag != None and tag != "":
                    isEmpty = False
                    break

            # print("TAG LIST EMPTY? ", isEmpty)

            for ds in dsList:

                ds = h5py.Dataset(ds.id)

                # if (ds.name != "/Time/TIME"):

                if isEmpty:

                    filteredDsList.append(str(ds.name))
                    continue

                # togliere spazi e tutte le lettere in lowcase
                for tag in tagList:

                    if absolute:

                        if tag != None and tag != "":

                            # print("TAG: ", tag, " -- ATTR: " , ds.attrs['PARAMETER'])

                            if (
                                tag + " " == ds.attrs["PARAMETER"]
                                or tag == ds.name.split("/")[2]
                                or tag == ds.attrs["LOCATION1"]
                                or tag == ds.attrs["LOCATION2"]
                            ):

                                isAbsolute = True
                            else:
                                isAbsolute = False
                                break

                            # print("IS ABSOLUTE? ", isAbsolute)

                    else:

                        if (
                            tag + " " == ds.attrs["PARAMETER"]
                            or tag == ds.name.split("/")[2]
                            or tag == ds.attrs["LOCATION1"]
                            or tag == ds.attrs["LOCATION2"]
                        ):

                            filteredDsList.append(str(ds.name))
                            break

                if isAbsolute:

                    filteredDsList.append(str(ds.name))

                # else:

                #         filteredDsList.append(str(ds.name))

            # widgets.tableWidget_showSearchResult.clear()

            widgets.tableWidget_showSearchResult.setRowCount(len(filteredDsList))
            widgets.tableWidget_showSearchResult.setColumnCount(1)

            # widgets.tableWidget_showSearchResult.resizeColumnToContents(0)
            # print("W: ", widgets.tableWidget_showSearchResult.columnWidth(0))
            widgets.tableWidget_showSearchResult.setColumnWidth(0, 300)

            indX = 0
            for fds in filteredDsList:
                # fds = h5py.Dataset(fds.id)
                widgets.tableWidget_showSearchResult.setItem(
                    indX, 0, QTableWidgetItem(str(fds))
                )
                indX += 1

        ### Fill result table with dataset choose data ###
        def FillTable(dsList, itemSel):

            toFillArray = []
            # C:/Users/Andrea/Desktop/HDF5ManagerGithub/HDF5 File/NEW TEST/
            # C:/Users/e_del/Documents/hdf5manager/HDF5 File/NEW TEST/
            path = (
                "C:/Users/Andrea/Desktop/HDF5ManagerGithub/HDF5 File/"
                + widgets.lineEdit_url_6.text() + "/"
                + widgets.lineEdit_url_1.text()
            )
            with h5py.File(
                path
                + ".h5",
                "r",
            ) as hdf:

                dsList = h5search.ReadH5File(hdf)

                dsName = ""
                taglist = []

                for e in dsList:

                    if e.name == itemSel:

                        toFillArray = np.array(e)

                        dsName = e.name

                        # if(dsName != "/Time/TIME"):

                        taglist.append(e.attrs["PARAMETER"])
                        taglist.append(e.attrs["LOCATION1"])
                        taglist.append(e.attrs["LOCATION2"])
                        taglist.append(e.attrs["LOCATION3"])
                        taglist.append(e.attrs["UNIT"])
                        taglist.append(e.attrs["RANGEMAX"])
                        taglist.append(e.attrs["RANGEMIN"])
                        taglist.append(e.attrs["ERROR"])        #7

                        # break

                # if(dsName != "/Time/TIME"):
                #print(type(taglist[0]) )
                print("TAGLIST: ", taglist)
                windowLabel = "&" + dsName.split("/")[2] + "& - { / [Param: &" + taglist[0] + "&] / [L1:&" + taglist[1] + "&] / [L2: &" + str(taglist[2]) + "&] / [L3:&" + taglist[3] + "&] / [UNIT: &" + taglist[4] + "&]  / [RANGEMAX:&" + f"{str(taglist[5])}" + "&] / [RANGEMIN:&" + f"{str(taglist[6])}" + "&] / [ERROR:&" + f"{str(taglist[7])}" + "&]}" #

                # else:

                #windowLabel = dsName.split("/")[2]

                indX = 0
                indY = 0

                if self.nw is None:

                    self.nw = AnotherWindow()

                newWindow.append(self.nw)

                tableTest = self.nw.AddTable(
                    toFillArray.shape[0], 4, windowLabel, dsName.split("/")[2]
                )
                
                tableTest.setEditTriggers(QTableWidget.EditTrigger.NoEditTriggers)

                for y in range(4):

                    indX = 0

                    for x in range(toFillArray.shape[0]):
                        
                        if y == 0:
                            
                             tableTest.setItem(
                                x, y, QTableWidgetItem(str(toFillArray[x][0]))
                            )
                             
                        elif y == 1:
                            
                            tableTest.setItem(
                                x, y, QTableWidgetItem(str(toFillArray[x][1] - taglist[7]))
                            )
                            
                        elif y == 2:
                            
                            tableTest.setItem(
                                x, y, QTableWidgetItem(str(toFillArray[x][1]))
                            )
                        
                        elif y == 3:
                            
                            tableTest.setItem(
                                x, y, QTableWidgetItem(str(toFillArray[x][1] + taglist[7]))
                            )

                        indX += 1

                indY += 1

                self.nw.show()

            self.nw = None

        ### Create file h5 from file url ###
        def CreateH5():

            h5create.Create(
                "prova_" + time.strftime("%H_%M_%S"), widgets.lineEdit_2.text()
            )

        ### Set column label name with dataset attribute ###
        # def SetHorizontalLabel(path):
        #     widgets.tableWidget_showResult.setHorizontalHeaderLabels(searchhdf5.SearchDatasetAttr(path))

        ######## GET BUTTON CLICKED #########

        btn = self.sender()
        btnName = btn.objectName()

        ######## PRINT BTN NAME #########

        print(f'Button "{btnName}" pressed!')
        print(f'Button "{btn}" pressed!')

        ######## SELECT RESULT #########

        if btnName == "tableWidget_showSearchResult":

            listItemsSelected = btn.selectedItems()
            # print("DATASET SELECTED ", listItemsSelected[0].text())
            FillTable(ds_list_item, listItemsSelected[0].text())

            # SetHorizontalLabel(widgets.lineEdit_url_1.text()
            # + listItemsSelected[0].text())

        ######## SHOW HOME PAGE #########

        if btnName == "btn_home":
            widgets.stackedWidget.setCurrentWidget(widgets.home)
            UIFunctions.resetStyle(self, btnName)
            btn.setStyleSheet(UIFunctions.selectMenu(btn.styleSheet()))
            print(
                "CURRENT WIDGET: ", widgets.stackedWidget.currentWidget().objectName()
            )

        ######## SHOW WIDGETS PAGE #########

        if btnName == "btn_widgets":
            widgets.stackedWidget.setCurrentWidget(widgets.widgets)
            UIFunctions.resetStyle(self, btnName)
            btn.setStyleSheet(UIFunctions.selectMenu(btn.styleSheet()))
            print(
                "CURRENT WIDGET: ", widgets.stackedWidget.currentWidget().objectName()
            )

        ######## SHOW CREATE PAGE #########

        if btnName == "btn_create_page":
            widgets.stackedWidget.setCurrentWidget(widgets.create_page)
            UIFunctions.resetStyle(self, btnName)
            btn.setStyleSheet(UIFunctions.selectMenu(btn.styleSheet()))
            print(
                "CURRENT WIDGET: ", widgets.stackedWidget.currentWidget().objectName()
            )

        ######## SHOW TEST PAGE #########

        if btnName == "btn_test":
            widgets.stackedWidget.setCurrentWidget(widgets.test)  # SET PAGE
            UIFunctions.resetStyle(self, btnName)  # RESET ANOTHERS BUTTONS SELECTED
            btn.setStyleSheet(UIFunctions.selectMenu(btn.styleSheet()))  # SELECT MENU
            print(
                "CURRENT WIDGET: ", widgets.stackedWidget.currentWidget().objectName()
            )

        if btnName == "btn_search":
            GetData(self)

        if btnName == "btn_create":
            CreateH5()

        # keyboard.add_hotkey("enter", lambda: GetData(self))

    ######## RESIZE EVENTS #########

    def resizeEvent(self, event):

        # Update Size Grips

        UIFunctions.resize_grips(self)

    ######## MOUSE CLICK EVENTS #########

    def mousePressEvent(self, event):

        ######## SET DRAG POS WINDOW #########

        self.dragPos = event.globalPos()


######## PRINT MOUSE EVENTS #########

# if event.buttons() == Qt.LeftButton:
#     print('Mouse click: LEFT CLICK')
# if event.buttons() == Qt.RightButton:
#     print('Mouse click: RIGHT CLICK')

if __name__ == "__main__":
    app = QApplication(sys.argv)
    app.setWindowIcon(QIcon("icon.ico"))
    window = MainWindow()
    sys.exit(app.exec())
