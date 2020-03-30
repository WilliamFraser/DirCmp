#!/usr/bin/python3
"""
    Copyright 2020 William Fraser

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program (gpl-3.0.txt).  If not,
    see <https://www.gnu.org/licenses/>.
"""

import os
import shutil
import sys
from filecmp import cmp as COMPAREFILES
import PyQt5.QtWidgets as QtGui
from PyQt5.QtGui import (QColor, QStandardItemModel, QStandardItem,
                         QFont, QIcon, QCursor, QInputMethodQueryEvent,
                         QBrush)
from PyQt5.QtCore import (QAbstractItemModel, QModelIndex, Qt, QVariant,
                          pyqtSignal, QSettings, QRect)
from datetime import datetime
import threading
import json
from subprocess import Popen, PIPE


class CmpUtil(object):
    """ Class to hold, save, and retrieve data about comparison
    utilities."""
    def __init__(self, name, exe, displayoutput, handlesfolders, handlesfiles,
                 defaultforfiles=False, defaultforfolders=False):
        self.name = name
        self.exe = exe
        self.displayoutput = displayoutput
        self.handlesfolders = handlesfolders
        self.handlesfiles = handlesfiles
        self.defaultforfiles = defaultforfiles
        self.defaultforfolders = defaultforfolders

    def GetDefaultForFiles(s):
        """Returns CmpUtil object for the default utility for handling
        comparing files in the supplied QSettings object.
        Returns None if no default set.
        """

        filedefault = s.value("filedefault", defaultValue=None)
        if not filedefault:
            return None
        folderdefault = s.value("folderdefault", defaultValue="")
        s.beginGroup(filedefault)

        ret = CmpUtil(filedefault,
                      s.value("exe", defaultValue=""),
                      s.value("displayoutput", defaultValue=False, type=bool),
                      s.value("handlesfolders", defaultValue=False, type=bool),
                      s.value("handlesfiles", defaultValue=True, type=bool),
                      True,
                      folderdefault == filedefault)
        s.endGroup()
        return ret

    def GetDefaultForFolders(s):
        """Returns CmpUtil object for the default utility for handling
        comparing directories in the supplied QSettings object.
        Returns None if no default set.
        """

        folderdefault = s.value("folderdefault", defaultValue=None)
        if not folderdefault:
            return None
        filedefault = s.value("filedefault", defaultValue="")
        s.beginGroup(folderdefault)

        ret = CmpUtil(folderdefault,
                      s.value("exe", defaultValue=""),
                      s.value("displayoutput", defaultValue=False, type=bool),
                      s.value("handlesfolders", defaultValue=False, type=bool),
                      s.value("handlesfiles", defaultValue=True, type=bool),
                      folderdefault == filedefault,
                      True)
        s.endGroup()
        return ret

    def GetFromSettings(s):
        """returns list of CmpUtil objects, saved in the supplied
        QSettings object.  Will initiate with default objects if
        doesn't exist."""

        if not s.value("Initiated", False, type=bool):
            # name, exe, wait, displayoutput, folders, files
            for n, e, o, fo, fi in [("diff", "/usr/bin/diff", True, True,
                                     True),
                                    ("meld", "/usr/bin/meld", False, False,
                                     True),
                                    ("colordiff", "/usr/bin/colordiff", True,
                                     True, True),
                                    ("vimdiff", "/usr/bin/vimdiff", True,
                                     False, True),
                                    ("kompare", "/usr/bin/kompare", False,
                                     True, True),
                                    ("diffuse", "/usr/bin/diffuse", False,
                                     False, True),
                                    ("xxdiff", "/usr/bin/xxdiff", False, True,
                                     True),
                                    ("kdiff3", "/usr/bin/kdiff3", False, True,
                                     True),
                                    ("tkdiff", "/usr/bin/tkdiff", False, False,
                                     True)]:
                s.beginGroup(n)
                s.setValue("exe", e)
                s.setValue("displayoutput", o)
                s.setValue("handlesfolders", fo)
                s.setValue("handlesfiles", fi)
                s.endGroup()

            s.setValue("Initiated", True)
            s.sync()

        filedefault = s.value("filedefault", defaultValue="")
        folderdefault = s.value("folderdefault", defaultValue="")
        ret = []
        for group in s.childGroups():
            if group is "General":
                continue
            s.beginGroup(group)

            ret += [CmpUtil(group,
                            s.value("exe", defaultValue=""),
                            s.value("displayoutput", defaultValue=False,
                                    type=bool),
                            s.value("handlesfolders", defaultValue=False,
                                    type=bool),
                            s.value("handlesfiles", defaultValue=True,
                                    type=bool),
                            filedefault == group,
                            folderdefault == group)]
            s.endGroup()
        return ret

    def SaveToSettings(s, utils):
        """Saves the supplied list of CmpUtil objects to the supplied
        QSettings object."""

        s.clear()

        filedefault = None
        folderdefault = None

        for u in utils:
            s.beginGroup(u.name)
            s.setValue("exe", u.exe)
            s.setValue("displayoutput", u.displayoutput)
            s.setValue("handlesfolders", u.handlesfolders)
            s.setValue("handlesfiles", u.handlesfiles)
            if u.defaultforfiles:
                filedefault = u.name
            if u.defaultforfolders:
                folderdefault = u.name
            s.endGroup()

        if filedefault:
            s.setValue("filedefault", filedefault)
        if folderdefault:
            s.setValue("folderdefault", folderdefault)
        s.setValue("Initiated", True)
        s.sync()


# Enum not available until python 3.4 so create my own
class CmpResult:
    """Class to encapsulate the possible results of file and directory
    comparisons."""

    File_Identical = 1
    File_In_A_Only = 2
    File_In_B_Only = 3
    File_Meta_Different = 4
    File_Different = 5
    File_Incomprable = 6
    File_Await_Comparison = 7
    File_Being_Compared = 8

    Description = ['',
                   'Identical',
                   'In Directory A Only',
                   'In Directory B Only',
                   'Meta data Different',
                   'Files Different',
                   'Files Incomprable',
                   'Awaiting Comparison',
                   'Being Compared']

    RGBValues = [0xFFFFFF,
                 0xFFFFFF,
                 0x8C92AC,
                 0x8C92AC,
                 0xFFE640,
                 0xFF4040,
                 0xAFEEEE,
                 0xFFFF33,
                 0xFFFF33]


def ExtractMeta(stat):
    """returns dictionary of file stats from a stat object."""

    return {"last_access": int(stat.st_atime),
            "last_change": int(stat.st_ctime),
            "last_modify": int(stat.st_mtime),
            "size": stat.st_size,
            "user_id": stat.st_uid,
            "group_id": stat.st_gid,
            "mode": os.st.S_IMODE(stat.st_mode),
            "access": os.st.filemode(stat.st_mode)[1:],
            "type": os.st.S_IFMT(stat.st_mode),
            "isdir": os.st.S_ISDIR(stat.st_mode),
            "isfile": os.st.S_ISREG(stat.st_mode)}


def GetUnilateralTree(dirpath, cmpresult):
    """returns results of comparison of a directory tree without a
    comprable tree to compare with.  dirpath is the start point for the
    tree, cmpresult is the default result."""

    results = []

    dircontents = os.listdir(dirpath)

    for f in dircontents:
        name = os.path.join(dirpath, f)

        if os.path.isdir(name):
            results += [{"result": cmpresult, "filename": f,
                         "children": GetUnilateralTree(name, cmpresult)}]
        else:
            results += [{"result": cmpresult, "filename": f, "children": None}]

    return results


def CmpFile(filename, file1, meta1, file2, meta2, IgnoreMeta=True,
            CompareDataLater=False):
    """Returns the result of comparing two files."""

    if meta1["size"] != meta2["size"] or meta1["type"] != meta2["type"]:
        return {"result": CmpResult.File_Different, "filename": filename,
                "children": None}
    if not meta1["isdir"] and not meta1["isfile"]:
        return {"result": CmpResult.File_Incomprable, "filename": filename,
                "children": None}
    if CompareDataLater:
        return {"result": CmpResult.File_Await_Comparison,
                "filename": filename,
                "children": None}
    if not COMPAREFILES(file1, file2, shallow=False):
        return {"result": CmpResult.File_Different, "filename": filename,
                "children": None}
    if IgnoreMeta or meta1 == meta2:
        return {"result": CmpResult.File_Identical, "filename": filename,
                "children": None}
    return {"result": CmpResult.File_Meta_Different, "filename": filename,
            "children": None}


def DirCmp(dirpath1, dirpath2, recursive=True, IgnoreMeta=True,
           CompareDataLater=False):
    """Compares the contents of the two specified directories
    """

    results = []

    dircontents1 = os.listdir(dirpath1)
    dircontents2 = os.listdir(dirpath2)

    for f in dircontents1:
        file1 = os.path.join(dirpath1, f)
        stat1 = os.stat(file1, follow_symlinks=False)
        meta1 = ExtractMeta(stat1)

        # if in dir1 but not dir2
        if f not in dircontents2:
            # handle directory
            if meta1["isdir"] and recursive:
                results += [{"result": CmpResult.File_In_A_Only, "filename": f,
                             "children": GetUnilateralTree(
                                 file1, CmpResult.File_In_A_Only)}]
            # handle unique file
            else:
                results += [{"result": CmpResult.File_In_A_Only, "filename": f,
                             "children": None}]
            continue

        # remove from second directory list
        dircontents2.remove(f)

        # get details file 2
        file2 = os.path.join(dirpath2, f)
        stat2 = os.stat(file2, follow_symlinks=False)
        meta2 = ExtractMeta(stat2)

        if meta1["isfile"]:
            results += [CmpFile(f, file1, meta1, file2, meta2, IgnoreMeta,
                        CompareDataLater)]
            continue

        if meta1["isdir"]:
            # compare subdir
            subdircmpresult = DirCmp(file1, file2, recursive, IgnoreMeta,
                                     CompareDataLater)
            # get results for all children
            cmps = [x["result"] for x in subdircmpresult]
            # now work out what result for whole subdir is
            if CmpResult.File_Being_Compared in cmps:
                r = CmpResult.File_Being_Compared
            elif CmpResult.File_Await_Comparison in cmps:
                r = CmpResult.File_Await_Comparison
            elif (CmpResult.File_Different in cmps or
                  CmpResult.File_In_A_Only in cmps or
                  CmpResult.File_In_B_Only in cmps):
                r = CmpResult.File_Different
            elif CmpResult.File_Meta_Different in cmps:
                r = CmpResult.File_Meta_Different
            elif CmpResult.File_Incomprable in cmps:
                r = CmpResult.File_Incomprable
            else:
                r = CmpResult.File_Identical

            results += [{"result": r, "filename": f,
                         "children": subdircmpresult}]
            continue

        # file type incomprable
        results += [{"result": CmpResult.File_Incomprable, "filename": f,
                     "children": None}]

    # go through any entries only in directory2
    for f in dircontents2:
        file2 = os.path.join(dirpath2, f)
        # handle directory
        if os.path.isdir(file2) and recursive:
            results += [{"result": CmpResult.File_In_B_Only, "filename": f,
                         "children": GetUnilateralTree(
                             file2, CmpResult.File_In_B_Only)
                         }]
        # handle unique file
        else:
            results += [{"result": CmpResult.File_In_B_Only, "filename": f,
                         "children": None}]

    return results


class CmpResultItem(object):
    """Class to encapsulate a file/directory result as handled in a
    treeview."""

    def __init__(self, name, result, parent, children=None):
        self.name = name
        self.result = result
        self.parent = parent
        self.children = None
        if children:
            if type(children[0]) is dict:
                self.children = [CmpResultItem(c['filename'], c['result'],
                                 self, c['children']) for c in children]
            else:
                self.children = [CmpResultItem(c[0], c[1], self,
                                 None if len(c) is 2
                                     else c[2]) for c in children]

    def __repr__(self):
        return "<CmpResultItem name:{} result:{} children:{}>".format(
            self.name, self.result, self.children if self.children else "None")

    def isFolder(self):
        return False if self.children is None else True

    def child(self, row):
        return self.children[row]

    def childCount(self):
        return len(self.children) if self.children else 0

    def name(self):
        return self.name

    def parent(self):
        return self.parent

    def result(self):
        return self.result

    def row(self):
        if self.parent:
            return self.parent.children.index(self)
        return 0

    def localpath(self):
        ret = []
        if self.parent:
            p = self.parent
            while p.name:
                ret = [p.name] + ret
                p = p.parent
        return ret

    def sortByName(self, rev=False):
        if self.children:
            self.children = (sorted([c for c in self.children if c.isFolder()],
                                    key=lambda x: x.name, reverse=rev) +
                             sorted([c for c in self.children if
                                     not c.isFolder()],
                                    key=lambda x: x.name, reverse=rev))
            for c in self.children:
                c.sortByName()

    def sortByResult(self, rev=False):
        if self.children:
            self.children = sorted(self.children, key=lambda x: x.result,
                                   reverse=rev)
            for c in self.children:
                c.sortByResult()

    def iterateSelfAndChildren(self):
        if self.children:
            for child in self.children:
                yield from child.iterateSelfAndChildren()
        yield self

    def removeChild(self, position):
        if position < 0 or not self.children or position > len(self.children):
            return False
        child = self.children.pop(position)
        child.parent = None
        if child.children:
            for c in child.children:
                c.removeChild(0)
        return True

    def getNextAwaitingComparison(self):
        if self.children:
            for item in self.children:
                if item.result == CmpResult.File_Await_Comparison:
                    if item.isFolder():
                        yield from item.getNextAwaitingComparison()
                    else:
                        item.result = CmpResult.File_Being_Compared
                        yield item.localpath() + [item.name]
            path = self.localpath() + [self.name]
            if path[0]:
                yield self.localpath() + [self.name]

    def getChildByName(self, name):
        if self.children:
            for c in self.children:
                if c.name == name:
                    return c
        return None

    def ResultUpdatePossiblyNeeded(self, c):
        cmps = [x.result for x in self.children]
        # now work out what result for whole subdir is
        if(CmpResult.File_Different in cmps or
           CmpResult.File_In_A_Only in cmps or
           CmpResult.File_In_B_Only in cmps):
            if self.result != CmpResult.File_Different and self.parent:
                self.result = CmpResult.File_Different
                return self.parent.ResultUpdatePossiblyNeeded(self)
            return self
        elif CmpResult.File_Meta_Different in cmps:
            if self.result != CmpResult.File_Meta_Different and self.parent:
                self.result = CmpResult.File_Meta_Different
                return self.parent.ResultUpdatePossiblyNeeded(self)
            return self
        elif CmpResult.File_Incomprable in cmps:
            if self.result != CmpResult.File_Incomprable and self.parent:
                self.result = CmpResult.File_Incomprable
                return self.parent.ResultUpdatePossiblyNeeded(self)
            return self
        elif CmpResult.File_Being_Compared in cmps:
            if self.result != CmpResult.File_Being_Compared and self.parent:
                self.result = CmpResult.File_Being_Compared
                return self.parent.ResultUpdatePossiblyNeeded(self)
            return self
        elif CmpResult.File_Await_Comparison in cmps:
            if self.result != CmpResult.File_Await_Comparison and self.parent:
                self.result = CmpResult.File_Await_Comparison
                return self.parent.ResultUpdatePossiblyNeeded(self)
            return self
        else:
            if self.result != CmpResult.File_Identical and self.parent:
                self.result = CmpResult.File_Identical
                return self.parent.ResultUpdatePossiblyNeeded(self)
            return self
        return c


class CmpResultEncoder(json.JSONEncoder):
    """A JSONEncoder to encode CmpResultItems."""

    def default(self, obj):
        if isinstance(obj, CmpResultItem):
            if obj.children:
                return [obj.name, obj.result, obj.children]
            return [obj.name, obj.result]
        if isinstance(obj, CmpResultModel):
            return {"CmpResultModel_dirA": obj.dira,
                    "CmpResultModel_dirB": obj.dirb,
                    "IgnoreMeta": obj.IgnoreMeta,
                    "root": obj.rootItem}
        return super(RoundTripEncoder, self).default(obj)


class CmpResultDecoder(json.JSONDecoder):
    """A JSONDecoder to decode CmpResultItems."""

    def __init__(self, *args, **kwargs):
        json.JSONDecoder.__init__(self, object_hook=self.object_hook, *args,
                                  **kwargs)

    def object_hook(self, obj):
        if('CmpResultModel_dirA' not in obj or
           "CmpResultModel_dirB" not in obj):
            return obj
        cmpRM = CmpResultModel(None, obj['CmpResultModel_dirA'],
                               obj['CmpResultModel_dirB'], obj['IgnoreMeta'])
        r = obj['root'][1]
        cdata = None if len(obj['root']) is 2 else obj['root'][2]
        cmpRM.rootItem = CmpResultItem(None, r, None, cdata)
        return cmpRM


class CmpThread(threading.Thread):
    """A thread to handle comparing files in the background.
    this allows the main program to handle the user interface while
    comparison of many or large files can happen in the background."""

    def __init__(self, resultmodel):
        threading.Thread.__init__(self)
        self.resultmodel = resultmodel
        self.NeedToStop = False

    def StopThread(self):
        self.NeedToStop = True

    def run(self):
        # itterate files to compare
        for f in self.resultmodel.getNextAwaitingComparison():
            if self.NeedToStop:
                break

            # compare files
            file1 = os.path.join(self.resultmodel.dira, *f)
            file2 = os.path.join(self.resultmodel.dirb, *f)
            meta1 = ExtractMeta(os.stat(file1, follow_symlinks=False))
            meta1 = ExtractMeta(os.stat(file2, follow_symlinks=False))

            if os.path.isdir(file1):
                i = self.resultmodel.rootItem
                for p in f:
                    i = i.getChildByName(p)

                cmps = [x.result for x in i.children]
                # now work out what result for whole subdir is
                if CmpResult.File_Being_Compared in cmps:
                    result = CmpResult.File_Being_Compared
                elif CmpResult.File_Await_Comparison in cmps:
                    result = CmpResult.File_Await_Comparison
                elif (CmpResult.File_Different in cmps or
                      CmpResult.File_In_A_Only in cmps or
                      CmpResult.File_In_B_Only in cmps):
                    result = CmpResult.File_Different
                elif CmpResult.File_Meta_Different in cmps:
                    result = CmpResult.File_Meta_Different
                elif CmpResult.File_Incomprable in cmps:
                    result = CmpResult.File_Incomprable
                else:
                    result = CmpResult.File_Identical

            elif not COMPAREFILES(file1, file2, shallow=False):
                result = CmpResult.File_Different
            elif self.resultmodel.IgnoreMeta or meta1 == meta2:
                result = CmpResult.File_Identical
            else:
                result = CmpResult.File_Meta_Different

            if self.NeedToStop:
                break

            # send result to model
            self.resultmodel.UpdateResult(f, result)
        # remove reference to this thread before terminating
        self.resultmodel.ComparisonThread = None


class CmpResultModel(QAbstractItemModel):
    """An ItemModel for treeview that handles results data for the view."""

    def __init__(self, data, ResultNotificationSignal=None, dira=None,
                 dirb=None, IgnoreMeta=True):
        super(CmpResultModel, self).__init__()
        self.dira = dira
        self.dirb = dirb
        self.IgnoreMeta = IgnoreMeta
        self.rootItem = CmpResultItem(None, None, None, data)
        self.ResultNotificationSignal = ResultNotificationSignal
        self.ComparisonThread = None

        if(data and
           CmpResult.File_Await_Comparison in [x['result'] for x in data]):
            self.ComparisonThread = CmpThread(self)
            self.ComparisonThread.setDaemon(True)
            self.ComparisonThread.start()

    def __del__(self):
        if self.ComparisonThread:
            self.ComparisonThread.StopThread()

    def DoAwaitedComparisions(self):
        # end comparison if ongoing as may be past where data has
        # changed and needs restarting
        if self.ComparisonThread:
            self.ComparisonThread.StopThread()
            # wait for it to finish
            self.ComparisonThread.join()

        self.ComparisonThread = CmpThread(self)
        self.ComparisonThread.setDaemon(True)
        self.ComparisonThread.start()

    def getNextAwaitingComparison(self):
        return self.rootItem.getNextAwaitingComparison()

    def UpdateResult(self, filenameparts, result):
        i = self.rootItem
        for p in filenameparts:
            i = i.getChildByName(p)
        i.result = result

        top = i.parent.ResultUpdatePossiblyNeeded(i)

        if top is self.rootItem:
            index1 = self.createIndex(0, 0, top.parent)
        else:
            index1 = self.createIndex(top.parent.children.index(top), 0,
                                      top.parent)

        index2 = self.createIndex(i.parent.children.index(i), 1, i.parent)
        self.dataChanged.emit(index1, index2)

        if self.ResultNotificationSignal:
            self.ResultNotificationSignal.emit(i)
            if top != i:
                self.ResultNotificationSignal.emit(top)

    def StopComparison(self):
        if self.ComparisonThread:
            self.ComparisonThread.StopThread()
            self.ComparisonThread.join()
            self.ComparisonThread = None

    def rowCount(self, parent=QModelIndex()):
        if not parent.isValid():
            p_Item = self.rootItem
        else:
            p_Item = parent.internalPointer()
        return p_Item.childCount()

    def columnCount(self, parent):
        return 2

    def data(self, index, role=Qt.DisplayRole):
        if not index.isValid():
            return QVariant()

        i = index.internalPointer()
        column = index.column()
        if role == Qt.DisplayRole:
            if column == 0:
                return QVariant(i.name)
            elif column == 1:
                return QVariant(CmpResult.Description[i.result])
            return QVariant(QString("{}={}".format(
                i.name, CmpResult.Description[i.result])))

        elif role == Qt.BackgroundColorRole:
            return QVariant(QColor(CmpResult.RGBValues[i.result]))

        elif role == Qt.DecorationRole and column == 0 and i.isFolder():
            return QIcon.fromTheme('folder')

        return QVariant()

    def headerData(self, section, orientation, role=Qt.DisplayRole):
        if role == Qt.TextAlignmentRole:
            return QVariant(int(Qt.AlignLeft | Qt.AlignVCenter))
        if role != Qt.DisplayRole:
            return QVariant()
        if orientation == Qt.Horizontal:
            if section == 0:
                return QVariant("")
            elif section == 1:
                return QVariant("Result")
            return QVariant(int(section + 1))

        return QVariant()

    def index(self, row, column, parent):
        if not self.hasIndex(row, column, parent):
            return QModelIndex()

        if not parent.isValid():
            parentItem = self.rootItem
        else:
            parentItem = parent.internalPointer()

        childItem = parentItem.children[row]
        if childItem:
            return self.createIndex(row, column, childItem)
        else:
            return QModelIndex()

    def parent(self, index):
        if not index.isValid():
            return QModelIndex()

        childItem = index.internalPointer()
        if not childItem:
            return QModelIndex()

        parentItem = childItem.parent

        if parentItem is self.rootItem or not parentItem:
            return QModelIndex()

        return self.createIndex(parentItem.row(), 0, parentItem)

    def removeRow(self, row, parent):
        if not parent.isValid():
            parentNode = self.rootItem
        else:
            parentNode = parent.internalPointer()

        parentNode.removeChild(row)
        return True

    def sort(self, col, order):
        self.layoutAboutToBeChanged.emit()
        if col == 0:
            self.rootItem.sortByName(rev=(order == Qt.DescendingOrder))
        elif col == 1:
            self.rootItem.sortByResult(rev=(order == Qt.DescendingOrder))
        self.layoutChanged.emit()


class DirCmpGUI(QtGui.QMainWindow):
    """Main window that compares, displays, and edits directory comparisons."""

    resultUpdateSignal = pyqtSignal(CmpResultItem)

    def __init__(self, dira="", dirb=""):
        super(DirCmpGUI, self).__init__()

        self.setWindowTitle("DirCmp")

        self.lastDisplayedResult = None

        lay = QtGui.QVBoxLayout()

        # setup directory A UI widgets
        lay2 = QtGui.QHBoxLayout()
        lay2.addWidget(QtGui.QLabel("Directory A:"))
        self.ledira = QtGui.QLineEdit(self)
        self.ledira.setText(dira)
        self.ledira.sizePolicy().setHorizontalPolicy(
            QtGui.QSizePolicy.Expanding)
        self.ledira.sizePolicy().setHorizontalStretch(1)
        lay2.addWidget(self.ledira)
        bdira = QtGui.QPushButton("Browse", self)
        bdira.clicked.connect(self.handleBrowseA)
        lay2.addWidget(bdira)

        lay.addLayout(lay2)

        # setup directory B UI widgets
        lay2 = QtGui.QHBoxLayout()
        lay2.addWidget(QtGui.QLabel("Directory B:"))
        self.ledirb = QtGui.QLineEdit(self)
        self.ledirb.setText(dirb)
        self.ledirb.sizePolicy().setHorizontalPolicy(
            QtGui.QSizePolicy.Expanding)
        self.ledirb.sizePolicy().setHorizontalStretch(1)
        lay2.addWidget(self.ledirb)
        bdirb = QtGui.QPushButton("Browse", self)
        bdirb.clicked.connect(self.handleBrowseB)
        lay2.addWidget(bdirb)

        lay.addLayout(lay2)

        # set up compare button UI
        lay2 = QtGui.QHBoxLayout()
        lay2.addStretch(1)
        bcmp = QtGui.QPushButton("Compare", self)
        lay2.addWidget(bcmp)
        lay2.addStretch(1)
        bcmp.clicked.connect(self.CompareButtonSelected)
        lay.addLayout(lay2)

        lay2 = QtGui.QHBoxLayout()

        # set up file view UI widget
        cmpview = QtGui.QTreeView()
        self.cmpview = cmpview
        cmpview.setSortingEnabled(True)
        cmpview.myfont = QFont('monospace', 10)
        cmpview.setFont(cmpview.myfont)
        cmpview.setSelectionBehavior(QtGui.QAbstractItemView.SelectRows)
        cmpview.setModel(CmpResultModel(None, None))
        cmpview.setUniformRowHeights(True)
        cmpview.resizeColumnToContents(0)
        cmpview.selectionModel().currentChanged.connect(self.CmpEntryChanged)
        cmpview.sortByColumn(0, Qt.AscendingOrder)
        cmpview.setExpandsOnDoubleClick(False)

        cmpview.sizePolicy().setHorizontalPolicy(
            QtGui.QSizePolicy.Expanding)
        cmpview.sizePolicy().setHorizontalStretch(1)

        lay2.addWidget(cmpview)

        lay3 = QtGui.QVBoxLayout()

        # set up result display UI widgets
        grid = QtGui.QGridLayout()
        grid.setSpacing(10)

        self.resultUI = []

        self.resultUI.append(QtGui.QLabel('<h1>Results:</h1>'))
        self.resultUI[0].setAlignment(Qt.AlignHCenter | Qt.AlignVCenter)
        grid.addWidget(self.resultUI[0], 0, 0, 1, 3)
        l = QtGui.QLabel('Directory A')
        l.setAlignment(Qt.AlignRight | Qt.AlignVCenter)
        grid.addWidget(l, 1, 0, 1, 1)
        grid.addWidget(QtGui.QLabel('Directory B'), 1, 2, 1, 1)
        for y in range(1, 9):
            self.resultUI.append([QtGui.QLabel('-') for i in range(0, 3)])
            for x in range(0, 3):
                grid.addWidget(self.resultUI[y][x], y+1, x)
            self.resultUI[y][0].setAlignment(Qt.AlignRight | Qt.AlignVCenter)
            self.resultUI[y][1].setAlignment(Qt.AlignHCenter | Qt.AlignVCenter)

        for x in [0, 2]:
            self.resultUI[1][x].setWordWrap(True)

        lay3.addLayout(grid)

        self.cmpFiles = QtGui.QPushButton("Compare Selected files", self)
        self.cmpFiles.clicked.connect(self.handleCmpSelectedFiles)
        lay3.addWidget(self.cmpFiles)

        lay3.addStretch(1)

        # add action buttons
        self.bAtoB = QtGui.QPushButton("Copy A to B", self)
        self.bAtoB.clicked.connect(self.handleCopyAtoB)
        lay3.addWidget(self.bAtoB, alignment=Qt.AlignRight)
        self.bBtoA = QtGui.QPushButton("Copy B to A", self)
        self.bBtoA.clicked.connect(self.handleCopyBtoA)
        lay3.addWidget(self.bBtoA, alignment=Qt.AlignRight)
        self.bDelA = QtGui.QPushButton("Delete from A", self)
        self.bDelA.clicked.connect(self.handleDelA)
        lay3.addWidget(self.bDelA, alignment=Qt.AlignRight)
        self.bDelB = QtGui.QPushButton("Delete from B", self)
        self.bDelB.clicked.connect(self.handleDelB)
        lay3.addWidget(self.bDelB, alignment=Qt.AlignRight)
        self.SetActionButtons()

        self.SetCmpDetails()

        lay2.addLayout(lay3)

        lay.addLayout(lay2)

        centralWidget = QtGui.QWidget()
        centralWidget.setLayout(lay)
        self.setCentralWidget(centralWidget)

        menubar = self.menuBar()
        fileMenu = menubar.addMenu('&File')

        loadButton = QtGui.QAction('Load Previous Comparison', self)
        loadButton.setShortcut('Ctrl+L')
        loadButton.setStatusTip('Load Previous Comparison from file')
        loadButton.triggered.connect(self.LoadPreviousComparison)
        fileMenu.addAction(loadButton)

        saveButton = QtGui.QAction('Save Comparison', self)
        saveButton.setShortcut('Ctrl+S')
        saveButton.setStatusTip('Save Comparison to file.')
        saveButton.triggered.connect(self.SavePreviousComparison)
        fileMenu.addAction(saveButton)

        fileMenu.addSeparator()

        exitButton = QtGui.QAction('Exit', self)
        exitButton.setShortcut('Ctrl+Q')
        exitButton.setStatusTip('Exit application')
        exitButton.triggered.connect(self.close)
        fileMenu.addAction(exitButton)

        editMenu = menubar.addMenu('&Edit')

        editutilButton = QtGui.QAction('Edit Compare Utilities', self)
        editutilButton.setShortcut('Ctrl+U')
        editutilButton.setStatusTip(
            'Edit the programs that compare files and directories.')
        editutilButton.triggered.connect(self.EditCmpUtils)
        editMenu.addAction(editutilButton)

        self.setGeometry(0, 0, 900, 600)
        self.show()
        # self.showMaximized()

        self.resultUpdateSignal.connect(self.handleResultUpdateSignal)

    def EditCmpUtils(self):
        CmpUtilManager.EditCmpUtils()

    def handleCmpSelectedFiles(self):
        si = self.cmpview.selectedIndexes()
        if not si or len(si) == 0:
            return
        i = si[0].internalPointer()

        s = QSettings("DirCmp")
        cmputil = None
        if i.isFolder():
            cmputil = CmpUtil.GetDefaultForFolders(s)
        else:
            cmputil = CmpUtil.GetDefaultForFiles(s)

        if not cmputil:
            cmputil = CmpUtilManager.SelectFileCmpUtil(not i.isFolder(),
                                                       i.isFolder())

        if not cmputil:
            return

        dira = self.cmpview.model().dira
        dirb = self.cmpview.model().dirb

        process = Popen([cmputil.exe,
                         os.path.join(dira, *i.localpath(), i.name),
                         os.path.join(dirb, *i.localpath(), i.name)],
                        stdout=PIPE)
        (output, err) = process.communicate()
        exit_code = process.wait()

        if cmputil.displayoutput:
            d = QtGui.QDialog()
            d.setWindowTitle("Compare results:")
            d.setModal(True)

            lay = QtGui.QVBoxLayout()
            te = QtGui.QPlainTextEdit(output.decode("utf8"), self)
            te.setReadOnly(True)
            lay.addWidget(te)

            lay2 = QtGui.QHBoxLayout()
            lay2.addStretch(1)
            bok = QtGui.QPushButton("Ok")
            bok.clicked.connect(d.accept)
            lay2.addWidget(bok)
            lay.addLayout(lay2)

            d.setLayout(lay)

            d.exec_()

        while i.parent:
            i.result = CmpResult.File_Await_Comparison
            i = i.parent

        self.cmpview.model().DoAwaitedComparisions()

    def copyFile(self, fFrom, fTo):
        path = os.path.dirname(fTo)
        if not os.path.exists(path):
            os.makedirs(path, exist_ok=True)
        shutil.copy2(fFrom, fTo)

    def handleCopyAtoB(self):
        si = self.cmpview.selectedIndexes()
        if not si or len(si) == 0:
            return

        dira = self.cmpview.model().dira
        dirb = self.cmpview.model().dirb

        i = si[0].internalPointer()
        question = ('''Are you sure you want to copy "{}"{} from directory \
A("{}") to directory B("{}")?'''
                    .format(i.name,
                            "(including children)" if i.children else "",
                            os.path.join(dira, *i.localpath()),
                            os.path.join(dirb, *i.localpath())))
        reply = QtGui.QMessageBox.question(self, "Are you Sure?", question,
                                           (QtGui.QMessageBox.Yes |
                                            QtGui.QMessageBox.No),
                                           QtGui.QMessageBox.No)
        if reply == QtGui.QMessageBox.No:
            return

        for j in i.iterateSelfAndChildren():
            fTo = os.path.join(dirb, *j.localpath(), j.name)
            if j.isFolder():
                if not os.path.exists(fTo):
                    os.makedirs(fTo, exist_ok=True)
            else:
                self.copyFile(os.path.join(dira, *j.localpath(), j.name), fTo)

            while j.parent:
                j.result = CmpResult.File_Await_Comparison
                j = j.parent

        self.cmpview.model().DoAwaitedComparisions()

    def handleCopyBtoA(self):
        si = self.cmpview.selectedIndexes()
        if not si or len(si) == 0:
            return

        dira = self.cmpview.model().dira
        dirb = self.cmpview.model().dirb

        i = si[0].internalPointer()
        question = '''Are you sure you want to copy "{}"{} from directory \
B("{}") to directory A("{}")?'''.format(i.name,
                                        ("(including children)" if i.children
                                         else ""),
                                        os.path.join(dirb, *i.localpath()),
                                        os.path.join(dira, *i.localpath()))
        reply = QtGui.QMessageBox.question(self, "Are you Sure?", question,
                                           (QtGui.QMessageBox.Yes |
                                            QtGui.QMessageBox.No),
                                           QtGui.QMessageBox.No)
        if reply == QtGui.QMessageBox.No:
            return

        for j in i.iterateSelfAndChildren():
            fTo = os.path.join(dira, *j.localpath(), j.name)
            if j.isFolder():
                if not os.path.exists(fTo):
                    os.makedirs(fTo, exist_ok=True)
            else:
                self.copyFile(os.path.join(dirb, *j.localpath(), j.name),
                              fTo)

            while j.parent:
                j.result = CmpResult.File_Await_Comparison
                j = j.parent

        self.cmpview.model().DoAwaitedComparisions()

    def handleDel(self, delA):
        si = self.cmpview.selectedIndexes()
        if not si or len(si) == 0:
            return
        i = si[0].internalPointer()

        dira = self.cmpview.model().dira
        dirb = self.cmpview.model().dirb

        # are we sure we want to delete?
        question = '''Are you sure you want to delete "{}"{} from directory \
{}("{}")?'''.format(i.name, "(including children)" if i.children else "",
                    "A" if delA else "B",
                    os.path.join(dira if delA else dirb, *i.localpath()))
        reply = QtGui.QMessageBox.question(self, "Are you Sure?", question,
                                           (QtGui.QMessageBox.Yes |
                                            QtGui.QMessageBox.No),
                                           QtGui.QMessageBox.No)
        if reply == QtGui.QMessageBox.No:
            return

        # removes file from comparison GUI, removing reference that
        # might try to be used later
        self.SetCmpDetails(None)

        # remember filename and parent for later, i will not be valid by
        # time have to physically remove them
        f = os.path.join(dira if delA else dirb, *i.localpath(), i.name)
        p = i.parent

        # remove from table if needed
        if i.result is (CmpResult.File_In_A_Only if delA
                        else CmpResult.File_In_B_Only):
            self.cmpview.model().beginRemoveRows(si[0].parent(), si[0].row(),
                                                 si[0].row())
            success = self.cmpview.model().removeRow(si[0].row(),
                                                     parent=si[0].parent())
            self.cmpview.model().endRemoveRows()
        else:
            for item in i.iterateSelfAndChildren():
                item.result = (CmpResult.File_In_B_Only if delA
                               else CmpResult.File_In_A_Only)

        # remove file/directory
        if os.path.isdir(f):
            shutil.rmtree(f)
        else:
            os.remove(f)

        # parent's status may have changed
        # get results for all children
        cmps = [x.result for x in p.children]
        # now work out what result for whole subdir is
        if CmpResult.File_Being_Compared in cmps:
            result = CmpResult.File_Being_Compared
        elif CmpResult.File_Await_Comparison in cmps:
            result = CmpResult.File_Await_Comparison
        elif (CmpResult.File_Different in cmps or
              CmpResult.File_In_A_Only in cmps or
              CmpResult.File_In_B_Only in cmps):
            result = CmpResult.File_Different
        elif CmpResult.File_Meta_Different in cmps:
            result = CmpResult.File_Meta_Different
        elif CmpResult.File_Incomprable in cmps:
            result = CmpResult.File_Incomprable
        else:
            result = CmpResult.File_Identical

        # possible to be left with one empty directory
        existsa = os.path.exists(os.path.join(self.cmpview.model().dira,
                                              *p.localpath(), p.name))
        existsb = os.path.exists(os.path.join(self.cmpview.model().dirb,
                                              *p.localpath(), p.name))
        if(p.isFolder() and p != self.cmpview.model().rootItem and
           existsa != existsb):
            result = (CmpResult.File_In_A_Only if existsa else CmpResult
                      .File_In_B_Only)

        self.cmpview.model().UpdateResult(p.localpath() + [p.name], result)

    def handleDelA(self):
        self.handleDel(True)

    def handleDelB(self):
        self.handleDel(False)

    def formattime(self, t):
        return datetime.fromtimestamp(t).strftime("%Y-%m-%d %H:%M:%S")

    def SetActionButtons(self, cmpresult=None):
        if (not cmpresult or
            cmpresult.result in [CmpResult.File_Identical,
                                 CmpResult.File_Await_Comparison,
                                 CmpResult.File_Being_Compared]):
            for button in [self.cmpFiles, self.bAtoB, self.bBtoA, self.bDelA,
                           self.bDelB]:
                button.setVisible(False)
                button.setDisabled(True)
            return

        for button in [self.cmpFiles, self.bAtoB, self.bBtoA, self.bDelA,
                       self.bDelB]:
            button.setVisible(True)
            button.setDisabled(False)

        if cmpresult.result == CmpResult.File_In_A_Only:
            self.bBtoA.setVisible(False)
            self.bBtoA.setDisabled(True)
            self.bDelB.setVisible(False)
            self.bDelB.setDisabled(True)

        if cmpresult.result == CmpResult.File_In_B_Only:
            self.bAtoB.setVisible(False)
            self.bAtoB.setDisabled(True)
            self.bDelA.setVisible(False)
            self.bDelA.setDisabled(True)

    def SetCmpDetails(self, cmpresult=None):
        self.lastDisplayedResult = cmpresult

        self.SetActionButtons(cmpresult)

        if cmpresult is None:
            self.resultUI[0].setText('<h1>Results:</h1>')
            for y in range(1, 9):
                self.resultUI[y][0].setText('-')
                self.resultUI[y][1].setText(['', '<b>file</b>', '<b>size</b>',
                                             '<b>last access</b>',
                                             '<b>last change</b>',
                                             '<b>last modified</b>',
                                             '<b>access</b>', '<b>user id</b>',
                                             '<b>group id</b>'][y])
                self.resultUI[y][2].setText('-')
            return

        filea = os.path.join(self.cmpview.model().dira,
                             *cmpresult.localpath(), cmpresult.name)
        fileb = os.path.join(self.cmpview.model().dirb,
                             *cmpresult.localpath(), cmpresult.name)
        stata = os.stat(filea, follow_symlinks=False) if os.path.exists(
            filea) else None
        statb = os.stat(fileb, follow_symlinks=False) if os.path.exists(
            fileb) else None
        metaa = ExtractMeta(stata) if stata else None
        metab = ExtractMeta(statb) if statb else None

        def setColor(ui, a, b, x):
            if a is None and b is None:
                return
            if a is not None and b is not None and a[x] == b[x]:
                return
            ui[0].setText('<font color="#FF4040">'+ui[0].text()+'</font>')
            ui[2].setText('<font color="#FF4040">'+ui[2].text()+'</font>')

        def setTime(ui, a, b, x):
            ui[0].setText(self.formattime(a[x]) if a else "")
            ui[2].setText(self.formattime(b[x]) if b else "")

        if cmpresult.result == CmpResult.File_Identical:
            resulttext = '<h1>Results: {}</h1>'
        else:
            resulttext = ('<h1>Results: <font color="#{:06X}">{{}}</font></h1>'
                          .format(CmpResult.RGBValues[cmpresult.result]))
        self.resultUI[0].setText(
            resulttext.format(CmpResult.Description[cmpresult.result]))
        self.resultUI[1][0].setText(filea)
        self.resultUI[1][2].setText(fileb)
        self.resultUI[2][0].setText("{:,}".format(
            metaa['size']) if metaa else "Does not exist")
        self.resultUI[2][2].setText("{:,}".format(
            metab['size']) if metab else "Does not exist")
        setColor(self.resultUI[2], metaa, metab, 'size')
        setTime(self.resultUI[3], metaa, metab, 'last_access')
        setColor(self.resultUI[3], metaa, metab, 'last_access')
        setTime(self.resultUI[4], metaa, metab, 'last_change')
        setColor(self.resultUI[4], metaa, metab, 'last_change')
        setTime(self.resultUI[5], metaa, metab, 'last_modify')
        setColor(self.resultUI[5], metaa, metab, 'last_modify')
        self.resultUI[6][0].setText(metaa['access'][:3] + " " +
                                    metaa['access'][3:6] + " " +
                                    metaa['access'][6:] if metaa else "")
        self.resultUI[6][2].setText(metab['access'][:3] + " " +
                                    metab['access'][3:6] + " " +
                                    metab['access'][6:] if metab else "")
        setColor(self.resultUI[6], metaa, metab, 'access')
        self.resultUI[7][0].setText(str(metaa['user_id']) if metaa else "")
        self.resultUI[7][2].setText(str(metab['user_id']) if metab else "")
        setColor(self.resultUI[7], metaa, metab, 'user_id')
        self.resultUI[8][0].setText(str(metaa['group_id']) if metaa else "")
        self.resultUI[8][2].setText(str(metab['group_id']) if metab else "")
        setColor(self.resultUI[8], metaa, metab, 'group_id')

    def CmpEntryChanged(self, selected, deselected):
        if selected.isEmpty() or selected.count() == 0:
            return
        i = selected.indexes()[0].internalPointer()
        self.SetCmpDetails(i)

    def handleBrowseA(self):
        newdir = QtGui.QFileDialog.getExistingDirectory(
            self, "Select Directory A", self.ledira.text(),
            QtGui.QFileDialog.ShowDirsOnly)
        if newdir != "":
            self.ledira.setText(newdir)

    def handleBrowseB(self):
        newdir = QtGui.QFileDialog.getExistingDirectory(
            self, "Select Directory B", self.ledirb.text(),
            QtGui.QFileDialog.ShowDirsOnly)
        if newdir != "":
            self.ledirb.setText(newdir)

    def LoadPreviousComparison(self):
        f = QtGui.QFileDialog.getOpenFileName(self, 'Open file')[0]
        if f is '':
            return
        with open(f) as json_file:
            newmodel = json.load(json_file, cls=CmpResultDecoder)
            self.cmpview.setModel(newmodel)
            self.cmpview.resizeColumnToContents(0)
            self.cmpview.sortByColumn(0, Qt.AscendingOrder)

    def SavePreviousComparison(self):
        f = QtGui.QFileDialog.getSaveFileName(self, 'Save file')[0]
        if f is '':
            return
        with open(f, 'w') as outfile:
            json.dump(self.cmpview.model(), outfile, cls=CmpResultEncoder)

    def CompareButtonSelected(self):
        x = DirCmp(self.ledira.text(), self.ledirb.text(),
                   CompareDataLater=True)
        newmodel = CmpResultModel(x, self.resultUpdateSignal,
                                  self.ledira.text(), self.ledirb.text())
        self.cmpview.setModel(newmodel)
        self.cmpview.resizeColumnToContents(0)
        self.cmpview.sortByColumn(0, Qt.AscendingOrder)
        self.cmpview.selectionModel().selectionChanged.connect(
            self.CmpEntryChanged)

    def handleResultUpdateSignal(self, item):
        if item and self.lastDisplayedResult == item:
            self.SetCmpDetails(item)


class ComboBoxEditor(QtGui.QComboBox):
    """Barebones editor for ComboboxItemDelegate."""

    def __init__(self, parent, delegate, row):
        super(ComboBoxEditor, self).__init__(parent)
        self.delegate = delegate
        self.row = row

    def hidePopup(self):
        super(ComboBoxEditor, self).hidePopup()
        self.delegate.closeEditor.emit(self)


class ComboBoxItemDelegate(QtGui.QItemDelegate):
    """QItemDelegate to handle peculiarities of default application
       item list."""

    def __init__(self, parent):
        super(QtGui.QItemDelegate, self).__init__(parent)
        self.tablewidget = parent

    def createEditor(self, parent, option, index):
        e = ComboBoxEditor(parent, self, index.row())
        e.addItems(ComboBoxItemDelegate.options)
        e.setCurrentText(index.data())
        e.activated.connect(self.handleItemSelect)
        e.setGeometry(option.rect)
        e.showPopup()
        return e

    def handleItemSelect(self, selected):
        forfiles = selected & 1 == 1
        forfolders = selected & 2 == 2
        row = self.sender().row

        r = 0
        for u in self.tablewidget.cmputils:
            if forfiles:
                u.defaultforfiles = False
            if forfolders:
                u.defaultforfolders = False
            self.tablewidget.item(r, 5).setText(ComboBoxItemDelegate
                                                .getoption(u))
            r += 1

        self.tablewidget.cmputils[row].defaultforfiles = forfiles
        self.tablewidget.cmputils[row].defaultforfolders = forfolders

        self.tablewidget.item(row, 5).setText(ComboBoxItemDelegate
                                              .options[selected])
        self.closeEditor.emit(self.sender())

    options = ["", "Files", "Folders", "Files & Folders"]

    def getoption(cu):
        return (ComboBoxItemDelegate
                .options[(1 if cu.defaultforfiles else 0) +
                         (2 if cu.defaultforfolders else 0)])


class CmpUtilManager(QtGui.QDialog):
    """Dialog that allows the user to edit comparison programs, or
       select them."""

    def __init__(self, role, forfiles=False, forfolders=False):
        super(QtGui.QDialog, self).__init__()
        self.createUI(role, forfiles, forfolders)
        self.selectedutil = None

    def adjustrowflags(table, row):
        exists = os.path.exists(table.cmputils[row].exe)
        enabled = (table.item(row, 0).flags()
                   .__and__(Qt.ItemIsSelectable) == Qt.ItemIsSelectable)
        if exists == enabled:
            return

        for c in range(0, 6):
            x = table.item(row, c)
            if exists:
                x.setFlags(x.flags().__or__(Qt.ItemIsSelectable))
                x.setBackground(x.defaultbackground)
                x.setToolTip(None)
            else:
                x.setFlags(x.flags().__and__(~Qt.ItemIsSelectable))
                x.setBackground(QBrush(Qt.lightGray))
                x.setToolTip("Not installed")

    def addTableRow(self, row, cmputil):
        x = QtGui.QTableWidgetItem(cmputil.name)
        x.defaultbackground = x.background()
        self.tableWidget.setItem(row, 0, x)
        x = QtGui.QTableWidgetItem(cmputil.exe)
        x.defaultbackground = x.background()
        self.tableWidget.setItem(row, 1, x)
        x = QtGui.QTableWidgetItem()
        x.defaultbackground = x.background()
        x.setCheckState(Qt.Checked if cmputil.displayoutput else Qt.Unchecked)
        x.setTextAlignment(Qt.AlignCenter)
        self.tableWidget.setItem(row, 2, x)
        x = QtGui.QTableWidgetItem()
        x.defaultbackground = x.background()
        x.setCheckState(Qt.Checked if cmputil.handlesfolders else Qt.Unchecked)
        x.setTextAlignment(Qt.AlignCenter)
        self.tableWidget.setItem(row, 3, x)
        x = QtGui.QTableWidgetItem()
        x.defaultbackground = x.background()
        x.setCheckState(Qt.Checked if cmputil.handlesfiles else Qt.Unchecked)
        x.setTextAlignment(Qt.AlignCenter)
        self.tableWidget.setItem(row, 4, x)
        x = QtGui.QTableWidgetItem(ComboBoxItemDelegate.getoption(cmputil))
        x.defaultbackground = x.background()
        self.tableWidget.setItem(row, 5, x)
        CmpUtilManager.adjustrowflags(self.tableWidget, row)

    def createUI(self, role, forfiles=False, forfolders=False):
        self.role = role
        if role == "Edit":
            self.setWindowTitle("Edit Programs to compare files and folders")
        else:
            self.setWindowTitle("Select Program to compare selected items")
        self.sizePolicy().setHorizontalPolicy(QtGui.QSizePolicy.Expanding)

        utils = CmpUtil.GetFromSettings(QSettings("DirCmp"))

        tableWidget = QtGui.QTableWidget(len(utils), 6, self)
        tableWidget.cmputils = utils
        self.tableWidget = tableWidget
        tableWidget.setSelectionBehavior(QtGui.QTableView.SelectRows)
        tableWidget.setSelectionMode(QtGui.QTableView.SingleSelection)
        tableWidget.setHorizontalHeaderLabels(["Name",
                                               "Exe path",
                                               "Display output",
                                               "Handles folders",
                                               "Handles Files",
                                               "Default for:"])
        selected = None

        for row in range(0, len(utils)):
            self.addTableRow(row, utils[row])

            if role == "Edit":
                continue

            utilexists = os.path.exists(utils[row].exe)
            if(utilexists and not selected and forfiles and
               utils[row].defaultforfiles):
                selected = row
            if(utilexists and not selected and forfolders and
               utils[row].defaultforfolders):
                selected = row

        tableWidget.setItemDelegateForColumn(5,
                                             ComboBoxItemDelegate(
                                                 tableWidget))
        tableWidget.cellChanged.connect(self.cellChanged)
        tableWidget.cellClicked.connect(self.handleCellClicked)
        self.lastRowClicked = selected

        tableWidget.setSizeAdjustPolicy(QtGui.QAbstractScrollArea
                                        .AdjustToContents)
        tableWidget.resizeColumnsToContents()
        if selected:
            tableWidget.selectRow(selected)

        lay = QtGui.QHBoxLayout()
        bnew = QtGui.QPushButton("New", self)
        bnew.clicked.connect(self.handleNew)
        lay.addWidget(bnew)
        bdelete = QtGui.QPushButton("Delete", self)
        bdelete.clicked.connect(self.handleDelete)
        lay.addWidget(bdelete)
        lay.addStretch(1)
        bcancel = QtGui.QPushButton("Cancel", self)
        bcancel.clicked.connect(self.handleCancel)
        lay.addWidget(bcancel)
        bok = QtGui.QPushButton("Save" if role == "Edit" else "Ok", self)
        bok.clicked.connect(self.handleOk)
        lay.addWidget(bok)

        self.layout = QtGui.QVBoxLayout()
        self.layout.addWidget(tableWidget)
        self.layout.addLayout(lay)
        self.setLayout(self.layout)

    def cellChanged(self, row, column):
        if column == 0:
            newname = self.tableWidget.item(row, 0).text()
            oldname = self.tableWidget.cmputils[row].name
            if newname == oldname:
                return
            if newname in [x.name for x in self.tableWidget.cmputils]:
                self.tableWidget.item(row, 0).setText(oldname)
            else:
                self.tableWidget.cmputils[row].name = newname
        if column == 1:
            newexe = self.tableWidget.item(row, 1).text()
            self.tableWidget.cmputils[row].exe = newexe
            CmpUtilManager.adjustrowflags(self.tableWidget, row)
            if os.path.exists(self.tableWidget.cmputils[row].exe):
                self.tableWidget.selectRow(row)
        if column == 2:
            (self.tableWidget.cmputils[row]
             .displayoutput) = (self.tableWidget.item(row, 2)
                                .checkState() == Qt.Checked)
        if column == 3:
            (self.tableWidget.cmputils[row]
             .handlesfolders) = (self.tableWidget.item(row, 3)
                                 .checkState() == Qt.Checked)
        if column == 4:
            (self.tableWidget.cmputils[row]
             .handlesfiles) = (self.tableWidget.item(row, 4)
                               .checkState() == Qt.Checked)

    def handleNew(self):
        row = len(self.tableWidget.cmputils)
        i = 1
        while True:
            name = "new {}".format(i)
            if name not in [x.name for x in self.tableWidget.cmputils]:
                break
            i += 1

        cmputil = CmpUtil(name, "/usr/bin/diff", True, True, True)
        self.tableWidget.cmputils += [cmputil]
        self.tableWidget.insertRow(row)
        self.addTableRow(row, cmputil)

    def handleCellClicked(self, r, c):
        self.lastRowClicked = r

    def handleDelete(self):
        if self.lastRowClicked:
            self.tableWidget.removeRow(self.lastRowClicked)
            del self.tableWidget.cmputils[self.lastRowClicked]
        self.lastRowClicked = None

    def handleCancel(self):
        self.done(0)

    def handleOk(self):
        if self.role == "Edit":
            CmpUtil.SaveToSettings(QSettings("DirCmp"),
                                   self.tableWidget.cmputils)
        else:
            x = self.tableWidget.selectedItems()
            self.selectedutil = (None if not x or len(x) == 0 else
                                 self.tableWidget.cmputils[x[0].row()])

        self.done(0)

    def EditCmpUtils():
        CmpUtilManager("Edit").exec_()

    def SelectFileCmpUtil(forfiles=False, forfolders=False):
        d = CmpUtilManager("Select", forfiles, forfolders)

        d.exec_()

        return d.selectedutil


if __name__ == '__main__':
    app = QtGui.QApplication(sys.argv)

    sftGUI = DirCmpGUI()

    sys.exit(app.exec_())
