#/*
#* Copyright 2025 Rochus Keller <mailto:me@rochus-keller.ch>
#*
#* This file is part of the Micron IDE application.
#*
#* The following is the license that applies to this copy of the
#* application. For a license to use the application under conditions
#* other than those described here, please email to me@rochus-keller.ch.
#*
#* GNU General Public License Usage
#* This file may be used under the terms of the GNU General Public
#* License (GPL) versions 2.0 or 3.0 as published by the Free Software
#* Foundation and appearing in the file LICENSE.GPL included in
#* the packaging of this file. Please review the following information
#* to ensure GNU General Public Licensing requirements will be met:
#* http://www.fsf.org/licensing/licenses/info/GPLv2.html and
#* http://www.gnu.org/copyleft/gpl.html.
#*/

QT       += core gui network widgets

TARGET = MicronIDE
TEMPLATE = app
CONFIG += HAVE_SCREEN

INCLUDEPATH += ..

#DEFINES += _MIC_IDE_USE_ELFLINKER_MUSL_

SOURCES +=  MicProject2.cpp \
    MicHighlighter.cpp \
    MicIde.cpp \
    ../GuiTools/CodeEditor.cpp \
    ../GuiTools/DocSelector.cpp \
    ../GuiTools/DocTabWidget.cpp

HEADERS  += MicProject2.h \
    MicHighlighter.h \
    MicIde.h \
    ../GuiTools/CodeEditor.h \
    ../GuiTools/DocSelector.h \
    ../GuiTools/DocTabWidget.h \
    Version.h

HAVE_SCREEN {
    DEFINES += _MIC_HAVE_SCREEN_
    HEADERS += oakwood/ScreenQt.h
    SOURCES += oakwood/ScreenQt.cpp
}

HAVE_LUAJIT {
    DEFINES += _MIC_HAVE_LUAJIT_
    INCLUDEPATH += ../LuaJIT-2.1/src
    HEADERS += \
        MilLjBcGen.h \
        ../LjTools/LuaJitComposer2.h \
        ../LjTools/LuaJitBytecode2.h \
        ../LjTools/LuaJitHelper.h \
        ../LjTools/Engine2.h
    SOURCES += \
        MilLjBcGen.cpp \
        ../LjTools/LuaJitComposer2.cpp \
        ../LjTools/LuaJitBytecode2.cpp \
        ../LjTools/LuaJitHelper.cpp \
        ../LjTools/Engine2.cpp
linux | macx {
    LIBS += $$absolute_path(../LuaJIT-2.1/src/libluajit.a, $$_PRO_FILE_PWD_)
    QMAKE_LFLAGS += -rdynamic -ldl
    #rdynamic is required so that the LjLibFfi functions are visible to LuaJIT FFI
}
win32 {
    LIBS += -L../LuaJIT-2.1/src -llua51
}
}

include( MicUtils.pri )
include( MicParser.pri )
include( MilParser2.pri )
include( ../GuiTools/Menu.pri )
include( ../LeanDap/LeanDapInt.pri )

CONFIG(debug, debug|release) {
        DEFINES += _DEBUG
}

!win32 {
    QMAKE_CXXFLAGS += -Wno-reorder -Wno-unused-parameter -Wno-unused-function -Wno-unused-variable -Wno-switch \
        -Wno-deprecated-declarations -Wno-sign-compare -Wno-parentheses -Wno-unused-parameter -Werror=return-type
}

RESOURCES += MicIde.qrc MilCompiler.qrc StaticLibs.qrc


