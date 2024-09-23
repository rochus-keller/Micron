QT       += core

QT       -= gui

TARGET = MicCompiler
CONFIG   += console
CONFIG   -= app_bundle

TEMPLATE = app

INCLUDEPATH += ..

DEFINES += _DEBUG
    
include(MicParser.pri)

SOURCES += \
    MicMain.cpp \
    MicEiGen.cpp \
    MicCilGen.cpp \
    MicMilInterpreter.cpp

HEADERS += \
    MicEiGen.h \
    MicCilGen.h \
    MicMilInterpreter.h




include( ../PeLib/PeLib.pri )
