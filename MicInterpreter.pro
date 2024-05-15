QT       += core

QT       -= gui

TARGET = MicInterpreter
CONFIG   += console
CONFIG   -= app_bundle
CONFIG += MIC_INTERPRETER

TEMPLATE = app

INCLUDEPATH += ..

DEFINES += _DEBUG


    
include(MicParser.pri)
include(MilParser.pri)

SOURCES += \
    MicInterpreter.cpp \
    MilInterpreter.cpp

HEADERS += \
    MilInterpreter.h



