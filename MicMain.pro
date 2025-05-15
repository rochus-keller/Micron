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
    MicEiGen.cpp
    #MicCilGen.cpp \

HEADERS += \
    MicEiGen.h
    #MicCilGen.h \


include( MicUtils.pri )
include( MilParser2.pri )

RESOURCES += \
    MilCompiler.qrc
