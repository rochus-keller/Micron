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
    MicMain.cpp
    #MicCilGen.cpp \

HEADERS +=
    #MicCilGen.h \


include( MicUtils.pri )
include( MilParser2.pri )
include( ../EiGen/Common.pri )

RESOURCES += \
    MilCompiler.qrc
