QT       += core

QT       -= gui

TARGET = MilCompiler
CONFIG   += console
CONFIG   -= app_bundle

TEMPLATE = app

INCLUDEPATH += ..

DEFINES += _DEBUG


SOURCES += \
    MilCompiler.cpp

HEADERS +=

RESOURCES += \
    MilCompiler.qrc


include( MicUtils.pri )
include(MilParser2.pri)
