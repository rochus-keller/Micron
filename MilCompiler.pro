QT       += core

QT       -= gui

TARGET = MilCompiler
CONFIG   += console
CONFIG   -= app_bundle

TEMPLATE = app

INCLUDEPATH += ..

DEFINES += _DEBUG


SOURCES += \
    MilCompiler.cpp \
    MilCeeGen.cpp

HEADERS += \
    MilCeeGen.h

RESOURCES += \
    MilCompiler.qrc


include(MilParser2.pri)
