QT       += core

QT       -= gui

TARGET = MilCompiler
CONFIG   += console
CONFIG   -= app_bundle

TEMPLATE = app

INCLUDEPATH += ..

DEFINES += _DEBUG


SOURCES += \
    MicRowCol.cpp \
    MilCompiler.cpp \
    MicSymbol.cpp

HEADERS += \
    MicRowCol.h \
    MicSymbol.h

RESOURCES += \
    MilCompiler.qrc


include(MilParser2.pri)
