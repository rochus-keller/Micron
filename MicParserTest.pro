QT       += core

QT       -= gui

TARGET = ParserTest
CONFIG   += console
CONFIG   -= app_bundle

TEMPLATE = app

INCLUDEPATH += ..

DEFINES += _DEBUG

SOURCES += \
    MicParserTest.cpp \
    MicParser.cpp \
    MicSynTree.cpp 

HEADERS += \
    MicParser.h \
    MicSynTree.h 
    
include(MicParser.pri)



