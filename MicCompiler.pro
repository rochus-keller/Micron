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
    MicCompiler.cpp \ 
    MicEiGen.cpp \
    MicCilGen.cpp

HEADERS += \
    MicEiGen.h \
    MicCilGen.h




include( ../PeLib/PeLib.pri )
