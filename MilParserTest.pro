QT       += core

QT       -= gui

TARGET = ParserTest
CONFIG   += console
CONFIG   -= app_bundle

TEMPLATE = app

INCLUDEPATH += ..

DEFINES += _DEBUG

SOURCES +=  MilParserTest.cpp 

include(MilParser.pri)
