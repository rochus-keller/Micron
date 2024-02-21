QT       += core

QT       -= gui

TARGET = ParserTest
CONFIG   += console
CONFIG   -= app_bundle

TEMPLATE = app

INCLUDEPATH += ..

DEFINES += _DEBUG

SOURCES += \
    MilParserTest.cpp \
    MilLexer.cpp \
    MilParser.cpp \
    MicRowCol.cpp \
    MilToken.cpp \
    MilTokenType.cpp \
    MilSynTree.cpp

HEADERS += \
    MilLexer.h \
    MilParser.h \
    MicRowCol.h \
    MilToken.h \
    MilTokenType.h \
    MilSynTree.h



