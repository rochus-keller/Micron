QT       += core

QT       -= gui

TARGET = ParserTest
CONFIG   += console
CONFIG   -= app_bundle

TEMPLATE = app

INCLUDEPATH += ..

DEFINES += _DEBUG

SOURCES += \
    ParserTest.cpp \
    MicLexer.cpp \
    MicParser.cpp \
    MicPpLexer.cpp \
    MicRowCol.cpp \
    MicSynTree.cpp \
    MicToken.cpp \
    MicTokenType.cpp

HEADERS += \
    MicLexer.h \
    MicParser.h \
    MicPpLexer.h \
    MicRowCol.h \
    MicSynTree.h \
    MicToken.h \
    MicTokenType.h



