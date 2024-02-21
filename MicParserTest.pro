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
    MicLexer.cpp \
    MicParser.cpp \
    MicPpLexer.cpp \
    MicRowCol.cpp \
    MicSynTree.cpp \
    MicToken.cpp \
    MicTokenType.cpp \
    MicParser2.cpp \
    MicAst.cpp

HEADERS += \
    MicLexer.h \
    MicParser.h \
    MicPpLexer.h \
    MicRowCol.h \
    MicSynTree.h \
    MicToken.h \
    MicTokenType.h \
    MicParser2.h \
    MicAst.h



