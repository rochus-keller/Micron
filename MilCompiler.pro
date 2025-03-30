QT       += core

QT       -= gui

TARGET = MilCompiler
CONFIG   += console
CONFIG   -= app_bundle

TEMPLATE = app

INCLUDEPATH += ..

DEFINES += _DEBUG


SOURCES += \
    MilLexer.cpp \
    MilToken.cpp \
    MilTokenType.cpp \
    MilParser2.cpp \
    MilAst.cpp \
    MicRowCol.cpp \
    MilProject.cpp \
    MilCompiler.cpp \
    MicSymbol.cpp

HEADERS += \
    MilLexer.h \
    MilToken.h \
    MilTokenType.h \
    MilParser2.h \
    MilAst.h \
    MicRowCol.h \
    MilProject.h \
    MicSymbol.h

RESOURCES += \
    MilCompiler.qrc
