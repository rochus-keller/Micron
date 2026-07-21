QT       += core
QT       -= gui

TARGET = micc
CONFIG   += console
CONFIG   -= app_bundle
CONFIG += HAVE_SCREEN_SDL

TEMPLATE = app

INCLUDEPATH += ..

CONFIG(debug, debug|release) {
    DEFINES += _DEBUG
}

include(MicParser.pri)

SOURCES += \
    MicMain.cpp \
    MicProject2.cpp

HEADERS += \
    MicProject2.h

HAVE_SCREEN_SDL {
    DEFINES += _MIC_HAVE_SCREEN_
    LIBS += -lSDL2
    SOURCES += oakwood/ScreenSdl.c
}

HAVE_SCREEN_XCB {
    DEFINES += _MIC_HAVE_SCREEN_
    SOURCES += oakwood/ScreenXcb.c
    LIBS += -lxcb
}

HAVE_SCREEN_QT {
    QT += gui widgets
    DEFINES += _MIC_HAVE_SCREEN_ _MIC_HAVE_SCREEN_QT_
    SOURCES += oakwood/ScreenQt.cpp
    HEADERS += \
        oakwood/ScreenQt.h
}

include( MicUtils.pri )
include( MilParser2.pri )

RESOURCES += \
    MilCompiler.qrc


!win32 {
    QMAKE_CXXFLAGS += -Wno-reorder -Wno-unused-parameter -Wno-unused-function -Wno-unused-variable -Wno-switch \
        -Wno-deprecated-declarations -Wno-sign-compare -Wno-parentheses -Wno-unused-parameter -Werror=return-type
}



