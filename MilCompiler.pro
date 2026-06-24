QT       += core
QT       -= gui

TARGET = milc
CONFIG   += console
CONFIG   -= app_bundle
CONFIG += HAVE_SCREEN_XCB

TEMPLATE = app

INCLUDEPATH += ..

DEFINES += _DEBUG


SOURCES += \
    MilCompiler.cpp

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

RESOURCES += \
    MilCompiler.qrc

include( MicUtils.pri )
include(MilParser2.pri)

!win32 {
    QMAKE_CXXFLAGS += -Wno-reorder -Wno-unused-parameter -Wno-unused-function -Wno-unused-variable -Wno-switch \
        -Wno-deprecated-declarations -Wno-sign-compare -Wno-parentheses -Wno-unused-parameter -Werror=return-type
}
