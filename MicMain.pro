QT       += core
QT       -= gui

TARGET = micc
CONFIG   += console
CONFIG   -= app_bundle

TEMPLATE = app

INCLUDEPATH += ..

CONFIG(debug, debug|release) {
        #DEFINES += _DEBUG
}

include(MicParser.pri)

SOURCES += \
    MicMain.cpp


include( MicUtils.pri )
include( MilParser2.pri )

RESOURCES += \
    MilCompiler.qrc


!win32 {
    QMAKE_CXXFLAGS += -Wno-reorder -Wno-unused-parameter -Wno-unused-function -Wno-unused-variable -Wno-switch \
        -Wno-deprecated-declarations -Wno-sign-compare -Wno-parentheses -Wno-unused-parameter -Werror=return-type
}

