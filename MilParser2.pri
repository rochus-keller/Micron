

SOURCES += \
    MilLexer.cpp \
    MilToken.cpp \
    MilTokenType.cpp \
    MilParser2.cpp \
    MilAst.cpp \
    MilProject.cpp \
    MilValidator.cpp \
    MilCeeGen.cpp \
    MilInterpreter.cpp \
    $$PWD/runtime/MIC++.c

HEADERS += \
    MilLexer.h \
    MilToken.h \
    MilTokenType.h \
    MilParser2.h \
    MilAst.h \
    MilProject.h \
    MilValidator.h \
    MilCeeGen.h \
    MilInterpreter.h \
    $$PWD/MilVmOps.h
