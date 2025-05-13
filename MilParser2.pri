

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
    $$PWD/runtime/MIC++.c \
    $$PWD/oakwood/Input+.c \
    $$PWD/oakwood/MathL+.c \
    $$PWD/oakwood/Out+.c \
    $$PWD/MilVmCode.cpp

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
    $$PWD/MilVmOps.h \
    $$PWD/MilVmCode.h
