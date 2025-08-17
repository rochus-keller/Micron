

SOURCES += \
    $$PWD/MilCilAsmGen.cpp \
    MilLexer.cpp \
    MilToken.cpp \
    MilTokenType.cpp \
    MilParser2.cpp \
    MilAst.cpp \
    MilProject.cpp \
    MilValidator.cpp \
    MilCeeGen.cpp \
    MilInterpreter.cpp \
    runtime/MIC++.c \
    oakwood/Input+.c \
    oakwood/MathL+.c \
    oakwood/Out+.c \
    MilVmCode.cpp \
    MilVmOakwood.cpp \
    MilEmitter.cpp \
    MilRenderer.cpp

HEADERS += \
    $$PWD/MilCilAsmGen.h \
    MilLexer.h \
    MilToken.h \
    MilTokenType.h \
    MilParser2.h \
    MilAst.h \
    MilProject.h \
    MilValidator.h \
    MilCeeGen.h \
    MilInterpreter.h \
    MilVmOps.h \
    MilVmCode.h \
    MilVmOakwood.h \
    MilEmitter.h \
    MilRenderer.h
