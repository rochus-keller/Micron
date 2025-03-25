

SOURCES += \
    MilLexer.cpp \
    MilParser.cpp \
    MilToken.cpp \
    MilTokenType.cpp \
    MilSynTree.cpp \
    $$PWD/MilParser2.cpp \
    $$PWD/MilAst.cpp

HEADERS += \
    MilLexer.h \
    MilParser.h \
    MilToken.h \
    MilTokenType.h \
    MilSynTree.h \
    $$PWD/MilParser2.h \
    $$PWD/MilAst.h


!MIC_INTERPRETER {
SOURCES += MicRowCol.cpp

HEADERS += MicRowCol.h
}
