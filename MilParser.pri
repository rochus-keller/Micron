

SOURCES += \
    MilLexer.cpp \
    MilParser.cpp \
    MilToken.cpp \
    MilTokenType.cpp \
    MilSynTree.cpp

HEADERS += \
    MilLexer.h \
    MilParser.h \
    MilToken.h \
    MilTokenType.h \
    MilSynTree.h


!MIC_INTERPRETER {
SOURCES += MicRowCol.cpp

HEADERS += MicRowCol.h
}
