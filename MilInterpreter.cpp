#include "MilInterpreter.h"
using namespace Mil;

// TODO: first define a binary representation which can be re-used to run INVAR procs in the compiler

class Interpreter::Imp
{
    SynTree* root;

    struct Instr {

    };

    struct Proc {

    };

public:
    void run(SynTree* st)
    {
        root = st;
    }
};

Interpreter::Interpreter()
{
    imp = new Imp;
}

Interpreter::~Interpreter()
{
    delete imp;
}

void Interpreter::run(SynTree* st)
{
    imp->run(st);
}

