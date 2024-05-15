#ifndef MILINTERPRETER_H
#define MILINTERPRETER_H

#include <MilSynTree.h>

namespace Mil
{
class Interpreter
{
public:
    Interpreter();
    ~Interpreter();

    void run(Mil::SynTree*);
private:
    class Imp;
    Imp* imp;
};
}

#endif // MILINTERPRETER_H
