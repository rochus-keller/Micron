/*
* Copyright 2025 Rochus Keller <mailto:me@rochus-keller.ch>
*
* This file is part of the Micron language project.
*
* The following is the license that applies to this copy of the
* file. For a license to use the file under conditions
* other than those described here, please email to me@rochus-keller.ch.
*
* GNU General Public License Usage
* This file may be used under the terms of the GNU General Public
* License (GPL) versions 2.0 or 3.0 as published by the Free Software
* Foundation and appearing in the file LICENSE.GPL included in
* the packaging of this file. Please review the following information
* to ensure GNU General Public Licensing requirements will be met:
* http://www.fsf.org/licensing/licenses/info/GPLv2.html and
* http://www.gnu.org/copyleft/gpl.html.
*/

#include "MilVmOakwood.h"
#include "MilInterpreter.h"
extern "C" {
#include "oakwood/Input.h"
#include "oakwood/MathL.h"
#include "oakwood/Out.h"
}
using namespace Mil;

// int Input$Time()
static bool Input_Time(void* args, void* ret)
{
    int res = Input$Time();
    Interpreter::retI4(ret, res);
    return true;
}

// double MathL$sqrt(double x)
static bool MathL_sqrt(void* args, void* ret)
{
    const double res = MathL$sqrt(Interpreter::toR8(args,0));
    Interpreter::retR8(ret,res);
    return true;
}
// double MathL$power(double x, double b)
static bool MathL_power(void* args, void* ret)
{
    const double res = MathL$power(Interpreter::toR8(args,0), Interpreter::toR8(args,Interpreter::StackAlign));
    Interpreter::retR8(ret,res);
    return true;
}
// double MathL$exp(double x)
static bool MathL_exp(void* args, void* ret)
{
    const double res = MathL$exp(Interpreter::toR8(args,0));
    Interpreter::retR8(ret,res);
    return true;
}
// double MathL$ln(double x)
static bool MathL_ln(void* args, void* ret)
{
    const double res = MathL$ln(Interpreter::toR8(args,0));
    Interpreter::retR8(ret,res);
    return true;
}
// double MathL$log(double x, double base)
static bool MathL_log(void* args, void* ret)
{
    const double res = MathL$log(Interpreter::toR8(args,0), Interpreter::toR8(args,Interpreter::StackAlign));
    Interpreter::retR8(ret,res);
    return true;
}
// double MathL$round(double x)
static bool MathL_round(void* args, void* ret)
{
    const double res = MathL$round(Interpreter::toR8(args,0));
    Interpreter::retR8(ret,res);
    return true;
}
// double MathL$sin(double x)
static bool MathL_sin(void* args, void* ret)
{
    const double res = MathL$sin(Interpreter::toR8(args,0));
    Interpreter::retR8(ret,res);
    return true;
}
// double MathL$cos(double x)
static bool MathL_cos(void* args, void* ret)
{
    const double res = MathL$cos(Interpreter::toR8(args,0));
    Interpreter::retR8(ret,res);
    return true;
}
// double MathL$tan(double x)
static bool MathL_tan(void* args, void* ret)
{
    const double res = MathL$tan(Interpreter::toR8(args,0));
    Interpreter::retR8(ret,res);
    return true;
}
// double MathL$arcsin(double x)
static bool MathL_arcsin(void* args, void* ret)
{
    const double res = MathL$arcsin(Interpreter::toR8(args,0));
    Interpreter::retR8(ret,res);
    return true;
}
// double MathL$arccos(double x)
static bool MathL_arccos(void* args, void* ret)
{
    const double res = MathL$arccos(Interpreter::toR8(args,0));
    Interpreter::retR8(ret,res);
    return true;
}
// double MathL$arctan(double x)
static bool MathL_arctan(void* args, void* ret)
{
    const double res = MathL$arctan(Interpreter::toR8(args,0));
    Interpreter::retR8(ret,res);
    return true;
}
// double MathL$arctan2(double x, double y)
static bool MathL_arctan2(void* args, void* ret)
{
    const double res = MathL$arctan2(Interpreter::toR8(args,0), Interpreter::toR8(args,Interpreter::StackAlign));
    Interpreter::retR8(ret,res);
    return true;
}
// double MathL$sinh(double x)
static bool MathL_sinh(void* args, void* ret)
{
    const double res = MathL$sinh(Interpreter::toR8(args,0));
    Interpreter::retR8(ret,res);
    return true;
}
// double MathL$cosh(double x)
static bool MathL_cosh(void* args, void* ret)
{
    const double res = MathL$cosh(Interpreter::toR8(args,0));
    Interpreter::retR8(ret,res);
    return true;
}
// double MathL$tanh(double x)
static bool MathL_tanh(void* args, void* ret)
{
    const double res = MathL$tanh(Interpreter::toR8(args,0));
    Interpreter::retR8(ret,res);
    return true;
}
// double MathL$arcsinh(double x)
static bool MathL_arcsinh(void* args, void* ret)
{
    const double res = MathL$arcsinh(Interpreter::toR8(args,0));
    Interpreter::retR8(ret,res);
    return true;
}
// double MathL$arccosh(double x)
static bool MathL_arccosh(void* args, void* ret)
{
    const double res = MathL$arccosh(Interpreter::toR8(args,0));
    Interpreter::retR8(ret,res);
    return true;
}
// double MathL$arctanh(double x)
static bool MathL_arctanh(void* args, void* ret)
{
    const double res = MathL$arctanh(Interpreter::toR8(args,0));
    Interpreter::retR8(ret,res);
    return true;
}
// void Out$Int(long long i, int n)
static bool Out_Int(void* args, void* ret)
{
    Out$Int(Interpreter::toI8(args,0), Interpreter::toI4(args,Interpreter::StackAlign));
    return true;
}
// void Out$Real(float x, int n)
static bool Out_Real(void* args, void* ret)
{
    Out$Real(Interpreter::toR4(args,0), Interpreter::toI4(args,Interpreter::StackAlign));
    return true;
}
// void Out$LongReal(double x, int n)
static bool Out_LongReal(void* args, void* ret)
{
    Out$LongReal(Interpreter::toR8(args,0), Interpreter::toI4(args,Interpreter::StackAlign));
    return true;
}
// void Out$Ln()
static bool Out_Ln(void* args, void* ret)
{
    Out$Ln();
    return true;
}
// void Out$Char(char c)
static bool Out_Char(void* args, void* ret)
{
    Out$Char(Interpreter::toI4(args,0));
    return true;
}
// void Out$String(char* str)
static bool Out_String(void* args, void* ret)
{
    Out$String((char*)Interpreter::toP(args,0));
    return true;
}


void VmOakwood::addTo(Interpreter* ip)
{
    ip->registerProc("Input", "Time", Input_Time);
    ip->registerProc("Out", "Int", Out_Int);
    ip->registerProc("Out", "Real", Out_Real);
    ip->registerProc("Out", "LongReal", Out_LongReal);
    ip->registerProc("Out", "Ln", Out_Ln);
    ip->registerProc("Out", "Char", Out_Char);
    ip->registerProc("Out", "String", Out_String);
    ip->registerProc("MathL", "sqrt", MathL_sqrt);
    ip->registerProc("MathL", "power", MathL_power);
    ip->registerProc("MathL", "exp", MathL_exp);
    ip->registerProc("MathL", "ln", MathL_ln);
    ip->registerProc("MathL", "log", MathL_log);
    ip->registerProc("MathL", "round", MathL_round);
    ip->registerProc("MathL", "sin", MathL_sin);
    ip->registerProc("MathL", "cos", MathL_cos);
    ip->registerProc("MathL", "tan", MathL_tan);
    ip->registerProc("MathL", "arcsin", MathL_arcsin);
    ip->registerProc("MathL", "arccos", MathL_arccos);
    ip->registerProc("MathL", "arctan", MathL_arctan);
    ip->registerProc("MathL", "arctan2", MathL_arctan2);
    ip->registerProc("MathL", "sinh", MathL_sinh);
    ip->registerProc("MathL", "cosh", MathL_cosh);
    ip->registerProc("MathL", "tanh", MathL_tanh);
    ip->registerProc("MathL", "arcsinh", MathL_arcsinh);
    ip->registerProc("MathL", "arccosh", MathL_arccosh);
    ip->registerProc("MathL", "arctanh", MathL_arctanh);
}
