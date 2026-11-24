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

#include "MilCeeGen.h"
#include "MilValidator.h"
#include <QDateTime>
#include <QCoreApplication>
#include <QtDebug>
using namespace Mil;

static QByteArray toCStringLiteral(const QByteArray &in);

static QByteArray escape(const QByteArray& str)
{

    static QSet<QByteArray> keywords;
    if( keywords.isEmpty() )
    {
        keywords << "auto" << "break" << "case" << "char" << "const" << "continue" << "default"
                 << "do" << "double" << "else" << "enum" << "extern" << "float" << "for" << "goto"
                 << "if" << "inline" << "int" << "long" << "register" << "restrict" << "return" << "short"
                 << "signed" << "sizeof" << "static" << "struct" << "switch" << "typedef" << "union"
                 << "unsigned" << "void" << "volatile" << "while" << "_Bool" << "_Complex" << "_Imaginary";
    }
    if( keywords.contains(str) )
        return str + "$$"; // avoid collision with C keywords
    else
        return str;
}

static inline QByteArray name(Declaration* d)
{
    return escape(d->name);
}

static Declaration* typeDecl(Type* t)
{
    if( t == 0 )
        return 0;
    t = t->deref();
    if( t && t->decl && t->decl->kind == Declaration::TypeDecl )
        return t->decl;
    return 0;
}

static bool pointerNeedsTypeDecl(Type* t)
{
    if( t == 0 )
        return false;
    t = t->deref();
    if( t->kind == Type::Pointer )
        return false;
    return !t->isSOA() && t->kind != Type::Union;
}

static void collectTypeDeps(Type* t, bool byValue, bool scanMethods,
                            QSet<Declaration*>& deps, QSet<Declaration*>& scanned,
                            QSet<Type*>& seen)
{
    if( t == 0 )
        return;

    t = t->deref();
    if( t == 0 )
        return;

    if( t->kind == Type::Pointer )
    {
        Type* target = t->getType();
        if( target == 0 )
            return;
        Type* targetType = target->deref();
        if( t->isPtrToOpenArray() && targetType && targetType->kind == Type::Array )
            target = targetType->getType();
        targetType = target ? target->deref() : 0;
        if( targetType == 0 )
            return;
        if( targetType->kind == Type::Pointer )
            collectTypeDeps(target, false, false, deps, scanned, seen);
        else if( pointerNeedsTypeDecl(targetType) )
            collectTypeDeps(target, true, false, deps, scanned, seen);
        return;
    }

    Declaration* decl = typeDecl(t);
    if( byValue && decl )
    {
        deps.insert(decl);
        if( scanned.contains(decl) )
            return;
        scanned.insert(decl);
    }

    if( seen.contains(t) )
        return;
    seen.insert(t);

    switch( t->kind )
    {
    case Type::Proc:
        collectTypeDeps(t->getType(), true, false, deps, scanned, seen);
        foreach( Declaration* param, t->subs )
        {
            if( !param->typebound )
                collectTypeDeps(param->getType(), true, false, deps, scanned, seen);
        }
        break;
    case Type::Array:
        if( t->len )
            collectTypeDeps(t->getType(), true, false, deps, scanned, seen);
        break;
    case Type::Struct:
    case Type::Union:
        foreach( Declaration* field, t->subs )
        {
            if( field->kind == Declaration::Field )
                collectTypeDeps(field->getType(), true, false, deps, scanned, seen);
        }
        break;
    case Type::Object:
        foreach( Declaration* field, t->getFieldList(true) )
            collectTypeDeps(field->getType(), true, false, deps, scanned, seen);
        break;
    default:
        break;
    }

    if( scanMethods && (t->kind == Type::Object || t->kind == Type::Interface ||
                        (t->typebound && t->kind == Type::Struct)) )
    {
        foreach( Declaration* proc, t->getMethodTable() )
        {
            collectTypeDeps(proc->getType(), true, false, deps, scanned, seen);
            foreach( Declaration* param, proc->getParams() )
            {
                if( !param->typebound )
                    collectTypeDeps(param->getType(), true, false, deps, scanned, seen);
            }
        }
    }
}

CeeGen::CeeGen(AstModel* mdl):mdl(mdl),haveGc(false),inFrame(false),inFinallyBody(false),hasRaise(false),callTempDepth(0)
{
    Q_ASSERT(mdl);
}

static QByteArray qualident(Declaration* d)
{
    if( d->outer )
        return qualident(d->outer) + "$" + name(d->forwardToProc());
    else
        return name(d);
}

static inline QByteArray paramName(Declaration* d)
{
    if( d->typebound )
        return "$" + name(d);
    else
        return name(d);
}

static inline QByteArray itabName(Type* iface, Type* cls)
{
    return "$itab$" + name(iface->decl) + "$" + name(cls->decl);
}

static inline QByteArray thunkName(Type* iface, Type* cls, Declaration* d)
{
    return "$thunk$" + name(iface->decl) + "$" + name(cls->decl) + "$" + name(d);
}

bool CeeGen::generate(Declaration* module, QIODevice* header, QIODevice* body)
{
    Q_ASSERT( module && header );
    curMod = module;
    itabs.clear();
    curLevel = 0;
    hout.setDevice(header);
    QString dummy;
    if( body )
        bout.setDevice(body);
    else
        bout.setString(&dummy, QIODevice::WriteOnly);

    const QByteArray guard = "__" + module->name.toUpper() + "_INCLUDED__";
    const QString dedication = genDedication();

    hout << "#ifndef " << guard << endl;
    hout << "#define " << guard << endl << endl;
    hout << "// " << module->name << ".h" << endl;
    hout << dedication << endl << endl;

    bout << "// " << module->name << ".c" << endl;
    bout << dedication << endl << endl;

    Declaration* sub = curMod->subs;
    while( sub )
    {
        if(sub->kind == Declaration::Importer )
             bout << "#include \"" << escapeFilename(sub->name) << ".h\"" << endl;

        sub = sub->next;
    }
    bout << "#include \"" << escapeFilename(module->name) << ".h\"" << endl;
    bout << "#include \"MIC+.h\"" << endl;
    bout << "#include <stdlib.h>" << endl;
    bout << "#include <string.h>" << endl;
    if( haveGc )
        bout << "#include <gc.h>" << endl;
    bout << "#include <math.h>" << endl << endl;

    visitModule();

    hout << endl << "#endif // " << guard << endl << endl;
    return true;
}

bool CeeGen::requiresBody(Declaration* module)
{
    if( module == 0 )
        return false;
    Declaration* sub = module->subs;
    while(sub)
    {
        switch(sub->kind)
        {
        case Declaration::VarDecl:
            return true;
        case Declaration::Procedure:
            if( sub->forward )
                break;
            // TODO: inline and invar are implemented as normal procedures so far
            if( sub->invar || sub->inline_ )
                return true;
            if( !sub->extern_ && !sub->foreign_ )
                return true;
            break;
        case Declaration::TypeDecl:
            if( sub->getType()->kind == Type::Object || sub->getType()->kind == Type::Struct )
            {
                foreach( Declaration* d, sub->getType()->subs )
                {
                    if( d->kind == Declaration::Procedure && !d->forward )
                        return true;
                }
            }
            break;
        }

        sub = sub->next;
    }
    return false;
}

QString CeeGen::genDedication()
{
    return "// this file was generated by " + QCoreApplication::applicationName() + " "
                                    + QCoreApplication::applicationVersion() + " on " + QDateTime::currentDateTime().toString();
}

void CeeGen::enqueueHeaderCopy(Declaration* d, bool fromCopy,
                               const QHash<Declaration*, int>& importPos,
                               const QHash<Declaration*, int>& typePos, int firstTypePos,
                               QSet<Declaration*>& copySet, QList<Declaration*>& pendingCopies)
{
    if( d == 0 || d->kind != Declaration::TypeDecl )
        return;
    Declaration* module = d->getModule();
    if( module == 0 || module == curMod )
        return;
    const int importedAt = importPos.value(module, -1);
    const int neededAt = typePos.value(d, firstTypePos);
    if( importedAt >= 0 &&
         ((fromCopy && firstTypePos >= 0 && importedAt < firstTypePos) ||
          (!fromCopy && neededAt >= 0 && importedAt < neededAt)) )
        return;
    if( !copySet.contains(d) )
    {
        copySet.insert(d);
        pendingCopies.append(d);
    }
}

void CeeGen::orderHeaderCopy(Declaration* d,
                             const QHash<Declaration*, QSet<Declaration*> >& copyDeps,
                             QSet<Declaration*>& active, QSet<Declaration*>& emitted,
                             QList<Declaration*>& order)
{
    if( emitted.contains(d) )
        return;
    if( active.contains(d) )
        return;
    active.insert(d);
    foreach( Declaration* dep, copyDeps.value(d) )
        orderHeaderCopy(dep, copyDeps, active, emitted, order);
    active.remove(d);
    emitted.insert(d);
    order.append(d);
}

void CeeGen::emitGuardedType(Declaration* d, bool copy)
{
    const QByteArray guard = qualident(d) + "$def";
    hout << "#ifndef " << guard << endl;
    hout << "#define " << guard << endl;
    typeDecl(hout, d);
    hout << ";" << endl;
    Type* t = deref(d->getType());
    if( t && (t->kind == Type::Interface || (t->kind == Type::Proc && t->typebound)) )
    {
        const QByteArray typeName = qualident(d);
        const char* secondField = t->kind == Type::Interface ? "vtab$" : "proc";
        hout << "static inline int " << typeName << "$eq$(" << typeName << " a, " << typeName
             << " b) { return a.self == b.self && a." << secondField << " == b."
             << secondField << "; }" << endl;
    }
    if( t && t->objectInit && t->isSOA() )
    {
        if( copy )
            hout << "void " << qualident(d) << "$init$(" << typeRef(t)
                 << "* obj, unsigned int n);" << endl;
        else
            emitInitializer(d->getType());
    }
    hout << "#endif // " << guard << endl << endl;
}

void CeeGen::visitModule()
{
   QHash<Declaration*, int> importPos;
   QHash<Declaration*, int> typePos;
   int firstTypePos = -1;
   int pos = 0;
   bool hasImporter = false;
   Declaration* scan = curMod->subs;
   while( scan )
   {
       if( scan->kind == Declaration::Importer )
           hasImporter = true;
       else if( scan->kind == Declaration::Import && scan->imported )
           importPos.insert(scan->imported, pos);
       else if( scan->kind == Declaration::TypeDecl )
       {
           if( firstTypePos < 0 )
               firstTypePos = pos;
           typePos.insert(scan, pos);
       }
       scan = scan->next;
       ++pos;
   }

   QSet<Declaration*> copySet;
   QList<Declaration*> pendingCopies;
   QHash<Declaration*, QSet<Declaration*> > copyDeps;

   if( hasImporter && firstTypePos >= 0 )
   {
       pos = 0;
       scan = curMod->subs;
       while( scan )
       {
           if( scan->kind == Declaration::TypeDecl )
           {
               QSet<Declaration*> deps;
               QSet<Declaration*> scanned;
               QSet<Type*> seen;
               collectTypeDeps(scan->getType(), true, true, deps, scanned, seen);
               foreach( Declaration* dep, deps )
                   enqueueHeaderCopy(dep, false, importPos, typePos, firstTypePos, copySet, pendingCopies);
           }
           scan = scan->next;
           ++pos;
       }

       for( int i = 0; i < pendingCopies.size(); ++i )
       {
           Declaration* d = pendingCopies[i];
           QSet<Declaration*> deps;
           QSet<Declaration*> scanned;
           QSet<Type*> seen;
           collectTypeDeps(d->getType(), true, false, deps, scanned, seen);
           foreach( Declaration* dep, deps )
           {
               const int before = copySet.size();
               enqueueHeaderCopy(dep, true, importPos, typePos, firstTypePos, copySet, pendingCopies);
               if( copySet.size() != before || copySet.contains(dep) )
                   copyDeps[d].insert(dep);
           }
       }
   }

   QList<Declaration*> copyOrder;
   QSet<Declaration*> active;
   QSet<Declaration*> emitted;
   foreach( Declaration* d, pendingCopies )
       orderHeaderCopy(d, copyDeps, active, emitted, copyOrder);

   bool copiesEmitted = false;
   Declaration* sub = curMod->subs;
   while( sub )
   {
       switch(sub->kind)
       {
       case Declaration::Import:
            hout << "#include \"" << escapeFilename(sub->name) << ".h\"" << endl;
            break;
       case Declaration::TypeDecl:
           if( !copiesEmitted )
           {
               foreach( Declaration* copy, copyOrder )
                   emitGuardedType(copy, true);
               copiesEmitted = true;
           }
           emitGuardedType(sub, false);
           break;
       case Declaration::ConstDecl:
           hout << "#define " << qualident(sub) << " ";
           if( sub->c && sub->c->kind == Constant::B )
           {
               const ByteString* bytes = sub->c->b;
               hout << toCStringLiteral(QByteArray::fromHex(
                   QByteArray(reinterpret_cast<const char*>(bytes->b), bytes->len)));
           }else
               constValue(hout, sub->c, sub->getType());
           hout << endl;
           break;
       }

       sub = sub->next;
   }
   hout << endl;

   bout << "typedef struct $Class { struct $Class* super; } $Class;" << endl;
   bout << "static int $isinst(const void* super, void* sub) {" << endl;
   bout << "    $Class* cls = ($Class*)sub;" << endl;
   bout << "    while(cls) {" << endl;
   bout << "        if( cls == super ) return 1;" << endl;
   bout << "        cls = cls->super;" << endl;
   bout << "    }" << endl;
   bout << "    return 0;" << endl;
   bout << "}" << endl << endl;

   bool initFound = false;
   sub = curMod->subs;
   while( sub )
   {
       switch( sub->kind )
       {
       case Declaration::TypeDecl:
           {
               Type* t = deref(sub->getType());
               if( t && (t->kind == Type::Object || (t->typebound && t->kind == Type::Struct) || t->kind == Type::Interface) )
               {
                   foreach( Declaration* p, t->subs )
                   {
                       if( p->kind == Declaration::Procedure )
                           visitProcedure(p);
                   }
                   visitMetaDecl(sub);
               }
           }
           break;
       case Declaration::VarDecl:
           variable(bout, sub);
           bout << ";" << endl << endl;
           hout << "extern ";
           variable(hout, sub);
           hout << ";" << endl;
           break;
       case Declaration::Procedure:
           visitProcedure(sub);
           if( sub->entryPoint )
               initFound = true;
           break;
       }

       sub = sub->next;
   }

   if( !initFound )
   {
        Declaration proc;
        proc.kind = Declaration::Procedure;
        proc.entryPoint = true;
        proc.outer = curMod;
        proc.name = "begin$";
        visitProcedure(&proc);
   }
}

void CeeGen::visitProcedure(Declaration* proc)
{
    curProc = proc;
    hout << "extern ";
    procHeader(hout, proc);
    hout << ";" << endl;
    done.clear();

    Type* receiver = proc->getReceiver() ? deref(proc->getReceiver()) : 0;

    if( !proc->forward && !proc->extern_ && !proc->foreign_ && (receiver == 0 || receiver->kind != Type::Interface) )
    {
        const bool hasFinally = proc->pd && proc->pd->finally;
        generateItabs(proc->body);
        if( hasFinally )
            generateItabs(proc->pd->finally);

        scannedPcalls.clear();
        pcallIds.clear();
        pcallSites.clear();
        hasRaise = false;
        scanPcalls(proc->body);
        if( hasFinally )
            scanPcalls(proc->pd->finally);
        int bodyCallTemps = 0;
        int finallyCallTemps = 0;
        scannedCallTemps.clear();
        activeCallTemps.clear();
        scanCallTemps(proc->body, 0, bodyCallTemps);
        if( hasFinally )
        {
            scannedCallTemps.clear();
            activeCallTemps.clear();
            scanCallTemps(proc->pd->finally, 0, finallyCallTemps);
        }
        if( hasFinally || hasRaise || !pcallSites.isEmpty() )
        {
            bout << "#if defined(__GNUC__)" << endl;
            bout << "extern void MIC$$raise(void* e) __attribute__((noreturn));" << endl;
            bout << "#else" << endl;
            bout << "extern void MIC$$raise(void* e);" << endl;
            bout << "#endif" << endl;
            bout << "extern void* MIC$$pcall(void (*fn)(void*), void* ctx);" << endl << endl;
        }
        for( int i = 0; i < pcallSites.size(); ++i )
            emitPcallThunk(pcallSites[i], i);

        if( hasFinally )
        {
            const QByteArray frameName = qualident(proc) + "$frame";
            const QByteArray bodyName = qualident(proc) + "$body";
            DeclList params = proc->getParams();
            DeclList locals = proc->getLocals();
            const bool hasResult = typeRef(proc->getType()) != "void";

            bout << "typedef struct {" << endl;
            foreach( Declaration* param, params )
                bout << ws(0) << typeRef(param->getType()) << " " << paramName(param) << ";" << endl;
            foreach( Declaration* local, locals )
                bout << ws(0) << typeRef(local->getType()) << " " << name(local) << ";" << endl;
            if( hasResult )
                bout << ws(0) << typeRef(proc->getType()) << " ret$;" << endl;
            if( params.isEmpty() && locals.isEmpty() && !hasResult )
                bout << ws(0) << "char unused$;" << endl;
            bout << "} " << frameName << ";" << endl << endl;

            bout << "static void " << bodyName << "(void* c$) {" << endl;
            bout << ws(0) << frameName << "* f$ = (" << frameName << "*)c$;" << endl;
            bout << ws(0) << "void* _ptr$;" << endl;
            bout << ws(0) << "unsigned int _len$;" << endl;
            emitCallTemps(bout, bodyCallTemps);
            done.clear();
            inFrame = true;
            inFinallyBody = true;
            callTempDepth = 0;
            createLdindLocals(proc->body);
            statementSeq(bout, proc->body);
            inFrame = false;
            inFinallyBody = false;
            bout << "}" << endl << endl;

            procHeader(bout, proc);
            bout << " {" << endl;
            bout << ws(0) << frameName << " f$0;" << endl;
            bout << ws(0) << frameName << "* f$ = &f$0;" << endl;
            bout << ws(0) << "void* _ptr$;" << endl;
            bout << ws(0) << "unsigned int _len$;" << endl;
            emitCallTemps(bout, finallyCallTemps);
            foreach( Declaration* param, params )
            {
                bout << ws(0) << "f$->" << paramName(param) << " = ";
                if( param->typebound )
                    bout << "(" << typeRef(param->getType()) << ")" << name(param);
                else
                    bout << name(param);
                bout << ";" << endl;
            }
            foreach( Declaration* local, locals )
                emitSoapInit(bout, "f$->" + name(local), local->getType(), 0 );

            bout << ws(0) << "void* e$ = MIC$$pcall(" << bodyName << ", f$);" << endl;
            done.clear();
            inFrame = true;
            inFinallyBody = false;
            callTempDepth = 0;
            createLdindLocals(proc->pd->finally);
            statementSeq(bout, proc->pd->finally);
            inFrame = false;
            bout << ws(0) << "if( e$ ) MIC$$raise(e$);" << endl;
            if( hasResult )
                bout << ws(0) << "return f$->ret$;" << endl;
            else
                bout << ws(0) << "return;" << endl;
            bout << "}" << endl << endl;
        }else
        {
            procHeader(bout, proc);
            bout << " {" << endl;
            if( proc->entryPoint && !curMod->nobody )
            {
                bout << ws(0) << "static int done$ = 0;" << endl;
                bout << ws(0) << "if(!done$) {" << endl;
                bout << ws(1) <<     "done$ = 1;" << endl;
                Declaration* sub = curMod->subs;
                while( sub )
                {
                    if( sub->kind == Declaration::Import && !sub->imported->nobody )
                         bout << ws(1) << name(sub->imported) << "$begin$();" << endl;
                    else if( sub->kind == Declaration::VarDecl )
                        emitSoapInit(bout, qualident(sub), sub->getType(), 1 );
                    sub = sub->next;
                }
                bout << ws(0) << "} else return;" << endl;
            }
            bout << ws(0) << "void* _ptr$;" << endl;
            bout << ws(0) << "unsigned int _len$;" << endl;
            emitCallTemps(bout, bodyCallTemps);
            Declaration* sub = proc->subs;
            while(sub)
            {
                if( sub->kind == Declaration::LocalDecl )
                {
                    bout << ws(0);
                    parameter(bout, sub);
                    bout << ";" << endl;
                    emitSoapInit(bout, name(sub), sub->getType(), 0 );
                }else if( sub->kind == Declaration::ParamDecl && sub->typebound )
                {
                    bout << ws(0) << typeRef(sub->getType()) << " " << paramName(sub) <<
                            " = (" << typeRef(sub->getType()) << ")" << name(sub) << ";" << endl;
                }
                sub = sub->next;
            }
            callTempDepth = 0;
            createLdindLocals(proc->body);
            statementSeq(bout, proc->body);
            bout << "}" << endl << endl;
        }
    }
    curProc = 0;
}

void CeeGen::scanPcalls(Statement* body)
{
    while( body )
    {
        if( body->kind == IL_raise )
            hasRaise = true;
        scanPcalls(body->args);
        if( body->hasE() )
            scanPcalls(body->e);
        if( body->body )
            scanPcalls(body->body);
        body = body->next;
    }
}


void CeeGen::scanPcalls(Expression* e)
{
    while( e )
    {
        if( scannedPcalls.contains(e) )
        {
            e = e->next;
            continue;
        }
        scannedPcalls.insert(e);
        if( e->kind == IL_pcall )
        {
            pcallIds.insert(e, pcallSites.size());
            pcallSites.append(e);
        }
        scanPcalls(e->lhs);
        scanPcalls(e->rhs);
        if( e->hasE() )
            scanPcalls(e->e);
        e = e->next;
    }
}

void CeeGen::scanCallTemps(Statement* body, int depth, int& maxDepth)
{
    while( body )
    {
        scanCallTemps(body->args, depth, maxDepth);
        if( body->hasE() )
            scanCallTemps(body->e, depth, maxDepth);
        if( body->body )
            scanCallTemps(body->body, depth, maxDepth);
        body = body->next;
    }
}

void CeeGen::scanCallTemps(Expression* e, int depth, int& maxDepth)
{
    QSet<Expression*> seenInList;
    while( e )
    {
        if( seenInList.contains(e) )
            return;
        seenInList.insert(e);
        if( !activeCallTemps.contains(e) &&
                !(scannedCallTemps.contains(e) && scannedCallTemps.value(e) >= depth) )
        {
            scannedCallTemps.insert(e, depth);
            activeCallTemps.insert(e);
            const bool nestedCall = e->kind == IL_callvirt || e->kind == IL_callmi;
            const int childDepth = nestedCall ? depth + 1 : depth;
            if( nestedCall )
                maxDepth = qMax(maxDepth, childDepth);
            scanCallTemps(e->lhs, childDepth, maxDepth);
            scanCallTemps(e->rhs, childDepth, maxDepth);
            if( e->hasE() )
                scanCallTemps(e->e, childDepth, maxDepth);
            activeCallTemps.remove(e);
        }
        e = e->next;
    }
}

bool CeeGen::isSimpleCallReceiver(Expression* e)
{
    if( e == 0 )
        return false;
    switch( e->kind )
    {
    case IL_ldvar:
    case IL_ldarg:
    case IL_ldarg_s:
    case IL_ldarg_0:
    case IL_ldarg_1:
    case IL_ldarg_2:
    case IL_ldarg_3:
    case IL_ldloc:
    case IL_ldloc_s:
    case IL_ldloc_0:
    case IL_ldloc_1:
    case IL_ldloc_2:
    case IL_ldloc_3:
        return true;
    case IL_ldfld:
        return isSimpleCallReceiver(e->lhs);
    case IL_ldind_i1:
    case IL_ldind_i2:
    case IL_ldind_i4:
    case IL_ldind_i8:
    case IL_ldind_ip:
    case IL_ldind_ipp:
    case IL_ldind_r4:
    case IL_ldind_r8:
    case IL_ldind_u1:
    case IL_ldind_u2:
    case IL_ldind_u4:
    case IL_ldind_u8:
        return isSimpleCallReceiver(e->lhs);
    case IL_ldind:
        if( e->lhs == 0 || e->lhs->getType() == 0 )
            return false;
        if( deref(e->lhs->getType())->isPtrToOpenCharArray() ||
                deref(e->lhs->getType())->kind == Type::StringLit )
            return false;
        return isSimpleCallReceiver(e->lhs);
    default:
        return false;
    }
}

void CeeGen::emitCallTemps(QTextStream& out, int count)
{
    for( int i = 0; i < count; ++i )
        out << ws(0) << "void* _cv$" << i << ";" << endl;
}

QList<Declaration*> CeeGen::pcallFormals(Expression* e)
{
    if( e->d->kind == Declaration::Procedure )
        return e->d->getParams();
    return deref(e->d->getType())->subs;
}

void CeeGen::emitPcallThunk(Expression* e, int index)
{
    const QByteArray siteName = qualident(curProc) + "$pcall$" + QByteArray::number(index);
    const QList<Declaration*> formals = pcallFormals(e);
    bout << "typedef struct {" << endl;
    bout << ws(0) << "void (*p)(";
    if( formals.isEmpty() )
        bout << "void";
    else
        for( int i = 0; i < formals.size(); ++i )
        {
            if( i != 0 )
                bout << ", ";
            bout << typeRef(formals[i]->getType());
        }
    bout << ");" << endl;
    for( int i = 0; i < formals.size(); ++i )
        bout << ws(0) << typeRef(formals[i]->getType()) << " a" << i << ";" << endl;
    bout << "} " << siteName << ";" << endl << endl;
    bout << "static void " << siteName << "$thunk(void* c) {" << endl;
    bout << ws(0) << siteName << "* a = (" << siteName << "*)c;" << endl;
    bout << ws(0) << "a->p(";
    for( int i = 0; i < formals.size(); ++i )
    {
        if( i != 0 )
            bout << ", ";
        bout << "a->a" << i;
    }
    bout << ");" << endl;
    bout << "}" << endl << endl;
}

QByteArray CeeGen::localRef(Declaration* d) const
{
    return (inFrame ? QByteArray("f$->") : QByteArray()) + name(d);
}

QByteArray CeeGen::paramRef(Declaration* d) const
{
    return (inFrame ? QByteArray("f$->") : QByteArray()) + paramName(d);
}

void CeeGen::visitMetaDecl(Declaration* d)
{
    const QByteArray className = qualident(d);
    const QByteArray guard = className + "$Vtab$$def";
    Type* t = deref(d->getType());
    hout << "#ifndef " << guard << endl;
    hout << "#define " << guard << endl;
    hout << "struct " << className << "$Vtab$ {" << endl;

    if( t->kind != Type::Interface)
        bout << "const struct " << className << "$Vtab$ " << className << "$vtab$ = { " << endl;

    if( t->getType() )
    {
        hout << ws(0) << "const struct " << typeRef(t->getType()) << "$Vtab$* super$;" << endl;
        bout << ws(0) << "&" << typeRef(t->getType()) << "$vtab$," << endl;
    }else if(t->kind == Type::Object)
    {
        hout << ws(0) << "void* super$;" << endl;
        bout << ws(0) << "0," << endl;
    }

    DeclList methods = t->getMethodTable();
    foreach( Declaration* p, methods )
    {
        Q_ASSERT( p->kind == Declaration::Procedure && !p->forward );
        if( t->kind != Type::Interface)
            bout << ws(0) << qualident(p) << ", " << endl;

        hout << ws(0) << typeRef(p->getType()) << " (*" << name(p) << ")";
        hout << "(";
        DeclList params = p->getParams();
        for( int i = 0; i < params.size(); i++ )
        {
            if( i != 0 )
                hout << ", ";
            parameter(hout, params[i]);
        }
        hout << ");" << endl;
    }

    if( t->kind != Type::Interface)
        bout << "};" << endl << endl;
    hout << "};" << endl;

    if( t->kind != Type::Interface)
        hout << "extern const struct " << className << "$Vtab$ " << className << "$vtab$;" << endl;

    hout << "#endif // " << guard << endl;
}

QByteArray CeeGen::typeRef(Type* t)
{
    if( t == 0 )
        return "void";
    t = t->deref();
    switch(t->kind)
    {
    case Type::Any:
        return "void";
    case Type::StringLit:
        return "const char*";
    case Type::ByteArrayLit:
        return "const unsigned char*";
    case Type::NIL:
        return "NULL";
    case Type::DBLNIL:
        return "DBLINTPTR"; // TODO
    case Type::BOOL:
        return "unsigned char";
    case Type::CHAR:
        return "char";
    case Type::INT8:
        return "char";
    case Type::INT16:
        return "short";
    case Type::INT32:
        return "int";
    case Type::INT64:
        return "long long";
    case Type::UINT8:
        return "unsigned char";
    case Type::UINT16:
        return "unsigned short";
    case Type::UINT32:
        return "unsigned int";
    case Type::UINT64:
        return "unsigned long long";
    case Type::FLOAT32:
        return "float";
    case Type::FLOAT64:
        return "double";
    case Type::INTPTR:
        return "void*";
    case Type::DBLINTPTR:
        return "DBLINTPTR"; // TODO
    }

    if( t->kind == Type::Pointer )
    {

        Type* to = deref(t->getType());
        if( t->isPtrToOpenArray())
            // pointer to open arrays have no extra typedef, instead we use element_type*
            to = deref(to->getType());
        QByteArray prefix;
        if( to->isSOA() ) // array types are embedded in a struct
            prefix = "struct ";
        else if( to->kind == Type::Union )
            prefix = "union ";
        return prefix + typeRef(to) + "*";
    }else if( t->decl )
        return qualident(t->decl);
    else
        return "?TYPE";
}

void CeeGen::procHeader(QTextStream& out, Declaration* proc)
{
    out << typeRef(proc->getType()) << " ";
    out << qualident(proc->forwardToProc());
    out << "(";
    DeclList params = proc->getParams();
    for( int i = 0; i < params.size(); i++ )
    {
        if( i != 0 )
            out << ", ";
        parameter(out, params[i]);
    }
    out << ")";
}

void CeeGen::parameter(QTextStream& out, Declaration* param)
{
    if( param->typebound )
        out << "void* " << name(param);
    else
        out << typeRef(param->getType()) << " " << name(param);
}

void CeeGen::variable(QTextStream& out, Declaration* var)
{
    out << typeRef(var->getType()) << " " << qualident(var);
}

void CeeGen::typeDecl(QTextStream& out, Declaration* d)
{
    Type* t = deref(d->getType());
    if( t == 0 )
    {
        out << "// undeclared type " << d->name;
        return;
    }

    if( t->kind == Type::Object || (t->typebound && t->kind == Type::Struct) )
    {
        // forward declaration for class objects
        out << "typedef struct " << qualident(d) << "$Vtab$ " << qualident(d) << "$Vtab$;" << endl;
    }

    if( t->kind == Type::Array && t->len == 0 )
    {
        out << "// no typedef for open array " << qualident(d) << " (" << typeRef(t->getType()) << "*)";
        return; // we need not typedef for open arrays, instead they are referenced by element_type*
    }

    out << "typedef ";
    if( t->kind < Type::MaxBasicType )
        out << typeRef(t);
    else
        switch( t->kind )
        {
        case Type::Pointer:
            pointerTo(out, t);
            break;
        case Type::Proc:
            if( t->typebound )
            {
                out << "struct " << qualident(d) << " {" << endl;
                out << ws(0) << "void* self;" << endl;
                out << ws(0) << typeRef(t->getType()) << " (*proc)(void* self";
                DeclList params = t->subs;
                for( int i = 0; i < params.size(); i++ )
                {
                    if( t->typebound || i != 0 )
                        out << ", ";
                    parameter(out, params[i]);
                }
                out << ");" << endl;
                out << "}";
            }else
            {
                out << typeRef(t->getType()) << " (*";
                out << qualident(d);
                out << ")(";
                DeclList params = t->subs;
                for( int i = 0; i < params.size(); i++ )
                {
                    if( i != 0 )
                        out << ", ";
                    parameter(out, params[i]);
                }
                out << ")";
                return;
            }
            break;
        case Type::Array:
            if( t->len != 0 )
                out << "struct " << qualident(d) << " { " << typeRef(t->getType()) << " _[" << t->len << "];" << " }";
            else
                Q_ASSERT(false); // out << typeRef(t->getType()) << "* ";
            break;

        case Type::Union:
        case Type::Struct:
            out << (t->kind == Type::Struct ? "struct " : "union ") << qualident(d) << " {" << endl;
            foreach( Declaration* field, t->subs )
            {
                if( field->kind == Declaration::Field )
                    out << ws(0) << typeRef(field->getType()) << " " << name(field) << ";" << endl;
            }
            out << "}";
            break;
        case Type::Object: {
                out << "struct " << qualident(d) << " {" << endl;
                out << ws(0) << "const " << qualident(d) << "$Vtab$* vtab$;" << endl;
                QList<Declaration*> fields = t->getFieldList(true);
                foreach( Declaration* field, fields ) // TODO: was t->subs, but this cannot be correct
                {
                     out << ws(0) << typeRef(field->getType()) << " " << name(field) << ";" << endl;
                }
                out << "}";
            } break;
        case Type::NameRef:
            out << typeRef(t->getType());
            break;
        case Type::Interface:
            out << "struct " << qualident(d) << " {" << endl;
            out << ws(0) << "void* self;" << endl;
            out << ws(0) << "const struct " << qualident(d) << "$Vtab$* vtab$;" << endl;
            out << "}";
            break;
        }
    out << " " << qualident(d);
}

void CeeGen::pointerTo(QTextStream& out, Type* ptr)
{
    Type* to = ptr->getType();
    Type* to2 = deref(to);
    if( to2 && to2->kind == Type::Array && to2->len == 0 )
    {
        // Pointer to array is translated to pointer to array element
        ptr = to2;
        to = ptr->getType();
        to2 = deref(to);
    }
    if( to2 && to2->isSUOA() )
    {
        if( deref(to)->kind == Type::Union )
            out << "union ";
        else
            out << "struct ";
    }
    out << typeRef(ptr->getType()) << "*";
}

static inline bool isPrintableAscii(unsigned char c)
{
    return c >= 0x20 && c <= 0x7e;
}

static inline char octDigit(unsigned int v)
{
    return char('0' + (v & 7));
}

static QByteArray toCStringLiteral(const QByteArray &in)
{
    QByteArray out;
    out += '"';

    bool lastWasOctalEscape = false;

    for (int i = 0; i < in.size(); ++i) {
        unsigned char c = static_cast<unsigned char>(in.at(i));

        if (lastWasOctalEscape && c >= '0' && c <= '9') {
            out += "\"\"";
        }

        switch (c) {
        case '\\':
            out += "\\\\";
            lastWasOctalEscape = false;
            break;
        case '\"':
            out += "\\\"";
            lastWasOctalEscape = false;
            break;
        case '\a':
            out += "\\a";
            lastWasOctalEscape = false;
            break;
        case '\b':
            out += "\\b";
            lastWasOctalEscape = false;
            break;
        case '\f':
            out += "\\f";
            lastWasOctalEscape = false;
            break;
        case '\n':
            out += "\\n";
            lastWasOctalEscape = false;
            break;
        case '\r':
            out += "\\r";
            lastWasOctalEscape = false;
            break;
        case '\t':
            out += "\\t";
            lastWasOctalEscape = false;
            break;
        case '\v':
            out += "\\v";
            lastWasOctalEscape = false;
            break;
        default:
            if (isPrintableAscii(c)) {
                out += QChar(c);
                lastWasOctalEscape = false;
            } else {
                out += '\\';
                out += QChar(octDigit(c >> 6));
                out += QChar(octDigit(c >> 3));
                out += QChar(octDigit(c));
                lastWasOctalEscape = true;
            }
            break;
        }
    }

    out += '"';
    return out;
}

void CeeGen::constValue(QTextStream& out, Constant* c, Type* hint)
{
    if( c == 0 )
    {
        out << 0;
        return;
    }
    switch( c->kind )
    {
    case Constant::D:
        out << c->d;
        break;
    case Constant::I:
        out << c->i;
        break;
    case Constant::P:
        out << "0x" << QByteArray::number(c->p,16);
        break;
    case Constant::S:
        out << toCStringLiteral(c->s);
        break;
    case Constant::B:
        {
            const ByteString* ba = c->b;
            out << "{";
            for( int i = 0; i < ba->len; i++ )
            {
                if( i != 0 && i % 16 == 0 )
                    out << endl << "\t";
                out << "0x" << QByteArray::number(quint8(ba->b[i]),16) << ", ";
            }
            out << "}";
        }
        break;
    case Constant::R:
        constValue(out, c->r->c, 0);
        break;
    case Constant::C:
        {
            bool fixArray = false;
            Type* cType = c->c->type ? deref(c->c->type) : 0;
            if( c->c->type )
            {
                Type* t = c->c->type;
#if 0
                if( hint )
                    t = hint;
#endif
                out << "(" << typeRef(t) << ")";
                if( cType && cType->kind == Type::Array && cType->len != 0 )
                    fixArray = true;
            }
            out << "{";
            if( fixArray )
                out << "{";
            for( int i = 0; i < c->c->c.size(); i++ )
            {
                if( i != 0 )
                    out << ", ";
                if( !c->c->c[i].name.isEmpty() )
                {
                    if( cType && cType->kind == Type::Array )
                        out << "[" << escape(c->c->c[i].name) << "]=";
                    else
                        out << "." << escape(c->c->c[i].name) << "=";
                }
                constValue(out, c->c->c[i].c, 0);
            }
            if( fixArray )
                out << "}";
            out << "}";
        }
        break;
    }
}

void CeeGen::expressionAs(QTextStream& out, Expression* e, Type* targetType,
                          Type* hint, bool targetIsVoidPointer)
{
    Type* target = targetType ? deref(targetType) : 0;
    Type* source = e && e->getType() ? deref(e->getType()) : 0;
    const QByteArray targetRef = typeRef(targetType);
    const QByteArray sourceRef = e && e->getType() ? typeRef(e->getType()) : QByteArray();
    Type* exprHint = hint ? hint : targetType;

    if( !targetIsVoidPointer && source && source->kind == Type::Pointer &&
        target && target->kind == Type::Pointer && targetRef != sourceRef )
    {
        if( targetIsVoidPointer || (target && target->kind == Type::Pointer) )
            exprHint = targetType;
        out << "(" << targetRef << ")(";
        expression(out, e, exprHint);
        out << ")";
    }else
    {
        if( targetIsVoidPointer || (target && target->kind == Type::Pointer) )
            exprHint = targetType;
        expression(out, e, exprHint);
    }
}

void CeeGen::emitAssignment(QTextStream& out, const QString& lhs, Type* lhsType,
                            Expression* rhs, Type* hint)
{
    Type* rhsType = rhs ? rhs->getType() : 0;
    Type* lhsValueType = deref(lhsType);
    Type* rhsValueType = deref(rhsType);
    const bool lhsAggregate = lhsValueType->kind == Type::Struct ||
            lhsValueType->kind == Type::Union || lhsValueType->kind == Type::Object ||
            (lhsValueType->kind == Type::Array && lhsValueType->len != 0);
    const bool rhsAggregate = rhsValueType->kind == Type::Struct ||
            rhsValueType->kind == Type::Union || rhsValueType->kind == Type::Object ||
            (rhsValueType->kind == Type::Array && rhsValueType->len != 0);
    const QByteArray lhsRef = typeRef(lhsType);
    const QByteArray rhsRef = typeRef(rhsType);

    if( !lhsAggregate || !rhsAggregate || lhsRef == rhsRef ||
            lhsRef == "void" || lhsRef == "NULL" ||
            rhsRef == "void" || rhsRef == "NULL" ||
            lhsRef.contains("?TYPE") || rhsRef.contains("?TYPE") )
    {
        out << lhs << " = ";
        expressionAs(out, rhs, lhsType, hint);
    }else
    {
        out << "memcpy(&(" << lhs << "), &(union { " << rhsRef
             << " source; " << lhsRef << " target; }){ .source = ";
        expression(out, rhs, rhsType);
        out << " }, sizeof(" << lhsRef << ")";
    }
}

void CeeGen::statementSeq(QTextStream& out, Statement* s, int level)
{
    while(s)
    {
        switch( s->kind )
        {
        case Statement::ExprStat:
            if( s->args )
            {
                out << ws(level);
                // NOTE: s->args points to the expr in a sequence after which the stack is empty
                expression(out, s->args);
                out << ";" << endl;
            }
            break;

        case IL_if:
            out << ws(level) << "if( ";
            expression(out, s->args);
            out << " ) {" << endl;
            statementSeq(out, s->body, level+1);
            out << ws(level) << "}";
            if( s->next && s->next->kind == IL_else )
            {
                s = s->next;
                out << " else {" << endl;
                statementSeq(out, s->body, level+1);
                out << ws(level) << "}";
            }
            out << endl;
            break;

        case IL_loop:
            out << ws(level) << "while( 1 ) {" << endl;
            statementSeq(out, s->body, level+1);
            out << ws(level) << "}" << endl;
            break;

        case IL_repeat:
            out << ws(level) << "do {" << endl;
            statementSeq(out, s->body, level+1);
            out << ws(level) << "} while( !";
            expression(out, s->args);
            out << " );" << endl;
            break;

        case IL_switch:
            out << ws(level) << "switch( ";
            expression(out, s->args);
            out << " ) {" << endl;
            while( s->next && s->next->kind == IL_case )
            {
                s = s->next;
                Expression* e = s->e;
                while(e)
                {
                    out << ws(level) << "case " << e->i << ":" << endl;
                    e = e->next;
                }
                out << ws(level+1) << "{" << endl;
                statementSeq(out, s->body, level+2);
                out << ws(level+1) << "} break;" << endl;
            }
            if( s->next && s->next->kind == IL_else )
            {
                s = s->next;
                out << ws(level) << "default:" << endl;
                out << ws(level+1) << "{" << endl;
                statementSeq(out, s->body, level+2);
                out << ws(level+1) << "} break;" << endl;
            }
            out << ws(level) << "}" << endl;
            break;

        case IL_while:
            out << ws(level) << "while( ";
            expression(out, s->args);
            out << " ) {" << endl;
            statementSeq(out, s->body, level+1);
            out << ws(level) << "}" << endl;
            break;
        case IL_exit:
            out << ws(level) << "break;" << endl;
            break;

        case IL_stloc:
        case IL_stloc_s:
        case IL_stloc_0:
        case IL_stloc_1:
        case IL_stloc_2:
        case IL_stloc_3:
            // ASSIG
            {
                DeclList locals = curProc->getLocals();
                Q_ASSERT(s->id < locals.size());
                out << ws(level);
                QString lhsText;
                QTextStream lhsOut(&lhsText);
                lhsOut << localRef(locals[s->id]);
                lhsOut.flush();
                emitAssignment(out, lhsText, locals[s->id]->getType(), s->args,
                               locals[s->id]->getType());
                out << ";" << endl;
            }
            break;

        case IL_starg:
        case IL_starg_s:
            // ASSIG
            {
                DeclList params = curProc->getParams();
                Q_ASSERT(s->id < params.size());
                out << ws(level);
                QString lhsText;
                QTextStream lhsOut(&lhsText);
                lhsOut << paramRef(params[s->id]);
                lhsOut.flush();
                emitAssignment(out, lhsText, params[s->id]->getType(), s->args,
                               params[s->id]->getType());
                out << ";" << endl;
            }
            break;

        case IL_stind:
        case IL_stind_i1:
        case IL_stind_i2:
        case IL_stind_i4:
        case IL_stind_i8:
        case IL_stind_r4:
        case IL_stind_r8:
        case IL_stind_ip:
            // ASSIG
            {
                Q_ASSERT( s->args && s->args->kind == Expression::Argument );
                Type* pointeeType = deref(s->args->lhs->getType())->getType();
                Type* storeType = s->kind == IL_stind && s->d ?
                            s->d->getType() : pointeeType;
                const bool castAddress = s->kind == IL_stind && s->d &&
                            typeRef(storeType) != typeRef(pointeeType);
                out << ws(level);
                QString lhsText;
                QTextStream lhsOut(&lhsText);
                if( castAddress )
                {
                    lhsOut << "(*(" << typeRef(storeType) << "*)";
                    expression(lhsOut, s->args->lhs);
                    lhsOut << ")";
                }else
                {
                    lhsOut << "*";
                    expression(lhsOut, s->args->lhs);
                }
                lhsOut.flush();
                emitAssignment(out, lhsText, storeType, s->args->rhs, s->args->lhs->getType());
                out << ";" << endl;
            }
            break;

        case IL_stind_ipp:
            // ASSIG
            {
                Q_ASSERT( s->args && s->args->kind == Expression::Argument );
                out << ws(level) << "*";
                expression(out, s->args->lhs);
                out << " = ";
                Type* storeType = deref(s->args->lhs->getType())->getType();
                if( s->args->rhs->kind == IL_ldmeth )
                {
                    out << "(" << typeRef(s->args->lhs->getType()->getType()) << ")";
                    expression(out, s->args->rhs, s->args->lhs->getType());
                }else
                    expressionAs(out, s->args->rhs, storeType, s->args->lhs->getType());
                out << ";" << endl;
            }
            break;

        case IL_stelem:
        case IL_stelem_i1:
        case IL_stelem_i2:
        case IL_stelem_i4:
        case IL_stelem_i8:
        case IL_stelem_r4:
        case IL_stelem_r8:
        case IL_stelem_ip:
            // ASSIG
            {
                Q_ASSERT( s->args && s->args->kind == Expression::Argument &&
                          s->args->lhs && s->args->rhs &&
                          s->args->next && s->args->next->kind == Expression::Argument &&
                          s->args->next->rhs && s->args->next->lhs == 0);
                Type* arrayType = deref(s->args->next->rhs->getType())->getType();
                Type* elementType = arrayType->getType();
                Type* storeType = s->kind == IL_stelem && s->d ?
                            s->d->getType() : elementType;
                const bool castAddress = s->kind == IL_stelem && s->d &&
                            typeRef(storeType) != typeRef(elementType);
                out << ws(level);
                QString lhsText;
                QTextStream lhsOut(&lhsText);
                if( castAddress )
                    lhsOut << "(*(" << typeRef(storeType) << "*)&(";
                expression(lhsOut, s->args->next->rhs);
                if( deref(s->args->next->rhs->getType())->isPtrToFixArray() )
                    lhsOut << "->_";
                lhsOut << "[";
                expression(lhsOut, s->args->lhs);
                lhsOut << "]";
                if( castAddress )
                    lhsOut << "))";
                lhsOut.flush();
                emitAssignment(out, lhsText, storeType, s->args->rhs, elementType);
                out << ";" << endl;
            }
            break;

        case IL_stfld:
            // ASSIG
            {
                Q_ASSERT( s->args && s->args->kind == Expression::Argument );
                out << ws(level);
                QString lhsText;
                QTextStream lhsOut(&lhsText);
                lhsOut << "(";
                expression(lhsOut, s->args->lhs);
                lhsOut << ")->" << name(s->d);
                lhsOut.flush();
                emitAssignment(out, lhsText, s->d->getType(), s->args->rhs, s->d->getType());
                out << ";" << endl;
            }
            break;

        case IL_stvar:
            // ASSIG
            {
                out << ws(level);
                QString lhsText;
                QTextStream lhsOut(&lhsText);
                lhsOut << qualident(s->d);
                lhsOut.flush();
                emitAssignment(out, lhsText, s->d->getType(), s->args, s->d->getType());
            }
            out << ";" << endl;
            break;

        case IL_ret:
            if( inFinallyBody )
            {
                if( s->args )
                {
                    out << ws(level) << "{ f$->ret$ = ";
                    expressionAs(out, s->args, curProc->getType(), curProc->getType());
                    out << "; return; }" << endl;
                }else
                    out << ws(level) << "return;" << endl;
            }else
            {
                out << ws(level) << "return";
                if( s->args )
                {
                    out << " ";
                    expressionAs(out, s->args, curProc->getType(), curProc->getType());
                }
                out << ";" << endl;
            }
            break;

        case IL_raise:
            out << ws(level) << "MIC$$raise(";
            expression(out, s->args);
            out << ");" << endl;
            break;

        case IL_pop:
            out << ws(level);
            // we have to call this even if the result is discarded because args could be a function call
            expression(out, s->args);
            out << ";" << endl;
            break;

        case IL_free:
            out << ws(level) << "free(";
            expression(out, s->args);
            out << ");" << endl;
            break;

        case IL_label:
            out << ws(level) << escape(s->name) << ":" << endl;
            break;

        case IL_goto:
            out << ws(level) << "goto " << escape(s->name) << ";" << endl;
            break;

        case IL_line:
            // TODO
            break;

        case IL_strcpy: {
                Q_ASSERT( s->args && s->args->kind == Expression::Argument && s->args->lhs && s->args->rhs );
                out << ws(level) << "strncpy(";
                expression(out, s->args->lhs);
                Q_ASSERT( deref(s->args->lhs->getType())->isPtrToFixArray() );
                out << "->_";
                out << ", ";
                expression(out, s->args->rhs);
                out << ", ";
                Type* lhsT = deref(s->args->lhs->getType());
                Type* a = deref(lhsT->getType());
                Q_ASSERT( a->len );
                out << a->len << " - 1";
                out << "); ";
                expression(out, s->args->lhs);
                if( deref(s->args->lhs->getType())->isPtrToFixArray() )
                    out << "->_";
                out << "[" << a->len << " - 1] = 0;";
                out << endl;
            } break;

        default:
            qCritical() << "operation not implemented:" << s_opName[s->kind];
            Q_ASSERT(false);
        }

        s = s->next;
    }
}

void CeeGen::emitBinOP(QTextStream& out, Expression* e, const char* op)
{
    out << "(";
    expression(out, e->lhs);
    out << " " << op << " ";
    expression(out, e->rhs);
    out << ")";
}

void CeeGen::emitRelOP(QTextStream& out, Expression* e, const char* op)
{
    out << "(";
    expression(out, e->lhs);
    out << " " << op << " ";
    expression(out, e->rhs);
    out << ")";
}

void CeeGen::emitSoapInit(QTextStream& out, const QByteArray& name, Type* t, int level)
{
    t = deref(t);
    if( t->kind == Type::Pointer && t->pointerInit )
        out << ws(level) << name << " = NULL;" << endl;
    else if( t->isSUOA() )
    {
        if( t->pointerInit ) // it's cheaper to directly zero the whole thing
            out << ws(level) << "memset(&" << name << ", 0, sizeof(" << typeRef(t) << "));" << endl;
    }
    emitSoaInit(out, name, false, t, level);
}

void CeeGen::emitSoaInit(QTextStream& out, const QByteArray& name, bool nameIsPtr, Type* t, int level)
{
    t = deref(t);
    if( !t->objectInit )
        return;
    if( t->isSO() )
        out << ws(level) << qualident(t->decl) << "$init$(" << (nameIsPtr ? "" : "&") << name << ", 1);" << endl;
    else if( t->kind == Type::Array && t->len && deref(t->getType())->isSO() )
        out << ws(level) << qualident(deref(t->getType())->decl) << "$init$(" << name << (nameIsPtr ? "->" : ".") << "_, " << t->len << ");" << endl;
}

static void collectArgs(Expression* e, QList<Expression*>& args)
{
    if( e && e->kind == Expression::Argument )
    {
        if( e->next )
            collectArgs(e->next, args);
        if( e->lhs )
            args << e->lhs;
        args << e->rhs;
    }else if(e)
        args << e;
}

Type*CeeGen::deref(Type* t)
{
    if( t && t->kind == Type::NameRef )
        return deref(t->getType());
    else if( t )
        return t;
    else
        return mdl->getBasicType(Type::Undefined);
}

void CeeGen::emitInitializer(Type* t)
{
    t = deref(t);
    hout << "void " << qualident(t->decl) << "$init$(" << typeRef(t) << "* obj, unsigned int n);" << endl;
    bout << "void " << qualident(t->decl) << "$init$(" << typeRef(t) << "* obj, unsigned int n) {" << endl;
    // obj is a pointer to the object to be initialized; we can interpret this pointer as a single or an array of this object
    bout << ws(0) << "int i;" << endl;
    bout << ws(0) << "for( i = 0; i < n; i++ ) {" << endl; // this code is generalized to an array of this object of length n

    if( t->kind == Type::Object )
        bout << ws(1) << "obj[i].vtab$ = &" << qualident(t->decl) << "$vtab$;" << endl; // set the vptr to its class object
    else if( t->kind == Type::Array && t->len && t->objectInit )
    {
        Type* et = deref(t->getType());
        bout << ws(1) << qualident(et->decl) << "$init$(obj[i]._, " << t->len << ");" << endl;
    }

    foreach( Declaration* field, t->subs )
    {
        if( field->kind != Declaration::Field )
            continue;
        Type* tt = deref(field->getType());
        if( tt->objectInit && tt->isSO() )
        {
            bout << ws(1) << qualident(tt->decl) << "$init$(&obj[i]." << name(field) << ", 1);" << endl;
        }else if( tt->objectInit && tt->isA() )
        {
            Type* et = deref(tt->getType());
            if( et->isSO() )
                bout << ws(1) << qualident(tt->decl) << "$init$(obj[i]." << name(field) << ", " << tt->len << ");" << endl;
        }
    }

    bout << ws(0) << "}" << endl;
    bout << "}" << endl << endl;
}

void CeeGen::createLdindLocals(Statement * s)
{
    while( s )
    {
        if( s->hasE() )
            createLdindLocals(s->e);
        if( s->body )
            createLdindLocals(s->body);
        s = s->next;
    }
}

void CeeGen::createLdindLocals(Expression * e)
{
    while( e )
    {
        Type* t = deref(e->lhs ? e->lhs->getType() : 0);
        if( e->kind == IL_ldind && (t->isPtrToOpenCharArray() || t->kind == Type::StringLit ) )
        {
            if( !done.contains(e->d) )
            {
                done.insert(e->d);
                bout << ws(0) << typeRef(e->d->getType()) << " __tmp$" << typeRef(e->d->getType()) << ";" << endl;
            }
        }
        if( (e->kind == IL_iif || e->kind == IL_if || e->kind == IL_then || e->kind == IL_else ) && e->e != 0 )
            createLdindLocals(e->e);
        e = e->next;
    }
}

void CeeGen::prefix(QTextStream &out, Type * t)
{
    t = deref(t);
    if( !t->isPtrToOpenArray() )
        out << "(&";
    // TODO: check whether we should return the "_" member of fixed size arrays instead of &struct
}

void CeeGen::postfix(QTextStream &out, Type * t)
{
    t = deref(t);
    if( !t->isPtrToOpenArray() )
        out << ")";
}

void CeeGen::generateItabs(Statement *body)
{
    if( body == 0 )
        return;
    while( body )
    {
        generateItabs(body->body);
        // this is redundant to body->e:
        //generateItabs(body->args);
        if( body->hasE() )
            generateItabs(body->e);
        body = body->next;
    }
}

void CeeGen::generateItabs(Expression *e)
{
    while( e )
    {
        // this is redundant to e->e and e->next:
        // generateItabs(e->lhs);
        // generateItabs(e->rhs);
        if( e->hasE() )
            generateItabs(e->e);

        if( e->kind != IL_ldiface )
        {
            e = e->next;
            continue;
        }

        Type* cls = deref(e->lhs->getType());
        if( cls->kind == Type::Pointer )
            cls = deref(cls->getType());

        Type* iface = deref(e->d->getType());

        if( itabs.contains(qMakePair(iface,cls)) )
            return;
        itabs.insert(qMakePair(iface,cls));

        if( cls->kind == Type::Object )
        {
            // add one thunk per method which does the virtual dispatch on the object reference
            foreach( Declaration* proc, iface->subs )
            {
                Q_ASSERT( proc && proc->kind == Declaration::Procedure );
                proc = cls->findSubByName(proc->name);
                Q_ASSERT( proc && proc->kind == Declaration::Procedure );

                // TODO ifaceref or methref equality does not work with thunks and static itabs
                bout << "static " << typeRef(proc->getType()) << " ";
                bout << thunkName(iface,cls,proc);
                bout << "(";
                DeclList params = proc->getParams();
                for( int i = 0; i < params.size(); i++ )
                {
                    if( i != 0 )
                        bout << ", ";
                    parameter(bout, params[i]);
                }
                bout << ") {" << endl;
                bout << ws(0) << qualident(cls->decl) << "* $self = (" << qualident(cls->decl) << "*)self;" << endl;
                bout << ws(0) << "$self->vtab$->" << name(proc) << "(";
                for( int i = 0; i < params.size(); i++ )
                {
                    if( i != 0 )
                        bout << ", ";
                    bout << name(params[i]);
                }

                bout << ");" << endl;
                bout << "}" << endl << endl;
            }
            bout << "static const struct " << qualident(e->d) << "$Vtab$ " << itabName(iface,cls) << " = {" << endl;
            foreach( Declaration* proc, iface->subs )
            {
                Q_ASSERT( proc && proc->kind == Declaration::Procedure );
                bout << ws(0) << thunkName(iface,cls,proc) << "," << endl;
            }
            bout << "};" << endl;
        }else if( cls->kind == Type::Struct )
        {
            bout << "static const struct " << qualident(e->d) << "$Vtab$ " << itabName(iface,cls) << " = {" << endl;
            foreach( Declaration* proc, iface->subs )
            {
                Q_ASSERT( proc && proc->kind == Declaration::Procedure );
                proc = cls->findSubByName(proc->name);
                Q_ASSERT( proc && proc->kind == Declaration::Procedure );
                bout << ws(0) << qualident(proc) << "," << endl;
            }
            bout << "};" << endl;
        }else
            Q_ASSERT(false);

        bout << endl;

        e = e->next;
    }
}

void CeeGen::expression(QTextStream& out, Expression* e, Type *hint)
{
    bool decayFixArray = false;
    bool isPtr = false;

    // Detect if we are passing a fixed array to an open array/pointer
    if( hint && e && e->getType() )
    {
        Type* from = deref(e->getType());
        Type* to = deref(hint);

        if( from && to )
        {
            const bool fromFixVal = (from->kind == Type::Array && from->len != 0);
            const bool fromFixPtr = from->isPtrToFixArray();

            if( fromFixVal || fromFixPtr )
            {
                // Determine if the target type expects a C pointer/open array
                const bool toIsOpen = to->isPtrToOpenArray() ||
                                (to->kind == Type::Array && to->len == 0) ||
                                (to->kind == Type::Pointer && !to->isPtrToFixArray()) ||
                                to->kind == Type::StringLit ||
                                to->kind == Type::ByteArrayLit ||
                                to->kind == Type::INTPTR ||
                                to->kind == Type::Any;

                if( toIsOpen ) {
                    decayFixArray = true;
                    isPtr = fromFixPtr;
                }
            }
        }
    }

    if( decayFixArray )
        out << "(";

    switch(e->kind)
    {
    case IL_add:
        emitBinOP(out, e, "+");
        break;
    case IL_div_un:
    case IL_div:
        emitBinOP(out, e, "/"); // TODO: UN
        break;
    case IL_mul:
        emitBinOP(out, e, "*");
        break;
    case IL_rem:
    case IL_rem_un:
        emitBinOP(out, e, "%"); // TODO: UN
        break;
    case IL_sub:
        emitBinOP(out, e, "-");
        break;

    case IL_and:
        emitBinOP(out, e, "&");
        break;
    case IL_or:
        emitBinOP(out, e, "|");
        break;
    case IL_xor:
        emitBinOP(out, e, "^");
        break;

    case IL_shl:
        emitBinOP(out, e, "<<");
        break;
    case IL_shr_un:
    case IL_shr:
        emitBinOP(out, e, ">>"); // TODO: UN
        break;

    case IL_neg:
        out << "-";
        expression(out, e->lhs);
        break;

    case IL_abs:
        if( deref(e->lhs->getType())->isInteger() )
            out << "abs(";
        else
            out << "fabs(";
        expression(out, e->lhs);
        out << ")";
        break;

    case IL_not:
        out << "~";
        expression(out, e->lhs);
        break;

    case IL_ldc_i4_0:
    case IL_ldc_i4_1:
    case IL_ldc_i4_2:
    case IL_ldc_i4_3:
    case IL_ldc_i4_4:
    case IL_ldc_i4_5:
    case IL_ldc_i4_6:
    case IL_ldc_i4_7:
    case IL_ldc_i4_8:
    case IL_ldc_i4_m1:
    case IL_ldc_i4_s:
    case IL_ldc_i4:
    case IL_ldc_i8:
        out << e->i;
        break;

    case IL_ldc_r4:
    case IL_ldc_r8:
        out << QByteArray::number(e->f,'e',16); // empirically optimized, 17 is too much
        break;

    case IL_ldnull:
        out << "NULL";
        break;
    case IL_ldnull_ipp:
        {
            Type* t = deref(hint);
            if( hint && (t->kind == Type::Interface || (t->kind == Type::Proc && t->typebound)) )
                out << "(" << typeRef(hint) << "){ NULL, NULL }";
            else
                out << "{ NULL, NULL }";
        }
        break;


    case IL_ldstr:
    case IL_ldc_obj:
        constValue(out, e->c, hint);
        break;

    case IL_conv_i1:
    case IL_conv_i2:
    case IL_conv_i4:
    case IL_conv_u1:
    case IL_conv_u2:
    case IL_conv_u4:
    case IL_conv_i8:
    case IL_conv_u8:
    case IL_conv_r4:
    case IL_conv_r8:
        out << "((" << typeRef(Validator::tokToBasicType(mdl, e->kind)) << ")";
        expression(out, e->lhs);
        out << ")";
        break;

    case IL_cast_r4:
    case IL_cast_r8:
    case IL_cast_i8:
    case IL_cast_i4:
#if 0
        // this only works if e->lhs is an lvalue, but it could also be e.g. an arithmetic expression
        out << "(*(" << typeRef(Validator::tokToBasicType(mdl, e->kind)) << "*)&";
        expression(out, e->lhs);
        out << ")";
#else
        out << "((union { " << typeRef(e->lhs->getType()) << " from; "
            << typeRef(Validator::tokToBasicType(mdl, e->kind)) << " to; })"
            << "{ .from = ";
        expression(out, e->lhs);
        out << "}).to";
#endif
        break;


    case IL_ceq:
        {
            Type* lhsType = deref(e->lhs->getType());
            Type* rhsType = deref(e->rhs->getType());
            const bool lhsTwoWord = lhsType->kind == Type::Interface ||
                    (lhsType->kind == Type::Proc && lhsType->typebound) || lhsType->kind == Type::DBLNIL;
            const bool rhsTwoWord = rhsType->kind == Type::Interface ||
                    (rhsType->kind == Type::Proc && rhsType->typebound) || rhsType->kind == Type::DBLNIL;
            if( lhsTwoWord || rhsTwoWord )
            {
                Type* type = lhsType->kind == Type::DBLNIL ? rhsType : lhsType;
                if( type->kind == Type::DBLNIL )
                    emitRelOP(out, e, "==");
                else
                {
                    out << typeRef(type) << "$eq$(";
                    expression(out, e->lhs, type);
                    out << ", ";
                    expression(out, e->rhs, type);
                    out << ")";
                }
            }else
                emitRelOP(out, e, "==");
        }
        break;
    case IL_cgt_un: // TODO: UN
    case IL_cgt:
        emitRelOP(out, e, ">");
        break;
    case IL_clt_un: // TODO: UN
    case IL_clt:
        emitRelOP(out, e, "<");
        break;

    case IL_ldvar:
        out << qualident(e->d);
        break;
    case IL_ldvara:
        prefix(out, e->getType());
        out << qualident(e->d);
        postfix(out, e->getType());
        break;

    case IL_ldarg_0:
    case IL_ldarg_1:
    case IL_ldarg_2:
    case IL_ldarg_3:
    case IL_ldarg_s:
    case IL_ldarg:
        {
            DeclList params = curProc->getParams();
            Q_ASSERT( e->id < params.size() );
            out << paramRef(params[e->id]);
        }
        break;

    case IL_ldarga_s:
    case IL_ldarga:
        {
            DeclList params = curProc->getParams();
            Q_ASSERT( e->id < params.size() );
            prefix(out,e->getType());
            out << paramRef(params[e->id]);
            postfix(out,e->getType());;
        }
        break;

    case IL_ldloc_0:
    case IL_ldloc_1:
    case IL_ldloc_2:
    case IL_ldloc_3:
    case IL_ldloc_s:
    case IL_ldloc:
        {
            DeclList locals = curProc->getLocals();
            Q_ASSERT( e->id < locals.size() );
            out << localRef(locals[e->id]);
        }
        break;

    case IL_ldloca_s:
    case IL_ldloca:
        {
            DeclList locals = curProc->getLocals();
            Q_ASSERT( e->id < locals.size() );
            prefix(out,e->getType());
            out << localRef(locals[e->id]);
            postfix(out,e->getType());
        }
        break;

    case IL_ldind_i1:
    case IL_ldind_i2:
    case IL_ldind_i4:
    case IL_ldind_i8:
    case IL_ldind_ip:
    case IL_ldind_ipp:
    case IL_ldind_r4:
    case IL_ldind_r8:
    case IL_ldind_u1:
    case IL_ldind_u2:
    case IL_ldind_u4:
    case IL_ldind_u8:
    case IL_ldind:
        if( e->kind == IL_ldind && (deref(e->lhs->getType())->isPtrToOpenCharArray() || deref(e->lhs->getType())->kind == Type::StringLit) )
        {
            out << "(strncpy(" << " __tmp$" << typeRef(e->d->getType()) << "._, ";
            expression(out, e->lhs);
            out << ", " << e->d->getType()->len << " - 1)";
            out << ", __tmp$" << typeRef(e->d->getType()) << "._[" << e->d->getType()->len << " - 1] = 0";
            out << ", __tmp$" << typeRef(e->d->getType()) << ")";
        }else
        {
            Type* pointeeType = deref(e->lhs->getType())->getType();
            const bool castAddress = e->kind == IL_ldind && e->d &&
                        typeRef(e->d->getType()) != typeRef(pointeeType);
            if( castAddress )
                out << "(*(" << typeRef(e->d->getType()) << "*)(";
            else
                out << "(*";
            expression(out, e->lhs);
            if( castAddress )
                out << "))";
            else
                out << ")";
        }
        break;

    case IL_ldelem_i1:
    case IL_ldelem_i2:
    case IL_ldelem_i4:
    case IL_ldelem_i8:
    case IL_ldelem_ip:
    case IL_ldelem_r4:
    case IL_ldelem_r8:
    case IL_ldelem_u1:
    case IL_ldelem_u2:
    case IL_ldelem_u4:
    case IL_ldelem_u8:
    case IL_ldelem:
        {
            Type* arrayType = deref(e->lhs->getType())->getType();
            Type* elementType = arrayType->getType();
            const bool castAddress = e->kind == IL_ldelem && e->d &&
                        typeRef(e->d->getType()) != typeRef(elementType);
            if( castAddress )
                out << "(*(" << typeRef(e->d->getType()) << "*)&(";
            expression(out, e->lhs);
            if( deref(e->lhs->getType())->isPtrToFixArray() )
                out << "->_";
            out << "[";
            expression(out, e->rhs);
            out << "]";
            if( castAddress )
                out << "))";
        }
        break;

    case IL_ldelema:
        prefix(out,e->getType());
        expression(out, e->lhs);
        if( deref(e->lhs->getType())->isPtrToFixArray() )
            out << "->_";
        out << "[";
        expression(out, e->rhs);
        out << "]";
        postfix(out,e->getType());
        break;

    case IL_ldfld:
        out << "(";
        expression(out, e->lhs );
        out << "->" << name(e->d);
        out << ")";
        break;

    case IL_ldflda:
        prefix(out,e->getType());
        out << "(";
        expression(out, e->lhs );
        out << "->" << name(e->d);
        out << ")";
        postfix(out,e->getType());
        break;

    case IL_ldproc:
        out << qualident(e->d);
        break;

    case IL_ldiface: {
            Type* cls = deref(e->lhs->getType());
            if( cls->kind == Type::Pointer )
                cls = deref(cls->getType());

            Type* iface = deref(e->d->getType());
            out << "(" << typeRef(iface) << ")";
            out << "{";
            expression(out, e->lhs);
            out << ", &" <<  itabName(iface,cls) << " }";
        }break;

    case IL_ldmeth: {
            Declaration* method = e->d->forwardToProc();
            Type* cls = deref(e->lhs->getType());
            if( cls->kind == Type::Pointer )
                cls = deref(cls->getType());
            if( cls->kind == Type::Interface )
            {
                if( hint )
                    out << "(" << typeRef(hint) << ")";
                out << "{(_ptr$ = &";
                expression(out, e->lhs);
                out << ", ((" << typeRef(e->lhs->getType()) << "*)_ptr$)->self), ((" << typeRef(e->lhs->getType()) << "*)_ptr$)";
                out << "->vtab$->" << name(method) << "}";
            }else if( cls->kind == Type::Struct )
            {
                if( hint )
                    out << "(" << typeRef(hint) << ")";
                out << "{";
                expression(out, e->lhs);
                out << ", " << qualident(method) << "}";
            }else
            {
                if( hint )
                    out << "(" << typeRef(hint) << ")";
                out << "{(_ptr$ = ";
                expression(out, e->lhs);
                out << ", _ptr$), ((" << typeRef(e->lhs->getType()) << ")_ptr$)";
                out << "->vtab$->" << name(method) << "}";
            }
        } break;

    case IL_castptr:
        {
            Type* tt = deref(e->d->getType());
            if( tt->kind == Type::Array && tt->len == 0 )
            {
                const QByteArray targetRef = typeRef(e->getType());
                const QByteArray sourceRef = typeRef(e->lhs->getType());
                // TODO: if e->d->getType() is an open array, qualident(t->decl) is not declared.
                // despite this fix, there are still crashes in S9
                if( targetRef != sourceRef )
                    out << "(" << targetRef << ")(";
                expression(out, e->lhs );
                if( targetRef != sourceRef )
                    out << ")";
            }else
            {
                out << "((" << typeRef(e->d->getType()) << "*)";
                expression(out, e->lhs );
                out << ")";
            }
        } break;

    case IL_call:
    case IL_callinst:
        {
            Declaration* proc = e->d->forwardToProc();
            out << qualident(proc) << "(";
            QList<Expression*> args;
            if( e->kind == IL_callinst )
                args << e->lhs;
            collectArgs(e->rhs, args);
            QList<Declaration*> formals = proc->getParams(true);
            Q_ASSERT( args.size() == formals.size() );
            for( int i = 0; i < args.size(); i++ )
            {
                if( i != 0 )
                    out << ", ";
                expressionAs(out, args[i], formals[i]->getType(),
                             formals[i]->getType(), formals[i]->typebound);
            }
            out << ")";
        }
        break;

    case IL_callvirt:
        {
            Declaration* method = e->d->forwardToProc();
            QList<Declaration*> selfFormals = method->getParams(true);
            Type* selfType = selfFormals.isEmpty() ? 0 : selfFormals.first()->getType();
            const bool selfIsVoidPointer = !selfFormals.isEmpty() && selfFormals.first()->typebound;
            const int depth = callTempDepth++;
            const bool direct = isSimpleCallReceiver(e->lhs);
            if( direct )
            {
                out << "((" << typeRef(e->lhs->getType()) << ")";
                expression(out, e->lhs);
                out << ")->vtab$->" << name(method) << "(";
                expressionAs(out, e->lhs, selfType, selfType, selfIsVoidPointer);
            }else
            {
                out << "(_cv$" << depth << " = ";
                expression(out, e->lhs);
                out << ", ((" << typeRef(e->lhs->getType()) << ")_cv$" << depth << ")";
                out << "->vtab$->" << name(method) << "(_cv$" << depth;
            }
            QList<Expression*> args;
            collectArgs(e->rhs, args);
            QList<Declaration*> formals = method->getParams(false);
            Q_ASSERT( args.size() == formals.size() );
            for( int i = 0; i < args.size(); i++ )
            {
                out << ", ";
                expressionAs(out, args[i], formals[i]->getType(), formals[i]->getType());
            }
            out << (direct ? ")" : "))");
            --callTempDepth;
        }
        break;

    case IL_calli:
        {
            expression(out, e->lhs );
            Type* proc = deref(e->lhs->getType());
            out << "(";
            QList<Expression*> args;
            collectArgs(e->rhs, args);
            Q_ASSERT( args.size() == proc->subs.size() );
            for( int i = 0; i < args.size(); i++ )
            {
                if( i != 0 )
                    out << ", ";
                expressionAs(out, args[i], proc->subs[i]->getType(),
                             proc->subs[i]->getType(), proc->subs[i]->typebound);
            }
            out << ")";
        }
        break;

    case IL_pcall:
        {
            const int index = pcallIds.value(e, -1);
            Q_ASSERT(index >= 0);
            const QByteArray siteName = qualident(curProc) + "$pcall$" + QByteArray::number(index);
            const QList<Declaration*> formals = pcallFormals(e);
            QList<Expression*> args;
            collectArgs(e->rhs, args);
            Q_ASSERT(args.size() == formals.size());
            out << "MIC$$pcall(" << siteName << "$thunk, &(" << siteName << "){ ";
            expression(out, e->lhs);
            for( int i = 0; i < args.size(); ++i )
            {
                out << ", ";
                expressionAs(out, args[i], formals[i]->getType(), formals[i]->getType());
            }
            out << " })";
        }
        break;

    case IL_callmi:
        if( e->lhs && e->lhs->kind == IL_ldmeth && e->d && deref(e->d->getReceiver())->kind == Type::Interface )
        {
            Expression* receiver = e->lhs->lhs;
            const int depth = callTempDepth++;
            const bool direct = isSimpleCallReceiver(receiver);
            if( direct )
            {
                out << "((" << typeRef(receiver->getType()) << "*)&";
                expression(out, receiver);
                out << ")->vtab$->" << name(e->d);
                out << "(((" << typeRef(receiver->getType()) << "*)&";
                expression(out, receiver);
                out << ")->self";
            }else
            {
                out << "(_cv$" << depth << " = &";
                expression(out, receiver);
                out << ", ((" << typeRef(receiver->getType()) << "*)_cv$" << depth << ")->vtab$->" << name(e->d);
                out << "(((" << typeRef(receiver->getType()) << "*)_cv$" << depth << ")->self";
            }
            QList<Expression*> args;
            collectArgs(e->rhs, args);
            Type* proc = deref(receiver->getType());
            Q_ASSERT( args.size() == proc->subs.size() );
            for( int i = 0; i < args.size(); i++ )
            {
                out << ", ";
                expressionAs(out, args[i], proc->subs[i]->getType(),
                             proc->subs[i]->getType(), proc->subs[i]->typebound);
            }
            out << (direct ? ")" : "))");
            --callTempDepth;
        }else
        {
            const int depth = callTempDepth++;
            const bool direct = isSimpleCallReceiver(e->lhs);
            if( direct )
            {
                out << "((" << typeRef(e->lhs->getType()) << "*)&";
                expression(out, e->lhs, e->lhs->getType() );
                out << ")->proc(((";
                out << typeRef(e->lhs->getType()) << "*)&";
                expression(out, e->lhs, e->lhs->getType() );
                out << ")->self";
            }else
            {
                out << "(_cv$" << depth << " = &";
                expression(out, e->lhs, e->lhs->getType() );
                out << ", ((" << typeRef(e->lhs->getType()) << "*)_cv$" << depth << ")->proc(((";
                out << typeRef(e->lhs->getType()) << "*)_cv$" << depth << ")->self";
            }
            QList<Expression*> args;
            collectArgs(e->rhs, args);
            Type* proc = deref(e->lhs->getType());
            Q_ASSERT( args.size() == proc->subs.size() );
            for( int i = 0; i < args.size(); i++ )
            {
                out << ", ";
                expressionAs(out, args[i], proc->subs[i]->getType(),
                             proc->subs[i]->getType(), proc->subs[i]->typebound);
            }
            out << (direct ? ")" : "))");
            --callTempDepth;
        }
        break;


    case IL_newobj:
    case IL_newobj0:
    case IL_newobjgc:
        Q_ASSERT(e->getType()->kind == Type::Pointer);
        if( deref(e->getType())->objectInit ||
                (e->d && deref(e->d->getType())->objectInit) )
        {
            const QByteArray name = typeRef(e->d->getType());
            if( haveGc && e->kind == IL_newobjgc )
                out << "(_ptr$ = GC_MALLOC(sizeof(";
            else
                out << "(_ptr$ = calloc(1, sizeof(";
            out << name;
            out << ")),\n\t\t";
            out << name << "$init$(" << "((" + name + "*)_ptr$)" << ", 1), (";
            out << typeRef(e->getType());
            out << ")_ptr$)";
        }else
        {
            out << "(";
            out << typeRef(e->getType());
            if( haveGc && e->kind == IL_newobjgc )
                out << ")GC_MALLOC(sizeof(";
            else
                out << ")calloc(1, sizeof(";
            out << typeRef(e->getType()->getType());
            out << "))";
        }
        break;

    case IL_newarr:
    case IL_newarr0:
    case IL_newarrgc:
        if( deref(e->getType())->objectInit )
        {
            Type* et = deref(e->d->getType());
            const QByteArray name = typeRef(et);
            out << "(_len$ = ";
            expression(out, e->lhs);
            if( haveGc && e->kind == IL_newarrgc )
                out << ", _ptr$ = GC_MALLOC(_len$ * sizeof(";
            else
                out << ", _ptr$ = calloc(_len$,sizeof(";
            out << name << ")),\n\t\t";
            out << name << "$init$(" << "((" << name << "*)_ptr$)" << ", _len$), (";
            out << typeRef(et);
            out << "*)_ptr$)";
        }else
        {
            Type* et = deref(e->d->getType());
            out << "(";
            out << typeRef(et);
            if( haveGc && e->kind == IL_newarrgc )
            {
                out << "*)GC_MALLOC(";
                expression(out, e->lhs);
                out << " * sizeof(";
            }else {
                out << "*)calloc(";
                expression(out, e->lhs);
                out << ",sizeof(";
            }
            out << typeRef(et) << "))";
        }
        break;

#if 0
    case IL_initobj:
        {
        // obsolete
            Type* t = deref(e->d->getType());
            const QByteArray name = typeRef(t);
            out << "_ptr$ = ";
            expression(out, e->lhs );
            out << "; memset(_ptr$, 0, sizeof(" << name << "));";
            emitSoaInit(out, "((" + name + "*)_ptr$)", true, t, curLevel );
        }
        break;
#endif

    case IL_dup:
        expression(out, e->lhs);
        // TODO: temp instead of multiple eval of same exr
        break;

    case IL_nop:
        break;

    case IL_ptroff:
        out << "(";
        expression(out, e->lhs);
        out << " += ";
        expression(out, e->rhs);
        out << ")";
        break;

    case IL_iif: {
            Expression* if_ = e->e;
            Q_ASSERT(if_ && if_->kind == IL_if && if_->next->kind == IL_then && if_->next->next->kind == IL_else &&
                     if_->next->next->next == 0); // no IL_end
            Expression* then_ = if_->next;
            Expression* else_ = if_->next->next;
            out << "(";
            expression(out, if_->lhs);
            out << " ? ";
            expression(out, then_->lhs);
            out << " : ";
            expression(out, else_->lhs);
            out << ") ";
        } break;

    case IL_isinst: {
            out << "$isinst(&" << typeRef(e->d->getType()) << "$vtab$, ";
            expression(out, e->lhs );
            Q_ASSERT(deref(e->lhs->getType())->kind == Type::Pointer);
            // we pass the pointer to the instance, not the class pointer, because the former can be NULL
            out << ")";
        } break;

    case IL_sizeof:
    case IL_newvla:
        qWarning() << "TODO: CeeGen" << s_opName[e->kind] << "not implemented";
        break;

    default:
        Q_ASSERT(false);
    }

    if( decayFixArray )
    {
        if( isPtr )
            out << ")->_";
        else
            out << ")._";
    }
}

