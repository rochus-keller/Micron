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

#include "MicIde.h"
#include "MicHighlighter.h"
#include "MicAst.h"
#include "MicLexer.h"
#include "MicProject2.h"
#include "MilCeeGen.h"
#include <QtDebug>
#include <QDockWidget>
#include <QApplication>
#include <QStandardPaths>
#include <QDir>
#include <QDateTime>
#include <QSettings>
#include <QShortcut>
#include <QScrollBar>
#include <QMessageBox>
#include <QFileDialog>
#include <QFileInfo>
#include <QBuffer>
#include <QHeaderView>
#include <QLabel>
#include <QVBoxLayout>
#include <QDesktopWidget>
#include <QInputDialog>
#include <QTextBrowser>
#include <QProcess>
#include <QTreeWidget>
#include <stdarg.h>
#include <GuiTools/AutoMenu.h>
#include <GuiTools/CodeEditor.h>
#include <GuiTools/AutoShortcut.h>
#include <GuiTools/DocTabWidget.h>
#include <MonoTools/MonoEngine.h>
#include <MonoTools/MonoIlView.h>
using namespace Mic;
using namespace Mono;

#ifdef Q_OS_MAC
#define OBN_BREAK_SC "SHIFT+F8"
#define OBN_ABORT_SC "CTRL+SHIFT+Y"
#define OBN_CONTINUE_SC "CTRL+Y"
#define OBN_STEPIN_SC "CTRL+SHIFT+I"
#define OBN_STEPOVER_SC "CTRL+SHIFT+O"
#define OBN_STEPOUT_SC "CTRL+SHIFT+T"
#define OBN_ENDBG_SC "F4"
#define OBN_TOGBP_SC "F8"
#define OBN_GOBACK_SC "ALT+CTRL+Left"
#define OBN_GOFWD_SC "ALT+CTRL+Right"
#define OBN_NEXTDOC_SC "ALT+TAB"
#define OBN_PREVDOC_SC "ALT+SHIFT+TAB"
#else
#define OBN_BREAK_SC "SHIFT+F9"
#define OBN_ABORT_SC "SHIFT+F5"
#define OBN_CONTINUE_SC "F5"
#define OBN_STEPIN_SC "F11"
#define OBN_STEPOVER_SC "F10"
#define OBN_STEPOUT_SC "SHIFT+F11"
#define OBN_ENDBG_SC "F8"
#define OBN_TOGBP_SC "F9"
#define OBN_GOBACK_SC "ALT+Left"
#define OBN_GOFWD_SC "ALT+Right"
#define OBN_NEXTDOC_SC "CTRL+TAB"
#define OBN_PREVDOC_SC "CTRL+SHIFT+TAB"
#endif

class Ide::Editor : public CodeEditor
{
public:
    Editor(Ide* p, Project2* pro):CodeEditor(p),d_pro(pro),d_ide(p),dbgRow(0),dbgCol(0)
    {
        setCharPerTab(2);
        setTypingLatency(400);
        setPaintIndents(false);
        d_hl = new Highlighter( document() );
        updateTabWidth();
        QSettings set;
        if( !set.contains("CodeEditor/Font") )
        {
            QFont monospace("Monospace",9);
            if( !monospace.exactMatch() )
            {
                monospace = QFont("DejaVu Sans Mono",9);
                monospace.setStyleName("Book");
            }
            setFont(monospace);
        }
    }

    ~Editor()
    {
    }

    Ide* d_ide;
    Highlighter* d_hl;
    Project2* d_pro;
    int dbgRow, dbgCol;

    void setExt( bool on )
    {
        for( int i = Builtin::ABS; i < Builtin::Max; i++ )
            d_hl->addBuiltIn(Builtin::name[i]);
        for( int i = Type::Nil; i <= Type::MaxBasicType; i++ )
            d_hl->addBuiltIn(Type::name[i]);
        d_hl->addBuiltIn("ANYREC");
    }

    void clearBackHisto()
    {
        d_backHisto.clear();
    }

    void markNonTerms(const SymList& syms)
    {
        d_nonTerms.clear();
        QTextCharFormat format;
        format.setBackground( QColor(237,235,243) );
        foreach( Symbol* s, syms )
        {
            Declaration* ident = s->decl;
            Q_ASSERT( ident );
            QTextCursor c( document()->findBlockByNumber( qMax(int(s->pos.d_row - 1),0)) );
            c.setPosition( c.position() + qMax(int(s->pos.d_col - 1), 0) );
            int pos = c.position();
            c.setPosition( pos + s->len, QTextCursor::KeepAnchor );

            QTextEdit::ExtraSelection sel;
            sel.format = format;
            sel.cursor = c;

            d_nonTerms << sel;
        }
        updateExtraSelections();
    }

    void updateExtraSelections()
    {
        ESL sum;

        QTextEdit::ExtraSelection line;
        line.format.setBackground(QColor(Qt::yellow).lighter(170));
        line.format.setProperty(QTextFormat::FullWidthSelection, true);
        line.cursor = textCursor();
        line.cursor.clearSelection();
        sum << line;

        sum << d_nonTerms;

        if( !d_pro->errors.isEmpty() )
        {
            QTextCharFormat errorFormat;
            errorFormat.setUnderlineStyle(QTextCharFormat::WaveUnderline);
            errorFormat.setUnderlineColor(Qt::magenta);
            for( int i = 0; i < d_pro->errors.size(); i++ )
            {
                const Project2::Error& e = d_pro->errors.at(i);
                if( e.path != getPath() )
                    continue;
                QTextCursor c( document()->findBlockByNumber(e.pos.d_row - 1) );

                c.setPosition( c.position() + e.pos.d_col - 1 );
                c.movePosition(QTextCursor::EndOfWord, QTextCursor::KeepAnchor);

                QTextEdit::ExtraSelection sel;
                sel.format = errorFormat;
                sel.cursor = c;
                sel.format.setToolTip(e.msg);

                sum << sel;
            }
        }

        sum << d_link;

        if( d_ide->d_debugging && d_ide->d_status == Ide::Running && d_ide->d_mode != Ide::LineMode )
        {
            dbgRow = d_ide->d_curRow-1;
            dbgCol = d_ide->d_curCol-1;
            QTextEdit::ExtraSelection line;
            line.format.setBackground(QColor(Qt::yellow));
            line.format.setUnderlineStyle(QTextCharFormat::SingleUnderline);
            line.format.setUnderlineColor(Qt::red);
            line.cursor = QTextCursor(document()->findBlockByNumber(dbgRow));
            line.cursor.setPosition(line.cursor.position() + dbgCol);
            line.cursor.select(QTextCursor::WordUnderCursor);
            sum << line;
        }


        setExtraSelections(sum);
    }

    void mousePressEvent(QMouseEvent* e)
    {
        if( !d_link.isEmpty() )
        {
            QTextCursor cur = cursorForPosition(e->pos());
            d_ide->pushLocation( Ide::Location( getPath(), cur.blockNumber(), cur.positionInBlock(), verticalScrollBar()->value() ) );
            QApplication::restoreOverrideCursor();
            d_link.clear();
        }
        if( QApplication::keyboardModifiers() == Qt::ControlModifier )
        {
            QTextCursor cur = cursorForPosition(e->pos());
            Symbol* e = d_pro->findSymbolBySourcePos(
                        getPath(),cur.blockNumber() + 1,cur.positionInBlock() + 1);
            if( e )
            {
                Declaration* decl = e->decl;
                d_ide->pushLocation( Ide::Location( getPath(), cur.blockNumber(), cur.positionInBlock(), verticalScrollBar()->value() ) );
                if( decl->kind == Declaration::Import && e->pos == decl->pos )
                {
                    Import imp = decl->data.value<Import>();
                    decl = imp.resolved;
                }else if( decl->kind == Declaration::Procedure )
                {
                    Declaration* super = decl->data.value<Declaration*>();
                    if( decl->typebound && super )
                        decl = super;
                }
                d_ide->showEditor( decl, false, true );
                //setCursorPosition( sym->pos.d_row - 1, sym->pos.d_col - 1, true );
            }
            updateExtraSelections();
        }else
            QPlainTextEdit::mousePressEvent(e);
    }

    void mouseMoveEvent(QMouseEvent* e)
    {
        QPlainTextEdit::mouseMoveEvent(e);
        if( QApplication::keyboardModifiers() == Qt::ControlModifier )
        {
            QTextCursor cur = cursorForPosition(e->pos());
            Symbol* e = d_pro->findSymbolBySourcePos(
                        getPath(),cur.blockNumber() + 1,cur.positionInBlock() + 1);
            const bool alreadyArrow = !d_link.isEmpty();
            d_link.clear();
            if( e )
            {
                Declaration* sym = e->decl;
                Q_ASSERT(sym);
                const int off = cur.positionInBlock() + 1 - e->pos.d_col;
                cur.setPosition(cur.position() - off);
                cur.setPosition( cur.position() + sym->name.size(), QTextCursor::KeepAnchor );

                QTextEdit::ExtraSelection sel;
                sel.cursor = cur;
                sel.format.setFontUnderline(true);
                d_link << sel;
                /*
                d_linkLineNr = sym->pos.d_row - 1;
                d_linkColNr = sym->pos.d_col - 1;
                */
                if( !alreadyArrow )
                    QApplication::setOverrideCursor(Qt::ArrowCursor);
            }
            if( alreadyArrow && d_link.isEmpty() )
                QApplication::restoreOverrideCursor();
            updateExtraSelections();
        }else if( !d_link.isEmpty() )
        {
            QApplication::restoreOverrideCursor();
            d_link.clear();
            updateExtraSelections();
        }
    }

    void onUpdateModel()
    {
        markNonTerms(SymList());
    }
};

class Ide::DocTab : public DocTabWidget
{
public:
    DocTab(QWidget* p):DocTabWidget(p,false) {}

    // overrides
    bool isUnsaved(int i)
    {
        Ide::Editor* edit = static_cast<Ide::Editor*>( widget(i) );
        return edit->isModified();
    }

    bool save(int i)
    {
        Ide::Editor* edit = static_cast<Ide::Editor*>( widget(i) );
        if( !edit->saveToFile( edit->getPath(), false ) )
            return false;
        return true;
    }
};

static Ide* s_this = 0;

extern "C" {
    typedef int (*MIC$$PRINTF)(const char *fmt, va_list ap);
    extern MIC$$PRINTF MIC$$printf; // defined in MIC++.c

    static int my_logger(const char *fmt, va_list ap)
    {
        va_list apcopy;
        va_copy(apcopy, ap);
        const int size = vsnprintf(0, 0, fmt, ap);

        QByteArray buffer(size + 1, '\0');
        vsnprintf(buffer.data(), size + 1, fmt, apcopy);
        va_end(apcopy);
        s_this->logMessage( buffer, Ide::LogInfo, false);
        if( buffer.contains('\n') )
            QApplication::processEvents();
        return size;
    }
}

static void report(QtMsgType type, const QString& message )
{
    if( s_this )
    {
        switch(type)
        {
        case QtDebugMsg:
            // NOP s_this->logMessage(message);
            break;
        case QtWarningMsg:
            s_this->logMessage(message, Ide::LogWarning);
            break;
        case QtCriticalMsg:
        case QtFatalMsg:
            s_this->logMessage(message, Ide::LogError);
            break;
        }
    }
}

static QtMessageHandler s_oldHandler = 0;
void messageHander(QtMsgType type, const QMessageLogContext& ctx, const QString& message)
{
    if( s_oldHandler )
        s_oldHandler(type, ctx, message );
    report(type,message);
}

Ide::Ide(QWidget *parent)
    : QMainWindow(parent),d_lock(false),d_filesDirty(false),d_pushBackLock(false),
      d_lock2(false),d_lock3(false),d_lock4(false),d_debugging(false),d_ovflCheck(true),d_mode(LineMode),
      d_suspended(false),d_curRow(0),d_curCol(0),d_curThread(0),d_status(Idle),
      d_breakOnExceptions(false),d_noWarnings(false),d_incremental(false)
{
    s_this = this;

    d_pro = new Project2(this);

    d_dbg = new Debugger(this);
    d_eng = new Engine(this);
    d_eng->setToConsole(false);
    connect(d_eng, SIGNAL(onConsole(QString,bool)), this, SLOT(onConsole(QString,bool)) );
    connect(d_eng, SIGNAL(onError(QString)), this, SLOT(onError(QString)));
    connect(d_eng,SIGNAL(onFinished(int,bool)), this, SLOT(onFinished(int,bool)));
    connect(d_dbg, SIGNAL(sigEvent(DebuggerEvent)), this, SLOT(onDbgEvent(DebuggerEvent)));
    connect(d_dbg, SIGNAL(sigError(QString)), this, SLOT(onError(QString)) );

    d_tab = new DocTab(this);
    d_tab->setCloserIcon( ":/images/close.png" );
    Gui::AutoMenu* pop = new Gui::AutoMenu( d_tab, true );
    pop->addCommand( tr("Forward Tab"), d_tab, SLOT(onDocSelect()), tr(OBN_NEXTDOC_SC) );
    pop->addCommand( tr("Backward Tab"), d_tab, SLOT(onDocSelect()), tr(OBN_PREVDOC_SC) );
    pop->addCommand( tr("Close Tab"), d_tab, SLOT(onCloseDoc()), tr("CTRL+W") );
    pop->addCommand( tr("Close All"), d_tab, SLOT(onCloseAll()) );
    pop->addCommand( tr("Close All Others"), d_tab, SLOT(onCloseAllButThis()) );
    addTopCommands( pop );

    new Gui::AutoShortcut( tr(OBN_NEXTDOC_SC), this, d_tab, SLOT(onDocSelect()) );
    new Gui::AutoShortcut( tr(OBN_PREVDOC_SC), this, d_tab, SLOT(onDocSelect()) );
    new Gui::AutoShortcut( tr("CTRL+W"), this, d_tab, SLOT(onCloseDoc()) );

    connect( d_tab, SIGNAL( currentChanged(int) ), this, SLOT(onTabChanged() ) );
    connect( d_tab, SIGNAL(closing(int)), this, SLOT(onTabClosing(int)) );

    setDockNestingEnabled(true);
    setCorner( Qt::BottomRightCorner, Qt::RightDockWidgetArea );
    setCorner( Qt::BottomLeftCorner, Qt::LeftDockWidgetArea );
    setCorner( Qt::TopRightCorner, Qt::RightDockWidgetArea );
    setCorner( Qt::TopLeftCorner, Qt::LeftDockWidgetArea );

    createMods();
    createMod();
    createXref();
    createHier();
    createErrs();
    createIlView();
    createLocals();
    createStack();
    createTerminal();
    createModsMenu();

    setCentralWidget(d_tab);

    createMenuBar();

    s_oldHandler = qInstallMessageHandler(messageHander);
    MIC$$printf = my_logger;

    QSettings s;

    const QRect screen = QApplication::desktop()->screenGeometry();
    resize( screen.width() - 20, screen.height() - 30 ); // so that restoreState works
    if( s.value("Fullscreen").toBool() )
        showFullScreen();
    else
        showMaximized();

    const QVariant state = s.value( "DockState" );
    if( !state.isNull() )
        restoreState( state.toByteArray() );


    connect( d_pro,SIGNAL(sigRenamed()),this,SLOT(onCaption()) );
    connect( d_pro,SIGNAL(sigModified(bool)),this,SLOT(onCaption()) );
}
Ide::~Ide()
{
    QSettings s;
    s.setValue( "DockState", saveState() );
}

void Ide::loadFile(const QString& path)
{
    QFileInfo info(path);

    if( info.isDir() && info.suffix() != ".obxpro" )
    {
        d_pro->initializeFromDir( path );
    }else
    {
        d_pro->loadFrom(path);
    }

    QDir::setCurrent(QFileInfo(path).absolutePath());

    onCaption();

    compile(true);
}

void Ide::logMessage(const QString& msg, LogLevel l, bool addNewLine)
{
    if( l == LogError && msg.contains("warning", Qt::CaseInsensitive ) )
        l = LogWarning;

    QTextCharFormat f;
    switch( l )
    {
    case LogInfo:
        f.setForeground(Qt::black);
        break;
    case SysInfo:
        f.setForeground(Qt::darkGreen);
        break;
    case LogWarning:
        f.setForeground(Qt::blue);
        break;
    case LogError:
        f.setForeground(Qt::red);
        break;
    }
    d_term->setCurrentCharFormat(f);

    if( addNewLine )
        d_term->append(msg.trimmed());
    else
    {
        d_term->textCursor().movePosition(QTextCursor::End);
        d_term->textCursor().insertText(msg);
        if( d_term->verticalScrollBar() )
            d_term->verticalScrollBar()->setValue(d_term->verticalScrollBar()->maximum());
    }

}

void Ide::closeEvent(QCloseEvent* event)
{
    const bool ok = checkSaved( tr("Quit Application"));
    event->setAccepted(ok);
}

void Ide::createTerminal()
{
    QDockWidget* dock = new QDockWidget( tr("Output"), this );
    dock->setObjectName("Output");
    dock->setAllowedAreas( Qt::AllDockWidgetAreas );
    dock->setFeatures( QDockWidget::DockWidgetMovable | QDockWidget::DockWidgetClosable );
    d_term = new QTextBrowser(dock);
    dock->setWidget(d_term);
    addDockWidget( Qt::BottomDockWidgetArea, dock );
    new Gui::AutoShortcut( tr("CTRL+SHIFT+C"), this, d_term, SLOT(clear()) );
    connect( d_eng, SIGNAL(onConsole(QString,bool)), dock, SLOT(show()) );
}

void Ide::createIlView()
{
    QDockWidget* dock = new QDockWidget( tr("Bytecode"), this );
    dock->setVisible(false);
    dock->setObjectName("Bytecode");
    dock->setAllowedAreas( Qt::AllDockWidgetAreas );
    dock->setFeatures( QDockWidget::DockWidgetMovable | QDockWidget::DockWidgetClosable );
    QWidget* pane = new QWidget(dock);
    QVBoxLayout* vbox = new QVBoxLayout(pane);
    vbox->setMargin(0);
    vbox->setSpacing(0);
    d_ilTitle = new QLabel(pane);
    d_ilTitle->setMargin(2);
    d_ilTitle->setWordWrap(true);
    vbox->addWidget(d_ilTitle);
    d_il = new Mono::IlView(dock, d_dbg);
    vbox->addWidget(d_il);
    dock->setWidget(pane);
    addDockWidget( Qt::RightDockWidgetArea, dock );
}

void Ide::createMods()
{
    QDockWidget* dock = new QDockWidget( tr("Modules"), this );
    dock->setObjectName("Modules");
    dock->setAllowedAreas( Qt::AllDockWidgetAreas );
    dock->setFeatures( QDockWidget::DockWidgetMovable );
    d_mods = new QTreeWidget(dock);
    d_mods->setHeaderHidden(true);
    d_mods->setExpandsOnDoubleClick(false);
    d_mods->setAlternatingRowColors(true);
    dock->setWidget(d_mods);
    addDockWidget( Qt::LeftDockWidgetArea, dock );
    connect( d_mods, SIGNAL(itemDoubleClicked(QTreeWidgetItem*,int)),this,SLOT(onModsDblClicked(QTreeWidgetItem*,int)) );
}

void Ide::createMod()
{
    QDockWidget* dock = new QDockWidget( tr("Module"), this );
    dock->setObjectName("Module");
    dock->setAllowedAreas( Qt::AllDockWidgetAreas );
    dock->setFeatures( QDockWidget::DockWidgetMovable | QDockWidget::DockWidgetClosable );
    QWidget* pane = new QWidget(dock);
    QVBoxLayout* vbox = new QVBoxLayout(pane);
    vbox->setMargin(0);
    vbox->setSpacing(0);
    d_modTitle = new QLabel(pane);
    d_modTitle->setMargin(2);
    d_modTitle->setWordWrap(true);
    vbox->addWidget(d_modTitle);
    d_mod = new QTreeWidget(dock);
    d_mod->setHeaderHidden(true);
    d_mod->setExpandsOnDoubleClick(false);
    d_mod->setAlternatingRowColors(true);
    vbox->addWidget(d_mod);
    dock->setWidget(pane);
    addDockWidget( Qt::LeftDockWidgetArea, dock );
    connect( d_mod, SIGNAL(itemDoubleClicked(QTreeWidgetItem*,int)),this,SLOT(onModDblClicked(QTreeWidgetItem*,int)) );
}

void Ide::createHier()
{
    QDockWidget* dock = new QDockWidget( tr("Hierarchy"), this );
    dock->setObjectName("Hierarchy");
    dock->setAllowedAreas( Qt::AllDockWidgetAreas );
    dock->setFeatures( QDockWidget::DockWidgetMovable | QDockWidget::DockWidgetClosable );
    QWidget* pane = new QWidget(dock);
    QVBoxLayout* vbox = new QVBoxLayout(pane);
    vbox->setMargin(0);
    vbox->setSpacing(0);
    d_hierTitle = new QLabel(pane);
    d_hierTitle->setMargin(2);
    d_hierTitle->setWordWrap(true);
    vbox->addWidget(d_hierTitle);
    d_hier = new QTreeWidget(dock);
    d_hier->setHeaderHidden(true);
    d_hier->setExpandsOnDoubleClick(false);
    d_hier->setAlternatingRowColors(true);
    vbox->addWidget(d_hier);
    dock->setWidget(pane);
    addDockWidget( Qt::RightDockWidgetArea, dock );
    connect( d_hier, SIGNAL(itemDoubleClicked(QTreeWidgetItem*,int)),this,SLOT(onHierDblClicked(QTreeWidgetItem*,int)) );
}

void Ide::createErrs()
{
    QDockWidget* dock = new QDockWidget( tr("Issues"), this );
    dock->setObjectName("Issues");
    dock->setAllowedAreas( Qt::AllDockWidgetAreas );
    dock->setFeatures( QDockWidget::DockWidgetMovable | QDockWidget::DockWidgetClosable );
    d_errs = new QTreeWidget(dock);
    d_errs->setSizePolicy(QSizePolicy::MinimumExpanding,QSizePolicy::Preferred);
    d_errs->setAlternatingRowColors(true);
    d_errs->setHeaderHidden(true);
    d_errs->setSortingEnabled(false);
    d_errs->setAllColumnsShowFocus(true);
    d_errs->setRootIsDecorated(false);
    d_errs->setColumnCount(3);
    d_errs->header()->setSectionResizeMode(0, QHeaderView::ResizeToContents);
    d_errs->header()->setSectionResizeMode(1, QHeaderView::ResizeToContents);
    d_errs->header()->setSectionResizeMode(2, QHeaderView::Stretch);
    dock->setWidget(d_errs);
    addDockWidget( Qt::BottomDockWidgetArea, dock );
    connect(d_errs, SIGNAL(itemDoubleClicked(QTreeWidgetItem*,int)), this, SLOT(onErrorsDblClicked()) );
    connect( new QShortcut( tr("ESC"), this ), SIGNAL(activated()), dock, SLOT(hide()) );
}

void Ide::createXref()
{
    QDockWidget* dock = new QDockWidget( tr("Xref"), this );
    dock->setObjectName("Xref");
    dock->setAllowedAreas( Qt::AllDockWidgetAreas );
    dock->setFeatures( QDockWidget::DockWidgetMovable | QDockWidget::DockWidgetClosable );
    QWidget* pane = new QWidget(dock);
    QVBoxLayout* vbox = new QVBoxLayout(pane);
    vbox->setMargin(0);
    vbox->setSpacing(0);
    d_xrefTitle = new QLabel(pane);
    d_xrefTitle->setMargin(2);
    d_xrefTitle->setWordWrap(true);
    vbox->addWidget(d_xrefTitle);
    d_xref = new QTreeWidget(pane);
    d_xref->setAlternatingRowColors(true);
    d_xref->setHeaderHidden(true);
    d_xref->setAllColumnsShowFocus(true);
    d_xref->setRootIsDecorated(false);
    vbox->addWidget(d_xref);
    dock->setWidget(pane);
    addDockWidget( Qt::RightDockWidgetArea, dock );
    connect(d_xref, SIGNAL(itemDoubleClicked(QTreeWidgetItem*,int)), this, SLOT(onXrefDblClicked()) );
}

void Ide::createStack()
{
    QDockWidget* dock = new QDockWidget( tr("Stack"), this );
    dock->setVisible(false);
    dock->setObjectName("Stack");
    dock->setAllowedAreas( Qt::AllDockWidgetAreas );
    dock->setFeatures( QDockWidget::DockWidgetMovable | QDockWidget::DockWidgetClosable );
    d_stackView = new QTreeWidget(dock);
    d_stackView->setHeaderHidden(true);
    d_stackView->setAlternatingRowColors(true);
    d_stackView->setColumnCount(4); // Level, Name, Pos, Mod
    d_stackView->header()->setSectionResizeMode(0, QHeaderView::ResizeToContents);
    d_stackView->header()->setSectionResizeMode(1, QHeaderView::ResizeToContents);
    d_stackView->header()->setSectionResizeMode(2, QHeaderView::ResizeToContents);
    d_stackView->header()->setSectionResizeMode(3, QHeaderView::Stretch);
    dock->setWidget(d_stackView);
    addDockWidget( Qt::RightDockWidgetArea, dock );
    connect( d_stackView, SIGNAL(itemDoubleClicked(QTreeWidgetItem*,int)),this,SLOT(onStackDblClicked(QTreeWidgetItem*,int)) );
}

void Ide::createLocals()
{
    QDockWidget* dock = new QDockWidget( tr("Locals"), this );
    dock->setVisible(false);
    dock->setObjectName("Locals");
    dock->setAllowedAreas( Qt::AllDockWidgetAreas );
    dock->setFeatures( QDockWidget::DockWidgetMovable | QDockWidget::DockWidgetClosable );
    d_localsView = new QTreeWidget(dock);
    d_localsView->setHeaderHidden(true);
    d_localsView->setAlternatingRowColors(true);
    d_localsView->setColumnCount(2); // Name, Value
    d_localsView->header()->setSectionResizeMode(0, QHeaderView::ResizeToContents);
    d_localsView->header()->setSectionResizeMode(1, QHeaderView::Stretch);
    dock->setWidget(d_localsView);
    addDockWidget( Qt::RightDockWidgetArea, dock );
    connect( d_localsView, SIGNAL(itemExpanded(QTreeWidgetItem*)), this, SLOT(onLocalExpanded(QTreeWidgetItem*)));
}

void Ide::createModsMenu()
{
    Gui::AutoMenu* pop = new Gui::AutoMenu( d_mods, true );
    pop->addCommand( "Show File", this, SLOT(onOpenFile()) );
    pop->addAction("Expand all", d_mods, SLOT(expandAll()) );
    pop->addSeparator();
    pop->addCommand( "New Project", this, SLOT(onNewPro()), tr("CTRL+N"), false );
    pop->addCommand( "Open Project...", this, SLOT(onOpenPro()), tr("CTRL+O"), false );
    pop->addCommand( "Save Project", this, SLOT(onSavePro()), tr("CTRL+SHIFT+S"), false );
    pop->addCommand( "Save Project as...", this, SLOT(onSaveAs()) );
    pop->addSeparator();
    pop->addCommand( "New Module...", this, SLOT(onNewModule()), tr("CTRL+SHIFT+N"), false );
    pop->addCommand( "Add existing Modules...", this, SLOT(onAddFiles()) );
    pop->addCommand( "Remove Module...", this, SLOT(onRemoveFile()) );
    pop->addSeparator();
    pop->addCommand( "Add Import Path...", this, SLOT(onAddDir()) );
    pop->addCommand( "Remove Import Path...", this, SLOT(onRemoveDir()) );
    pop->addSeparator();
    pop->addCommand( "Set Build Directory...", this, SLOT( onBuildDir() ) );
    pop->addCommand( "Built-in Oakwood", this, SLOT(onOakwood()) );
    pop->addCommand( "Set Micron File System Root...", this, SLOT( onWorkingDir() ) );
    pop->addSeparator();
    pop->addCommand( "Check Syntax", this, SLOT(onParse()), tr("CTRL+T"), false );
    pop->addCommand( "Compile", this, SLOT(onCompile()), tr("CTRL+B"), false );
    pop->addCommand( "Set Command...", this, SLOT(onSetRunCommand()) );
    pop->addCommand( "Set Input File...", this, SLOT(onSetInputFile()) );
    pop->addSeparator();
    pop->addCommand( "Run on interpreter", this, SLOT(onInterpret()), tr("CTRL+SHIFT+R"), false);
    pop->addCommand( "Run on Mono", this, SLOT(onRun()), tr("CTRL+R"), false );
    addDebugMenu(pop);
    addTopCommands(pop);

    new Gui::AutoShortcut( tr("CTRL+O"), this, this, SLOT(onOpenPro()) );
    new Gui::AutoShortcut( tr("CTRL+N"), this, this, SLOT(onNewPro()) );
    new Gui::AutoShortcut( tr("CTRL+SHIFT+N"), this, this, SLOT(onNewModule()) );
    new Gui::AutoShortcut( tr("CTRL+SHIFT+S"), this, this, SLOT(onSavePro()) );
    new Gui::AutoShortcut( tr("CTRL+S"), this, this, SLOT(onSaveFile()) );
    new Gui::AutoShortcut( tr("CTRL+SHIFT+R"), this, this, SLOT(onInterpret()) );
    new Gui::AutoShortcut( tr("CTRL+R"), this, this, SLOT(onRun()) );
    new Gui::AutoShortcut( tr("CTRL+T"), this, this, SLOT(onParse()) );
    new Gui::AutoShortcut( tr("CTRL+B"), this, this, SLOT(onCompile()) );
    new Gui::AutoShortcut( tr(OBN_GOBACK_SC), this, this, SLOT(handleGoBack()) );
    new Gui::AutoShortcut( tr(OBN_GOFWD_SC), this, this, SLOT(handleGoForward()) );
    new Gui::AutoShortcut( tr(OBN_TOGBP_SC), this, this, SLOT(onToggleBreakPt()) );
    new Gui::AutoShortcut( tr(OBN_ENDBG_SC), this, this, SLOT(onEnableDebug()) );
    new Gui::AutoShortcut( tr(OBN_BREAK_SC), this, this, SLOT(onBreak()) );
    new Gui::AutoShortcut( tr(OBN_ABORT_SC), this, this, SLOT(onAbort()) );
    new Gui::AutoShortcut( tr(OBN_CONTINUE_SC), this, this, SLOT(onContinue()) );
    new Gui::AutoShortcut( tr(OBN_STEPIN_SC), this, this, SLOT(onStepIn()) );
    new Gui::AutoShortcut( tr(OBN_STEPOVER_SC), this, this, SLOT(onStepOver()) );
    new Gui::AutoShortcut( tr(OBN_STEPOUT_SC), this, this, SLOT(onStepOut()) );

}

void Ide::createMenuBar()
{
    Gui::AutoMenu* pop = new Gui::AutoMenu( tr("File"), this );
    pop->addCommand( "New Project", this, SLOT(onNewPro()), tr("CTRL+N"), false );
    pop->addCommand( "Open Project...", this, SLOT(onOpenPro()), tr("CTRL+O"), false );
    pop->addCommand( "Save Project", this, SLOT(onSavePro()), tr("CTRL+SHIFT+S"), false );
    pop->addCommand( "Save Project as...", this, SLOT(onSaveAs()) );
    pop->addSeparator();
    pop->addCommand( "Save", this, SLOT(onSaveFile()), tr("CTRL+S"), false );
    pop->addCommand( tr("Close file"), d_tab, SLOT(onCloseDoc()), tr("CTRL+W") );
    pop->addCommand( tr("Close all"), d_tab, SLOT(onCloseAll()) );
    pop->addCommand( "Convert all files to UTF-8...", this, SLOT(onConvertAllToUtf8()) );
    pop->addSeparator();
#ifdef QT_PRINTSUPPORT_LIB
    pop->addAutoCommand( "Print...", SLOT(handlePrint()), tr("CTRL+P"), true );
    pop->addAutoCommand( "Export PDF...", SLOT(handleExportPdf()), tr("CTRL+SHIFT+P"), true );
    pop->addSeparator();
#endif
    pop->addAction(tr("Quit"),qApp,SLOT(quit()), tr("CTRL+Q") );

    pop = new Gui::AutoMenu( tr("Edit"), this );
    pop->addAutoCommand( "Undo", SLOT(handleEditUndo()), tr("CTRL+Z"), true );
    pop->addAutoCommand( "Redo", SLOT(handleEditRedo()), tr("CTRL+Y"), true );
    pop->addSeparator();
    pop->addAutoCommand( "Cut", SLOT(handleEditCut()), tr("CTRL+X"), true );
    pop->addAutoCommand( "Copy", SLOT(handleEditCopy()), tr("CTRL+C"), true );
    pop->addAutoCommand( "Paste", SLOT(handleEditPaste()), tr("CTRL+V"), true );
    pop->addSeparator();
    pop->addAutoCommand( "Find...", SLOT(handleFind()), tr("CTRL+F"), true );
    pop->addAutoCommand( "Find again", SLOT(handleFindAgain()), tr("F3"), true );
    pop->addAutoCommand( "Replace...", SLOT(handleReplace()) );
    pop->addSeparator();
    pop->addAutoCommand( "&Go to line...", SLOT(handleGoto()), tr("CTRL+G"), true );
    pop->addSeparator();
    pop->addAutoCommand( "Indent", SLOT(handleIndent()) );
    pop->addAutoCommand( "Unindent", SLOT(handleUnindent()) );
    pop->addAutoCommand( "Fix Indents", SLOT(handleFixIndent()) );
    pop->addAutoCommand( "Set Indentation Level...", SLOT(handleSetIndent()) );

    pop = new Gui::AutoMenu( tr("Project"), this );
    pop->addCommand( "New Module...", this, SLOT(onNewModule()), tr("CTRL+SHIFT+N"), false );
    pop->addCommand( "Add existing Modules...", this, SLOT(onAddFiles()) );
    pop->addCommand( "Remove Module...", this, SLOT(onRemoveFile()) );
    pop->addSeparator();
    pop->addCommand( "Set Build Directory...", this, SLOT( onBuildDir() ) );
    pop->addCommand( "Built-in Oakwood", this, SLOT(onOakwood()) );
    pop->addCommand( "Set Configuration Variables...", this, SLOT( onSetOptions()) );
    pop->addCommand( "Set Micron File System Root...", this, SLOT( onWorkingDir() ) );

    pop = new Gui::AutoMenu( tr("Build && Run"), this );
    pop->addCommand( "Check Syntax", this, SLOT(onParse()), tr("CTRL+T"), false );
    pop->addCommand( "Compile", this, SLOT(onCompile()), tr("CTRL+B"), false );
    pop->addCommand( "Suppress Warnings", this, SLOT(onNoWarnings()) );
    //pop->addCommand( "Incremental Build (alpha!)", this, SLOT(onIncremental()) );
    pop->addCommand( "Set Command...", this, SLOT(onSetRunCommand()) );
    pop->addCommand( "Set Input File...", this, SLOT(onSetInputFile()) );
    pop->addSeparator();
    pop->addCommand( "Export MIL...", this, SLOT(onExportMil()) );
    pop->addCommand( "Export low-level bytecode...", this, SLOT(onExportLl()) );
    pop->addCommand( "Export ECMA-335 CIL...", this, SLOT(onExportCil()) );
    pop->addCommand( "Export LLVM IR...", this, SLOT(onExportLlvm()) );
    pop->addCommand( "Export C99...", this, SLOT(onExportC()) );
    pop->addSeparator();
    pop->addCommand( "Run on interpreter", this, SLOT(onInterpret()), tr("CTRL+SHIFT+R"), false);
    pop->addCommand( "Run on Mono", this, SLOT(onRun()), tr("CTRL+R"), false );

    pop = new Gui::AutoMenu( tr("Debug"), this );
    pop->addCommand( "Enable debugging", this, SLOT(onEnableDebug()),tr(OBN_ENDBG_SC), false );
    pop->addCommand( "Generate overflow checks", this, SLOT(onOvflChecks()) );
    pop->addCommand( "Bytecode mode", this, SLOT(onByteMode()) );
    pop->addCommand( "Row/column mode", this, SLOT(onRowColMode()) );
    pop->addSeparator();
    pop->addCommand( "Toggle breakpoint", this, SLOT(onToggleBreakPt()), tr(OBN_TOGBP_SC), false);
    pop->addCommand( "Remove all breakpoints", this, SLOT(onRemoveAllBreakpoints()));
    pop->addCommand( "Break on (all) exceptions", this, SLOT(onBreakOnExceptions()));
    pop->addSeparator();
    pop->addCommand( "Step in", this, SLOT(onStepIn()), tr(OBN_STEPIN_SC), false);
    pop->addCommand( "Step over", this, SLOT(onStepOver()), tr(OBN_STEPOVER_SC), false);
    pop->addCommand( "Step out", this, SLOT(onStepOut()), tr(OBN_STEPOUT_SC), false);
    pop->addCommand( "Break", this, SLOT(onBreak()), tr(OBN_BREAK_SC), false);
    pop->addCommand( "Continue", this, SLOT(onContinue()), tr(OBN_CONTINUE_SC), false);
    pop->addCommand( "Abort", this, SLOT(onAbort()), tr(OBN_ABORT_SC), false);

    pop = new Gui::AutoMenu( tr("Window"), this );
    pop->addCommand( tr("Next Tab"), d_tab, SLOT(onDocSelect()), tr(OBN_NEXTDOC_SC) );
    pop->addCommand( tr("Previous Tab"), d_tab, SLOT(onDocSelect()), tr(OBN_PREVDOC_SC) );
    pop->addSeparator();
    pop->addCommand( "Go Back", this, SLOT(handleGoBack()), tr(OBN_GOBACK_SC), false );
    pop->addCommand( "Go Forward", this, SLOT(handleGoForward()), tr(OBN_GOFWD_SC), false );
    pop->addSeparator();
    pop->addAutoCommand( "Set &Font...", SLOT(handleSetFont()) );
    pop->addAutoCommand( "Show &Linenumbers", SLOT(handleShowLinenumbers()) );
    pop->addCommand( "Show Fullscreen", this, SLOT(onFullScreen()) );
    pop->addSeparator();
    QMenu* sub2 = createPopupMenu();
    sub2->setTitle( tr("Show Window") );
    pop->addMenu( sub2 );

    Gui::AutoMenu* help = new Gui::AutoMenu( tr("Help"), this, true );
    help->addCommand( "&About this application...", this, SLOT(onAbout()) );
    help->addCommand( "&About Qt...", this, SLOT(onQt()) );
}

void Ide::onParse()
{
    ENABLED_IF( !d_pro->getFiles().isEmpty() && d_status == Idle);
    compile(QApplication::keyboardModifiers() == Qt::ShiftModifier);
}

void Ide::onCompile()
{
    ENABLED_IF( !d_pro->getFiles().isEmpty() &&  d_status == Idle);
    compile(QApplication::keyboardModifiers() == Qt::ShiftModifier,true);
}

void Ide::onRun()
{
    ENABLED_IF( !d_pro->getFiles().isEmpty() && d_status == Idle );

    if( run() && d_debugging )
        d_suspended = false;
}

void Ide::onInterpret()
{
    ENABLED_IF( !d_pro->getFiles().isEmpty() && d_status == Idle );

    logMessage("\nStarting application...\n\n",SysInfo,false);
    d_pro->interpret();
}

void Ide::onExportLl()
{
    ENABLED_IF( !d_pro->getFiles().isEmpty() && d_status == Idle );

    const QString dirPath = QFileDialog::getExistingDirectory(this, tr("Save Micron Low-Level Bytecode (LL)"), d_pro->getBuildDir(true) );

    if (dirPath.isEmpty())
        return;

    // TODO
    if( !compile(false,false) ) // otherwise allocated flag is already set after one generator run
        return;
    d_pro->interpret(dirPath);
}

void Ide::onAbort()
{
    ENABLED_IF(d_status == Running);

    if( d_debugging )
        d_dbg->exit();
    else
        d_eng->finish();
}

void Ide::onNewPro()
{
    ENABLED_IF(true);

    if( !checkSaved( tr("New Project")) )
        return;

    // we need a path up front because this path is also the first root path to the source code
    QString fileName = QFileDialog::getSaveFileName(this, tr("New Project"),
                                                          QFileInfo(d_pro->getProjectPath()).absolutePath(),
                                                          tr("Micron Project (*.micpro)") );

    if (fileName.isEmpty())
        return;

    QDir::setCurrent(QFileInfo(fileName).absolutePath());

    if( !fileName.endsWith(".obxpro",Qt::CaseInsensitive ) )
        fileName += ".obxpro";

    d_pro->createNew();
    d_tab->onCloseAll();
    compile(true);


    d_pro->saveTo(fileName);

}

void Ide::onOpenPro()
{
    ENABLED_IF( true );

    if( !checkSaved( tr("New Project")) )
        return;

    const QString fileName = QFileDialog::getOpenFileName(this, tr("Open Project"),QDir::currentPath(),
                                                          tr("Micron Project (*.micpro)") );
    if (fileName.isEmpty())
        return;

    QDir::setCurrent(QFileInfo(fileName).absolutePath());

    d_tab->onCloseAll();
    clear();
    d_pro->loadFrom(fileName);

    compile(true);
}

void Ide::onSavePro()
{
    ENABLED_IF( d_pro->isDirty() );

    if( !d_pro->getProjectPath().isEmpty() )
        d_pro->save();
    else
        onSaveAs();
}

void Ide::onSaveFile()
{
    Editor* edit = static_cast<Editor*>( d_tab->getCurrentTab() );
    ENABLED_IF( edit && edit->isModified() );

    edit->saveToFile( edit->getPath() );
    Project2::File* f = d_pro->findFile( edit->getPath() );
    if( f )
        f->d_cache.clear();
}

void Ide::onSaveAs()
{
    ENABLED_IF(true);

    QString fileName = QFileDialog::getSaveFileName(this, tr("Save Project"),
                                                          QFileInfo(d_pro->getProjectPath()).absolutePath(),
                                                          tr("Micron Project (*.micpro)") );

    if (fileName.isEmpty())
        return;

    QDir::setCurrent(QFileInfo(fileName).absolutePath());

    if( !fileName.endsWith(".obxpro",Qt::CaseInsensitive ) )
        fileName += ".obxpro";

    d_pro->saveTo(fileName);
    onCaption();
}

void Ide::onCaption()
{
    const QString star = d_pro->isDirty() || d_filesDirty ? "*" : "";
    if( d_pro->getProjectPath().isEmpty() )
    {
        setWindowTitle(tr("<unnamed>%2 - %1").arg(qApp->applicationName()).arg(star));
    }else
    {
        QFileInfo info(d_pro->getProjectPath());
        setWindowTitle(tr("%1%2 - %3").arg(info.fileName()).arg(star).arg(qApp->applicationName()) );
    }
}

void Ide::onFullScreen()
{
    CHECKED_IF(true,isFullScreen());
    QSettings s;
    if( isFullScreen() )
    {
        showMaximized();
        s.setValue("Fullscreen", false );
    }else
    {
        showFullScreen();
        s.setValue("Fullscreen", true );
    }
}

void Ide::onCursor()
{
    fillXref();
    if( d_lock )
        return;
    d_lock = true;
    Editor* edit = static_cast<Editor*>( d_tab->getCurrentTab() );
    if( edit )
    {
        int row, col;
        edit->getCursorPosition(&row,&col);
        //d_bcv->gotoLine(RowCol(row+1,col+1).packed());
    }
    d_lock = false;
}

void Ide::onExportMil()
{
    ENABLED_IF( d_pro->errors.isEmpty() );

    const QString dirPath = QFileDialog::getExistingDirectory(this, tr("Save Micron Intermediate Language (MIL)"), d_pro->getBuildDir(true) );

    if (dirPath.isEmpty())
        return;

    // TODO
    if( !compile(false,false) ) // otherwise allocated flag is already set after one generator run
        return;

    if( !d_pro->generateMil(dirPath) )
        QMessageBox::critical(this,tr("Save MIL"),tr("There was an error when generating IL; "
                                                   "see Output window for more information"));
}

void Ide::onExportLlvm()
{
    ENABLED_IF( d_pro->errors.isEmpty() );

    const QString dirPath = QFileDialog::getExistingDirectory(this, tr("Save LLVM Intermediate Language (LL)"), d_pro->getBuildDir(true) );

    if (dirPath.isEmpty())
        return;

    // TODO
    if( !compile(false,false) ) // otherwise allocated flag is already set after one generator run
        return;

    if( !d_pro->generateLlvm(dirPath) )
        QMessageBox::critical(this,tr("Save LLVM IR"),tr("There was an error when generating LL; "
                                                   "see Output window for more information"));
}

void Ide::onExportCil()
{
    ENABLED_IF( d_pro->errors.isEmpty() );

    const QString dirPath = QFileDialog::getExistingDirectory(this, tr("Save ECMA-335 CIL"), d_pro->getBuildDir(true) );

    if (dirPath.isEmpty())
        return;

    if( !compile(false,false) ) // otherwise allocated flag is already set after one generator run
        return;

    if( !d_pro->generateCil(dirPath) )
        QMessageBox::critical(this,tr("Save CIL"),tr("There was an error when generating IL; "
                                                   "see Output window for more information"));
}

void Ide::onExportC()
{
    ENABLED_IF( d_pro->errors.isEmpty() );

    const QString dirPath = QFileDialog::getExistingDirectory(this, tr("Save C"), d_pro->getBuildDir(true) );

    if (dirPath.isEmpty())
        return;

    if( !compile(false,false) ) // otherwise allocated flag is already set after one generator run
        return;

    if( !d_pro->generateC(dirPath) )
        QMessageBox::critical(this,tr("Save C"),tr("There was an error when generating C; "
                                                   "see Output window for more information"));
}

void Ide::onModsDblClicked(QTreeWidgetItem* item, int)
{
    Project2::File* s = item->data(0,Qt::UserRole).value<Project2::File*>();
    if( s == 0 )
        return;

    showEditor( s->d_filePath );
}

void Ide::onModDblClicked(QTreeWidgetItem* item, int)
{
    Declaration* s = item->data(0,Qt::UserRole).value<Declaration*>();
    if( s == 0 )
        return;

    d_lock2 = true;
    const QString path = d_tab->getCurrentDoc().toString();
    item->setExpanded(true);
    showEditor( path, s->pos.d_row, s->pos.d_col, false, true );
    d_lock2 = false;
}

void Ide::onHierDblClicked(QTreeWidgetItem* item, int)
{
    Declaration* s = item->data(0,Qt::UserRole).value<Declaration*>();
    if( s == 0 )
        return;

    d_lock4 = true;

    s = s->getModule();
    ModuleData md = s->data.value<ModuleData>();
    showEditor( md.source, s->pos.d_row, s->pos.d_col, false, true );
    item->setExpanded(true);
    d_lock4 = false;
}

void Ide::onStackDblClicked(QTreeWidgetItem* item, int)
{
    if( item == 0 )
        return;

    const QString source = item->data(3,Qt::UserRole).toString();
    const quint32 line = item->data(2,Qt::UserRole).toUInt();
    if( !source.isEmpty() )
    {
        showEditor( source, RowCol::unpackRow2(line), RowCol::unpackCol2(line) );
    }
    d_curLevel = item->data(0,Qt::UserRole).toInt();
    fillLocals();
}

static inline QString toString( const QVariantList& v )
{
    QString res;
    for( int i = 0; i < v.size(); i++ )
    {
        const QChar ch = v[i].toChar();
        // if( ch.isNull() )
        //    break;
        res += ch;
    }
    return res;
}

static inline QByteArray escapeRecordName( QByteArray name )
{
    if( name.startsWith('#') )
        return "<anonymous record>";
    //else
    name.replace('#','.');
    return name;
}

static void setValue( QTreeWidgetItem* item, const QVariant& var, Debugger* dbg )
{
    if( var.canConvert<ObjectRef>() )
    {
        ObjectRef r = var.value<ObjectRef>();
        switch(r.type)
        {
        case ObjectRef::Nil:
            item->setText(1,"nil");
            break;
        case ObjectRef::String:
            {
                const QString str = dbg->getString(r.id);
                item->setToolTip(1,str);
                if( str.length() > 32 )
                    item->setText(1,"\"" + str.left(32).simplified() + "...");
                else
                    item->setText(1,"\"" + str.simplified() + "\"");
            }
            break;
        case ObjectRef::Array:
        case ObjectRef::SzArray:
            {
                const quint32 len = dbg->getArrayLength(r.id);
                QVariantList vals;
                if( len )
                    vals = dbg->getArrayValues(r.id,1);
                if( !vals.isEmpty() && vals.first().type() == QVariant::Char )
                {
                    // This is an ARRAY OF CHAR
                    QVariantList vals = dbg->getArrayValues(r.id,len);
                    const QString raw = toString(vals);
                    QString str = raw;
                    bool anyNonPrint = false, anyNonAnsi = false;
                    for( int i = 0; i < str.size(); i++ )
                    {
                        if( !raw[i].isPrint() && !raw[i].isNull() )
                            anyNonPrint = true;
                        if( raw[i].unicode() > 255 )
                            anyNonAnsi = true;
                    }
                    QString tooltip;
                    if( anyNonPrint )
                    {
                        if( anyNonAnsi )
                        {
                            str = "$";
                            for( int i = 0; i < raw.size(); i++ )
                            {
                                if( i != 0 )
                                    str += " ";
                                str += QString::number( raw[i].unicode(), 16 );
                            }
                            str += "$";
                        }else
                            str = "$" + raw.toLatin1().toHex() + "$";
                        tooltip = str;
                    }else
                    {
                        tooltip = "\"" + str + "\"";
                        str = tooltip.simplified();
                    }

                    if( str.length() > 32 )
                        str =  str.left(32) + "...";
                    str += " (" + QString::number(raw.size()) + ")";

                    item->setToolTip(1,tooltip);
                    item->setText(1,str);
                }else
                {
                    item->setText(1,QString("<array length %1>").arg(len) );
                    item->setToolTip(1,item->text(1));
                    item->setChildIndicatorPolicy(QTreeWidgetItem::ShowIndicator);
                    item->setData(1,Qt::UserRole, var);
                    item->setData(1,Qt::UserRole+1, len);
                }
            }
            break;
        case ObjectRef::Class:
            {
                const quint32 type = dbg->getObjectType(r.id);
                item->setData(1,Qt::UserRole, var);
                item->setData(1,Qt::UserRole+1, type);
                // class is indeed an object
                if( type )
                {
                    const QByteArray name = dbg->getTypeInfo(type).spaceName();
                    if( name.startsWith('@') )
                    {
                        item->setText(1,"<procedure pointer>");
                        item->setToolTip(1,item->text(1));
                        break;
                    }
                    item->setText(1,escapeRecordName(name));
                    item->setToolTip(1,item->text(1));
                }else
                    item->setText(1,"<class>");
                item->setChildIndicatorPolicy(QTreeWidgetItem::ShowIndicator);
            }
            break;
        case ObjectRef::Object:
            item->setText(1,"<object>");
            item->setChildIndicatorPolicy(QTreeWidgetItem::ShowIndicator);
            item->setData(1,Qt::UserRole, var);
            break;
        case ObjectRef::Type:
            item->setText(1,"<type>");
            break;
        default:
            item->setText(1,"<unknown typeref>");
            break;
        }
    }else if( var.canConvert<UnmanagedPtr>() )
    {
        UnmanagedPtr ptr = var.value<UnmanagedPtr>();
#if 1
        if( sizeof(void*) == 4 )
            item->setText(1,QString("<pointer> 0x%1").arg(quint32(ptr.ptr&0xffffffff),0,16));
        else
            item->setText(1,QString("<pointer> 0x%1").arg(ptr.ptr,0,16));
#else
        if( ptr.ptr )
            item->setText(1,QString("<pointer>"));
        else
            item->setText(1,QString("<null>"));
#endif
        item->setToolTip(1,item->text(1));
    }else if( var.canConvert<ValueType>() )
    {
        //item->setData(1,Qt::UserRole, var);
#if 1
        ValueType v = var.value<ValueType>();
        const QByteArray name = dbg->getTypeInfo(v.cls).spaceName();
        if( !name.isEmpty() )
        {
            item->setText(1,escapeRecordName(name));
            item->setToolTip(1,item->text(1));
        }else
            item->setText(1,"<cstruct>");

        QList<Mono::Debugger::FieldInfo> names = dbg->getFields(v.cls,true,false);
        for( int i = 0; i < v.fields.size(); i++ )
        {
            QTreeWidgetItem* sub = new QTreeWidgetItem(item);
            if( i < names.size() )
                sub->setText(0,names[i].name);
            setValue(sub,v.fields[i],dbg);
            // TODO: also here the issue applies for unsafe records with embedded arrays
            // the values of fields after the array are wrong
        }
#else
        item->setText(1,"<cstruct>");
#endif
    }else if( var.isNull() )
        item->setText(1,"nil");
    else
    {
        int w = 2;
        switch( int(var.type()) )
        {
        case QMetaType::QChar:
            {
                const QChar ch = var.toChar();
                QString str;
                if( ch.isPrint() )
                    str = QString("'%1' ").arg(ch);
                const quint32 code = ch.unicode();
                str += QString("%1x %2").arg(code,2,16,QChar('0')).arg(code).toUpper();
                item->setText(1,str);
            }
            break;
        case QMetaType::Short:
        case QMetaType::UShort:
            w = 4;
        case QMetaType::Char:
        case QMetaType::SChar:
        case QMetaType::UChar:
            {
                const int v = var.toInt();
                const QChar ch(qAbs(v));
                QString str;
                str += QString("%1\t%2h").arg(v).arg(v,w,16,QChar('0'));
                if( ch.isPrint() )
                    str += QString(" '%1'").arg(ch);
                item->setText(1,str);
            }
            break;
        default:
            item->setText(1,var.toString());
            break;
        }
        item->setToolTip(1,item->text(1));
    }
}

static void expand(QTreeWidgetItem* item, Debugger* dbg)
{
    const QVariant var = item->data(1,Qt::UserRole);

    if( var.canConvert<ObjectRef>() )
    {
        ObjectRef r = var.value<ObjectRef>();
        switch(r.type)
        {
        case ObjectRef::SzArray:
        case ObjectRef::Array:
            {
                const int len = item->data(1,Qt::UserRole+1).toInt();
                QVariantList vals = dbg->getArrayValues(r.id,qMin(len, 55) );
                for( int i = 0; i < vals.size(); i++ )
                {
                    QTreeWidgetItem* sub = new QTreeWidgetItem(item);
                    sub->setText(0,QString("[%1]").arg(i) );
                    setValue(sub,vals[i],dbg);
                }
                item->setExpanded(true);
            }
            break;
        case ObjectRef::Class:
        case ObjectRef::Object:
            {
                QList<Debugger::FieldInfo> fields = dbg->getFields(item->data(1,Qt::UserRole+1).toUInt(),true,false);
                QList<quint32> ids;
                for( int i = 0; i < fields.size(); i++ )
                    ids << fields[i].id;
                QVariantList values = dbg->getValues(r.id,ids);
                for( int i = 0; i < fields.size(); i++ )
                {
                    QTreeWidgetItem* sub = new QTreeWidgetItem(item);
                    sub->setText(0,fields[i].name );
                    if( i < values.size() )
                        setValue(sub,values[i],dbg);
                    else
                        sub->setText(1,"<no value>");
                }
                item->sortChildren(0,Qt::AscendingOrder);
            }
            break;
        case ObjectRef::Type:
            {
                QList<Debugger::FieldInfo> fields = dbg->getFields(r.id, false, true);
                QList<quint32> ids;
                for( int i = 0; i < fields.size(); i++ )
                    ids << fields[i].id;
                QVariantList values = dbg->getValues(r.id,ids,true);
                for( int i = 0; i < fields.size(); i++ )
                {
                    QTreeWidgetItem* sub = new QTreeWidgetItem(item);
                    sub->setText(0,fields[i].name );
                    if( i < values.size() )
                        setValue(sub,values[i],dbg);
                    else
                        sub->setText(1,"<no value>");
                }
                item->sortChildren(0,Qt::AscendingOrder);
            }
        }
    }
}

void Ide::onLocalExpanded(QTreeWidgetItem* item)
{
    if( item == 0 || item->childCount() > 0 )
        return;

    expand(item,d_dbg);
}

void Ide::onTabChanged()
{
    const QString path = d_tab->getCurrentDoc().toString();

    onEditorChanged();

    if( !path.isEmpty() )
    {
        Project2::File* f = d_pro->findFile(path);
        if( f )
        {
            fillModule(f->d_mod);
            // showBc(d_rt->findByteCode(f.second));
            onCursor();
            return;
        }
    }
    // else
    fillModule(0);
}

void Ide::onTabClosing(int i)
{
    Project2::File* f = d_pro->findFile(d_tab->getDoc(i).toString());
    if( f ) // zero for oakwoods
        f->d_cache.clear();
}

void Ide::onEditorChanged()
{
    // only fired once when editor switches from unmodified to modified and back
    // not fired for every key press
    d_filesDirty = false;
    for( int i = 0; i < d_tab->count(); i++ )
    {
        Editor* e = static_cast<Editor*>( d_tab->widget(i) );
        if( e->isModified() )
            d_filesDirty = true;
        const QString path = d_tab->getDoc(i).toString();
        Project2::File* f = d_pro->findFile(path);
        QString name;
        if( f && f->d_mod )
            name = f->d_mod->name;
        else
            name = QFileInfo( path ).fileName();
        d_tab->setTabText(i, name + ( e->isModified() ? "*" : "" ) );
        d_tab->setTabToolTip( i, path );
    }
    onCaption();
}

void Ide::onErrorsDblClicked()
{
    QTreeWidgetItem* item = d_errs->currentItem();
    const int row = item->data(1, Qt::UserRole ).toInt();
    const int col = item->data(2, Qt::UserRole ).toInt();
    if( row == 0 || col == 0 )
        return;
    showEditor( item->data(0, Qt::UserRole ).toString(), row, col );
}

static bool errorEntryLessThan(const Project2::Error &s1, const Project2::Error &s2)
{
    if( s1.path < s2.path )
        return true;
    if( s1.path > s2.path )
        return false;
    if( s1.pos.d_row < s2.pos.d_row )
        return true;
    if( s1.pos.d_row > s2.pos.d_row )
        return false;
    if( s1.pos.d_col < s2.pos.d_col )
        return true;
    if( s1.pos.d_col > s2.pos.d_col )
        return false;
    return true; // s1.d_nr < s2.d_nr;
}

void Ide::onErrors()
{
    d_errs->clear();
    QList<Project2::Error> errs = d_pro->errors;
    std::sort(errs.begin(), errs.end(), errorEntryLessThan );

    for( int i = 0; i < errs.size(); i++ )
    {
#if 0
        // TODO
        if( d_noWarnings && !errs[i].d_isErr)
            continue;
#endif
        QTreeWidgetItem* item = new QTreeWidgetItem(d_errs);
        item->setText(2, errs[i].msg );
        item->setToolTip(2, item->text(2) );
        //if( errs[i].d_isErr )
            item->setIcon(0, QPixmap(":/images/exclamation-red.png") );
        //else
        //    item->setIcon(0, QPixmap(":/images/exclamation-circle.png") );
        Project2::File* f = d_pro->findFile(errs[i].path);
        if( f && f->d_mod )
            item->setText(0, f->d_mod->name );
        else
            item->setText(0, QFileInfo(errs[i].path).completeBaseName() );
        item->setToolTip(0, errs[i].path );
        item->setText(1, QString("%1:%2").arg(errs[i].pos.d_row).arg(errs[i].pos.d_col));
        item->setData(0, Qt::UserRole, errs[i].path );
        item->setData(1, Qt::UserRole, errs[i].pos.d_row );
        item->setData(2, Qt::UserRole, errs[i].pos.d_col );
    }
    if( errs.size() )
        d_errs->parentWidget()->show();

    for( int i = 0; i < d_tab->count(); i++ )
    {
        Editor* e = static_cast<Editor*>( d_tab->widget(i) );
        Q_ASSERT( e );
        e->updateExtraSelections();
    }
}

void Ide::onOpenFile()
{
    ENABLED_IF( d_mods->currentItem() );

    onModsDblClicked( d_mods->currentItem(), 0 );
}

void Ide::onOakwood()
{
    CHECKED_IF( true, d_pro->useBuiltInOakwood() );

    d_pro->setUseBuiltInOakwood( !d_pro->useBuiltInOakwood() );
}

void Ide::onAddFiles()
{
    ENABLED_IF(true);

    QByteArrayList path;

    QList<QTreeWidgetItem*> sel = d_mods->selectedItems();
    if( sel.size() == 1 && sel.first()->type() == 1 )
        path = sel.first()->text(0).toLatin1().split('.');

    QString filter;
    foreach( const QString& suf, d_pro->getSuffixes() )
        filter += " *" + suf;
    const QStringList files = QFileDialog::getOpenFileNames(this,tr("Add Modules"),QString(),filter );
    foreach( const QString& f, files )
    {
        if( !d_pro->addFile(f,path) )
            qWarning() << "cannot add module" << f;
    }
    compile(true);
}

void Ide::onNewModule()
{
    ENABLED_IF(true);

    QByteArrayList path;

    QList<QTreeWidgetItem*> sel = d_mods->selectedItems();
    if( sel.size() == 1 && sel.first()->type() == 1 )
        path = sel.first()->text(0).toLatin1().split('.');

    const QByteArray name = QInputDialog::getText(this,tr("New Module"), tr("Enter a unique module name:") ).toLatin1();
    if( name.isEmpty() )
        return;

    if( !Lexer::isValidIdent(name) )
    {
        QMessageBox::critical(this,tr("New Module"), tr("'%1' is not a valid module name").arg(name.constData()) );
        return;
    }
    QDir dir;
    const Project2::FileGroup* fg = d_pro->findFileGroup(path);
    for( int i = 0; i < fg->d_files.size(); i++ )
    {
        if( i == 0 )
            dir = fg->d_files[i]->d_filePath;
        if( fg->d_files[i]->d_mod != 0 && fg->d_files[i]->d_mod->name == name )
        {
            QString where;
            if( !path.isEmpty() )
                where = " in " + path.join('.');
            QMessageBox::critical(this,tr("New Module"), tr("'%1' is not unique%2")
                                  .arg(name.constData()).arg(where) );
            return;
        }
    }

    QString filePath = QFileDialog::getSaveFileName(this,tr("New Module"), dir.absoluteFilePath(name + ".obx"),"*.obx");
    if( filePath.isEmpty() )
        return;

    if( !filePath.toLower().endsWith(".obx") )
        filePath += ".obx";

    QFile f(filePath);
    if( !f.open(QIODevice::WriteOnly) )
    {
        QMessageBox::critical(this,tr("New Module"), tr("Cannot open file for writing: '%1'").arg(filePath) );
        return;
    }
    f.write("module ");
    f.write(name);
    f.write("\n\n\n");
    f.write("end ");
    f.write(name);
    f.write("\n");
    f.close();

    if( !d_pro->addFile(filePath,path) )
        qWarning() << "cannot add module" << filePath;
    compile(true);
}

void Ide::onAddDir()
{
    ENABLED_IF(true);

    bool ok;
    const QByteArray path = QInputDialog::getText(this,tr("Add Inport Path"), tr("Idents separated by '.':"),
                                                  QLineEdit::Normal, QString(), &ok ).toLatin1();
    if( !ok )
        return;
    if( !path.isEmpty() && !::isalpha( path[0] ) )
    {
        QMessageBox::critical(this,tr("Add Inport Path"),tr("import path starts with invalid character: %1").arg(path[0]));
        return;
    }
    for( int i = 1; i < path.size(); i++ )
    {
        const char ch = path[i];
        if( !::isalnum(ch) && ch != '.' )
        {
            QMessageBox::critical(this,tr("Add Inport Path"),tr("invalid character in import path: %1").arg(ch));
            return;
        }
    }
    QByteArrayList segments = path.split('.');
    for( int i = 0; i < segments.size(); i++ )
    {
        if( segments[i].isEmpty() )
        {
            QMessageBox::critical(this,tr("Add Inport Path"),tr("identifier cannot be empty"));
            return;
        }
    }
    d_pro->addPackagePath(segments);
    fillMods();
}

void Ide::onRemoveFile()
{
    QTreeWidgetItem* cur = d_mods->currentItem();
    ENABLED_IF( cur && cur->type() == 0 );

    Project2::File* f = cur->data(0,Qt::UserRole).value<Project2::File*>();
    if( f == 0 )
    {
        qWarning() << "remove file: invalid file item";
        return;
    }

    if( QMessageBox::warning( this, tr("Remove Module"),
                              tr("Do you really want to remove module '%1' from project?").arg(f->d_name.constData()),
                           QMessageBox::Yes | QMessageBox::Cancel, QMessageBox::Yes ) != QMessageBox::Yes )
        return;
    if( !d_pro->removeFile( f->d_filePath ) )
        qWarning() << "cannot remove module" << f->d_name;
    else
        compile(true);
}

void Ide::onRemoveDir()
{
    ENABLED_IF( d_mods->currentItem() && d_mods->currentItem()->type() == 1
                && d_mods->currentItem()->childCount() == 0 );

    if( QMessageBox::warning( this, tr("Remove Import Path"),
                              tr("Do you really want to remove '%1' from project?").arg(d_mods->currentItem()->text(0)),
                           QMessageBox::Yes | QMessageBox::Cancel, QMessageBox::Yes ) != QMessageBox::Yes )
        return;
    QByteArrayList path = d_mods->currentItem()->text(0).toLatin1().split('.');
    if( !d_pro->removePackagePath( path ) )
        qWarning() << "cannot remove import path" << d_mods->currentItem()->text(0);
    fillMods();
}

void Ide::onEnableDebug()
{
    CHECKED_IF( d_status == Idle, d_debugging );

    d_debugging = !d_debugging;
}

void Ide::onOvflChecks()
{
    CHECKED_IF( d_status == Idle, d_ovflCheck );

    d_ovflCheck = !d_ovflCheck;
}

void Ide::onBreak()
{
    //ENABLED_IF(!d_suspended && d_mode == Running && d_debugging);

    //d_dbg->addBreakpoint(0,0);
    // we cannot support break yet because Mono might stop at a place where a step command leads to a VM crash.
    //d_suspended = true;
    //d_dbg->suspend();
}

bool Ide::checkSaved(const QString& title)
{
    if( d_filesDirty )
    {
        switch( QMessageBox::critical( this, title, tr("There are modified files; do you want to save them?"),
                               QMessageBox::Yes | QMessageBox::No | QMessageBox::Cancel, QMessageBox::Yes ) )
        {
        case QMessageBox::Yes:
            // TODO
            break;
        case QMessageBox::No:
            break;
        default:
            return false;
        }
    }
    if( d_pro->isDirty() )
    {
        switch( QMessageBox::critical( this, title, tr("The the project has not been saved; do you want to save it?"),
                               QMessageBox::Yes | QMessageBox::No | QMessageBox::Cancel, QMessageBox::Yes ) )
        {
        case QMessageBox::Yes:
            if( !d_pro->getProjectPath().isEmpty() )
                return d_pro->save();
            else
            {
                const QString path = QFileDialog::getSaveFileName( this, title, QString(), "Micron Project (*.micpro)" );
                if( path.isEmpty() )
                    return false;
                QDir::setCurrent(QFileInfo(path).absolutePath());
                return d_pro->saveTo(path);
            }
            break;
        case QMessageBox::No:
            return true;
        default:
            return false;
        }
    }
    return true;
}

#if 0
// TODO
static bool preloadLib( Project2* pro, const QByteArray& name )
{
    bool found;
    pro->getFc()->getFile(name, &found);
    if( found )
        return true;
    QFile f( QString(":/oakwood/%1.Def" ).arg(name.constData() ) );
    if( !f.open(QIODevice::ReadOnly) )
    {
        qCritical() << "unknown preload" << name;
        return false;
    }
    pro->getFc()->addFile( name, f.readAll(), true );
    return true;
}
#endif

bool Ide::compile(bool all, bool doGenerate )
{
    if( !d_incremental )
        all = true;
    for( int i = 0; i < d_tab->count(); i++ )
    {
        Editor* e = static_cast<Editor*>( d_tab->widget(i) );
        Project2::File* f = d_pro->findFile( e->getPath() );
        if( f == 0 )
            continue;
        if( e->isModified() )
            f->d_cache = e->toPlainText().toUtf8();
        else
            f->d_cache.clear();
    }

    const QTime start = QTime::currentTime();
    d_status = Compiling;
    const bool res = d_pro->parse(); // !all);
    d_status = Idle;
    qDebug() << "recompiled" << d_pro->getFiles().size() << "files with" << d_pro->getSloc() << "SLOC in"
             << start.msecsTo(QTime::currentTime()) << "[ms]";
    if( res && doGenerate )
    {
       if( !generate(all) )
           QMessageBox::critical(this,tr("Compiler"),tr("There was an error when generating an assembly; "
                                                        "see Output window for more information"));
    }
    onErrors();
    fillMods();
    fillModule(0);
    fillHier(0);
    fillXref();
    onTabChanged();
    return true;
}

bool Ide::generate(bool forceAll)
{
    if( d_status != Idle )
        return false;

#if 0
    // TODO

    // NOTE fastasm seems still to make problems, maybe related to the mscorelib.dll version;
    // the same code which issues a runtime exception runs without problems when compiled with
    // ILASM or Pelib; so somehow fastasm seems to generate wrong code or meta for the same IL.
    // Anyway we can do well without fastasm because neither fastasm nor ILASM generate useful MDBs.
    const CilGen::How how = CilGen::Pelib; // CilGen::Fastasm; CilGen::Ilasm
    // Pelib is factor 1.4 faster than Fastasm for generating the IL and factor ~3 incl. IL to assembly compilation;
    // compared to ilasm.exe for compilation (instead of fastasm.exe) Pelib is even a factor 29 faster.

    const QString buildPath = d_pro->getBuildDir(true);
    QDir buildDir(buildPath);
    if( !buildDir.mkpath(buildPath) )
    {
        QMessageBox::critical(this, tr("Generating Assemblies"), tr("Cannot create build directory '%1'").arg(buildPath));
        return false;
    }
    QFile test(buildDir.absoluteFilePath("__obx_probe__"));
    if( !test.open(QIODevice::WriteOnly) )
    {
        QMessageBox::critical(this, tr("Generating Assemblies"), tr("Cannot write to files in '%1'").arg(buildPath));
        return false;
    }
    test.remove();

    if( how == CilGen::Fastasm && !checkEngine(true) )
        return false;

    const QTime start = QTime::currentTime();
    d_status = Generating;
    const bool ok = CilGen::translateAll(d_pro, how, d_debugging && d_ovflCheck, buildPath, forceAll );
    qDebug() << "generated in" << start.msecsTo(QTime::currentTime()) << "[ms]";

    if( ok && how == CilGen::Fastasm )
    {
        logMessage("\nGenerating application...\n\n",SysInfo,false);
        QDir monoDir( d_eng->getMonoDir() );
        d_eng->init();
        d_eng->setWorkDir(buildPath);
        d_eng->run( monoDir.absoluteFilePath("fastasm.exe"), QStringList() << buildDir.absoluteFilePath("batch") );
        if( !d_debugging )
        {
            QFile files(buildDir.absoluteFilePath("modules"));
            if( files.open(QIODevice::ReadOnly) )
            {
                while( !files.atEnd() )
                {
                    const QByteArray line = files.readLine().trimmed();
                    const QString fileName = buildDir.absoluteFilePath(QString::fromUtf8(line));
                    QFile::remove(fileName+".dll.mdb");
                    QFile::remove(fileName+".exe.mdb");
                }
            }
        }
    }else
        d_status = Idle;

    return ok;
#endif
    return false;
}

bool Ide::run()
{
    if( d_status != Idle )
        return false;
    if( !checkEngine() )
        return false;
    QDir buildDir( d_pro->getBuildDir(true) );
    if( d_debugging && buildDir.entryList(QStringList() << "*.mdb", QDir::Files ).isEmpty() )
    {
        logMessage("No debugging information found, using bytecode level debugger",SysInfo);
        d_mode = BytecodeMode;
    }
    logMessage("\nStarting application...\n\n",SysInfo,false);
    d_eng->init( d_debugging ? d_dbg->open() : 0 );
    d_eng->setAssemblySearchPaths( QStringList() << d_pro->getBuildDir(true), true );
    d_eng->setEnv( "OBERON_FILE_SYSTEM_ROOT", d_pro->getWorkingDir(true) );
    d_eng->run( buildDir.absoluteFilePath("Main#.exe"));
    d_status = Running;
    return true;
}

static bool sortNamed( Project2::File* lhs, Project2::File* rhs )
{
    return lhs->d_name.toLower() < rhs->d_name.toLower();
}

typedef QPair<QByteArray,QList<Project2::File*> > Group;

static bool sortNamed1( const Group& lhs, const Group& rhs )
{
    return lhs.first.toLower() < rhs.first.toLower();
}

template<class T>
static void fillModTree( T* parent, const QList<Project2::File*>& mods )
{
    foreach( Project2::File* m, mods )
    {
        QTreeWidgetItem* item = new QTreeWidgetItem(parent);
        item->setText(0, m->d_name);
        item->setToolTip(0,m->d_filePath);
#if 0
        if( m->d_isDef )
            item->setIcon(0, QPixmap(":/images/definition.png") );
        else
#endif
            item->setIcon(0, QPixmap(":/images/module.png") );
        item->setData(0,Qt::UserRole,QVariant::fromValue(m));
    }
}

void Ide::fillMods()
{
    d_mods->clear();

    const Project2::FileGroups& paths = d_pro->getFileGroups();
    typedef QList<Group> Sort1;
    Sort1 sortFg;
    foreach( const Project2::FileGroup& fg, paths )
        sortFg.append( qMakePair( fg.d_package.join('.'), fg.d_files ) );
    std::sort( sortFg.begin(), sortFg.end(), sortNamed1 );

    for( int j = 0; j < sortFg.size(); j++ )
    {
        QTreeWidgetItem* item = 0;
        if( !sortFg[j].first.isEmpty() )
        {
            item = new QTreeWidgetItem(d_mods,1);
            item->setText(0, sortFg[j].first);
            item->setToolTip( 0, item->text(0) );
            item->setIcon(0, QPixmap(":/images/folder.png") );
        }

        QList<Project2::File*> files = sortFg[j].second;
        std::sort( files.begin(), files.end(), sortNamed );
        if( item )
            fillModTree(item,files);
        else
            fillModTree( d_mods, files );
    }
    d_mods->expandAll();
}

void Ide::addTopCommands(Gui::AutoMenu* pop)
{
    Q_ASSERT( pop != 0 );
    pop->addSeparator();
    pop->addCommand( "Go Back", this, SLOT(handleGoBack()), tr(OBN_GOBACK_SC), false );
    pop->addCommand( "Go Forward", this, SLOT(handleGoForward()), tr(OBN_GOFWD_SC), false );
    pop->addSeparator();
    pop->addAutoCommand( "Set &Font...", SLOT(handleSetFont()) );
    pop->addAutoCommand( "Show &Linenumbers", SLOT(handleShowLinenumbers()) );
    pop->addCommand( "Show Fullscreen", this, SLOT(onFullScreen()) );
    pop->addSeparator();
    pop->addAction(tr("Quit"),qApp,SLOT(quit()) );
}

Ide::Editor* Ide::showEditor(const QString& path, int row, int col, bool setMarker, bool center )
{
    if( path.isEmpty() )
        return 0;

    QString filePath = path;
    Project2::File* f = d_pro->findFile(path);
    if( f != 0 )
        filePath = f->d_filePath;

    const int i = d_tab->findDoc(filePath);
    Editor* edit = 0;
    if( i != -1 )
    {
        d_tab->setCurrentIndex(i);
        edit = static_cast<Editor*>( d_tab->widget(i) );
    }else
    {
        edit = new Editor(this,d_pro);
        createModsMenu(edit);

        connect(edit, SIGNAL(modificationChanged(bool)), this, SLOT(onEditorChanged()) );
        connect(edit,SIGNAL(cursorPositionChanged()),this,SLOT(onCursor()));
        connect(edit,SIGNAL(sigUpdateLocation(int,int)),this,SLOT(onUpdateLocation(int,int)));

        edit->setExt(true);
        QFile in(filePath);
        if( in.open(QIODevice::ReadOnly) )
        {
            const QByteArray text = Lexer::extractText(&in);

            QBuffer b;
            b.buffer() = text;
            b.open(QIODevice::ReadOnly);
            edit->loadFromFile(&b, filePath);
        }else
            qWarning() << "cannot open file for reading" << filePath;

        if( f && f->d_mod )
        {
            const QSet<quint32> lines = d_breakPoints.value(f->d_mod->name);
            foreach( quint32 line, lines )
                edit->addBreakPoint(line - 1);
        }

        d_tab->addDoc(edit,filePath);
        onEditorChanged();
    }
    // showBc( d_rt->findByteCode(f.second) );
    //qDebug() << "show" << row << col << path;
    if( row > 0 && col > 0 )
    {
        d_curRow = row;
        d_curCol = col;
        edit->setCursorPosition( row-1, col-1, center );
        if( setMarker )
            edit->setPositionMarker(row-1);
    }
    edit->setFocus();
    return edit;
}

void Ide::showEditor(Declaration *n, bool setMarker, bool center)
{
    Declaration* mod = n->getModule();
    if( mod )
        showEditor( mod->data.value<ModuleData>().source, n->pos.d_row, n->pos.d_col, setMarker, center );
}

void Ide::showEditor(const Ide::Location& loc)
{
    Editor* e = showEditor( loc.d_file, loc.d_line+1, loc.d_col+1 );
    if( e )
        e->verticalScrollBar()->setValue(loc.d_yoff);
}

void Ide::createModsMenu(Ide::Editor* edit)
{
    Gui::AutoMenu* pop = new Gui::AutoMenu( edit, true );
    pop->addCommand( "Save", this, SLOT(onSaveFile()), tr("CTRL+S"), false );
    pop->addSeparator();
    pop->addCommand( "Compile", this, SLOT(onCompile()), tr("CTRL+B"), false );
    pop->addCommand( "Export MIL...", this, SLOT(onExportMil()) );
    pop->addCommand( "Export ECMA-335 CIL...", this, SLOT(onExportCil()) );
    pop->addCommand( "Export LLVM IR...", this, SLOT(onExportLlvm()) );
    pop->addCommand( "Export C99...", this, SLOT(onExportC()) );
    pop->addSeparator();
    pop->addCommand( "Run on interpreter", this, SLOT(onInterpret()), tr("CTRL+SHIFT+R"), false);
    pop->addCommand( "Run on Mono", this, SLOT(onRun()), tr("CTRL+R"), false );
    addDebugMenu(pop);
    pop->addSeparator();
    pop->addCommand( "Undo", edit, SLOT(handleEditUndo()), tr("CTRL+Z"), true );
    pop->addCommand( "Redo", edit, SLOT(handleEditRedo()), tr("CTRL+Y"), true );
    pop->addSeparator();
    pop->addCommand( "Cut", edit, SLOT(handleEditCut()), tr("CTRL+X"), true );
    pop->addCommand( "Copy", edit, SLOT(handleEditCopy()), tr("CTRL+C"), true );
    pop->addCommand( "Paste", edit, SLOT(handleEditPaste()), tr("CTRL+V"), true );
    pop->addSeparator();
    pop->addCommand( "Find...", edit, SLOT(handleFind()), tr("CTRL+F"), true );
    pop->addCommand( "Find again", edit, SLOT(handleFindAgain()), tr("F3"), true );
    pop->addCommand( "Replace...", edit, SLOT(handleReplace()) );
    pop->addSeparator();
    pop->addCommand( "&Goto...", edit, SLOT(handleGoto()), tr("CTRL+G"), true );
    pop->addSeparator();
    pop->addCommand( "Indent", edit, SLOT(handleIndent()) );
    pop->addCommand( "Unindent", edit, SLOT(handleUnindent()) );
    pop->addCommand( "Fix Indents", edit, SLOT(handleFixIndent()) );
    pop->addCommand( "Set Indentation Level...", edit, SLOT(handleSetIndent()) );
    pop->addSeparator();
#ifdef QT_PRINTSUPPORT_LIB
    pop->addCommand( "Print...", edit, SLOT(handlePrint()), tr("CTRL+P"), true );
    pop->addCommand( "Export PDF...", edit, SLOT(handleExportPdf()), tr("CTRL+SHIFT+P"), true );
#endif
    addTopCommands(pop);
}

void Ide::addDebugMenu(Gui::AutoMenu* pop)
{
    Gui::AutoMenu* sub = new Gui::AutoMenu(tr("Debugger"), this, false );
    pop->addMenu(sub);
    sub->addCommand( "Enable debugging", this, SLOT(onEnableDebug()),tr(OBN_ENDBG_SC), false );
    sub->addCommand( "Generate overflow checks", this, SLOT(onOvflChecks()) );
    sub->addCommand( "Toggle breakpoint", this, SLOT(onToggleBreakPt()), tr(OBN_TOGBP_SC), false);
    sub->addCommand( "Remove all breakpoints", this, SLOT(onRemoveAllBreakpoints()));
    sub->addCommand( "Step in", this, SLOT(onStepIn()), tr(OBN_STEPIN_SC), false);
    sub->addCommand( "Step over", this, SLOT(onStepOver()), tr(OBN_STEPOVER_SC), false);
    sub->addCommand( "Step out", this, SLOT(onStepOut()), tr(OBN_STEPOUT_SC), false);
    sub->addCommand( "Break", this, SLOT(onBreak()), tr(OBN_BREAK_SC), false);
    sub->addCommand( "Continue", this, SLOT(onContinue()), tr(OBN_CONTINUE_SC), false);
    sub->addCommand( "Abort", this, SLOT(onAbort()), tr(OBN_ABORT_SC), false);

}

static bool sortExList( const Symbol* lhs, const Symbol* rhs )
{
    Declaration* lm = lhs->decl;
    Declaration* rm = rhs->decl;
    const QByteArray ln = lm ? lm->name : QByteArray();
    const QByteArray rn = rm ? rm->name : QByteArray();
    const quint32 ll = lhs->pos.packed();
    const quint32 rl = rhs->pos.packed();

    return ln < rn || (!(rn < ln) && ll < rl);
}

static bool sortUsageByMod( const QPair<Declaration*, SymList>& lhs, const QPair<Declaration*, SymList>& rhs )
{
    Declaration* lm = lhs.first;
    Declaration* rm = rhs.first;
    const QByteArray ln = lm ? lm->name : QByteArray();
    const QByteArray rn = rm ? rm->name : QByteArray();

    return ln < rn;
}

static const char* roleName( Symbol* e )
{
    switch( e->kind )
    {
    case Symbol::Decl:
    case Symbol::Module:
        return "decl";
    case Symbol::Lval:
        return "lhs";
    case Symbol::End:
        return "end";
    default:
        break;
    }
    return "";
}

static Declaration* adjustForModIdx( Declaration* sym )
{
    while( sym )
    {
        switch( sym->kind )
        {
        case Declaration::Procedure:
        case Declaration::Module:
            return sym;
        }
        sym = sym->outer;
    }
    return 0;
}

void Ide::syncEditorMarks(Symbol* selected, Declaration* module)
{
    if( selected == 0 || selected->decl == 0 )
        return;
    Editor* edit = static_cast<Editor*>( d_tab->getCurrentTab() );
    if( edit == 0 )
        return;

    Symbol* syms = d_pro->getSymbolsOfModule(module);
    if( syms == 0 )
    {
        edit->markNonTerms(SymList());
        return;
    }

    Symbol* s = syms;
    SymList marks;
    while( s )
    {
        // mark all symbols which point to the same declaration as the one currently selected
        if( s->decl == selected->decl )
            marks << s;
        s = s->next;
        if( s == syms )
            break;
    }

    edit->markNonTerms(marks);
}

void Ide::syncModView(Declaration* decl)
{
    QTreeWidgetItem* mi = d_modIdx.value(decl);
    if( mi == 0 )
        mi = d_modIdx.value(adjustForModIdx(decl));
    if( mi && !d_lock2 )
    {
        d_mod->scrollToItem(mi,QAbstractItemView::PositionAtCenter);
        mi->setExpanded(true);
        d_mod->setCurrentItem(mi);
    }
}

static inline QString declKindName(Declaration* decl)
{
    switch( decl->kind )
    {
    case Declaration::Field:
        return "Field";
    case Declaration::VarDecl:
    case Declaration::LocalDecl:
        return "Var";
    case Declaration::ParamDecl:
        return "Param";
    case Declaration::TypeDecl:
        return "Type";
    case Declaration::ConstDecl:
        if( decl->getType() && decl->getType()->kind == Type::ConstEnum )
            return "Enum";
        else
            return "Const";
    case Declaration::Import:
        return "Import";
    case Declaration::Builtin:
        return "Builtin";
    case Declaration::Procedure:
        return "Proc";
    case Declaration::Module:
        return "Module";
    default:
        break;
    }
    return "???";
}

void Ide::fillXref()
{
    Editor* edit = static_cast<Editor*>( d_tab->getCurrentTab() );
    if( edit == 0 )
    {
        d_xref->clear();
        d_xrefTitle->clear();
        return;
    }
    Project2::File* f = d_pro->findFile(edit->getPath());
    if( f == 0 || f->d_mod == 0 )
        return;

    int line, col;
    edit->getCursorPosition( &line, &col );
    line += 1;
    col += 1;
    Declaration* scope = 0;
    Symbol* hit = d_pro->findSymbolBySourcePos(f->d_mod, line, col, &scope);
    if( hit && hit->decl )
    {
        d_xref->clear();
        d_xrefTitle->setText(QString("%1 '%2'").arg(declKindName(hit->decl)).arg(hit->decl->name.constData()));
        QTreeWidgetItem* black = fillXref(hit, f->d_mod);
        if( black && !d_lock3 )
        {
            d_xref->scrollToItem(black, QAbstractItemView::PositionAtCenter);
            d_xref->setCurrentItem(black);
        }
        syncModView(hit->decl);
        syncEditorMarks(hit, f->d_mod);
        fillHier(hit->decl);
    }else
        edit->markNonTerms(SymList());
}

QTreeWidgetItem* Ide::fillXref(Symbol* hit, Declaration* module)
{
    if( hit == 0 || hit->decl == 0 )
        return 0;

    QTreeWidgetItem* black = 0;

    if( hit->decl->kind == Declaration::Import && hit->decl->anonymous )
    {
        Symbol sym = *hit;
        sym.decl = hit->decl->data.value<Import>().resolved;
        if( sym.decl )
            black = fillXref(&sym, module);
    }

    Project2::UsageByMod usage = d_pro->getUsage(hit->decl);

    QFont f = d_xref->font();
    f.setBold(true);

    std::sort( usage.begin(), usage.end(), sortUsageByMod );
    for( int i = 0; i < usage.size(); i++ )
    {
        SymList syms = usage[i].second;
        std::sort( syms.begin(), syms.end(), sortExList );
        QString modName;
        if( usage[i].first )
            modName = usage[i].first->name;
        else
            modName = "<global>";
        foreach( Symbol* s, syms )
        {
            QTreeWidgetItem* item = new QTreeWidgetItem(d_xref);
            item->setText( 0, QString("%1 (%2:%3 %4)")
                        .arg(modName)
                        .arg(s->pos.d_row).arg(s->pos.d_col)
                        .arg( roleName(s) ));
            if( s == hit )
            {
                item->setFont(0,f);
                black = item;
            }
            item->setToolTip( 0, item->text(0) );
            item->setData( 0, Qt::UserRole, QVariant::fromValue( s ) ); // symbol in module
            item->setData( 1, Qt::UserRole, QVariant::fromValue( usage[i].first ) ); // module where the sym was found
            if( usage[i].first != module )
                item->setForeground( 0, Qt::gray );
        }
    }
    return black;
}

void Ide::fillStack()
{
    d_stackView->clear();

    d_stack = d_dbg->getStack(d_curThread);
    d_curLevel = -1;
    if( !d_stack.isEmpty() )
    {
        d_curLevel = 0;
        bool opened = false;
        d_scopes = QVector<Declaration*>(d_stack.size());
        for( int level = 0; level < d_stack.size(); level++ )
        {
            Debugger::MethodDbgInfo info = d_dbg->getMethodInfo(d_stack[level].method);
            Declaration* scope = 0;
            Debugger::MethodDbgInfo::Loc loc;
            Project2::File* fm = d_pro->findFile(info.sourceFile);
            if( fm )
            {
                loc = info.find(d_stack[level].il_offset);
                if( loc.valid )
                {
                    Symbol* e = d_pro->findSymbolBySourcePos(info.sourceFile,loc.row,loc.col, &scope);
                }
            }

            QByteArray methodName, typeName;

            if( scope == 0 )
            {
                methodName = d_dbg->getMethodName(d_stack[level].method);
                const quint32 owner = d_dbg->getMethodOwner(d_stack[level].method);
                const Debugger::TypeInfo type = d_dbg->getTypeInfo(owner);
                typeName = type.name;
            }else
            {
                Declaration* mod = scope->getModule();
                if( scope == mod )
                {
                    methodName = "<begin>";
                    typeName = mod->name;
                }else
                {
                    methodName = scope->name;
                    typeName = scope->getModule()->name;
                    if( scope->kind == Declaration::Procedure )
                    {
                        Declaration* p = scope;
                        if( p->outer )
                            typeName += "." + p->outer->name;
                    }
                }
                d_scopes[level] = scope;
            }
            // Level, Name, Pos, Mod
            QTreeWidgetItem* item = new QTreeWidgetItem(d_stackView);
            item->setText(0,QString::number(level));
            item->setData(0,Qt::UserRole,level);
            item->setText(1,methodName.constData());
            if( loc.valid && d_mode != BytecodeMode )
            {
                item->setText(2,QString("%1:%2").arg(loc.row).arg(loc.col));
                item->setData(2, Qt::UserRole, RowCol(loc.row,loc.col).packed() );
            }else
            {
                item->setText(2,QString("IL_%1").arg(d_stack[level].il_offset,4,16,QChar('0')));
                item->setData(2, Qt::UserRole, d_stack[level].il_offset );
            }
            item->setToolTip(2, info.sourceFile );
            item->setText(3, typeName );
            item->setData(3, Qt::UserRole, info.sourceFile );
            item->setToolTip(3, typeName );
            if( !opened )
            {
                if( loc.valid )
                {
                    Editor* edit = showEditor(info.sourceFile, loc.row, loc.col, true );
                    if( d_mode != LineMode && edit )
                    {
                        d_lock = true;
                        edit->dbgRow = loc.row - 1;
                        edit->dbgCol = qMax(0,loc.col - 1);
                        edit->setCursorPosition( edit->dbgRow, edit->dbgCol, true );
                        edit->setPositionMarker(edit->dbgRow);
                        d_lock = false;
                    }
                }
                opened = true;
            }
        }
    }else
        qWarning() << "stack is empty";

    fillLocals();

    d_stackView->parentWidget()->show();
}

void Ide::fillLocals()
{
    d_localsView->clear();
    d_il->clear();

    if( d_curLevel < d_stack.size() )
    {
        if( d_mode == BytecodeMode )
            d_il->load(d_stack[d_curLevel].method, d_stack[d_curLevel].il_offset );

        const quint32 cls = d_dbg->getMethodOwner(d_stack[d_curLevel].method);
        d_ilTitle->setText( d_dbg->getTypeInfo(cls).spaceName() + " " +
                            d_dbg->getMethodName(d_stack[d_curLevel].method) );


        QByteArrayList names = d_dbg->getParamNames(d_stack[d_curLevel].method);
        const bool isStatic = d_dbg->isMethodStatic(d_stack[d_curLevel].method);
        const QVariantList params = d_dbg->getParamValues(d_curThread, d_stack[d_curLevel].id, !isStatic, names.size() );
        if( isStatic )
        {
            for( int i = 0; i < params.size(); i++ )
            {
                QTreeWidgetItem* item = new QTreeWidgetItem(d_localsView);
                if( i < names.size() && !names[i].isEmpty() )
                    item->setText(0, names[i] );
                else
                    item->setText(0, tr("<param %1>").arg(i));
                setValue(item, params[i], d_dbg);
            }
        }else if( !params.isEmpty() )
        {
            QTreeWidgetItem* item = new QTreeWidgetItem(d_localsView);
            item->setText(0, "<this>" );
            setValue(item, params[0], d_dbg);
            for( int i = 1; i < params.size(); i++ )
            {
                QTreeWidgetItem* item = new QTreeWidgetItem(d_localsView);
                if( i-1 < names.size() && !names[i-1].isEmpty() )
                    item->setText(0, names[i-1] );
                else
                    item->setText(0, tr("<param %1>").arg(i));
                setValue(item, params[i], d_dbg);
            }
        }

        names = d_dbg->getLocalNames(d_stack[d_curLevel].method);
        const QVariantList locals = d_dbg->getLocalValues(d_curThread, d_stack[d_curLevel].id, names.size() );
        for( int i = 0; i < locals.size(); i++ )
        {
            QTreeWidgetItem* item = new QTreeWidgetItem(d_localsView);
            if( i < names.size() && !names[i].isEmpty() )
                item->setText(0, names[i] );
            else
                item->setText(0, tr("<local %1>").arg(i));
            setValue(item, locals[i], d_dbg);
        }

        d_localsView->sortItems(0,Qt::AscendingOrder);

        Q_ASSERT( d_curLevel < d_scopes.size());
        Declaration* m = d_scopes[d_curLevel] ? d_scopes[d_curLevel]->getModule() : 0;
        const quint32 type = getMonoModule(m);
        if( m && type )
        {
            QTreeWidgetItem* item = new QTreeWidgetItem(d_localsView);
            QString name = m->name;
            item->setToolTip(1,name);
            if( name.size() > 20 )
                name = name.left(20) + "...";
            item->setText(1,name);
            item->setText(0,"<module>");
            QVariant var = QVariant::fromValue(ObjectRef(ObjectRef::Type,type));
            //QVariant var = QVariant::fromValue(ObjectRef(ObjectRef::Class,d_dbg->getTypeObject(type)));
            item->setData(1,Qt::UserRole, var);
            item->setData(1,Qt::UserRole+1, type);
            item->setChildIndicatorPolicy(QTreeWidgetItem::ShowIndicator);
        }
    }

    d_localsView->parentWidget()->show();
}

static inline Type* derefed( Type* type )
{
    return type;
}

template <class T>
static inline void createArrayElems( QTreeWidgetItem* parent, const void* ptr,
                                     int bytecount, int numOfFetchedElems, char format = 0 )
{
    const int count = bytecount / sizeof(T);
    parent->setText(1, QString("<array length %1>").arg(count) );
    const T* arr = (const T*)ptr;
    for( int i = 0; i < qMin(count,numOfFetchedElems); i++ )
    {
        QTreeWidgetItem* item = new QTreeWidgetItem(parent);
        item->setText(0,QString::number(i));
        switch(format)
        {
        case 'b':
            item->setText(1, arr[i] ? "true" : "false" );
            break;
        case 's':
            item->setText(1,QString("{%1}").arg((quint32)arr[i],32,2,QChar('0')));
            break;
        default:
            item->setText(1,QString::number(arr[i]));
            break;
        }
    }
}

static void fillModItems(QTreeWidgetItem* item, Declaration* n, Declaration* p, Type* r,
                         bool sort, QHash<Declaration*,QTreeWidgetItem*>& idx , Project2* pro);

static void fillRecord(QTreeWidgetItem* item, Declaration* n, Type* r, bool sort,
                       QHash<Declaration*,QTreeWidgetItem*>& idx, Project2* pro )
{
    fillModItems(item,n, 0, r, sort, idx, pro);
    if( r->getType() )
        item->setText(0, item->text(0) + " ⇑");
    if( n->hasSubs )
    {
        DeclList subs = pro->getSubs(n);
        item->setText(0, item->text(0) + QString(" ⇓%1").arg(subs.size()));
    }
    item->setToolTip( 0, item->text(0) );
}

template<class T>
static void createModItem(T* parent, Declaration* n, Type* t, bool nonbound, bool sort,
                          QHash<Declaration*,QTreeWidgetItem*>& idx, Project2* pro )
{
    bool isAlias = false;
    if( t == 0 )
        t = n->getType();
    else
        isAlias = true;
    if( t == 0 )
        return;
    if( idx.contains(n) )
    {
        // qWarning() << "fillMod recursion at" << n->getModule()->d_file << n->pos.d_row << n->name;
        return; // can legally happen if record decl contains a typedef using record, as e.g. Meta.Item.ParamCallVal.IP
    }
    switch( n->kind )
    {
    case Declaration::TypeDecl:
        if( n != t->decl )
        {
            // this is a type alias
            if( t->kind == Type::Record || t->kind == Type::Object )
                createModItem(parent,n,t,nonbound, sort, idx, pro);
        }else
            switch( t->kind )
            {
            case Type::Pointer:
                if( (t->getType()->kind == Type::Record || t->getType()->kind == Type::Object) && t->getType()->decl->getModule() == n->getModule() )
                {
                    QTreeWidgetItem* item = new QTreeWidgetItem(parent);
                    Type* base = t->getType();
                    if( !isAlias  )
                        fillRecord(item,n,base,sort,idx, pro);
                    else
                        fillModItems(item,n, 0, 0, sort, idx, pro);
                }
                break;
            case Type::Record:
            case Type::Object:
            case Type::Interface:
                {
                    QTreeWidgetItem* item = new QTreeWidgetItem(parent);
                    if( !isAlias  )
                        fillRecord(item,n,t,sort,idx, pro);
                    else
                        fillModItems(item,n, 0, 0, sort, idx, pro);
                }
                break;
            }
        break;
    case Declaration::Procedure:
        if( !nonbound || !n->typebound )
        {
            QTreeWidgetItem* item = new QTreeWidgetItem(parent);
            fillModItems(item,n, n, 0, sort, idx, pro);
            if( n->data.value<Declaration*>() )
                item->setText(0, item->text(0) + " ⇑");
            if( n->hasSubs )
            {
                DeclList subs = pro->getSubs(n);
                item->setText(0, item->text(0) + QString(" ⇓%1").arg(subs.size()));
            }
            item->setToolTip( 0, item->text(0) );
        }
        break;
    }
}

template <class T>
static void walkModItems(T* parent, Declaration* p, Type* r, bool sort,
                         QHash<Declaration*,QTreeWidgetItem*>& idx, Project2* pro)
{
    typedef QMultiMap<QByteArray,Declaration*> Sort;
    if( p && sort)
    {
        Sort tmp;
        Declaration* n = p->link;
        while( n )
        {
            tmp.insert( n->name.toLower(), n );
            n = n->next;
        }
        Sort::const_iterator i;
        for( i = tmp.begin(); i != tmp.end(); ++i )
            createModItem(parent,i.value(),0,true, sort, idx, pro);
    }else if( p )
    {
        Declaration* n = p->link;
        while( n )
        {
            createModItem(parent,n,0,true, sort, idx, pro);
            n = n->next;
        }
    }
    if( r && sort )
    {
        Sort tmp;
        foreach( Declaration* n, r->subs )
        {
            if( n->kind == Declaration::Procedure )
                tmp.insert( n->name.toLower(), n );
        }
        Sort::const_iterator i;
        for( i = tmp.begin(); i != tmp.end(); ++i )
            createModItem(parent,i.value(),0,false, sort, idx, pro);
    }else if( r )
    {
        foreach( Declaration* n, r->subs )
        {
            if( n->kind == Declaration::Procedure )
                createModItem(parent,n,0,false, sort, idx, pro);
        }
    }
}

static void fillModItems( QTreeWidgetItem* item, Declaration* n, Declaration* p, Type* r,
                          bool sort, QHash<Declaration*,QTreeWidgetItem*>& idx, Project2* pro )
{
    const bool pub = n->visi > Declaration::Private;
    item->setText(0,n->name);
    item->setData(0, Qt::UserRole, QVariant::fromValue(n) );
    idx.insert(n,item);
    switch( n->kind )
    {
    case Declaration::TypeDecl:
        if( r && r->getType() == 0 && r->subs.isEmpty() )
            item->setIcon(0, QPixmap( pub ? ":/images/struct.png" : ":/images/struct_priv.png" ) );
        else if( r == 0 && p == 0 )
            item->setIcon(0, QPixmap( pub ? ":/images/alias.png" : ":/images/alias_priv.png" ) );
        else
            item->setIcon(0, QPixmap( pub ? ":/images/class.png" : ":/images/class_priv.png" ) );
        break;
    case Declaration::Procedure:
        item->setIcon(0, QPixmap( pub ? ":/images/func.png" : ":/images/func_priv.png" ) );
        break;
    }
    walkModItems(item,p,r,sort, idx, pro);
}

void Ide::fillModule(Declaration* m)
{
    d_mod->clear();
    d_modIdx.clear();
    d_modTitle->clear();
    if( m == 0 )
        return;
    d_modTitle->setText( QString("'%1'").arg(m->name.constData()) );
    walkModItems(d_mod, m, 0, true, d_modIdx, d_pro );
}

template<class T>
static QTreeWidgetItem* fillHierProc( T* parent, Declaration* p, Declaration* ref, Project2* pro )
{
    QTreeWidgetItem* item = new QTreeWidgetItem(parent);
    Q_ASSERT( p->typebound && p->link && p->link->typebound && p->link->getType() );

    // TODO Qualident record = p->link->type->decl->data.value<Qualident>();
    // item->setText(0, record.second);
    item->setData(0, Qt::UserRole, QVariant::fromValue(p) );
    item->setIcon(0, QPixmap( p->visi >= Declaration::ReadWrite ? ":/images/func.png" : ":/images/func_priv.png" ) );
    item->setToolTip(0,item->text(0));

    QTreeWidgetItem* ret = 0;
    DeclList subs = pro->getSubs(p);
    foreach( Declaration* sub, subs )
    {
        QTreeWidgetItem* tmp = fillHierProc(item, sub, ref, pro);
        if( tmp )
            ret = tmp;
    }
    if( ret == 0 && p == ref )
            ret = item;
    return ret;
}

template<class T>
static QTreeWidgetItem* fillHierClass( T* parent, Declaration* n, Type* p, Type* ref, Project2* pro )
{
    QTreeWidgetItem* item = new QTreeWidgetItem(parent);
    Declaration* name = p->decl;
    if( name )
#if 0
        // TODO
        item->setText(0, name->getQualifiedName().join('.') );
    else
#endif
        item->setText(0, name->name);
    item->setData(0, Qt::UserRole, QVariant::fromValue( name ) );
    item->setIcon(0, QPixmap( name->visi >= Declaration::ReadWrite ? ":/images/class.png" : ":/images/class_priv.png" ) );
    item->setToolTip(0,item->text(0));
    QTreeWidgetItem* ret = 0;
    DeclList subs = pro->getSubs(n);
    foreach( Declaration* sub, subs )
    {
        QTreeWidgetItem* tmp = fillHierClass(item, sub, sub->getType(), ref, pro);
        if( tmp )
            ret = tmp;
    }
    if( ret == 0 )
            ret = item;
    return ret;
}

void Ide::fillHier(Declaration* n)
{
    if( d_lock4 )
        return;
    d_hier->clear();
    d_hierTitle->clear();
    if( n == 0 )
        return;
    QFont f = d_hier->font();
    f.setBold(true);
    QTreeWidgetItem* ref = 0;
    switch( n->kind )
    {
    case Declaration::TypeDecl:
        if( n->getType() )
            switch( n->getType()->kind )
            {
            case Type::Record:
                {
                    Type* r = n->getType();
                    Type* r0 = r;
                    d_hierTitle->setText( QString("Inheritance of class '%1'").arg( n->name.constData() ) );
                    while( r->getType() )
                        r = r->getType();
                    ref = fillHierClass( d_hier, n, r, r0, d_pro );
                    Q_ASSERT( ref );
                }
                break;
            }
        break;
    case Declaration::Procedure:
        {
            Declaration* p = n;
            if( !p->typebound )
                return;
            d_hierTitle->setText( QString("Overrides of procedure '%1'").arg( n->name.constData() ) );
            while( p->data.value<Declaration*>() ) // go to super
                p = p->data.value<Declaration*>();
            ref = fillHierProc( d_hier, p, n, d_pro );
            Q_ASSERT( ref );
        }
        break;
    }
    d_hier->sortByColumn(0,Qt::AscendingOrder);
    if( ref )
    {
        ref->setFont(0,f);
        // d_hier->expandAll();
        ref->setExpanded(true);
        d_hier->scrollToItem(ref,QAbstractItemView::PositionAtCenter);
        d_hier->setCurrentItem(ref);
    }
}

void Ide::removePosMarkers()
{
    for( int i = 0; i < d_tab->count(); i++ )
    {
        Editor* e = static_cast<Editor*>( d_tab->widget(i) );
        e->setPositionMarker(-1);
        e->updateExtraSelections();
    }
    //d_bcv->clearMarker();
}

void Ide::handleGoBack()
{
    ENABLED_IF( d_backHisto.size() > 1 );

    d_pushBackLock = true;
    d_forwardHisto.push_back( d_backHisto.last() );
    d_backHisto.pop_back();
    showEditor( d_backHisto.last() );
    d_pushBackLock = false;
}

void Ide::handleGoForward()
{
    ENABLED_IF( !d_forwardHisto.isEmpty() );

    Location cur = d_forwardHisto.last();
    d_forwardHisto.pop_back();
    showEditor( cur );
}

void Ide::onUpdateLocation(int line, int col)
{
    Editor* e = static_cast<Editor*>( sender() );
    e->clearBackHisto();
    pushLocation(Location(e->getPath(), line,col,e->verticalScrollBar()->value()));
}

void Ide::onXrefDblClicked()
{
    QTreeWidgetItem* item = d_xref->currentItem();
    if( item )
    {
        Symbol* e = item->data(0,Qt::UserRole).value<Symbol*>();
        Q_ASSERT( e );
        Declaration* m = item->data(1,Qt::UserRole).value<Declaration*>();
        Q_ASSERT( m );
        d_lock3 = true;
        showEditor( m->data.value<ModuleData>().source, e->pos.d_row, e->pos.d_col, false, true );
        d_lock3 = false;
    }
}

QByteArray dottedName( Declaration* n )
{
    Q_ASSERT(n);
    // this is a copy of CilGen dottedName
    QByteArray name = n->name;
    Declaration* scope = n->outer;
    if( scope )
    {
        const int tag = scope->kind;
        if( tag != Declaration::Module )
        {
            if( tag == Declaration::Procedure )
            {
                Declaration* proc = scope;
                if( proc->typebound )
                {
                    name = scope->name + "#" + name;
                    scope = proc->outer;
                    Q_ASSERT( scope );
                }
            }
            return dottedName(scope) + "#" + name;
        }
    }
    return name;
}

void Ide::onToggleBreakPt()
{
    Editor* edit = static_cast<Editor*>( d_tab->getCurrentTab() );
    ENABLED_IF( edit );

    quint32 line;
    const bool on = edit->toggleBreakPoint(&line);
    Project2::File* fm = d_pro->findFile(edit->getPath());
    if( fm == 0 || fm->d_mod == 0 )
        return;
    if( on )
        d_breakPoints[fm->d_mod->name].insert(line + 1);
    else
        d_breakPoints[fm->d_mod->name].remove(line + 1);
    if( d_status == Running && d_debugging )
    {
        const bool res = updateBreakpoint(fm->d_mod,line+1,on);
        if( !res )
            qWarning() << "cannot change breakpoint at line" << line << "in" << fm->d_mod->name;
    }
}

void Ide::onStepIn()
{
    ENABLED_IF((!d_pro->getFiles().isEmpty() && d_status == Idle) || (d_suspended && d_debugging));
    if( d_suspended && d_debugging )
        d_dbg->stepIn(d_curThread, d_mode == LineMode);
    else
    {
        d_debugging = true;
        if( run() )
            d_suspended = true;
    }
}

void Ide::onStepOver()
{
    ENABLED_IF((!d_pro->getFiles().isEmpty() && d_status == Idle) || (d_suspended && d_debugging));
    if( d_suspended && d_debugging )
        d_dbg->stepOver(d_curThread, d_mode == LineMode );
    else
    {
        d_debugging = true;
        if( run() )
            d_suspended = true;
    }
}

void Ide::onStepOut()
{
    ENABLED_IF(d_suspended && d_debugging);
    d_dbg->stepOut(d_curThread, d_mode == LineMode );
}

void Ide::onContinue()
{
    ENABLED_IF( (!d_pro->getFiles().isEmpty() && d_status == Idle) || (d_suspended && d_debugging) );

    if( d_suspended && d_debugging )
    {
        d_dbg->resume();
        removePosMarkers();
        d_stackView->clear();
        d_localsView->clear();
    }else
    {
        d_debugging = true;
        if( run() )
            d_suspended = false;
    }
}

void Ide::onWorkingDir()
{
    ENABLED_IF(true);

    bool ok;
    const QString res = QInputDialog::getText(this,tr("Micron File System Root"),
                                              tr("Enter Path (supports %PRODIR% and %APPDIR%):"), QLineEdit::Normal,
                                              d_pro->getWorkingDir(), &ok );
    if( !ok )
        return;
    d_pro->setWorkingDir(res);
}

void Ide::onBuildDir()
{
    ENABLED_IF(true);

    bool ok;
    const QString res = QInputDialog::getText(this,tr("Set Build Directory"),
                                              tr("Enter Path (supports %PRODIR% and %APPDIR%):"), QLineEdit::Normal,
                                              d_pro->getBuildDir(), &ok );
    if( !ok )
        return;
    d_pro->setBuildDir(res);
}

void Ide::pushLocation(const Ide::Location& loc)
{
    if( d_pushBackLock )
        return;
    if( !d_backHisto.isEmpty() && d_backHisto.last() == loc )
        return; // o ist bereits oberstes Element auf dem Stack.
    d_backHisto.removeAll( loc );
    d_backHisto.push_back( loc );
}

void Ide::clear()
{
    d_backHisto.clear();
    d_forwardHisto.clear();
    d_pro->clear();
    d_mods->clear();
    d_mod->clear();
    d_hier->clear();
    d_modIdx.clear();
    d_stackView->clear();
    d_scopes.clear();
    d_localsView->clear();
    d_xrefTitle->clear();
    d_modTitle->clear();
    d_hierTitle->clear();
    d_xref->clear();
    d_errs->clear();
}

bool Ide::checkEngine(bool withFastasm)
{
    QDir monoPath( QApplication::applicationDirPath() );
    if( !monoPath.cd("mono") )
    {
        QMessageBox::critical(this, tr("Running Mono"),
                              tr("Cannot find the mono subdirectory under '%1").arg(monoPath.absolutePath()));
        return false;
    }
    if( ( withFastasm && !QFileInfo( monoPath.absoluteFilePath("fastasm.exe") ).exists() ) ||
            !QFileInfo( monoPath.absoluteFilePath("mscorlib.dll") ).exists() )
    {
        QMessageBox::critical(this, tr("Running Mono"),
                              tr("The mono subdirectory doesn't seem to be complete '%1").arg(monoPath.absolutePath()));
        return false;
    }
    d_eng->setMonoDir(monoPath.absolutePath());
    return true;
}

static QByteArray monoEscape( const QByteArray& name )
{
    QByteArray res = name;
    //res.replace(',',"\\,");
    //res.replace('.',"\\.");
    return res;
}

quint32 Ide::getMonoModule(Declaration *m)
{
    quint32 assemblyId = d_loadedAssemblies.value(m);
    if( assemblyId == 0 )
        return 0;
    // this is the class representing the Micron module (don't mix up with the module term in Mono)

    // NOTE: d_dbt->findType (both on VM and assembly level) doesn't find any type in the form of
    // som.IdentityDictionary(DeltaBlue.Sym,DeltaBlue.Strength)
    // som\.IdentityDictionary(DeltaBlue\.Sym\,DeltaBlue\.Strength)
    // som.IdentityDictionary
    // som\.IdentityDictionary
    // but only as IdentityDictionary!!
    // similarly nested classes are not found:
    // not CD2+reduceCollisionSet.ForEachInterface
    // not CD2+reduceCollisionSet\.ForEachInterface
    // but CD2+CollisionDetector does work

#if 0
    // doesn't seem to work
    QList<quint32> types = d_dbg->findType(monoEscape(m->getName())+","+monoEscape(m->getName()));
    if( types.size() == 1 )
        return types.first();
    else
        return 0;
#else
    // doesn't find types with (.,) in name, neither escaped nor unescaped, neither on VM nor assembly level
    //const QByteArray escaped = monoEscape(m->getFullName());
    const QByteArray escaped = m->name;
    //qDebug() << "searching for" << escaped << "in" << m->getName();
    return d_dbg->findType(escaped,assemblyId);
#endif
}

bool Ide::updateBreakpoint(Declaration *m, quint32 line, bool add)
{
    if( d_status != Running || !d_debugging )
        return false;
    const quint32 assemblyId = d_loadedAssemblies.value(m);
    if( assemblyId == 0 )
        return false;
    Declaration* scope;
    d_pro->findSymbolBySourcePos(m,line,1, &scope );
    if( scope == 0 )
        return false;
    Declaration* proc = 0;
    switch( scope->kind )
    {
    case Declaration::Procedure:
        proc = scope;
        break;
    case Declaration::Module:
        break;
    default:
        return false;
    }
    quint32 type;
    QByteArray procName;
    if( proc && proc->typebound )
    {
        QByteArray name = dottedName(proc->outer);
        name = m->name + "+" + name;
        type = d_dbg->findType(name,assemblyId);
        procName = proc->name;
    }else
    {
        type = getMonoModule(m);
        if( proc )
            procName = dottedName(proc);
        else
            procName = ".cctor";
    }
    if( type == 0 )
        return false;

    QList<quint32> meth = d_dbg->getMethods(type,procName);
    if( meth.size() != 1 )
        return false;

    Debugger::MethodDbgInfo info = d_dbg->getMethodInfo(meth.first());
    const quint32 iloff = info.find(line,-1);
    if( add )
        return d_dbg->addBreakpoint(meth.first(),iloff);
    else
        return d_dbg->removeBreakpoint(meth.first(),iloff);
}

void Ide::onAbout()
{
    ENABLED_IF(true);

    QMessageBox::about( this, qApp->applicationName(),
      tr("<html>Release: %1   Date: %2<br><br>"

      "Welcome to the Micron IDE.<br>"
      "See <a href=\"https://github.com/rochus-keller/Micron\">"
         "here</a> for more information.<br><br>"

      "Author: Rochus Keller, me@rochus-keller.ch<br><br>"

      "License: <a href=\"https://www.gnu.org/licenses/license-list.html#GNUGPL\">GNU GPL v2 or v3</a>"
      "</html>" ).arg( qApp->applicationVersion() ).arg( QDateTime::currentDateTime().toString("yyyy-MM-dd") ));
}

void Ide::onQt()
{
    ENABLED_IF(true);
    QMessageBox::aboutQt(this,tr("About the Qt Framework") );
}

void Ide::onByteMode()
{
    CHECKED_IF( true, d_mode == BytecodeMode );

    if( d_mode != BytecodeMode )
        d_mode = BytecodeMode;
    else
        d_mode = LineMode;
}

void Ide::onSetRunCommand()
{
    ENABLED_IF(d_status == Idle);

    bool ok = false;
    QString res = QInputDialog::getText(this,tr("Set Command"),tr("<module>.<command> to run at start of program:"),
                             QLineEdit::Normal, d_pro->renderMain(), &ok );
    if( !ok )
        return;

    Project2::ModProc m;
    QStringList tmp = res.split('.');
    if( tmp.size() > 2 || ( tmp.first().isEmpty() && !tmp.last().isEmpty() ) )
        QMessageBox::critical(this,tr("Set Command"), tr("Invalid command syntax") );
    else
    {
        m.first = tmp.first().toUtf8();
        if( tmp.size() == 2 )
            m.second = tmp.last().toUtf8();
        d_pro->setMain(m);
    }
}

void Ide::onConsole(const QString& msg, bool err)
{
    logMessage(msg, err ? LogError : LogInfo, false );
}

void Ide::onError(const QString& msg)
{
    QMessageBox::critical(this, tr("Mono Engine"), msg );
}

void Ide::onFinished(int exitCode, bool normalExit)
{
    if( d_status == Generating )
    {
        QDir outDir(d_pro->getBuildDir(true));
        QFile files(outDir.absoluteFilePath("modules"));
        if( files.open(QIODevice::ReadOnly) )
        {
            while( !files.atEnd() )
            {
                const QByteArray line = files.readLine().trimmed();
                const QString fileName = outDir.absoluteFilePath(QString::fromUtf8(line)+".il");
                // TODO QFile::remove(fileName);
            }
        }
        files.remove();
        QFile::remove(outDir.absoluteFilePath("batch"));
    }else if( d_status == Running )
    {
        if( normalExit )
            logMessage(tr("\nThe application finished with code %1\n\n").arg(exitCode),SysInfo,false);
        else
            logMessage(tr("\nThe execution engine crashed\n\n"),LogError,false);
        if( d_debugging )
            d_dbg->close();
        d_status = Idle;
        d_loadedAssemblies.clear();
        removePosMarkers();
        d_il->clear();
        d_ilTitle->clear();
        d_localsView->clear();
        d_stackView->clear();
    }
    d_suspended = false;
    d_status = Idle;
}

void Ide::onDbgEvent(const Mono::DebuggerEvent& e)
{
    //qDebug() << "event arrived" << Mono::DebuggerEvent::s_event[e.event] << e.thread << e.object;
    switch( e.event )
    {
    case Mono::DebuggerEvent::VM_START:
        d_curThread = e.thread;
        //d_rootDomain = e.object;
        d_dbg->enableUserBreak();
        if( !d_suspended )
            d_dbg->resume();
        break;
    case Mono::DebuggerEvent::STEP:
    case Mono::DebuggerEvent::BREAKPOINT:
    case Mono::DebuggerEvent::USER_BREAK:
        {
            d_suspended = true;
            if( e.event == Mono::DebuggerEvent::USER_BREAK )
                logMessage("\ntrap hit\n", SysInfo, false );
            else if( e.event == Mono::DebuggerEvent::BREAKPOINT )
                logMessage("\nbreakpoint hit\n", SysInfo, false );

            fillStack();
        }
        break;
    case Mono::DebuggerEvent::EXCEPTION:
        {
            //d_dbg->suspend(); // SUSPEND_POLICY_ALL instead
            if( d_breakOnExceptions )
            {
                // unfortunatedly mono does not break on uncaught exceptions!
                // so this breaks on all exceptions, even caught ones!
                d_stack = d_dbg->getStack(d_curThread);
                if( !d_stack.isEmpty() )
                {
                    Debugger::MethodDbgInfo ex = d_dbg->getMethodInfo(d_stack[0].method);
                    if( !ex.sourceFile.isEmpty() )
                    {
                        logMessage("\nexception hit\n", SysInfo, false );
                        d_suspended = true;
                        fillStack();
                        break;
                    }
#if 1
                    else
                    {
                        const QByteArray name = d_dbg->getMethodName(d_stack[0].method);
                        const quint32 owner = d_dbg->getMethodOwner(d_stack[0].method);
                        const Debugger::TypeInfo info = d_dbg->getTypeInfo(owner);
                        logMessage(tr("\nexception hit at %1::%2\n").arg(info.fullName.constData()).
                                   arg(name.constData()), SysInfo, false );
                        d_suspended = true;
                        fillStack();
                        break;
                    }
#endif
                }
            }
            // else
            d_dbg->resume();
        }
        break;
    case Mono::DebuggerEvent::ASSEMBLY_LOAD:
        {
            QByteArray name = d_dbg->getAssemblyName(e.object);
            // name is a string like "Harness, Version=0.0.0.0, Culture=neutral, PublicKeyToken=null"
            const int pos = name.indexOf(", Version=");
            if( pos != 0 )
                name = name.left(pos);
            qDebug() << "loaded assembly" << name;
            Declaration* module = d_pro->findModule(name);
            if( module )
                d_loadedAssemblies[module] = e.object;
            else
            {
                // things like OBX.Runtime, Input, etc. are not found
                break;
            }
            const QSet<quint32>& lines = d_breakPoints.value(name);
            foreach( quint32 line, lines )
            {
                const bool res = updateBreakpoint(module,line,true);
                if( !res )
                    qWarning() << "cannot set breakpoint at line" << line << "in" << name;
            }
        }
        break;
    }
}

void Ide::onRemoveAllBreakpoints()
{
    ENABLED_IF(true);
    d_breakPoints.clear();
    for( int i = 0; i < d_tab->count(); i++ )
    {
        Editor* e = static_cast<Editor*>( d_tab->widget(i) );
        e->clearBreakPoints();
    }
    if( d_status == Running && d_debugging )
        d_dbg->clearAllBreakpoints();
}

void Ide::onBreakOnExceptions()
{
    CHECKED_IF(true, d_breakOnExceptions);

    d_breakOnExceptions = !d_breakOnExceptions;
}

void Ide::onRowColMode()
{
    CHECKED_IF( true, d_mode == RowColMode );

    if( d_mode != RowColMode )
        d_mode = RowColMode;
    else
        d_mode = LineMode;
}

void Ide::onSetInputFile()
{
    ENABLED_IF(d_status==Idle);

    const QString path = QFileDialog::getOpenFileName(this, tr("Set Input File"),QString() );
    if( path.isEmpty() )
        return;
    d_eng->setInputFile(path);
}

void Ide::onSetOptions()
{
    ENABLED_IF(true);

    QByteArrayList l = d_pro->getOptions();
    qSort(l);

    bool ok;
    const QString options = QInputDialog::getMultiLineText(this,tr("Set Configuration Variables"),
                                                           tr("Please enter a unique identifier per variable:"),
                                                           l.join('\n'), &ok );
    if( !ok )
        return;

    Lexer lex;
    QList<Token> toks = lex.tokens(options);
    l.clear();
    QStringList errs;
    foreach( const Token& t, toks )
    {
        if( t.d_type == Tok_ident )
            l << t.d_val;
        else
            errs << QString::fromUtf8(t.d_val);
    }

    if( !errs.isEmpty() )
        QMessageBox::warning(this,tr("Set Configuration Variables"),
                             tr("The following entries are illegal and ignored: \n%1").arg(errs.join('\n')));

    d_pro->setOptions(l);
}

void Ide::onNoWarnings()
{
    CHECKED_IF(true, d_noWarnings );
    d_noWarnings = !d_noWarnings;
    onErrors();
}

void Ide::onConvertAllToUtf8()
{
    ENABLED_IF(true);

    if( QMessageBox::warning( this, tr("Convert all Files"),
                              tr("Do you really want to convert all files of the project to UTF-8?\n"
                                 "This cannot be undone!"),
                           QMessageBox::Yes | QMessageBox::Cancel, QMessageBox::Yes ) != QMessageBox::Yes )
        return;

    QHash<QString,Editor*> open;
    for( int i = 0; i < d_tab->count(); i++ )
    {
        Editor* edit = static_cast<Editor*>( d_tab->widget(i) );
        if( edit == 0 )
            continue; // can this happen?
        open.insert(edit->getPath(),edit);
    }
    const Project2::FileHash& files = d_pro->getFiles();
    for( Project2::FileHash::const_iterator i = files.begin(); i != files.end(); ++i )
    {
        QHash<QString,Editor*>::const_iterator j = open.find(i.value()->d_filePath);
        if( j != open.end() )
            j.value()->saveToFile(j.key(),false);
        else
        {
            QFile f(i.value()->d_filePath);
            f.open(QIODevice::ReadOnly);
            const QByteArray text = Lexer::extractText(&f);
            f.close();
            f.open(QIODevice::WriteOnly);
            f.write(text);
        }
    }
}

void Ide::onIncremental()
{
    CHECKED_IF(true,d_incremental);
    d_incremental = !d_incremental;
}

int main(int argc, char *argv[])
{
    QApplication a(argc, argv);
    a.setOrganizationName("Dr. Rochus Keller");
    a.setOrganizationDomain("www.rochus-keller.ch");
    a.setApplicationName("Micron IDE");
    a.setApplicationVersion("0.2.31");
    a.setStyle("Fusion");    
    QFontDatabase::addApplicationFont(":/font/DejaVuSansMono.ttf"); // "DejaVu Sans Mono"

#ifdef QT_STATIC
    QFontDatabase::addApplicationFont(":/font/NotoSans.ttf"); // "Noto Sans"
    QFont af("Noto Sans",10);
    a.setFont(af);
#endif

#ifdef Q_OS_MAC
    QDir cur = QCoreApplication::applicationDirPath();
    if( cur.path().endsWith("/Contents/MacOS") )
    {
        // we're in a bundle
        cur.cdUp();
        cur.cdUp();
        cur.cdUp();
    }
    QDir::setCurrent(cur.path());
#endif

    Ide w;
    if( a.arguments().size() > 1 )
        w.loadFile(a.arguments()[1] );

    return a.exec();
}
