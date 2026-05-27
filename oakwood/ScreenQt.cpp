/*
* Copyright 2026 Rochus Keller <mailto:me@rochus-keller.ch>
*
* This file is part of the Micron programming language project.
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

#include "ScreenQt.h"
#include <QPainter>
#include <QApplication>
#include <QCloseEvent>
#include <QShortcut>
#include <stdint.h>
#include <time.h>
#include <QBitmap>
#include <QPixmap>
using namespace Screen;

// Sync enums with Screen.mic!!!
enum EvtType {
    EVT_NONE       = 0,
    EVT_MOUSE_MOVE = 1,
    EVT_MOUSE_DOWN = 2,
    EVT_MOUSE_UP   = 3,
    EVT_KEY_DOWN   = 4,
    EVT_KEY_UP     = 5,
    EVT_KEY_CHAR   = 6
};

enum { BTN_LEFT = 1, BTN_MIDDLE = 2, BTN_RIGHT = 3 };

enum {
    FLAG_LSB_FIRST = 1,
    FLAG_BOTTOM_UP = 2
};

enum {
    KEY_BACKSPACE = 0x100,
    KEY_TAB       = 0x101,
    KEY_RETURN    = 0x102,
    KEY_ESCAPE    = 0x103,
    KEY_DELETE    = 0x104,
    KEY_LEFT      = 0x105,
    KEY_RIGHT     = 0x106,
    KEY_UP        = 0x107,
    KEY_DOWN      = 0x108,
    KEY_LSHIFT    = 0x110,
    KEY_RSHIFT    = 0x111,
    KEY_LCTRL     = 0x112,
    KEY_RCTRL     = 0x113,
    KEY_CAPSLOCK  = 0x114
};

static inline int32_t packEvent(int type, int data)
{
    return (type << 24) | (data & 0x00FFFFFF);
}

static inline int32_t packMouseMove(int px, int py)
{
    return packEvent(EVT_MOUSE_MOVE, ((px & 0xFFF) << 12) | (py & 0xFFF));
}

static inline quint32 tick()
{
    static clock_t start = 0;
    if(start == 0)
        start = clock();
    clock_t t = clock();
    return (quint32)(((double)(t - start) * 1000.0) / CLOCKS_PER_SEC);
}

#define MAX(x, y) (((x) > (y)) ? (x) : (y))
#define MIN(x, y) (((x) < (y)) ? (x) : (y))

static Widget* s_inst = 0;
bool Widget::s_run = true;

Widget::Widget(QWidget* parent)
    : QWidget(parent), d_buffer(0), d_bufLen(0),
      d_width(0), d_height(0), d_flags(0),
      d_btnLeft(false), d_btnMid(false), d_btnRight(false),
      d_ctrlDown(false)
{
    setMouseTracking(true);
    setFocusPolicy(Qt::StrongFocus);
    setCursor(Qt::BlankCursor);
    setWindowTitle("Micron Screen (Qt)");
    show();
    new QShortcut(tr("ALT+X"), this, SLOT(close()));
}

Widget::~Widget()
{
    s_inst = 0;
}

Widget* Widget::inst()
{
    if( !s_inst )
    {
        s_inst = new Widget();
        s_run = true;
    }
    return s_inst;
}

void Widget::setBuffer(uint8_t* buf, int bLen, int w, int h, unsigned int fl)
{
    d_buffer = buf;
    d_bufLen = bLen;
    d_width  = w;
    d_height = h;
    d_flags  = fl;
    d_screen = QImage(w, h, QImage::Format_RGB32);
    d_screen.fill(0xFFFFFFFF);
    d_updateArea = QRect();
    setFixedSize(w, h);
    // Mark whole screen dirty up front
    d_updateArea = QRect(0, 0, w, h);
    update();
}

void Widget::updateImage()
{
    if( d_updateArea.isNull() || !d_buffer )
        return;

    const int ax = d_updateArea.x();
    const int ay = d_updateArea.y();
    const int aw = d_updateArea.width();
    const int ah = d_updateArea.height();
    const int dw = d_screen.bytesPerLine();

    if( d_flags & FLAG_LSB_FIRST )
    {
        // Oberon-style: LSB-first, 32-bit word aligned
        const int sw = (d_width / 32) * 4;
        for( int y = 0; y < ah; y++ )
        {
            const int srcLine = (d_flags & FLAG_BOTTOM_UP) ?
                        (d_height - 1 - (ay + y)) : (ay + y);
            const uint8_t* src = d_buffer + sw * srcLine;
            uint32_t* p = (uint32_t*)(d_screen.bits() + dw * (ay + y)) + ax;

            for( int x = ax; x < ax + aw; x++ )
            {
                const int wordIdx = x / 32;
                const int bitIdx  = x % 32;
                uint32_t word = ((const uint32_t*)src)[wordIdx];
                uint32_t v = (word >> bitIdx) & 1;
                *p++ = v ? 0xFFFFFFFF : 0xFF000000;
            }
        }
    } else
    {
        // Smalltalk-style: MSB-first, 16-bit word aligned
        const int pixPerWord = 16;
        const int pixLineWidth = ((d_width + pixPerWord - 1) / pixPerWord) * pixPerWord;
        const int sw = pixLineWidth / 8;

        for( int y = 0; y < ah; y++ )
        {
            int srcLine = (d_flags & FLAG_BOTTOM_UP) ?
                        (d_height - 1 - (ay + y)) : (ay + y);
            const uint8_t* src = d_buffer + sw * srcLine;
            uint32_t* p = (uint32_t*)(d_screen.bits() + dw * (ay + y)) + ax;

            for( int x = ax; x < ax + aw; x++ )
            {
                uint8_t v = (src[x >> 3] >> (7 - (x & 7))) & 1;
                *p++ = v ? 0xFF000000 : 0xFFFFFFFF;
            }
        }
    }

    update(d_updateArea);
    d_updateArea = QRect();
}

void Widget::updateArea(int x, int y, int w, int h)
{
    QRect r(x, y, w, h);
    r = r.intersected(QRect(0, 0, d_width, d_height));
    if( r.isEmpty() )
        return;
    d_updateArea |= r;
}

void Widget::processAndRender()
{
    updateImage();
    QApplication::processEvents();
}

void Widget::enqueueEvent(int32_t e)
{
    d_events.enqueue(e);
}

int32_t Widget::nextEvent()
{
    if( d_events.isEmpty() )
        return 0;
    return d_events.dequeue();
}

void Widget::getMouseState(int* ox, int* oy, int* buttons)
{
    if( ox )
        *ox = d_mousePos.x();
    if( oy )
        *oy = d_mousePos.y();
    int b = 0;
    if( d_btnLeft )
        b |= 4;
    if( d_btnMid )
        b |= 2;
    if( d_btnRight )
        b |= 1;
    if( buttons )
        *buttons = b;
}

void Widget::setCursorBitmap(uint8_t* bits, int w, int h, int hotX, int hotY)
{
    if( bits && w > 0 && h > 0)
    {
        // Use ARGB32 with explicit alpha to avoid Format_Mono/QBitmap
        // color table inversion issues across Qt versions and platforms.
        const int bytesPerLine = (w + 7) / 8;
        QImage img(w, h, QImage::Format_ARGB32);
        for( int y = 0; y < h; y++ )
        {
            const uint8_t* row = bits + y * bytesPerLine;
            for( int x = 0; x < w; x++ )
            {
                const int bit = (row[x >> 3] >> (7 - (x & 7))) & 1;
                img.setPixel(x, y, bit ? 0xFF000000u : 0x00000000u);
            }
        }
        QWidget::setCursor(QCursor(QPixmap::fromImage(img), hotX, hotY));
    } else
    {
        QWidget::setCursor(Qt::BlankCursor);
    }
}

void Widget::warpCursor(int x, int y)
{
    QCursor::setPos(mapToGlobal(QPoint(x, y)));
}

int Widget::mapKeyCode(int qtKey)
{
    switch( qtKey )
    {
    case Qt::Key_Backspace:
        return KEY_BACKSPACE;
    case Qt::Key_Tab:
        return KEY_TAB;
    case Qt::Key_Return:
    case Qt::Key_Enter:
        return KEY_RETURN;
    case Qt::Key_Escape:
        return KEY_ESCAPE;
    case Qt::Key_Delete:
        return KEY_DELETE;
    case Qt::Key_Left:
        return KEY_LEFT;
    case Qt::Key_Right:
        return KEY_RIGHT;
    case Qt::Key_Up:
        return KEY_UP;
    case Qt::Key_Down:
        return KEY_DOWN;
    case Qt::Key_Shift:
        return KEY_LSHIFT;
    case Qt::Key_Control:
        return KEY_LCTRL;
    case Qt::Key_CapsLock:
        return KEY_CAPSLOCK;
    case Qt::Key_Space:
        return ' ';
    }
    if( qtKey >= Qt::Key_A && qtKey <= Qt::Key_Z )
        return qtKey - Qt::Key_A + 'a';
    if( qtKey >= Qt::Key_0 && qtKey <= Qt::Key_9 )
        return qtKey - Qt::Key_0 + '0';
    if( qtKey >= 0x20 && qtKey <= 0x7E )
        return qtKey;
    return 0;
}

void Widget::paintEvent(QPaintEvent* event)
{
    if( d_screen.isNull() )
        return;
    const QRect r = event->rect();
    if( r.isNull() )
        return;
    QPainter p(this);
    p.setRenderHints(QPainter::Antialiasing | QPainter::TextAntialiasing |
                     QPainter::SmoothPixmapTransform, false);
    p.drawImage(r, d_screen, r);
}

void Widget::closeEvent(QCloseEvent* event)
{
    s_run = false;
    event->accept();
    deleteLater();
}

void Widget::mouseMoveEvent(QMouseEvent* event)
{
    QPoint pos = event->pos();
    pos.setX(MAX(MIN(pos.x(), d_width - 1), 0));
    pos.setY(MAX(MIN(pos.y(), d_height - 1), 0));

    if( pos != d_mousePos )
    {
        d_mousePos = pos;
        enqueueEvent(packMouseMove(pos.x(), pos.y()));
    }
}

void Widget::handleMouseButton(bool press, int qtButton)
{
    int btn = 0;
    switch(qtButton)
    {
    case Qt::LeftButton:
        btn = BTN_LEFT;
        d_btnLeft = press;
        break;
    case Qt::RightButton:
        btn = BTN_RIGHT;
        d_btnRight = press;
        break;
    case Qt::MiddleButton:
        btn = BTN_MIDDLE;
        d_btnMid = press;
        break;
    default:
        return;
    }
    enqueueEvent(packEvent(press ? EVT_MOUSE_DOWN : EVT_MOUSE_UP, btn));
}

void Widget::mousePressEvent(QMouseEvent* event)
{
    handleMouseButton(true, event->button());
}

void Widget::mouseReleaseEvent(QMouseEvent* event)
{
    handleMouseButton(false, event->button());
}

void Widget::keyPressEvent(QKeyEvent* event)
{
    if( event->key() == Qt::Key_Q &&
        (event->modifiers() & Qt::ControlModifier) )
    {
        s_run = false;
        close();
        return;
    }

    if( event->key() == Qt::Key_Control )
        d_ctrlDown = true;

    const int code = mapKeyCode(event->key());
    if( code )
        enqueueEvent(packEvent(EVT_KEY_DOWN, code));

    if( !event->text().isEmpty() )
    {
        char ch = event->text()[0].toLatin1();
        if( ch >= 0x20 || ch == '\t' || ch == '\r' || ch == '\n' || ch == 0x1B || ch == 0x08 )
            enqueueEvent(packEvent(EVT_KEY_CHAR, (unsigned char)ch));
    }
}

void Widget::keyReleaseEvent(QKeyEvent* event)
{
    if( event->key() == Qt::Key_Control )
        d_ctrlDown = false;

    const int code = mapKeyCode(event->key());
    if( code )
        enqueueEvent(packEvent(EVT_KEY_UP, code));
}

void Widget::inputMethodEvent(QInputMethodEvent* event)
{
    const QString text = event->commitString();
    if( !text.isEmpty() && text.at(0).isPrint() )
    {
        const char ch = text.at(0).toLatin1();
        if( ch )
            enqueueEvent(packEvent(EVT_KEY_CHAR, (unsigned char)ch));
    }
}

extern "C" {

#ifdef _WIN32
#define DllExport __declspec(dllexport)
#else
#define DllExport
#endif

DllExport void Screen$begin$()
{
    // NOP
}

DllExport int32_t Screen$Open(uint8_t* buf, int32_t bLen, int32_t w, int32_t h, unsigned int fl)
{
    Widget::inst()->setBuffer(buf, bLen, w, h, fl);
    return 1;
}

DllExport void Screen$Close()
{
    if( Widget::inst() )
        Widget::inst()->close();
}

DllExport void Screen$UpdateArea(int32_t x, int32_t y, int32_t w, int32_t h)
{
    Widget::inst()->updateArea(x, y, w, h);
}

DllExport int32_t Screen$ProcessEvents(int32_t sleep)
{
    (void)sleep; /* Qt uses QApplication::processEvents, non-blocking */
    Widget* w = Widget::inst();
    int before = w->eventsPending();
    w->processAndRender();
    if (!Widget::s_run) return -1;
    int after = w->eventsPending();
    return after - before;
}

DllExport int32_t Screen$NextEvent()
{
    return Widget::inst()->nextEvent();
}

DllExport int32_t Screen$GetMouseState(int32_t* x, int32_t* y)
{
    int buttons = 0;
    Widget::inst()->getMouseState((int*)x, (int*)y, &buttons);
    return buttons;
}

DllExport void Screen$SetCursor(uint8_t* bits, int32_t w, int32_t h,
                                 int32_t hotX, int32_t hotY)
{
    Widget::inst()->setCursorBitmap(bits, w, h, hotX, hotY);
}

DllExport void Screen$SetCursorPos(int32_t x, int32_t y)
{
    Widget::inst()->warpCursor(x, y);
}

DllExport uint32_t Screen$GetTicks()
{
    return tick();
}

} /* extern "C" */
