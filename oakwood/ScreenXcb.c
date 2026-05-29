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

/*
* Raw xcb backend for the Screen oakwood module. This backend replaces
* ScreenSdl.c because SDL2 turned out to be too big and the build with less 
* options too fragile; building statically with Musl and no dlopen seems impossible.
* Only depends on libxcb core (no xcb-util, no xcb-image, no xcb-keysyms). Linux only.
* Link with: -lxcb
* Optional: -lXau (for X authorization; not needed when XAUTHORITY is not used)
*/

#include <xcb/xcb.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <time.h>
#include <poll.h>

#define DllExport

// Sync enums with Screen.mic!!!
enum {
    FLAG_LSB_FIRST = 1,
    FLAG_BOTTOM_UP = 2
};

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

enum { QUEUE_LEN = 512 };

static int32_t evtQueue[QUEUE_LEN];
static int evtHead = 0, evtTail = 0, evtCount = 0;

static void evtEnqueue(int32_t e)
{
    if( evtCount == QUEUE_LEN )
        return;
    evtQueue[evtHead] = e;
    evtHead = (evtHead + 1) % QUEUE_LEN;
    evtCount++;
}

static int32_t evtDequeue(void)
{
    int32_t e;
    if( evtCount == 0 )
        return 0;
    e = evtQueue[evtTail];
    evtTail = (evtTail + 1) % QUEUE_LEN;
    evtCount--;
    return e;
}

static int32_t packEvent(int type, int data)
{
    return (type << 24) | (data & 0x00FFFFFF);
}

static int32_t packMouseMove(int px, int py)
{
    return packEvent(EVT_MOUSE_MOVE, ((px & 0xFFF) << 12) | (py & 0xFFF));
}

static struct timespec timerStart;
static int timerInit = 0;

static uint32_t getMillis(void)
{
    struct timespec now;
    if( !timerInit )
    {
        clock_gettime(CLOCK_MONOTONIC, &timerStart);
        timerInit = 1;
    }
    clock_gettime(CLOCK_MONOTONIC, &now);
    return (uint32_t)((now.tv_sec - timerStart.tv_sec) * 1000 +
                      (now.tv_nsec - timerStart.tv_nsec) / 1000000);
}

/* X11 keysym definition subset to avoid dependency on X11/keysymdef.h */
#define XK_BackSpace    0xff08
#define XK_Tab          0xff09
#define XK_Return       0xff0d
#define XK_Escape       0xff1b
#define XK_Delete       0xffff
#define XK_Left         0xff51
#define XK_Up           0xff52
#define XK_Right        0xff53
#define XK_Down         0xff54
#define XK_Shift_L      0xffe1
#define XK_Shift_R      0xffe2
#define XK_Control_L    0xffe3
#define XK_Control_R    0xffe4
#define XK_Caps_Lock    0xffe5
#define XK_space        0x0020
#define XK_a            0x0061
#define XK_z            0x007a
#define XK_0            0x0030
#define XK_9            0x0039


static xcb_connection_t* conn = NULL;
static xcb_window_t      win  = 0;
static xcb_gcontext_t    gc   = 0;
static xcb_screen_t*     scr  = NULL;

static uint8_t*  buffer   = NULL;
static int       bufLen   = 0;
static uint8_t*  pixelBuf = NULL;
static int WIDTH = 0, HEIGHT = 0;
static unsigned int flags = 0;

static int dirtyX = 0, dirtyY = 0, dirtyW = 0, dirtyH = 0;
static int isDirty = 0;

static int mouseX = 0, mouseY = 0;
static int btnLeft = 0, btnMid = 0, btnRight = 0;

static int ctrlDown = 0;

static xcb_cursor_t customCursor = 0;

static xcb_keysym_t* keymap      = NULL;
static int           keymapMin   = 0;
static int           keymapMax   = 0;
static int           keysymsPerCode = 0;

static uint32_t maxReqBytes = 0;

static xcb_atom_t wmDeleteWindow = 0;
static xcb_atom_t wmProtocols    = 0;

#define MAX(x, y) (((x) > (y)) ? (x) : (y))
#define MIN(x, y) (((x) < (y)) ? (x) : (y))


static void loadKeymap(void)
{
    const xcb_setup_t* setup = xcb_get_setup(conn);
    xcb_get_keyboard_mapping_cookie_t cookie;
    xcb_get_keyboard_mapping_reply_t* reply;
    xcb_keysym_t* syms;
    int nsyms;

    keymapMin = setup->min_keycode;
    keymapMax = setup->max_keycode;

    cookie = xcb_get_keyboard_mapping(conn, (xcb_keycode_t)keymapMin,
                                      keymapMax - keymapMin + 1);
    reply = xcb_get_keyboard_mapping_reply(conn, cookie, NULL);
    if( !reply )
        return;

    keysymsPerCode = reply->keysyms_per_keycode;
    nsyms = xcb_get_keyboard_mapping_keysyms_length(reply);
    if( keymap )
        free(keymap);
    keymap = (xcb_keysym_t*)malloc(nsyms * sizeof(xcb_keysym_t));
    if( keymap )
    {
        syms = xcb_get_keyboard_mapping_keysyms(reply);
        memcpy(keymap, syms, nsyms * sizeof(xcb_keysym_t));
    }
    free(reply);
}

static xcb_keysym_t keycodeToKeysym(xcb_keycode_t code, int col)
{
    int idx;
    if( !keymap || code < keymapMin || code > keymapMax )
        return 0;
    idx = (code - keymapMin) * keysymsPerCode + col;
    return keymap[idx];
}

static int mapKeysym(xcb_keysym_t sym)
{
    switch( sym )
    {
    case XK_BackSpace:
        return KEY_BACKSPACE;
    case XK_Tab:
        return KEY_TAB;
    case XK_Return:
        return KEY_RETURN;
    case XK_Escape:
        return KEY_ESCAPE;
    case XK_Delete:
        return KEY_DELETE;
    case XK_Left:
        return KEY_LEFT;
    case XK_Right:
        return KEY_RIGHT;
    case XK_Up:
        return KEY_UP;
    case XK_Down:
        return KEY_DOWN;
    case XK_Shift_L:
        return KEY_LSHIFT;
    case XK_Shift_R:
        return KEY_RSHIFT;
    case XK_Control_L:
        return KEY_LCTRL;
    case XK_Control_R:
        return KEY_RCTRL;
    case XK_Caps_Lock:
        return KEY_CAPSLOCK;
    case XK_space:
        return ' ';
    }
    if( sym >= XK_a && sym <= XK_z )
        return (int)sym;
    if( sym >= XK_0 && sym <= XK_9 )
        return (int)sym;
    if( sym >= 0x20 && sym <= 0x7E )
        return (int)sym;
    return 0;
}

static void renderDirtyArea(void)
{
    int ax, ay, aw, ah, sw, dw, maxRow;

    if( !pixelBuf || !buffer )
        return;

    if( flags & FLAG_LSB_FIRST )
        sw = (WIDTH / 32) * 4;
    else
    {
        const int pixPerWord = 16;
        const int pixLineWidth = ((WIDTH + pixPerWord - 1) / pixPerWord) * pixPerWord;
        sw = pixLineWidth / 8;
    }

    dw = WIDTH * 4;
    maxRow = bufLen / sw;
    if( maxRow > HEIGHT )
        maxRow = HEIGHT;

    ax = dirtyX;
    aw = dirtyW;
    ah = dirtyY + dirtyH;
    if( ah > maxRow )
        ah = maxRow;
    if( ah > HEIGHT )
        ah = HEIGHT;
    ay = dirtyY;
    if( ay >= ah )
        return;

    for( int y = ay; y < ah; y++ )
    {
        int srcLine, dstLine;
        uint32_t* p;

        if( flags & FLAG_BOTTOM_UP )
        {
            srcLine = (HEIGHT - 1 - y);
            dstLine = y;
        } else
        {
            srcLine = y;
            dstLine = y;
        }

        const uint8_t* src = buffer + sw * srcLine;
        p = (uint32_t*)(pixelBuf + dw * dstLine) + ax;

        if( flags & FLAG_LSB_FIRST )
        {
            for( int x = ax; x < ax + aw; x++ )
            {
                const int wordIdx = x / 32;
                const int bitIdx  = x % 32;
                uint32_t word = ((uint32_t*)src)[wordIdx];
                uint32_t v = (word >> bitIdx) & 1;
                *p++ = v ? 0xFFFFFFFF : 0xFF000000;
            }
        } else
        {
            for( int x = ax; x < ax + aw; x++ )
            {
                const int byteIdx = x >> 3;
                if( byteIdx < sw )
                {
                    uint8_t v = src[byteIdx];
                    v = (v >> (7 - (x & 7))) & 1;
                    *p = v ? 0xFF000000 : 0xFFFFFFFF;
                }
                p++;
            }
        }
    }
}

static void blitToWindow(int x, int y, int w, int h)
{
    /* xcb_put_image has a maximum request size. We split into horizontal strips if the data exceeds that limit. */
    const int stride = WIDTH * 4;
    int rowsPerPut;

    if( maxReqBytes == 0 )
        maxReqBytes = xcb_get_maximum_request_length(conn) * 4;

    /* Subtract header size (sizeof request header is 24 bytes, round up) */
    rowsPerPut = ((int)maxReqBytes - 32) / stride;
    if( rowsPerPut <= 0 )
        rowsPerPut = 1;

    while( h > 0 )
    {
        int rows = MIN(h, rowsPerPut);
        const uint8_t* data = pixelBuf + (y * stride) + (x * 4);

        if( x == 0 && w == WIDTH )
        {
            /* Full-width strip: contiguous in pixelBuf */
            xcb_put_image(conn, XCB_IMAGE_FORMAT_Z_PIXMAP,
                          win, gc,
                          (uint16_t)w, (uint16_t)rows,
                          (int16_t)x, (int16_t)y,
                          0, (uint8_t)scr->root_depth,
                          (uint32_t)(w * rows * 4), data);
        } else
        {
            /* Partial width: must copy scanlines to a contiguous buffer */
            uint8_t* tmp = (uint8_t*)malloc(w * rows * 4);
            if( tmp )
            {
                for( int r = 0; r < rows; r++ )
                {
                    const uint8_t* srcRow = pixelBuf + ((y + r) * stride) + (x * 4);
                    memcpy(tmp + r * w * 4, srcRow, w * 4);
                }
                xcb_put_image(conn, XCB_IMAGE_FORMAT_Z_PIXMAP,
                              win, gc,
                              (uint16_t)w, (uint16_t)rows,
                              (int16_t)x, (int16_t)y,
                              0, (uint8_t)scr->root_depth,
                              (uint32_t)(w * rows * 4), tmp);
                free(tmp);
            }
        }

        y += rows;
        h -= rows;
    }
}

static void setupDeleteProtocol(void)
{
    xcb_intern_atom_cookie_t c1, c2;
    xcb_intern_atom_reply_t *r1, *r2;

    c1 = xcb_intern_atom(conn, 0, 12, "WM_PROTOCOLS");
    c2 = xcb_intern_atom(conn, 0, 16, "WM_DELETE_WINDOW");
    r1 = xcb_intern_atom_reply(conn, c1, NULL);
    r2 = xcb_intern_atom_reply(conn, c2, NULL);
    if( r1 && r2 )
    {
        wmProtocols    = r1->atom;
        wmDeleteWindow = r2->atom;
        xcb_change_property(conn, XCB_PROP_MODE_REPLACE, win,
                            wmProtocols, 4 /*XCB_ATOM_ATOM*/, 32, 1, &wmDeleteWindow);
    }
    free(r1);
    free(r2);
}

static void disposeWindow(void)
{
    if( customCursor )
    {
        xcb_free_cursor(conn, customCursor);
        customCursor = 0;
    }
    if( gc )
    {
        xcb_free_gc(conn, gc);
        gc = 0;
    }
    if( win )
    {
        xcb_destroy_window(conn, win);
        win = 0;
    }
    if( conn )
        xcb_flush(conn);
}

#ifndef _MIC_NO_BEGIN_
DllExport void Screen$begin$(void)
{
    /* NOP; connection is established lazily in Open */
}
#endif

DllExport int32_t Screen$Open(uint8_t* buf, int32_t bLen, int32_t w, int32_t h, unsigned int fl)
{
    uint32_t mask;
    uint32_t values[2];

    if( !conn )
    {
        conn = xcb_connect(NULL, NULL);
        if( xcb_connection_has_error(conn) )
        {
            fprintf(stderr, "xcb_connect failed\n");
            conn = NULL;
            return 0;
        }
        scr = xcb_setup_roots_iterator(xcb_get_setup(conn)).data;
        loadKeymap();
    } else
    {
        disposeWindow();
    }

    WIDTH  = w;
    HEIGHT = h;
    buffer = buf;
    bufLen = bLen;
    flags  = fl;

    /* Create window */
    win = xcb_generate_id(conn);
    mask = XCB_CW_BACK_PIXEL | XCB_CW_EVENT_MASK;
    values[0] = scr->white_pixel;
    values[1] = XCB_EVENT_MASK_EXPOSURE
              | XCB_EVENT_MASK_KEY_PRESS
              | XCB_EVENT_MASK_KEY_RELEASE
              | XCB_EVENT_MASK_BUTTON_PRESS
              | XCB_EVENT_MASK_BUTTON_RELEASE
              | XCB_EVENT_MASK_POINTER_MOTION
              | XCB_EVENT_MASK_STRUCTURE_NOTIFY;

    xcb_create_window(conn, XCB_COPY_FROM_PARENT, win, scr->root,
                      0, 0, (uint16_t)w, (uint16_t)h, 0,
                      XCB_WINDOW_CLASS_INPUT_OUTPUT,
                      scr->root_visual,
                      mask, values);

    /* Set window title */
    xcb_change_property(conn, XCB_PROP_MODE_REPLACE, win,
                        XCB_ATOM_WM_NAME, XCB_ATOM_STRING, 8,
                        19, "Micron Screen (XCB)");

    /* WM_DELETE_WINDOW so window close button works */
    setupDeleteProtocol();

    /* Create graphics context */
    gc = xcb_generate_id(conn);
    xcb_create_gc(conn, gc, win, 0, NULL);

    /* Show the window */
    xcb_map_window(conn, win);
    xcb_flush(conn);

    /* Allocate 32bpp pixel buffer */
    if( pixelBuf )
        free(pixelBuf);
    pixelBuf = (uint8_t*)malloc(w * h * 4);
    if( pixelBuf )
        memset(pixelBuf, 0xFF, w * h * 4);

    evtHead = evtTail = evtCount = 0;

    mouseX = mouseY = 0;
    btnLeft = btnMid = btnRight = 0;
    ctrlDown = 0;

    /* Mark whole screen dirty for initial render */
    isDirty = 1;
    dirtyX = 0; dirtyY = 0; dirtyW = w; dirtyH = h;

    return 1;
}

DllExport void Screen$Close(void)
{
    disposeWindow();
    if( pixelBuf )
    {
        free(pixelBuf);
        pixelBuf = NULL;
    }
    if( keymap )
    {
        free(keymap);
        keymap = NULL;
    }
    if( conn )
    {
        xcb_disconnect(conn);
        conn = NULL;
    }
    buffer = NULL;
    bufLen = 0;
}

DllExport void Screen$UpdateArea(int32_t x, int32_t y, int32_t w, int32_t h)
{
    int left, top, right, bottom;

    if( w <= 0 || h <= 0 )
        return;

    left   = MAX(x, 0);
    top    = MAX(y, 0);
    right  = MIN(x + w, WIDTH);
    bottom = MIN(y + h, HEIGHT);
    if( left >= right || top >= bottom )
        return;

    if( !isDirty )
    {
        dirtyX = left;
        dirtyY = top;
        dirtyW = right - left;
        dirtyH = bottom - top;
        isDirty = 1;
    } else
    {
        const int oldRight  = dirtyX + dirtyW;
        const int oldBottom = dirtyY + dirtyH;
        dirtyX = MIN(dirtyX, left);
        dirtyY = MIN(dirtyY, top);
        dirtyW = MAX(oldRight, right) - dirtyX;
        dirtyH = MAX(oldBottom, bottom) - dirtyY;
    }
}

DllExport int32_t Screen$ProcessEvents(int32_t sleep)
{
    xcb_generic_event_t* ev;
    int newEvents = 0;

    if( !conn || !win )
        return -1;

    /* Render dirty area */
    if( isDirty )
    {
        renderDirtyArea();
        blitToWindow(dirtyX, dirtyY, dirtyW, dirtyH);
        xcb_flush(conn);
        isDirty = 0;
        dirtyX = dirtyY = dirtyW = dirtyH = 0;
    }

    /* Wait for events if sleep > 0, using poll on the xcb fd */
#ifndef _WIN32
    if( sleep > 0 )
    {
        struct pollfd pfd;
        pfd.fd = xcb_get_file_descriptor(conn);
        pfd.events = POLLIN;
        poll(&pfd, 1, sleep);
    }
#endif

    /* Compress mouse movement */
    int mouseMoved = 0;
    int finalX = mouseX, finalY = mouseY;

    /* Poll all pending events */
    while( (ev = xcb_poll_for_event(conn)) != NULL )
    {
        const uint8_t type = ev->response_type & 0x7F;
        switch( type )
        {
        case XCB_EXPOSE:
        {
            /* Re-blit the exposed area */
            xcb_expose_event_t* ex = (xcb_expose_event_t*)ev;
            int ex_x = ex->x, ex_y = ex->y;
            int ex_w = ex->width, ex_h = ex->height;
            /* Clamp to buffer bounds */
            if( ex_x + ex_w > WIDTH )
                ex_w = WIDTH - ex_x;
            if( ex_y + ex_h > HEIGHT )
                ex_h = HEIGHT - ex_y;
            if( ex_w > 0 && ex_h > 0 )
            {
                blitToWindow(ex_x, ex_y, ex_w, ex_h);
                xcb_flush(conn);
            }
            break;
        }

        case XCB_MOTION_NOTIFY:
        {
            xcb_motion_notify_event_t* me = (xcb_motion_notify_event_t*)ev;
            finalX = MAX(MIN((int)me->event_x, WIDTH - 1), 0);
            finalY = MAX(MIN((int)me->event_y, HEIGHT - 1), 0);
            mouseMoved = 1;
            break;
        }

        case XCB_BUTTON_PRESS:
        case XCB_BUTTON_RELEASE:
        {
            xcb_button_press_event_t* be = (xcb_button_press_event_t*)ev;
            const int down = (type == XCB_BUTTON_PRESS);
            int btn = 0;
            switch( be->detail )
            {
            case 1: btn = BTN_LEFT;   btnLeft  = down; break;
            case 2: btn = BTN_MIDDLE; btnMid   = down; break;
            case 3: btn = BTN_RIGHT;  btnRight = down; break;
            }
            if( btn )
            {
                evtEnqueue(packEvent(down ? EVT_MOUSE_DOWN : EVT_MOUSE_UP, btn));
                newEvents++;
            }
            break;
        }

        case XCB_KEY_PRESS:
        case XCB_KEY_RELEASE:
        {
            xcb_key_press_event_t* ke = (xcb_key_press_event_t*)ev;
            const int down = (type == XCB_KEY_PRESS);
            xcb_keysym_t sym = keycodeToKeysym(ke->detail, 0);

            /* Track Ctrl for Ctrl+Q quit */
            if( sym == XK_Control_L || sym == XK_Control_R )
                ctrlDown = down;

            if( down && ctrlDown )
            {
                xcb_keysym_t lsym = keycodeToKeysym(ke->detail, 0);
                if( lsym == 0x0071 /* 'q' */ )
                {
                    free(ev);
                    return -1;
                }
            }

            {
                const int code = mapKeysym(sym);
                if( code )
                {
                    evtEnqueue(packEvent(down ? EVT_KEY_DOWN : EVT_KEY_UP, code));
                    newEvents++;
                }
            }

            /* Generate KEY_CHAR on key press for printable characters */
            if( down )
            {
                /* Use column 0 for unshifted, column 1 for shifted */
                int col = (ke->state & XCB_MOD_MASK_SHIFT) ? 1 : 0;
                xcb_keysym_t charSym = keycodeToKeysym(ke->detail, col);
                /* Handle Caps Lock: swap case for letters */
                if( ke->state & XCB_MOD_MASK_LOCK )
                {
                    if( charSym >= 'a' && charSym <= 'z' )
                        charSym -= 32;
                    else if( charSym >= 'A' && charSym <= 'Z' )
                        charSym += 32;
                }
                if( charSym >= 0x20 && charSym <= 0x7E )
                {
                    evtEnqueue(packEvent(EVT_KEY_CHAR, (int)charSym));
                    newEvents++;
                }
            }
            break;
        }

        case XCB_CLIENT_MESSAGE:
        {
            xcb_client_message_event_t* cm = (xcb_client_message_event_t*)ev;
            if( cm->data.data32[0] == wmDeleteWindow )
            {
                free(ev);
                return -1;
            }
            break;
        }

        case XCB_MAPPING_NOTIFY:
        {
            xcb_mapping_notify_event_t* mn = (xcb_mapping_notify_event_t*)ev;
            if( mn->request == XCB_MAPPING_KEYBOARD )
                loadKeymap();
            break;
        }

        default:
            break;
        }

        free(ev);
    }

    if( mouseMoved )
    {
        if( finalX != mouseX || finalY != mouseY )
        {
            /* only send changes */
            evtEnqueue(packMouseMove(finalX, finalY));
            newEvents++;
        }
        mouseX = finalX;
        mouseY = finalY;
    }

    if( xcb_connection_has_error(conn) )
        return -1;

    return newEvents;
}

DllExport int32_t Screen$NextEvent(void)
{
    return evtDequeue();
}

DllExport int32_t Screen$GetMouseState(int32_t* x, int32_t* y)
{
    if( x )
        *x = mouseX;
    if( y )
        *y = mouseY;
    return (btnLeft ? 4 : 0) | (btnMid ? 2 : 0) | (btnRight ? 1 : 0);
}

DllExport void Screen$SetCursor(uint8_t* bits, int32_t w, int32_t h,
                                 int32_t hotX, int32_t hotY)
{
    xcb_pixmap_t srcPix, maskPix;
    uint32_t cursorVal;

    if( !conn || !win )
        return;

    if( customCursor )
    {
        xcb_free_cursor(conn, customCursor);
        customCursor = 0;
    }

    if( !bits || w <= 0 || h <= 0 )
    {
        /* Restore default cursor */
        cursorVal = XCB_CURSOR_NONE;
        xcb_change_window_attributes(conn, win, XCB_CW_CURSOR, &cursorVal);
        xcb_flush(conn);
        return;
    }

    /* Create source pixmap (1bpp): set bits = black foreground */
    srcPix = xcb_generate_id(conn);
    xcb_create_pixmap(conn, 1, srcPix, win, (uint16_t)w, (uint16_t)h);

    /* Create mask pixmap (1bpp): all set = fully opaque */
    maskPix = xcb_generate_id(conn);
    xcb_create_pixmap(conn, 1, maskPix, win, (uint16_t)w, (uint16_t)h);

    {
        /* GC for the 1-bit pixmaps */
        xcb_gcontext_t pixGc = xcb_generate_id(conn);
        uint32_t gcvals[2];
        int rowBytes = (w + 7) / 8;

        /* xcb_put_image for 1bpp XY_BITMAP expects rows padded to 32-bit */
        int padBytes = ((w + 31) / 32) * 4;
        uint8_t* padded = (uint8_t*)calloc(padBytes * h, 1);

        if( padded )
        {
            /* Copy bitmap data, reversing bit order from MSB-first to LSB-first
             * because XCB XY_BITMAP uses LSB-first bit order within bytes */
            for( int y = 0; y < h; y++ )
            {
                for( int b = 0; b < rowBytes; b++ )
                {
                    uint8_t src = bits[y * rowBytes + b];
                    uint8_t dst = 0;
                    /* Reverse bits: MSB-first -> LSB-first */
                    dst |= (src >> 7) & 0x01;
                    dst |= (src >> 5) & 0x02;
                    dst |= (src >> 3) & 0x04;
                    dst |= (src >> 1) & 0x08;
                    dst |= (src << 1) & 0x10;
                    dst |= (src << 3) & 0x20;
                    dst |= (src << 5) & 0x40;
                    dst |= (src << 7) & 0x80;
                    padded[y * padBytes + b] = dst;
                }
            }

            /* Put cursor shape into source pixmap */
            gcvals[0] = 1; /* foreground = 1 */
            gcvals[1] = 0; /* background = 0 */
            xcb_create_gc(conn, pixGc, srcPix,
                          XCB_GC_FOREGROUND | XCB_GC_BACKGROUND, gcvals);
            xcb_put_image(conn, XCB_IMAGE_FORMAT_XY_BITMAP,
                          srcPix, pixGc,
                          (uint16_t)w, (uint16_t)h, 0, 0, 0, 1,
                          (uint32_t)(padBytes * h), padded);

            /* Mask = same as source: only set pixels are visible */
            xcb_change_gc(conn, pixGc, XCB_GC_FOREGROUND | XCB_GC_BACKGROUND, gcvals);
            xcb_put_image(conn, XCB_IMAGE_FORMAT_XY_BITMAP,
                          maskPix, pixGc,
                          (uint16_t)w, (uint16_t)h, 0, 0, 0, 1,
                          (uint32_t)(padBytes * h), padded);
            free(padded);
        }

        xcb_free_gc(conn, pixGc);
    }

    /* Create the cursor: black foreground, white background */
    customCursor = xcb_generate_id(conn);
    xcb_create_cursor(conn, customCursor,
                      srcPix, maskPix,
                      0, 0, 0,           /* fg: black */
                      0xFFFF, 0xFFFF, 0xFFFF, /* bg: white */
                      (uint16_t)hotX, (uint16_t)hotY);

    xcb_free_pixmap(conn, srcPix);
    xcb_free_pixmap(conn, maskPix);

    /* Apply cursor to window */
    cursorVal = customCursor;
    xcb_change_window_attributes(conn, win, XCB_CW_CURSOR, &cursorVal);
    xcb_flush(conn);
}

DllExport void Screen$SetCursorPos(int32_t x, int32_t y)
{
    if( !conn || !win )
        return;
    xcb_warp_pointer(conn, XCB_NONE, win, 0, 0, 0, 0, (int16_t)x, (int16_t)y);
    xcb_flush(conn);
}

DllExport uint32_t Screen$GetTicks(void)
{
    return getMillis();
}
