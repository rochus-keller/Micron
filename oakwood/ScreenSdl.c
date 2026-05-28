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

#include <SDL2/SDL.h>
#include <assert.h>
#include <string.h>

#ifdef _WIN32
#define DllExport __declspec(dllexport)
#else
#define DllExport
#endif

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

// Abstract key codes (must match Screen.mic)
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

static SDL_Window*   window   = NULL;
static SDL_Texture*  texture  = NULL;
static SDL_Renderer* renderer = NULL;
static uint8_t* buffer  = NULL;
static int      bufLen  = 0;
static uint8_t* pixelBuf = NULL;
static int WIDTH = 0, HEIGHT = 0;
static unsigned int flags = 0;
static int sdlInitialized = 0;

// Dirty rectangle tracking
static SDL_Rect dirtyArea = {0, 0, 0, 0};
static int isDirty = 0;

// Mouse state
static int mouseX = 0, mouseY = 0;
static int btnLeft = 0, btnMid = 0, btnRight = 0;

// Modifier tracking (for Ctrl+Q quit shortcut)
static int ctrlDown = 0;

// Custom cursor
static SDL_Cursor* customCursor = NULL;

#define MAX(x, y) (((x) > (y)) ? (x) : (y))
#define MIN(x, y) (((x) < (y)) ? (x) : (y))

static void updateTexture(SDL_Rect* patch)
{
    int ax, ay, aw, ah, sw, dw, maxRow;

    if( patch->w <= 0 || patch->h <= 0 )
        return;

    if( flags & FLAG_LSB_FIRST )
    {
        // Oberon-style: LSB-first, 32-bit word aligned
        sw = (WIDTH / 32) * 4;
    } else
    {
        // Smalltalk-style: MSB-first, 16-bit word aligned
        const int pixPerWord = 16;
        const int pixLineWidth = ((WIDTH + pixPerWord - 1) / pixPerWord) * pixPerWord;
        sw = pixLineWidth / 8;
    }

    dw = WIDTH * 4;
    maxRow = bufLen / sw;
    if( maxRow > HEIGHT )
        maxRow = HEIGHT;

    ax = patch->x;
    aw = patch->w;
    ah = patch->y + patch->h;
    if( ah > maxRow )
        ah = maxRow;
    if( ah > HEIGHT )
        ah = HEIGHT;
    ay = patch->y;
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
            // Oberon: bit 0 is leftmost pixel within each 32-bit word
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
            // Smalltalk: MSB-first within each byte
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

    // Upload modified rectangle to GPU
    {
        uint8_t* patchPixels = pixelBuf + (ay * dw) + (ax * 4);
        SDL_UpdateTexture(texture, patch, patchPixels, dw);
    }
}

static int mapKeyCode(SDL_Keycode sym)
{
    switch( sym )
    {
    case SDLK_BACKSPACE:
        return KEY_BACKSPACE;
    case SDLK_TAB:
        return KEY_TAB;
    case SDLK_RETURN:
        return KEY_RETURN;
    case SDLK_ESCAPE:
        return KEY_ESCAPE;
    case SDLK_DELETE:
        return KEY_DELETE;
    case SDLK_LEFT:
        return KEY_LEFT;
    case SDLK_RIGHT:
        return KEY_RIGHT;
    case SDLK_UP:
        return KEY_UP;
    case SDLK_DOWN:
        return KEY_DOWN;
    case SDLK_LSHIFT:
        return KEY_LSHIFT;
    case SDLK_RSHIFT:
        return KEY_RSHIFT;
    case SDLK_LCTRL:
        return KEY_LCTRL;
    case SDLK_RCTRL:
        return KEY_RCTRL;
    case SDLK_CAPSLOCK:
        return KEY_CAPSLOCK;
    case SDLK_SPACE:
        return ' ';
    }
    if( sym >= SDLK_a && sym <= SDLK_z )
        return sym;
    if( sym >= SDLK_0 && sym <= SDLK_9 )
        return sym;
    if( sym >= 0x20 && sym <= 0x7E )
        return sym;
    return 0;
}

static int decodeUtf8char(const char* encoded)
{
    int c = (unsigned char)encoded[0];
    if( c <= 0x7F )
        return c;
    if( c >= 0xC0 && c <= 0xDF )
        return ((c & 0x1F) << 6) | ((unsigned char)encoded[1] & 0x3F);
    return '?';
}

static int mapButton(int sdlButton)
{
    switch( sdlButton )
    {
    case SDL_BUTTON_LEFT:
        return BTN_LEFT;
    case SDL_BUTTON_MIDDLE:
        return BTN_MIDDLE;
    case SDL_BUTTON_RIGHT:
        return BTN_RIGHT;
    default:
        return 0;
    }
}

static void disposeWindow(void)
{
    if( texture )
        SDL_DestroyTexture(texture);
    if( renderer )
        SDL_DestroyRenderer(renderer);
    if( window )
        SDL_DestroyWindow(window);
    window   = NULL;
    renderer = NULL;
    texture  = NULL;
}

#ifndef _MIC_NO_BEGIN_
DllExport void Screen$begin$(void)
{
    // NOP; SDL is initialized lazily in Open
}
#endif

DllExport int32_t Screen$Open(uint8_t* buf, int32_t bLen, int32_t w, int32_t h, unsigned int fl)
{
    if( !sdlInitialized )
    {
        if( SDL_Init(SDL_INIT_VIDEO) < 0 )
        {
            fprintf(stderr, "SDL_Init failed: %s\n", SDL_GetError());
            return 0;
        }
        sdlInitialized = 1;
    }

    disposeWindow();

    WIDTH  = w;
    HEIGHT = h;
    buffer = buf;
    bufLen = bLen;
    flags  = fl;

    window = SDL_CreateWindow("Micron Screen (SDL)",
                              SDL_WINDOWPOS_UNDEFINED,
                              SDL_WINDOWPOS_UNDEFINED,
                              WIDTH, HEIGHT,
                              SDL_WINDOW_SHOWN);
    if( !window )
    {
        SDL_Log("Cannot create window: %s", SDL_GetError());
        return 0;
    }

    renderer = SDL_CreateRenderer(window, -1, SDL_RENDERER_ACCELERATED);
    if( !renderer )
    {
        SDL_Log("Cannot create renderer: %s", SDL_GetError());
        disposeWindow();
        return 0;
    }

    texture = SDL_CreateTexture(renderer,
                                SDL_PIXELFORMAT_ARGB8888,
                                SDL_TEXTUREACCESS_STREAMING,
                                WIDTH, HEIGHT);
    if( !texture )
    {
        SDL_Log("Cannot create texture: %s", SDL_GetError());
        disposeWindow();
        return 0;
    }

    if( pixelBuf )
        free(pixelBuf);
    pixelBuf = (uint8_t*)malloc(WIDTH * HEIGHT * 4);
    if( pixelBuf )
        memset(pixelBuf, 0xFF, WIDTH * HEIGHT * 4);

    // Reset event queue
    evtHead = evtTail = evtCount = 0;

    // Reset mouse state
    mouseX = mouseY = 0;
    btnLeft = btnMid = btnRight = 0;
    ctrlDown = 0;

    // Mark whole screen dirty for initial render
    isDirty = 1;
    dirtyArea.x = 0;
    dirtyArea.y = 0;
    dirtyArea.w = w;
    dirtyArea.h = h;

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
    if( customCursor )
    {
        SDL_FreeCursor(customCursor);
        customCursor = NULL;
    }
    buffer = NULL;
    bufLen = 0;
}

DllExport void Screen$UpdateArea(int32_t x, int32_t y, int32_t w, int32_t h)
{
    int left, top, right, bottom;

    if( w <= 0 || h <= 0 )
        return;

    // Clamp to screen bounds
    left   = MAX(x, 0);
    top    = MAX(y, 0);
    right  = MIN(x + w, WIDTH);
    bottom = MIN(y + h, HEIGHT);
    if( left >= right || top >= bottom )
        return;

    if( !isDirty )
    {
        dirtyArea.x = left;
        dirtyArea.y = top;
        dirtyArea.w = right - left;
        dirtyArea.h = bottom - top;
        isDirty = 1;
    } else
    {
        const int oldRight  = dirtyArea.x + dirtyArea.w;
        const int oldBottom = dirtyArea.y + dirtyArea.h;
        dirtyArea.x = MIN(dirtyArea.x, left);
        dirtyArea.y = MIN(dirtyArea.y, top);
        dirtyArea.w = MAX(oldRight, right) - dirtyArea.x;
        dirtyArea.h = MAX(oldBottom, bottom) - dirtyArea.y;
    }
}

DllExport int32_t Screen$ProcessEvents(int32_t sleep)
{
    SDL_Event e;
    SDL_Rect r;
    int hasEvent;
    int newEvents = 0;

    if( !window )
        return -1;

    if( isDirty )
    {
        updateTexture(&dirtyArea);
        SDL_RenderClear(renderer);
        r.x = 0; r.y = 0; r.w = WIDTH; r.h = HEIGHT;
        SDL_RenderCopy(renderer, texture, &r, &r);
        SDL_RenderPresent(renderer);
        isDirty = 0;
        dirtyArea.x = dirtyArea.y = dirtyArea.w = dirtyArea.h = 0;
    }

    hasEvent = (sleep > 0) ? SDL_WaitEventTimeout(&e, sleep) : SDL_PollEvent(&e);

    // Compress mouse movement: track final position
    int mouseMoved = 0;
    int finalX = mouseX, finalY = mouseY;

    while( hasEvent )
    {
        switch( e.type )
        {
        case SDL_QUIT:
        case SDL_APP_TERMINATING:
            return -1;

        case SDL_WINDOWEVENT:
            if( e.window.event == SDL_WINDOWEVENT_CLOSE )
                return -1;
            break;

        case SDL_MOUSEMOTION:
            finalX = MAX(MIN(e.motion.x, WIDTH - 1), 0);
            finalY = MAX(MIN(e.motion.y, HEIGHT - 1), 0);
            mouseMoved = 1;
            break;

        case SDL_MOUSEBUTTONDOWN:
        case SDL_MOUSEBUTTONUP:
        {
            const int down = (e.button.state == SDL_PRESSED);
            const int btn = mapButton(e.button.button);
            if( btn )
            {
                if( btn == BTN_LEFT )   btnLeft  = down;
                if( btn == BTN_MIDDLE ) btnMid   = down;
                if( btn == BTN_RIGHT )  btnRight = down;
                evtEnqueue(packEvent(down ? EVT_MOUSE_DOWN : EVT_MOUSE_UP, btn));
                newEvents++;
            }
            break;
        }

        case SDL_TEXTINPUT:
        {
            const int ch = decodeUtf8char(e.text.text);
            if( ch > 0 )
            {
                evtEnqueue(packEvent(EVT_KEY_CHAR, ch));
                newEvents++;
            }
            break;
        }

        case SDL_KEYDOWN:
        case SDL_KEYUP:
        {
            const int down = (e.key.state == SDL_PRESSED);
            SDL_Keycode sym = e.key.keysym.sym;

            // Track Ctrl for Ctrl+Q quit
            if( sym == SDLK_LCTRL || sym == SDLK_RCTRL )
                ctrlDown = down;
            if( sym == SDLK_q && down && ctrlDown )
                return -1;

            {
                const int code = mapKeyCode(sym);
                if( code )
                {
                    evtEnqueue(packEvent(down ? EVT_KEY_DOWN : EVT_KEY_UP, code));
                    newEvents++;
                }
            }
            break;
        }
        } // switch

        hasEvent = SDL_PollEvent(&e);
    }

    // Push compressed mouse movement as a single event
    if( mouseMoved )
    {
        if( finalX != mouseX || finalY != mouseY )
        {
            evtEnqueue(packMouseMove(finalX, finalY));
            newEvents++;
        }
        mouseX = finalX;
        mouseY = finalY;
    }

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
    if( customCursor )
    {
        SDL_FreeCursor(customCursor);
        customCursor = NULL;
    }

    if( bits && w > 0 && h > 0 )
    {
        customCursor = SDL_CreateCursor(bits, bits, w, h, hotX, hotY);
        SDL_SetCursor(customCursor);
    } else
    {
        SDL_SetCursor(SDL_GetDefaultCursor());
    }
}

DllExport void Screen$SetCursorPos(int32_t x, int32_t y)
{
    if( window )
        SDL_WarpMouseInWindow(window, x, y);
}

DllExport uint32_t Screen$GetTicks(void)
{
    return SDL_GetTicks();
}
