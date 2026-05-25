#ifndef _ST80_DISPLAY_
#define _ST80_DISPLAY_

#ifdef _WIN32
#define DllExport __declspec(dllexport)
#else
#define DllExport
#endif

DllExport void Display$setScreenBuffer(uint8_t* b, int32_t len, int32_t w, int32_t h);
DllExport int32_t Display$processEvents();
DllExport uint16_t Display$nextEvent();
DllExport void Display$setCursorPos(int32_t x, int32_t y);
DllExport void Display$setCursorBuffer(uint8_t* b, int32_t w, int32_t h);
DllExport void Display$updateArea(int32_t x,int32_t y,int32_t w,int32_t h,int32_t cx,int32_t cy,int32_t cw,int32_t ch);
DllExport void Display$close();
DllExport uint32_t Display$getTicks();

#endif _ST80_DISPLAY_
