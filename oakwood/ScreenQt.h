#ifndef _MIC_SCREENQT_
#define _MIC_SCREENQT_

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

#include <QWidget>
#include <QQueue>

namespace Screen
{
    class Widget : public QWidget
    {
        Q_OBJECT
    public:
        explicit Widget(QWidget* parent = 0);
        ~Widget();
        static Widget* inst();
        static bool s_run;

        void setBuffer(uint8_t* buf, int bufLen, int w, int h, unsigned int flags);
        void updateArea(int x, int y, int w, int h);
        void processAndRender();

        int32_t nextEvent();
        int eventsPending() const { return d_events.size(); }

        void getMouseState(int* x, int* y, int* buttons);
        void setCursorBitmap(uint8_t* bits, int w, int h, int hotX, int hotY);
        void warpCursor(int x, int y);

    signals:
        void sigEventQueue();

    protected:
        void paintEvent(QPaintEvent*);
        void closeEvent(QCloseEvent*);
        void mouseMoveEvent(QMouseEvent*);
        void mousePressEvent(QMouseEvent*);
        void mouseReleaseEvent(QMouseEvent*);
        void keyPressEvent(QKeyEvent*);
        void keyReleaseEvent(QKeyEvent*);
        void inputMethodEvent(QInputMethodEvent*);

    private:
        void enqueueEvent(int32_t e);
        void handleMouseButton(bool press, int qtButton);
        int mapKeyCode(int qtKey);
        void updateImage();

        uint8_t* d_buffer;
        int d_bufLen;
        int d_width, d_height;
        unsigned int d_flags;
        QImage d_screen;
        QRect d_updateArea;
        QQueue<int32_t> d_events;
        QPoint d_mousePos;
        bool d_btnLeft, d_btnMid, d_btnRight;
        bool d_ctrlDown;
    };
}

#endif // _MIC_SCREENQT_
