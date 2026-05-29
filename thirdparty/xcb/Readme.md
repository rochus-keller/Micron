This is a version of libxcb used as a backend for Micron ScreenXcb.c. The idea is to 
provide a GUI for the Musl based i386 Linux build with no extra dependencies.

## Regenerating Protocol Sources

The `xproto.c`, `bigreq.c`, `xc_misc.c` files are auto-generated from
xcb-proto XML descriptions. To regenerate (only needed if upgrading xcb):

```bash
git clone https://gitlab.freedesktop.org/xorg/lib/libxcb.git
git clone https://gitlab.freedesktop.org/xorg/proto/xcbproto.git

PYTHONPATH=xcbproto python3 libxcb/src/c_client.py \
    -c "libxcb" -l "libxcb" -s "3" xcbproto/src/xproto.xml
PYTHONPATH=xcbproto python3 libxcb/src/c_client.py \
    -c "libxcb" -l "libxcb" -s "3" xcbproto/src/bigreq.xml
PYTHONPATH=xcbproto python3 libxcb/src/c_client.py \
    -c "libxcb" -l "libxcb" -s "3" xcbproto/src/xc_misc.xml
```


## License

- `libxcb` sources: MIT/X11 license
- `xcb_auth.c`: MIT license (self-contained reimplementation)
