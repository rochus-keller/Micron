/* Minimal self-contained xcb_auth implementation.
 * Reads MIT-MAGIC-COOKIE-1 from ~/.Xauthority without libXau.
 * Falls back gracefully to no-auth if .Xauthority is not available.
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#include "xcb.h"
#include "xcbint.h"

/* .Xauthority entry families (from X11/X.h) */
#define XAUTH_FAMILY_INTERNET  0
#define XAUTH_FAMILY_LOCAL     256
#define XAUTH_FAMILY_WILD      65535
#define XAUTH_FAMILY_INTERNET6 6

#define AUTH_PROTO_MIT_MAGIC_COOKIE "MIT-MAGIC-COOKIE-1"
#define AUTH_PROTO_MIT_MAGIC_COOKIE_LEN 18

/* Read a big-endian 16-bit value from file */
static int read_short(FILE *f, unsigned short *val)
{
    unsigned char buf[2];
    if( fread(buf, 1, 2, f) != 2 )
        return 0;
    *val = (unsigned short)((buf[0] << 8) | buf[1]);
    return 1;
}

/* Read a counted string (2-byte BE length + data) from file */
static int read_counted_string(FILE *f, char **out, unsigned short *outlen)
{
    unsigned short len;
    if( !read_short(f, &len) )
        return 0;
    if( len == 0 )
    {
        *out = NULL;
        *outlen = 0;
        return 1;
    }
    *out = malloc(len);
    if( !*out )
        return 0;
    if( fread(*out, 1, len, f) != len )
    {
        free(*out);
        *out = NULL;
        return 0;
    }
    *outlen = len;
    return 1;
}

/* Skip a counted string without allocating */
static int skip_counted_string(FILE *f)
{
    unsigned short len;
    if( !read_short(f, &len) )
        return 0;
    if( len > 0 && fseek(f, len, SEEK_CUR) != 0 )
        return 0;
    return 1;
}

/* Find MIT-MAGIC-COOKIE-1 for the given display in ~/.Xauthority */
static int find_auth_cookie(int display, char **cookie_out, unsigned short *cookie_len_out)
{
    const char *xauthority;
    FILE *f;
    char dispstr[32];
    int displen;
    char hostname[256];

    xauthority = getenv("XAUTHORITY");
    if( !xauthority )
    {
        const char *home = getenv("HOME");
        static char path[512];
        if( !home )
            return 0;
        snprintf(path, sizeof(path), "%s/.Xauthority", home);
        xauthority = path;
    }

    f = fopen(xauthority, "rb");
    if( !f )
        return 0;

    displen = snprintf(dispstr, sizeof(dispstr), "%d", display);
    if( gethostname(hostname, sizeof(hostname)) != 0 )
        hostname[0] = '\0';

    while( !feof(f) )
    {
        unsigned short family;
        char *addr = NULL, *number = NULL, *name = NULL, *data = NULL;
        unsigned short addr_len, number_len, name_len, data_len;

        if( !read_short(f, &family) )
            break;
        if( !read_counted_string(f, &addr, &addr_len) )
            break;
        if( !read_counted_string(f, &number, &number_len) )
        {
            free(addr);
            break;
        }
        if( !read_counted_string(f, &name, &name_len) )
        {
            free(addr);
            free(number);
            break;
        }
        if( !read_counted_string(f, &data, &data_len) )
        {
            free(addr);
            free(number);
            free(name);
            break;
        }

        /* Match: family must be LOCAL or WILD, display number must match */
        int match = 0;
        if( family == XAUTH_FAMILY_WILD )
        {
            match = 1;
        }
        else if( family == XAUTH_FAMILY_LOCAL )
        {
            /* Check display number matches */
            if( number_len == (unsigned short)displen &&
                memcmp(number, dispstr, displen) == 0 )
            {
                match = 1;
            }
        }

        if( match && name_len == AUTH_PROTO_MIT_MAGIC_COOKIE_LEN &&
            memcmp(name, AUTH_PROTO_MIT_MAGIC_COOKIE, AUTH_PROTO_MIT_MAGIC_COOKIE_LEN) == 0 )
        {
            /* Found it */
            free(addr);
            free(number);
            free(name);
            fclose(f);
            *cookie_out = data;
            *cookie_len_out = data_len;
            return 1;
        }

        free(addr);
        free(number);
        free(name);
        free(data);
    }

    fclose(f);
    return 0;
}

int _xcb_get_auth_info(int fd, xcb_auth_info_t *info, int display)
{
    char *cookie = NULL;
    unsigned short cookie_len = 0;

    (void)fd;

    memset(info, 0, sizeof(*info));

    if( !find_auth_cookie(display, &cookie, &cookie_len) )
        return 0; /* No auth available — connection proceeds without authentication */

    /* Set the auth protocol name */
    info->name = malloc(AUTH_PROTO_MIT_MAGIC_COOKIE_LEN);
    if( !info->name )
    {
        free(cookie);
        return 0;
    }
    memcpy(info->name, AUTH_PROTO_MIT_MAGIC_COOKIE, AUTH_PROTO_MIT_MAGIC_COOKIE_LEN);
    info->namelen = AUTH_PROTO_MIT_MAGIC_COOKIE_LEN;

    /* Set the auth data (the cookie) */
    info->data = cookie;
    info->datalen = cookie_len;

    return 1;
}
