/* Minimal config.h for libxcb static build (Linux i386, glibc/musl) */
#ifndef CONFIG_H
#define CONFIG_H

/* We have sendmsg on Linux */
#define HAVE_SENDMSG 1

/* We have getaddrinfo on Linux */
#define HAVE_GETADDRINFO 1

/* Abstract sockets (Linux-only) for X11 local connections */
#define HAVE_ABSTRACT_SOCKETS 1

/* IOV_MAX fallback */
#ifndef IOV_MAX
#define IOV_MAX 1024
#endif

/* No XDM auth - we don't need libXdmcp */
/* #undef HASXDMAUTH */

/* No Trusted Solaris */
/* #undef HAVE_TSOL_LABEL_H */
/* #undef HAVE_IS_SYSTEM_LABELED */

/* No sockaddr_sun_len on Linux */
/* #undef HAVE_SOCKADDR_SUN_LEN */

/* XCB internal buffer queue size */
#define XCB_QUEUE_BUFFER_SIZE 16384

#endif /* CONFIG_H */
