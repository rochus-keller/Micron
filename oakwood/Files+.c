#include "Files.h"
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

static Files$Handle Stdin = {0}, Stdout = {0}, Stderr = {0};

struct Files$Handle* Files$Open(const char* name, int mode)
{
    // ReadOnly = 0, Create = 1, Modify = 2
    const char* m = mode == 0 ? "rb" : mode == 1 ? "wb+" : "rb+";
    FILE* f = fopen(name, m);
    if( f )
    {
        Files$Handle* res = (Files$Handle*)malloc(sizeof(Files$Handle));
        res->f = f;
        return res;
    }
    return 0;
}
 
struct Files$Handle* Files$System(int what)
{
    // Stdin = 0, Stdout = 1, Stderr = 2
    switch(what)
    {
    case 0:
        Stdin.f = stdin;
        return &Stdin;
    case 1:
        Stdout.f = stdout;
        return &Stdout;
    case 2:
        Stderr.f = stderr;
        return &Stderr;
    default:
        assert(0);
    }

    return 0;
}

void Files$Close(struct Files$Handle* f)
{
    if( f )
    {
        fclose(f->f);
        free(f);
    }
}

unsigned char Files$Delete(char* name)
{
    return remove(name) == 0;
}

unsigned char Files$Eof(struct Files$Handle* f)
{
    if( f )
        return feof(f->f);
    else
        return 1;
}

unsigned char Files$Read(struct Files$Handle* f, unsigned char* x)
{
    assert(f);
    assert(f->f);
    const int count = fread(x, 1, 1, f->f);
    return count == 1;
}

unsigned char Files$Write(struct Files$Handle* f, unsigned char x)
{
    assert(f);
    assert(f->f);
    const int count = fwrite(&x, 1, 1, f->f);
    return count == 1;
}

unsigned int Files$ReadBytes(struct Files$Handle* f, unsigned char* x, unsigned int n)
{
    assert(f);
    assert(f->f);
    const int count = fread(x, 1, n, f->f);
    if( count >= 0 )
        return count;
    else
        return 0;
}

unsigned char Files$Set(struct Files$Handle* f, unsigned int pos)
{
    assert(f);
    assert(f->f);
    const int res = fseek(f->f, pos, SEEK_SET);
    return res == 0;
}

unsigned int Files$Length(struct Files$Handle* f)
{
    assert(f);
    assert(f->f);
    
    const long pos = ftell(f->f);
    
    if (fseek(f->f, 0, SEEK_END) != 0) {   
        return 0;
    }

    const long size = ftell(f->f);               

    fseek(f->f, pos, SEEK_SET);
    
    if( size >= 0 )
        return size;   
    else
        return 0;                     
}

unsigned int Files$Pos(struct Files$Handle* f)
{
    assert(f);
    assert(f->f);

    const long pos = ftell(f->f);
    if( pos >= 0 )
        return pos;
    else
        return 0;
}

unsigned char Files$Rename(char* old, char* new_)
{
    return rename(old, new_) == 0;
}

// TODO: complete


#ifdef _WIN32
#include <windows.h>

struct Files$DirHandle {
    HANDLE hFind;
    WIN32_FIND_DATAA findData;
    int first_returned;
};

struct Files$DirHandle* Files$OpenDir(const char* path) {
    char search_path[MAX_PATH];
    // Windows requires an asterisk to search a directory (e.g., "C:\path\*")
    snprintf(search_path, sizeof(search_path), "%s\\*", path);
    
    WIN32_FIND_DATAA fd;
    HANDLE h = FindFirstFileA(search_path, &fd);
    
    if (h == INVALID_HANDLE_VALUE) {
        return NULL; // Directory not found or access denied
    }
    
    struct Files$DirHandle* dir = (struct Files$DirHandle*)malloc(sizeof(struct Files$DirHandle));
    if (!dir) {
        FindClose(h);
        return NULL;
    }
    
    dir->hFind = h;
    dir->findData = fd;
    dir->first_returned = 0;
    return dir;
}

unsigned char Files$ReadDir(struct Files$DirHandle* d, char* name, unsigned int maxLen, unsigned char* isDir) {
    if (!d) return 0;

    while (1) {
        if (!d->first_returned) {
            d->first_returned = 1;
        } else {
            if (!FindNextFileA(d->hFind, &d->findData)) {
                return 0; // No more files
            }
        }

        // Skip the current "." and parent ".." directory links
        if (strcmp(d->findData.cFileName, ".") == 0 || strcmp(d->findData.cFileName, "..") == 0) {
            continue;
        }

        snprintf(name, maxLen, "%s", d->findData.cFileName);
        
        if (isDir) {
            *isDir = (d->findData.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) ? 1 : 0;
        }
        
        return 1;
    }
}

void Files$CloseDir(struct Files$DirHandle* d) {
    if (d) {
        FindClose(d->hFind);
        free(d);
    }
}

#else // POSIX

#include <dirent.h>
#include <sys/stat.h>

struct Files$DirHandle {
    DIR* dir;
    char basePath[1024]; // Required for stat() fallback checking if an item is a directory
};

struct Files$DirHandle* Files$OpenDir(const char* path) {
    DIR* d = opendir(path);
    if (!d) {
        return NULL; // Directory not found or access denied
    }

    struct Files$DirHandle* dir = (struct Files$DirHandle*)malloc(sizeof(struct Files$DirHandle));
    if (!dir) {
        closedir(d);
        return NULL;
    }
    
    dir->dir = d;
    snprintf(dir->basePath, sizeof(dir->basePath), "%s", path);
    return dir;
}

unsigned char Files$ReadDir(struct Files$DirHandle* d, char* name, unsigned int maxLen, unsigned char* isDir) {
    if (!d || !d->dir) return 0;

    struct dirent* entry;
    while (1) {
        entry = readdir(d->dir);
        if (!entry) {
            return 0; // No more files
        }

        // Skip the current "." and parent ".." directory links
        if (strcmp(entry->d_name, ".") == 0 || strcmp(entry->d_name, "..") == 0) {
            continue;
        }

        snprintf(name, maxLen, "%s", entry->d_name);

        if (isDir) {
            int is_directory = 0;
            
#ifdef _DIRENT_HAVE_D_TYPE
            if (entry->d_type != DT_UNKNOWN) {
                is_directory = (entry->d_type == DT_DIR);
            } else
#endif
            {
                char fullPath[2048];
                snprintf(fullPath, sizeof(fullPath), "%s/%s", d->basePath, entry->d_name);
                struct stat st;
                if (stat(fullPath, &st) == 0) {
                    is_directory = S_ISDIR(st.st_mode);
                }
            }
            
            *isDir = is_directory ? 1 : 0;
        }
        
        return 1;
    }
}

void Files$CloseDir(struct Files$DirHandle* d) {
    if (d) {
        if (d->dir) {
            closedir(d->dir);
        }
        free(d);
    }
}

#endif
