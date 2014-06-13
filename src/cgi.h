#ifndef __CGI_H_
#define __CGI_H_

#include <stddef.h>

int cgi_init(char *global_argv[]);
size_t cgi_item(char *str, size_t maxlen);
void cgi_header(char *content_type);

#endif
