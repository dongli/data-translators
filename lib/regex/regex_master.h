/*
MIT License

Copyright (c) 2018 Joshua Oliva

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/
#ifndef REGEX_MASTER_H
#define REGEX_MASTER_H
#ifdef __cplusplus
    #include <string>
    #include <regex>
#else
    #include <stdbool.h>
#endif
#include <stdlib.h>
#include <string.h>

#define regex_master_version 1.00

#ifdef __cplusplus
using namespace std;
extern "C"{
#endif
    struct regex_str{
        char*str;
        int len;
    };
    struct regex_data{
        char*str;
        int len;
        int start;
        int end;
    };
    struct regex_container{
        struct regex_data *x;
        int len;
    };
    char * regex_replace0(char*str,char *rgx,char *str_replace,int*str_length);
    char * regex_replace1(char*str,char *rgx,char *str_replace,int *mflags,int *mflags_size,int*str_length);
    char * regex_replace2(char*str,char *rgx,char *str_replace,int *sflags,int *sflags_size,int *mflags,int *mflags_size,int*str_length);
    char * regex_replace3(char*str,char *rgx,char *str_replace,int *sflags,int *sflags_size,int*str_length);

    bool regex_contains0(char*str,char*rgx);
    bool regex_contains1(char*str,char*rgx,int *mflags,int *mflags_size);
    bool regex_contains2(char*str,char*rgx,int *sflags,int *sflags_size,int *mflags,int *mflags_size);
    bool regex_contains3(char*str,char*rgx,int *sflags,int *sflags_size);

    bool regex_match_logical0(char*str,char*rgx);
    bool regex_match_logical1(char*str,char*rgx,int *mflags,int *mflags_size);
    bool regex_match_logical2(char*str,char*rgx,int *sflags,int *sflags_size,int *mflags,int *mflags_size);
    bool regex_match_logical3(char*str,char*rgx,int *sflags,int *sflags_size);

    int regex_indexof0(char*str,char*rgx);
    int regex_indexof1(char*str,char*rgx,int *mflags,int *mflags_size);
    int regex_indexof2(char*str,char*rgx,int *sflags,int *sflags_size,int *mflags,int *mflags_size);
    int regex_indexof3(char*str,char*rgx,int *sflags,int *sflags_size);

    int regex_lastindexof0(char*str,char*rgx);
    int regex_lastindexof1(char*str,char*rgx,int *mflags,int *mflags_size);
    int regex_lastindexof2(char*str,char*rgx,int *sflags,int *sflags_size,int *mflags,int *mflags_size);
    int regex_lastindexof3(char*str,char*rgx,int *sflags,int *sflags_size);

    int regex_indexofend0(char*str,char*rgx);
    int regex_indexofend1(char*str,char*rgx,int *mflags,int *mflags_size);
    int regex_indexofend2(char*str,char*rgx,int *sflags,int *sflags_size,int *mflags,int *mflags_size);
    int regex_indexofend3(char*str,char*rgx,int *sflags,int *sflags_size);

    int regex_lastindexofend0(char*str,char*rgx);
    int regex_lastindexofend1(char*str,char*rgx,int *mflags,int *mflags_size);
    int regex_lastindexofend2(char*str,char*rgx,int *sflags,int *sflags_size,int *mflags,int *mflags_size);
    int regex_lastindexofend3(char*str,char*rgx,int *sflags,int *sflags_size);

    struct regex_container* regex_search0(char*str,char*rgx,int*max_size);
    struct regex_container* regex_search1(char*str,char*rgx,int *mflags,int *mflags_size,int*max_size);
    struct regex_container* regex_search2(char*str,char*rgx,int *sflags,int *sflags_size,int *mflags,int *mflags_size,int*max_size);
    struct regex_container* regex_search3(char*str,char*rgx,int *sflags,int *sflags_size,int*max_size);

    struct regex_str* regex_match0(char*str,char*rgx,int*matchnum);
    struct regex_str* regex_match1(char*str,char*rgx,int *mflags,int *mflags_size,int*matchnum);
    struct regex_str* regex_match2(char*str,char*rgx,int *sflags,int *sflags_size,int *mflags,int *mflags_size,int*matchnum); 
    struct regex_str* regex_match3(char*str,char*rgx,int *sflags,int *sflags_size,int*matchnum);
#ifdef __cplusplus
}

struct regex_str* str_matches(std::cmatch cm,int *matchnum);
std::regex_constants::match_flag_type rgx_flag_match(int *mflags,int *mflags_size);
std::regex_constants::syntax_option_type rgx_flag_syntax(int *sflags,int *sflags_size);
#endif
#endif

