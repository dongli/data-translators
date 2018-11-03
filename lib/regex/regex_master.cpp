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
#include "regex_master.h"

//--------------------------------replace ------------------------------------------
    char * regex_replace0(char*str,char *rgx,char *str_replace,int*str_length)//regex replace
    {
        string rx(regex_replace(str,std::regex(rgx),str_replace));
        char *cx=(char*)malloc((sizeof(char)*rx.length()+1));
        *str_length=rx.length();
        strcpy(cx,rx.c_str());
        return cx;
    }
    char * regex_replace1(char*str,char *rgx,char *str_replace,int *mflags,int *mflags_size,int*str_length)//regex replace
    {
        string rx(regex_replace(str,std::regex(rgx),str_replace,rgx_flag_match(mflags,mflags_size)));
        char *cx=(char*)malloc((sizeof(char)*rx.length()+1));
        *str_length=rx.length();
        strcpy(cx,rx.c_str());
        return cx;
    }
    char * regex_replace2(char*str,char *rgx,char *str_replace,int *sflags,int *sflags_size,int *mflags,int *mflags_size,int*str_length)//regex replace
    {
        string rx(regex_replace(str,std::regex(rgx,rgx_flag_syntax(sflags,sflags_size)),str_replace,rgx_flag_match(mflags,mflags_size)));
        char *cx=(char*)malloc((sizeof(char)*rx.length()+1));
        *str_length=rx.length();
        strcpy(cx,rx.c_str());
        return cx;
    }
    char * regex_replace3(char*str,char *rgx,char *str_replace,int *sflags,int *sflags_size,int*str_length)//regex replace all
    {
        string rx(regex_replace(str,std::regex(rgx,rgx_flag_syntax(sflags,sflags_size)),str_replace));
        char *cx=(char*)malloc((sizeof(char)*rx.length()+1));
        *str_length=rx.length();
        strcpy(cx,rx.c_str());
        return cx;
    }
//------------------------------------------logical contains ---------------------------------------------------------------
    bool regex_contains0(char*str,char*rgx)
    {
        std::cmatch cm;
        return regex_search(str,cm,std::regex(rgx));
    }
    bool regex_contains1(char*str,char*rgx,int *mflags,int *mflags_size)
    {
        std::cmatch cm;
        return regex_search(str,cm,std::regex(rgx),rgx_flag_match(mflags,mflags_size));
    }
    bool regex_contains2(char*str,char*rgx,int *sflags,int *sflags_size,int *mflags,int *mflags_size)
    {
        std::cmatch cm;
        return regex_search(str,cm,std::regex(rgx,rgx_flag_syntax(sflags,sflags_size)),rgx_flag_match(mflags,mflags_size));
    }
    bool regex_contains3(char*str,char*rgx,int *sflags,int *sflags_size)
    {
        std::cmatch cm;
        return regex_search(str,cm,std::regex(rgx,rgx_flag_syntax(sflags,sflags_size)));
    }
    
//--------------------------------------------logical match--------------------------------------------------------------------
    bool regex_match_logical0(char*str,char*rgx)
    {
        std::cmatch cm;
        return regex_match(str,cm,std::regex(rgx));
    }
    bool regex_match_logical1(char*str,char*rgx,int *mflags,int *mflags_size)
    {
        std::cmatch cm;
        return regex_match(str,cm,std::regex(rgx),rgx_flag_match(mflags,mflags_size));
    }
    bool regex_match_logical2(char*str,char*rgx,int *sflags,int *sflags_size,int *mflags,int *mflags_size)
    {
        std::cmatch cm;
        return regex_match(str,cm,std::regex(rgx,rgx_flag_syntax(sflags,sflags_size)),rgx_flag_match(mflags,mflags_size));
    }
    bool regex_match_logical3(char*str,char*rgx,int *sflags,int *sflags_size)
    {
        std::cmatch cm;
        return regex_match(str,cm,std::regex(rgx,rgx_flag_syntax(sflags,sflags_size)));
    }
//--------------------------------------------indexof--------------------------------------------------------------------
    int regex_indexof0(char*str,char*rgx)
    {
        std::cmatch cm;
        int last=-1;
        std::regex_search(str, cm, std::regex(rgx));
        if(cm.size()>0) last=cm.position();
        return last;
    }
    int regex_indexof1(char*str,char*rgx,int *mflags,int *mflags_size)
    {
        std::cmatch cm;
        int last=-1;
        std::regex_search(str, cm, std::regex(rgx),rgx_flag_match(mflags,mflags_size));
        if(cm.size()>0) last=cm.position();
        return last;
    }
    int regex_indexof2(char*str,char*rgx,int *sflags,int *sflags_size,int *mflags,int *mflags_size)
    {
        std::cmatch cm;
        int last=-1;
        std::regex_search(str, cm, std::regex(rgx,rgx_flag_syntax(sflags,sflags_size)),rgx_flag_match(mflags,mflags_size));
        if(cm.size()>0) last=cm.position();
        return last;
    }
    int regex_indexof3(char*str,char*rgx,int *sflags,int *sflags_size)
    {
        std::cmatch cm;
        int last=-1;
        std::regex_search(str, cm, std::regex(rgx,rgx_flag_syntax(sflags,sflags_size)));
        if(cm.size()>0) last=cm.position();
        return last;
    }
    //--------------------------------------------lastindexof-------------------------------------------
    int regex_lastindexof0(char*str,char*rgx)
    {
        std::regex e(rgx);
        std::cmatch cm;
        int last=-1;
        for(int index=0;regex_search(&str[index],cm,e);)
        {
            last=index+cm.position();
            index+=cm.position()+cm.length();
        }
        return last;
    }
    int regex_lastindexof1(char*str,char*rgx,int *mflags,int *mflags_size)
    {
        std::regex e(rgx);
        std::cmatch cm;
        int last=-1;
        for(int index=0;regex_search(&str[index],cm,e,rgx_flag_match(mflags,mflags_size));)
        {
            last=index+cm.position();
            index+=cm.position()+cm.length();
        }
        return last;
    }
    int regex_lastindexof2(char*str,char*rgx,int *sflags,int *sflags_size,int *mflags,int *mflags_size)
    {
        std::regex e(rgx,rgx_flag_syntax(sflags,sflags_size));
        std::cmatch cm;
        int last=-1;
        for(int index=0;regex_search(&str[index],cm,e,rgx_flag_match(mflags,mflags_size));)
        {
            last=index+cm.position();
            index+=cm.position()+cm.length();
        }
        return last;
    }
    int regex_lastindexof3(char*str,char*rgx,int *sflags,int *sflags_size)
    {
        std::regex e(rgx,rgx_flag_syntax(sflags,sflags_size));
        std::cmatch cm;
        int last=-1;
        for(int index=0;regex_search(&str[index],cm,e);)
        {
            last=index+cm.position();
            index+=cm.position()+cm.length();
        }
        return last;
    }
    //--------------------------------------------indexofend-------------------------------------------------------------------
    int regex_indexofend0(char*str,char*rgx)
    {
        std::cmatch cm;
        int last=-1;
        std::regex_search(str, cm, std::regex(rgx));
        if(cm.size()>0) last=cm.position()+cm.length()-1;
        return last;
    }
    int regex_indexofend1(char*str,char*rgx,int *mflags,int *mflags_size)
    {
        std::cmatch cm;
        int last=-1;
        std::regex_search(str, cm, std::regex(rgx),rgx_flag_match(mflags,mflags_size));
        if(cm.size()>0) last=cm.position()+cm.length()-1;
        return last;
    }
    int regex_indexofend2(char*str,char*rgx,int *sflags,int *sflags_size,int *mflags,int *mflags_size)
    {
        std::cmatch cm;
        int last=-1;
        std::regex_search(str, cm, std::regex(rgx,rgx_flag_syntax(sflags,sflags_size)),rgx_flag_match(mflags,mflags_size));
        if(cm.size()>0) last=cm.position()+cm.length()-1;
        return last;
    }
    int regex_indexofend3(char*str,char*rgx,int *sflags,int *sflags_size)
    {
        std::cmatch cm;
        int last=-1;
        std::regex_search(str, cm, std::regex(rgx,rgx_flag_syntax(sflags,sflags_size)));
        if(cm.size()>0) last=cm.position()+cm.length()-1;
        return last;
    }
//--------------------------------------------lastindexofend-------------------------------------------
    int regex_lastindexofend0(char*str,char*rgx)
    {
        std::regex e(rgx);
        std::cmatch cm;
        int last=-1;
        for(int index=0;regex_search(&str[index],cm,e);)
        {
            last=index+cm.position()+cm.length()-1;
            index+=cm.position()+cm.length();
        }
        return last;
    }
    int regex_lastindexofend1(char*str,char*rgx,int *mflags,int *mflags_size)
    {
        std::regex e(rgx);
        std::cmatch cm;
        int last=-1;
        for(int index=0;regex_search(&str[index],cm,e,rgx_flag_match(mflags,mflags_size));)
        {
            last=index+cm.position()+cm.length()-1;
            index+=cm.position()+cm.length();
        }
        return last;
    }
    int regex_lastindexofend2(char*str,char*rgx,int *sflags,int *sflags_size,int *mflags,int *mflags_size)
    {
        std::regex e(rgx,rgx_flag_syntax(sflags,sflags_size));
        std::cmatch cm;
        int last=-1;
        for(int index=0;regex_search(&str[index],cm,e,rgx_flag_match(mflags,mflags_size));)
        {
            last=index+cm.position()+cm.length()-1;
            index+=cm.position()+cm.length();
        }
        return last;
    }
    int regex_lastindexofend3(char*str,char*rgx,int *sflags,int *sflags_size)
    {
        std::regex e(rgx,rgx_flag_syntax(sflags,sflags_size));
        std::cmatch cm;
        int last=-1;
        for(int index=0;regex_search(&str[index],cm,e);)
        {
            last=index+cm.position()+cm.length()-1;
            index+=cm.position()+cm.length();
        }
        return last;
    }
//--------------------------------------------------Search--------------------------------------------------------------
    struct regex_container* regex_search0(char*str,char*rgx,int*max_size)
    {
        char var[strlen(str)+1];
        strcpy(var,str);
        std::regex e(rgx);
        std::cmatch cm;
        int size=40;
        struct regex_container *tmp=(struct regex_container*)malloc(sizeof(struct regex_container)*size);
        int x=0;
        for(string::size_type index=0;regex_search(var,cm,e);x++)
        {
            tmp[x].len=cm.size();
            tmp[x].x=(struct regex_data*)malloc(sizeof(struct regex_data)*cm.size());
            for(string::size_type y=0,z=cm.size(); y<z; y++)
            {
                tmp[x].x[y].str=(char*)malloc(sizeof(char)*(cm[y].length()+1));
                strcpy(tmp[x].x[y].str,std::string(cm[y]).c_str());
                tmp[x].x[y].len=cm[y].length();
                tmp[x].x[y].start=index+cm.position(y);
                tmp[x].x[y].end=index+cm.position(y)+cm[y].length()-1;
            }
            strcpy(var,std::string(cm.suffix()).c_str());
            index+=cm.position()+cm.length();
            if(x+1>=size)
            {
                size=size*2;
                tmp=(struct regex_container*)realloc(tmp,sizeof(struct regex_container)*size);
            }
        }
        *max_size=x;
        return tmp;
    }
    struct regex_container* regex_search1(char*str,char*rgx,int *mflags,int *mflags_size,int*max_size)
    {
        char var[strlen(str)+1];
        strcpy(var,str);
        std::regex e(rgx);
        std::cmatch cm; 
        int size=40;
        struct regex_container *tmp=(struct regex_container*)malloc(sizeof(struct regex_container)*size);
        int x=0;
        for(string::size_type index=0;regex_search(var, cm, e,rgx_flag_match(mflags,mflags_size));x++)
        {
            tmp[x].len=cm.size();
            tmp[x].x=(struct regex_data*)malloc(sizeof(struct regex_data)*cm.size());
            for(string::size_type y=0,z=cm.size(); y<z; y++)
            {
                tmp[x].x[y].str=(char*)malloc(sizeof(char)*(cm[y].length()+1));
                strcpy(tmp[x].x[y].str,std::string(cm[y]).c_str());
                tmp[x].x[y].len=cm[y].length();
                tmp[x].x[y].start=index+cm.position(y);
                tmp[x].x[y].end=index+cm.position(y)+cm[y].length()-1;
            }
            strcpy(var,std::string(cm.suffix()).c_str());
            index+=cm.position()+cm.length();
            if(x+1>=size)
            {
                size=size*2;
                tmp=(struct regex_container*)realloc(tmp,sizeof(struct regex_container)*size);
            }
        }
        *max_size=x;
        return tmp;
    }
    struct regex_container* regex_search2(char*str,char*rgx,int *sflags,int *sflags_size,int *mflags,int *mflags_size,int*max_size)
    {
        char var[strlen(str)+1];
        strcpy(var,str);
        std::regex e(rgx,rgx_flag_syntax(sflags,sflags_size));
        std::cmatch cm;
        int size=40;
        struct regex_container *tmp=(struct regex_container*)malloc(sizeof(struct regex_container)*size);
        int x=0;
        for(string::size_type index=0;regex_search(var, cm, e,rgx_flag_match(mflags,mflags_size));x++)
        {
            tmp[x].len=cm.size();
            tmp[x].x=(struct regex_data*)malloc(sizeof(struct regex_data)*cm.size());
            for(string::size_type y=0,z=cm.size(); y<z; y++)
            {
                tmp[x].x[y].str=(char*)malloc(sizeof(char)*(cm[y].length()+1));
                strcpy(tmp[x].x[y].str,std::string(cm[y]).c_str());
                tmp[x].x[y].len=cm[y].length();
                tmp[x].x[y].start=index+cm.position(y);
                tmp[x].x[y].end=index+cm.position(y)+cm[y].length()-1;
            }
            strcpy(var,std::string(cm.suffix()).c_str());
            index+=cm.position()+cm.length();
            if(x+1>=size)
            {
                size=size*2;
                tmp=(struct regex_container*)realloc(tmp,sizeof(struct regex_container)*size);
            }
        }
        *max_size=x;
        return tmp;
    }
    struct regex_container* regex_search3(char*str,char*rgx,int *sflags,int *sflags_size,int*max_size)
    {
        char var[strlen(str)+1];
        strcpy(var,str);
        std::regex e(rgx,rgx_flag_syntax(sflags,sflags_size));
        std::cmatch cm;
        int size=40;
        struct regex_container *tmp=(struct regex_container*)malloc(sizeof(struct regex_container)*size);
        int x=0;
        for(string::size_type index=0;regex_search(var, cm, e);x++)
        {
            tmp[x].len=cm.size();
            tmp[x].x=(struct regex_data*)malloc(sizeof(struct regex_data)*cm.size());
            for(string::size_type y=0,z=cm.size(); y<z; y++)
            {
                tmp[x].x[y].str=(char*)malloc(sizeof(char)*(cm[y].length()+1));
                strcpy(tmp[x].x[y].str,std::string(cm[y]).c_str());
                tmp[x].x[y].len=cm[y].length();
                tmp[x].x[y].start=index+cm.position(y);
                tmp[x].x[y].end=index+cm.position(y)+cm[y].length()-1;
            }
            strcpy(var,std::string(cm.suffix()).c_str());
            index+=cm.position()+cm.length();
            if(x+1>=size)
            {
                size=size*2;
                tmp=(struct regex_container*)realloc(tmp,sizeof(struct regex_container)*size);
            }
        }
        *max_size=x;
        return tmp;
    }

//----------------------------------------------------------------------------------------------------------------
    struct regex_str* regex_match0(char*str,char*rgx,int*matchnum)
    {
        cmatch cm;
        regex_match(str,cm,std::regex(rgx));
        return str_matches(cm,matchnum);
    }
    struct regex_str* regex_match1(char*str,char*rgx,int *mflags,int *mflags_size,int*matchnum)
    {
        cmatch cm;
        regex_match(str,cm,std::regex(rgx),rgx_flag_match(mflags,mflags_size));
        return str_matches(cm,matchnum);
    }
    struct regex_str* regex_match2(char*str,char*rgx,int *sflags,int *sflags_size,int *mflags,int *mflags_size,int*matchnum)
    {
        cmatch cm;
        regex_match(str,cm,std::regex(rgx,rgx_flag_syntax(sflags,sflags_size)),rgx_flag_match(mflags,mflags_size));
        return str_matches(cm,matchnum);
    }
    struct regex_str* regex_match3(char*str,char*rgx,int *sflags,int *sflags_size,int*matchnum) //just syntax flags
    {
        cmatch cm;
        regex_match(str,cm,std::regex(rgx,rgx_flag_syntax(sflags,sflags_size)));
        return str_matches(cm,matchnum);
    }
//----------------------------------------------------------------------------------------------------------------
    struct regex_str* str_matches(std::cmatch cm,int *matchnum)
    {
        *matchnum=cm.size();
        struct regex_str *xs=(struct regex_str*)malloc(sizeof(struct regex_str)*(*matchnum));
        for (unsigned i=0; i<cm.size(); ++i)
        {
            xs[i].str=(char*)malloc(sizeof(char)*cm[i].length());
            xs[i].len=cm[i].length();
            strcpy(xs[i].str,std::string(cm[i]).c_str());
        }
        return xs;
    }
    std::regex_constants::syntax_option_type rgx_flag_syntax(int *sflags,int *sflags_size)
    {
        std::regex_constants::syntax_option_type flags1;
        int d=0;
        for(int x=0;x<*sflags_size;x++){
            if(sflags[x]>=4){
                d=sflags[x];
                break;
            }
        }
        if(d>=4&&d<=9){
            switch(sflags[d])
            {
                case 4:flags1=std::regex_constants::ECMAScript;
                case 5:flags1=std::regex_constants::basic;
                case 6:flags1=std::regex_constants::extended;
                case 7:flags1=std::regex_constants::awk;
                case 8:flags1=std::regex_constants::grep;
                case 9:flags1=std::regex_constants::egrep;
            }
        }
        else{
            flags1=regex_constants::ECMAScript;
        }
        for(int x=0;x<*sflags_size;x++)
        {
            switch(sflags[x])
            {
                case 0:flags1|=std::regex_constants::icase;
                case 1:flags1|=std::regex_constants::nosubs;
                case 2:flags1|=std::regex_constants::optimize;
                case 3:flags1|=std::regex_constants::collate;
                /*case 4:flags1|=regex_constants::ECMAScript;
                case 5:flags1|=regex_constants::basic;
                case 6:flags1|=regex_constants::extended;
                case 7:flags1|=regex_constants::awk;
                case 8:flags1|=regex_constants::grep;
                case 9:flags1|=regex_constants::egrep;*/
            }
        }
        return flags1;
    }
    std::regex_constants::match_flag_type rgx_flag_match(int *mflags,int *mflags_size)
    {
        std::regex_constants::match_flag_type flags2;
        for(int x=0;x<*mflags_size;x++)
        {
            switch(mflags[x])
            {
                case 0:flags2|=std::regex_constants::match_default;
                case 1:flags2|=std::regex_constants::match_not_bol;
                case 2:flags2|=std::regex_constants::match_not_eol;
                case 3:flags2|=std::regex_constants::match_not_bow;
                case 4:flags2|=std::regex_constants::match_not_eow;
                case 5:flags2|=std::regex_constants::match_any;
                case 6:flags2|=std::regex_constants::match_not_null;
                case 7:flags2|=std::regex_constants::match_continuous;
                case 8:flags2|=std::regex_constants::match_prev_avail;
                case 9:flags2|=std::regex_constants::format_default;
                case 10:flags2|=std::regex_constants::format_sed;
                case 11:flags2|=std::regex_constants::format_no_copy;
                case 12:flags2|=std::regex_constants::format_first_only;
            }
        }
        return flags2;
    }