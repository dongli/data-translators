!MIT License

!Copyright (c) 2018 Joshua Oliva

!Permission is hereby granted, free of charge, to any person obtaining a copy
!of this software and associated documentation files (the "Software"), to deal
!in the Software without restriction, including without limitation the rights
!to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
!copies of the Software, and to permit persons to whom the Software is
!furnished to do so, subject to the following conditions:

!The above copyright notice and this permission notice shall be included in all
!copies or substantial portions of the Software.

!THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
!IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
!FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
!AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
!LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
!OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
!SOFTWARE.

module regex
    USE iso_c_binding;
    public::regex_replace,regex_match,regex_match_logical,&
    regex_search,regex_contains,regex_indexof,regex_indexofend,regex_lastindexof,&
    regex_lastindexofend,reg_data,r_matches,r_data,reg_matches,reg_str

    integer,parameter,public::r_match_default=0
    integer,parameter,public::r_match_not_bol=1
    integer,parameter,public::r_match_not_eol=2
    integer,parameter,public::r_match_not_bow=3
    integer,parameter,public::r_match_not_eow=4
    integer,parameter,public::r_match_any=5
    integer,parameter,public::r_match_not_null=6
    integer,parameter,public::r_match_continuous=7
    integer,parameter,public::r_match_prev_avail=8
    integer,parameter,public::r_format_default=9
    integer,parameter,public::r_format_sed=10
    integer,parameter,public::r_format_no_copy=11
    integer,parameter,public::r_format_first_only=12
    
    
    integer,parameter,public::r_icase=0
    integer,parameter,public::r_nosubs=1
    integer,parameter,public::r_optimize=2
    integer,parameter,public::r_collate=3
    integer,parameter,public::r_ECMAScript=4
    integer,parameter,public::r_basic=5
    integer,parameter,public::r_extended=6
    integer,parameter,public::r_awk=7
    integer,parameter,public::r_grep=8
    integer,parameter,public::r_egrep=9 
    private

    type r_data
        type(c_ptr)::str
        integer(kind=c_int)::len 
        integer(kind=c_int)::start 
        integer(kind=c_int)::end 
    end type r_data
    type r_matches
        type(c_ptr)::cptr
        integer(kind=c_int)::len 
    end type r_matches

    type reg_str
        character(len=:),allocatable::str
    end type reg_str

    type reg_data
        character(len=:),allocatable::str
        integer::start,end
    end type reg_data
    type reg_matches
        type(reg_data),allocatable::match(:)
    end type reg_matches
interface 
        subroutine C_free(ptr) bind(C,name="free") 
            use iso_c_binding,only:c_ptr
            type(c_ptr), value, intent(in) :: ptr
        end subroutine C_free

        function rgx_replace0(str,rx,str_replace,xs) bind(c,name="regex_replace0")
            use iso_c_binding,only:c_ptr,c_char,c_int
            character(len=1,kind=c_char),intent(in)::str(*),rx(*),str_replace(*)
            integer(kind=c_int),intent(inout)::xs
            type(c_ptr)::rgx_replace0
        end function rgx_replace0

        function rgx_replace1(str,rx,str_replace,mflags,mflags_size,xs) bind(c,name="regex_replace1")
            use iso_c_binding,only:c_ptr,c_char,c_int
            character(len=1,kind=c_char),intent(in)::str(*),rx(*),str_replace(*)
            integer(kind=c_int),intent(in)::mflags(*),mflags_size
            integer(kind=c_int),intent(inout)::xs
            type(c_ptr)::rgx_replace1
        end function rgx_replace1

        function rgx_replace2(str,rx,str_replace,sflags,sflags_size,mflags,mflags_size,xs) bind(c,name="regex_replace2")
            use iso_c_binding,only:c_ptr,c_char,c_int
            character(len=1,kind=c_char),intent(in)::str(*),rx(*),str_replace(*)
            integer(kind=c_int),intent(in)::sflags(*),sflags_size,mflags(*),mflags_size
            integer(kind=c_int),intent(inout)::xs
            type(c_ptr)::rgx_replace2
        end function rgx_replace2

        function rgx_replace3(str,rx,str_replace,sflags,sflags_size,xs) bind(c,name="regex_replace3")
            use iso_c_binding,only:c_ptr,c_char,c_int
            character(len=1,kind=c_char),intent(in)::str(*),rx(*),str_replace(*)
            integer(kind=c_int),intent(in)::sflags(*),sflags_size
            integer(kind=c_int),intent(inout)::xs
            type(c_ptr)::rgx_replace3
        end function rgx_replace3

        function rgx_match_logical0(str,rgx) bind(c,name="regex_match_logical0")
            use iso_c_binding,only:c_char,c_int,c_bool
            character(len=1,kind=c_char),intent(in)::str(*),rgx(*)
            logical(kind=c_bool)::rgx_match_logical0
        end function rgx_match_logical0

        function rgx_match_logical1(str,rgx,mflags,mflags_size) bind(c,name="regex_match_logical1")
            use iso_c_binding,only:c_char,c_int,c_bool
            character(len=1,kind=c_char),intent(in)::str(*),rgx(*)
            integer(kind=c_int),intent(in)::mflags(*),mflags_size
            logical(kind=c_bool)::rgx_match_logical1
        end function rgx_match_logical1

        function rgx_match_logical2(str,rgx,sflags,sflags_size,mflags,mflags_size) bind(c,name="regex_match_logical2")
            use iso_c_binding,only:c_char,c_int,c_bool
            character(len=1,kind=c_char),intent(in)::str(*),rgx(*)
            integer(kind=c_int),intent(in)::sflags(*),sflags_size,mflags(*),mflags_size
            logical(kind=c_bool)::rgx_match_logical2
        end function rgx_match_logical2

        function rgx_match_logical3(str,rgx,sflags,sflags_size) bind(c,name="regex_match_logical3")
            use iso_c_binding,only:c_char,c_int,c_bool
            character(len=1,kind=c_char),intent(in)::str(*),rgx(*)
            integer(kind=c_int),intent(in)::sflags(*),sflags_size
            logical(kind=c_bool)::rgx_match_logical3
        end function rgx_match_logical3

        function rgx_contains0(str,rgx) bind(c,name="regex_contains0")
            use iso_c_binding,only:c_char,c_int,c_bool
            character(len=1,kind=c_char),intent(in)::str(*),rgx(*)
            logical(kind=c_bool)::rgx_contains0
        end function rgx_contains0

        function rgx_contains1(str,rgx,mflags,mflags_size) bind(c,name="regex_contains1")
            use iso_c_binding,only:c_char,c_int,c_bool
            character(len=1,kind=c_char),intent(in)::str(*),rgx(*)
            integer(kind=c_int),intent(in)::mflags(*),mflags_size
            logical(kind=c_bool)::rgx_contains1
        end function rgx_contains1

        function rgx_contains2(str,rgx,sflags,sflags_size,mflags,mflags_size) bind(c,name="regex_contains2")
            use iso_c_binding,only:c_char,c_int,c_bool
            character(len=1,kind=c_char),intent(in)::str(*),rgx(*)
            integer(kind=c_int),intent(in)::sflags(*),sflags_size,mflags(*),mflags_size
            logical(kind=c_bool)::rgx_contains2
        end function rgx_contains2

        function rgx_contains3(str,rgx,sflags,sflags_size) bind(c,name="regex_contains3")
            use iso_c_binding,only:c_char,c_int,c_bool
            character(len=1,kind=c_char),intent(in)::str(*),rgx(*)
            integer(kind=c_int),intent(in)::sflags(*),sflags_size
            logical(kind=c_bool)::rgx_contains3
        end function rgx_contains3

        function rgx_indexof0(str,rgx) bind(c,name="regex_indexof0")
            use iso_c_binding,only:c_char,c_int
            character(len=1,kind=c_char),intent(in)::str(*),rgx(*)
            integer(kind=c_int)::rgx_indexof0
        end function rgx_indexof0

        function rgx_indexof1(str,rgx,mflags,mflags_size) bind(c,name="regex_indexof1")
            use iso_c_binding,only:c_char,c_int
            character(len=1,kind=c_char),intent(in)::str(*),rgx(*)
            integer(kind=c_int),intent(in)::mflags(*),mflags_size
            integer(kind=c_int)::rgx_indexof1
        end function rgx_indexof1

        function rgx_indexof2(str,rgx,sflags,sflags_size,mflags,mflags_size) bind(c,name="regex_indexof2")
            use iso_c_binding,only:c_char,c_int
            character(len=1,kind=c_char),intent(in)::str(*),rgx(*)
            integer(kind=c_int),intent(in)::sflags(*),sflags_size,mflags(*),mflags_size
            integer(kind=c_int)::rgx_indexof2
        end function rgx_indexof2

        function rgx_indexof3(str,rgx,sflags,sflags_size) bind(c,name="regex_indexof3")
            use iso_c_binding,only:c_char,c_int
            character(len=1,kind=c_char),intent(in)::str(*),rgx(*)
            integer(kind=c_int),intent(in)::sflags(*),sflags_size
            integer(kind=c_int)::rgx_indexof3
        end function rgx_indexof3

        function rgx_indexofend0(str,rgx) bind(c,name="regex_indexofend0")
            use iso_c_binding,only:c_char,c_int
            character(len=1,kind=c_char),intent(in)::str(*),rgx(*)
            integer(kind=c_int)::rgx_indexofend0
        end function rgx_indexofend0

        function rgx_indexofend1(str,rgx,mflags,mflags_size) bind(c,name="regex_indexofend1")
            use iso_c_binding,only:c_char,c_int
            character(len=1,kind=c_char),intent(in)::str(*),rgx(*)
            integer(kind=c_int),intent(in)::mflags(*),mflags_size
            integer(kind=c_int)::rgx_indexofend1
        end function rgx_indexofend1

        function rgx_indexofend2(str,rgx,sflags,sflags_size,mflags,mflags_size) bind(c,name="regex_indexofend2")
            use iso_c_binding,only:c_char,c_int
            character(len=1,kind=c_char),intent(in)::str(*),rgx(*)
            integer(kind=c_int),intent(in)::sflags(*),sflags_size,mflags(*),mflags_size
            integer(kind=c_int)::rgx_indexofend2
        end function rgx_indexofend2

        function rgx_indexofend3(str,rgx,sflags,sflags_size) bind(c,name="regex_indexofend3")
            use iso_c_binding,only:c_char,c_int
            character(len=1,kind=c_char),intent(in)::str(*),rgx(*)
            integer(kind=c_int),intent(in)::sflags(*),sflags_size
            integer(kind=c_int)::rgx_indexofend3
        end function rgx_indexofend3

        function rgx_lastindexof0(str,rgx) bind(c,name="regex_lastindexof0")
            use iso_c_binding,only:c_char,c_int
            character(len=1,kind=c_char),intent(in)::str(*),rgx(*)
            integer(kind=c_int)::rgx_lastindexof0
        end function rgx_lastindexof0

        function rgx_lastindexof1(str,rgx,mflags,mflags_size) bind(c,name="regex_lastindexof1")
            use iso_c_binding,only:c_char,c_int
            character(len=1,kind=c_char),intent(in)::str(*),rgx(*)
            integer(kind=c_int),intent(in)::mflags(*),mflags_size
            integer(kind=c_int)::rgx_lastindexof1
        end function rgx_lastindexof1

        function rgx_lastindexof2(str,rgx,sflags,sflags_size,mflags,mflags_size) bind(c,name="regex_lastindexof2")
            use iso_c_binding,only:c_char,c_int
            character(len=1,kind=c_char),intent(in)::str(*),rgx(*)
            integer(kind=c_int),intent(in)::sflags(*),sflags_size,mflags(*),mflags_size
            integer(kind=c_int)::rgx_lastindexof2
        end function rgx_lastindexof2

        function rgx_lastindexof3(str,rgx,sflags,sflags_size) bind(c,name="regex_lastindexof3")
            use iso_c_binding,only:c_char,c_int
            character(len=1,kind=c_char),intent(in)::str(*),rgx(*)
            integer(kind=c_int),intent(in)::sflags(*),sflags_size
            integer(kind=c_int)::rgx_lastindexof3
        end function rgx_lastindexof3

        function rgx_lastindexofend0(str,rgx) bind(c,name="regex_lastindexofend0")
            use iso_c_binding,only:c_char,c_int
            character(len=1,kind=c_char),intent(in)::str(*),rgx(*)
            integer(kind=c_int)::rgx_lastindexofend0
        end function rgx_lastindexofend0

        function rgx_lastindexofend1(str,rgx,mflags,mflags_size) bind(c,name="regex_lastindexofend1")
            use iso_c_binding,only:c_char,c_int
            character(len=1,kind=c_char),intent(in)::str(*),rgx(*)
            integer(kind=c_int),intent(in)::mflags(*),mflags_size
            integer(kind=c_int)::rgx_lastindexofend1
        end function rgx_lastindexofend1

        function rgx_lastindexofend2(str,rgx,sflags,sflags_size,mflags,mflags_size) bind(c,name="regex_lastindexofend2")
            use iso_c_binding,only:c_char,c_int
            character(len=1,kind=c_char),intent(in)::str(*),rgx(*)
            integer(kind=c_int),intent(in)::sflags(*),sflags_size,mflags(*),mflags_size
            integer(kind=c_int)::rgx_lastindexofend2
        end function rgx_lastindexofend2

        function rgx_lastindexofend3(str,rgx,sflags,sflags_size) bind(c,name="regex_lastindexofend3")
            use iso_c_binding,only:c_char,c_int
            character(len=1,kind=c_char),intent(in)::str(*),rgx(*)
            integer(kind=c_int),intent(in)::sflags(*),sflags_size
            integer(kind=c_int)::rgx_lastindexofend3
        end function rgx_lastindexofend3

        function rgx_search0(str,rgx,matchnum) bind(c,name="regex_search0")
            use iso_c_binding,only:c_ptr,c_char,c_int
            character(len=1,kind=c_char),intent(in)::str(*),rgx(*)
            integer(kind=c_int),intent(inout)::matchnum
            type(c_ptr)::rgx_search0
        end function rgx_search0

        function rgx_search1(str,rgx,mflags,mflags_size,matchnum) bind(c,name="regex_search1")
            use iso_c_binding,only:c_ptr,c_char,c_int
            character(len=1,kind=c_char),intent(in)::str(*),rgx(*)
            integer(kind=c_int),intent(in)::mflags(*),mflags_size
            integer(kind=c_int),intent(inout)::matchnum
            type(c_ptr)::rgx_search1
        end function rgx_search1

        function rgx_search2(str,rgx,sflags,sflags_size,mflags,mflags_size,matchnum) bind(c,name="regex_search2")
            use iso_c_binding,only:c_ptr,c_char,c_int
            character(len=1,kind=c_char),intent(in)::str(*),rgx(*)
            integer(kind=c_int),intent(in)::sflags(*),sflags_size,mflags(*),mflags_size
            integer(kind=c_int),intent(inout)::matchnum
            type(c_ptr)::rgx_search2
        end function rgx_search2

        function rgx_search3(str,rgx,sflags,sflags_size,matchnum) bind(c,name="regex_search3")
            use iso_c_binding,only:c_ptr,c_char,c_int
            character(len=1,kind=c_char),intent(in)::str(*),rgx(*)
            integer(kind=c_int),intent(in)::sflags(*),sflags_size
            integer(kind=c_int),intent(inout)::matchnum
            type(c_ptr)::rgx_search3
        end function rgx_search3

        function rgx_match0(str,rgx,matchnum) bind(c,name="regex_match0")
            use iso_c_binding,only:c_ptr,c_char,c_int
            character(len=1,kind=c_char),intent(in)::str(*),rgx(*)
            integer(kind=c_int),intent(inout)::matchnum
            type(c_ptr)::rgx_match0
        end function rgx_match0
        function rgx_match1(str,rgx,mflags,mflags_size,matchnum) bind(c,name="regex_match1")
            use iso_c_binding,only:c_ptr,c_char,c_int
            character(len=1,kind=c_char),intent(in)::str(*),rgx(*)
            integer(kind=c_int)::mflags(*),mflags_size
            integer(kind=c_int),intent(inout)::matchnum
            type(c_ptr)::rgx_match1
        end function rgx_match1
        function rgx_match2(str,rgx,sflags,sflags_size,mflags,mflags_size,matchnum) bind(c,name="regex_match2")
            use iso_c_binding,only:c_ptr,c_char,c_int
            character(len=1,kind=c_char),intent(in)::str(*),rgx(*)
            integer(kind=c_int)::sflags(*),sflags_size,mflags(*),mflags_size
            integer(kind=c_int),intent(inout)::matchnum
            type(c_ptr)::rgx_match2
        end function rgx_match2
        function rgx_match3(str,rgx,sflags,sflags_size,matchnum) bind(c,name="regex_match3")
            use iso_c_binding,only:c_ptr,c_char,c_int
            character(len=1,kind=c_char),intent(in)::str(*),rgx(*)
            integer(kind=c_int)::sflags(*),sflags_size
            integer(kind=c_int),intent(inout)::matchnum
            type(c_ptr)::rgx_match3
        end function rgx_match3
        
    end interface 

    contains 
    logical function regex_match_logical(str,rx,sflags,mflags) result(x)
        character(len=*),intent(in)::str,rx
        integer,optional::sflags(:),mflags(:)
        if(present(sflags).and.present(mflags)) then
            x=regex_match_logical2(str,rx,sflags,mflags)
        else if(present(mflags)) then
            x=regex_match_logical1(str,rx,mflags)
        else if(present(sflags)) then
            x=regex_match_logical3(str,rx,sflags)
        else
            x=regex_match_logical0(str,rx)
        end if
    end function regex_match_logical

    logical function regex_match_logical0(str,regex) result(x) 
        character(len=*),intent(in)::str,regex
        character(len=1,kind=c_char),allocatable::strx(:),rgx(:)
        call fstring_to_cstring(str,strx) 
        call fstring_to_cstring(regex,rgx)
        x=rgx_match_logical0(strx,rgx)
    end function regex_match_logical0

    logical function regex_match_logical1(str,regex,mflags) result(x) 
        character(len=*),intent(in)::str,regex
        character(len=1,kind=c_char),allocatable::strx(:),rgx(:)
        integer(kind=c_int),intent(in)::mflags(:)
        call fstring_to_cstring(str,strx) 
        call fstring_to_cstring(regex,rgx)
        x=rgx_match_logical1(strx,rgx,mflags,size(mflags))
    end function regex_match_logical1

    logical function regex_match_logical2(str,regex,sflags,mflags) result(x) 
        character(len=*),intent(in)::str,regex
        character(len=1,kind=c_char),allocatable::strx(:),rgx(:)
        integer(kind=c_int),intent(in)::mflags(:),sflags(:)
        call fstring_to_cstring(str,strx) 
        call fstring_to_cstring(regex,rgx)
        x=rgx_match_logical2(strx,rgx,sflags,size(sflags),mflags,size(mflags))
    end function regex_match_logical2

    logical function regex_match_logical3(str,regex,sflags) result(x) 
        character(len=*),intent(in)::str,regex
        character(len=1,kind=c_char),allocatable::strx(:),rgx(:)
        integer(kind=c_int),intent(in)::sflags(:)
        call fstring_to_cstring(str,strx) 
        call fstring_to_cstring(regex,rgx)
        x=rgx_match_logical3(strx,rgx,sflags,size(sflags))
    end function regex_match_logical3

    logical function regex_contains(str,rx,sflags,mflags) result(x)
        character(len=*),intent(in)::str,rx
        integer,optional::sflags(:),mflags(:)
        if(present(sflags).and.present(mflags)) then
            x=regex_match_contains2(str,rx,sflags,mflags)
        else if(present(mflags)) then
            x=regex_match_contains1(str,rx,mflags)
        else if(present(sflags)) then
            x=regex_match_contains3(str,rx,sflags)
        else
            x=regex_match_contains0(str,rx)
        end if
    end function regex_contains
    logical function regex_match_contains0(str,regex) result(x) 
        character(len=*),intent(in)::str,regex
        character(len=1,kind=c_char),allocatable::strx(:),rgx(:)
        call fstring_to_cstring(str,strx) 
        call fstring_to_cstring(regex,rgx)
        x=rgx_contains0(strx,rgx)
    end function regex_match_contains0

    logical function regex_match_contains1(str,regex,mflags) result(x) 
        character(len=*),intent(in)::str,regex
        character(len=1,kind=c_char),allocatable::strx(:),rgx(:)
        integer(kind=c_int),intent(in)::mflags(:)
        call fstring_to_cstring(str,strx) 
        call fstring_to_cstring(regex,rgx)
        x=rgx_contains1(strx,rgx,mflags,size(mflags))
    end function regex_match_contains1

    logical function regex_match_contains2(str,regex,sflags,mflags) result(x) 
        character(len=*),intent(in)::str,regex
        character(len=1,kind=c_char),allocatable::strx(:),rgx(:)
        integer(kind=c_int),intent(in)::mflags(:),sflags(:)
        call fstring_to_cstring(str,strx) 
        call fstring_to_cstring(regex,rgx)
        x=rgx_contains2(strx,rgx,sflags,size(sflags),mflags,size(mflags))
    end function regex_match_contains2

    logical function regex_match_contains3(str,regex,sflags) result(x) 
        character(len=*),intent(in)::str,regex
        character(len=1,kind=c_char),allocatable::strx(:),rgx(:)
        integer(kind=c_int),intent(in)::sflags(:)
        call fstring_to_cstring(str,strx) 
        call fstring_to_cstring(regex,rgx)
        x=rgx_contains3(strx,rgx,sflags,size(sflags))
    end function regex_match_contains3

    integer function regex_indexof(str,rx,sflags,mflags) result(x)
        character(len=*),intent(in)::str,rx
        integer,optional::sflags(:),mflags(:)
        if(present(sflags).and.present(mflags)) then
            x=regex_indexof2(str,rx,sflags,mflags)
        else if(present(mflags)) then
            x=regex_indexof1(str,rx,mflags)
        else if(present(sflags)) then
            x=regex_indexof3(str,rx,sflags)
        else
            x=regex_indexof0(str,rx)
        end if
        if(x>=0) x=x+1
    end function regex_indexof
    integer function regex_indexof0(str,regex) result(x) 
        character(len=*),intent(in)::str,regex
        character(len=1,kind=c_char),allocatable::strx(:),rgx(:)
        call fstring_to_cstring(str,strx) 
        call fstring_to_cstring(regex,rgx)
        x=rgx_indexof0(strx,rgx)
    end function regex_indexof0

    integer function regex_indexof1(str,regex,mflags) result(x) 
        character(len=*),intent(in)::str,regex
        character(len=1,kind=c_char),allocatable::strx(:),rgx(:)
        integer(kind=c_int),intent(in)::mflags(:)
        call fstring_to_cstring(str,strx) 
        call fstring_to_cstring(regex,rgx)
        x=rgx_indexof1(strx,rgx,mflags,size(mflags))
    end function regex_indexof1

    integer function regex_indexof2(str,regex,sflags,mflags) result(x) 
        character(len=*),intent(in)::str,regex
        character(len=1,kind=c_char),allocatable::strx(:),rgx(:)
        integer(kind=c_int),intent(in)::mflags(:),sflags(:)
        call fstring_to_cstring(str,strx) 
        call fstring_to_cstring(regex,rgx)
        x=rgx_indexof2(strx,rgx,sflags,size(sflags),mflags,size(mflags))
    end function regex_indexof2

    integer function regex_indexof3(str,regex,sflags) result(x) 
        character(len=*),intent(in)::str,regex
        character(len=1,kind=c_char),allocatable::strx(:),rgx(:)
        integer(kind=c_int),intent(in)::sflags(:)
        call fstring_to_cstring(str,strx) 
        call fstring_to_cstring(regex,rgx)
        x=rgx_indexof3(strx,rgx,sflags,size(sflags))
    end function regex_indexof3

    integer function regex_lastindexof(str,rx,sflags,mflags) result(x)
        character(len=*),intent(in)::str,rx
        integer,optional::sflags(:),mflags(:)
        if(present(sflags).and.present(mflags)) then
            x=regex_lastindexof2(str,rx,sflags,mflags)
        else if(present(mflags)) then
            x=regex_lastindexof1(str,rx,mflags)
        else if(present(sflags)) then
            x=regex_lastindexof3(str,rx,sflags)
        else
            x=regex_lastindexof0(str,rx)
        end if
        if(x>=0) x=x+1
    end function regex_lastindexof
    integer function regex_lastindexof0(str,regex) result(x) 
        character(len=*),intent(in)::str,regex
        character(len=1,kind=c_char),allocatable::strx(:),rgx(:)
        call fstring_to_cstring(str,strx) 
        call fstring_to_cstring(regex,rgx)
        x=rgx_lastindexof0(strx,rgx)
    end function regex_lastindexof0

    integer function regex_lastindexof1(str,regex,mflags) result(x) 
        character(len=*),intent(in)::str,regex
        character(len=1,kind=c_char),allocatable::strx(:),rgx(:)
        integer(kind=c_int),intent(in)::mflags(:)
        call fstring_to_cstring(str,strx) 
        call fstring_to_cstring(regex,rgx)
        x=rgx_lastindexof1(strx,rgx,mflags,size(mflags))
    end function regex_lastindexof1

    integer function regex_lastindexof2(str,regex,sflags,mflags) result(x) 
        character(len=*),intent(in)::str,regex
        character(len=1,kind=c_char),allocatable::strx(:),rgx(:)
        integer(kind=c_int),intent(in)::mflags(:),sflags(:)
        call fstring_to_cstring(str,strx) 
        call fstring_to_cstring(regex,rgx)
        x=rgx_lastindexof2(strx,rgx,sflags,size(sflags),mflags,size(mflags))
    end function regex_lastindexof2

    integer function regex_lastindexof3(str,regex,sflags) result(x) 
        character(len=*),intent(in)::str,regex
        character(len=1,kind=c_char),allocatable::strx(:),rgx(:)
        integer(kind=c_int),intent(in)::sflags(:)
        call fstring_to_cstring(str,strx) 
        call fstring_to_cstring(regex,rgx)
        x=rgx_lastindexof3(strx,rgx,sflags,size(sflags))
    end function regex_lastindexof3

    integer function regex_indexofend(str,rx,sflags,mflags) result(x)
        character(len=*),intent(in)::str,rx
        integer,optional::sflags(:),mflags(:)
        if(present(sflags).and.present(mflags)) then
            x=regex_indexofend2(str,rx,sflags,mflags)
        else if(present(mflags)) then
            x=regex_indexofend1(str,rx,mflags)
        else if(present(sflags)) then
            x=regex_indexofend3(str,rx,sflags)
        else
            x=regex_indexofend0(str,rx)
        end if
        if(x>=0) x=x+1
    end function regex_indexofend
    integer function regex_indexofend0(str,regex) result(x) 
        character(len=*),intent(in)::str,regex
        character(len=1,kind=c_char),allocatable::strx(:),rgx(:)
        call fstring_to_cstring(str,strx) 
        call fstring_to_cstring(regex,rgx)
        x=rgx_indexofend0(strx,rgx)
    end function regex_indexofend0

    integer function regex_indexofend1(str,regex,mflags) result(x) 
        character(len=*),intent(in)::str,regex
        character(len=1,kind=c_char),allocatable::strx(:),rgx(:)
        integer(kind=c_int),intent(in)::mflags(:)
        call fstring_to_cstring(str,strx) 
        call fstring_to_cstring(regex,rgx)
        x=rgx_indexofend1(strx,rgx,mflags,size(mflags))
    end function regex_indexofend1

    integer function regex_indexofend2(str,regex,sflags,mflags) result(x) 
        character(len=*),intent(in)::str,regex
        character(len=1,kind=c_char),allocatable::strx(:),rgx(:)
        integer(kind=c_int),intent(in)::mflags(:),sflags(:)
        call fstring_to_cstring(str,strx) 
        call fstring_to_cstring(regex,rgx)
        x=rgx_indexofend2(strx,rgx,sflags,size(sflags),mflags,size(mflags))
    end function regex_indexofend2

    integer function regex_indexofend3(str,regex,sflags) result(x) 
        character(len=*),intent(in)::str,regex
        character(len=1,kind=c_char),allocatable::strx(:),rgx(:)
        integer(kind=c_int),intent(in)::sflags(:)
        call fstring_to_cstring(str,strx) 
        call fstring_to_cstring(regex,rgx)
        x=rgx_indexofend3(strx,rgx,sflags,size(sflags))
    end function regex_indexofend3

    integer function regex_lastindexofend(str,rx,sflags,mflags) result(x)
        character(len=*),intent(in)::str,rx
        integer,optional::sflags(:),mflags(:)
        if(present(sflags).and.present(mflags)) then
            x=regex_lastindexofend2(str,rx,sflags,mflags)
        else if(present(mflags)) then
            x=regex_lastindexofend1(str,rx,mflags)
        else if(present(sflags)) then
            x=regex_lastindexofend3(str,rx,sflags)
        else
            x=regex_lastindexofend0(str,rx)
        end if
        if(x>=0) x=x+1
    end function regex_lastindexofend
    integer function regex_lastindexofend0(str,regex) result(x) 
        character(len=*),intent(in)::str,regex
        character(len=1,kind=c_char),allocatable::strx(:),rgx(:)
        call fstring_to_cstring(str,strx) 
        call fstring_to_cstring(regex,rgx)
        x=rgx_lastindexofend0(strx,rgx)
    end function regex_lastindexofend0

    integer function regex_lastindexofend1(str,regex,mflags) result(x) 
        character(len=*),intent(in)::str,regex
        character(len=1,kind=c_char),allocatable::strx(:),rgx(:)
        integer(kind=c_int),intent(in)::mflags(:)
        call fstring_to_cstring(str,strx) 
        call fstring_to_cstring(regex,rgx)
        x=rgx_lastindexofend1(strx,rgx,mflags,size(mflags))
    end function regex_lastindexofend1

    integer function regex_lastindexofend2(str,regex,sflags,mflags) result(x) 
        character(len=*),intent(in)::str,regex
        character(len=1,kind=c_char),allocatable::strx(:),rgx(:)
        integer(kind=c_int),intent(in)::mflags(:),sflags(:)
        call fstring_to_cstring(str,strx) 
        call fstring_to_cstring(regex,rgx)
        x=rgx_lastindexofend2(strx,rgx,sflags,size(sflags),mflags,size(mflags))
    end function regex_lastindexofend2

    integer function regex_lastindexofend3(str,regex,sflags) result(x) 
        character(len=*),intent(in)::str,regex
        character(len=1,kind=c_char),allocatable::strx(:),rgx(:)
        integer(kind=c_int),intent(in)::sflags(:)
        call fstring_to_cstring(str,strx) 
        call fstring_to_cstring(regex,rgx)
        x=rgx_lastindexofend3(strx,rgx,sflags,size(sflags))
    end function regex_lastindexofend3

    function regex_search(str,rx,sflags,mflags) result(x)
        character(len=*),intent(in)::str,rx
        integer,optional::sflags(:),mflags(:)
        type(reg_matches),allocatable::x(:)
        if(present(sflags).and.present(mflags)) then
            x=regex_search2(str,rx,sflags,mflags)
        else if(present(mflags)) then
            x=regex_search1(str,rx,mflags)
        else if(present(sflags)) then
            x=regex_search3(str,rx,sflags)
        else
            x=regex_search0(str,rx)
        end if
    end function regex_search

    function regex_search0(str,regex) result(match_data)  
        character(len=*),intent(in)::str,regex
        character(len=1,kind=c_char),allocatable::strx(:),rgx(:)
        integer(kind=c_int)::struct_amt
        type(r_matches),pointer::fptr(:)
        type(r_data),pointer::str_data(:)
        type(c_ptr)::cptr
        integer :: i,i2
        character(kind=c_char,len=1),pointer::z(:)
        type(reg_matches),allocatable::match_data(:)
        call fstring_to_cstring(str,strx) 
        call fstring_to_cstring(regex,rgx)
        cptr=rgx_search0(strx,rgx,struct_amt)
        allocate(match_data(struct_amt))
        if(struct_amt>0) then
            call c_f_pointer(cptr, fptr,[struct_amt])
            do i=1,struct_amt
                allocate(match_data(i)%match(fptr(i)%len))
                call c_f_pointer(fptr(i)%cptr, str_data,[fptr(i)%len])
                do i2=1,fptr(i)%len
                    call c_f_pointer(str_data(i2)%str, z,[str_data(i2)%len])
                    call cstring_to_fstring(z,match_data(i)%match(i2)%str,str_data(i2)%len)
                    match_data(i)%match(i2)%start=str_data(i2)%start+1
                    match_data(i)%match(i2)%end=str_data(i2)%end+1
                    call C_free(str_data(i2)%str)
                    nullify(z)
                end do
                call C_free(fptr(i)%cptr)
                nullify(str_data)
            end do
            nullify(fptr)
        end if
      call C_free(cptr)
    end function regex_search0

    function regex_search1(str,regex,mflags) result(match_data)  
        character(len=*),intent(in)::str,regex
        integer,intent(in)::mflags(:)
        character(len=1,kind=c_char),allocatable::strx(:),rgx(:)
        integer(kind=c_int)::struct_amt
        type(r_matches),pointer::fptr(:)
        type(r_data),pointer::str_data(:)
        type(c_ptr)::cptr
        integer :: i,i2
        character(kind=c_char,len=1),pointer::z(:)
        type(reg_matches),allocatable::match_data(:)
        call fstring_to_cstring(str,strx) 
        call fstring_to_cstring(regex,rgx)
        cptr=rgx_search1(strx,rgx,mflags,size(mflags),struct_amt)
        allocate(match_data(struct_amt))
        if(struct_amt>0) then
            call c_f_pointer(cptr, fptr,[struct_amt])
            do i=1,struct_amt
                allocate(match_data(i)%match(fptr(i)%len))
                call c_f_pointer(fptr(i)%cptr, str_data,[fptr(i)%len])
                do i2=1,fptr(i)%len
                    call c_f_pointer(str_data(i2)%str, z,[str_data(i2)%len])
                    call cstring_to_fstring(z,match_data(i)%match(i2)%str,str_data(i2)%len)
                    match_data(i)%match(i2)%start=str_data(i2)%start+1
                    match_data(i)%match(i2)%end=str_data(i2)%end+1
                    call C_free(str_data(i2)%str)
                    nullify(z)
                end do
                call C_free(fptr(i)%cptr)
                nullify(str_data)
            end do
            nullify(fptr)
        end if
      call C_free(cptr)
    end function regex_search1

    function regex_search2(str,regex,sflags,mflags) result(match_data)  
        character(len=*),intent(in)::str,regex
        integer,intent(in)::sflags(:),mflags(:)
        character(len=1,kind=c_char),allocatable::strx(:),rgx(:)        
        integer(kind=c_int)::struct_amt
        type(r_matches),pointer::fptr(:)
        type(r_data),pointer::str_data(:)
        type(c_ptr)::cptr
        integer :: i,i2
        character(kind=c_char,len=1),pointer::z(:)
        type(reg_matches),allocatable::match_data(:)
        call fstring_to_cstring(str,strx) 
        call fstring_to_cstring(regex,rgx)
        cptr=rgx_search2(strx,rgx,sflags,size(sflags),mflags,size(mflags),struct_amt)
        allocate(match_data(struct_amt))
        if(struct_amt>0) then
            call c_f_pointer(cptr, fptr,[struct_amt])
            do i=1,struct_amt
                allocate(match_data(i)%match(fptr(i)%len))
                call c_f_pointer(fptr(i)%cptr, str_data,[fptr(i)%len])
                do i2=1,fptr(i)%len
                    call c_f_pointer(str_data(i2)%str, z,[str_data(i2)%len])
                    call cstring_to_fstring(z,match_data(i)%match(i2)%str,str_data(i2)%len)
                    match_data(i)%match(i2)%start=str_data(i2)%start+1
                    match_data(i)%match(i2)%end=str_data(i2)%end+1
                    call C_free(str_data(i2)%str)
                    nullify(z)
                end do
                call C_free(fptr(i)%cptr)
                nullify(str_data)
            end do
            nullify(fptr)
        end if
      call C_free(cptr)
    end function regex_search2

    function regex_search3(str,regex,sflags) result(match_data)  
        character(len=*),intent(in)::str,regex
        integer,intent(in)::sflags(:)
        character(len=1,kind=c_char),allocatable::strx(:),rgx(:)
        integer(kind=c_int)::struct_amt
        type(r_matches),pointer::fptr(:)
        type(r_data),pointer::str_data(:)
        type(c_ptr)::cptr
        integer :: i,i2
        character(kind=c_char,len=1),pointer::z(:)
        type(reg_matches),allocatable::match_data(:)
        call fstring_to_cstring(str,strx) 
        call fstring_to_cstring(regex,rgx)
        cptr=rgx_search3(strx,rgx,sflags,size(sflags),struct_amt)
        allocate(match_data(struct_amt))
        if(struct_amt>0) then
            call c_f_pointer(cptr, fptr,[struct_amt])
            do i=1,struct_amt
                allocate(match_data(i)%match(fptr(i)%len))
                call c_f_pointer(fptr(i)%cptr, str_data,[fptr(i)%len])
                do i2=1,fptr(i)%len
                    call c_f_pointer(str_data(i2)%str, z,[str_data(i2)%len])
                    call cstring_to_fstring(z,match_data(i)%match(i2)%str,str_data(i2)%len)
                    match_data(i)%match(i2)%start=str_data(i2)%start+1
                    match_data(i)%match(i2)%end=str_data(i2)%end+1
                    call C_free(str_data(i2)%str)
                    nullify(z)
                end do
                call C_free(fptr(i)%cptr)
                nullify(str_data)
            end do
            nullify(fptr)
        end if
      call C_free(cptr)
    end function regex_search3

    function regex_match(str,rx,sflags,mflags) result(x)
        character(len=*),intent(in)::str,rx
        integer,optional::sflags(:),mflags(:)
        type(reg_str),allocatable::x(:)
        !character(len=:),allocatable::x(:)
        if(present(sflags).and.present(mflags)) then
            x=regex_match2(str,rx,sflags,mflags)
        else if(present(mflags)) then
            x=regex_match1(str,rx,mflags)
        else if(present(sflags)) then
            x=regex_match3(str,rx,sflags)
        else
            x=regex_match0(str,rx)
        end if
    end function regex_match

    function regex_match0(str,regex) result(fstrings)  
        character(len=*),intent(in)::str,regex
        character(len=1,kind=c_char),allocatable::strx(:),rgx(:)
        integer(kind=c_int)::str_amt
        type(r_matches),pointer::fptr(:)
        type(c_ptr)::cptr
        integer :: i
        character(kind=c_char,len=1),pointer::z(:)
        type(reg_str),allocatable::fstrings(:)

        call fstring_to_cstring(str,strx) 
        call fstring_to_cstring(regex,rgx)
        cptr=rgx_match0(strx,rgx,str_amt)
        allocate(fstrings(str_amt))
        if(str_amt>0) then
            call c_f_pointer(cptr,fptr,[str_amt])
            do i=1,str_amt
                call c_f_pointer(fptr(i)%cptr, z,[fptr(i)%len])
                call cstring_to_fstring(z,fstrings(i)%str,fptr(i)%len)
                call C_free(fptr(i)%cptr)
                nullify(z)
            end do
            nullify(fptr)
        end if
        call C_free(cptr)
    end function regex_match0

    function regex_match1(str,regex,mflags) result(fstrings)  
        character(len=*),intent(in)::str,regex
        character(len=1,kind=c_char),allocatable::strx(:),rgx(:)
        integer(kind=c_int)::mflags(:),str_amt
        type(r_matches),pointer::fptr(:)
        type(c_ptr)::cptr
        integer :: i
        character(kind=c_char,len=1),pointer::z(:)
        type(reg_str),allocatable::fstrings(:)
        call fstring_to_cstring(str,strx) 
        call fstring_to_cstring(regex,rgx)
        cptr=rgx_match1(strx,rgx,mflags,size(mflags),str_amt)
        allocate(fstrings(str_amt))
        if(str_amt>0) then
            call c_f_pointer(cptr,fptr,[str_amt])
            do i=1,str_amt
                call c_f_pointer(fptr(i)%cptr, z,[fptr(i)%len])
                call cstring_to_fstring(z,fstrings(i)%str,fptr(i)%len)
                call C_free(fptr(i)%cptr)
                nullify(z)
            end do
            nullify(fptr)
        end if
        call C_free(cptr)
    end function regex_match1

    function regex_match2(str,regex,sflags,mflags) result(fstrings)  
        character(len=*),intent(in)::str,regex
        character(len=1,kind=c_char),allocatable::strx(:),rgx(:)
        integer(kind=c_int)::sflags(:),mflags(:),str_amt
        type(r_matches),pointer::fptr(:)
        type(c_ptr)::cptr
        integer :: i
        character(kind=c_char,len=1),pointer::z(:)
        type(reg_str),allocatable::fstrings(:)
        call fstring_to_cstring(str,strx) 
        call fstring_to_cstring(regex,rgx)
        cptr=rgx_match2(strx,rgx,sflags,size(sflags),mflags,size(mflags),str_amt)
        allocate(fstrings(str_amt))
        if(str_amt>0) then
            call c_f_pointer(cptr,fptr,[str_amt])
            do i=1,str_amt
                call c_f_pointer(fptr(i)%cptr, z,[fptr(i)%len])
                call cstring_to_fstring(z,fstrings(i)%str,fptr(i)%len)
                call C_free(fptr(i)%cptr)
                nullify(z)
            end do
            nullify(fptr)
        end if
        call C_free(cptr)
    end function regex_match2

    function regex_match3(str,regex,sflags) result(fstrings)  
        character(len=*),intent(in)::str,regex
        character(len=1,kind=c_char),allocatable::strx(:),rgx(:)
        integer(kind=c_int)::sflags(:),str_amt
        type(r_matches),pointer::fptr(:)
        type(c_ptr)::cptr
        integer :: i
        character(kind=c_char,len=1),pointer::z(:)
        type(reg_str),allocatable::fstrings(:)
        call fstring_to_cstring(str,strx) 
        call fstring_to_cstring(regex,rgx)
        cptr=rgx_match3(strx,rgx,sflags,size(sflags),str_amt)
        allocate(fstrings(str_amt))
        if(str_amt>0) then
            call c_f_pointer(cptr,fptr,[str_amt])
            do i=1,str_amt
                call c_f_pointer(fptr(i)%cptr, z,[fptr(i)%len])
                call cstring_to_fstring(z,fstrings(i)%str,fptr(i)%len)
                call C_free(fptr(i)%cptr)
                nullify(z)
            end do
            nullify(fptr)
        end if
        call C_free(cptr)
    end function regex_match3

    function cstrlen(carray) result(res)
        character(kind=c_char), intent(in) :: carray(:)
        integer :: res
        integer :: ii
        do ii = 1, size(carray)
          if (carray(ii) == c_null_char) then
            res = ii - 1
            return
          end if
        end do
        res = ii
      end function cstrlen

    function regex_replace(str,rx,replace,sflags,mflags) result(x)
        character(len=*),intent(in)::str,rx,replace
        integer,optional::sflags(:),mflags(:)
        character(len=:),allocatable::x
        if(present(sflags).and.present(mflags)) then
            x=regex_replace3(str,rx,replace,sflags,mflags)
        else if(present(mflags)) then
            x=regex_replace2(str,rx,replace,mflags)
        else if(present(sflags)) then
            x=regex_replace4(str,rx,replace,sflags)
        else
            x=regex_replace1(str,rx,replace)
        end if
    end function regex_replace
    function regex_replace1(str,regex,replace) result(ex)
        character(len=*),intent(in)::str,regex,replace
        character(len=1,kind=c_char),allocatable::strx(:),rgx(:),rpx(:)
        character(kind=c_char,len=1),pointer::rx(:)
        character(len=:),allocatable::ex
        integer(kind=c_int)::wx
        type(c_ptr)::z
        integer i
        call fstring_to_cstring(str,strx) 
        call fstring_to_cstring(regex,rgx)
        call fstring_to_cstring(replace,rpx)
        z=rgx_replace0(strx,rgx,rpx,wx)
        call c_f_pointer(z, rx,[wx])
        call cstring_to_fstring(rx,ex,wx)
        call C_free(z)
        nullify(rx)
    end function regex_replace1

    function regex_replace2(str,regex,replace,flags) result(ex)
        character(len=*),intent(in)::str,regex,replace
        character(len=1,kind=c_char),allocatable::strx(:),rgx(:),rpx(:)
        integer(kind=c_int),intent(in)::flags(:)
        character(kind=c_char,len=1),pointer::rx(:)
        character(len=:),allocatable::ex
        integer(kind=c_int)::wx
        type(c_ptr)::z
        call fstring_to_cstring(str,strx) 
        call fstring_to_cstring(regex,rgx)
        call fstring_to_cstring(replace,rpx)
        z=rgx_replace1(strx,rgx,rpx,flags,size(flags),wx)
        call c_f_pointer(z, rx,[wx])
        call cstring_to_fstring(rx,ex,wx)
        call C_free(z)
        nullify(rx)
    end function regex_replace2

    function regex_replace3(str,regex,replace,sflags,mflags) result(ex)
        character(len=*),intent(in)::str,regex,replace
        character(len=1,kind=c_char),allocatable::strx(:),rgx(:),rpx(:)
        integer(kind=c_int),intent(in)::sflags(:),mflags(:)
        character(kind=c_char,len=1),pointer::rx(:)
        character(len=:),allocatable::ex
        integer(kind=c_int)::wx
        type(c_ptr)::z
        call fstring_to_cstring(str,strx) 
        call fstring_to_cstring(regex,rgx)
        call fstring_to_cstring(replace,rpx)
        z=rgx_replace2(strx,rgx,rpx,sflags,size(sflags),mflags,size(mflags),wx)
        call c_f_pointer(z, rx,[wx])
        call cstring_to_fstring(rx,ex,wx)
        call C_free(z)
        nullify(rx)
    end function regex_replace3

    function regex_replace4(str,regex,replace,sflags) result(ex)
        character(len=*),intent(in)::str,regex,replace 
        character(len=1,kind=c_char),allocatable::strx(:),rgx(:),rpx(:)
        integer(kind=c_int),intent(in)::sflags(:)
        character(kind=c_char,len=1),pointer::rx(:)
        character(len=:),allocatable::ex
        integer(kind=c_int)::wx
        type(c_ptr)::z
        call fstring_to_cstring(str,strx)
        call fstring_to_cstring(regex,rgx)
        call fstring_to_cstring(replace,rpx)
        z=rgx_replace3(strx,rgx,rpx,sflags,size(sflags),wx)
        call c_f_pointer(z, rx,[wx])
        call cstring_to_fstring(rx,ex,wx)
        call C_free(z)
        nullify(rx)
    end function regex_replace4

    subroutine fstring_to_cstring(xs,mk) 
        character(len=*),intent(in)::xs
        character(len=1,kind=c_char),allocatable,intent(inout)::mk(:)
        character(len=:),allocatable::tmp
        integer::i
        tmp=trim(adjustl(xs))//c_null_char
        allocate(mk(len(tmp)));
        do i=1,len(tmp)
            mk(i)=tmp(i:i)
        end do
    end subroutine fstring_to_cstring

    subroutine cstring_to_fstring(cstr,fstr,clen)
        character(len=1,kind=c_char),intent(in)::cstr(*)
        character(len=:),allocatable,intent(inout)::fstr
        integer,intent(in),optional::clen
        integer::i=1,i2
        if(present(clen)) then
            allocate(character(len=clen)::fstr)
            do i=1,clen
                fstr(i:i)=cstr(i)
            end do
        else
            do while(cstr(i)/=c_null_char)
                i=i+1
            end do
            i=i-1
            allocate(character(len=i)::fstr)
            do i2=1,i
                fstr(i2:i2)=cstr(i2)
            end do
        end if
    end subroutine cstring_to_fstring

      function add_null(x) result(z)
        character(len=*),intent(in)::x
        character(len=:),allocatable::z
        z=x//c_null_char
      end function add_null
end module regex