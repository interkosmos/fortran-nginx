! ngx_link_func.f90
!
! Fortran 2003 ISO C binding interfaces to the `ngx_link_function` 3rd party
! module for nginx. Currently, these bindings do not support the shared memory
! features of `ngx_link_function` (cache, palloc, shm, shmtx) that allow
! resource sharing between nginx workers.
!
! Author:  Philipp Engel
! Licence: ISC
module ngx_link_func
    use, intrinsic :: iso_c_binding
    implicit none
    private

    character(len=*, kind=c_char), parameter, public :: NGX_LINK_FUNC_CONTENT_TYPE_PLAINTEXT = &
        'text/plain' // c_null_char
    character(len=*, kind=c_char), parameter, public :: NGX_LINK_FUNC_CONTENT_TYPE_HTML = &
        'text/html charset=utf-8' // c_null_char
    character(len=*, kind=c_char), parameter, public :: NGX_LINK_FUNC_CONTENT_TYPE_JSON = &
        'application/json' // c_null_char
    character(len=*, kind=c_char), parameter, public :: NGX_LINK_FUNC_CONTENT_TYPE_JSONP = &
        'application/javascript' // c_null_char
    character(len=*, kind=c_char), parameter, public :: NGX_LINK_FUNC_CONTENT_TYPE_XFORMENCODED = &
        'application/x-www-form-urlencoded' // c_null_char

    type, bind(c), public :: ngx_link_func_ctx_t
        type(c_ptr)            :: req_args
        type(c_ptr)            :: req_body
        integer(kind=c_size_t) :: req_body_len
        type(c_ptr)            :: shared_mem
        type(c_ptr)            :: r_
        type(c_ptr)            :: pl_
        type(c_ptr)            :: log_
    end type ngx_link_func_ctx_t

    type, bind(c), public :: ngx_link_func_cycle_t
        type(c_ptr)         :: shared_mem
        integer(kind=c_int) :: has_error
        type(c_ptr)         :: cycle_
        type(c_ptr)         :: srv_cf_
        type(c_ptr)         :: pl_
        type(c_ptr)         :: log_
    end type ngx_link_func_cycle_t

    public :: c_f_str_ptr
    public :: c_strlen
    public :: ngx_link_func_cyc_log_debug
    public :: ngx_link_func_cyc_log_err
    public :: ngx_link_func_cyc_log_info
    public :: ngx_link_func_cyc_log_warn
    public :: ngx_link_func_get_header
    public :: ngx_link_func_get_query_param
    public :: ngx_link_func_log_debug
    public :: ngx_link_func_log_err
    public :: ngx_link_func_log_info
    public :: ngx_link_func_log_warn
    public :: ngx_link_func_write_resp

    interface
        ! u_char *ngx_link_func_get_header(ngx_link_func_ctx_t *ctx, const char*key)
        function ngx_link_func_get_header_(ctx, key) bind(c, name='ngx_link_func_get_header')
            import :: c_char, c_ptr, ngx_link_func_ctx_t
            type(ngx_link_func_ctx_t), intent(in) :: ctx
            character(kind=c_char),    intent(in) :: key
            type(c_ptr)                           :: ngx_link_func_get_header_
        end function ngx_link_func_get_header_

        ! void *ngx_link_func_get_query_param(ngx_link_func_ctx_t *ctx, const char *key)
        function ngx_link_func_get_query_param_(ctx, key) bind(c, name='ngx_link_func_get_query_param')
            import :: c_char, c_ptr, ngx_link_func_ctx_t
            type(ngx_link_func_ctx_t), intent(in) :: ctx
            character(kind=c_char),    intent(in) :: key
            type(c_ptr)                           :: ngx_link_func_get_query_param_
        end function ngx_link_func_get_query_param_

        ! void ngx_link_func_cyc_log_debug(ngx_link_func_cycle_t *ctx, const char* msg)
        subroutine ngx_link_func_cyc_log_debug(ctx, msg) bind(c, name='ngx_link_func_cyc_log_debug')
            import :: c_char, ngx_link_func_cycle_t
            type(ngx_link_func_cycle_t), intent(in) :: ctx
            character(kind=c_char),      intent(in) :: msg
        end subroutine ngx_link_func_cyc_log_debug

        ! void ngx_link_func_cyc_log_error(ngx_link_func_cycle_t *ctx, const char* msg)
        subroutine ngx_link_func_cyc_log_err(ctx, msg) bind(c, name='ngx_link_func_cyc_log_err')
            import :: c_char, ngx_link_func_cycle_t
            type(ngx_link_func_cycle_t), intent(in) :: ctx
            character(kind=c_char),      intent(in) :: msg
        end subroutine ngx_link_func_cyc_log_err

        ! void ngx_link_func_cyc_log_info(ngx_link_func_cycle_t *ctx, const char* msg)
        subroutine ngx_link_func_cyc_log_info(ctx, msg) bind(c, name='ngx_link_func_cyc_log_info')
            import :: c_char, ngx_link_func_cycle_t
            type(ngx_link_func_cycle_t), intent(in) :: ctx
            character(kind=c_char),      intent(in) :: msg
        end subroutine ngx_link_func_cyc_log_info

        ! void ngx_link_func_cyc_log_warn(ngx_link_func_cycle_t *ctx, const char* msg)
        subroutine ngx_link_func_cyc_log_warn(ctx, msg) bind(c, name='ngx_link_func_cyc_log_warn')
            import :: c_char, ngx_link_func_cycle_t
            type(ngx_link_func_cycle_t), intent(in) :: ctx
            character(kind=c_char),      intent(in) :: msg
        end subroutine ngx_link_func_cyc_log_warn

        ! void ngx_link_func_log_debug(ngx_link_func_ctx_t *ctx, const char* msg)
        subroutine ngx_link_func_log_debug(ctx, msg) bind(c, name='ngx_link_func_log_debug')
            import :: c_char, ngx_link_func_ctx_t
            type(ngx_link_func_ctx_t), intent(in) :: ctx
            character(kind=c_char),    intent(in) :: msg
        end subroutine ngx_link_func_log_debug

        ! void ngx_link_func_log_err(ngx_link_func_ctx_t *ctx, const char* msg)
        subroutine ngx_link_func_log_err(ctx, msg) bind(c, name='ngx_link_func_log_err')
            import :: c_char, ngx_link_func_ctx_t
            type(ngx_link_func_ctx_t), intent(in) :: ctx
            character(kind=c_char),    intent(in) :: msg
        end subroutine ngx_link_func_log_err

        ! void ngx_link_func_log_info(ngx_link_func_ctx_t *ctx, const char* msg)
        subroutine ngx_link_func_log_info(ctx, msg) bind(c, name='ngx_link_func_log_info')
            import :: c_char, ngx_link_func_ctx_t
            type(ngx_link_func_ctx_t), intent(in) :: ctx
            character(kind=c_char),    intent(in) :: msg
        end subroutine ngx_link_func_log_info

        ! void ngx_link_func_log_warn(ngx_link_func_ctx_t *ctx, const char* msg)
        subroutine ngx_link_func_log_warn(ctx, msg) bind(c, name='ngx_link_func_log_warn')
            import :: c_char, ngx_link_func_ctx_t
            type(ngx_link_func_ctx_t), intent(in) :: ctx
            character(kind=c_char),    intent(in) :: msg
        end subroutine ngx_link_func_log_warn

        ! void ngx_link_func_write_resp(ngx_link_func_ctx_t *ctx, uintptr_t status_code, const char* status_line, const char* content_type, const char* resp_content, size_t resp_len)
        subroutine ngx_link_func_write_resp(ctx, status_code, status_line, content_type, resp_content, resp_len) &
                bind(c, name='ngx_link_func_write_resp')
            import :: c_char, c_intptr_t, c_ptr, c_size_t, ngx_link_func_ctx_t
            type(ngx_link_func_ctx_t), intent(in)        :: ctx
            integer(kind=c_intptr_t),  intent(in), value :: status_code
            character(kind=c_char),    intent(in)        :: status_line
            character(kind=c_char),    intent(in)        :: content_type
            character(kind=c_char),    intent(in)        :: resp_content
            integer(kind=c_size_t),    intent(in), value :: resp_len
        end subroutine
    end interface

    interface
        ! size_t strlen(char *str)
        function c_strlen(str) bind(c, name='strlen')
            import :: c_ptr, c_size_t
            type(c_ptr), intent(in), value :: str
            integer(c_size_t)              :: c_strlen
        end function c_strlen
    end interface
contains
    function ngx_link_func_get_query_param(ctx, key)
        !! Wrapper for `ngx_link_func_get_query_param()` that converts the
        !! returned C pointer to Fortran string. The argument `key` must be
        !! null-terminated.
        type(ngx_link_func_ctx_t), intent(in) :: ctx
        character(len=*),          intent(in) :: key
        character(len=:), allocatable         :: ngx_link_func_get_query_param
        type(c_ptr)                           :: ptr

        ptr = ngx_link_func_get_query_param_(ctx, key)
        if (.not. c_associated(ptr)) return
        allocate (character(len=c_strlen(ptr)) :: ngx_link_func_get_query_param)
        call c_f_str_ptr(ptr, ngx_link_func_get_query_param)
    end function ngx_link_func_get_query_param

    function ngx_link_func_get_header(ctx, key)
        !! Wrapper for `ngx_link_func_get_header()` that converts the returned C
        !! pointer to Fortran string. The argument `key` must be
        !! null-terminated.
        type(ngx_link_func_ctx_t), intent(in) :: ctx
        character(kind=c_char),    intent(in) :: key
        character(len=:), allocatable         :: ngx_link_func_get_header
        type(c_ptr)                           :: ptr

        ptr = ngx_link_func_get_header_(ctx, key)
        if (.not. c_associated(ptr)) return
        allocate (character(len=c_strlen(ptr)) :: ngx_link_func_get_header)
        call c_f_str_ptr(ptr, ngx_link_func_get_header)
    end function ngx_link_func_get_header

    subroutine c_f_str_ptr(c_string, f_string)
        !! Utility routine that copies a C string, passed as a C pointer, to a
        !! Fortran string.
        type(c_ptr),      intent(in)           :: c_string
        character(len=*), intent(out)          :: f_string
        character(kind=c_char, len=1), pointer :: char_ptrs(:)
        integer                                :: i

        if (.not. c_associated(c_string)) then
            f_string = ' '
            return
        end if

        call c_f_pointer(c_string, char_ptrs, [ huge(0) ])

        i = 1

        do while (char_ptrs(i) /= c_null_char .and. i <= len(f_string))
            f_string(i:i) = char_ptrs(i)
            i = i + 1
        end do

        if (i < len(f_string)) f_string(i:) = ' '
    end subroutine c_f_str_ptr
end module ngx_link_func
