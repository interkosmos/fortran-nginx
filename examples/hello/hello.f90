! hello.f90
!
! Very basic Fortran web application for nginx. Requires the 3rd party module
! `nginx-link-function`. Add the shared library `hello.so` to your
! 'nginx.conf`:
!
!     http {
!         server {
!             listen            80;
!             server_name       localhost;
!             ngx_link_func_lib "/usr/local/etc/nginx/hello.so";
!
!             location = /greeting {
!                 ngx_link_func_call "greeting";
!             }
!         }
!     }
!
! Restart nginx and Open `http://localhost/greeting` with your web browser.
!
! Author:  Philipp Engel
! Licence: ISC
module hello
    use, intrinsic :: iso_c_binding
    use :: ngx_link_func
    implicit none
    public :: greeting
    public :: ngx_link_func_exit_cycle
    public :: ngx_link_func_init_cycle

    logical, save :: is_service_on = .false.
contains
    ! void ngx_link_func_init_cycle(ngx_link_func_cycle_t* cyc)
    subroutine ngx_link_func_init_cycle(cyc) bind(c)
        type(ngx_link_func_cycle_t), intent(in) :: cyc

        call ngx_link_func_cyc_log_info(cyc, 'Starting the web app ...' // c_null_char)
        is_service_on = .true.
    end subroutine ngx_link_func_init_cycle

    ! void ngx_link_func_exit_cycle(ngx_link_func_cycle_t* cyc)
    subroutine ngx_link_func_exit_cycle(cyc) bind(c)
        type(ngx_link_func_cycle_t), intent(in) :: cyc

        call ngx_link_func_cyc_log_info(cyc, 'Shutting down the web app ...' // c_null_char)
        is_service_on = .false.
    end subroutine ngx_link_func_exit_cycle

    subroutine greeting(ctx) bind(c)
        character(len=*), parameter :: content = '<!DOCTYPE html>' // &
                                                 '<html lang="en">' // &
                                                 '<head>' // &
                                                 '<meta charset="utf-8">' // &
                                                 '<title>Fortran Web App</title>' // &
                                                 '</head>' // &
                                                 '<body><h1>Hello, from Fortran!</h1></body>' // &
                                                 '</html>'
        type(ngx_link_func_ctx_t), intent(in) :: ctx

        call ngx_link_func_log_info(ctx, 'Sending greeting ...' // c_null_char)
        call ngx_link_func_write_resp(ctx, &
                                      int(200, kind=8), &
                                      '200 OK' // c_null_char, &
                                      NGX_LINK_FUNC_CONTENT_TYPE_PLAINTEXT, &
                                      content // c_null_char, &
                                      int(len(content), kind=8))
    end subroutine greeting
end module hello
