! laas.f90
!
! "LAPACK as a Service" web application for nginx, solves the system of linear
! equations A * x = B:
!
!     | a1  a3 | * | x1 | = | b1 |
!     | a2  a4 |   | x2 |   | b2 |
!
! Send a GET request to:
!
!     http://localhost/api/solve?a1=2.0&a2=3.0&a3=1.0&a4=1.0&b1=5.0&b2=6.0
!
! Get the response:
!
!     x1,1.00000000,x2,3.00000000
!
! Author:  Philipp Engel
! Licence: ISC
module laas
    use, intrinsic :: iso_c_binding, only: c_null_char
    use :: ngx_link_func
    implicit none

    logical, save :: is_service_on = .false.
contains
    subroutine ngx_link_func_init_cycle(cyc) bind(c)
        type(ngx_link_func_cycle_t), intent(in) :: cyc

        call ngx_link_func_cyc_log_info(cyc, 'Starting the web app ...' // c_null_char)
        is_service_on = .true.
    end subroutine ngx_link_func_init_cycle

    subroutine ngx_link_func_exit_cycle(cyc) bind(c)
        type(ngx_link_func_cycle_t), intent(in) :: cyc

        call ngx_link_func_cyc_log_info(cyc, 'Shutting down the web app ...' // c_null_char)
        is_service_on = .false.
    end subroutine ngx_link_func_exit_cycle

    subroutine ngx_solve(ctx) bind(c)
        use :: f95_lapack,   only: la_gesv
        use :: la_precision, only: wp => dp
        type(ngx_link_func_ctx_t), intent(in) :: ctx
        character(len=128)                    :: response
        real(kind=wp)                         :: a(2, 2)
        real(kind=wp)                         :: b(2)
        character(len=:), allocatable         :: a1, a2, a3, a4, b1, b2
        integer                               :: rc

        call ngx_link_func_log_info(ctx, 'Solving ...' // c_null_char)

        a = 0._wp
        b = 0._wp

        a1 = ngx_link_func_get_query_param(ctx, 'a1' // c_null_char)
        a2 = ngx_link_func_get_query_param(ctx, 'a2' // c_null_char)
        a3 = ngx_link_func_get_query_param(ctx, 'a3' // c_null_char)
        a4 = ngx_link_func_get_query_param(ctx, 'a4' // c_null_char)

        read (a1, *, iostat=rc) a(1, 1)
        read (a2, *, iostat=rc) a(2, 1)
        read (a3, *, iostat=rc) a(1, 2)
        read (a4, *, iostat=rc) a(2, 2)

        b1 = ngx_link_func_get_query_param(ctx, 'b1' // c_null_char)
        b2 = ngx_link_func_get_query_param(ctx, 'b2' // c_null_char)

        read (b1, *, iostat=rc) b(1)
        read (b2, *, iostat=rc) b(2)

        call la_gesv(a=a, b=b, info=rc)

        write (response, '("x1,", f0.8, ",x2,", f0.8)') b(1), b(2)

        call ngx_link_func_write_resp(ctx, &
                                      int(200, kind=8), &
                                      '200 OK' // c_null_char, &
                                      'text/plain' // c_null_char, &
                                      trim(response) // c_null_char, &
                                      int(len_trim(response), kind=8))
    end subroutine ngx_solve
end module laas
