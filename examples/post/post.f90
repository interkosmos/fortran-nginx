! post.f90
!
! Example that parses and returns the given POST parameters.
!
! Simply call:
! $ curl -X POST -d "param1=test&param2=12345" "http://localhost/post"
!
! Author:  Philipp Engel
! Licence: ISC
module post
    use, intrinsic :: iso_c_binding, only: c_null_char
    use :: ngx_link_func
    implicit none

    logical, save :: is_service_on = .false.
contains
    function count_sub_string(s1, s2) result(c)
        !! Returns the number of occurences of string `s2` in string `s1`.
        character(len=*), intent(in) :: s1
        character(len=*), intent(in) :: s2
        integer                      :: c
        integer                      :: p, pos_n

        c = 0
        p = 1

        if (len(s2) == 0) return

        do
            pos_n = index(s1(p:), s2)
            if (pos_n == 0) return
            c = c + 1
            p = p + pos_n + len(s2)
        end do
    end function count_sub_string

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

    subroutine ngx_post(ctx) bind(c)
        type(ngx_link_func_ctx_t), intent(in) :: ctx
        character(len=:), allocatable         :: req_body
        character(len=:), allocatable         :: response
        character(len=256), allocatable       :: params(:)
        character(len=256)                    :: pair(2)
        integer                               :: nparams
        integer                               :: i

        allocate (character(len=ctx%req_body_len) :: req_body)
        call c_f_str_ptr(ctx%req_body, req_body)

        nparams = 1 + count_sub_string(req_body, '&')
        allocate (params(nparams))
        call split(req_body, params, '&')

        response = 'POST Parameters:' // new_line('A')

        do i = 1, nparams
            call split(params(i), pair, '=')
            response = response // trim(pair(1)) // ': ' // trim(pair(2)) // new_line('A')
        end do

        call ngx_link_func_write_resp(ctx, &
                                      int(200, kind=8), &
                                      '200 OK' // c_null_char, &
                                      'text/html' // c_null_char, &
                                      response // c_null_char, &
                                      int(len(response), kind=8))

        deallocate (params)
    end subroutine ngx_post

    subroutine split(str, array, del)
        !! Splits a string by a given delimiter into an array of strings.
        character(len=*), intent(in)    :: str
        character(len=*), intent(inout) :: array(:)
        character(len=*), intent(in)    :: del
        integer                         :: n, pos1, pos2

        pos1 = 1
        n    = 0

        do
            pos2 = index(str(pos1:), del)

            if (pos2 == 0) then
                n = n + 1
                if (n > size(array)) exit
                array(n) = str(pos1:)
                exit
            end if

            n = n + 1
            array(n) = str(pos1:pos1 + pos2 - 2)
            pos1 = pos1 + pos2
        end do
    end subroutine split
end module post
