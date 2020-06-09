! plot.f90
!
! Example web application that returns a plot in PNG format of the
! Lotka-Volterra ODEs, solved using the Runge-Kutta 4th order method.
!
! The initial population sizes of predator and prey species are passed by the
! HTTP GET parameters `a` and `b`:
!
!     http://localhost/?a=5&b=20
!
! The min. (0) and max. (50) values for both parameters are hard-coded.
!
! Be aware that the DISLIN library `./lib/dislin-*.a` must have been compiled
! with flag `-fPIC`!!
!
! Equations:
!
!     u(t) >= 0     -   Population size of prey at time t.
!     v(t) >= 0     -   Population size of predator at time t.
!
!     d/dt u = u * (alpha - beta * v)
!     d/dt v = -v * (gamma - delta * u)
!
! Model Parameters:
!
!     u             -   Prey population.
!     v             -   Predator population.
!     alpha         -   Reproduction rate of prey.
!     beta          -   Death rate of prey by predator.
!     gamma         -   Death rate of predator.
!     delta         -   Reproduction rate of predator by prey.
!
! Author:  Philipp Engel
! Licence: ISC
module plot
    use, intrinsic :: iso_c_binding,   only: c_null_char
    use, intrinsic :: iso_fortran_env, only: compiler_version, wp => real32
    use :: ngx_link_func
    implicit none
    private

    !! Point estimates for model parameters, taken from:
    !!     https://www.math.tamu.edu/~phoward/m442/modbasics.pdf
    real(kind=wp), parameter :: ALPHA = 0.470_wp
    real(kind=wp), parameter :: BETA  = 0.024_wp
    real(kind=wp), parameter :: DELTA = 0.023_wp
    real(kind=wp), parameter :: GAMMA = 0.760_wp

    logical, save :: is_service_on = .false.

    public :: ngx_plot
    public :: ngx_link_func_exit_cycle
    public :: ngx_link_func_init_cycle
contains
    function rk4(r, t, h)
        !! Runge-Kutta 4th order solver.
        real(kind=wp), intent(in) :: r(2)   ! Initial values.
        real(kind=wp), intent(in) :: t      ! Step.
        real(kind=wp), intent(in) :: h      ! Step size.
        real(kind=wp)             :: rk4(2)
        real(kind=wp)             :: k1(2), k2(2), k3(2), k4(2)

        k1 = h * f(r,            t)
        k2 = h * f(r + 0.5 * k1, t + 0.5 * h)
        k3 = h * f(r + 0.5 * k2, t + 0.5 * h)
        k4 = h * f(r + k3,       t + h)

        rk4 = (k1 + (2 * k2) + (2 * k3) + k4) / 6
    end function rk4

    function f(r, t)
        !! The Lotka-Volterra ODEs.
        real(kind=wp), intent(in) :: r(2)
        real(kind=wp), intent(in) :: t
        real(kind=wp)             :: f(2)
        real(kind=wp)             :: u, v

        u = r(1)
        v = r(2)

        f(1) =  u * (ALPHA - BETA * v)
        f(2) = -v * (GAMMA - DELTA * u)
    end function f

    subroutine ngx_link_func_exit_cycle(cyc) bind(c)
        type(ngx_link_func_cycle_t), intent(in) :: cyc

        call ngx_link_func_cyc_log_info(cyc, 'Shutting down the web app ...' // c_null_char)
        is_service_on = .false.
    end subroutine ngx_link_func_exit_cycle

    subroutine ngx_link_func_init_cycle(cyc) bind(c)
        type(ngx_link_func_cycle_t), intent(in) :: cyc

        call ngx_link_func_cyc_log_info(cyc, 'Starting the web app ...' // c_null_char)
        is_service_on = .true.
    end subroutine ngx_link_func_init_cycle

    subroutine ngx_plot(ctx) bind(c)
        real(kind=wp), parameter              :: T_MAX = 50.0_wp
        real(kind=wp), parameter              :: H     = 0.01_wp
        integer,       parameter              :: N     = T_MAX / H
        type(ngx_link_func_ctx_t), intent(in) :: ctx
        character(len=:), allocatable         :: png
        character(len=:), allocatable         :: a, b
        integer                               :: i, rc
        real(kind=wp)                         :: t(N)  = [ (H * i, i = 1,  N) ]
        real(kind=wp)                         :: r(2)
        real(kind=wp)                         :: x(N), y(N)

        ! Initial prey population.
        a = ngx_link_func_get_query_param(ctx, 'a' // c_null_char)
        ! Initial predator population.
        b = ngx_link_func_get_query_param(ctx, 'b' // c_null_char)

        read (a, *, iostat=rc) r(1)
        read (b, *, iostat=rc) r(2)

        r(1) = min(50, max(1, int(r(1))))
        r(2) = min(50, max(1, int(r(2))))

        ! Solve ODE.
        do i = 1, N
            x(i) = r(1)
            y(i) = r(2)
            r = r + rk4(r, t(i), H)
        end do

        call plot_curves(t, x, y, N, T_MAX, png)
        call ngx_link_func_write_resp(ctx, &
                                      int(200, kind=8), &
                                      '200 OK' // c_null_char, &
                                      'image/png' // c_null_char, &
                                      png // c_null_char, &
                                      int(len(png), kind=8))
        deallocate (png)
    end subroutine ngx_plot

    subroutine plot_curves(t, x, y, n, t_max, png)
        !! Plotting routine that calls DISLIN and returns the rendered PNG image
        !! in `png`.
        use :: dislin
        integer,                       intent(in)    :: n
        real(kind=wp),                 intent(inout) :: t(n)
        real(kind=wp),                 intent(inout) :: x(n)
        real(kind=wp),                 intent(inout) :: y(n)
        real(kind=wp),                 intent(in)    :: t_max
        character(len=:), allocatable, intent(inout) :: png
        character(len=80)                            :: leg
        character(len=80)                            :: sub
        character(len=80)                            :: lab
        integer                                      :: dt(8)
        integer                                      :: bs, c, m
        real                                         :: dv

        ! Set DISLIN output.
        call metafl('virt')
        call scrmod('revers')
        call winsiz(1024, 768)
        call imgfmt('rgb')

        ! Initialise DISLIN.
        call disini()
        call errmod('all', 'off')   ! Disable console output.
        call texmod('on')           ! Enable TeX features.
        call simplx()               ! Load single stroke font.

        ! Set title and sub-title.
        call titlin('Lotka-Volterra Equations', 1)
        write (sub, '(4(a, f5.3))') '$\alpha$ = ', ALPHA, &
                                    ', $\beta$ = ', BETA, &
                                    ', $\delta$ = ', DELTA, &
                                    ', $\gamma$ = ', GAMMA
        call titlin(trim(sub), 3)

        ! Set bottom label.
        call date_and_time(values=dt)
        call getver(dv)
        write (lab, '(i4, 5(a, i2.2), 3a, f4.1)') &
            dt(1), '/', dt(2), '/', dt(3), ' ', dt(5), ':', dt(6), ':', dt(7), &
            ', ', compiler_version(), ', DISLIN ', dv
        call messag(trim(lab), 100, 2000)

        ! Set axis.
        call name('x-axis', 'Time')
        call name('y-axis', 'Population Size')

        call labdig(-1, 'x')
        call ticks(10, 'xy')

        c = intrgb(.95, .95, .95)
        call axsbgd(c)

        ! Set plot limits and grid.
        call graf(0., t_max, 0., 10., 0., 300., 0., 50.)
        call setrgb(.7, .7, .7)
        call grid(1, 1)

        ! Show title.
        call color('fore')
        call title()

        ! Plot curves.
        call color('red')
        call curve(t, x, n)

        call color('green')
        call curve(t , y, n)

        ! Show legend.
        call color('fore')
        call legini(leg, 2, 8)
        call legtit(' ')
        call leglin(leg, 'Predator', 1)
        call leglin(leg, 'Prey', 2)
        call legend(leg, 7)

        ! Copy raw PNG to buffer.
        call rbfpng(png, 0, bs)
        if (allocated(png)) deallocate (png)
        allocate (character(len=bs) :: png)
        call rbfpng(png, bs, m)

        ! Clean up.
        call disfin()
    end subroutine plot_curves
end module plot
