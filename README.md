# fortran-nginx
A collection of ISO C binding interfaces to the 3rd party module
[nginx-link-function](https://github.com/Taymindis/nginx-link-function/) for
[nginx](https://nginx.org/), to write server-side web applications in Fortran
2003.

Shared memory features (`cache`, `palloc`, `shm`, `shmtx`) that allow resource
sharing between nginx workers are not supported.

## Dependencies
Either install [nginx](https://nginx.org/) or
[OpenResty](https://openresty.org/) with the
[nginx-link-function](https://github.com/Taymindis/nginx-link-function/) module.

On FreeBSD, build [www/nginx](https://www.freshports.org/www/nginx) with the
`LINK` option enabled from ports:

```
# cd /usr/local/ports/www/nginx/
# make config
# make
# make install
```

Or, just install a package that has been compiled with `LINK`:

```
# pkg install www/nginx
# pkg info www/nginx
```

## Build
Use the provided `Makefile` to build the interface bindings `ngx_link_func.o`,
or simply run:

```
$ gfortran -c src/ngx_link_func.f90
```

## Example
Your Fortran web application must implement at least the routines
`ngx_link_func_init_cycle()` and `ngx_link_func_exit_cycle()`. All routines
callable from outside must have the `bind(c)` attribute.

```fortran
! webapp.f90
module webapp
    use, intrinsic :: iso_c_binding, only: c_null_char
    use :: ngx_link_func
    implicit none
    logical, save :: is_service_on = .false.
contains
    subroutine ngx_hello(ctx) bind(c)
        character(len=*), parameter           :: str = 'Hello, from Fortran!'
        type(ngx_link_func_ctx_t), intent(in) :: ctx

        call ngx_link_func_log_info(ctx, 'Sending response ...' // c_null_char)
        call ngx_link_func_write_resp(ctx, &
                                      int(200, kind=8), &
                                      '200 OK' // c_null_char, &
                                      'text/plain' // c_null_char, &
                                      str // c_null_char, &
                                      int(len(str), kind=8))
    end subroutine ngx_hello

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
end module webapp
```

Compile the shared library `webapp.so` with:

```
$ gfortran -shared -fPIC -o webapp.so webapp.f90 ngx_link_func.o
```

If you use GNU Fortran, make sure that nginx can find the run-time library
`libgfortran.so`!

Load the shared library by setting `ngx_link_func_lib` and `ngx_link_func_call`
in your `nginx.conf`:

```nginx
server {
    listen            80;
    server_name       localhost;
    ngx_link_func_lib "/usr/local/etc/nginx/webapp.so";

    location / {
        ngx_link_func_call "ngx_hello";
    }
}
```

Start the nginx daemon:

```
# service nginx start
```

Then, open `http://localhost/` in your web browser.

## Further Examples
Additional examples can be found in `examples/`:

  * **hello** returns a basic HTML response.
  * **laas** (LAPACK as a Service) solves a system of linear equations *A · x = B* using [LAPACK95](https://www.netlib.org/lapack95/).
  * **plot** returns a plot of the [Lotka-Volterra](https://en.wikipedia.org/wiki/Lotka–Volterra_equations) ODEs in PNG format, using the [DISLIN](http://dislin.de/) library. Pass the initial population sizes through HTTP GET parameters `a` and `b` ([example output](examples/plot/output.png)).
  * **post** parses HTTP POST parameters.

Build the examples with:

```
$ make <name>
```

## Licence
ISC
