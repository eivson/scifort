module scifort_timer_mod
    use scifort_kinds_mod, only: I64, R64
    
    implicit none

    private

    integer, parameter, public :: ICLOCK=I64
    integer, parameter, public :: RCLOCK=R64

    integer(ICLOCK), protected, save, public :: timer_stamp_rate     = 0                ! If unitialized, this gonna cause a division by zero
    logical,         protected, save, public :: timer_is_initialized = .FALSE.

    public :: timer_initialize, timer_stamp, timer_elapsed_time, &
              timer_get_time, timer_write, timer_print

    interface
        module subroutine timer_initialize()
        end subroutine timer_initialize

        module function timer_stamp() result(stamp)
            integer(ICLOCK) :: stamp
        end function timer_stamp
    end interface

    interface timer_elapsed_time
        module function timer_elapsed_time_anywhere(stamp_0, stamp_1) result(dt)
            integer(ICLOCK), intent(in) :: stamp_0, stamp_1
            real(RCLOCK)                :: dt
        end function timer_elapsed_time_anywhere

        module function timer_elapsed_time_now(stamp_0) result(dt)
            integer(ICLOCK), intent(in) :: stamp_0
            real(RCLOCK)                :: dt
        end function timer_elapsed_time_now
    end interface timer_elapsed_time

    interface 
        module pure subroutine timer_get_time(dt, days, hours, minutes, seconds)
            real(RCLOCK),    intent(in)  :: dt
            integer(ICLOCK), intent(out) :: days, hours, minutes
            real(RCLOCK),    intent(out) :: seconds
        end subroutine timer_get_time
    end interface

    interface timer_write
        module subroutine timer_write_time(days, hours, minutes, seconds, unit)
            integer(ICLOCK), intent(in) :: days, hours, minutes
            real(RCLOCK),    intent(in) :: seconds
            integer,         intent(in) :: unit
        end subroutine timer_write_time

        module subroutine timer_write_dt(dt, unit)
            real(RCLOCK), intent(in) :: dt
            integer,      intent(in) :: unit
        end subroutine timer_write_dt

        module subroutine timer_write_stamp_anywhere(stamp_0, stamp_1, unit)
            integer(ICLOCK), intent(in) :: stamp_0, stamp_1
            integer,         intent(in) :: unit
        end subroutine timer_write_stamp_anywhere

        module subroutine timer_write_stamp_now(stamp_0, unit)
            integer(ICLOCK), intent(in) :: stamp_0
            integer,         intent(in) :: unit
        end subroutine timer_write_stamp_now
    end interface timer_write

    interface timer_print
        module subroutine timer_print_time(days, hours, minutes, seconds)
            integer(ICLOCK), intent(in) :: days, hours, minutes
            real(RCLOCK),    intent(in) :: seconds
        end subroutine timer_print_time

        module subroutine timer_print_dt(dt)
            real(RCLOCK), intent(in) :: dt
        end subroutine timer_print_dt

        module subroutine timer_print_stamp_anywhere(stamp_0, stamp_1)
            integer(ICLOCK), intent(in) :: stamp_0, stamp_1
        end subroutine timer_print_stamp_anywhere

        module subroutine timer_print_stamp_now(stamp_0)
            integer(ICLOCK), intent(in) :: stamp_0
        end subroutine timer_print_stamp_now
    end interface timer_print
end module scifort_timer_mod
