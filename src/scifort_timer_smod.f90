submodule (scifort_timer_mod) scifort_timer_smod
    use scifort_io_env_mod, only: io_env_outunit
    
    implicit none

    real(RCLOCK), parameter :: SECONDS_PER_MINUTE = 60.0_RCLOCK
    real(RCLOCK), parameter :: SECONDS_PER_HOUR   = 3600.0_RCLOCK
    real(RCLOCK), parameter :: SECONDS_PER_DAY    = 24*SECONDS_PER_HOUR

    character(*), parameter :: ELAPSED_TIME_HEAD = "Elapsed time: "
contains
    module procedure timer_initialize
        if (.not.timer_is_initialized) then
            call system_clock(COUNT_RATE=timer_stamp_rate)
            timer_is_initialized = .TRUE.
        end if
    end procedure timer_initialize


    module procedure timer_stamp
        call system_clock(stamp)
    end procedure timer_stamp


    module procedure timer_elapsed_time_anywhere
        dt = real(stamp_1 - stamp_0, KIND=RCLOCK) / timer_stamp_rate
    end procedure timer_elapsed_time_anywhere


    module procedure timer_elapsed_time_now
        integer(ICLOCK) :: stamp_1

        stamp_1 = timer_stamp()
        dt      = timer_elapsed_time(stamp_0, stamp_1)
    end procedure timer_elapsed_time_now


    module procedure timer_get_time
        real(RCLOCK) :: rem

        call time_fact(dt,       SECONDS_PER_DAY,    days,    seconds)
        call time_fact(seconds,  SECONDS_PER_HOUR,   hours,   rem)
        call time_fact(rem,      SECONDS_PER_MINUTE, minutes, seconds)
    contains
        pure subroutine time_fact(dt, rate, p, r)
            real(RCLOCK),    intent(in)  :: dt, rate
            integer(ICLOCK), intent(out) :: p
            real(RCLOCK),    intent(out) :: r

            p = floor(dt / rate, KIND=ICLOCK)
            r = mod(dt, rate)
        end subroutine time_fact
    end procedure timer_get_time


    module procedure timer_write_time
        character(*), parameter :: LAYOUT="(1X,A,3(I0,A),ES0.4,A)"

        write(UNIT=unit, FMT=LAYOUT)   &
             ELAPSED_TIME_HEAD,        &
             days, " day(s) ",         &
             hours, " hour(s) ",       &
             minutes, " min(s) and ",  &
             seconds,  " sec(s)."
    end procedure timer_write_time


    module procedure timer_write_dt
        if (dt > SECONDS_PER_HOUR) then
            dhms_block: block
                integer(ICLOCK) :: days, hours, minutes
                real(RCLOCK)    :: seconds
        
                call timer_get_time(dt, days, hours, minutes, seconds)
                call timer_write(days, hours, minutes, seconds, unit)
            end block dhms_block
        else
            s_block: block
                character(*), parameter :: LAYOUT="(1X,A,ES0.4,A)"

                write(UNIT=unit, FMT=LAYOUT) &
                     ELAPSED_TIME_HEAD,      &
                     dt, " sec(s)."
            end block s_block
        end if
    end procedure timer_write_dt


    module procedure timer_write_stamp_anywhere
        real(RCLOCK) :: dt

        dt = timer_elapsed_time(stamp_0, stamp_1)
        call timer_write(dt, unit)
    end procedure timer_write_stamp_anywhere


    module procedure timer_write_stamp_now
        real(RCLOCK) :: dt

        dt = timer_elapsed_time(stamp_0)
        call timer_write(dt, unit)
    end procedure timer_write_stamp_now


    module procedure timer_print_time
        call timer_write(days, hours, minutes, seconds, io_env_outunit)
    end procedure timer_print_time


    module procedure timer_print_dt
        call timer_write(dt, io_env_outunit)
    end procedure timer_print_dt


    module procedure timer_print_stamp_anywhere
        call timer_write(stamp_0, stamp_1, io_env_outunit)
    end procedure timer_print_stamp_anywhere


    module procedure timer_print_stamp_now
        call timer_write(stamp_0, io_env_outunit)
    end procedure timer_print_stamp_now
end submodule scifort_timer_smod
