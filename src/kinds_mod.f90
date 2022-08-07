module scifort_kinds_mod
    use, intrinsic :: iso_fortran_env, only: &
         I32 => INT32, I64 => INT64, R32 => REAL32, R64 => REAL64

    implicit none

    integer, parameter :: IK=I32
    integer, parameter :: RK=R64
end module scifort_kinds_mod
