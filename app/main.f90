program main
    use utf8
    use iso_c_binding, only: c_char
    implicit none
    type(utf8_string) :: s
    character(len=:, kind=c_char), allocatable :: c

    call construct_utf8_string(s, "Fortran さいこう")

    write (*, "('The number of code points: ',g0)") utf8_len(s)

    block !! iterate all code points
        type(utf8_string_iterator) :: it
        it = s%iterator()

        do while (it%has_next())
            write (*, "(3A)") "'", it%get_next(), "'"
        end do
    end block

    write (*, "('The 10th code point is ',3A)") "'", utf8_at(s,10), "'."

    call utf8_reverse(s)
    c = s
    write (*, "(A)") c

end program main
