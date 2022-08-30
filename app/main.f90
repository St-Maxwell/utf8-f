program main
    use utf8
    implicit none
    type(utf8_string) :: s

    call construct_utf8_string(s, "Fortran さいこう")

    write (*, "('The number of code points: ',g0)") s%utf8_len()

    block !! iterate all code points
        type(utf8_string_iterator) :: it
        it = s%iterator()

        do while (it%has_next())
            write (*, "(3A)") "'", it%get_next(), "'"
        end do
    end block

    write (*, "('The 10th code point is ',3A)") "'", s%utf8_at(10), "'."

    call s%utf8_reverse()
    write (*, "(A)") s%str

end program main
