program main
    use utf8
    implicit none
    type(utf8_string) :: s
    integer :: i

    call construct_utf8_string(s, "Fortran さいこう")
    do i = 1, s%utf8_len()
        write(*,"(3A)") "'",s%utf8_at(i),"'"
    end do
    call s%utf8_reverse()
    write(*,"(A)") s%str

end program main
