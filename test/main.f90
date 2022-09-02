program utf8_test
    use testdrive
    use iso_fortran_env
    use utf8_test_helper_function
    use utf8_test_utf8_string
    implicit none
    integer :: stat, is
    type(testsuite_type), dimension(:), allocatable :: testsuites
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
                 new_testsuite("test_numbytes_codepoint", collect_numbytes_codepoint), &
                 new_testsuite("test_utf8_len", collect_utf8_len), &
                 new_testsuite("test_utf8_at", collect_utf8_at), &
                 new_testsuite("test_utf8_reverse", collect_utf8_reverse), &
                 new_testsuite("test_utf8_slice", collect_utf8_slice), &
                 new_testsuite("test_utf8_index", collect_utf8_index) &
                 ]

    do is = 1, size(testsuites)
        write (error_unit, fmt) "Testing:", testsuites(is)%name
        call run_testsuite(testsuites(is)%collect, error_unit, stat)
    end do

    if (stat > 0) then
        write (error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
        error stop
    end if

end program utf8_test
