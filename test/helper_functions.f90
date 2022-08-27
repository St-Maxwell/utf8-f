module utf8_test_helper_function
    use testdrive
    use utf8
    implicit none
    private
    public :: collect_numbytes_codepoint

contains

    subroutine collect_numbytes_codepoint(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("numbytes_codepoint_1", numbytes_codepoint_1), &
                    new_unittest("numbytes_codepoint_2", numbytes_codepoint_2), &
                    new_unittest("numbytes_codepoint_3", numbytes_codepoint_3), &
                    new_unittest("numbytes_codepoint_4", numbytes_codepoint_4), &
                    new_unittest("numbytes_codepoint_5", numbytes_codepoint_5), &
                    new_unittest("numbytes_codepoint_6", numbytes_codepoint_6), &
                    new_unittest("numbytes_codepoint_7", numbytes_codepoint_7), &
                    new_unittest("numbytes_codepoint_8", numbytes_codepoint_8) &
        ]

    end subroutine collect_numbytes_codepoint

    subroutine numbytes_codepoint_1(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        character(kind=c_char, len=*), parameter :: str = "M" ! U+004D

        call check(error, codepoint_num_bytes(cast_byte(str(1:1))), 1)

    end subroutine numbytes_codepoint_1

    subroutine numbytes_codepoint_2(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        character(kind=c_char, len=*), parameter :: str = "а" ! Cyrillic Small Letter A (U+0430)

        call check(error, codepoint_num_bytes(cast_byte(str(1:1))), 2)

    end subroutine numbytes_codepoint_2

    subroutine numbytes_codepoint_3(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        character(kind=c_char, len=*), parameter :: str = "二" ! U+4E8C

        call check(error, codepoint_num_bytes(cast_byte(str(1:1))), 3)

    end subroutine numbytes_codepoint_3

    subroutine numbytes_codepoint_4(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        character(kind=c_char, len=*), parameter :: str = "𐌂" ! Old Italic Letter Ke (U+10302)

        call check(error, codepoint_num_bytes(cast_byte(str(1:1))), 4)

    end subroutine numbytes_codepoint_4

    !! test some limit cases

    subroutine numbytes_codepoint_5(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, codepoint_num_bytes(int(z'80',1)), 1)

    end subroutine numbytes_codepoint_5

    subroutine numbytes_codepoint_6(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, codepoint_num_bytes(int(z'C1',1)), 1)

    end subroutine numbytes_codepoint_6

    subroutine numbytes_codepoint_7(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, codepoint_num_bytes(int(z'F5',1)), 1)

    end subroutine numbytes_codepoint_7

    subroutine numbytes_codepoint_8(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, codepoint_num_bytes(int(z'FF',1)), 1)

    end subroutine numbytes_codepoint_8

end module utf8_test_helper_function
