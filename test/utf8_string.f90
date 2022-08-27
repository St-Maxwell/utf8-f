module utf8_test_utf8_string
    use testdrive
    use utf8
    implicit none
    private
    public :: collect_utf8_len, collect_utf8_at, collect_utf8_reverse

contains

    subroutine collect_utf8_len(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("utf8_len_1", utf8_len_1), &
                    new_unittest("utf8_len_2", utf8_len_2), &
                    new_unittest("utf8_len_3", utf8_len_3), &
                    new_unittest("utf8_len_4", utf8_len_4), &
                    new_unittest("utf8_len_5", utf8_len_5) &
                    ]

    end subroutine collect_utf8_len

    subroutine utf8_len_1(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(utf8_string) :: s

        call construct_utf8_string(s, "Schroedinger's cat")
        call check(error, s%utf8_len(), 18)

    end subroutine utf8_len_1

    subroutine utf8_len_2(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(utf8_string) :: s

        call construct_utf8_string(s, "薛定谔的猫")
        call check(error, s%utf8_len(), 5)

    end subroutine utf8_len_2

    subroutine utf8_len_3(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(utf8_string) :: s

        call construct_utf8_string(s, "☉☽♀♂♁♃☿♄")
        call check(error, s%utf8_len(), 8)

    end subroutine utf8_len_3

    subroutine utf8_len_4(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(utf8_string) :: s

        call construct_utf8_string(s, "😂🐶🐱🍔🖥")
        call check(error, s%utf8_len(), 5)

    end subroutine utf8_len_4

    subroutine utf8_len_5(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(utf8_string) :: s

        call construct_utf8_string(s, "सद्धर्मपुण्डरीकसूत्र")
        call check(error, s%utf8_len(), 20)

    end subroutine utf8_len_5

    subroutine collect_utf8_at(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("utf8_at_1", utf8_at_1), &
                    new_unittest("utf8_at_2", utf8_at_2), &
                    new_unittest("utf8_at_3", utf8_at_3), &
                    new_unittest("utf8_at_4", utf8_at_4) &
                    ]

    end subroutine collect_utf8_at

    subroutine utf8_at_1(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(utf8_string) :: s

        call construct_utf8_string(s, "The Lady Is A Tramp")
        call check(error, s%utf8_at(8), "y")

    end subroutine utf8_at_1

    subroutine utf8_at_2(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(utf8_string) :: s

        call construct_utf8_string(s, "Ievan suu oli vehnäsellä")
        call check(error, s%utf8_at(24), "ä")

    end subroutine utf8_at_2

    subroutine utf8_at_3(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(utf8_string) :: s

        call construct_utf8_string(s, "沧海一声笑 滔滔两岸潮")
        call check(error, s%utf8_at(5), "笑")

    end subroutine utf8_at_3

    subroutine utf8_at_4(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(utf8_string) :: s

        call construct_utf8_string(s, "SOS　猿の惑星そう　結果 we are ape")
        call check(error, s%utf8_at(12), "結")

    end subroutine utf8_at_4

    subroutine collect_utf8_reverse(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("utf8_reverse_1", utf8_reverse_1), &
                    new_unittest("utf8_reverse_2", utf8_reverse_2), &
                    new_unittest("utf8_reverse_3", utf8_reverse_3), &
                    new_unittest("utf8_reverse_4", utf8_reverse_4), &
                    new_unittest("utf8_reverse_5", utf8_reverse_5) &
                    ]

    end subroutine collect_utf8_reverse

    subroutine utf8_reverse_1(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(utf8_string) :: s

        call construct_utf8_string(s, "Fortran")
        call s%utf8_reverse()
        call check(error, s%str, "nartroF")

    end subroutine utf8_reverse_1

    subroutine utf8_reverse_2(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(utf8_string) :: s

        call construct_utf8_string(s, "⏱")
        call s%utf8_reverse()
        call check(error, s%str, "⏱")

    end subroutine utf8_reverse_2

    subroutine utf8_reverse_3(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(utf8_string) :: s

        call construct_utf8_string(s, "🌒🌓🌔🌕🌖🌗🌘")
        call s%utf8_reverse()
        call check(error, s%str, "🌘🌗🌖🌕🌔🌓🌒")

    end subroutine utf8_reverse_3

    subroutine utf8_reverse_4(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(utf8_string) :: s

        call construct_utf8_string(s, "नमस्ते")
        call s%utf8_reverse()
        call check(error, s%str, "ेत्समन")

    end subroutine utf8_reverse_4

    subroutine utf8_reverse_5(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(utf8_string) :: s

        call construct_utf8_string(s, "σ迁移反应是一种σ键在π共轭体系中移动的反应")
        call s%utf8_reverse()
        call check(error, s%str, "应反的动移中系体轭共π在键σ种一是应反移迁σ")

    end subroutine utf8_reverse_5

end module utf8_test_utf8_string
