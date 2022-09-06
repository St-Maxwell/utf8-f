module utf8_test_utf8_string
    use testdrive
    use utf8, i1 => c_int8_t
    implicit none
    private
    public :: collect_utf8_len, collect_utf8_at, collect_utf8_reverse, &
              collect_utf8_slice, collect_utf8_index, collect_utf8_is_valid

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


    subroutine collect_utf8_slice(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("utf8_slice_1", utf8_slice_1), &
                    new_unittest("utf8_slice_2", utf8_slice_2), &
                    new_unittest("utf8_slice_3", utf8_slice_3), &
                    new_unittest("utf8_slice_4", utf8_slice_4), &
                    new_unittest("utf8_slice_5", utf8_slice_5) &
                    ]

    end subroutine collect_utf8_slice

    subroutine utf8_slice_1(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(utf8_string) :: s

        call construct_utf8_string(s, "一二三四五六七八九十")
        call check(error, s%utf8_slice(3, 5), "三四五")

    end subroutine utf8_slice_1

    subroutine utf8_slice_2(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(utf8_string) :: s

        call construct_utf8_string(s, "一二三四五六七八九十")
        call check(error, s%utf8_slice(0, 2), "一二")

    end subroutine utf8_slice_2

    subroutine utf8_slice_3(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(utf8_string) :: s

        call construct_utf8_string(s, "一二三四五六七八九十")
        call check(error, s%utf8_slice(8, 11), "八九十")

    end subroutine utf8_slice_3

    subroutine utf8_slice_4(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(utf8_string) :: s

        call construct_utf8_string(s, "一二三四五六七八九十")
        call check(error, s%utf8_slice(5,2), "")

    end subroutine utf8_slice_4

    subroutine utf8_slice_5(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(utf8_string) :: s

        call construct_utf8_string(s, "नमस्ते")
        call check(error, s%utf8_slice(3,5), "स्त")

    end subroutine utf8_slice_5


    subroutine collect_utf8_index(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("utf8_index_1", utf8_index_1), &
                    new_unittest("utf8_index_2", utf8_index_2), &
                    new_unittest("utf8_index_3", utf8_index_3), &
                    new_unittest("utf8_index_4", utf8_index_4) &
                    ]

    end subroutine collect_utf8_index

    subroutine utf8_index_1(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(utf8_string) :: s

        call construct_utf8_string(s, "一二三四五六七八九十")
        call check(error, s%utf8_index("三四五"), 3)

    end subroutine utf8_index_1

    subroutine utf8_index_2(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(utf8_string) :: s

        call construct_utf8_string(s, "नमस्ते")
        call check(error, s%utf8_index("स्त"), 3)

    end subroutine utf8_index_2

    subroutine utf8_index_3(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(utf8_string) :: s

        call construct_utf8_string(s, "一二三四五六七八九十")
        call check(error, s%utf8_index("123"), 0)

    end subroutine utf8_index_3

    subroutine utf8_index_4(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(utf8_string) :: s

        call construct_utf8_string(s, "abcd")
        call check(error, s%utf8_index("ａｂ"), 0)

    end subroutine utf8_index_4


    subroutine collect_utf8_is_valid(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("utf8_is_valid_1", utf8_is_valid_1), &
                    new_unittest("utf8_is_valid_2", utf8_is_valid_2), &
                    new_unittest("utf8_is_valid_3", utf8_is_valid_3), &
                    new_unittest("utf8_is_valid_4", utf8_is_valid_4), &
                    new_unittest("utf8_is_valid_5", utf8_is_valid_5), &
                    new_unittest("utf8_is_valid_6", utf8_is_valid_6), &
                    new_unittest("utf8_is_valid_7", utf8_is_valid_7), &
                    new_unittest("utf8_is_valid_8", utf8_is_valid_8), &
                    new_unittest("utf8_is_valid_9", utf8_is_valid_9), &
                    new_unittest("utf8_is_valid_10", utf8_is_valid_10), &
                    new_unittest("utf8_is_valid_11", utf8_is_valid_11), &
                    new_unittest("utf8_is_valid_12", utf8_is_valid_12), &
                    new_unittest("utf8_is_valid_13", utf8_is_valid_13), &
                    new_unittest("utf8_is_valid_14", utf8_is_valid_14), &
                    new_unittest("utf8_is_valid_15", utf8_is_valid_15), &
                    new_unittest("utf8_is_valid_16", utf8_is_valid_16), &
                    new_unittest("utf8_is_valid_17", utf8_is_valid_17), &
                    new_unittest("utf8_is_valid_18", utf8_is_valid_18), &
                    new_unittest("utf8_is_valid_19", utf8_is_valid_19), &
                    new_unittest("utf8_is_valid_20", utf8_is_valid_20) &
                    ]

    end subroutine collect_utf8_is_valid

    subroutine utf8_is_valid_1(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(utf8_string) :: s
        character(len=1, kind=c_char) :: raw

        !                                  x80
        raw = transfer([integer(kind=i1)::-128],raw)

        call construct_utf8_string(s, raw)
        call check(error, s%utf8_is_valid(), .false.)

    end subroutine utf8_is_valid_1

    subroutine utf8_is_valid_2(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(utf8_string) :: s
        character(len=2, kind=c_char) :: raw

        !                                 xFF x00
        raw = transfer([integer(kind=i1)::-1,0],raw)

        call construct_utf8_string(s, raw)
        call check(error, s%utf8_is_valid(), .false.)

    end subroutine utf8_is_valid_2

    subroutine utf8_is_valid_3(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(utf8_string) :: s
        character(len=3, kind=c_char) :: raw

        !                                 xC2 xFE x00
        raw = transfer([integer(kind=i1)::-62,-2,0],raw)

        call construct_utf8_string(s, raw)
        call check(error, s%utf8_is_valid(), .false.)

    end subroutine utf8_is_valid_3

    subroutine utf8_is_valid_4(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(utf8_string) :: s
        character(len=3, kind=c_char) :: raw

        !                                 xC2 x7F x00
        raw = transfer([integer(kind=i1)::-62,127,0],raw)

        call construct_utf8_string(s, raw)
        call check(error, s%utf8_is_valid(), .false.)

    end subroutine utf8_is_valid_4

    subroutine utf8_is_valid_5(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(utf8_string) :: s
        character(len=1, kind=c_char) :: raw

        !                                 xE0
        raw = transfer([integer(kind=i1)::-32],raw)

        call construct_utf8_string(s, raw)
        call check(error, s%utf8_is_valid(), .false.)

    end subroutine utf8_is_valid_5

    subroutine utf8_is_valid_6(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(utf8_string) :: s
        character(len=4, kind=c_char) :: raw

        !                                 xE0 xA7 xC0 x00
        raw = transfer([integer(kind=i1)::-32,-89,-64,0],raw)

        call construct_utf8_string(s, raw)
        call check(error, s%utf8_is_valid(), .false.)

    end subroutine utf8_is_valid_6

    subroutine utf8_is_valid_7(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(utf8_string) :: s
        character(len=4, kind=c_char) :: raw

        !                                 xE0 xFF xFF xFF
        raw = transfer([integer(kind=i1)::-32,-1,-1,-1],raw)

        call construct_utf8_string(s, raw)
        call check(error, s%utf8_is_valid(), .false.)

    end subroutine utf8_is_valid_7

    subroutine utf8_is_valid_8(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(utf8_string) :: s
        character(len=4, kind=c_char) :: raw

        !                                 xED x71 xA7 x00
        raw = transfer([integer(kind=i1)::-19,113,-89,0],raw)

        call construct_utf8_string(s, raw)
        call check(error, s%utf8_is_valid(), .false.)

    end subroutine utf8_is_valid_8

    subroutine utf8_is_valid_9(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(utf8_string) :: s
        character(len=4, kind=c_char) :: raw

        !                                 xED xA0 xFF xFF
        raw = transfer([integer(kind=i1)::-19,-96,-1,-1],raw)

        call construct_utf8_string(s, raw)
        call check(error, s%utf8_is_valid(), .false.)

    end subroutine utf8_is_valid_9

    subroutine utf8_is_valid_10(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(utf8_string) :: s
        character(len=4, kind=c_char) :: raw

        !                                 xE0 xA7 xA7 x00
        raw = transfer([integer(kind=i1)::-32,-89,-89,0],raw)

        call construct_utf8_string(s, raw)
        call check(error, s%utf8_is_valid(), .true.)

    end subroutine utf8_is_valid_10

    subroutine utf8_is_valid_11(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(utf8_string) :: s
        character(len=1, kind=c_char) :: raw

        !                                 xED
        raw = transfer([integer(kind=i1)::-19],raw)

        call construct_utf8_string(s, raw)
        call check(error, s%utf8_is_valid(), .false.)

    end subroutine utf8_is_valid_11

    subroutine utf8_is_valid_12(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(utf8_string) :: s
        character(len=1, kind=c_char) :: raw

        !                                 xF0
        raw = transfer([integer(kind=i1)::-16],raw)

        call construct_utf8_string(s, raw)
        call check(error, s%utf8_is_valid(), .false.)

    end subroutine utf8_is_valid_12

    subroutine utf8_is_valid_13(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(utf8_string) :: s
        character(len=1, kind=c_char) :: raw

        !                                 xF4
        raw = transfer([integer(kind=i1)::-12],raw)

        call construct_utf8_string(s, raw)
        call check(error, s%utf8_is_valid(), .false.)

    end subroutine utf8_is_valid_13

    subroutine utf8_is_valid_14(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(utf8_string) :: s
        character(len=5, kind=c_char) :: raw

        !                                 xF4 x90  x90  x90  x00
        raw = transfer([integer(kind=i1)::-12,-112,-112,-112,0],raw)

        call construct_utf8_string(s, raw)
        call check(error, s%utf8_is_valid(), .false.)

    end subroutine utf8_is_valid_14

    subroutine utf8_is_valid_15(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(utf8_string) :: s
        character(len=5, kind=c_char) :: raw

        !                                 xF0 x8F  x91  xB5 x00
        raw = transfer([integer(kind=i1)::-16,-113,-111,-75,0],raw)

        call construct_utf8_string(s, raw)
        call check(error, s%utf8_is_valid(), .false.)

    end subroutine utf8_is_valid_15

    subroutine utf8_is_valid_16(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(utf8_string) :: s
        character(len=5, kind=c_char) :: raw

        !                                 xF0 xC7 x91  xB5 x00
        raw = transfer([integer(kind=i1)::-16,-57,-111,-75,0],raw)

        call construct_utf8_string(s, raw)
        call check(error, s%utf8_is_valid(), .false.)

    end subroutine utf8_is_valid_16

    subroutine utf8_is_valid_17(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(utf8_string) :: s
        character(len=5, kind=c_char) :: raw

        !                                 xF4 x7F x91  xB5 x00
        raw = transfer([integer(kind=i1)::-12,127,-111,-75,0],raw)

        call construct_utf8_string(s, raw)
        call check(error, s%utf8_is_valid(), .false.)

    end subroutine utf8_is_valid_17

    subroutine utf8_is_valid_18(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(utf8_string) :: s
        character(len=5, kind=c_char) :: raw

        !                                 xF4 x92  x91  xB5 x00
        raw = transfer([integer(kind=i1)::-12,-110,-111,-75,0],raw)

        call construct_utf8_string(s, raw)
        call check(error, s%utf8_is_valid(), .false.)

    end subroutine utf8_is_valid_18

    subroutine utf8_is_valid_19(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(utf8_string) :: s
        character(len=2, kind=c_char) :: raw

        !                                 xF4 x92
        raw = transfer([integer(kind=i1)::-12,-110],raw)

        call construct_utf8_string(s, raw)
        call check(error, s%utf8_is_valid(), .false.)

    end subroutine utf8_is_valid_19

    subroutine utf8_is_valid_20(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(utf8_string) :: s
        character(len=3, kind=c_char) :: raw

        !                                 xF4 x92  x91
        raw = transfer([integer(kind=i1)::-12,-110,-111],raw)

        call construct_utf8_string(s, raw)
        call check(error, s%utf8_is_valid(), .false.)

    end subroutine utf8_is_valid_20


end module utf8_test_utf8_string
