module utf8
    use iso_c_binding, only: c_char, c_int8_t
    implicit none
    !private
    !public :: utf8_string

    type :: utf8_string
        !private
        character(len=:, kind=c_char), allocatable :: str
    contains
        procedure :: len => len_utf8_string
        procedure :: utf8_len => utf8_len_utf8_string
        procedure :: byte_at
        procedure :: utf8_at
        procedure :: utf8_slice
        procedure :: utf8_index
        procedure :: utf8_reverse
    end type

    interface construct_utf8_string
        module procedure :: construct_utf8_string_from_string
    end interface

    !> refer to Table 3-7. Well-Formed UTF-8 Byte Sequences
    !> in http://www.unicode.org/versions/Unicode10.0.0/UnicodeStandard-10.0.pdf
    integer, dimension(0:255), parameter :: NUM_BYTES_UTF8 = [ &
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, & ! 0x00..0x0F
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, & ! 0x10..0x1F
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, & ! 0x20..0x2F
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, & ! 0x30..0x3F
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, & ! 0x40..0x4F
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, & ! 0x50..0x5F
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, & ! 0x60..0x6F
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, & ! 0x70..0x7F
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, & ! 0x80..0x8F
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, & ! 0x90..0x9F
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, & ! 0xA0..0xAF
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, & ! 0xB0..0xBF
        0, 0, & ! 0xC0..0xC1 - disallowed in UTF-8
        2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, & ! 0xC2..0xCF
        2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, & ! 0xD0..0xDF
        3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, & ! 0xE0..0xEF
        4, 4, 4, 4, 4, & ! 0xF0..0xF4
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 & ! 0xF5..0xFF - disallowed in UTF-8
        ]

contains

    subroutine construct_utf8_string_from_string(this, string)
        type(utf8_string), intent(out) :: this
        character(len=*), intent(in) :: string

        allocate(this%str, source=string)

    end subroutine construct_utf8_string_from_string

    !> return the number of raw bytes
    pure function len_utf8_string(this) result(l)
        class(utf8_string), intent(in) :: this
        integer :: l

        l = len(this%str)

    end function len_utf8_string

    !> return the number of UTF-8 code points
    pure function utf8_len_utf8_string(this) result(l)
        class(utf8_string), intent(in) :: this
        integer :: l
        integer :: i

        l = 0; i = 1
        do
            if (i > this%len()) exit
            i = i + codepoint_num_bytes(cast_byte(this%str(i:i)))
            l = l + 1
        end do

    end function utf8_len_utf8_string

    !> return the byte at specified position
    pure function byte_at(this, idx) result(s)
        class(utf8_string), intent(in) :: this
        integer, intent(in) :: idx
        character(len=1, kind=c_char) :: s

        s = this%str(idx:idx)

    end function byte_at

    !> return the code point at specified position
    pure function utf8_at(this, idx) result(s)
        class(utf8_string), intent(in) :: this
        integer, intent(in) :: idx
        character(len=:, kind=c_char), allocatable :: s
        integer :: i, j, n

        i = 1; j = 1
        do
            if (i > this%len()) exit
            n = codepoint_num_bytes(cast_byte(this%str(i:i)))
            if (j == idx) then
                allocate(s, source=this%str(i:i+n-1))
                exit
            end if
            i = i + n
            j = j + 1
        end do

    end function utf8_at

    pure function utf8_slice(this, begin, end) result(substr)
        class(utf8_string), intent(in) :: this
        integer, intent(in) :: begin
        integer, intent(in) :: end
        character(len=:, kind=c_char), allocatable :: substr
    end function utf8_slice

    pure function utf8_index(this, substring) result(idx)
        class(utf8_string), intent(in) :: this
        character(len=*, kind=c_char), intent(in) :: substring
        integer :: idx
    end function utf8_index

    subroutine utf8_reverse(this)
        class(utf8_string), intent(inout) :: this
        character(len=:, kind=c_char), allocatable :: tmp
        integer :: i, j, l, n
    
        l = this%len()
        call move_alloc(from=this%str, to=tmp)
        allocate(character(len=l, kind=c_char) :: this%str)

        i = 1; j = l
        do
            if (i > l) exit
            n = codepoint_num_bytes(cast_byte(tmp(i:i)))
            this%str(j-n+1:j) = tmp(i:i+n-1)
            i = i + n
            j = j - n
        end do

    end subroutine utf8_reverse

!> private helper function

    !> cast char to byte (8-bits integer in Fortran)
    pure function cast_byte(char) result(byte)
        character(kind=c_char, len=1), intent(in) :: char
        integer(kind=c_int8_t) :: byte

        byte = transfer(char, byte)

    end function cast_byte

    !> get the number of bytes of a code point based on its first byte
    pure function codepoint_num_bytes(byte) result(n)
        integer(kind=c_int8_t), intent(in) :: byte
        integer :: n

        n = NUM_BYTES_UTF8(iand(int(byte, 4), int(z'000000FF', 4)))
        if (n == 0) n = 1

    end function codepoint_num_bytes

end module utf8
