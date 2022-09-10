module utf8_const
    use iso_c_binding, only: c_char, c_int8_t, c_int32_t
    implicit none
    public

    integer, parameter :: utf8_ok = 0
    integer, parameter :: utf8_invalid_byte = 1
    integer, parameter :: utf8_invalid_unicode_hex = 2
    integer, parameter :: utf8_invalid_unicode_surrogate = 3

    !> refer to Table 3-7. Well-Formed UTF-8 Byte Sequences
    !> in http://www.unicode.org/versions/Unicode10.0.0/UnicodeStandard-10.0.pdf
    !> display: private
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

end module utf8_const