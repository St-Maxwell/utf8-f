# utf8-f
Prototype of UTF-8 manipulation in Fortran.


## Example
```fortran
use utf8
implicit none
type(utf8_string) :: s

call construct_utf8_string(s, "Fortran さいこう")
write (*, "('The number of code points: ',g0)") s%utf8_le()

block !! iterate all code points
    type(utf8_string_iterator) :: it
    it = s%iterator()
    do while (it%has_next())
        write (*, "(3A)") "'", it%get_next(), "'"
    end do
end block

write (*, "('The 10th code point is ',3A)") "'", s%utf8_a(10), "'."

call s%utf8_reverse()
write (*, "(A)") s%str

!! output:
!The number of code points: 12
!'F'
!'o'
!'r'
!'t'
!'r'
!'a'
!'n'
!' '
!'さ'
!'い'
!'こ'
!'う'
!The 10th code point is 'い'
!うこいさ nartroF
```

## To-do
* many methods for string type
* thinking about performance
