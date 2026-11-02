program monte_carlo_pi
implicit none

integer, parameter :: n = 10000000
integer :: i, inside, j
real (8) :: x, y, pi, avg, total

inside = 0

total = 0 

call random_seed()

do i = 1, 4
    
    inside = 0
    
do j =  1, n


    call random_number(x)
    call random_number(y)

    if (x * x + y * y <= 1.0_8) then 
        inside = inside + 1 
    end if

end do

    pi = 4.0_8 * real(inside,8) / real(n,8)

 total = total + pi

    print *, "attempt", i 
    print *, "Total points: ", n
    print *, "Points inside: ", inside
    print *, "Calculated pi: ", pi
    print *, ""
    
end do

avg = total / 4

print *, "Average pi is", avg

print '(A, F12.8)', "Percentage error (%): ", &
    (abs(3.141592653589793_8 - avg) / 3.141592653589793_8) * 100.0_8

read *,

end program monte_carlo_pi
