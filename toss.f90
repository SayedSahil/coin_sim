program toss
implicit none

integer:: i,j,k,l, count

real(16):: t
integer, parameter:: trials =100000000

integer, parameter:: tosses = 8
integer :: trial(1:tosses)

! random_number(t)
count = 0
	do i = 0, trials
	trial = 0
		do j = 1,tosses
		call random_number(t)
		if (t.ge.0.50d0) then
		 	trial(j) = 1
		 else
		 	trial(j) = 0

		 end if
		 end do

		 do j=1,tosses-1
		 if (trial(j).ne.1 .and. trial(j+1).ne.1)then
		 count = count+1
		 print*, count
		 EXIT
		 end if
		end do
	end do
print*, 1.0d0*count/trials

end program toss