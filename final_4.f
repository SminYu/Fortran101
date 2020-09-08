c234567
	program final_4
	implicit none
	
	integer*8 input, i, j, nn, output
	integer*8 dum, a(0:30)
	
	print *, 'Input the number'
	read *, input
	
	print *, 'Input the n for n-th notation'
	read *, nn
	
	a=0
	dum=input
	
	do i=20,0,-1
		do j=1,nn-1
		If (dum/(nn**i) .EQ. j) then
			a(i)=j
			dum=dum-(j*(nn**i))
		end if
		end do
	end do
	
	output=0
	
	do i=0,20,1
		output=output+(a(i)*(10**i))
	end do
	
	print *, 'The input is', input
	print *, 'The notation is', nn
	print *, 'The output is', output
	
	stop
	end
	
	
