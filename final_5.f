c234567
	program final_5
	implicit none
	
	integer a(0:2), input, b(0:2), ball, stk
	integer i,j,dum,outt
	data a(0),a(1),a(2)/1,3,5/
	
	print *, 'Input the number'
	read *, input
	
	do i=2,0,-1
		do j=0,9
			If (input/(10**i) .EQ. j) then
				b(i)=j
				input=input-j*(10**i)
			end if
		end do
	end do
	
	stk=0
	ball=0
	outt=0
	
	do i=0,2
		If (a(i) .EQ. b(i)) then
			stk=stk+1
		end if
		
		do j=0,2
			If (a(i) .EQ. b(j)) then
				ball=ball+1
			end if
		end do
		
	end do
	
	ball=ball-stk
	
	If ((stk .EQ. 0) .AND. (ball .EQ. 0)) then
		print *, 'Out'
		outt=1
	else 
		print *, stk, 'Strike'
		print *, ball, 'Ball'
	end if

c posible way to find answer
	If (outt .EQ. 1) then
		print *, 'The ways you can go is 216'
	else
		if (stk .EQ. 0) then
			if (ball .EQ. 1) then
				print *, 'The way you can go is'
			else if (ball .EQ. 2) then
				print *, 'The way you can go is'
			else
				print *, 'The way you can go is'
			end if
			
		else if (stk .EQ. 1) then
			if (ball .EQ. 0) then
				print *, 'The way you can go is'
			else if (ball .EQ. 1) then
				print *, 'The way you can go is '
			else
				print *, 'The way you can go is '
			end if
	
		else if (stk .EQ. 2) then
			print *, 'The way you can go is 21'
			
		else
			print *, 'Correct answer'
			
		end if	
	end if
	
	stop
	end
	
