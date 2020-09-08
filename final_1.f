c234567
	program final_1
	implicit none
	
	real x(10000),y(10000),r(10000)
	real*8 out1, out2, out3
	integer i, j, nn, cnt
	
	nn=10000
	cnt=0
	
	open (900, file='final_1_data.dat')
	
	do i=1,nn
		read (900,'(F8.6,3X,F8.6)') x(i), y(i)
		r(i)=sqrt(x(i)**2+y(i)**2)
		If (r(i) .LT. 1.) then
			cnt=cnt+1
		end if
	end do
	
	
	cnt=4*cnt
	
	print *, 'The number in the circle is', cnt
	
	out1=real(cnt)/real(nn)
	
	print *, 'The pi is', out1
	
	out2=0.
	do i=1,nn
		out2=out2+(1/real(i)**2)
	end do
	
	out2=sqrt(out2*6.)
	
	print *, 'The pi is', out2
	
	out3=0.
	do i=-nn,nn,1
		out3=out3+exp(-real(i)**2)
	end do
	out3=(out3)**2
	
	print *, 'The pi is', out3
	
	stop
	end
		
			
	

