c234567
	program final_3
	implicit none
	
	integer ii, dum, i, j, nn, kk, cnt, k
	integer mode
	integer a(100)
	
	nn=100
	
	do i=1,nn
		a(i)=i
	end do

c 1)	
	ii=5
	dum=a(ii)
	do i=ii,nn-1
		a(i)=a(i+1)
	end do
	a(nn)=dum
	
c 2)
	do i=1,nn
		a(i)=i
	end do
	
	read *, kk
	
c round repeat
	dum=0
	cnt=0
	mode=0
	
	do k=1,nn
c each round	
	If (cnt .EQ. nn-1) goto 200
		do i=kk-mode,nn-cnt,kk
			print *, a(i)
			a(i)=0
c dead man counting
			cnt=cnt+1
		end do
		
		do i=kk-mode,nn-cnt,kk
			If (a(i) .EQ. 0) then
				do j=i,nn-1
					a(j)=a(j+1)
					a(nn)=0
				end do
			end if
		end do
		mode=mod(mode+mod(nn-cnt,kk),kk)
	end do
	
  200 continue
  	
	print *, 'The last standing is', a(1)
		
	stop
	end
	
