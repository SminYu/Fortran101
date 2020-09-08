c234567
	program final_2
	implicit none
	
	real dum, amid, afin, Acf, Bcf, A, B, Acf2
	real mid(200), fin(200), avg(200), third(200)
	real dum2, dum3
	integer nn, i, j
	character score(200)
	
	nn=200
	
	open (900,file='final_2_score.dat')
c read and get the avg.	
	do i=1,nn
		read (900,'(F7.4,3X,F7.4)') mid(i), fin(i)
		avg(i)=(mid(i)+fin(i))/2.
	end do
c sorting	
	do i=1,nn
		do j=1,nn
			If (avg(i) .GT. avg(j)) then
				dum=avg(i)
				avg(i)=avg(j)
				avg(j)=dum
				
				dum2=mid(i)
				mid(i)=mid(j)
				mid(j)=dum2
				
				dum3=fin(i)
				fin(i)=fin(j)
				fin(j)=dum3
			end if
		end do
	end do
	
c scoring	
	do i=1,nn
		If (i .LE. nn/10.) then
			score(i)='A'
		else if (i .LE. nn*(3./10.)) then
			score(i)='B'
		else if (i .LE. nn/2.) then
			score(i)='C'
		else if (i .LE. nn*(8./10.)) then
			score(i)='D'
		else
			score(i)='F'
		end if
	end do
	
c avg of mid and fin test
	amid=0.
	afin=0.
	do i=1,nn
		amid=amid+(mid(i)/real(nn))
		afin=afin+(fin(i)/real(nn))
	end do
	
	acf=0.
	acf2=0.
c linear fnc
	do i=1,nn
		Acf=Acf+((mid(i)-amid)*(fin(i)-afin))
	end do
	
	do i=1,nn
		Acf2=Acf2+(mid(i)-amid)**2
	end do
	
	Acf=Acf/Acf2
	Bcf=afin-(Acf*amid)
	print *, Acf, Bcf
	
	
	
c subroutine using to predict third test
	call prediction(third, nn, mid, acf, bcf)
c open the file to write	
	open (901, file='final_2_score2.dat')
c write on the file
	do i=1,nn
		write(901, *) mid(i), fin(i), avg(i), i, score(i), third(i)
	end do	
		
	stop
	end
	

	subroutine prediction(third, nn, avg, a, b)
	integer nn, i
	real third(nn), avg(nn), A, B
	
	do i=1,nn
		third(i)=A*avg(i)+B
	end do
	
	return
	end
	
	
	

	

		
		
		
		
		
		
		
			
