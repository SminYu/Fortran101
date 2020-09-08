c234567
      program hw2
      implicit none
    
      real ar1(100), ar2(100), ar3(100), ar4(100), ar5(100), ar6(100)
      real a1, a2, a3, mean, std, maxi, mini
      real medi, nth, uppq, lowq
      integer i, m, num, fnc

	m=100

      do i=1,m
	 ar1(i)=i
	end do

	do i=1,m
	 ar2(i)=i
	end do

	fnc=0
	print *, 'Choose the function you want'
	print *, '1 : Statistics of array'
	print *, '2 : Quardratic equation solve'
	print *, '3 : Array calculation'
	print *, '4 : Digit counter'
	read (*,*) fnc

	If (fnc .EQ. 1) then
	 call statistics(ar1, m, mean, std, maxi, 
     ~mini, medi, nth, uppq, lowq)
	 print *, 'mean =', mean
	 print *, 'standard deviation=', std
	 print *, 'maximum =', maxi
	 print *, 'minimum =', mini
	 print *, 'median =', medi
	 print *, 'The n-th number is', nth
	 print *, 'The upper quartile is', uppq
	 print *, 'The lower quartile is', lowq


	else if (fnc .EQ. 2) then
	 print *, 'Quardratic equation (ax^2+bx+c) solve'
	 print *, 'Please put real numbers of a, b, c'
	 read (*,*) a1, a2, a3
	 call eqsol(a1,a2,a3)
	
	else if (fnc .EQ. 3) then
	 call summation(ar1, ar2, ar3, ar4, ar5, ar6, m)

	else if (fnc .EQ. 4) then
	 print *, 'Digits for a natural number counter'
	 print *, 'Please put the integer'
	 read (*,*) num
	 call digit(num)
	else
	end if

	stop
	end

c-----subroutine statistics

      subroutine statistics(array, nn, mean, std, maxi,
     ~mini, medi, nth, uppq, lowq)
      integer nn, i, j
      real array(nn), mean, std, maxi, mini, medi, nth, dum
	real uppq, lowq

c-----mean
      mean = 0.

      do i=1, nn
       mean = mean+array(i)
	end do 

      mean = mean / nn

c-----std
      std = 0.

      do i=1, nn
       std = std + (array(i)-mean)**2
	end do

      std = sqrt(std/nn)

c-----maxi and mini, medi, quartile
c-----sort and get the value
      do i=1,nn
       do j=1,nn
        if (array(i) .GE. array(j)) then
         dum = array(i)
         array(i)=array(j)
         array(j)=dum
        end if
       end do	
      end do
      
	maxi=array(1)
	mini=array(nn)
	medi=array(nn/2)
	uppq=array(nn/4)
	lowq=array(nn*(3/4))

      return
      end	

c-----the solutions of the quadratic equation
c-----ax^2+bx+c

	subroutine eqsol(a,b,c)
      real a,b,c,x1,x2,d

      d = b**2-4*a*c
      
      if(d .LT. 0) then
       write (*,*) 'Discriment is', d
       write (*,*) 'Complex root x are'
       write (*,*) (-b)/(2*a), '+', sqrt((-d))/(2*a), 'i'
       write (*,*) (-b)/(2*a), '-', sqrt((-d))/(2*a), 'i'

       else if(d .EQ. 0) then
       x1=(-b)/(2*a)
       write (*,*) 'Discriment is zero'
       write (*,*) 'multiple root x is', x1

       else
       x1=(-b+sqrt(d))/(2*a)
       x2=(-b-sqrt(d))/(2*a)
       write (*,*) 'Discriment is', d
       write (*,*) 'the root x are', x1, 'and', x2

      end if
	
	return
	end


c-----summation of array

	subroutine summation(array1, array2, array3, array4, array5, array6, mm)
	integer i, mm
	real array1(mm), array2(mm), array3(mm), array4(mm), array5(mm), array6(mm)
	
	do i=1, mm
	 array3(i)=array1(i)+array2(i)
	 array4(i)=array1(i)-array2(i)
	 array5(i)=array1(i)*array2(i)
	 array6(i)=array1(i)/array2(i)
	end do

	print *, 'A=', array1
	print *, 'B=', array2
	print *, 'Summation=', array3
	print *, 'Difference=', array4
	print *, 'Multiplication=', array5
	print *, 'Division=', array6

	return
	end

c-----digit count

	subroutine digit(numb)
	integer numb, i, dum, dum2, cnt, cnt2(0:9), n
	 
	dum=numb

	cnt=0
	do i=1, 40
		If (dum .GE. 10) then
			dum=dum/10
			cnt=i
		else
	 		goto 100
		end if
	end do
  100	continue
	
 	do i=0,9
	 cnt2(i)=0
	end do

	do i=cnt,0,-1
		if (numb .GT. 0) then
			dum2=numb/(10**i)
			do n=0,9
			if (dum2 .EQ. n) then
				cnt2(n)=cnt2(n)+1
	   			numb=numb-(dum2*(10**i))
	  		else
			end if
			end do
		else
	  	end if
	end do
	
	print *, 'The number of digits'
	do i=0,9,1
	 print *, i, '=', cnt2(i)
	end do
	
	return
	end
	
	


	 
	



