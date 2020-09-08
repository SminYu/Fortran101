c234567
      program ftn

      implicit none
      real array(100), average, mean, dev, std, mini, maxi
      real minimum, maximum, quartile, sort, sorting
      real med, median
      integer i, numb

      do 10 i = 1,100
       array(i) = real(i)       
   10 continue
      
      print *, 'Type the N value for the N-th number'
      read (*,*) numb
      
      i=100
      average = mean(array, i)
      dev = std(array, i)
      minimum = mini(array, i)
      maximum = maxi(array, i)
      sorting = sort(array, i, numb)
      med = median(array, i)

      print *, '-----------------'
      print *, 'Average =', average
      print *, 'Median = ', med
      print *, 'Standard deviation =', dev
      print *, 'Minimum =', minimum 
      print *, 'Maximum =', maximum
      print *, 'First quartile =', quartile(array, 100, 1)
      print *, 'Second quartile =', quartile(array, 100, 2)
      print *, 'Third quartile =', quartile(array, 100, 3)
      print *, 'The N-th number =', sorting

      stop
      end

c---------- function of mean -------

      real function mean(array, nn)
      integer i
      real array(nn), mm

      mm = 0.
      do 20 i=1,nn
       mm = mm+array(i)
   20 continue
      
      mean = mm/nn

      return
      end

c---------- functin of std. dev. -----

      real function std(array,nn)
      integer i
      real array(nn), mm, mean

      mm = mean(array, nn)
      std = 0.
      do 30 i=1,nn
       std = std + (array(i)-mm)**2
  30  continue
 
      std = sqrt(std/nn)

      return
      end

c---------- function of minumum --------

      real function mini(array,nn)
      integer i
      real array(nn), mm

      mm = array(1)
      do i=1,nn
       if (array(i) .LT. mm) then
        mm=array(i)
       else
       end if
      end do

      mini = mm

      return
      end
      
c----------- function of maximum --------

      real function maxi(array,nn)
      integer i
      real array(nn), mm

      mm = array(1)
      do i=1,nn
       if (array(i) .GT. mm) then
        mm=array(i)
       else
       end if
      end do

      maxi = mm

      return
      end

c----- function of quartile ----
  
      real function quartile(array, nn, num)
      integer nn, num, i, j, ind
      real array(nn), dum
 
      do i=1,nn
       do j=1,nn
        if (array(i) .GE. array(j)) then
         dum = array(i)
         array(i)=array(j)
         array(j)=dum
        end if
       end do
      end do
      
      ind=nn/4*num

      quartile=array(ind)

      return
      end

c----------- function of median -----------

      real function median(array, nn)
      integer nn, mm
      real array(nn), dum
 
      do i=1,nn
       do j=1,nn
        if (array(i) .GE. array(j)) then
         dum = array(i)
         array(i)=array(j)
         array(j)=dum
        end if
       end do
      end do
      
      mm=int(nn/2.)

      median = array(mm)

      return
      end

c----------- function of sort of N ----------

      real function sort(array,nn,N)
      integer i, nn, N
      real array(nn), dum

      do i=1,nn
       do j=1,nn
        if (array(i) .GE. array(j)) then
         dum = array(i)
         array(i)=array(j)
         array(j)=dum
        end if
       end do
      end do
            
      sort=array(N)

      return
      end
      





