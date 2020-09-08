c234567
c 앞뒤가 같은 수
      program mirror_number
      implicit none
      integer nth, i, max, mm, number, dd
      integer dgt(0:1000)
      logical status(i)
      
      write (*,*) 'The n-th mirror number'
      read (*,*) nth
      
      do i=10,1,-1
          if (nth .LT. 2*10**i-1) then
              max=i+1
          end if
      end do
      
      print *, max
      
      mm=(2*10**(max))-1
      
      print *, mm
      
      cc=0
      do number=0,mm
          
          call digit(number, dd, dgt)
          
          call checker(number, dd, dgt)
          
          If (status(i) .EQ. .FALSE.) then
             cc=cc+1
          else
          end if
          
      end do
      
      stop
      end
      
      
      subroutine digit(number, dd, dgt)

          do i=1, 1000
              number=n
      
      return
      end
      
      
      subroutine checker(number, dd)