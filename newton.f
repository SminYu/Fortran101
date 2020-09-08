c234567
      program newton
      implicit none
      real*8  xx, expon, ct1, ct2, expt, a(20)
      integer i, nn
      
      xx=2.
      
      call facto(a)
      
      nn=100
      
      open (111, file='result.dat')
      
      write (111, '(A,1X,I3.3)') 'Number of iteration', nn
      write (111, '(I3.3,1X,F10.5)') 0, xx
      
      do i=1,nn
          call tangent(xx, ct1, ct2)
          xx=-(ct2/ct1)
          write (111, '(I3.3,1X,F10.5)') i, xx
      end do
      
      print *, 'The result is'
      print *, xx
      
      stop
      end

c-----factorial function
      subroutine facto(factorial)
      integer i 
      real*8 factorial(20)
      
      do i=1,20
          factorial(i)=1
      end do
      
      do i=1,20
          do j=1,i
              factorial(i)=factorial(i)*j
          end do
      end do
      
      
      return
      end
      
c-----exponential function
      real*8 function expon(x)
      real*8 x, a(20)
      integer i
      
      call facto(a)
      
      expon=1
      
      do i=1,20
          expon=expon+(x**i/a(i))
      end do
      
      return
      end
      
c-----tangent line
      subroutine tangent(x, coeff1, coeff2)
      real*8 x, coeff1, coeff2, expon
      
      coeff1=expon(x)
      coeff2=(expon(x)*(1-x))-1
      
      return
      end
      