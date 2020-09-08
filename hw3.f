c234567
      program newton
      implicit none
      real*8  xx, expon, ct, expt, a(20)
      integer i, nn
      
      print *, 'Input the initial x'
      read (*,*) xx
      
      call facto(a)

	nn=1000

      call tangent(xx, ct)
      
      open (111, file='result.dat')
      
      write (111, '(A,1X,I4.4)') 'Number of iteration', nn
      write (111, '(I4.4,1X,F10.5)') 0, xx
      
      do i=1,nn
          xx=xx-(expon(xx)-1)/ct
          write (111, '(I4.4,1X,F10.5)') i, xx
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
      subroutine tangent(x, coeff)
      real*8 x, coeff, expon
      
      coeff=expon(x)
      
      return
      end

