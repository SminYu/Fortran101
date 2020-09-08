c234567
c 1이상 5000미만인 self-number의 합
      program self_number
      integer a(1:5000), c(1:5000), s(1:5000), d(1:5000), nn, res, dum
      integer dum2
c initialization
      data nn, res/5000, 0/
      c=0
      s=0
      d=0
c a(i)는 i
      do i=1,nn
          a(i)=i
      end do
c c(i)는 a(i)의 자릿수-1 (10**n의 최대 n)
      do i=1,5000
          do j=3, 0, -1
              If (i/(10**j) .LT. 10) then
                  c(i) = j
              end if
          end do
      end do
c s(i)는 a(i)의 각 자릿수의 합
      do i=1,5000
          dum=i
c dum2는 dum의 10**j 자릿수
c s(i) =+ dum2 하여서 자릿수의 합을 만든다
c dum에서 dum2 * 10**j를 빼서 처음부터 반복
          do j=c(i),0,-1
              dum2=dum/(10**j)
              s(i)=s(i)+dum2
              dum=dum-dum2*(10**j)
          end do
c d(i)는 s(i) + i (각 자릿수의 합과 자기 자신의 합)
          d(i)=s(i)+i
      end do
c 서로 다른 수의 generator가 같은 경우 0으로 해서 겹치지 않게 함 
c generator가 5000보다 크면 0으로 해서 필요하지 않은 수를 제외
      do i=1,nn
          do j=1,nn
              If ((d(i) .EQ. d(j) .AND. i .NE. j) .OR. (d(j) .GE. 5000))
     ~then
                  d(j)=0
              else
              end if
          end do
      end do
c res는 1이상 5000미만인 d(i)의 합
      do i=1,nn
          res=res+d(i)
      end do
c 1이상 5000미만인 수의 합 = ind    
      ind=4999*5000/2
c ind - res는 5000미만인 d(i)가 없는 수의 합
      print *, 'The answer is', ind-res
c 정답은 1227365
      stop
      end