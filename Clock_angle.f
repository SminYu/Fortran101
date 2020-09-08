c234567
c 시침과 분침 사이의 각도 계산
      program clock_angle
      implicit none

      real time, hour, min, sec, hrang, minang, angbtw
      integer d90, d180

      d90=0
      d180=0
      
      do hour=0,23
          do min=0,59
              do sec=0,59
      
c 분침은 60분에 360도 움직인다.
c 360도/60분 = 6도/1분 = 6도/60초 = 0.1도/1초
      minang=6.0*min+0.1*sec
      
c 시침은 12시간에 360도 움직인다.
c 360도/12시간 = 30도/시간 = 0.5도/분 = 0.5도/60초 = 120분의1도/초
c 정오가 넘으면 (12시 이후) 0시부터 시작한것과 같음 (12시간을 빼서 계산)
      If (hour .LT. 12.0) then
          hrang=30.0*hour+0.5*min+(1/120.0)*sec
      else
          hrang=30.0*(hour-12.0)+0.5*min+(1/120.0)*sec
      end if
      
c 시침 분침 사이의 각도를 빼고 절댓값
      angbtw = abs(hrang-minang)

c 둔각일 경우 예각으로 변환
      If (angbtw .GE. 180.0) angbtw=360.0-angbtw

      
      If (angbtw .EQ. 90) then
          print *, '90 degrees', hour,':',min,':',sec
          d90=d90+1
      else if (angbtw .EQ. 180) then
          print *, '180 degrees', hour,':',min,':',sec
          d180=d180+1
      else
      end if
      
              end do
          end do
      end do
      
      
      print *, 'total 90', d90
      print *, 'total 180', d180
      
      stop
      end