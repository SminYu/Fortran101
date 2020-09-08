c234567
      program calnder
      implicit none
      
      integer date, day, mon, year, def
      integer year2, mon2, day2, day3
      integer chk1, chk2, chk3, diff, dow
      logical leap
      
c 1900년 1월 1일은 월요일이다.
      
      print *, 'input the year, month, day'
      read (*,*) year,mon,day
      
      year2=year-1900
      mon2=mon-1
      day2=day-1
      
c 1년이 365일이고 윤년고려(4년에 한번, 40년에 한번 x, 400년에 한번)
      chk1=(year/4)-475
      chk2=(year/100)-19
      chk3=(year/400)-4
      leap = .FALSE.
      If (mod(year, 4) .EQ. 0) then
          leap = .TRUE.
          If (mod(year, 100) .EQ. 0) then
              leap = .FALSE.
              If (mod(year, 400) .EQ. 0) then
                  leap = .TRUE.
              else
              end if
          else
          end if
      else
      end if  
      print *, 'Leap year', chk1-chk2+chk3
      diff=(year2*365)+chk1-chk2+chk3
      
c 1,3,5,7,8,10,12월은 31일
c 4,6,9,11월은 30일
c 2월은 29일 or 28일(윤년만)
      If (mon2 .GE. 0) then
      If (mon2 .GE. 1) diff=diff+31
      if (mon2 .GE. 2) then
          If (leap .EQ. .TRUE.) then
              diff=diff+29
          else
              diff=diff+28
          end if
      end if
      if (mon2 .GE. 3) diff=diff+31
      if (mon2 .GE. 4) diff=diff+30
      if (mon2 .GE. 5) diff=diff+31
      if (mon2 .GE. 6) diff=diff+30
      if (mon2 .GE. 7) diff=diff+31
      if (mon2 .GE. 8) diff=diff+31
      if (mon2 .GE. 9) diff=diff+30
      if (mon2 .GE. 10) diff=diff+31
      if (mon2 .GE. 11) diff=diff+30
      else
      end if
c 날짜 더함
      diff=diff+day2
c 요일 계산
      print*, diff
      dow=mod(diff, 7)
      If (dow .EQ. 0) then
          print *, 'Monday'
      else if (dow .EQ. 1) then
          print *, 'Tuesday'
      else if (dow .EQ. 2) then
          print *, 'Wednsday'
      else if (dow .EQ. 3) then
          print *, 'Thursday'
      else if (dow .EQ. 4) then
          print *, 'Friday'
      else if (dow .EQ. 5) then
          print *, 'Saturday'  
      else
          print *, 'Sunday'
      end if
      
      stop
      end
      