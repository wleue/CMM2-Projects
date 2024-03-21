' SpinClock
' Rev 1.0.0 William M Leue 1/25/2021
' Rev 1.0.1 1/26/2021 - fixed bug in hour turnover.
' Rev 1.1.0 1/26/2021 - rewrote the sequencer to be less
'  baroque, more maintainable, and hopefully bug-free.
' Rev 1.1.1 1/27/2021 -- fixed hopefully last bug
' Rev 1.1.2 1/28/2021 -- nope, another bug in 24 hour time (fixed)
'  added debug mode to churn through all hours to catch bugs
option default integer
option base 1

const NUMH = 4
const NUMV = 6
const NUMGH = 4
const NUMGV = 1
const NHANDS = 2
const NVALS = 10

const HRS24 = 1  ' change to 1 for 24-hour time
const DEBUG = 0  ' run through all times fast for debugging

const CRAD = 20
const CDIAM = 2*CRAD
const GAP = 20
const BGAP = 60

const CCOLOR = RGB(200, 200, 200)

const GWIDTH = NUMH*CDIAM
const GHEIGHT = NUMV*CDIAM
const TWIDTH = NUMGH*GWIDTH + (NUMGH-2)*GAP+BGAP
const THEIGHT = NUMGV*GHEIGHT + GAP
const LMARGIN = MM.HRES\2 - TWIDTH\2
const VMARGIN = MM.VRES\2 - THEIGHT\2

const NSPINS = 160
const MSTART = NSPINS\4
const ABINC = 360.0/(1.0*NSPINS)

' globals
dim valdata(NVALS, NUMV, NUMH, NHANDS)
dim hour, minute
dim prev_hour = 0
dim prev_minute = 0
dim redraw = 0
dim float aboost = 0.0
dim colons = 0
dim spinning = 0
dim first = 1
dim mutate(NUMH*NUMV, 2)
dim advance(NUMV, NUMH)
dim mindex
dim current_place = 0
dim time_flag

' main program
open "debug.txt" for output as #1
mode 1, 16
page write 1
cls RGB(WHITE)

EndSpin
ReadValData
ReadMutationData
UpdateTime
time_flag = 0
if DEBUG then
  hour = 1
  minute = 59
end if
prev_hour = hour
prev_minute = minute
DrawClock
Sequencer
end

' Time Sequencer. Uses entirely synchronous
' polling for time updates, digit spinning,
' and colon blinking.
sub Sequencer
  do
    
    ' time update
    if DEBUG = 0 then
      UpdateTime
    else
      inc hour
      if hour > 23 then hour = 0
      if HRS24 = 0 then
        if hour > 12 then hour = hour-12
        if hour = 0 then hour = 12
      end if
      time_flag = 1
      DrawClock
      prev_hour = hour
      pause 300
    end if

    ' if time has updated then get ready to spin
    ' the spin loop suspends time polling and colon
    ' blinking. Also, the previous hour and minute
    ' can't be updated until spinning is complete.
    if DEBUG = 0 and time_flag = 1 then
      time_flag = 0
      InitSpin
      do while spinning < NSPINS
        Spin
        DrawClock
      loop
      EndSpin
      prev_hour = hour
      prev_minute = minute      
      DrawClock
    end if
    
    ' flash the colons
    if DEBUG = 0 then
      colons = 1 - colons
      DrawColons
    end if
    
    ' 1-second polling
    if DEBUG = 0 then
      pause 1000
    end if
end if

  loop
end sub

' Update the time when the minute changes
sub UpdateTime
  local t$, hr$, mn$

  time_flag = 0
  t$ = time$
  hr$ = MID$(t$, 1, 2)
  mn$ = MID$(t$, 4, 2)
  hour = val(hr$)
  if HRS24 = 0 then
    if hour > 12 then hour = hour-12
    if hour = 0 then hour = 12
  end if
  
  minute = val(mn$)
  if minute <> prev_minute then
    time_flag = 1
  end if
end sub

' Initialize parameters for spinning
sub InitSpin
  mindex = 1
  math set 0, advance()
  mrow = mutate(mindex, 1)
  mcol = mutate(mindex, 2)
  advance(mrow, mcol) = 1
  aboost = 0.0
  spinning = 1
end sub

' Spin, Baby, Spin!
sub Spin
  inc spinning
  inc aboost, ABINC
  if spinning > MSTART and (spinning mod 2) > 0 then
    if mindex < NUMV*NUMH then inc mindex
    mrow = mutate(mindex, 1)
    mcol = mutate(mindex, 2)
    advance(mrow, mcol) = 1
  end if
end sub

' Clean up after spinning
sub EndSpin
  spinning = 0
  aboost = 0.0
  math set 0, advance()
  mindex = 1
end sub

' Read the cell data for digits 1..9, 0
sub ReadValData
  local i, j, k, m
  for i = 1 to NVALS
    for j = 1 to NUMV
      for k = 1 to NUMH
        for m = 1 to NHANDS
          read valdata(i, j, k, m)
        next m
      next k
    next j
  next i
end sub

' Read the mutation data for a digit
sub ReadMutationData
  local i, j
  for i = 1 to NUMV*NUMH
    for j = 1 to 2
      read mutate(i, j)
    next j
  next i
end sub
    
' Draw the Clock
sub DrawClock
  local row, col, n
  local digits(NUMGH)
  static prev_digits(NUMGH)
  local changed(NUMGH)

  page write 1

  ' extract separate digits
  if HRS24 = 1 then
    if hour = 0 then
      digits(1) = 10
      digits(2) = 10
    else
      if hour < 10 then
        digits(1) = 10
        digits(2) = hour
      else
        digits(1) = hour\10
        od = hour mod 10
        if od = 0 then od = 10
        digits(2) = od
      end if
    end if
  else
    if hour < 10 then
      digits(1) = 10
      digits(2) = hour
    else
      digits(1) = hour\10
      od = hour mod 10
      if od = 0 then od = 10
      digits(2) = od
    end if
  end if
  if minute < 10 then
    digits(3) = 10
    od = minute
    if od = 0 then od = 10
    digits(4) = od
  else
    digits(3) = minute\10
    od = minute mod 10
    if od = 0 then od = 10
    digits(4) = od
  end if
  
    
  ' set flags for digits that have changed
  for i = 1 to 4
    if digits(i) <> prev_digits(i) then
      changed(i) = 1
    else
      changed(i) = 0
    end if
    if spinning = 0 or prev_digits(i) = 0 then
      prev_digits(i) = digits(i)
    end if
  next i
    
  ' draw the digits
  n = 0
  for row = 1 to NUMGV
    for col = 1 to NUMGH
      inc n
      if changed(n) then
        DrawGroup row, col, digits(col), prev_digits(col)
        current_place = n
      end if
    next col
  next row
  page write 1
  page copy 1 to 0, B
  tick = 0
end sub

' Draw the circles that make one digit
sub DrawGroup row, col, digit, prev_digit
  local x, y, cx, cy, i, j

  x = LMARGIN + (col-1)*(GWIDTH+GAP)
  if col > 2 then x = x - GAP + BGAP
  y = VMARGIN + (row-1)*(GHEIGHT+GAP)
  np = 0 : nn = 0
  for i = 1 to NUMV
    cy = y + (i-1)*CDIAM + CRAD
    for j = 1 to NUMH
      cx = x + (j-1)*CDIAM + CRAD
      circle cx, cy, CRAD,,, CCOLOR, RGB(WHITE)
      if advance(i, j) then
        DrawHands i, j, cx, cy, digit
        DrawHands i, j, cx, cy, digit
      else
        DrawHands i, j, cx, cy, prev_digit
        DrawHands i, j, cx, cy, prev_digit
      end if
    next j
  next i
end sub  

' Draw the 'hands' on a circle
sub DrawHands row, col, cx, cy, digit
  local i, j, hi, h2, x1, y1, x2, y2, c
  local float a1, a2, ab
  
  if digit < 1 or digit > 10 then
    page write 0
    mode 1,8
    cls
    print "ERROR: digit out of range: " + str$(digit)
    print "hour: " + str$(hour) + " prev_hour: " + str$(prev_hour)
    print "minute: " + str$(minute) + " prev_minute: " + str$(prev_minute)
    print "current place: " + str$(current_place)
    end
  end if
  h1 = valdata(digit, row, col, 1)
  h2 = valdata(digit, row, col, 2)
  c = RGB(BLACK)
  if h1 = 5 and h2 = 5 then 
    exit sub
  end if
  if spinning > 0 then 
    ab = aboost
  else
    ab = 0.0
  end if
  a1 = h1*45.0 + ab
  a2 = h2*45.0 + ab
  x1 = cx + int((CRAD-1)*cos(rad(a1)) + 0.5)
  y1 = cy - int((CRAD-1)*sin(rad(a1)) + 0.5)
  line cx, cy, x1, y1,, c
  x2 = cx + int((CRAD-1)*cos(rad(a2)) + 0.5)
  y2 = cy - int((CRAD-1)*sin(rad(a2)) + 0.5)
  line cx, cy, x2, y2,, c
end sub  

'Draw the blinking colons
sub DrawColons
  local x, y1, y2, c
  x = LMARGIN + 2*GWIDTH + GAP + BGAP\2
  y1 = VMARGIN + 4*CRAD
  y2 = VMARGIN + 8*CRAD
  if colons then
    c = CCOLOR
  else
    c = RGB(BLACK)
  end if
  page write 0
  circle x, y1, CRAD,,, c
  circle x, y2, CRAD,,, c  
end sub

' Data for Circle Hands
' Angles are increments of 45 degrees, counting ccl from 0
' Each circle has two values for the 2 'hands'.
' The code 5, 5 is for a 'blank' (no hands).

' ONE
data 0, 6, 0, 4, 4, 6, 5, 5
data 0, 2, 4, 6, 2, 6, 5, 5
data 5, 5, 2, 6, 2, 6, 5, 5
data 5, 5, 2, 6, 2, 6, 5, 5
data 0, 6, 2, 4, 0, 2, 4, 6
data 0, 2, 0, 4, 0, 4, 2, 4

'TWO
data 5, 5, 0, 5, 4, 7, 5, 5
data 1, 6, 0, 6, 4, 6, 3, 6
data 0, 2, 2, 4, 2, 5, 2, 5
data 5, 5, 1, 5, 1, 5, 5, 5
data 1, 6, 0, 1, 0, 4, 4, 6
data 0, 2, 0, 4, 0, 4, 2, 4

' THREE
data 5, 5, 0, 5, 4, 7, 5, 5
data 1, 6, 0, 6, 4, 6, 3, 6
data 0, 2, 2, 4, 2, 5, 2, 5
data 0, 6, 4, 6, 3, 6, 3, 6
data 2, 7, 0, 2, 2, 4, 2, 5
data 5, 5, 0, 3, 1, 4, 5, 5

' FOUR
data 0, 6, 4, 6, 0, 6, 4, 6
data 2, 6, 2, 6, 2, 6, 2, 6
data 2, 6, 0, 2, 2, 4, 2, 6
data 0, 2, 0, 4, 4, 6, 2, 6
data 5, 5, 5, 5, 2, 6, 2, 6
data 5, 5, 5, 5, 0, 2, 2, 4

' FIVE
data 0, 6, 0, 4, 0, 4, 4, 6
data 2, 6, 0, 6, 0, 4, 2, 4
data 2, 6, 0, 2, 4, 7, 5, 5
data 0, 2, 0, 4, 4, 6, 3, 6
data 0, 6, 0, 4, 2, 4, 2, 5
data 0, 2, 0, 4, 1, 4, 5, 5

' SIX
data 5, 5, 0, 5, 4, 7, 5, 5
data 1, 6, 0, 6, 0, 4, 3, 4
data 2, 6, 0, 2, 4, 7, 5, 5
data 2, 6, 0, 6, 4, 6, 3, 6
data 2, 7, 0, 2, 2, 4, 2, 5
data 5, 5, 0, 3, 1, 4, 5, 5

' SEVEN
data 0, 6, 0, 4, 0, 4, 4, 6
data 0, 2, 0, 4, 4, 6, 2, 6
data 5, 5, 5, 5, 2, 5, 2, 5
data 5, 5, 1, 6, 1, 6, 5, 5
data 5, 5, 2, 6, 2, 6, 5, 5
data 5, 5, 0, 2, 2, 4, 5, 5

' EIGHT
data 5, 5, 0, 5, 4, 7, 5, 5
data 1, 6, 0, 6, 4, 6, 3, 6
data 2, 7, 0, 2, 2, 4, 2, 5
data 1, 6, 0, 6, 4, 6, 3, 6
data 2, 7, 0, 2, 2, 4, 2, 5
data 5, 5, 0, 3, 1, 4, 5, 5

' NINE
data 5, 5, 0, 5, 4, 7, 5, 5
data 1, 6, 0, 6, 4, 6, 3, 6
data 2, 7, 0, 2, 2, 4, 2, 6
data 5, 5, 0, 3, 4, 6, 2, 6
data 0, 7, 0, 4, 2, 4, 2, 5
data 5, 5, 0, 3, 1, 4, 5, 5

' ZERO
data 5, 5, 0, 5, 4, 7, 5, 5
data 1, 6, 0, 6, 4, 6, 3, 6
data 2, 6, 2, 6, 2, 6, 2, 6
data 2, 6, 2, 6, 2, 6, 2, 6
data 2, 7, 0, 2, 2, 4, 2, 5
data 5, 5, 0, 3, 1, 4, 5, 5

' mutation coords
data 2, 3, 4, 4, 5, 1, 2, 4
data 1, 1, 6, 3, 4, 2, 3, 2
data 1, 3, 4, 1, 5, 4, 5, 2
data 1, 2, 3, 1, 3, 3, 4, 3
data 6, 2, 1, 4, 6, 1, 6, 4
data 2, 1, 3, 4, 5, 3, 2, 3


