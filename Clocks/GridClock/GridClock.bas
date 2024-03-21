' GridClock
' Rev 1.0 William M Leue 14-Oct-2023

option default integer
option base 1
option angle degrees

' Constants

' Counts and Sizes
const NROWS  = 8
const NCOLS  = 15
const TNCL   = NROWS*NCOLS
const CLRAD  = 25
const NHANDS = 2
const HMARG  = mm.hres\2 - (NCOLS*2*CLRAD)\2
const VMARG  = mm.vres\2 - (NROWS*2*CLRAD)\2
const HH     = 1
const MH     = 2
const HHL    = int(0.7*CLRAD)
const MHL    = int(0.9*CLRAD)
const HWID   = 1

' Digit Parameters
const NDIGITS = 4
const NNUMBERS = 10
const NDROWS  = 6
const NDCOLS  = 3
const DFROW   = 2

' Full-grid Patterns
const ROWREP      = 3
const COLREP      = 7
const P_SQUARES1  = 1
const P_EXES1     = 2
const P_OCTAGONS1 = 3
const P_DIAMONDS1 = 4
const P_OCTAGONS2 = 5
const P_SQUARES2  = 6
const NPATTERNS   = 13
const P_TIME      = 10

const NSTEPS     = 25

' Flow Patterns
const MAX_FLOW_PATTERNS = 8
const MAX_ROW_GROUPS    = 8
const F_START_ANGLE     = 1
const F_AINC            = 2
const F_NCOMPS          = 2

' Globals
dim ct$
dim hours, minutes, seconds
dim float clangle(NROWS, NCOLS, NHANDS)
dim float next_clangle(NROWS, NCOLS, NHANDS)
dim clock_locs(NROWS, NCOLS, 2)
dim patterns(NPATTERNS, NROWS, NCOLS, NHANDS)
dim float aincs(NROWS, NCOLS, NHANDS)
dim digit_locs(NDIGITS, 2)
dim digit_patterns(NNUMBERS, NDROWS, NDCOLS, 2)
dim nflowpatterns = 0
dim nrowgroups(MAX_FLOW_PATTERNS)
dim flow_patterns(MAX_FLOW_PATTERNS, MAX_ROW_GROUPS, F_NCOMPS, NCOLS, NHANDS)
dim float fp = 0.0
dim float FPROB = 0.3
dim start = 2

' Main Program
open "debug.txt" for output as #1
ReadPatterns
ReadDigitPatterns
ReadFlowPatterns
cls
InitClocks
GetTime
ChooseTimePattern
ComputeAincs
for i = 1 to NSTEPS
  EvolveClocks i
  DrawClocks
next i
pause 5000
fp = rnd()
DrawClocks
do
  GetTime
  if (seconds >= 55) or (seconds <= 5) then
    ChooseTimePattern
    ComputeAincs
    for i = 1 to NSTEPS
      EvolveClocks i
      DrawClocks
    next i
    pause 5000
    fp = rnd()
  else
    t = 60 - seconds
    if fp <= FPROB then
      RunFlowPattern t
    else  
      Run4x4Patterns t
      end if
    end if
  end if
loop
end

' Run the 4x4 cyclic patterns for 't' seconds.
sub Run4x4Patterns t
  local p, i
  timer = 0
  do
    for p = start to NPATTERNS
      if timer >= t*1000 then exit for
      ChoosePattern p
      ComputeAincs
      for i = 1 to NSTEPS
        EvolveClocks i
        DrawClocks
      next i
    next p
    start = 1
    for p = NPATTERNS-1 to 2 step -1
      if timer >= t*1000 then exit for
      ChoosePattern p
      ComputeAincs
      for i = 1 to NSTEPS
        EvolveClocks i
        DrawClocks
      next i
    next p
  loop until timer >= t*1000
end sub

' Run one of the flow patterns for 't' seconds
sub RunFlowPattern t
  local p, f
  f = 1
  f = RandInt(1, nflowpatterns)
  ChooseFlowPattern f
  DrawClocks
  timer = 0
  do
    pause 25
    UpdateFlowPattern f
    DrawClocks
  loop until timer >= t*1000
end sub

' Read the pattern ROM for 4x4 patterns
' Each data block has 4 values for the top half of the figures and
' 4 values for the bottom half. Each pair of values are for the two different
' hands of each clock. The top and bottom 4 figures get replicated
' 7 times to fill each row, but the last column repetition is truncated to just
' two values to match the length of 15 values (NCOLS) per row.
' After 2 rows have been filled, they get replicated 3 times to complete the
' 8 (NROWS) rows in the grid.
sub ReadPatterns
  local p, row, col, h, nr, rep, bsize
  for p = 1 to NPATTERNS
    bsize = 2
    for row = 1 to 2
      for col = 1 to bsize
        for h = 1 to NHANDS
          read patterns(p, row, col, h)
        next h
      next col
    next row
    for rep = 1 to COLREP
      if rep = COLREP then bsize = 1
      for row = 1 to 2
        for col = 1 to bsize
          for h = 1 to NHANDS
            patterns(p, row, col+rep*2, h) = patterns(p, row, col, h)
          next h
        next col
      next row
    next rep
    for rep = 1 to ROWREP
      for row = 1 to 2
        for col = 1 to NCOLS
          for h = 1 to NHANDS
            patterns(p, row+rep*2, col, h) = patterns(p, row, col, h)
          next h
        next col
      next row
    next rep
  next p
end sub

' Read the digit patterns ROM
sub ReadDigitPatterns
  local d, row, col, scol
  scol = 2
  for d = 1 to NDIGITS
    digit_locs(d, 1) = scol
    digit_locs(d, 2) = DFROW
    inc scol, 3
    if d = 2 then inc scol, 1
  next d
  for d = 1 to NNUMBERS
    for row = 1 to NDROWS
      for col = 1 to NDCOLS
        read digit_patterns(d, row, col, 1)
        read digit_patterns(d, row, col, 2)
      next col
    next row
  next d
end sub

' Read flow patterns ROM
sub ReadFlowPatterns
  local col, comp, group, nrows, h
  read nflowpatterns
  for i = 1 to nflowpatterns
    read nrowgroups(i)
    for group = 1 to nrowgroups(i)
      for comp = 1 to F_NCOMPS
        for col = 1 to NCOLS
          for h = 1 to NHANDS
            read flow_patterns(i, group, comp, col, h)
          next h
        next col
      next comp
    next group
  next i
end sub
  
' Get the current time and break out its components
sub GetTime
  ct$ = time$
  cat ct$, ":"
  hours = val(field$(ct$, 1, ":"))
  minutes = val(field$(ct$, 2, ":"))
  seconds = val(field$(ct$, 3, ":"))
end sub
      
' Initialize Clock positions
sub InitClocks
  local row, col, h, x, y
  for row = 1 to NROWS
    y = VMARG + CLRAD + (row-1)*2*CLRAD
    for col = 1 to NCOLS
      x = HMARG + CLRAD + (col-1)*2*CLRAD
      clock_locs(row, col, 1) = x
      clock_locs(row, col, 2) = y
    next col
  next row
end sub

' Set a clock pattern on the clocks
sub InitClockPattern p
  local row, col, h
  for row = 1 to NROWS
    for col = 1 to NCOLS
      for h = 1 to NHANDS
        clangle(row, col, h) = patterns(p, row, col, h)
      next h  
    next col
  next row
end sub

' Draw the grid of clocks
sub DrawClocks
  local row, col, x, y, xv(4), yv(4), hx, hy, m$
  local float hang, mang
  page write 1
  for row = 1 to NROWS
    for col = 1 to NCOLS
      x = clock_locs(row, col, 1)
      y = clock_locs(row, col, 2)
      circle x, y, CLRAD,,, rgb(gray), rgb(black)
      hang = clangle(row, col, HH)
      mang = clangle(row, col, MH)
      hx = x + HHL*cos(hang)            : hy = y - HHL*sin(hang)
      xv(1) = x + HWID*cos(90.0+hang)   : yv(1) = y - HWID*sin(90.0+hang)
      xv(2) = hx + HWID*cos(90.0+hang)  : yv(2) = hy - HWID*sin(90.0+hang)
      xv(3) = hx + HWID*cos(-90.0+hang) : yv(3) = hy - HWID*sin(-90.0+hang)
      xv(4) = x + HWID*cos(-90.0+hang)  : yv(4) = y - HWID*sin(-90.0+hang)
      polygon 4, xv(), yv(), rgb(white), rgb(white)
      mx = x + MHL*cos(mang)            : my = y - MHL*sin(mang)
      xv(1) = x + HWID*cos(90.0+mang)   : yv(1) = y - HWID*sin(90.0+mang)
      xv(2) = mx + HWID*cos(90.0+mang)  : yv(2) = my - HWID*sin(90.0+mang)
      xv(3) = mx + HWID*cos(-90.0+mang) : yv(3) = my - HWID*sin(-90.0+mang)
      xv(4) = x + HWID*cos(-90.0+mang)  : yv(4) = y - HWID*sin(-90.0+mang)
     polygon 4, xv(), yv(), rgb(white), rgb(white)
    next col
  next row
  page write 0
  page copy 1 to 0, B
  m$ = date$ + "  " + time$
  text mm.hres\2, mm.vres-1, m$, "CB"
end sub

' Choose the next pattern to be displayed
sub ChoosePattern p
  local row, col, h
  for row = 1 to NROWS
    for col = 1 to NCOLS
      for h = 1 to NHANDS
        next_clangle(row, col, h) = patterns(p, row, col, h)
      next h
    next col
  next row
end sub

' Set up the P_TIME pattern for the current time
' and set the next clock angles
sub ChooseTimePattern
  local row, col, h, d, drow, dcol, arow, acol, dn, n
  for row = 1 to NROWS
    for col = 1 to NCOLS
      for h = 1 to NHANDS
        next_clangle(row, col, h) = 225
      next h  
    next col
  next row
  for d = 1 to NDIGITS
    select case d
      case 1
        n = hours\10
      case 2
        n = hours mod 10
      case 3
        n = minutes\10
      case 4
        n = minutes mod 10
    end select
    if n = 0 then n = 10
    dcol = digit_locs(d, 1)
    drow = digit_locs(d, 2)
    for row = 1 to NDROWS
      arow = drow+row-1
      for col = 1 to NDCOLS
        acol = dcol+col-1
        next_clangle(arow, acol, 1) = digit_patterns(n, row, col, 1)
        next_clangle(arow, acol, 2) = digit_patterns(n, row, col, 2)
      next col
    next row
  next d
end sub

' Choose a flow pattern and set the next clock angles
sub ChooseFlowPattern p
  local row, col, h, group, ng, nr
  ng = nrowgroups(p)
  nr = NROWS\ng
  for group = 1 to ng
    for row = 1 to nr
      for col = 1 to NCOLS
        for h = 1 to NHANDS
          clangle(row+(group-1)*nr, col, h) = flow_patterns(p, group, F_START_ANGLE, col, h)
        next h
      next col
    next row
  next group
end sub

' Update a flow pattern by changing all the angles
sub UpdateFlowPattern p
  local row, col, group, h, ng, nr, angle
  ng = nrowgroups(p)
  nr = NROWS\ng
  for group = 1 to ng
    for row = 1 to nr
      for col = 1 to NCOLS
        for h = 1 to NHANDS
          angle = clangle(row+(group-1)*nr, col, h)
          inc angle, flow_patterns(p, group, F_AINC, col, h)
          if angle > 360.0 then inc angle, -360.0
          if angle < -360.0 then inc angle, 360.0
          clangle(row+(group-1)*nr, col, h) = angle
        next h
      next col
    next row
  next group
end sub

' Compute the angle increments for each clock hand to move from its
' current angle to the new angle specified by the next pattern when
' 4x4 patterns are used. The direction the hands move is to make the
' change in angle as small as possible. This occasionally produces
' a clock whose hands move contrary to the other clocks, but I kinda
' like it.
sub ComputeAincs
  local row, col, h, s1, s2
  local float adelta, cangle, nangle
  for row = 1 to NROWS
    for col = 1 to NCOLS
      for h = 1 to NHANDS
        cangle = clangle(row, col, h)  
        nangle = next_clangle(row, col, h)
        adelta = nangle - cangle
        if adelta > 180.0 then
          do
            adelta = adelta - 360.0
          loop until adelta <= 360.0
        else if adelta < -180.0 then
          do
            adelta = 360.0 + adelta
          loop until adelta >= -180.0
        end if
        aincs(row, col, h) = adelta/NSTEPS
      next h
    next col
  next row 
end sub

' Gradually Change the clock hand angles from the current ones to the
' angles in next_clangle()
sub EvolveClocks i
  local row, col, h
  local float cangle
  for row = 1 to NROWS
    for col = 1 to NCOLS
      for h = 1 to NHANDS
        cangle = clangle(row, col, h) + aincs(row, col, h)
        clangle(row, col, h) = cangle
      next h
    next col
  next row
end sub

' return a uniformly distributed random integer in the specified closed range
function RandInt(a as integer, b as integer)
  local integer v, c
  c = b-a+1
  do
    v = a + (b-a+2)*rnd()
    if v >= a and v <= b then exit do
  loop
  RandInt = v
end function

' ROM for full-grid repeating 4x4 patterns
' (these get replicated to 8 rows, 15, columns, 2 hands)
      
' Squares1
data 0, 270, 180, 270
data 0, 90,  180, 90

' Exes1
data 315, 315, 225, 225
data 45,  45,  135, 135

' Octagons1
data 15, 255, 165, 285
data 105, 345, 195, 75

' Diamonds1
data 45, 225, 135, 315
data 135, 315, 225, 45

' Octagons2
data 75, 195, 105, 345
data 165, 285, 255, 15

' Exes2
data 315, 315, 225, 225
data 45,  45,  135, 135

' Squares2
data 90, 180, 90, 0
data 180, 270, 270, 0

' Chevrons1
data 45, 315, 135, 225
data 45, 315, 135, 225

' Chevrons2
data 135, 225, 45, 315
data 135, 225, 45, 315

' VW1
data 89, 91, 89, 91
data 89, 91, 89, 91

' VW2
data 269, 271, 269, 271
data 269, 271, 269, 271

' Tumbler1
data 0, 270, 180, 270
data 90, 0,  90,  180

' Tumbler2
data 90, 0, 90, 180
data 0, 270, 180, 270

' Number Patterns (3 x 6)

' One
data 0,   270, 180, 0,   180, 270
data 0,   90,  180, 270, 90,  270
data 225, 225, 90,  270, 90,  270
data 225, 225, 90,  270, 90,  270
data 225, 225, 90,  270, 90,  270
data 225, 225, 0,   90,  90,  180

' Two
data 0,   270, 180, 0,   180, 270
data 0,   90,  180, 270, 90,  270
data 0,   270, 90,  180, 90,  270
data 90,  270, 0,   270, 90,  180
data 90,  270, 0,   90,  180, 270
data 0,   90,  0,   180, 90,  180

' Three
data 0,   270, 180, 0,   180, 270
data 0,   90,  180, 270, 90,  270
data 0,   270, 90,  180, 90,  270
data 0,   90,  180, 270, 90,  270
data 0,   270, 90,  180, 90,  270
data 0,   90,  0,   180, 90,  180

' Four
data 0,   270, 180, 270, 180, 270
data 90,  270, 90,  270, 90,  270
data 90,  270, 0,   90,  90,  270
data 0,   90,  180, 270, 90,  270
data 225, 225, 90,  270, 90,  270
data 225, 225, 0,   90,  90,  180

' Five
data 0,   270, 180, 0,   180, 270
data 90,  270, 0,   270, 90,  180
data 90,  270, 0,   90,  180, 270
data 0,   90,  180, 270, 90,  270
data 0,   270, 90,  180, 90,  270
data 0,   90,  0,   180, 90,  180

' Six
data 0,   270, 180, 0,   180, 270
data 90,  270, 0,   270, 90,  180
data 90,  270, 0,   90,  180, 270
data 90,  270, 270, 270, 90,  270
data 90,  270, 90,  90,  90,  270
data 0,   90,  0,   180, 90,  180

' Seven
data 0,   270, 180, 0,   180, 270
data 0,   90,  180, 270, 90,  270
data 225, 225, 90,  225, 90,  225
data 45,  270, 45,  270, 225, 225
data 90,  270, 90,  270, 225, 225
data 0,   90,  90,  180, 225, 225

' Eight
data 0,   270, 180, 0,   180, 270
data 90,  270, 270, 270, 90,  270
data 90,  335, 90,  90,  90,  225
data 45,  270, 270, 270, 135, 270
data 90,  270, 90,  90,  90,  270
data 0,   90,  0,   180, 90,  180

' Nine
data 0,   270, 180, 0,   180, 270
data 90,  270, 270, 270, 90,  270
data 90,  270, 90,  90,  90,  270
data 0,   90,  180, 270, 90,  270
data 0,   270, 90,  180, 90,  270
data 0,   90,  0,   180, 90,  180

' Zero
data 0,   270, 180, 0,   180, 270
data 90,  270, 270, 270, 90,  270
data 90,  270, 90,  270, 90,  270
data 90,  270, 90,  270, 90,  270
data 90,  270, 270, 270, 90,  270
data 0,   90,  180, 0,   90,  180

' Flow Patterns
' Number of flow patterns
' For each flow pattern:
'   Number of distinct row groups (1, 2, 4, or 8)
'   For each row group:
'    2 sets of 30 values each:
'      Start angles for each hand
'      Angle increments for each hand (-=CW, +=CCW)
data 3

data 1
data 180, 0, 180, 0, 180, 0, 180, 0, 180, 0, 180, 0, 180, 0, 180, 0
data 180, 0, 180, 0, 180, 0, 180, 0, 180, 0, 180, 0, 180, 0
data 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7
data 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13, 14, 14

data 8
data 180, 0, 180, 0, 180, 0, 180, 0, 180, 0, 180, 0, 180, 0, 180, 0
data 180, 0, 180, 0, 180, 0, 180, 0, 180, 0, 180, 0, 180, 0
data 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7
data 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13, 14, 14
data 180, 0, 180, 0, 180, 0, 180, 0, 180, 0, 180, 0, 180, 0, 180, 0
data 180, 0, 180, 0, 180, 0, 180, 0, 180, 0, 180, 0, 180, 0
data 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8
data 9, 9, 10, 10, 11, 11, 12, 12, 13, 13, 14, 14, 15, 15
data 180, 0, 180, 0, 180, 0, 180, 0, 180, 0, 180, 0, 180, 0, 180, 0
data 180, 0, 180, 0, 180, 0, 180, 0, 180, 0, 180, 0, 180, 0
data 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9
data 10, 10, 11, 11, 12, 12, 13, 13, 14, 14, 15, 15, 16, 16
data 180, 0, 180, 0, 180, 0, 180, 0, 180, 0, 180, 0, 180, 0, 180, 0
data 180, 0, 180, 0, 180, 0, 180, 0, 180, 0, 180, 0, 180, 0
data 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10
data 11, 11, 12, 12, 13, 13, 14, 14, 15, 15, 16, 16, 17, 17
data 180, 0, 180, 0, 180, 0, 180, 0, 180, 0, 180, 0, 180, 0, 180, 0
data 180, 0, 180, 0, 180, 0, 180, 0, 180, 0, 180, 0, 180, 0
data 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10
data 11, 11, 12, 12, 13, 13, 14, 14, 15, 15, 16, 16, 17, 17
data 180, 0, 180, 0, 180, 0, 180, 0, 180, 0, 180, 0, 180, 0, 180, 0
data 180, 0, 180, 0, 180, 0, 180, 0, 180, 0, 180, 0, 180, 0
data 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9
data 10, 10, 11, 11, 12, 12, 13, 13, 14, 14, 15, 15, 16, 16
data 180, 0, 180, 0, 180, 0, 180, 0, 180, 0, 180, 0, 180, 0, 180, 0
data 180, 0, 180, 0, 180, 0, 180, 0, 180, 0, 180, 0, 180, 0
data 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8
data 9, 9, 10, 10, 11, 11, 12, 12, 13, 13, 14, 14, 15, 15
data 180, 0, 180, 0, 180, 0, 180, 0, 180, 0, 180, 0, 180, 0, 180, 0
data 180, 0, 180, 0, 180, 0, 180, 0, 180, 0, 180, 0, 180, 0
data 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7
data 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13, 14, 14

data 8
data 90, 270, 90, 270, 90, 270, 90, 270, 90, 270, 90, 270, 90, 270, 90, 270
data 90, 270, 90, 270, 90, 270, 90, 270, 90, 270, 90, 270, 90, 270
data 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5
data 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5
data 90, 270, 90, 270, 90, 270, 90, 270, 90, 270, 90, 270, 90, 270, 90, 270
data 90, 270, 90, 270, 90, 270, 90, 270, 90, 270, 90, 270, 90, 270
data 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6
data 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6
data 90, 270, 90, 270, 90, 270, 90, 270, 90, 270, 90, 270, 90, 270, 90, 270
data 90, 270, 90, 270, 90, 270, 90, 270, 90, 270, 90, 270, 90, 270
data 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7
data 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7
data 90, 270, 90, 270, 90, 270, 90, 270, 90, 270, 90, 270, 90, 270, 90, 270
data 90, 270, 90, 270, 90, 270, 90, 270, 90, 270, 90, 270, 90, 270
data 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8
data 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8
data 90, 270, 90, 270, 90, 270, 90, 270, 90, 270, 90, 270, 90, 270, 90, 270
data 90, 270, 90, 270, 90, 270, 90, 270, 90, 270, 90, 270, 90, 270
data 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9
data 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9
data 90, 270, 90, 270, 90, 270, 90, 270, 90, 270, 90, 270, 90, 270, 90, 270
data 90, 270, 90, 270, 90, 270, 90, 270, 90, 270, 90, 270, 90, 270
data 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10
data 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10
data 90, 270, 90, 270, 90, 270, 90, 270, 90, 270, 90, 270, 90, 270, 90, 270
data 90, 270, 90, 270, 90, 270, 90, 270, 90, 270, 90, 270, 90, 270
data 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11
data 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11
data 90, 270, 90, 270, 90, 270, 90, 270, 90, 270, 90, 270, 90, 270, 90, 270
data 90, 270, 90, 270, 90, 270, 90, 270, 90, 270, 90, 270, 90, 270
data 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12
data 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12


' =========================
' End of Source Code
' =========================











