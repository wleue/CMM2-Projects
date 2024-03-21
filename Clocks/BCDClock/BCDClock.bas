' BCDCLock. A binary-coded decimal clock
' Rev 1.0.0 William M Leue 1/25/2021

option default integer
option base 1

' Change HRS24 value to 0 for 12-hour time display
' Change SHOWSECS value to 0 to show only minutes and hours
const HRS24 = 1
const SHOWSECS = 1

const NPOWERS = 4
const MAX_DIGITS = 6

const D1COLOR_ON  = RGB(YELLOW)
const D2COLOR_ON  = RGB(GREEN)
const D4COLOR_ON  = RGB(BLUE)
const D8COLOR_ON  = RGB(RED)
const DARK = 20
const D1COLOR_OFF = RGB(DARK, DARK, 0)
const D2COLOR_OFF = RGB(0, DARK, 0)
const D4COLOR_OFF = RGB(0, 0, DARK)
const D8COLOR_OFF = RGB(DARK, 0, 0)

const SW = MM.HRES
const SH = MM.VRES
'const SW = 320
'const SH = 240
const CSIZE = SW\10
const GAP = CSIZE\2



dim hour, minute, second
dim prev_hour, prev_minute, prev_second
dim bitmap(MAX_DIGITS, NPOWERS)
dim on_colors(NPOWERS) = (D1COLOR_ON, D2COLOR_ON, D4COLOR_ON, D8COLOR_ON)
dim off_colors(NPOWERS) = (D1COLOR_OFF, D2COLOR_OFF, D4COLOR_OFF, D8COLOR_OFF)

' Main program
open "debug.txt" for output as #1
mode 1, 16
GetTime
DrawTime
settick 1000, Update, 1
do
  pause 10000
loop
end

' Get the current local time as hour, minute, second.
sub GetTime
  local t$, hr$, mn$, sc$
  t$ = TIME$
  hr$ = MID$(t$, 1, 2)
  mn$ = MID$(t$, 4, 2)
  sc$ = MID$(t$, 7, 2)
  hour = val(hr$)
  if HRS24 = 0 then
    if hour > 12 then hour = hour-12
  end if
  minute = val(mn$)
  second = val(sc$)
end sub

' Update clock every minute or second
sub Update
  local tock = 0
  GetTime
  if minute <> prev_minute then tock = 1
  if hour <> prev_hour then tock = 1
  if SHOWSECS <> 0 then
    if second <> prev_second then tock = 1
  end if
  if tock then
    MakeBitmap 
    DrawTime
    prev_hour = hour
    prev_minute = minute
    prev_second = second
  end if
end sub

' Make the bitmap for the BCD time display
sub MakeBitmap
  local digits(MAX_DIGITS)
  local d, p, v, w
  local q(NPOWERS) = (1, 2, 4, 8)
  digits(1) = hour\10
  digits(2) = hour mod 10
  digits(3) = minute\10
  digits(4) = minute mod 10
  digits(5) = second\10
  digits(6) = second mod 10
  for d = 1 to MAX_DIGITS
    v = digits(d)      
    for p = 1 to NPOWERS
      w = q(p)
      bitmap(d, p) = (v\w) mod 2
    next p
  next d  
end sub

'Draw the current time in BCD format
sub DrawTime
  local nd, lmargin, vmargin, m$, ap$
  local row, col, x, y, con, coff

  page write 1
  cls
  MakeLayout nd, lmargin, vmargin
  for row = 1 to NPOWERS
    y = vmargin + (NPOWERS-row)*(CSIZE+GAP)
    coff = off_colors(row)
    con = on_colors(row)
    for col =  1 to nd
      if bitmap(col, row) = 1 then
        c = con
      else
        c = coff
      end if
      x = lmargin + (col-1)*(CSIZE+GAP)
      box x, y, CSIZE, CSIZE,, RGB(20, 20, 20), c
    next col
  next row  
  page write 0
  page copy 1 to 0, B
  text SW\2, SH-2, time$, "CB"
end sub

' Compute layout
sub MakeLayout nd, lmargin, vmargin
  if SHOWSECS <> 0 then
    nd = 6
  else
    nd = 4
  end if
  lmargin = (SW - nd*CSIZE - (nd-1)*GAP)\2
  vmargin = (SH - 4*CSIZE - 3*GAP)\2
end sub
