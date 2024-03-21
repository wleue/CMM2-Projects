' Cyclic Cellular Automaton
' Rev 1.0.0 William M Leue 25-Mar-2022

option default integer
option base 1

const MIN_SIZE = 50
const MAX_SIZE = 100
const CSIZE = 4
const MIN_NUM_COLORS = 4
const MAX_NUM_COLORS = 20

dim boardSize = 0
dim numColors = 0
dim colors(MAX_NUM_COLORS)
dim dstep(2, 4)
dim board(2, 2)

' Main Program
open "debug.txt" for output as #1
ReadDSteps
GetParamValues
MakeColors
DrawBoard
RunAutomaton
end

sub ReadDSteps
  local dir, v
  for dir = 1 to 4
    for v = 1 to 2
      read dstep(v, dir)
    next v
  next dir
end sub

sub GetParamValues
  local m$, a$
  local size, nc, ok
  cls
  do
    ok = 1
    print "Enter board size (min = ";MIN_SIZE;", max = ";MAX_SIZE;"): ";
    input "", a$
    size = val(a$)
    if size < MIN_SIZE or size > MAX_SIZE then
      print "Sorry, that size is out of range"
      ok = 0
    end if
  loop until ok = 1
  print ""
  do
    ok = 1
    print "Enter number of colors (min = ";MIN_NUM_COLORS;", max = ";MAX_NUM_COLORS;"): ";
    input "", a$
    nc = val(a$)
    if nc < MIN_NUM_COLORS or nc > MAX_NUM_COLORS then
      print "Sorry, that number is out of range"
      ok = 0
    end if
  loop until ok = 1
  boardSize = size
  numColors = nc
  erase board
  dim board(boardSize, boardSize)
end sub

sub MakeColors
  local i
  local float hue, sat, brt, scale
  sat = 1.0
  brt = 1.0
  scale = 360.0/numColors
  hue = 0.0
  for i = 1 to numColors
    colors(i) = GetRGBColor(hue, sat, brt)
    inc hue, scale
  next i
end sub

sub DrawBoard
  local row, col, x, y, bw, bh, c, cx
  cls

  bw = 6 + boardSize*CSIZE : bh = 6 + boardSize*CSIZE
  for row = 2 to boardSize-1
    y = 3 + (row-1)*CSIZE
    for col = 2 to boardSize-1
      x = 3 + (col-1)*CSIZE
      cx = RandomIntegerInRange(1, numColors)
      board(col, row) = cx
      c = colors(cx)
      box x, y, CSIZE, CSIZE,, c, c
    next col
  next row
end sub

sub RunAutomaton
  local row, col, dir, dc, dr, nrow, ncol, cx, ncx, x, y, c
  local p = 0
  do
    for row = 2 to boardSize-1
      for col = 2 to boardSize-1
        cx = board(col, row) 
        for dir = 1 to 4
          dc = dstep(1, dir) : dr = dstep(2, dir)
inc p, 15
if p > 580 then p = 15
          ncol = col+dc : nrow = row+dr
print @(400, p);"dir: ";dir;" dc: ";dc;" dr: ";dr; " ncol: ";ncol; " nrow: ";nrow
            ncx = board(ncol, nrow)
            if (ncx = cx+1) or (cx = numColors and ncx = 1) then
              board(col, row) = ncx
              c = colors(cx)
              x = 3 + (col-1)*CSIZE
              y = 3 + (row-1)*CSIZE
              box x, y, CSIZE, CSIZE,, c, c
            end if
          end if
        next dir
      next col
    next row
  loop          
end sub

function RandomIntegerInRange(a, b)
  local c, v
  do
    c = b-a+1
    v = a + (b-a+2)*rnd()
    if v >= a and v <= b) then exit do
  loop
  RandomIntegerInRange = v
end function


' ColorConvert.inc -- BASIC include file suitable for inclusion
' wherever color specs using RGB or HSV need to be inter-converted.
' William M Leue 1/7/2021

' Convert an RGB Color to HSV values.
' the RGB values must be in range 0..255; the S and V values will
' be in range 0..1; the H value will be in range 0..360
sub RGB2HSV r, g, b, h as float, s as float, v as float
  local float rp, cmax, cmin, delta

  rp = r/255.0 : gp = g/255.0 : bp = b/255.0
  cmax = max(rp, max(gp, bp))
  cmin = min(rp, min(gp, bp))
  delta = cmax - cmin
  if delta = 0 then
    h = 0
  else if cmax = rp then
    h = 60*(((gp-bp)/delta) MOD 6)
  else if cmax = gp then
    h = 60*(((bp-rp)/delta) + 2)
  else
    h = 60*(((rp-gp)/delta) + 4)
  end if
  if cmax = 0 then
    s = 0
  else 
    s = delta/cmax
  end if
  v = cmax
end sub

' Convert an HSV value to its RGB equivalent
' The S and V values must be in range 0..1; the H value must
' be in range 0..360. The RGB values will be in range 0..255.
sub HSV2RGB h as float, s as float, v as float, r, g, b
  local float i, hh, f, p, q, t, x, c, rp, gp, bp
  c = v*s
  hh = h/60.0
  i = int(hh)
  f = hh - i
  p = v*(1-s)
  q = v*(1-s*f)
  t = v*(1-s*(1-f))
  x = c*(1.0 - hh MOD 2 - 1)
  
  select case i
    case 0
      rp = v : gp = t : bp = p
    case 1
      rp = q : gp = v : bp = p
    case 2
      rp = p : gp = v : bp = t
    case 3
      rp = p : gp = q : bp = v
    case 4
      rp = t : gp = p : bp = v
    case 5
      rp = v : gp = p : bp = q
  end select
  r = rp*255.0 : g = gp*255.0 : b = bp*255.0
end sub

' function to return an RGB color, given the h, s, and v
' values as input parameters. The S and V values must be
' in the range 0..1; the H value must be in the range
' 0..360. The output value will be a 24-bit RGB color.
function GetRGBColor(h as float, s as float, v as float)
  local r, g, b, c

  HSV2RGB h, s, v, r, g, b
  c = RGB(r ,g, b)
  GetRGBColor = c
end function  

' draw color swatches: the left-hand one is the original
' RGB color specified by the user. The right-hand one is
' that color first converted from RGB to HSV, and then
' back again to RGB. The two swatches will be the
' identical color.
sub ShowColors r,g,b,r2,g2,b2
  local c1, c2
  
  c1 = RGB(r,g,b)
  c2 = RGB(r2,g2,b2)
  box 200,200,100,100,1, RGB(WHITE), c1
  box 400,200,100,100,1, RGB(WHITE), c2
end sub

sub ShowPixelColors r,g,b
  local c1, x, y

  box 200,400,100,100,1, RGB(WHITE), c1
  c1 = RGB(r,g,b)
  for y = 401 to 498
    for x = 201 to 298
      pixel x, y, c1
    next x
  next y
end sub   

data -1, 0, 0, -1, 1, 0, 0, 1





