' Hitomezashi Stitch Patterns
' Inspired by a Mathologer YouTube vlog entry about same.
' Rev 1.0.0 William M Leue 09-Dec-2021
' Rev 1.0.1 13-Dec-2021 Fixed a bug when size <> 8.

option default integer
option base 1

' Constants
const MIN_SIZE = 8
const MAX_SIZE = 20
const RTYPE = 1
const AVAIL_COLORS = 6
const MIN_COLORS = 2
const MAX_COLORS = 6
const SIZE = 8

const ESC = 27

' Globals
dim gs = SIZE
dim g2 = SIZE\2
dim nc = MIN_COLORS
dim nw = MM.HRES\gs
dim nh = MM.VRES\gs
dim hvals(nw)
dim vvals(nh)
dim clist(AVAIL_COLORS) = (rgb(red), rgb(blue), rgb(green), rgb(yellow), rgb(cyan), rgb(magenta))
dim pwhite = 0
dim pc(MAX_COLORS) = (0, 0, 0, 0, 0, 0)
dim pcb = 0
dim chosen(MAX_COLORS)

' Main program
InitColors
SetupScreen

do
  MakeEdgeValues
  Stitch
  PickRandomColors
  FillColors
  'save image "Hitomezashi.bmp"
  z$ = INKEY$
  do
    z$ = INKEY$
    if asc(z$) = ESC then
      end
    end if
  loop until z$ <> ""
loop
end

' Create arrays of 1's and 0's randomly
sub MakeEdgeValues
  local i
  for i = 1 to nw
    hvals(i) = int(rnd + 0.5)
  next i
  for i = 1 to nh
    vvals(i) = int(rnd + 0.5)
  next i
end sub

' Draw the resulting randomly-generated stitch
sub Stitch
  local i, j, x, y, s
  cls rgb(white)
  'box 0, 0, 800, 600, 5, rgb(black)
  for i = 1 to nh
    y = i*gs
    if vvals(i) = 1 then
      s = 1
    else
      s = 0
    end if
    for j = 1 to nw
      x = (j-1)*gs
      if s then
        line x, y, x+gs, y,, rgb(black)
      end if
      s = 1 - s
    next j
  next i  
  for i = 1 to nw
    x = i*gs
    if hvals(i) = 1 then
      s = 1
    else
      s = 0
    end if
    for j = 1 to nh
      y = (j-1)*gs
      if s then
        line x, y, x, y+gs,, rgb(black)
      end if
      s = 1 - s
    next j
  next i  
end sub

' Make list of actual pixel color values (not same as specified rgb colors)
' Also get actual pixel color for black. Reset the pixel used back to white.
sub InitColors
  local i, c

  pixel g2, g2, rgb(white)
  pwhite = pixel(g2, g2)
  for i = 1 to MAX_COLORS
    c = clist(i)
    pixel g2, g2, c
    pc(i) = pixel(g2, g2)
  next i
  pixel g2, g2, rgb(black)
  pcb = pixel(g2, g2)
  pixel g2, g2, rgb(white) 
end sub

' Choose 'nc' random colors out of the full list, making sure of no duplicate colors
sub PickRandomColors
  do  
    rx = int((rnd()*MAX_COLORS))
  loop until rx >= 1 and rx <= MAX_COLORS
  chosen(1) = pc(rx)
  for i = 2 to nc
    do
      do  
        rx = int((rnd()*MAX_COLORS))
      loop until rx >= 1 and rx <= MAX_COLORS
      ok = 1
      for j = 1 to i-1
        if pc(rx) = chosen(j) then
          ok = 0
        end if
      next j
    loop until ok = 1  
    chosen(i) = pc(rx)
  next i
end sub

' Color in the stitch using flood fills starting at the center
' of each cell. Colors are added in a raster fashion, but of
' course the flood fill propagates in any direction, so only
' cells that are still uncolored get a new color fill; this
' makes the algorithm fast. Although the grid contains closed
' areas, that is an emergent phenomenon, so the program has no
' idea where closed areas are located. Instead, it searches for
' adjacent areas to the left and top that are already filled,
' and uses a different color to fill the current cell.
sub FillColors
  local i, j, x, y, s

  for i = 1 to nh
    y = (i-1)*gs + g2
    for j = 1 to nw
      x = (j-1)*gs + g2
      s = FindPreviousColor(x, y)
      if s > 0 then
        if s < nc then
          pixel fill x, y, chosen(s+1)
        else
          pixel fill x, y, chosen(1)
        end if
      end if
    next j
  next i
end sub

' Function to find closest previous color used by searching
' left or up. Once the previous color is found, then the
' current cell is filled with a different color.
' Returns 0 if cell is already colored. Returns 1 if cell is
' the upper left cell. Otherwise, returns the color index of
' the nearest cell to the left or to the top if at the left edge.
function FindPreviousColor(x, y)
  local i, tx, ty, p, cc

  if pixel(x, y) <> pwhite then
    FindPreviousColor = 0
    exit function
  end if
  if x <= gs then
    if y <= gs then
      FindPreviousColor = 1
      exit function
    end if
    ty = y
    cc = 0
    do while ty > 0
      inc ty, -gs
      p = pixel(x, ty)
      if p <> pwhite then
        for i = 1 to nc
          if p = chosen(i) then
            cc = i
            exit for
          end if
        next i
      end if
      if cc > 0 then exit do
    loop
    FindPreviousColor = cc
    exit function
  end if
  tx = x
  cc = 0
  do while tx > 0
    inc tx, -gs
    p = pixel(tx, y)
    if p <> pwhite then
      for i = 1 to nc
        if p = chosen(i) then
          cc = i
          exit for
        end if
      next i
    end if
    if cc > 0 then exit do
  loop
  FindPreviousColor = cc
end function

' Setup screen for options
sub SetupScreen
  cls
  print "Hitomezashi Stitches are a Japanese embroidery technique that uses a mesh of horizontal"
  print "and vertical stiches in a rigid grid pattern. Here we relax the rigid pattern by making"
  print "two arrays of random 1's and 0's for the horizontal and vertical directions. If a row, for"
  print "instance, has a 1 in the corresponding array index, the row starts with a line and then"
  print "alternates lines and spaces; but if the array location is a 0, then the row starts with a"
  print "space instead of a line, but alternates as before. This is done for both the horizontal"
  print "and vertical dimensions. Surprisingly, what emerges is a tiling of closed, irregular"
  print "areas. 
  print ""
  print "After making the grid, the program fills in the areas with contrasting colors that are"
  print "randomly chosen."
  print ""
  print "In this setup screen, you can choose the number of colors to use."
  print "After you set up, a different pattern will be displayed every time you hit a key. To quit,"
  print "press the Escape key,"
  'print ""
  'do
  '  print "Enter the cell size (";MIN_SIZE;"..";MAX_SIZE;"): ";
  '  input "", gs
  'loop until gs >= MIN_SIZE and gs <= MAX_SIZE
  'g2 = gs\2
  'nw = MM.HRES\gs
  'nh = MM.VRES\gs
  print ""
  do
    print "Enter the number of colors (";MIN_COLORS;"..";MAX_COLORS-1;"): ";
    input "", nc
  loop until nc >= MIN_COLORS and nc <= MAX_COLORS-1
end sub
