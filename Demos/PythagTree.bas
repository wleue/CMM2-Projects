' Pythagoras Tree Fractal
' Based on various published versions on Rosetta
' Rev 1.0.0 William M Leue 7/4/2021
' Rev 1.0.1 7/9/2021 - added fall and winter foliage coloration

option base 1

dim summer(6) = (255, 255, 0, -20,  0,  0)
dim fall(6)   = (87,   43, 0,  14,  8,  0)
dim winter(6) = (87,   39, 0,  14, 18, 21)

limit = 12

cls
print "Enter season 1..3 (summer, fall, winter): ";
input "", season
season = int(season)
if season < 1 then season = 1
if season > 3 then season = 3

select case season
  case 1
    rstart = summer(1) : rinc = summer(4)
    gstart = summer(2) : ginc = summer(5)
    bstart = summer(3) : binc = summer(6)
  case 2
    rstart = fall(1) : rinc = fall(4)
    gstart = fall(2) : ginc = fall(5)
    bstart = fall(3) : binc = fall(6)
  case 3
    rstart = winter(1) : rinc = winter(4)
    gstart = winter(2) : ginc = winter(5)
    bstart = winter(3) : binc = winter(6)
end select

cls
PythagTree 350, 500, 450, 500, 0
save image "PythagTree"
end

sub PythagTree x1, y1, x2, y2, depth
  local float dx, dy, x3, y3, x4, y4, x5, y5
  local float xv(4), yv(4)
  local integer r, c
  if depth > limit then exit sub
  dx = x2-x1 : dy = y1-y2
  x3 = x2-dy : y3 = y2-dx
  x4 = x1-dy : y4 = y1-dx
  x5 = x4 + 0.5*(dx-dy)
  y5 = y4 - 0.5*(dx+dy)
  xv(1) = x1 : yv(1) = y1
  xv(2) = x2 : yv(2) = y2
  xv(3) = x3 : yv(3) = y3
  xv(4) = x4 : yv(4) = y4
  r = rstart + depth*rinc
  g = gstart + depth*ginc
  b = bstart + depth*binc
  c = rgb(r, g, b)
  polygon 4, xv(), yv(), c, c
  PythagTree x4, y4, x5, y5, depth+1
  PythagTree x5, y5, x3, y3, depth+1
end sub


