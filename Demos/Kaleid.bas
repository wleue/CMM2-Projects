' Kaleidoscope for CMM2
' Use 8-fold symmetry to give a kaleidoscope effect.
' Rev 1.0.0 William M Leue 11/10/2020
' Rev 1.1.0 11/12/2020 - clip border to a circle; suggested
'  by Peter Mather.
' Rev 1.1.1 11/13/2020 - fixed video glitches, thanks
'  again to Peter Mather!
option base 1
dim xv(3), yv(3)
dim frame = 0

' ISR used to synchronize memory-intensive ops to frame
' refresh.
mode 1, 16, 0, fint
cls

' Set up a clipping circle
page write 2
cls
circle MM.HRES\2, MM.VRES\2, MM.VRES\2-20,,,, RGB(WHITE)
page write 1
cls

' Create a random triangle in the lower right-hand quadrant,
' and then reflect it 7 times across the x and y axes and
' the postitive and negative 45 degree lines to produce
' 8-fold symmetry.
cnt = 0
do
  frame = 0
  hy = MM.VRES\2
  hx = MM.HRES\2
  dx1 = rnd()*hy : dy1 = rnd()*hy
  dx2 = rnd()*hy : dy2 = rnd()*hy
  dx3 = rnd()*hy : dy3 = rnd()*hy  
  xv(1) = hx + dx1 : yv(1) = hy + dy1
  xv(2) = hx + dx2 : yv(2) = hy + dy2
  xv(3) = hx + dx3 : yv(3) = hy + dy3
  c = GetRandomColor()
  Polygon 3, xv(), yv(),, c
  xv(1) = hx - dx1 : yv(1) = hy + dy1
  xv(2) = hx - dx2 : yv(2) = hy + dy2
  xv(3) = hx - dx3 : yv(3) = hy + dy3
  Polygon 3, xv(), yv(),, c
  xv(1) = hx + dx1 : yv(1) = hy - dy1
  xv(2) = hx + dx2 : yv(2) = hy - dy2
  xv(3) = hx + dx3 : yv(3) = hy - dy3
  Polygon 3, xv(), yv(),, c
  xv(1) = hx - dx1 : yv(1) = hy - dy1
  xv(2) = hx - dx2 : yv(2) = hy - dy2
  xv(3) = hx - dx3 : yv(3) = hy - dy3
  Polygon 3, xv(), yv(),, c
  s = dx1 : dx1 = dy1 : dy1 = s
  s = dx2 : dx2 = dy2 : dy2 = s
  s = dx3 : dx3 = dy3 : dy3 = s
  xv(1) = hx + dx1 : yv(1) = hy + dy1
  xv(2) = hx + dx2 : yv(2) = hy + dy2
  xv(3) = hx + dx3 : yv(3) = hy + dy3
  Polygon 3, xv(), yv(),, c
  xv(1) = hx - dx1 : yv(1) = hy + dy1
  xv(2) = hx - dx2 : yv(2) = hy + dy2
  xv(3) = hx - dx3 : yv(3) = hy + dy3
  Polygon 3, xv(), yv(),, c
  xv(1) = hx + dx1 : yv(1) = hy - dy1
  xv(2) = hx + dx2 : yv(2) = hy - dy2
  xv(3) = hx + dx3 : yv(3) = hy - dy3
  Polygon 3, xv(), yv(),, c
  xv(1) = hx - dx1 : yv(1) = hy - dy1
  xv(2) = hx - dx2 : yv(2) = hy - dy2
  xv(3) = hx - dx3 : yv(3) = hy - dy3
  Polygon 3, xv(), yv(),, c
  
  ' clip image to a circle
  ' the do loop is needed to prevent video glitches
  ' by synchronizing both the page and_pixels and the
  ' page copy to the vertical blanking interval. This
  ' inner loop terminates when the vblank ISR sets the
  ' frame to 1. (Thanks, Peter Mather!)
  do while frame = 0
    page and_pixels 2,1,1
    page copy 1 to 0,b
  loop
  frame = 0 ' both this and the one at the top are needed!
  pause 50
  inc cnt
loop
end

' ISR for frame switch
sub fint
  frame = 1
end sub

' returns a random RGB color, generated originally as
' an HSV color with varying saturation and brightness and
' a random hue, then converted to 16-bit RGB.
function GetRandomColor()
  local float hue, sat, brt
  local r, g, b
  static sw = 0
  if rnd() < 0.01 then sw = 1-sw
  if sw then
    brt = 1.0
    sat = 1.0
  else
    brt = 0.4 + rnd()*0.6
    sat = 0.2 + rnd()*0.8
  end if
  hue = rnd()*360.0
  HSV2RGB hue, sat, brt, r, g, b
  GetRandomColor = RGB(r,g,b)
end function

' Convert an RGB color to HSV notation. The RGB components must
' be in the range 0,,255. The S and V values will
' be in range 0..1; the H value will be in range 0..360
sub RGB2HSV r, g, b, h, s, v
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
' The H and V values must be in range 0..1; the H value must
' be in range 0..360. The RGB values will be in range 0..255.
sub HSV2RGB h, s, v, r, g, b
  hh = h/60.0
  i = int(hh)
  f = hh - i
  p = v*(1-s)
  q = v*(1-s*f)
  t = v*(1-s*(1-f))
  
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
  r = rp*255 : g = gp*255 : b = bp*255
end sub

