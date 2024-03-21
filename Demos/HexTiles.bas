' Hextiles.bas -- Simple program to tile the screen with hexagons
' that change in colors. The colors morph from saturated random
' colors to darker saturated colors and to pastel and grayscale.
' A version of an After Dark screen saver I wrote many years ago.
' Rev 1.0.0 William M Leue 7/22/2020

CONST HCORR = 1.0  ' change to 0.75 for a 16:9 monitor if no letterboxing
CONST BSIDE = 60    ' big tiles
CONST LSIDE = 30    ' little tiles
CONST SCOLOR = RGB(180, 180, 180)  ' seam color
CONST FADE_CNT = 10
CONST DELAY = 3000
CONST HLRAT = 0.95
CONST HLCOLOR = RGB(WHITE)

dim integer fcolor
dim xv(7), yv(7)

mode 1,16
PrintExplantion

side = BSIDE

do  ' infinite loop alternating between big and small tiles

h = 1.5*HCORR*side
v = 2*side*sin(rad(60)) 
nrows = int(600/(v/2) + 0.5) + 2
ncol = int(800/h + 0.5)

fsat = 1.0 : sat = 1.0
fbrt = 1.0 : brt = 1.0

for seq = 1 to 6  ' various fades for saturation and brightness 

for fl = 1 to FADE_CNT ' each sequence has this many counts 

vstart = -h

for row = 1 to nrows
  if (row MOD 2) = 1 then
    hstart = HCORR*side + HCORR*side/2
  else
    hstart = 0
  end if
  for col = 1 to ncol
    xv(0) = hstart 
    yv(0) = vstart
    xv(1) = hstart + HCORR*side 
    yv(1) = vstart
    xv(2) = xv(1) - HCORR*side*cos(rad(120))
    yv(2) = yv(1) + side*sin(rad(120))
    xv(3) = xv(2) - HCORR*side*cos(rad(60))
    yv(3) = yv(2) + side*sin(rad(60))
    xv(4) = xv(3) - HCORR*side
    yv(4) = yv(3)
    xv(5) = xv(4) + HCORR*side*cos(rad(120))
    yv(5) = yv(4) - side*sin(rad(120))
    xv(6) = xv(0)
    yv(6) = yv(0)
    select case seq
      case 4
        brt = RND()
        hue = RND()*360.0
      case 5
        brt = RND()
        hue = 0
      case else
        hue = RND()*360.0
    end select
    HSV2RGB hue, sat, brt, rf, gf, bf
    fcolor = RGB(rf, gf, bf)
    polygon 6, xv(), yv(), SCOLOR, fcolor 
  
    hstart = hstart + HCORR*2*h
  next col
  vstart = vstart + v/2
next row

pause(DELAY)

select case seq
  case 1
    brt = brt - 0.08*fbrt
  case 2
    brt = brt + 0.08*fbrt
    sat = sat - 0.07*fsat    
  case 3
    sat = sat + 0.07*fsat
  case 4
    sat = sat - 0.1*fsat
  case 5
    hue = 0
    sat = 0
  case 6
    sat = sat + 0.1*fsat
    brt = 1.0
end select

next fl

next seq

cls
if side = BSIDE then
  side = LSIDE
else
  side = BSIDE
end if

loop

end



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

sub PrintExplantion
  cls
  print "Another little demo program. This was written mainly to test some subroutines"
  print "for converting between RGB and HSV color specification systems. The CMM2"
  print "colors are created using RGB values, but it is often more convenient to"
  print "specify colors using HSV (hue, saturation, value) parameters. The program"
  print "creates a field of hexagons whose randomly assigned colors cycle through"
  print "different ranges of HSV values, starting with highly-saturated, bright colors,"
  print "and gradually changing to darker colors, then brighter but less saturated colors,"
  print "then darker and less saturated colors ending in just grayscale values.
  print "Then it changes to smaller hexagons and repeats the cycle. The program runs"
  print "Until terminated by CTRL-C."
  print ""
  print "Press any Key to continue..."
  do : loop until INKEY$ <> ""
  cls
end sub










