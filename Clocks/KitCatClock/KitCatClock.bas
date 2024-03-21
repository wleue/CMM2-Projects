' A clock v 1.1.0

option default integer
option base 1

' overall image size
const KWIDTH = 181
const KHEIGHT = 555
const BASE_Y = 372

' Eyes
const EYE_Y = 100
const EYE_RAD = 28
const EYE_SPAC = 80

const PUPIL_HEIGHT = int(1.6*EYE_RAD + 0.5)
const PUPIL_WIDTH = int(0.5*EYE_RAD + 0.5)
const LPUPIL_MINX = int(-0.5*EYE_RAD + 0.5)
const LPUPIL_MAXX = int(0.4*EYE_RAD + 0.5)
const RPUPIL_MINX = int(-0.4*EYE_RAD + 0.5)
const RPUPIL_MAXX = int(0.5*EYE_RAD + 0.5)
const PUPIL_NVTX = 15

' Center of Face
const HCENTER_X = KWIDTH\2+2
const HCENTER_Y = 280
const HCENTER_RAD = 6

' Size of Oval Face
const FACE_W = 98
const FACE_H = 145

' Tail constants
const TAIL_Y = 395
const TAIL_H = 165
const TAIL_PENDULUM_LEN = 255
const TAIL_PIVOT_Y = HCENTER_Y
const TAIL_MAX_ANGLE = 15.0
const TAIL_WIDTH = 30
const NUM_TAIL_OFFSETS = 19
const NUM_LEFT_OFFSETS = 10
const NUM_RIGHT_OFFSETS = 9
const NSTNS = 12

' Hands
const HOUR_LEN  =  int(FACE_W/2.0 - 30.0)
const MIN_LEN   =  int(FACE_W/2.0 - 20)
const HOUR_FLEN = 20
const MIN_FLEN  = 20
const HAND_W1   =  8
const HAND_W2   = 18
const HAND_AD1  = 3
const HAND_AD2  = 2

' Animation
const NUM_PHASES = 10
const DELAY = 60

' Globals
dim x, y, center_x, center_y
dim HandLengths(4) = (HOUR_LEN, HOUR_FLEN, MIN_LEN, MIN_FLEN)
dim TailOffsets(NSTNS, 2)
dim float hour_angle
dim float minute_angle

mode 1,16

' Read Tail Offsets
for i = 1 to NSTNS
  read TailOffsets(i, 1)
next i
for i = 1 to NSTNS
  read TailOffsets(i, 2)
next i

' Load and Edit static image into page 2
x = MM.HRES\2 - KWIDTH\2
y = MM.VRES\2 - KHEIGHT\2
page write 2
load png "KitCatClock.png", x, y
box x, y+BASE_Y, KWIDTH, 200,, RGB(BLACK), RGB(BLACK)

' Set a periodic interrupt for updating the clock hands
settick 1000, UpdateClock

' And do the animation. The delay is timed so that
' the full animation takes about 1 second.
UpdateClock
do
  for i = 1 to NUM_PHASES
    DrawCat i
    pause DELAY
  next i
  for i = NUM_PHASES to 1 step -1
    DrawCat i
    pause DELAY
  next i
loop
end

' Draw the Cat with the specified animation phase
sub DrawCat phase
    page write 1
    cls
    page copy 2 to 1
    ctime = 2.5
    DrawEye 1, i
    DrawEye 2, i
    DrawTail i
    DrawHands hour_angle, minute_angle
    page write 0
    page copy 1 to 0, B
end sub

' Update the clock hands to show the current time
sub UpdateClock
  local hour, minute
  static prev_minute = 0
  local float rhour

  t$ = time$
  hr$ = MID$(t$, 1, 2)
  mn$ = MID$(t$, 4, 2)
  hour = val(hr$)
  minute = val(mn$)
  if hour > 12 then hour = hour-12
  if minute <> prev_minute then
    prev_minute = minute
    rhour = hour + minute/60.0
    hour_angle = (450.0 - rhour*30.0) mod 360.0
    if hour_angle > 360.0 then hour_angle = hour_angle - 360.0
    minute_angle = (450.0 - minute*6.0) mod 360.0
  end if
end sub

' Draw the clock hands
sub DrawHands hangle as float, mangle as float
  center_x = x+HCENTER_X
  center_y = y+HCENTER_Y
  circle x+HCENTER_X, y+HCENTER_Y, HCENTER_RAD,,, RGB(WHITE), RGB(WHITE)
  
  MakeHandPolygon 1, hangle
  MakeHandPolygon 2, mangle
end sub

' Create and draw a polygon for a clock hand
sub MakeHandPolygon which, angle as float
  local bxc, byc, bx1, bx2, nxc, nx1, nx2, fxc, fx1, fx2
  local inx1, inx2, iny1, iny2, clen, hlen, flen
  local xv(10), yv(10)

  clen = HCENTER_RAD
  hlen = HandLengths(2*which-1)
  flen = HandLengths(2*which)

  bxc = int(center_x + clen*cos(rad(angle)) + 0.5)
  byc = int(center_y - clen*sin(rad(angle)) + 0.5)
  bx1 = int(center_x + clen*cos(rad(angle-HAND_AD1)) + 0.5)
  by1 = int(center_y - clen*sin(rad(angle-HAND_AD1)) + 0.5)
  bx2 = int(center_x + clen*cos(rad(angle+HAND_AD1)) + 0.5)
  by2 = int(center_y - clen*sin(rad(angle+HAND_AD1)) + 0.5)
  nxc = int(center_x + hlen*cos(rad(angle)) + 0.5)
  nyc = int(center_y - hlen*sin(rad(angle)) + 0.5)
  nx1 = int(center_x + hlen*cos(rad(angle-HAND_AD1)) + 0.5)
  ny1 = int(center_y - hlen*sin(rad(angle-HAND_AD1)) + 0.5)
  nx2 = int(center_x + hlen*cos(rad(angle+HAND_AD1)) + 0.5)
  ny2 = int(center_y - hlen*sin(rad(angle+HAND_AD1)) + 0.5)
  inx1 = int(center_x + hlen*cos(rad(angle-HAND_AD2)) + 0.5)
  iny1 = int(center_y - hlen*sin(rad(angle-HAND_AD2)) + 0.5)
  inx2 = int(center_x + hlen*cos(rad(angle+HAND_AD2)) + 0.5)
  iny2 = int(center_y - hlen*sin(rad(angle+HAND_AD2)) + 0.5)
  fxc = int(center_x + (hlen+flen)*cos(rad(angle)) + 0.5)
  fyc = int(center_y - (hlen+flen)*sin(rad(angle)) + 0.5)
  fx1 = int(center_x + (hlen+flen)*cos(rad(angle-HAND_AD2)) + 0.5)
  fy1 = int(center_y - (hlen+flen)*sin(rad(angle-HAND_AD2)) + 0.5)
  fx2 = int(center_x + (hlen+flen)*cos(rad(angle+HAND_AD2)) + 0.5)
  fy2 = int(center_y - (hlen+flen)*sin(rad(angle+HAND_AD2)) + 0.5)
  
  xv(1) = bx1  : yv(1) = by1
  xv(2) = nx1  : yv(2) = ny1
  xv(3) = inx1 : yv(3) = iny1
  xv(4) = fx1  : yv(4) = fy1
  xv(5) = fxc  : yv(5) = fyc
  xv(6) = fx2  : yv(6) = fy2
  xv(7) = inx2 : yv(7) = iny2
  xv(8) = nx2  : yv(8) = ny2
  xv(9) = bx2  : yv(9) = by2
  xv(10) = xv(1) : yv(10)  = yv(1)
  polygon 10, xv(), yv(), RGB(WHITE), RGB(WHITE)
end sub

sub DrawEye which, phase
  local xc, pxc, pyc, xmin, xmax, px, lx, rx
  local float xinc
  local xv(17), yv(17)
  xc = x + KWIDTH\2
  lx = xc - EYE_SPAC\2 + 3
  rx = xc + EYE_SPAC\2
  select case which
    case 1
      pxc = lx
      xmin = LPUPIL_MINX
      xmax = LPUPIL_MAXX
    case 2
      pxc = rx
      xmin = RPUPIL_MINX
      xmax = RPUPIL_MAXX
  end select
  circle pxc, EYE_Y, EYE_RAD,,, RGB(WHITE), RGB(WHITE)

  xinc = (1.0*(xmax-xmin))/(1.0*NUM_PHASES)
  px = pxc + xmin + int(xinc*phase + 0.5)

  xv(1) = px                            : yv(1)  = EYE_Y + int(0.5*PUPIL_HEIGHT+0.5) 
  xv(2) = px-int(0.1*PUPIL_WIDTH+0.5)   : yv(2)  = EYE_Y + int(0.4*PUPIL_HEIGHT+0.5)
  xv(3) = px-int(0.3*PUPIL_WIDTH+0.5)   : yv(3)  = EYE_Y + int(0.3*PUPIL_HEIGHT+0.5)
  xv(4) = px-int(0.45*PUPIL_WIDTH+0.5)  : yv(4)  = EYE_Y + int(0.1*PUPIL_HEIGHT+0.5)
  xv(5) = px-int(0.5*PUPIL_WIDTH+0.5)   : yv(5)  = EYE_Y
  xv(6) = px-int(0.45*PUPIL_WIDTH+0.5)  : yv(6)  = EYE_Y - int(0.1*PUPIL_HEIGHT+0.5)
  xv(7) = px-int(0.3*PUPIL_WIDTH+0.5)   : yv(7)  = EYE_Y - int(0.3*PUPIL_HEIGHT+0.5)
  xv(8) = px-int(0.1*PUPIL_WIDTH+0.5)   : yv(8)  = EYE_Y - int(0.4*PUPIL_HEIGHT+0.5)

  
  xv(9)  = px+int(0.1*PUPIL_WIDTH+0.5)  : yv(9)  = EYE_Y + int(0.4*PUPIL_HEIGHT+0.5)
  xv(10) = px+int(0.3*PUPIL_WIDTH+0.5)  : yv(10) = EYE_Y + int(0.3*PUPIL_HEIGHT+0.5)
  xv(11) = px+int(0.45*PUPIL_WIDTH+0.5) : yv(11) = EYE_Y + int(0.1*PUPIL_HEIGHT+0.5)
  xv(12) = px+int(0.5*PUPIL_WIDTH+0.5)  : yv(12) = EYE_Y
  xv(13) = px+int(0.45*PUPIL_WIDTH+0.5) : yv(13) = EYE_Y - int(0.1*PUPIL_HEIGHT+0.5)
  xv(14) = px+int(0.3*PUPIL_WIDTH+0.5)  : yv(14) = EYE_Y - int(0.3*PUPIL_HEIGHT+0.5)
  xv(15) = px+int(0.1*PUPIL_WIDTH+0.5)  : yv(15) = EYE_Y - int(0.4*PUPIL_HEIGHT+0.5)
  xv(16) = px                           : yv(16) = EYE_Y - int(0.5*PUPIL_HEIGHT+0.5)
  xv(17) = xv(1)                        : yv(17) = yv(1)
  
  page write 1
  polygon 17, xv(), yv(), RGB(BLACK), RGB(BLACK)
end sub

' Draw the tail at the specified angle
sub DrawTail phase
  local px, py, i, tcx, tcy, bx, by
  local float angle, ainc, tangle, pct, sangle
  local tvl = 160
  static prev_tx1 = 0
  static prev_ty1 = 0
  static prev_bx = 0
  local slen = 15
  local sy
  static prev_ax1(NSTNS), prev_ay1(NSTNS), prev_ax2(NSTNS), prev_ay2(NSTNS)

  px = x+HCENTER_X : py = y+TAIL_PIVOT_Y
  ainc = 2.0*TAIL_MAX_ANGLE/(1.0*(NUM_PHASES-1))
  angle = -TAIL_MAX_ANGLE + (phase-1)*ainc
  tangle = 270.0 + angle
  bx = px + (BASE_Y-TAIL_PIVOT_Y)*cos(rad(tangle))
  by = BASE_Y
  tx1 = px + TAIL_PENDULUM_LEN*cos(rad(tangle))
  ty1 = py - TAIL_PENDULUM_LEN*sin(rad(tangle))
  prev_tx1 = tx1 : prev_ty1 = ty1 : prev_bx = bx
  for i = 1 to NSTNS
    pct = (1.0*i)/(1.0*NSTNS)
    sy = BASE_Y + int(pct*tvl + 0.5)    
    sangle = tangle - 90.0
    sx = int((1.0-pct)*bx) + int(pct*tx1 + 0.5)
    ax1 = sx + TailOffsets(i, 1) + int(slen*cos(rad(sangle)) + 0.5)
    ay1 = sy - int(slen*sin(rad(sangle)) + 0.5)
    ax2 = sx + TailOffsets(i, 2) - int(slen*cos(rad(sangle)) + 0.5)
    ay2 = sy + int(slen*sin(rad(sangle)) + 0.5)
    if i > 1 then
      if prev_ax1(i-1) <> 0 then
        line prev_ax1(i-1), prev_ay1(i-1), ax1, ay1, 2, RGB(60, 63, 60)
        line prev_ax1(i-1)+1, prev_ay1(i-1), ax1+1, ay1, 2, RGB(60, 63, 60)
        line prev_ax1(i-1)+2, prev_ay1(i-1), ax1+2, ay1, 2, RGB(60, 63, 60)
        line prev_ax2(i-1), prev_ay2(i-1), ax2, ay2, 2, RGB(60, 63, 60)
        line prev_ax2(i-1)+1, prev_ay2(i-1), ax2+2, ay2, 2, RGB(60, 63, 60)
        line prev_ax2(i-1)+2, prev_ay2(i-1), ax2+2, ay2, 2, RGB(60, 63, 60)
      end if
    end if
    prev_ax1(i) = ax1 : prev_ay1(i) = ay1
    prev_ax2(i) = ax2 : prev_ay2(i) = ay2
  next i
end sub

' Tail offset table
data -2, 1, 3, 5, 3, 1, 0, 4, 9, 13, 17, 20
data 2, 2, 3, 5, 3, 1, 1, 2, 2, 3, 0, -8

