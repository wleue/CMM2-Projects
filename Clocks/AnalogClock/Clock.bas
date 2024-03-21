'Analog clock for CMM2
'Rev 1.0.0 William M Leue 7/20/2020
'Works in CMM2 Mode 1 Graphics only.

'These 4 constants can be changed to meet your needs:
' HCORR: if viewing on a 4:3 monitor, change to 1.0. If viewing on a 16:9 monitor
'   that does not have letter-boxing, set to 0.75
' H24: set to 1 for 24-hour time keeping and clock face
' SHOW_SECONDS: set to 0 to suppress second hand
' CHIME: set to 0 to suppress hour chime sound.

CONST HCORR = 1.0  ' change to 0.75 if 16:9 monitor with no letterboxing
CONST H24 = 0
CONST SHOW_SECONDS = 1
CONST CHIME = 1

' Various other constants that control some aspects of graphic display
CONST CX = 400
CONST CY = 310
CONST RCOLOR = RGB(BLACK)
CONST FCOLOR = RGB(240, 238, 225)
CONST TCOLOR = RGB(RED)
CONST BLEN = 3
CONST CDRD = 8
CONST TXTSCL = 1

DATA "January", "February", "March", "April", "May", "June"
DATA "July", "August", "September", "October", "November", "December"

' variables that depend on 12-hour or 24-hour time display
dim integer prev_sec = -1
dim integer prev_min = -1
dim string MONTHS(12)
dim float hainc
dim integer cfont
dim integer hticks
dim integer hlen, mlen, slen
dim float hinrat, moutrat, minrat
dim integer hmax, hlast, nradius
dim integer radius, oradius
dim integer htcolor

' Chime timing constants and variables
CONST CHIME_INTERVAL = 2000
CONST CHIME_END = 4000
dim integer chcnt = 0
dim integer chgo = 0

'Main Program
cls

' Set various parameters for 12 or 24-hour time keeping
if H24 = 1 then
  radius = 260
  oradius = 270
  hainc = 15.0
  cfont = 4
  moutrat = 0.7
  minrat = 0.65 
  hinrat = 0.95
  htcolor = RCOLOR
  hlen = 130
  mlen = 155
  slen = 165
  hmax = 23
  hlast = 0
  nradius = 225
else
  radius = 250
  oradius = 270
  hainc = 30.0
  cfont = 5
  moutrat = 1.0
  minrat = 0.95
  hinrat = 0.9
  htcolor = TCOLOR
  hlen = 140
  mlen = 170
  slen = 180
  hmax = 11
  hlast = 12
  nradius = 200
end if
MakeMonths
DrawFace
dt$ = date$
DrawDate dt$

' Infinite loop to update hands of the clock
DO
  t$ = time$
  hr$ = MID$(t$, 1, 2)
  mn$ = MID$(t$, 4, 2)
  sc$ = MID$(t$, 7, 2)
  dt$ = date$ 
  hour = VAL(HR$)
  if H24 = 0 and hour > 12 then
    hour = hour - 12
  end if
  min = VAL(mn$)
  sec = VAL(sc$)

  tick = 0
  if sec <> prev_sec then tick = 1
  if min <> prev_min then tick = 1
  if tick = 1 then   
    prev_sec = sec
    prev_min = min
    rhour = hour + min/60.0
    hangle = rhour*hainc + 90
    mangle = min*6.0 + 90
    sangle = sec*6.0 + 90
    if hangle >= 360 then hangle = hangle - 360.0
    UpdateHands hangle, mangle, sangle
  end if
  if min = 0 then
    DrawDate dt$
  end if

  ' Ring hour chimes
  if CHIME = 1 then    
    if min = 0 and sec = 0 and nchime = 0 then
    nchime = hour
      if nchime > 12 then nchime = nchime - 12
      if chcnt = 0 then
        chcnt = nchime
        chwait = 0
      end if
    end if     
    if chcnt > 0 and tick = 1 then
      if chwait > 0 then
        chwait = chwait - 1
      else
        PLAY STOP
        PLAY WAV "chime.wav"
        chwait = 1
        if chcnt = 1 then chwait = 3
        if chcnt > 0 then chcnt = chcnt - 1
      end if
    end if
  end if
LOOP
END


' Make an array of Month names
SUB MakeMonths
  for i = 0 to 11
    READ Months(i)
  next i
END SUB


'Draw the Clock Face (only done once)
SUB DrawFace
  local integer i
  local float angle, ax1, ay1, ax2, ay2
  circle CX, CY, oradius, 1, HCORR, FCOLOR
  circle CX, CY, radius, 1, HCORR, RCOLOR, FCOLOR  
  for i = 0 to hmax
     angle = i*hainc
      ax1 = CX + HCORR*radius*cos(rad(angle))
      ay1 = CY + radius*sin(rad(angle))
      ax2 = CX + hinrat*HCORR*radius*cos(rad(angle))
      ay2 = CY + hinrat*radius*sin(rad(angle))
      line ax1, ay1, ax2, ay2,, htcolor 
    next i
  for i = 0 to 59
    angle = i*6.0
    ax1 = CX + HCORR*moutrat*radius*cos(rad(angle))
    ay1 = CY + moutrat*radius*sin(rad(angle))
    ax2 = CX + minrat*HCORR*radius*cos(rad(angle))
    ay2 = CY + minrat*radius*sin(rad(angle))
    line ax1, ay1, ax2, ay2,, TCOLOR
  next i
  for i = 1 to hmax
    angle = i*hainc + 90
    hval = i
    ax1 = CX - HCORR*nradius*cos(rad(angle))
    ay1 = CY - nradius*sin(rad(angle))
    TEXT ax1, ay1, STR$(hval), "CM", cfont, TXTSCL, RCOLOR, FCOLOR
  next i
  angle = hlast*hainc + 90
  ax1 = CX - HCORR*nradius*cos(rad(angle))
  ay1 = CY - nradius*sin(rad(angle))
  TEXT ax1, ay1, STR$(hlast), "CM", cfont, TXTSCL, RCOLOR, FCOLOR
END SUB

'Draw the Clock Hands (done every second or minute)
SUB UpdateHands hangle, mangle, sangle
  local integer c1, c2
  local float ha, ma, sa
  local integer hvx(3), hvy(3)
  local integer mvx(3), mvy(3)
  static float prev_hangle
  static float prev_mangle
  static float prev_sangle

  for i = 1 to 2  
    if i = 1 then
      ha = prev_hangle
      ma = prev_mangle
      sa = prev_sangle
      c1 = FCOLOR : c2 = FCOLOR
    else 
      ha = hangle
      ma = mangle
      sa = sangle
      prev_hangle = hangle
      prev_mangle = mangle
      prev_sangle = sangle
      c1 = RCOLOR : c2 = TCOLOR
    end if
    if i = 2 or hangle <> prev_hangle then
    hvx(0) = CX - HCORR*HLEN*cos(rad(ha))
    hvx(1) = CX - HCORR*BLEN*cos(rad(ha+90))
    hvx(2) = CX - HCORR*BLEN*cos(rad(ha-90))
    hvy(0) = CY - HLEN*sin(rad(ha))
    hvy(1) = CY - BLEN*sin(rad(ha+90))
    hvy(2) = CY - BLEN*sin(rad(ha-90))
    polygon 3, hvx(), hvy(), c1, c1
    end if
    if i = 2 or mangle <> prev_mangle then
    mvx(0) = CX - HCORR*MLEN*cos(rad(ma))
    mvx(1) = CX - HCORR*BLEN*cos(rad(ma+90))
    mvx(2) = CX - HCORR*BLEN*cos(rad(ma-90))
    mvy(0) = CY - MLEN*sin(rad(ma))
    mvy(1) = CY - BLEN*sin(rad(ma+90))
    mvy(2) = CY - BLEN*sin(rad(ma-90))
    polygon 3, mvx(), mvy(), c1, c1
    end if
    if SHOW_SECONDS = 1 then
      sx = CX - HCORR*SLEN*cos(rad(sa))
      sy = CY - SLEN*sin(rad(sa))
      line CX, CY, sx, sy,, c2
      circle CX, CY, CDRD, 0, HCORR, RCOLOR, RCOLOR
    end if  
  next i
END SUB

'Draw the Date above the Clock
sub DrawDate dt$
  local integer sp1, sp2, nx
  local string fdate$
  sp1 = INSTR(dt$, "-")
  sp2 = INSTR(sp1+1, dt$, "-")
  mx = VAL(MID$(dt$, sp1+1, sp2-1))
  fdate$ = MID$(dt$, 1, sp1-1) + "-" + Months(mx-1) + "-" + MID$(dt$, sp2+1)
  tx = 400 - len(fdate$)*8
  ty = 5
  TEXT tx, ty, fdate$, "LT", 4, 1, RGB(GREEN)
end sub

