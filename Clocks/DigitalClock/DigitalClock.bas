' Digital Clock -- shows current 24-hour time and date. Uses
' simulated 7-segment displays for the time.
' The colors cycle through the color wheel each hour.
' Rev 1.0.0 William M Leue 10-Oct-2023

option default integer
option base 1

' Constants
const NDIGIT = 4
const DH10   = 1
const DH01   = 2
const DM10   = 3
const DM01   = 4

' Simulated 7-segment display params
const NSEGS = 7     ' number of segments
const NFIGS = 18    ' number of expressible figures
const SEGL  = 30    ' half segment length
const SEGW  = 8     ' half segment width
const SGAP  = 2     ' inter-segment gap
const NVERT = 6     ' number of vertices in a segment
const HORIZ = 1     ' horizontal orientation
const VERTL = 2     ' vertical orientation
const MINUS = 16    ' index for minus sign
const BLANK = 17    ' index for blank
const CLSZ  = 10    ' colon size
const DWID  = 2*SEGL+5*SEGW      ' digit bounding box size
const DHGT  = 4*SEGL+6*SEGW+5*SGAP  ' " "
const DSEP  = DWID + int(0.25*DWID) ' digit horizontal separation
const SX    = 150   ' start coordinates of digits
const SY    = 200

const HINC  = 6     ' hue increment per minute

const DTY   = 400   ' text Y coordinate

' Globals
dim t24 = 1
dim ct$ = ""
dim dvalues(NDIGIT)
dim hours = 0
dim minutes = 0
dim seconds = 0
dim fx = 0
dim fy = 0
dim float hue = 0.0
dim float sat = 1.0
dim float brt = 1.0
dim SegActivations(NFIGS, NSEGS)

' Main Program
cls
ReadSegActivations
GetTime
hue = HINC*minutes
HSV2RGB(hue, sat, brt, r, g, b)
c = rgb(r, g, b)
DrawTimeSegs c
z$ = INKEY$
f = 1
do
  z$ = INKEY$
  if asc(z$) = 27 then
    cls
    end
  end if
  GetTime
  pause 1000
  BlinkColon c
  DrawDateTime c
  if seconds = 0 and f = 1 then
    f = 0
    inc hue, HINC
    if hue >= 360.0 then hue = 0.0
    HSV2RGB(hue, sat, brt, r, g, b)
    c = rgb(r, g, b)
    DrawTimeSegs rgb(r, g, b)
  end if
  if seconds > 1 then f = 1
loop
end

' Read the segment activations for the seven-segment display
' for the digits 0..9
sub ReadSegActivations
  local seg, i
  for i = 1 to NFIGS
    for seg = 1 to NSEGS
      read SegActivations(i, seg)
    next seg
  next i
end sub

' Get the current time and break it down into
' integer components.
sub GetTime
  ct$ = time$
  ct$ = ct$ + ":"
  hours = val(field$(ct$, 1, ":"))
  minutes = val(field$(ct$, 2, ":"))
  seconds = val(field$(ct$, 3, ":"))
end sub

' Blink the colon on or off once per second
sub BlinkColon c
  local x, y, xv(4), yv(4), uc
  static con = 1
  if con then
    uc = c
  else
    uc = rgb(black)
  end if 
  x = SX+DSEP+DWID-10
  y = SY+DHGT\3
  xv(1) = x-CLSZ  : yv(1) = y
  xv(2) = x       : yv(2) = y-CLSZ
  xv(3) = x+CLSZ  : yv(3) = y
  xv(4) = x       : yv(4) = y+CLSZ
  polygon 4, xv(), yv(), uc, uc
  y = SY+2*DHGT\3
  xv(1) = x-CLSZ  : yv(1) = y
  xv(2) = x       : yv(2) = y-CLSZ
  xv(3) = x+CLSZ  : yv(3) = y
  xv(4) = x       : yv(4) = y+CLSZ
  polygon 4, xv(), yv(), uc, uc
  con = 1 - con
end sub

' Draw the current time using a 4-digit 7-segment display
sub DrawTimeSegs c
  local d, x, y, v, fw
  x = SX
  y = SY
  fw = 2*SEGL+2*SEGW
  for d = 1 to NDIGIT
    select case d
      case DH10
        v = hours\10
      case DH01
        v = hours mod 10
      case DM10
        v = minutes\10
      case DM01
        v = minutes mod 10
    end select
    DrawSevenSegNumber x, y, v, c
    inc x, DSEP
    if d = 2 then inc x, DWID\2
  next d
end sub

' Display the decimal digit 'num' on a simulated 7-segment display
' at the specified location with the specified color.
' (x, y) - the pixel coordinates of the TLC of the digit.
' num    - the number to be displayed
' c      - the color to use
sub DrawSevenSegNumber x, y, num, c
  local seg, active
  box x-SEGL-2*SEGW-SGAP, y-SEGW, DWID, DHGT,, RGB(black), RGB(black)
  for seg = 1 to NSEGS
    active = SegActivations(num+1, seg)
    select case seg
      case 1    
        if active then DrawSegmentPolygon x, y, HORIZ, c
      case 2
        if active then DrawSegmentPolygon x-SEGL-SEGW-SGAP, y+SEGL+SEGW+SGAP, VERTL, c
      case 3
        if active then DrawSegmentPolygon x+SEGL+SEGW+SGAP, y+SEGL+SEGW+SGAP, VERTL, c
      case 4
        if active then DrawSegmentPolygon x, y+2*SEGL+2*SEGW+2*SGAP, HORIZ, c
      case 5
        if active then DrawSegmentPolygon x-SEGL-SEGW-SGAP, y+3*SEGL+3*SEGW+3*SGAP, VERTL, c
      case 6
        if active then DrawSegmentPolygon x+SEGL+SEGW+SGAP, y+3*SEGL+3*SEGW+3*SGAP, VERTL, c
      case 7
        if active then DrawSegmentPolygon x, y+4*SEGL+4*SEGW+4*SGAP, HORIZ, c
    end select
  next seg
end sub

' Draw the Polygon of a Segment. The (cx,cy) coordinates are the
' center of the segment. SEGL and SEGW are the half-length and
' half-width of the segment. 'dir' is the segment orientation,
' HORIZ or VERTL.
sub DrawSegmentPolygon cx, cy, dir, c
  local xv(NVERT), yv(NVERT)
  if dir = HORIZ then
    xv(1) = cx-SEGL-SEGW : yv(1) = cy
    xv(2) = cx-SEGL      : yv(2) = cy-SEGW
    xv(3) = cx+SEGL      : yv(3) = cy-SEGW
    xv(4) = cx+SEGL+SEGW : yv(4) = cy
    xv(5) = cx+SEGL      : yv(5) = cy+SEGW
    xv(6) = cx-SEGL      : yv(6) = cy+SEGW
  else
    xv(1) = cx           : yv(1) = cy-SEGL-SEGW
    xv(2) = cx+SEGW      : yv(2) = cy-SEGL
    xv(3) = cx+SEGW      : yv(3) = cy+SEGL
    xv(4) = cx           : yv(4) = cy+SEGL+SEGW
    xv(5) = cx-SEGW      : yv(5) = cy+SEGL
    xv(6) = cx-SEGW      : yv(6) = cy-SEGL
  end if
  polygon NVERT, xv(), yv(), c, c
end sub

' Draw Text showing Date and Time
sub DrawDateTime c
  local m$
  m$ = Time$ + " on " + Date$
  text mm.hres\2, DTY, space$(40), "CT", 4
  text mm.hres\2, DTY, m$, "CT", 4,, c, -1
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

' Seven-segment activation list for numbers 0 through 9, A-F, and minus sign and blank
data 1, 1, 1, 0, 1, 1, 1 '0
data 0, 0, 1, 0, 0, 1, 0 '1
data 1, 0, 1, 1, 1, 0, 1 '2
data 1, 0, 1, 1, 0, 1, 1 '3
data 0, 1, 1, 1, 0, 1, 0 '4
data 1, 1, 0, 1, 0, 1, 1 '5
data 1, 1, 0, 1, 1, 1, 1 '6
data 1, 0, 1, 0, 0, 1, 0 '7
data 1, 1, 1, 1, 1, 1, 1 '8
data 1, 1, 1, 1, 0, 1, 1 '9
data 1, 1, 1, 1, 1, 1, 0 'A
data 0, 1, 0, 1, 1, 1, 1 'B
data 1, 1, 0, 0, 1, 0, 1 'C
data 0, 0, 1, 1, 1, 1, 1 'D
data 1, 1, 0, 1, 1, 0, 1 'E
data 1, 1, 0, 1, 1, 0, 0 'F
data 0, 0, 0, 1, 0, 0, 0 '-
data 0, 0, 0, 0, 0, 0, 0 '(blank)


