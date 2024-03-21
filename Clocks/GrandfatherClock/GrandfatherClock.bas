' Grandfather Clock
' Shows the local time, UTC Time, Sunrise, Sunset, and Zodiac sign
' Rev 1.0.0 William M Leue 2/13/2021
' Rev 1.1.0 2/21/2021 - fixed bug in moon angle
' Rev 1.2.0 3/6/2021
'  Changed Moon Orbit radius for better visualization of full moon.
'  Added a dot on the moon scale showing the current moon phase day.
'  Replaced Zodiac sign with today's date.
'  Added altitude correction for sunrise and sunset.
'  Added the program version number to the bottom right clock frame.
' Rev 1.3.0 3/27/2021
'  Rounds sunrise and sunset times to nearest minute.
'  Fixed a bug in time conversions between HMS and decimal formats.
'  Added correction to sunrise and sunset times to remove bias.
'  Increased moon orbit radius slightly to align full moon.
option default integer
option base 1
option angle degrees

const REVISION$ = "1.3.0"

' Statutory Zenith angle
const ZENITH = 90.833
const ALTITUDE_COEF = 4921.3

' Here are the 4 constants whose values you need to change for your
' local location. The corrections are in minutes (float), and are only
' needed in certain locations, or so it seems. For instance, in Sydney,
' Australia, no corrections are needed.
' These corrections work for Albany, NY, USA but will not work for other locations.
' To find the corrections for your location, keep a record of the errors between the
' sunrise and sunset times that the clock shows versus published times online for your
' location. Compute the average error and enter that here in MINUTES as a floating-point value.
const LATITUDE = 42.5025
const LONGITUDE = -73.7526
const UTC_OFFSET = -4
const ELEVATION = 230.0
const SUNRISE_CORR = -1.5333
const SUNSET_CORR = 2.7670002

' Graphic Constants
const FACE_SQUARE_SIDE    = 400
const DIAL_RADIUS         = 180
const DIAL_INNER_RADIUS   = 80
const DIAL_NUMBERS_RADIUS = 170
const MOON_DIAL_RADIUS    = 150
const MOON_DIAL_INNER_RAD = 110
const MOON_RADIUS         = 42
const MOON_ORBIT_RADIUS   = 60
const BORDER_WIDTH        = 20

const MLEN                = DIAL_RADIUS\2
const HLEN                = int(0.7*MLEN)
const TWIDTH              = 2
const HM_HAND_CRAD        = 10
const SECONDS_DIAL_RADIUS = 30
const SECONDS_DIAL_XOFF   = 0
const SECONDS_DIAL_YOFF   = 72
const SECONDS_HAND_CRAD   = 5
const SLEN                = SECONDS_DIAL_RADIUS-6
const HAINC               = 30.0
const MSAINC              = 6.0
const INFO_BOX_WIDTH      = 88
const INFO_BOX_HEIGHT     = 20
const INFO_BOX_BANNER     = 10
const XINFO_BOX_OFFSET    = 193
const YINFO_BOX_OFFSET    = 188

const BCOLOR1 = RGB(160, 136, 69)
const BCOLOR2 = RGB(97, 82, 36)
const FCOLOR = RGB(238, 215, 171)
const DCOLOR = RGB(145, 144, 145)
const LCOLOR = RGB(BLACK)
const WCOLOR = RGB(61, 34, 22)
const SCOLOR = RGB(255, 228, 175)
const RCOLOR = RGB(100, 100, 100)
const HCOLOR = RGB(0, 0, 100)

' Globals
dim MDAYS(12) = (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
dim MNAMES$(12) = ("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
dim float sunrise, sunset, solar_noon, phase, zsign
dim srhms$, sshms$, utchms$
dim zodiacDates(12, 6)
dim zodiacName$(12)
dim cx = MM.HRES\2
dim cy = MM.VRES\2 + 70
dim hour, minute, second
dim prev_hour, prev_minute, prev_second
dim float prev_phase
dim oxv(60), oyv(60)
dim acircDark = 0  ' no sun
dim acircLight = 0 ' midnight sun

dim hour_hand_xv(114)
dim hour_hand_yv(114)
dim thour_hand_xv(114)
dim thour_hand_yv(114)

dim min_hand_xv(60)
dim min_hand_yv(60)
dim tmin_hand_xv(60)
dim tmin_hand_yv(60)

' Main Program
mode 1,16
open "debug.txt" for output as #1
ReadZodiacDates
ReadZodiacNames
ReadClockHandData
if LATITUDE = 0 and LONGITUDE = 0 then
  DrawConfigScreen
end if
UpdateAstronomicalData
DrawFace
page write 0
UpdateTime
end

' Draw the Clock face into Page 1
sub DrawFace
  local s, sy, b, ix, iy, bc, tx, ty, ct, cb
  local i, n, nx, ny, mr1, mr2, mr3, mr5
  local hsx1, hsx2, hsy, bx, by, bw, bh
  local float angle

  page write 1
  cls

  ' Moon Dial
  s = FACE_SQUARE_SIDE
  b = BORDER_WIDTH
  mr1 = MOON_DIAL_INNER_RAD
  mr2 = MOON_DIAL_RADIUS
  mr3 = mr1 + 15
  mr5 = mr1 + 28
  bx = cx-s\2-b
  by = cy-s\2-b
  bw = s+2*b
  bh = s+2*b
  box bx, by, bw, bh,, WCOLOR, WCOLOR
  box cx-s\2-b\2, cy-s\2-b\2, s+b, s+b,, BCOLOR2
  arc cx, cy-s\2, mr1, mr2+b, -90, 90, WCOLOR
  arc cx, cy-s\2, mr2+b\2-1, mr2+b\2, -87, 87, BCOLOR2
  line cx-160, cy-s\2-b\2, cx-s\2, cy-s\2-b\2,, BCOLOR2
  line cx+160, cy-s\2-b\2, cx+s\2, cy-s\2-b\2,, BCOLOR2
  arc cx, cy-s\2, 0, mr1, -90, 90, LCOLOR
  text bx+bw-3, by+bh-1, "v" + REVISION$, "RB", 7,, BCOLOR2, -1
  DrawMoon

  hsx1 = cx-MOON_ORBIT_RADIUS
  hsx2 = cx+MOON_ORBIT_RADIUS
  hsy = cy-s\2
  arc hsx1, hsy, 0, MOON_RADIUS, -90, 90, RGB(BLUE)
  arc hsx2, hsy, 0, MOON_RADIUS, -90, 90, RGB(BLUE)

  ' basic geometry and clock dial  
  box cx-s\2, cy - s\2, s, s,, LCOLOR, FCOLOR
  box cx-s\2, cy - s\2, s, s,, LCOLOR, FCOLOR
  circle cx, cy, DIAL_RADIUS,,, LCOLOR, DCOLOR
  load PNG "RomanClockFace.png", cx-200, cy-200
  arc cx, cy-s\2, mr1, MOON_DIAL_RADIUS, -90, 90, BCOLOR2
  for i = 1 to 7
    n = (i-1)*5
    angle = 180.0 - (i-1)*30.0
    if angle = 180.0 then angle = 174.0
    if angle = 0.0 then
      angle = 6.0
      n = 29
    end if
    if n = 0 then n = 1
    nx = cx + int(mr3*cos(angle) + 0.5)
    ny = cy - s\2 - int(mr3*sin(angle) + 0.5)
    text nx, ny, str$(n), "CT",,, BCOLOR1, BCOLOR2
    tx = cx + int(mr5*cos(angle) + 0.5)
    ty = cy - s\2 - int(mr5*sin(angle) + 0.5)
    circle tx, ty, 4,,, BCOLOR1, BCOLOR1
  next i
  for i = 1 to 29
    angle = (i-1)*6.0
    tx = cx + int(mr5*cos(angle) + 0.5)
    ty = cy - s\2 - int(mr5*sin(angle) + 0.5)
    circle tx, ty, 2,,, BCOLOR1, BCOLOR1
  next i
  arc cx, cy-s\2, mr5, mr5+1, -90, 90, BCOLOR1
  DrawMoonPhasePointer

  ' info boxes
  ct = FCOLOR
  if acircDark = 1 then
    ct = LCOLOR
  end if
  for i = 1 to 4
    select case i
      case 1
        ix = -XINFO_BOX_OFFSET
        iy = -YINFO_BOX_OFFSET
        ban$ = "Sunrise"
      case 2
        ix = XINFO_BOX_OFFSET - INFO_BOX_WIDTH
        iy = -YINFO_BOX_OFFSET
        ban$ = "Sunset"
      case 3
        ix = -XINFO_BOX_OFFSET-4
        iy = YINFO_BOX_OFFSET - INFO_BOX_HEIGHT
        ban$ = "Date"
      case 4
        ix = XINFO_BOX_OFFSET - INFO_BOX_WIDTH
        iy = YINFO_BOX_OFFSET - INFO_BOX_HEIGHT
        ban$ = "UTC"
    end select
    if i = 1 or i = 2 then
      box cx+ix, cy+iy, INFO_BOX_WIDTH, INFO_BOX_HEIGHT,, BCOLOR2, ct
    else if i = 3 then
      box cx+ix, cy+iy, INFO_BOX_WIDTH+8, INFO_BOX_HEIGHT,, BCOLOR2, FCOLOR
    else
      box cx+ix, cy+iy, INFO_BOX_WIDTH, INFO_BOX_HEIGHT,, BCOLOR2, FCOLOR
    end if
    bc = cx+ix + INFO_BOX_WIDTH\2
    text bc, cy+iy-10, ban$, "CT", 7, 1, LCOLOR, BCOLOR1
  next i
  if DEBUG then
    DrawLocationDialog
  end if
end if
page write 0
end sub

' Draw the Moon with the current phase
sub DrawMoon
  local mcx, mcy, dx, dy
  local float mangle
  local s = FACE_SQUARE_SIDE
  mangle = 180.0 - phase*180.0
  mcx = cx + int(MOON_ORBIT_RADIUS*cos(mangle) + 0.5)
  mcy = cy-s\2 - int(MOON_ORBIT_RADIUS*sin(mangle) + 0.5)
  circle mcx, mcy, MOON_RADIUS,,, RGB(YELLOW), RGB(YELLOW)
end sub

' Draw the Moon phase pointer on the moon dial
' (Has to be done after clock face refresh)
sub DrawMoonPhasePointer
  local float mangle, mdr
  local dx, dy
  local s = FACE_SQUARE_SIDE
  mangle = 180.0 - phase*180.0
  mdr = MOON_DIAL_INNER_RAD+29
  dx = cx + int(mdr*cos(mangle) + 0.5)
  dy = cy-s\2 - int(mdr*sin(mangle) + 0.5)
  circle dx, dy, 2,,, RGB(YELLOW), RGB(YELLOW)
end sub

' Update the time about once per second
' The loop here runs forever.
sub UpdateTime
  local h$, m$, s$, z$
  local utchour, cmd, aln
  local float utctime
  static lock = 0
  z$ = INKEY$
  do
    t$ = TIME$
    h$ = MID$(t$, 1, 2)
    m$ = MID$(t$, 4, 2)
    s$ = MID$(t$, 7, 2)
    hour = val(h$) + hour_offset
    if hour > 24 then hour = hour-24
    minute = val(m$)
    second = val(s$)
    utchour = hour - UTC_OFFSET
    if utchour < 0 then utchour = utchour+24
    if utchour > 24 then utchour = utchour-24
    utctime = utchour + minute/60.0 + second/3600.0    
    utchms$ = GetHMSTime$(utctime)
    if hour = 0 and minute = 0 then
      UpdateAstronomicalData
        if hour <> prev_hour and lock = 0 then
          DrawFace
          lock = 1
        end if
      end if
    end if
    if hour = 1 then lock = 0
    if hour > 12 then hour = hour-12
    if hour <> prev_hour or minute <> prev_minute then
      page copy 1 to 0, B
      pause 20
    end if      
    if second <> prev_second then
      DrawHands hour, minute, second
      DrawAstronomicalData
    end if
    prev_hour = hour
    prev_minute = minute
    prev_second = second
    pause 1000
  loop
end sub

' Update the sunrise and sunset times,
' phase of the moon, and sign of the zodiac
' once each day at midnight.
sub UpdateAstronomicalData
  SunCalcs today$, sunrise, sunset
  sunrise = sunrise + SUNRISE_CORR/60.0
  sunset = sunset + SUNSET_CORR/60.0
  srhms$ = GetHMSTime$(sunrise, 1)
  sshms$ = GetHMSTime$(sunset, 1)
  MoonCalcs today$, phase
  zsign = GetZodiacSignIndex(today$)
end sub
    
' Draw the Astronomical data into the info boxes
sub DrawAstronomicalData
  local ix, iy, w, ct, cb
  
  w = INFO_BOX_WIDTH\2
  ix = -XINFO_BOX_OFFSET
  iy = -YINFO_BOX_OFFSET
  ct = LCOLOR
  cb = FCOLOR
  if acircDark = 1 then
    ct = FCOLOR
    cb = LCOLOR
  end if

  ' sunrise
  if acircDark = 1 or acircLight = 1 then
    srhms$ = "None"
    sshms$ = "None"
  end if
  text cx+ix+w, cy+iy+5, srhms$, "CT",,, ct, cb
  
  ' sunset
  ix = XINFO_BOX_OFFSET - INFO_BOX_WIDTH
  iy = -YINFO_BOX_OFFSET
  text cx+ix+w, cy+iy+5, sshms$, "CT",,, ct, cb
  
  ' zodiac sign
  ix = -XINFO_BOX_OFFSET
  iy = YINFO_BOX_OFFSET - INFO_BOX_HEIGHT
  'text cx+ix+w, cy+iy+5, zodiacName$(zsign), "CT",,, LCOLOR, FCOLOR
  text cx+ix+w, cy+iy+5, FormattedDate$(), "CT",,, LCOLOR, FCOLOR

  ' utc time
  ix = XINFO_BOX_OFFSET - INFO_BOX_WIDTH
  iy = YINFO_BOX_OFFSET - INFO_BOX_HEIGHT
  text cx+ix+w, cy+iy+5, utchms$, "CT",,, LCOLOR, FCOLOR
end sub

' Draw Clock Hands
sub DrawHands hour, minute, second
  local hx1, hy1, mx1, my1, sx1, sy1, sx2, sy2, scx, scy
  local float hangle, mangle, sangle
  local hxv(4), hyv(4), mxv(4), myv(4)

  circle cx, cy, HM_HAND_CRAD,,, HCOLOR, HCOLOR
  if second <> prev_second then
    sangle = 90.0 - prev_second*MSAINC
    if sangle >= 360.0 then sangle = 90.0
    if sangle < -360.0 then sangle = 90.0
    scx = cx + SECONDS_DIAL_XOFF
    scy = cy + SECONDS_DIAL_YOFF
    sx1 = scx + int(SLEN*cos(sangle) + 0.5)
    sy1 = scy - int(SLEN*sin(sangle) + 0.5)
    line scx, scy, sx1, sy1,, SCOLOR
    circle cx+SECONDS_DIAL_XOFF, cy+SECONDS_DIAL_YOFF, SECONDS_HAND_CRAD,,, RGB(RED), RGB(RED)
    sangle = 90.0 - second*MSAINC
    if sangle >= 360.0 then sangle = 90.0
    if sangle < -360.0 then sangle = 90.0
    scx = cx + SECONDS_DIAL_XOFF
    scy = cy + SECONDS_DIAL_YOFF
    sx1 = scx + int(SLEN*cos(sangle) + 0.5)
    sy1 = scy - int(SLEN*sin(sangle) + 0.5)
    line scx, scy, sx1, sy1,, RGB(RED)
  end if
    hangle = 90.0 - hour*HAINC - minute/2.0
    if hangle >= 360.0 then hangle = 90.0
    if hangle < -360.0 then hangle = 90.0
    hx1 = cx + int(HLEN*cos(hangle) + 0.5)
    hy1 = cy - int(HLEN*sin(hangle) + 0.5)
    TransformHourHand hangle
    polygon 114, thour_hand_xv(), thour_hand_yv(), HCOLOR, HCOLOR
    'line cx, cy, hx1, hy1,, BCOLOR1
    mangle = 90.0 - minute*MSAINC
    if mangle >= 360.0 then mangle = 90.0
    if mangle < -360.0 then mangle = 90.0
    mx1 = cx + int(MLEN*cos(mangle) + 0.5)
    my1 = cy - int(MLEN*sin(mangle) + 0.5)
    TransformMinuteHand mangle
    polygon 60, tmin_hand_xv(), tmin_hand_yv(), HCOLOR, HCOLOR
    'line cx, cy, mx1, my1,, BCOLOR1
  circle cx, cy, HM_HAND_CRAD\2,,, HCOLOR, HCOLOR
end sub

sub DrawHourHand hangle as float
  local float tangle = hangle + 90.0
  local float r, tx, ty
  local i
  for i = 1 to 60
    tx = hour_hand_xv(i)
    ty = hour_hand_yv(i)
    r = sqr(tx*tx + ty*ty)
    oxv(i) = cx +r*hour_hand_xv(i)
  next i
end sub

' Format today's date in a user-friendly manner
Function FormattedDate$()
  local d$ = today$
  local out$ = ""
  local day, month, year
  local f$
  f$ = MID$(d$, 1, 2)
  day = val(f$)
  f$ = MID$(d$, 4, 2)
  month = val(f$)
  f$ = MID$(d$, 7, 4)
  year = val(f$)
  out$ = str$(day) + "-" + MNAMES$(month) + "-" + str$(year)
  FormattedDate$ = out$
end function

' Sun Calculations from National Almanac Office
' United States Naval Observatory
sub SunCalcs today$, sunrise as float, sunset as float
  local month, day, year, doy
  local float lngHour, trx, tsx, M, L, RA, RAH, sinDec, cosDec, cosH, HR, HS, TR, TS
  local float UTR, UTS, altitude_correction
  local Lquadrant, RAquadrant

  today$ = date$
  DayOfYear today$, month, day, year, doy
  GetLongitudeHour doy, lngHour, trx, tsx

  ' Sun's mean anomoly
  M = (0.9856 * trx) - 3.289

  ' Sun's true longitude
  L = M + (1.916 * sin(M)) + (0.020 * sin(2*M)) + 282.634
  if L < 0.0 then L = L + 360.0
  if L > 360.0 then L = L - 360.0

  'Sun's Right Ascension
  RA = atn(0.91764 * tan(L))
  Lquadrant = int(L/90.0) * 90
  RAquadrant = int(RA/90.0) * 90
  RA = RA +(Lquadrant - RAquadrant)
  RA = RA/15.0

  ' Sun's Declination
  sinDec = 0.39782 * sin(L)
  cosDec = cos(asin(sinDec))

  ' Sun's Local Hour Angle
  cosH = (cos(ZENITH) - (sinDec * sin(LATITUDE)))/(cosDec * cos(LATITUDE))
  acircDark = 0 : acircLight = 0
  if cosH > 1.0 then acircDark = 1
  if cosH < -1.0 then acircLight = 1  

  if acircDark = 0 and acircLight = 0 then
    HR = (360.0 - acos(cosH))/15.0
    HS = acos(cosH)/15.0
 
    ' Local time of Rising and Setting
    TR = HR + RA - (0.06571 * trx) - 6.622
    TS = HS + RA - (0.06571 * tsx) - 6.622

    ' Adjust back to UTC
    UTR = TR - lngHour
    if UTR < 0 then UTR = UTR + 24.0
    if UTR > 24.0 then UTR = UTR - 24.0
    UTS = TS - lngHour
    if UTS < 0 then UTS = UTS + 24.0
    if UTS > 24.0 then UTS = UTS - 24.0

    ' Convert back to local time as per latitude/longitude
    sunrise = UTR + UTC_OFFSET
    if sunrise > 24.0 then sunrise = sunrise - 24.0
    if sunrise < 0.0 then sunrise = sunrise + 24.0
    sunset = UTS + UTC_OFFSET
    if sunset > 24.0 then sunset = sunset - 24.0
    if sunset < 0.0 then sunset = sunset + 24.0

    ' Altitude correction (very minor unless you live on top of Everest)
    altitude_correction = ELEVATION/(60.0*ALTITUDE_COEF)
    sunrise = sunrise - altitude_correction
    sunset = sunset + altitude_correction
    solar_noon = sunrise + 0.5*(sunset-sunrise)
'print #1, "solar_noon: " + str$(solar_noon)
  end if
end sub

' Day of year given today's date in 'date$' format
' Also returns current month, day, year
sub DayOfYear today$, month, day, year, doy
  local leap, i, ndays
  local dy$, mn$, yr$
  local N1, N2, N3
  dy$ = MID$(today$, 1, 2)
  mn$ = MID$(today$, 4, 2)
  yr$ = MID$(today$, 7, 4)
  day = val(dy$)
  month = val(mn$)
  year = val(yr$)
  N1 = (275*month)\9
  N2 = (month + 9)\12
  N3 = (1 + (year - 4*(year\4) + 2) \ 3)
  doy = N1 - (N2*N3) + day - 30
end sub

' Convert longitude to hour value and calc approx rise/set times
sub GetLongitudeHour doy, lngHour as float, trx as float, tsx as float
  lngHour = LONGITUDE/15.0
  trx = doy + ((6.0 - lngHour)/24.0)
  tsx = doy + ((18.0 - lngHour)/24.0)
end sub

' Convert MMBasic time$ format to decimal hours
function GetDecimalTime(ctime$) as float
  local hour, minute, secod
  local float dec_time
  local hr$, mn$, sc$
  if instr(ctime$, ":") = 3 then
    hr$ = MID$(ctime$, 1, 2)
    mn$ = MID$(ctime$, 4, 2)
    sc$ = MID$(ctime$, 7, 2)
  else
    hr$ = MID$(ctime$, 1, 1)
    mn$ = MID$(ctime$, 3, 2)
    sc$ = MID$(ctime$, 6, 2)
  end if
  hour = val(hr$)
  minute = val(mn$)
  second = val(sc$)
  dec_time = hour + (1.0*minute)/60.0 + (1.0*second)/3600.0
  GetDecimalTime = dec_time
end function

' Get time in time$ format from decimal time
' If round <> 0, time is rounded to nearest minute.
function GetHMSTime$(dec_time as float, round)
  local hour, minute, second
  local float fract, fract2
  hour = int(dec_time)
  fract = dec_time - hour
  if round <> 0 then
    minute = int(fract*60.0 + 0.5)
    second = 0
  else
    minute = int(fract*60.0)
    fract2 = fract - minute/60.0
    second = int(fract2*3600.0 + 0.5)
    if second = 60 then second = 0
  end if
  if minute = 60 then
    hour = hour+1
    if hour > 24 then hour = hour-24
    minute = 0
  end if
  hr$ = format$(hour, "%02g")
  mn$ = format$(minute, "%02g")
  sc$ = format$(second, "%02g")
  if round <> 0 then
    GetHMSTime$ = hr$ + ":" + mn$
  else
    GetHMSTime$ = hr$ + ":" + mn$ + ":" + sc$
  end if
end function

' Calculate the Moon Phase for today
sub MoonCalcs today$, phase as float
  local month, day, year, doy
  local float jdate, dsn, nm, dc
  DayOfYear today$, month, day, year, doy
  jdate = JulianDate(month, day, year)
  dsn = jdate - 2451549.5
  nm = dsn/29.53
  phase = nm - int(nm)
  phase = int(phase*100.0)/100.0
  dc = phase*29.53
end sub

' Convert a date to Julian
function JulianDate(month, day, year) as float
  local M, D, Y, A, B, C, E, F
  local float JD
  M = month
  D = day
  Y = year
  if month <= 2 then
    M = M + 12
    Y = Y - 1
  end if
  A = Y\100
  B = A\4
  C = 2-A+B
  E = int(365.25 * (Y+4716))
  F = int(30.6001 * (M+1))
  JD = C+D+E+F -1524.5
  JulianDate = JD
end function

' Read the Zodiac Dates
sub ReadZodiacDates
  local i, j
  for i = 1 to 12
    for j = 1 to 6
      read zodiacDates(i, j)
    next j
  next i
end sub

' Read the Zodiac Names
sub ReadZodiacNames
  local i
  for i = 1 to 12
    read zodiacName$(i)
  next i
end sub

' Function to return the current sign of the zodiac
function GetZodiacSignIndex(today$)
  local month, day, year, doy, i, hit

  DayOfYear today$, month, day, year, doy
  hit = 0
  for i = 1 to 12
    if month = zodiacDates(i, 1) then
      if day >= zodiacDates(i, 2) and day <= zodiacDates(i, 3) then
        hit = i
        exit for
      end if
    end if  
    if month = zodiacDates(i, 4) then
      if day >= zodiacDates(i, 5) and day <= zodiacDates(i, 6) then
        hit = i
        exit for
      end if
    end if
  next i
  if hit = 0 then
    Error "BUG: Zodiac index not found"
  end if
  GetZodiacSignIndex = hit  
end function

' Read Fancy Clock Hand data
' Each data set gets reflected across the X axis
sub ReadClockHandData
  local i, hyc, myc
  hyc = 13
  myc = 10
  for i = 1 to 57
    read hour_hand_xv(i)
    read hour_hand_yv(i)
  next i
  for i = 1 to 57
    hour_hand_xv(57+i) = hour_hand_xv(57-i+1)
    hour_hand_yv(57+i) = 2*hyc - hour_hand_yv(57-i+1)
  next i
  for i = 1 to 114
    hour_hand_yv(i) = hour_hand_yv(i) - hyc
  next i
  for i = 1 to 30
    read min_hand_xv(i)
    read min_hand_yv(i)
  next i
  for i = 1 to 30
    min_hand_xv(30+i) = min_hand_xv(30-i+1)
    min_hand_yv(30+i) = 2*myc - min_hand_yv(30-i+1)
  next i
  for i = 1 to 60
    min_hand_yv(i) = min_hand_yv(i) - myc
  next i
end sub

' Rotate Hour Hand
sub TransformHourHand angle as float
  local hyc = 13
  local hx, hy
  local i, pcx, pcy
  for i = 1 to 114
    pcx = cx + int(hour_hand_xv(i)*cos(angle) + 0.5)
    pcy = cy - int(hour_hand_xv(i)*sin(angle) + 0.5)
    thour_hand_xv(i) = pcx
    thour_hand_xv(i) = pcx + int(hour_hand_yv(i)*cos(angle+90.0) + 0.5)
    thour_hand_yv(i) = pcy - int(hour_hand_yv(i)*sin(angle+90.0) + 0.5)
  next i
end sub

' Rotate Minute Hand
sub TransformMinuteHand angle
  local i, pcx, pcy
  local myc = 10
  for i = 1 to 60
    pcx = cx + int(min_hand_xv(i)*cos(angle) + 0.5)
    pcy = cy - int(min_hand_xv(i)*sin(angle) + 0.5)
    tmin_hand_xv(i) = pcx
    tmin_hand_xv(i) = pcx + int(min_hand_yv(i)*cos(angle+90.0) + 0.5)
    tmin_hand_yv(i) = pcy - int(min_hand_yv(i)*sin(angle+90.0) + 0.5)
  next i
end sub

' Draw the configuration screen
sub DrawConfigScreen
  cls
  text 10, 20,  "Welcome to the Grandfather Clock Configuration Screen"
  text 10, 40,  "====================================================="
  text 10, 80,  "Here you will set the local Latitude, Longitude, "
  text 10, 100, "Time Zone offset from UTC time, and elevation from"
  text 10, 120, "sea level. You only need do this once, and then the"
  text 10, 140, "Sunrise, Sunset, and UTC times that are displayed in"
  text 10, 160, "the corner windows will be correct."
  text 10, 200, "To quickly and easily get your latitude and longitude,"
  text 10, 220, "open Google Maps and find your location. Right-click"
  text 10, 240, "your location, and a pop-up window will show its"
  text 10, 260, "latitude and longitude. Make a note of these values."
  text 10, 280, "Note that latitude in the northern hemisphere is positive,"
  text 10, 300, "and negative in the southern. Longitude is positive"
  text 10, 320, "east of the prime meridian, and negative to its west."
  
  text 10, 360, "You also need to know your local Time Zone offset from"
  text 10, 380, "UTC (aka Greenwich Mean Time). Wikipedia has a map"
  text 10, 400, "showing Time Zone offsets all over the world."

  text 10, 440, "Your local elevation from sea level can be gotten from"
  text 10, 460, "an atlas or a phone app such as IOS Compass."
  text 10, 480, "If you can't find your elevation, just enter zero. The"
  text 10, 500, "elevation makes only tiny changes in times unless you"
  text 10, 520, "live on top of a very tall mountain!"
  text 10, 580, "Press any key to view the next screen."
  z$ = INKEY$
  do
    z$ = INKEY$
  loop until z$ <> ""

  cls
  text 10, 20,  "Once you have collected this information, edit the"
  text 10, 40,  "GrandfatherClock.bas program and change the values"
  text 10, 60,  "of the 3 constants at the top of the program:"
  text 10, 80,  "LATITUDE, LONGITUDE, UTC_OFFSET, and ELEVATION."
  text 10, 100, "Then save the program and restart it.

  text 10, 140, "If your clock is not showing the right time after"
  text 10, 160, "you configure the program, you probably entered the"
  text 10, 180, "wrong UTC time offset. If the sunrise and sunset"
  text 10, 200, "times are wrong by one or more hours, then you"
  text 10, 220, "probably entered the wrong longitude value." 
  text 10, 240, "The sunrise and sunset times will likely be a minute"
  text 10, 260, "or so different from published times, but this is because"
  text 10, 280, "the program does not compensate for atmospheric"
  text 10, 300, "refraction of light.

  text 10, 580, "Press any key to exit this screen."
  z$ = INKEY$
  do
    z$ = INKEY$
  loop until z$ <> ""
  end
end sub

' Month and Day data for Zodiac calc
' Each row: month, start day, end day, month, start day, end day
data 12, 22, 31,  1,  1, 19 ' Capricorn
data  1, 20, 31,  2,  1, 17 ' Aquarius
data  2, 18, 29,  3,  1, 19 ' Pisces
data  3, 20, 31,  4,  1, 19 ' Ares
data  4, 20, 30,  5,  1, 20 ' Taurus
data  5, 21, 31,  6,  1, 20 ' Gemini
data  6, 21, 30,  7,  1, 22 ' Cancer
data  7, 23, 31,  8,  1, 22 ' Leo
data  8, 23, 31,  9,  1, 22 ' Virgo
data  9, 23, 30, 10,  1, 22 ' Libra
data 10, 23, 31, 11,  1, 21 ' Scorpio
data 11, 22, 30, 12,  1, 21 ' Sagittarius

data "Capricorn", "Aquarius", "Pisces", "Ares", "Taurus", "Gemini"
data "Cancer", "Leo", "Virgo", "Libra", "Scorpio", "Sagittarius"

' Data for Ornate Clock Hands

' Hour Hand
data 0,  11, 22, 10, 23,  9, 24,  9, 25,  8, 29,  8, 30,  7, 31,  8, 31,  7, 31,  6, 32,  7
data 33,  8, 34,  8, 35,  8, 36,  9, 37,  9, 38, 10, 39, 10, 40, 10, 41, 10, 42, 11, 43, 11, 44, 11
data 45, 11, 46, 11, 47, 12, 48, 12, 49, 11, 50, 10, 51,  9, 52,  8, 53,  7, 54,  6, 55,  6, 56,  5
data 57,  4, 57,  4, 58,  4, 59,  3, 63,  3, 65,  5, 66,  4, 67,  6, 68,  7, 69,  8, 70,  9, 71,  9
data 72, 10, 73, 10, 74, 11, 75, 11, 76, 11, 77, 12, 79, 12, 88, 12, 93, 12, 103, 12

' Minute Hand
data   0, 6,  20, 6,  21, 6,  24, 5,  25, 4, 32,  4,  37, 5,  40, 6,  43, 7, 44, 7 
data  46, 6,  48, 5,  64, 6,  71, 7,  80, 8, 90,  9, 100, 9, 102, 8, 106, 7 
data 106, 4, 106, 3, 106, 2, 108, 2, 109, 3, 109, 4, 109, 6, 110, 8, 111, 8 
data 112, 9, 147, 9

