' WordClock
' Rev 1.0.0 William M Leue 11-Oct-2023
' Tells the current date and time, including the day
' of the week, in text, and adds an analog seconds clock.
' The colors cycle around the color wheel during each hour.
option default integer
option base 1
option angle degrees

' Constants
const NMONTHS = 12
const NDOW    = 7
const HINC    = 6

' Seconds clock constants
const CY      = 400
const CRAD    = 70
const CDIVS   = 12

' Gregorian Calendar constants
const MISSING_DAYS_SEP_1752 = 11
const SEP = 9
const SEP3 = 3
const CHANGE_YEAR = 1752

' Globals
dim hours = 0
dim minutes = 0
dim seconds = 0
dim day = 0
dim month = 0
dim year = 0
dim monthdays(2, NMONTHS)
dim months$(NMONTHS)
dim wdays$(NDOW)
dim float hue = 0.0
dim float sat = 1.0
dim float brt = 1.0

' Main Program
open "debug.txt" for output as #1
ReadMonthDays
cls
GetDateTime
pseconds = seconds
hue = HINC*minutes
HSV2RGB(hue, sat, brt, r, g, b)
c = rgb(r, g, b)
DrawDateTime c
DrawSecondsClock c
f = 1
z$ = INKEY$
' This loop is timed by polling the current time and looking
' for when the seconds value changes and the minute value
' changes. It could also be done with a timer interupt. The
' 'f' flag prevents early retriggers by adding hysteresis.
' The 'pseconds' variable prevents unecessary redraws of the clock.
do
  pause 200
  z$ = INKEY$
  if asc(z$) = 27 then
    cls
    end
  end if
  GetDateTime
  if seconds = 0 and f = 1 then
    inc hue, HINC
    if hue >= 360.0 then hue = 0.0
    HSV2RGB(hue, sat, brt, r, g, b)
    c = rgb(r, g, b)
    DrawDateTime c
    f = 0
  end if
  if seconds <> pseconds then
    DrawSecondsClock c
    pseconds = seconds
  end if
  if seconds > 1 then
    f = 1
  end if
loop
end

' Read the days per month, month and day names
sub ReadMonthDays
  local i, j
  for j = 1 to 2
    for i = 1 to NMONTHS
      read monthdays(j, i)
    next i
  next j  
  for i = 1 to NMONTHS
    read months$(i)
  next i
  for i = 1 to NDOW
    read wdays$(i)
  next i
end sub 

' Get the current time and date and break them down into
' integer components.
sub GetDateTime
  local ct$, dt$
  ct$ = time$
  ct$ = ct$ + ":"
  hours = val(field$(ct$, 1, ":"))
  minutes = val(field$(ct$, 2, ":"))
  seconds = val(field$(ct$, 3, ":"))
  dt$ = date$
  dt$ = dt$ + "-"
  day = val(field$(dt$, 1, "-"))
  month = val(field$(dt$, 2, "-"))
  year = val(field$(dt$, 3, "-"))
end sub

' Draw the Date and Time
sub DrawDateTime c
  local wd$, mn$, wdx, m$, hour12, ampm$
  local pday = day
  mn$ = months$(month)
  wdx = GetDayOfWeek(day, month, year)
  wd$ = wdays$(wdx)
  Get12HourTime hour12, ampm$
  m$ = "It is " + str$(hour12) + ":" + format$(minutes, "%02g") + " " + ampm$
  text mm.hres\2, 120, space$(60), "CT", 5
  text mm.hres\2, 120, m$, "CT", 5,, c
  m$ = "on "+ wd$ + ", " + mn$ + " " + str$(day) + ", " + str$(year)
  if day <> pday then
    text mm.hres\2, 180, space$(60), "CT", 5
    pday = day
  end if
  text mm.hres\2, 180, m$, "CT", 5,, c
end sub

' Get Day of Week from Month, Day, Year (works for Gregorian calendar)
function GetDayOfWeek(day, month, year)
  local leap, i, dow
  local absolute_day = 0
  inc absolute_day, ((year-1) * 365)
  inc absolute_day, TotalLeapDays(day, month, year)
  leap = IsLeapYear(year) + 1
  for i = 1 to month-1
    inc absolute_day, monthdays(leap, i)
  next i
  inc absolute_day, day-1
  if year > CHANGE_YEAR then
    inc absolute_day, -MISSING_DAYS_SEP_1752
  else
    if (year = CHANGE_YEAR) then
      if (month > SEP) or ((month = SEP) and (day >= SEP3)) then
        inc absolute_day, -MISSING_DAYS_SEP_1752
      end if
    end if
  end if
  dow = ((absolute_day + 6) mod 7) + 1
  GetDayOfWeek = dow
end function

' Compute total leap days from year 1572 to year requested.
' September 3, 1572 was the day the Gregorian calendar was
' instituted in most of the world.
function TotalLeapDays(day, month, year)
  local total = year\4
  if IsLeapYear(year) then inc total, -1
  if year > CHANGE_YEAR then
    inc total, -((year\100) - (CHANGE_YEAR\100))
    inc total, ((year\400) - (CHANGE_YEAR\400))
  end if
  TotalLeapDays = total
end function

' returns 1 if 'year' is a leap year, 0 if not.
function IsLeapYear(year)
  lp = 0
  if (year mod 4) = 0 then
    if ((year mod 100) <> 0) or ((year mod 400) = 0) then
      lp = 1
    end if
  end if
  IsLeapYear = lp
end function

' Get 12-hour time from 24-hour time
sub Get12HourTime hour12, ampm$
  if hours > 12 then
    hour12 = hours - 12
    ampm$ = "p.m."
  else
    hour12 = hours
    ampm$ = "a.m."
  end if
end sub

' Draw a seconds clock at lower center
sub DrawSecondsClock c
  local cx, x1, y1, x2, y2, x3, y3, i, s
  local float angle, ainc
  static float pangle = 0
  cx = mm.hres\2
  circle cx, CY, CRAD,,, c
  circle cx, CY, 3,,, c, c
  ainc = -360.0/CDIVS
  angle = 90.0
  s = 0
  for i = 1 to CDIVS
    x1 = cx + CRAD*cos(angle)
    y1 = CY - CRAD*sin(angle)
    x2 = cx + (CRAD+5)*cos(angle)
    y2 = CY - (CRAD+5)*sin(angle)
    x3 = cx + (CRAD+15)*cos(angle)
    y3 = CY - (CRAD+15)*sin(angle)
    line x1, y1, x2, y2,, c
    text x3, y3, str$(s), "CM",,, c
    inc angle, ainc
    inc s, 5
  next i
  angle = 90.0 - 6*seconds
  x1 = cx + (CRAD-6)*cos(pangle)
  y1 = CY - (CRAD-6)*sin(pangle)
  circle x1, y1, 2,,, rgb(black), rgb(black)
  line cx, CY, x1, y1,, rgb(black)
  pangle = angle
  x1 = cx + (CRAD-6)*cos(angle)
  y1 = CY - (CRAD-6)*sin(angle)
  line cx, CY, x1, y1,, c
  circle x1, y1, 2,,, c, c
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

' Days per month (non-leap and leap years)
data 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31
data 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31
  
data "January", "February", "March", "April", "May", "June"
data "July", "August", "September", "October", "November", "January"
data "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"

