' Simultate a Galton board to demonstrate how binomial
' distributions arrise naturally from binary choices.
' Rev 1.0.0 William M Leue 16-July-2021

option default integer
option base 1

const NUM_CHANS = 17
const PIN_SEP = 20
const CHAN_WIDTH = PIN_SEP\2
const BALL_RAD = (5*CHAN_WIDTH)/12
const PIN_RAD = 2
const YSTART = 50
const XCENTER = 200
const NUM_BALLS = 100
const INLET_Y = 15
const INLET_RAD = 10
const IDELAY = 60

const INFO_X = 650
const INFO_Y = 20
const GX = 500
const GY = 60
const GWIDTH = 299
const GHEIGHT = 300
const BAR_WIDTH = 10
const BAR_SEP = 7
const BAR_COLOR = rgb(green)
const NYSCALE = 8

const TER_X = GX
const TER_Y = 400
const TER_WIDTH = GWIDTH
const TER_HEIGHT = 170
const MAX_ERR_NUM = 5000

const UP = 128
const DOWN = 129
const ESC = 27

dim passes = 0
dim pin_nums(NUM_CHANS-1, NUM_CHANS-1)
dim chan_counts(NUM_CHANS)
dim total_counts(NUM_CHANS)
dim float ideal_ratio(NUM_CHANS)
dim float total_error(MAX_ERR_NUM)
dim float err_ratio = 1
dim prev_bx = -1
dim prev_by = -1
dim channel_length = 0
dim bigN = 0
dim bigT = 0
dim delay = IDELAY
dim paused = 0

'RTest
ComputeIdealBinomial NUM_CHANS
DrawInfoScreen
DrawBoard
NumberPins
ClearChannels
ClearTotals
SimulateBalls NUM_BALLS
end

' Test to find best dividing point for even chance
' (result: < 0.5 is better than <= 0.5)
sub RTest
  local i, n, hc, tc, diff
  local float pct
  n = 1000000
  hc = 0 : tc = 0
  for i = 1 to n
    if rnd() < 0.5 then
      inc tc
    else
      inc hc
    end if
  next i
  print "hc: ";hc; " tc: ";tc
  diff = hc - tc
  pct = (100.0*abs(diff))/(1.0*n)
  print "diff: ";diff;" pct: ";format$(pct, "%.03g")
  end
end sub

' Draw the static parts of the Galton Board
sub DrawBoard
  local row, col, x, y, p, c, clen, fx1, fx2
  cls
  text 10, 10, "UP ARROW : Faster"
  text 10, 25, "DOWN ARROR : Slower"
  text 10, 40, "ESCAPE: Quit"
  circle XCENTER, INLET_Y, INLET_RAD,,, rgb(white)
  for row = 1 to NUM_CHANS-1
    y = YSTART + (row-1)*PIN_SEP
    x = XCENTER - (0.5*(row-1))*PIN_SEP
    for p = 1 to row
      circle x, y, PIN_RAD,,, rgb(white), rgb(white)
      inc x, PIN_SEP
    next p
  next row
  channel_length = MM.VRES - y - 2*BALL_RAD - 2
  for c = 1 to NUM_CHANS+1
    x = XCENTER - (NUM_CHANS\2)*PIN_SEP + (c-1)*PIN_SEP
    if NUM_CHANS MOD 2 = 1 then x = x - PIN_SEP\2
    line x, y, x, y+channel_length
    if c = 1 then fx1 = x
    if c = NUM_CHANS+1 then fx2 = x
  next c
  line fx1, y+channel_length, fx2, y+channel_length
  PrintStatistics
  DrawGraph
  if passes > 0 then
    DrawErrorHistory
  end if
end sub    

' Number the pins
sub NumberPins
  local row, p
  for row = 1 to NUM_CHANS-1
    for p = 1 to row
      pin_nums(p, row) = row*100 + p
    next p
  next row
  'PrintPins
end sub

' Debugging
sub PrintPins
  local row, p
  for row = 1 to NUM_CHANS-1
    for p = 1 to row
      print #1, "row: ";row; " p: ";p; " pin: ";pin_nums(p, row)      
    next p
  next row
end sub

' Clear the channel counts
sub ClearChannels
  local c
  for c = 1 to NUM_CHANS
    chan_counts(c) = 0
  next c
end sub

' Clear the total counts
sub ClearTotals
  local c
  for c = 1 to NUM_CHANS
    total_counts(c) = 0
  next c
end sub

' Simulate the Drop of N balls
sub SimulateBalls n
  local b, s, sp, dp, row, by, cmd
  local z$ = INKEY$
  do
    if not paused then
      bigN = 0
      for b = 1 to n
        DrawBall 0
        row = 1
        sp = pin_nums(1, 1)
        DrawBall sp
        for row = 1 to NUM_CHANS-1
          ChooseSide sp, dp
          DrawBall dp, by
          sp = dp
          cmd = keydown(1)
          select case cmd
            case DOWN
              delay = delay+5
              if delay > 250 then delay = 250
            case UP
              delay = delay-5
              if delay < 0 then delay = 0
            case ESC
              cls
              end
          end select
        next row
        ChannelDrop dp, by
        inc bigN
        inc bigT
        PrintStatistics
      next b
      ClearChannels
      DrawBoard
      inc passes
    end if
  loop
end sub

' Choose which side of a pin the ball will drop from.
' Returns the index number of the destination pin.
sub ChooseSide sp, dp
  local srow, scol, drow, dcol
  srow = sp\100
  scol = sp - 100*srow
  drow = srow+1
  if rnd() < 0.5 then
    dcol = scol
  else
    dcol = scol+1
  end if
  dp = drow*100 + dcol
end sub

' Simulate the final drop of a ball into an accumulator channel.
sub ChannelDrop dp, by
  local c, ey, bx, row, clen
  row = dp\100
  c = dp - 100*row
  bx = XCENTER - (0.5*(row-1))*PIN_SEP + (c-1)*PIN_SEP
  ey = by + channel_length + BALL_RAD - PIN_SEP - chan_counts(c)*2*BALL_RAD - 2
  if prev_bx > 0 then
    circle prev_bx, prev_by, BALL_RAD,,, rgb(black), rgb(black)
  end if
  circle bx, ey, BALL_RAD,,, rgb(red), rgb(red)
  inc chan_counts(c)
  inc total_counts(c)
end sub

' Draw the ball on top of a pin
sub DrawBall pn, by
  local row, col, bx
  if pn = 0 then
    circle XCENTER, INLET_Y, BALL_RAD,,, rgb(red), rgb(red)
    prev_bx = XCENTER : prev_by = INLET_Y
    pause DELAY
    exit sub
  end if
  row = pn\100
  col = pn - 100*row
  by = YSTART + (row-1)*PIN_SEP - 2*BALL_RAD
  bx = XCENTER - (0.5*(row-1))*PIN_SEP + (col-1)*PIN_SEP
  if prev_bx > 0 then
    circle prev_bx, prev_by, BALL_RAD,,, rgb(black), rgb(black)
  end if
  circle bx, by, BALL_RAD,,, rgb(red), rgb(red)
  prev_bx = bx : prev_by = by
  pause DELAY
end sub

' Primt Various Statistics under the average graph
sub PrintStatistics
  local w$
  text GX, INFO_Y, space$(40)
  w$ = "Passes = " + str$(passes) + " n = " + str$(bigN)
  text GX+GWIDTH\2, INFO_Y, w$, "CT"
  text GX+GWIDTH\2, INFO_Y+20, space$(20), "CT"
  text GX+GWIDTH\2, INFO_Y+20, "T = " + str$(bigT), "CT
end sub

' Draw a bar graph of the average counts per channel
' over multiple runs. This gradually looks more like
' a textbook binomial distribution if you let it run
' for a while. (10 or more passes)
sub DrawGraph
  local tc, tmax, i, blen, x, y, ys, yssep, yb
  local float gscale, maxpct, ipct, ypct, terr, bratio
  line GX, GY, GX, GY+GHEIGHT
  line GX, GY+GHEIGHT, GX+GWIDTH, GY+GHEIGHT
  x = GX + BAR_SEP
  ys = GY + GHEIGHT
  for i = 1 to NUM_CHANS
    text x+BAR_WIDTH\2, ys+2, str$(i), "CT", 7
    inc x, BAR_WIDTH+BAR_SEP
  next i
  text GX+GWIDTH\2, ys+18, "Channel", "CT", 7
  if bigT = 0 then exit sub
  tmax = 0
  maxpct = 0.0
  for i = 1 to NUM_CHANS
    tc = total_counts(i)
    if tc > tmax then 
      tmax = tc
      maxpct = (100.0*tmax)/(1.0*bigT)
    end if
  next i
  ipct = maxpct/(NYSCALE-1)
  yssep = -int((1.0*GHEIGHT)/(1.0*NYSCALE) + 0.5)
  y = ys
  ypct = 0.0
  for i = 0 to NYSCALE
    text GX-2, y, format$(ypct, "%02.3f"), "RM", 7
    inc y, yssep
    inc ypct, ipct
  next i
  text GX-50, ys-GHEIGHT\2, "Percent of Total", "CMV", 7
  gscale = (1.0*GHEIGHT)/(1.0*tmax)
  x = GX + BAR_SEP
  terr = 0.0
  for i = 1 to NUM_CHANS
    blen = int(total_counts(i)*gscale + 0.5)
    bratio = total_counts(i)/(1.0*bigT)
    terr = terr + abs(ideal_ratio(i) - bratio)
    if blen > 0 then
      line x, ys-blen, x, ys, BAR_WIDTH, BAR_COLOR
    end if
    yb = ys - int(gscale*ideal_ratio(i)*bigT + 0.5)
    line x, yb, x+BAR_WIDTH, yb,, rgb(magenta)
    inc x, BAR_WIDTH+BAR_SEP
  next i
  if passes > 0 and passes <= MAX_ERR_NUM then
    total_error(passes) = terr
  end if
end sub

' Compute the ratios to the total for each channel
' for an Ideal Binomial Distribution
sub ComputeIdealBinomial n
  local i, k, zn, sum
  local bcoefs(NUM_CHANS)
  zn = n-1
  sum = 0
  for i = 1 to n
    k = i-1
    bcoefs(i) = NumCombinations(zn, k)
    inc sum, bcoefs(i)
  next i
  for i = 1 to n
    ideal_ratio(i) = (1.0*bcoefs(i))/(1.0*sum)
  next i    
end sub

' Compute the number of combinations of N things taken K at a time.
' This value is n!/(k! * (n-k)!)
function NumCombinations(n, k)
  local nfact, kfact, nmkfact
  nfact = Factorial(n)
  kfact = Factorial(k)
  nmkfact = Factorial(n-k)
  NumCombinations = nfact/(kfact*nmkfact)
end function

' Compute the Factorial of a positive integer
' (Note: this will blow up with n too large, but the numbers
' are ok in this application.)
function Factorial(n)
  local f, i
  f = 1
  for i = 2 to n
    f = f*i
  next i
  Factorial = f
end function

' Draw the Error history over passes
' (stops updating after max number of samples)
sub DrawErrorHistory
  local ys, i, x, y, k, plim
  local prev_x = TER_X
  local prev_y = TER_Y
  local float tscale
  tscale = (1.0*TER_HEIGHT)/total_error(1)
  ys = TER_Y + TER_HEIGHT
  line TER_X, TER_Y, TER_X, ys
  line TER_X, ys, TER_X+TER_WIDTH, ys
  text TER_X+TER_WIDTH\2, ys+3, "Passes (max 5000)", "CT", 7
  text TER_X - 2, TER_Y, "1.0", "RM", 7
  text TER_X - 2, ys, "0.0", "RM", 7
  text TER_X - 10, TER_Y+TER_HEIGHT\2, "Relative Error", "LMV", 7
  plim = min(passes, MAX_ERR_NUM)
  if plim <= GWIDTH then
    err_ratio = 1.0
  else
    err_ratio = (1.0*GWIDTH)/(1.0*plim)
  end if
  for i = 1 to plim
    x = TER_X + 1 + err_ratio*i
    y = int(ys - tscale*total_error(i) - 0.5) 
    line prev_x, prev_y, x, y,, rgb(orange)
    prev_x = x : prev_y = y
  next i
end sub

' Draw the Information Screen
sub DrawInfoScreen
  local z$
  cls
  print "The Galton Board, invented by and named after Francis Galton, (1822-1911)"
  print "demonstrates how a Binomial Distribution can be built up from a series of"
  print "events. A Binomial Distribution is a discrete approximation of a Normal Curve."
  print ""
  print "The Galton Board consists of a pyramid of pins through which beads or balls are"
  print "dropped, ending up in an array of bins. In theory, a ball which hits a pin has"
  print "an even chance of falling to the left or right, although in reality there is always"
  print "some bias toward one side or the other, due to physical imperfections and the momentum"
  print "of the balls."
  print ""
  print "This simulation ignores physics and makes the decision for a ball to go left or right"
  print "an exact 50-50 equal chance. There are 16 rows of pins, each row having one more pin"
  print "than its predecessor. There are 17 bins below the pins for the balls to fall into."
  print "Balls fall into the bins in batches of 100; then the bins are emptied and a new batch"
  print "of 100 balls begins. But the program totals the number of balls for each bin over all"
  print "batches."
  print ""
  print "The distribution of each separate batch is quite varied, although they all show a strong"
  print "central tendency, so that the center bins get many more balls than the outer ones."
  print "This is because there are many different paths that a ball can follow to end up in a"
  print "central bin, but many fewer paths to the outer bins. In fact, there is only ONE path"
  print "that a ball can follow to get to the leftmost or rightmost bin. So the chances of a ball"
  print "making 16 consecutive decisions to go the same direction is very low, about 0.0000153 per"
  print "ball, in fact."
  print ""
  print "The program shows the Galton Board at left. At the top right is the cumulative distribution"
  print "averaged over all the passes of 100 balls. This distribution very slowly converges to nearly"
  print "a perfect Binonmial Distribution, but never quite gets there because of the inherent"
  print "fluctuations in the random ball dropping process. The fluctuations get more and more"
  print "gradual as the number of sets of 100 balls fall, and nearly flat after a few thousand balls."
  print "The graph at lower right shows the history of the relative error of the cumulative"
  print "distribution compared to an ideal Bionmial Distribution."
  print ""
  print "The yellow bars show the current percentage of ball drops for each bin in the cumulative"
  print "distribution. The magenta lines near the top of each yellow bar show the percentages that"
  print "would result in a perfect Binomial Distribution. (These lines bounce up and down a bit at"
  print "the start because the scale varies to keep the most populated bin at full height. But the"
  print "magenta lines settle down quickly as the averages become more stable.)"
  print ""
  print "Press the UP arrow to increase the speed of the simulation; the DOWN key to slow it down."
  print "Press the Escape key to exit the program."
  text MM.HRES\2, 550, "Press any Key to Continue", "CT"
  z$ = INKEY$
  do
    z$ = INKEY$
  loop until z$ <> ""
end sub
