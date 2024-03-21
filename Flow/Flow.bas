' Flow Puzzle
' Rev 1.0.0 William M Leue 17-Jan-2024
' Rev 1.0.1 Various minor tweaks. 29-Jan-2024

option default integer
option base 1

' Constants
const CSIZE     = 40
const TSIZE     = 8
const SRAD      = CSIZE/4
const TRAD      = TSIZE/4
const PTHICK    = 4
const MIN_ORDER = 4
const MAX_ORDER = 10

const CCOLOR    = rgb(gray)
const BGCOLOR   = rgb(50, 50, 50)

' Commands
const HOME   = 134
const UP     = 128
const DOWN   = 129
const LEFT   = 130
const RIGHT  = 131
const SPACE  = 32
const DELETE = 127
const BACK   = 8
const RSTRT  = asc("R")
const ENTER  = 13
const ESC    = 27

' Codes
const T_NONE = 0
const T_DOT  = 1
const T_PIPE = 2
const T_BOTH = 4

const NORTH  = 1
const SOUTH  = 2
const EAST   = 3
const WEST   = 4
const NDIRS  = 4

' Thumbnail Chooser
const MAXROWS = 5
const MAXCOLS = 7
const MAXPUZ  = MAXROWS*MAXCOLS

' Globals
dim order = 2
dim grid(2, 2, 5)
dim cx = 0
dim cy = 0
dim arow    = 0
dim acol    = 0
dim pcolor  = 0
dim pmode   = 0
dim running = 0
dim fn$     = ""
dim chosen_pnum = 0
dim lname$  = ""

' Main Program
'open "debug.txt" for output as #1
SetPallette
ShowHelp
do
  cls
  DrawDifficultyChoice level$, lname$
  toff = 20
  DrawThumbnails toff, toff, mm.hres-toff-1, mm.vres-toff-1, lname$, level$, fn$
  LoadPuzzle fn$
  SetUpBoard
  DrawPuzzle
  DrawPlevel lname$, chosen_pnum
  HandleEvents
loop
end

' Set up the Pallette for dot and pipe colors
' 8-bit colors are ok for first 7 but last three need tweaks.
sub SetPallette
  MAP(1) = rgb(red)
  MAP(2) = rgb(green)
  MAP(3) = rgb(blue)
  MAP(4) = rgb(yellow)
  MAP(5) = rgb(cyan)
  MAP(6) = rgb(magenta)
  MAP(7) = rgb(pink)
  MAP(8) = rgb(255, 127, 0)
  MAP(9) = rgb(150, 150, 255)
  MAP(10) = rgb(75, 25, 0)
  MAP SET
end sub

' Set Up the Board
sub SetUpBoard
  local i, row, col
  cx = mm.hres\2 - (order*CSIZE)\2
  cy = mm.vres\2 - (order*CSIZE)\2
  arow = 1 : acol = 1
  running = 1
end sub

' Draw the Puzzle
sub DrawPuzzle
  local row, col
  cls
  box cx, cy, order*CSIZE, order*CSIZE,, CCOLOR, BGCOLOR
  for row = 1 to order
    for col = 1 to order
      DrawCell row, col, 0
    next col
  next row
  DrawInstructions
end sub  

' Draw a Cell in the Grid
sub DrawCell row, col, hilite
  local x, y, v, c, ec, fc, t
  v = grid(row, col, 1)
  t = v\100
  c = v mod 100
  ec = rgb(gray)
  if hilite = 1 then
    ec = rgb(yellow)
  else if hilite = 2 then
    ec = MAP(c)
  else
    ec = rgb(gray)
  end if
  x = CX + (col-1)*CSIZE
  y = CY + (row-1)*CSIZE
  box x, y, CSIZE, CSIZE,, ec, fc
  if (t = 1) or (t = 3) then
    circle x+CSIZE\2, y+CSIZE\2, SRAD,,, MAP(c), MAP(c)
  end if
  if (t = 2) or (t = 3) then
    DrawPipe row, col, MAP(c)
  end if
end sub

' Draw a Puzzle Thumbnail
sub DrawPThumbnail tx, ty
  local row, col
  box tx, ty, order*TSIZE, order*TSIZE,, CCOLOR, BGCOLOR
  for row = 1 to order
    for col = 1 to order
      DrawTCell tx, ty, row, col
    next col
  next row
end sub  

' Draw a Cell in the Thumbnail
sub DrawTCell tx, ty, row, col
  local x, y, v, c, ec, fc, t
  v = grid(row, col, 1)
  t = v\100
  c = v mod 100
  ec = rgb(gray)
  if hilite = 1 then
    ec = rgb(yellow)
  else if hilite = 2 then
    ec = MAP(c)
  else
    ec = rgb(gray)
  end if
  x = tx + (col-1)*TSIZE
  y = ty + (row-1)*TSIZE
  box x, y, TSIZE, TSIZE,, ec, fc
  if t = 1 then
    circle x+TSIZE\2, y+TSIZE\2, TRAD,,, MAP(c), MAP(c)
  end if
end sub

' Hilite the current active cell
sub HiliteCell row, col
  local x, y
  static prev_row = 0
  static prev_col = 0
  if prev_row > 0 then
    DrawCell prev_row, prev_col, 0
  end if    
  DrawCell row, col, 1
  prev_row = row : prev_col = col
end sub

' Draw a Pipe Segment
sub DrawPipe row, col, c
  local pr, pc, nr, nc, pw, ph, x, y, xf, yf
  pr = grid(row, col, 2)
  pc = grid(row, col, 3)
  if pr > 0 then
    if row-pr > 0 then
      x = CX + (pc-1)*CSIZE + CSIZE\2
      y = CY + (pr-1)*CSIZE + CSIZE\2
      pw = PTHICK    : ph = CSIZE + PTHICK\2
      xf = -PTHICK\2 : yf = -PTHICK\2
    else if row-pr < 0 then
      x = CX + (col-1)*CSIZE + CSIZE\2
      y = CY + (row-1)*CSIZE + CSIZE\2
      pw = PTHICK    : ph = CSIZE + PTHICK
      xf = -PTHICK\2 : yf = -PTHICK\2
    else if row-pr = 0 then
      if col-pc > 0 then
        x = CX + (pc-1)*CSIZE + CSIZE\2
        y = CY + (pr-1)*CSIZE + CSIZE\2
        pw = CSIZE+PTHICK\2     : ph = PTHICK
        xf = 0                  : yf = -PTHICK\2
      else
        x = CX + (col-1)*CSIZE + CSIZE\2
        y = CY + (row-1)*CSIZE + CSIZE\2
        pw = CSIZE     : ph = PTHICK
        xf = 0         : yf = -PTHICK\2
      end if
    end if
    box x+xf, y+yf, pw, ph,, c, c 
  end if
  nr = grid(row, col, 4)
  nc = grid(row, col, 5)
  if nr > 0 then
    if row-nr > 0 then
      x = CX + (nc-1)*CSIZE + CSIZE\2
      y = CY + (nr-1)*CSIZE + CSIZE\2
      pw = PTHICK    : ph = CSIZE + PTHICK\2
      xf = -PTHICK\2 : yf = 0
    else if row-nr < 0 then
      x = CX + (col-1)*CSIZE + CSIZE\2
      y = CY + (row-1)*CSIZE + CSIZE\2
      pw = PTHICK    : ph = CSIZE + PTHICK\2
      xf = -PTHICK\2 : yf = -PTHICK\2
    else if row-nr = 0 then
      if col-nc > 0 then
        x = CX + (nc-1)*CSIZE + CSIZE\2
        y = CY + (nr-1)*CSIZE + CSIZE\2
        pw = CSIZE+PTHICK\2  : ph = PTHICK
        xf = 0               : yf = -PTHICK\2
      else
        x = CX + (col-1)*CSIZE + CSIZE\2
        y = CY + (row-1)*CSIZE + CSIZE\2
        pw = CSIZE     : ph = PTHICK
        xf = 0         : yf = -PTHICK\2
      end if
    end if
    box x+xf, y+yf, pw, ph,, c, c
  end if
end sub

' Handle user inputs
sub HandleEvents
  local z$, cmd, c, prow, pcol
  HiliteCell arow, acol
  do
    z$ = INKEY$
    do
      z$ = INKEY$
    loop until z$ <> ""
    cmd = asc(UCASE$(z$))
    select case cmd
      case HOME
        exit sub
      case UP
        if arow > 1 then inc arow, -1
        if pmode then AddPipe arow, acol, prow, pcol
      case DOWN
        if arow < order then inc arow
        if pmode then AddPipe arow, acol, prow, pcol
      case LEFT
        if acol > 1 then inc acol, -1
        if pmode then AddPipe arow, acol, prow, pcol
      case RIGHT
        if acol < order then inc acol
        if pmode then AddPipe arow, acol, prow, pcol
      case DELETE
        DeletePipe arow, acol
        pmode = 0
      case SPACE
        if pmode = 0 then StartPipe arow, acol, prow, pcol
      case BACK
        BackspacePipe arow, acol, prow, pcol
        if prow > 0 then
          arow = prow : acol = pcol
        end if
      case RSTRT
        LoadPuzzle fn$
        DrawPuzzle
        pmode = 0
      case ESC
        cls
        end
    end select
    HiliteCell arow, acol
    if IsWon() then
      running = 0
      ShowWin
    end if
  loop
end sub

' Start a new pipe at an existing dot
sub StartPipe row, col, prow, pcol
  local v, t, c
  if not running then exit sub
  v = grid(row, col, 1)
  t = v\100
  c = v mod 100
  if t <> 1 then exit sub
  pmode = 1 : pcolor = c
  grid(row, col, 1) = 300+pcolor
  prow = row : pcol = col
  DrawMessage "Pipe Start"
end sub  

' Add a pipe section to an empty cell
' Pipe mode terminates at the matching dot.
sub AddPipe row, col, prow, pcol
  local v, t, gc
  if not running then exit sub
  if (row = prow) and (col = pcol) then exit sub
  v = grid(row, col, 1)
  t = v\100
  gc = v mod 100
  if t = 2 then
    DrawMessage "Pipe Cannot Intersect Self!"
    pause 500
    EraseMessage
    DeletePipe prow, pcol
    pmode = 0
    exit sub
  end if
  if t = 3 then
    DrawMessage "Pipe Cannot End on Start Dot!"
    pause 500
    EraseMessage
    DeletePipe prow, pcol
    pmode = 0
    exit sub
  end if
  if (gc > 0) and (gc <> pcolor) then
    DrawMessage "Pipe Cannot Touch another Color!
    pause 500
    EraseMessage
    pmode = 0
    exit sub
  end if
  grid(row, col, 2) = prow
  grid(row, col, 3) = pcol
  grid(prow, pcol, 4) = row
  grid(prow, pcol, 5) = col
  prow = row : pcol = col  
  if grid(row, col, 1) mod 100 = pcolor then
    grid(row, col, 1) = 300+pcolor
    pmode = 0 : prow = 0 : pcol = 0
  else
    grid(row, col, 1) = 200+pcolor
  end if
end sub

' Back the pipe up one cell
sub BackspacePipe row, col, prow, pcol
  local v, t, i, c
  if pmode = 0 then exit sub
  v = grid(row, col, 1)
  t = v\100
  c = v mod 100
  if t = 2 then
    prow = grid(row, col, 2)
    pcol = grid(row, col, 3)
    for i = 1 to 5
      grid(row, col, i) = 0
    next i
    grid(prow, pcol, 4) = 0
    grid(prow, pcol, 5) = 0
  end if
end sub

' Delete a pipe
sub DeletePipe row, col
  local v, t, c, tt, tc, trow, tcol, i
  v = grid(row, col, 1)
  t = v\100
  if t <> 2 then exit sub
  c = v mod 100
  br1 = 0 : bc1 = 0 : br2 = 0 : bc2 = 0
  for trow = 1 to order
    for tcol = 1 to order
      v = grid(trow, tcol, 1)
      tt = v\100
      tc = v mod 100
      if (tc = c) then
        if (tt = 2) then
          for i = 1 to 5
            grid(trow, tcol, i) = 0
          next i
        else if (tt = 3) then
          grid(trow, tcol, 1) = 100+tc
          for i = 2 to 5
            grid(trow, tcol, i) = 0
          next i
        end if
      end if
    next tcol
  next trow
  DrawPuzzle
end sub

' Detect a Win
function IsWon()
  local row, col, v, t, c
  for row = 1 to order
    for col = 1 to order
      v = grid(row, col, 1)
      if v = 0 then
        IsWon = 0
        exit function
      end if
      t = v\100 : c = v mod 100
      if t = 1 then
        IsWon = 0
        exit function
      end if
    next col
  next row
  IsWon = 1
end function

' Draw the instructions
sub DrawInstructions
  text mm.hres\2, mm.vres-60, "Navigate With Arrow Keys, Space=Start Pipe, Delete=Delete Pipe", "CT", 4
  text mm.hres\2, mm.vres-40, "Backspace=Back Up Pipe, R=Restart Puzzle, Escape=Quit", "CT", 4
  text mm.hres\2, mm.vres-20, "Press the HOME key to select another puzzle.", "CT", 4
end sub

' Draw a Message
sub DrawMessage m$
  text mm.hres\2, 5, m$, "CT", 4
end sub

' Erase a Message
sub EraseMessage
  text mm.hres\2, 5, space$(40), "CT", 4
end sub

' Draw the current level and puzzle number for that level
sub DrawPlevel lname$, pnum
  local m$
  box 0, 0, 80, 40,, rgb(black), rgb(black)
  m$ = "Level: " + lname$
  text 0, 0, m$, "LT"
  m$ = "Puzzle: " + str$(pnum)
  text 0, 15, m$, "LT"
end sub

' Show a Win
sub ShowWin
  box 300, 200, 200, 200,, rgb(white), rgb(green)
  text 400, 300, "Success!", "CM", 4,, rgb(black), -1
  text 401, 300, "Success!", "CM", 4,, rgb(black), -1
  text 402, 300, "Success!", "CM", 4,, rgb(black), -1
  pause 2000
  DrawPuzzle
end sub

' Load a puzzle from a '.flo' file.
sub LoadPuzzle path$
  local row, col, v, t, c, i. buf$
  on error skip 1
  open path$ for input as #3
  if mm.errno <> 0 then
    cls
    print "Error opening '" + path$ + "' for reading: ";mm.errmsg$
    end
  end if
  line input #3, buf$
  line input #3, buf$
  order = val(buf$)
  erase grid
  dim grid(order, order, 5)
  for row = 1 to order
    line input #3, buf$
    for col = 1 to order
      c = val(field$(buf$, col, ","))
      if c > 0 then grid(row, col, 1) = 100+c
    next col
  next row
  close #3
end sub

' Draw a dialog to choose difficulty level
sub DrawDifficultyChoice level$, lname$
  local which, z$, cmd
  local ch$(4) = ("Easy", "Medium", "Hard", "Expert")
  cls
  text mm.hres\2, 10, "Choose Difficulty Level", "CT", 4,, rgb(green)
  text 30, 100, ch$(1)
  text 30, 150, ch$(2)
  text 30, 200, ch$(3)
  text 30, 250, ch$(4)
  which = 1
  HiliteChoice which
  z$ = INKEY$
  do
    do
      z$ = INKEY$
    loop until z$ <> ""
    cmd = asc(UCASE$(z$))
    select case cmd
      case UP
        if which > 1 then
          inc which, -1
        else
          which = 4
        end if
      case DOWN
        if which < 4 then
          inc which
        else
          which = 1
        end if
      case ENTER
        lname$ = ch$(which)
        level$ = "PUZZLES/" + UCASE$(lname$)
        exit sub        
      case ESC
        cls
        end
    end select
    HiliteChoice which
  loop
end sub

' Draw a red arrow next to current difficulty level
sub HiliteChoice which
  local x, y, xv(3), yv(3)
  static py = 0
  y = 100 + (which-1)*50+6
  x = 26
  xv(1) = x
  xv(2) = x-12
  xv(3) = x-12
  if py > 0 then
    yv(1) = py
    yv(2) = py-6
    yv(3) = py+6
    polygon 3, xv(), yv(), rgb(black), rgb(black)
  end if
  yv(1) = y
  yv(2) = y-6
  yv(3) = y+6
  polygon 3, xv(), yv(), rgb(red), rgb(red)
  py = y
end sub

' Draw a dialog with thumbnails of the puzzles in a difficulty level
sub DrawThumbnails dx, dy, dw, dh, lvl$, path$, fn$
  local x, y, i, n, f$, row, col, tpath$, hilite, nrows
  local z$, cmd, pick, m$
  local pnames$(MAXPUZ), porder(MAXPUZ), ncols(MAXROWS), pnum(MAXROWS, MAXCOLS)
  cls
  box dx, dy, dw, dh
  m$ = lvl$ +" level: Use Arrow Keys Select a Puzzle, Press ENTER"
  text mm.hres\2, 0, m$, "CT", 4,, rgb(green)
  n = 0 : nrows = 0
  for i = 1 to MAXROWSa
    ncols(i) = 0
  next i
  f$ = dir$(path$+"/*.flo", FILE)
  do while f$ <> ""
    inc n
    pnames$(n) = f$
    f$ = dir$()
  loop
  row = 1 : col = 1
  for i = 1 to n
    x = dx+30 + (col-1)*100
    y = dy+30 + (row-1)*100
    tpath$ = path$ + "/" + pnames$(i)
    LoadPuzzle tpath$
    porder(i) = order
    pnum(row, col) = GetPuzzNum(pnames$(i))
    chosen_pnum = pnum(row, col)
    DrawPThumbnail x, y
    text x+20, y-10, str$(pnum(row, col)), "CB", 4
    if col = MAXCOLS then
      inc row : col = 1 : nrows = max(row, nrows)
    else
      inc col : ncols(row) = col
    end if
  next i
  row = 1 : col = 1 : pick = (row-1)*MAXCOLS + col
  HiliteThumbnail dx, dy, row, col, porder(pick)
  do
    do
      z$ = INKEY$
    loop until z$ <> ""
    cmd = asc(UCASE$(z$))
    select case cmd
      case UP
        if row > 1 then
          if col < ncols(row-1) then
            inc row, -1
            pick = (row-1)*MAXCOLS + col
            HiliteThumbnail dx, dy, row, col, porder(pick)
          end if
        end if
      case DOWN
        if row < nrows then
          if col < ncols(row+1) then
            inc row
            pick = (row-1)*MAXCOLS + col
            HiliteThumbnail dx, dy, row, col, porder(pick)
          end if
        end if
      case LEFT
        if col > 1 then
          inc col, -1
          pick = (row-1)*MAXCOLS + col
          HiliteThumbnail dx, dy, row, col, porder(pick)
        end if
      case RIGHT
        if col < ncols(row) then
          inc col
          pick = (row-1)*MAXCOLS + col
          HiliteThumbnail dx, dy, row, col, porder(pick)
        end if
      case ENTER
        fn$ = path$ + "/" + pnames$(pick)
        chosen_pnum = pnum(row, col)
        exit sub
      case ESC
        cls
        end
    end select
  loop
end sub

' Extract the puzzle number from its name
function GetPuzzNum(pname$)
  local v$
  v$ = MID$(pname$, 3, 3)
  GetPuzzNum = val(v$)
end function

' Hilite the currently selected thumbnail
sub HiliteThumbnail dx, dy, row, col, order
  local px, py, x, y, s
  static prev_row = 0
  static prev_col = 0
  static prev_order = 0
  if prev_row > 0 then
    px = dx+30 + (prev_col-1)*100
    py = dy+30 + (prev_row-1)*100
    s = prev_order*TSIZE
    box px-3, py-3, s+6, s+6,, rgb(black)
  end if
  px = dx+30 + (col-1)*100
  py = dy+30 + (row-1)*100
  s = order*TSIZE
  box px-3, py-3, s+6, s+6,, rgb(yellow)
  prev_row = row : prev_col = col : prev_order = order
end sub

' Help
sub ShowHelp
  cls
  text mm.hres\2, 1, "Help for Flow Game", "CT", 4,, rgb(green)
  print @(0, 30)
  print "Flow is a Game where you have to build pipes that connect two dots that have the"
  print "same color. The pipes can't cross any other pipes of different colors. To solve"
  print "the puzzle, each pair of same-colored dots must be connected with a pipe, and also"
  print "the every square in the puzzle grid has to have either a dot or a segment of a pipe"
  print "in it: there can't be any empty cells in the grid or you are not done."
  print ""
  print "To build a pipe, first use the arrow keys to navigate to either one of the dots you"
  print "want to connect. Press the SPACE bar. Then use the arrow keys to mavigate to the other"
  print "dot of the SAME color. Each time you move to a new cell in any direction, the pipe will"
  print "extend to the new cell. When you reach the matching dot, the pipe is complete and you"
  print "can move away without further changing the pipe.
  print ""
  print "The pipe can go through any empty cells and can bend as much as you want. But if you"
  print "hit another cell or pipe of a different color, the pipe breaks and you have to start over."
  print "You also cannot make the pipe loop back on itself."
  print ""
  print "To remove the most recent segments of a pipe you are building, press the BACKSPACE key,"
  print "Each time you press, the pipe will back up one cell. Then you can continue building it"
  print "in new directions. If you want to get rid of an ENTIRE pipe, select any of its segments"
  print "and press the DELETE key."
  text mm.hres\2, mm.vres-1, "Press Any Key to Continue", "CB"
  z$ = INKEY$
  do
    z$ = INKEY$
  loop until z$ <> ""
end sub
