' Make a 'Flow' Puzzle
' When saved, the puzzle is saved to one of the difficulty levels as a '.flo' file,
' and the full puzzle including solution is saved to the 'Create' file as a '.pip' file,
' in case it needs further work.
' Rev 1.0.0 William M Leue 17-Jan-2024

option default integer
option base 1

' Constants
const CSIZE     = 40
const SRAD      = CSIZE/4
const PTHICK    = 4
const MIN_ORDER = 4
const MAX_ORDER = 10

const CCOLOR    = rgb(gray)
const BGCOLOR   = rgb(50, 50, 50)

' Commands
const UP     = 128
const DOWN   = 129
const LEFT   = 130
const RIGHT  = 131
const ZERO   = asc("0")
const NINE   = asc("9")
const SPACE  = 32
const DELETE = 127
const BACK   = 8
const SV     = asc("S")
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

' Main Program
'open "debug.txt" for output as #1
SetPallette
ShowHelp
GetOrder
SetUpBoard
DrawPuzzle
save image "puzzle"
HandleEvents
end

' Set up the Pallette for dot and pipe colors
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

' Get the Puzzle Order from the User
sub GetOrder
  local m$, a$, ok
  cls
  print "Work on Existing Puzzle? (Y,N): ";
  input "", a$
  if LEFT$(UCASE$(a$), 1) = "Y" then
    print "Enter Puzzle Name: ";
    input "", fn$
    fn$ = "Puzzles/Create/" + fn$ + ".pip"
    LoadSolution fn$
  else
    do
      ok = 1
      m$ = "Enter grid order [" + str$(MIN_ORDER) + "-" + str$(MAX_ORDER) + "]: "
      print m$;
      input "", a$
      order = val(a$)
      if (order < MIN_ORDER) or (order > MAX_ORDER) then ok = 0
    loop until ok
    erase grid
    dim grid(order, order, 5)
  end if
end sub

' Set Up the Board
sub SetUpBoard
  local i, row, col
  cx = mm.hres\2 - (order*CSIZE)\2
  cy = mm.vres\2 - (order*CSIZE)\2
  arow = 1 : acol = 1
  running = 1
end sub

' Fetch the Puzzle from the Archive
sub FetchPuzzle
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
end sub  

' Draw a Cell in the Grid
sub DrawCell row, col, hilite
  local x, y, v, c, ec, fc, t
  v = grid(row, col, 1)
  t = v\100
  c = v mod 100
  ec = rgb(gray)
  if hilite then
    ec = rgb(yellow)
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
      pw = PTHICK    : ph = CSIZE
      xf = -PTHICK\2 : yf = 0
    else if row-pr < 0 then
      x = CX + (col-1)*CSIZE + CSIZE\2
      y = CY + (row-1)*CSIZE + CSIZE\2
      pw = PTHICK    : ph = CSIZE + PTHICK\2
      xf = -PTHICK\2 : yf = 0
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
      pw = PTHICK    : ph = CSIZE
      xf = -PTHICK\2 : yf = 0
    else if row-nr < 0 then
      x = CX + (col-1)*CSIZE + CSIZE\2
      y = CY + (row-1)*CSIZE + CSIZE\2
      pw = PTHICK    : ph = CSIZE + PTHICK\2
      xf = -PTHICK\2 : yf = 0
    else if row-nr = 0 then
      if col-nc > 0 then
        x = CX + (nc-1)*CSIZE + CSIZE\2
        y = CY + (nr-1)*CSIZE + CSIZE\2
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
      case ZERO to NINE
        AddDot cmd
      case SPACE
        if pmode = 0 then StartPipe arow, acol, prow, pcol
      case DELETE
        DeleteElement arow, acol
        pmode = 0
      case BACK
        BackspacePipe arow, acol, prow, pcol
        arow = prow : acol = pcol
      case SV
        cls
        SavePrep
      case ESC
        cls
        end
    end select
    HiliteCell arow, acol
  loop
end sub

' Add a dot of the specified color
sub AddDot cmd
  local c
  c = cmd - asc("0")
  if c = 0 then c = 10
  if GetNumDots(c) < 2 then
    grid(arow, acol, 1) = 100 + c
  end if
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
    DeleteElement prow, pcol
    pmode = 0
    exit sub
  end if
  if t = 3 then
    DrawMessage "Pipe Cannot End on Start Dot!"
    pause 500
    DeleteElement prow, pcol
    pmode = 0
    exit sub
  end if
  EraseMessage
  if (gc > 0) and (gc <> pcolor) then
    if t = 1 then
      exit sub
    end if
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

' Delete a cell element
' If a dot, delete both dots of this color and all pipes of this color
' If a pipe, just delete the pipes
sub DeleteElement row, col
  local v, t, c, tt, tc, trow, tcol, br1, bc1, br2, bc2
  v = grid(row, col, 1)
  t = v\100
  c = v mod 100
  br1 = 0 : bc1 = 0 : br2 = 0 : bc2 = 0
  for trow = 1 to order
    for tcol = 1 to order
      v = grid(trow, tcol, 1)
      tt = v\100
      tc = v mod 100
      if tc = c then
        if (tt = 1) or (tt = 3) then
          if br1 = 0 then
            br1 = trow : bc1 = tcol
          else
            br2 = trow : bc2 = tcol
          end if
        end if
      end if
    next tcol
  next trow
  for trow = 1 to order
    for tcol = 1 to order
      v = grid(trow, tcol, 1)
      tc = v mod 100
      if tc = c then
        for i = 1 to 5
          grid(trow, tcol, i) = 0
        next i
      end if
    next tcol
  next trow
  if t = 2 then
    grid(br1, bc1, 1) = 100+c
    grid(br2, bc2, 1) = 100+c
  end if
  DrawPuzzle
end sub

' Return the number of dots of the specified color
function GetNumDots(c)
  local row, col, n, v, t, gc
  n = 0
  for row = 1 to order
    for col = 1 to order
      v = grid(row, col, 1)
      t = v\100
      gc = v mod 100
      if gc = c then
        if (t = 1) or (t - 3) then inc n
      end if
    next col
  next row
  GetNumDots = n
end function

' Determine the filenames for saving puzzle and solution,
' then save them.
sub SavePrep
  local fn$, cn$, m$, lvl, ok, pnum
  local lnames$(4) = ("Easy", "Medium", "Hard", "Expert")
  cls
  m$ = "Enter Puzzle Level Number 1-4 (Easy, Medium, Hard, Expert): "
  do
    ok = 1
    print m$;
    input "", a$
    lvl = val(a$)
    if (lvl < 1) or (lvl > 4) then ok = 0
  loop until ok
  pnum = GetNextPuzzNum(lnames$(lvl))
  fn$ = "Puzzles/" + lnames$(lvl) + "/p" + format$(pnum+1, "%03g") + ".flo"
  SavePuzzle fn$
  cn$ = "Puzzles/Create/p" + format$(pnum+1, "%03g") + ".pip"
  SaveSolution cn$
  end
end sub

' Scan puzzles in level folder to find next puzzle number
function GetNextPuzzNum(lname$)
  local path$, fn$, v$, pnum, maxpnum
  path$ = "Puzzles/" + lname$
  maxpnum = 0
  fn$ = dir$(path$ + "/*", FILE)
  do while fn$ <> ""
    v$ = MID$(fn$, 3, 3)
    pnum = val(v$)
    if pnum > maxpnum then maxpnum = pnum
    fn$ = dir$()
  loop
  GetNextPuzzNum = maxpnum
end function

' Save the puzzle to a '.flo' file.
sub SavePuzzle path$
  local row, col, v, t, c, i
  on error skip 1
  open path$ for output as #3
  if mm.errno <> 0 then
    cls
    print "Error opening '" + path$ + "' for writing: ";mm.errmsg$
    end
  end if
  print #3, path$
  print #3, str$(order)
  for row = 1 to order
    for col = 1 to order
      v = grid(row, col, 1)
      t = v/100
      c = v mod 100
      if (t = 1) or (t = 3) then
        print #3, str$(c) + ",";
      else
        print #3, "0,";
      end if
    next col
    print #3, ""
  next row
  close #3
  cls
  print "Puzzle saved to '";path$;"'"
end sub

' Load a puzzle from a '.pip' file
' Save puzzle solution to a '.pip' file for later work.
sub SaveSolution path$
  local row, col, v, t, c, i
  on error skip 1
  open path$ for output as #3
  if mm.errno <> 0 then
    cls
    print "Error opening '" + path$ + "' for writing: ";mm.errno
    end
  end if
  print #3, path$
  print #3, str$(order)
  for row = 1 to order
    for col = 1 to order
      print #3, "[";
      for i = 1 to 5
        print #3, str$(grid(row, col, i)) + ",";
      next i
      print #3, "];";
    next col
    print #3, ""
  next row
  close #3
  print "Solution saved to '";path$;"'"
  end
end sub

' Load a puzzle solution from a '.pip' file.
sub LoadSolution path$
  local row, col, v, t, c, i, buf$, f2$, lf
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
      f2$ = field$(buf$, col, ";")
      lf = len(f2$)
      f2$ = MID$(f2$, 2, lf-2)
      for i = 1 to 5
        v = val(field$(f2$, i, ","))
        grid(row, col, i) = v
      next i
    next col
  next row
  close #3
end sub

' Draw a Message
sub DrawMessage m$
  text mm.hres\2, 5, m$, "CT", 4
end sub

' Erase a Message
sub EraseMessage
  text mm.hres\2, 5, space$(40), "CT", 4
end sub

' Show Commands
sub ShowHelp
  local z$ = INKEY$
  cls
  text mm.hres\2, 1, "Help for MakeFlowPuzzle", "CT", 4,, rgb(green)
  print @(0, 25)
  print "The MakeFlowPuzzle lets you create new puzzles for the 'Flow' program."
  print "You can create new puzzles from scratch or edit previously-created"
  print "puzzles."
  print ""
  print "To create a new puzzle, answer the initial question with 'N'. Then the"
  print "prgram will ask you for the 'order' of the puzzle. Answer with a number"
  print "from 4 to 10. The program will show you a blank puzzle grid with a cursor"
  print "in the top left-hand cell. Use the arrow keyboard keys to navigate around"
  print "the grid."
  print ""
  print "To drop a 'dot' in the current cell, press a number from 0 to 9 on tbe"
  print "keyboard. Zero stands for 10. A dot of the corresponding color will appear"
  print "on the current cell:"
  print "   1 = Red      2 = Green  3 = Blue    4 = Yellow    5 = Cyan"
  print "   6 = Magenta  7 = Pink   8 = Orange  9 = Lt Blue   10 = Brown"
  print "There must be exactly 2 dots of each different color. There need not be"
  print "as many colors used as the order of the grid."
  print "To connect 2 dots of the same color with a pipe, position the cursor on one"
  print "of the dots and press the Spacebar. Then use the arrow keys to navigate to"
  print "the other dot, using any path you choose. As the cursor moves, it will leave"
  print "the growing pipe behind it. The pipe stops growing when the other dot of the"
  print "same color is reached. Repeat for other colored dot pairs."
  print "Note that the pipe will be aborted if you run into a dot of the wrong color,"
  print "hit the pipe or another pipe, or try to run past the wall of the grid."
  print ""
  print "You can press BACKSPACE to erase the most recent parts of a pipe.
  print "To delete a whole pipe, position the cursor on one of its segments and press the"
  print "DELETE key. To delete BOTH dots of a certain color, position the cursor on"
  print "one of the dots and press the DELETE key: this will also delete any pipe"
  print "of the same color,"
  print "To make a puzzle that can be solved, you have to fill every cell of the grid"
  print "with either a dot or a piece of pipe. Any cells with nothing in them will"
  print "make the puzzle unsolvable."
  print ""
  print "Press 'S' to save the puzzle. The program will ask you to choose one of the"
  print "4 difficulty levels, and then it will save the puzzle to the specified level,"
  print "and will also save a copy of the full puzzle including pipes for later editing"
  print "in the 'Puzzles/Create' directory with a '.pip' filename extension."
  print ""
  print "If you want to edit a previously-created file, answer the initial question"
  print "with 'Y' and then enter the root filename, which will be of the form:"
  print "   'p' + num + '.pip', where 'num' is a 3-charater digit with leading zeros,"
  print "for instance, 'p027.pip'. However, you do not enter the filename extension,"
  print "just 'p027'. The file will be opened and you will see the full puzzle including"
  print "pipes. 
  text mm.hres\2, mm.vres-1, "Press Any Key to Continue", "CB"
  do
    z$ = INKEY$
  loop until z$ <> ""
end sub

