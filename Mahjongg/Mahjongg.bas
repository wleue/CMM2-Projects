' Mahjong Solitaire for CMM2
' Rev 1.0.0 William M Leue, 20-May-2022
' Tile images from Wikimedia Commons.

option default integer
option base 1

' Constants
const TILE_WIDTH  = 50
const TILE_HEIGHT = 61
const FACE_XOFF   = 4
const FACE_YOFF   = 3
const FACE_WIDTH  = 38
const FACE_HEIGHT = 49
const LYOFF       = -5
const LXOFF       = -5
const NTILES      = 42

const NUM_TOTAL_TILES = 144

const TILE_NROWS    = 5
const TILE_NCOLS    = 9
const TILE_NSUITS   = 3
const TILE_NRANKS   = 9
const TILE_NWINDS   = 4
const TILE_NDRAGONS = 3
const TILE_NFLOWERS = 4
const TILE_NSEASONS = 4
const TILE_NDUPS    = 4

const DOTS_INDEX    = 1
const BAMBOOS_INDEX = 10
const CHARS_INDEX   = 19
const WINDS_INDEX   = 28
const DRAGONS_INDEX = 32
const FLOWERS_INDEX = 35
const SEASONS_INDEX = 39

const PZ_NROWS   = 8
const PZ_MAXCOLS = 12
const PZ_NLAYERS = 5

const PZ_SP_SLOT1 = NUM_TOTAL_TILES-3
const PZ_SP_SLOT2 = NUM_TOTAL_TILES-2
const PZ_SP_SLOT3 = NUM_TOTAL_TILES-1
const PZ_SP_SLOT4 = NUM_TOTAL_TILES

const TILE_DIR$    = "Tiles"

const TXSTEP      = 51
const TYSTEP      = 65

const NORMAL      = 1
const DARK        = 2

const MCHAN = 2

const ESC    = 27
const HOME   = 134
const BACKSP = 8
const F12    = 156
const QUEST  = 63

const SLOT_X      = 1
const SLOT_Y      = 2
const SLOT_TILE   = 3
const SLOT_COL    = 4
const SLOT_ROW    = 5
const SLOT_LAYER  = 6

const ANNOTATE = 0

const BG_COLOR     = rgb(0,50,0)
const HILITE_COLOR = rgb(cyan)

' Globals
dim pz_rows(PZ_NLAYERS)
dim pz_cols(PZ_NROWS, PZ_NLAYERS-1)
dim timages(2, NTILES)
dim timages_dark(2, NTILES)
dim deck(NUM_TOTAL_TILES)
dim slots(6, NUM_TOTAL_TILES)
dim layer_offsets(PZ_NLAYERS) = (0, 84, 120, 136, 140)
dim layer_color(PZ_NLAYERS-1) = (rgb(blue), rgb(green), rgb(red), rgb(yellow))
dim tnames$(NTILES)
dim tnamesD$(NTILES)
dim image_ncols(4) = (9, 9, 9, 15)
dim undo_stack(4, NUM_TOTAL_TILES)
dim free_list(NUM_TOTAL_TILES)
dim nfree = 0

dim left_click = 0
dim left_busy = 0
dim mouse_x = 0
dim mouse_y = 0
dim pstate = 0
dim tx1 = 0
dim tx2 = 0
dim sp = 0
dim running = 0
dim nmoves = 0

' Main Program
mode 1,16
open "debug.txt" for output as #1
ReadPuzzleVals
ReadTileNames
LoadTileImages NORMAL
LoadTileImages DARK
InitMouse
MakeDeck
MakePuzzle
DrawPuzzle
PlayLoop
end

' Read the puzzle row and column numbers
sub ReadPuzzleVals
  local row, layer
  for layer = 1 to PZ_NLAYERS
    read pz_rows(layer)
  next layer
  for layer = 1 to PZ_NLAYERS-1
    for row = 1 to PZ_NROWS
      read pz_cols(row, layer)
    next row
  next layer
end sub

' Read the root filenames for the individual tile images
sub ReadTileNames
  local i
  for i = 1 to NTILES
    read tnames$(i)
  next i
  for i = 1 to NTILES
    read tnamesD$(i)
  next i
end sub

' Load arrays of tile images, both regular and dark versions
' The 'timages' and 'timages_dark' arrays contain the x, y locations
' of tiles in page 1 for later blitting to page 0.
sub LoadTileImages which
  local i, row, col, x, y, ys
  i = 1
  if which = NORMAL then
    ys = 0
  else
    ys = 4*TYSTEP
  end if
  page write 1
  for row = 1 to 4
    y = ys + (row-1)*TYSTEP
    for col = 1 to image_ncols(row)
      x = (col-1)*TXSTEP
      if which = NORMAL then
        timages(1, i) = x
        timages(2, i) = y
      else
        timages_dark(1, i) = x
        timages_dark(2, i) = y
      end if
      if which = DARK then
        pathD$ = TILE_DIR$ + "/" + tnamesD$(i)
        load png pathD$, x, y
      else
        path$ = TILE_DIR$ + "/" + tnames$(i)
        load png path$, x, y
      end if
      inc i
    next col
  next row
  page write 0
end sub     

' Make a 'deck' of the full 144 tiles and shuffle it
sub MakeDeck
  local n
  n = 1
  for suit = 1 to TILE_NSUITS
    select case suit
      case 1: start = DOTS_INDEX
      case 2: start = BAMBOOS_INDEX
      case 3: start = CHARS_INDEX
    end select
    for rank = 1 to TILE_NRANKS
      for dup = 1 to TILE_NDUPS
        deck(n) = start+rank-1
        inc n
      next dup
    next rank
  next suit
  for wind = 1 to TILE_NWINDS
    start = WINDS_INDEX
    for dup = 1 to TILE_NDUPS
      deck(n) = start+wind-1
      inc n
    next dup
  next wind
  for dragon = 1 to TILE_NDRAGONS
    start = DRAGONS_INDEX
    for dup = 1 to TILE_NDUPS
      deck(n) = start+dragon-1
      inc n
    next dup
  next dragon
  for flower = 1 to TILE_NFLOWERS
    deck(n) = FLOWERS_INDEX+flower-1
    inc n
  next flower
  for season = 1 to TILE_NSEASONS
    deck(n) = SEASONS_INDEX+season-1
    inc n
  next season
  ShuffleDeck
end sub

' Shuffle the Deck Randomly
sub ShuffleDeck
  local i, j, tmp
  for i = 1 to NUM_TOTAL_TILES
    j = RandomIntegerInRange(i, NUM_TOTAL_TILES)
    temp = deck(i)
    deck(i) = deck(j)
    deck(j) = temp
  next i
end sub

' Make the 'Dragon' puzzle. Compute tile locations and store in the 'slots' array
sub MakePuzzle
  local xc, yc, ys, xs, n, x, y, tile, tindex, ncols, row, col, layer, s, i
  cls
  n = 1
  xc = MM.HRES\2
  yc = MM.VRES\2
  y = yc - FACE_HEIGHT\2
  x = xc - 8*FACE_WIDTH
  tile = deck(n) : inc n
  SaveTileInfo x, y, tile, -1, 4, 1, PZ_SP_SLOT1
  x = x+FACE_WIDTH
  tile = deck(n) : inc n
  SaveTileInfo x, y, tile, 0, 4, 1, PZ_SP_SLOT2
  ys = yc - (PZ_NROWS\2)*FACE_HEIGHT
  s = 1
  for layer = 1 to PZ_NLAYERS-1
    if layer > 1 then
      inc ys, LYOFF
      inc xs, LXOFF
    end if
    for row = 1 to PZ_NROWS
      y = ys + (row-1)*FACE_HEIGHT
      ncols = pz_cols(row, layer)
      if ncols = 0 then continue for
      fcol = (PZ_MAXCOLS - ncols)\2 + 1
      lcol = fcol + ncols - 1
      xs = xc - (PZ_MAXCOLS/2)*FACE_WIDTH
      for col = 1 to PZ_MAXCOLS
        x = xs + (col-1)*FACE_WIDTH
        if col >= fcol and col <= lcol then
          tile = deck(n) : inc n
          SaveTileInfo x, y, tile, col, row, layer, s
          inc s
        end if
      next col
    next row
  next layer
  y = yc - FACE_HEIGHT\2
  x = xc + 6*FACE_WIDTH  
  tile = deck(n) : inc n
  SaveTileInfo x, y, tile, 13, 4, 1, PZ_SP_SLOT3
  x = xc - FACE_WIDTH\2
  tile = deck(n) : inc n
  SaveTileInfo x, y, tile, 6, 4, 5, PZ_SP_SLOT4
  running = 1
  sp = 0
  nmoves = 0
  for i = 1 to NUM_TOTAL_TILES
    free_list(i) = 0
  next i
  nfree = 0
end sub

' Save the information about a specific slot and the tile located there.
' The x, y, and level parameters are used to resolve mouse click tile
' identification, and the col, row, and layer parameters are used to
' resolve tile blocking questions.
sub SaveTileInfo x, y, tile, col, row, layer, s
  slots(SLOT_X, s)      = x
  slots(SLOT_Y, s)      = y
  slots(SLOT_TILE, s) = tile
  slots(SLOT_COL, s)    = col
  slots(SLOT_ROW, s)    = row
  slots(SLOT_LAYER, s)  = layer
end sub

' Returns true if the tile at the specified slot index is blocked by other tiles to its
' left, right, or top; false if the tile is free to be removed.
function IsBlocked(sindex)
  local row, col, layer, v, i
  row = slots(SLOT_ROW, sindex)
  col = slots(SLOT_COL, sindex)
  layer = slots(SLOT_LAYER, sindex)
  v = CheckSpecialTiles(sindex)
  if v >= 0 then
    IsBlocked = v
    exit function
  end if
  above = FindSlot(row, col, layer+1)
  if above <> 0 then
    if slots(SLOT_TILE, above) <> 0 then
      IsBlocked = 1
      exit function
    end if
  end if
  left = FindSlot(row, col-1, layer) 
  right = FindSlot(row, col+1, layer)
  if left = 0 or right = 0 then
    IsBlocked = 0
    exit function
  end if
  ltile = slots(SLOT_TILE, left)
  rtile = slots(SLOT_TILE, right)
  if ltile = 0 or rtile = 0 then
    IsBlocked = 0
    exit function
  end if
  IsBlocked = 1
end function

' Check for a tile being blocked by one of the 4 'special' tiles
' or perhaps IS one of the special tiles.
' Returns a 1 if blocked, 0 if not, or -1 if not involved with special tiles.
function CheckSpecialTiles(sindex)
  if sindex = PZ_SP_SLOT1 or sindex = PZ_SP_SLOT_3 or sindex = PZ_SP_SLOT4 then
    CheckSpecialTiles = 0
    exit function
  end if
  CheckSpecialTiles = 0
  if sindex = PZ_SP_SLOT2 then
      if slots(SLOT_TILE, PZ_SP_SLOT1) <> 0 then
        CheckSpecialTiles = 1
        exit function
      end if
  end if    
  row = slots(SLOT_ROW, sindex)
  col = slots(SLOT_COL, sindex)
  layer = slots(SLOT_LAYER, sindex)
  if layer = 1 then
    if (row = 4 or row = 5) and col = 1 then
      if slots(SLOT_TILE, PZ_SP_SLOT2) <> 0 then
        CheckSpecialTiles = 1
        exit function
      end if
    end if
    if (row = 4 or row = 5) and col = 12 then
      if slots(SLOT_TILE, PZ_SP_SLOT3) <> 0 then
        CheckSpecialTiles = 1
        exit function
      end if
    end if
  end if
  if layer = 4 then
    if slots(SLOT_TILE, PZ_SP_SLOT4) <> 0 then
      CheckSpecialTiles = 1
      exit function
    end if
  end if
  CheckSpecialTiles = -1
end function

' Return the slot index for the (row, col, layer) location, if one exists. If not, return zero.
function FindSlot(row, col, layer)
  local result, above, ix, fcol, lcol
  ix = 1
  if layer < 1 or layer > PZ_NLAYERS-1 then
    ix = 0
  else if row < 1 or row > PZ_NROWS then
    ix = 0
  end if
  if ix = 0 then
    FindSlot = 0
    exit function
  end if
  ncols = pz_cols(row, layer)
  if ncols = 0 then
    ix = 0
    FindSlot = 0
    exit function
  end if
  fcol = (PZ_MAXCOLS - ncols)\2 + 1
  lcol = fcol + ncols - 1
  if col < fcol or col > lcol then
    ix = 0
    FindSlot = 0
    exit  function
  end if
  ix = 0
  if layer > 1 then
    inc ix, layer_offsets(layer)
  end if
  if row > 1 then
    for i = 1 to row-1
      inc ix, pz_cols(i, layer)
    next i
  end if
  for i = 1 to col
    if i >= fcol and i <= lcol then
      inc ix
    end if
  next i
  if ix > NUM_TOTAL_TILES then ix = 0
  FindSlot = ix
end function
  
' Draw the 'Dragon' puzzle
sub DrawPuzzle
  local xc, yc, ys, xs, x, y, tile, tindex, ncols, row, col, layer, s, i, lrow
  cls BG_COLOR
  text mm.hres\2, 10, "Undo = Backspace  Restart = Home  New Puzzle = F12  Help = ?  Quit = Escape", "CT"
  gui cursor hide
  for i = 1 to NUM_TOTAL_TILES
    free_list(i) = 0
  next i
  nfree = 0
  xc = MM.HRES\2
  yc = MM.VRES\2
  y = yc - FACE_HEIGHT\2
  x = xc - 8*FACE_WIDTH
  tindex = PZ_SP_SLOT1
  tile = slots(SLOT_TILE, tindex)
  DrawTile tile, tindex, x, y, 1
  x = x+FACE_WIDTH
  tindex = PZ_SP_SLOT2
  tile = slots(SLOT_TILE, tindex)
  DrawTile tile, tindex, x, y, 1
  ys = yc - (PZ_NROWS\2)*FACE_HEIGHT
  for layer = 1 to PZ_NLAYERS-1
    if layer > 1 then
      inc ys, LYOFF
      inc xs, LXOFF
    end if
    for row = PZ_NROWS to 1 step -1
      y = ys + (row-1)*FACE_HEIGHT
      ncols = pz_cols(row, layer)
      if ncols = 0 then continue for
      xs = xc - (ncols/2)*FACE_WIDTH
      for col = 1 to PZ_MAXCOLS
        fcol = (PZ_MAXCOLS - ncols)\2 + 1
        lcol = fcol + ncols - 1
        x = xs + (col-fcol)*FACE_WIDTH
        tindex = FindSlot(row, col, layer)
        if tindex = 0 then continue for
        tile = slots(SLOT_TILE, tindex)
        DrawTile tile, tindex, x, y, 1
      next col
    next row
  next layer
  y = yc - FACE_HEIGHT\2
  x = xc + 6*FACE_WIDTH  
  tindex = PZ_SP_SLOT3
  tile = slots(SLOT_TILE, tindex)
  DrawTile tile, tindex, x, y, 1
  x = xc - FACE_WIDTH\2
  tindex = PZ_SP_SLOT4
  tile = slots(SLOT_TILE, tindex)
  DrawTile tile, tindex, x, y, 1
  if IsWon() then
    ShowWin()
  end if
  gui cursor show
end sub

' Draw a specified tile from the deck to the specified screen coordinates
sub DrawTile tile, tindex, x, y, dark
  if tile > 0 then
    if dark = 1 then
      if IsBlocked(tindex) then
        blit timages_dark(1,tile), timages_dark(2,tile), x, y, TILE_WIDTH, TILE_HEIGHT, 1, 4
      else
        blit timages(1,tile), timages(2,tile), x, y, TILE_WIDTH, TILE_HEIGHT, 1, 4
        inc nfree
        free_list(nfree) = tile
      end if
    else
      blit timages(1,tile), timages(2,tile), x, y, TILE_WIDTH, TILE_HEIGHT, 1, 4
    end if
    cx = x + FACE_WIDTH\2
    cy = y + FACE_HEIGHT\2
    if ANNOTATE then
      text cx, cy-9, str$(tile), "CM", 7
      text cx, cy+9, str$(tindex), "CM", 7
    end if
  end if
end sub

' Initialize Mouse and Cursor
sub InitMouse
  on error skip 1
  controller mouse open MCHAN, LClick
  if mm.errno <> 0 then
    print "Could not init mouse on channel " + str$(MCHAN)
    end
  end if
  gui cursor on 1, mm.hres\2, mm.vres\2, rgb(yellow)
  gui cursor show
  settick 20, UpdateCursor
end sub

' Make the cursor track the mouse
sub UpdateCursor
  gui cursor mouse(X), mouse(Y)
end sub

' Mouse left-click ISR. Store the position and set the state variables.
sub LCLick
  if left_busy = 0 then
    mouse_x = mouse(X)
    mouse_y = mouse(Y)
    left_click = 1
  end if
end sub

' Play Event Loop: handle all user inputs from mouse and keyboard
sub PlayLoop
  local z$, cmd
  z$ = INKEY$
  do
    z$ = INKEY$
    if z$ <> "" then
      cmd = asc(UCASE$(z$))
      select case cmd
        case ESC
          Quit
        case BACKSP
          UndoMove
        case HOME
          sp = 0
          MakePuzzle
          DrawPuzzle
        case F12
          sp = 0
          ShuffleDeck
          MakePuzzle
          DrawPuzzle
        case Quest
          ShowHelp
      end select
    end if
    if left_click then
      left_busy = 1
        HandleClick
      left_click = 0
      left_busy = 0
    end if
  loop
  Quit
end sub

' Quit the program with cleanup of mouse and cursor
sub Quit
  settick 0,0
  gui cursor off
  controller mouse close
  cls
  end
end sub

' Handle Mouse Clicks
sub HandleClick
  if not running then exit sub
  local tile1, tile2
  if pstate = 0 then
    tx1 = FindClickedTile(mouse_x, mouse_y)
    if tx1 <> 0 then
      tile1 = slots(SLOT_TILE, tx1)
      if isBlocked(tx1) then 
        exit sub
      end if 
      pstate = 1
      HiliteTile(tx1, 1)
      exit sub
    end if
  else if pstate = 1 then
    tx2 = FindClickedTile(mouse_x, mouse_y)
    if tx2 = tx1 then
      HiliteTile(tx1, 0)
      pstate = 0
      exit sub
    end if
    if tx2 <> 0 then
      if isBlocked(tx2) then 
        pstate = 0
        HiliteTile(tx1, 0)
        exit sub
      end if 
      tile1 = slots(SLOT_TILE, tx1)
      tile2 = slots(SLOT_TILE, tx2)
      if TileMatch(tile1, tile2) then
        RemoveTiles tx1, tx2
      end if
    end if
  end if
  pstate = 0
end sub

' Highlight the first tile to be clicked
sub HiliteTile tindex, turnon
  local x, y, xt, yt, layer, xoff, yoff
  layer = slots(SLOT_LAYER, tindex)
  xt = slots(SLOT_X, tindex)
  yt = slots(SLOT_Y, tindex)
  xoff = 4 : yoff = 8
  x = xt + xoff
  y = yt + yoff
  if turnon then
    rbox x-2, y-2, FACE_WIDTH+2, FACE_HEIGHT+2, 4, HILITE_COLOR
  else
    DrawPuzzle
  end if
end sub

' Find out which tile was clicked by the mouse, if any. The (x, y) parameters in
' the slots array determine which 'stack' of tiles was selected, and the 'layer'
' parameter determines which tile in the stack is currently topmost.
function FindClickedTile(mx, my)
  local i, tindex(PZ_NLAYERS), tlayer(PZ_NLAYERS), n, target
  for n = 1 to PZ_NLAYERS
    tindex(n) = 0
    tlayer(n) = 0
  next n
  n = 0
  target = 0
  for i = 1 to NUM_TOTAL_TILES
    if slots(SLOT_TILE, i) <> 0 then
      tx = slots(SLOT_X, i) : ty = slots(SLOT_Y, i)
      if mx >= tx and mx <= tx+FACE_WIDTH then
        if my >= ty and my <= ty+FACE_HEIGHT then
          inc n
          tindex(n) = i
          tlayer(n) = slots(SLOT_LAYER, i)
        end if
      end if
    end if
  next i
  lmax = -1
  target = 0
  for i = 1 to n
    if tlayer(i) > lmax then
      lmax = tlayer(i)
      target = tindex(i)
    end if
  next i
  FindClickedTile = target
end function

' Returns true if the two proffered tiles match, false otherwise
' Note that 'match' means different things depending on what tiles are being tested.
function TileMatch(tile1, tile2)
  if tile1 >= FLOWERS_INDEX and tile1 < FLOWERS_INDEX+TILE_NFLOWERS then
    if tile2 >= FLOWERS_INDEX and tile2 < FLOWERS_INDEX+TILE_NFLOWERS then
      TileMatch = 1
      exit function
    end if
  end if
  if tile1 >= SEASONS_INDEX and tile1 < SEASONS_INDEX+TILE_NSEASONS then
    if tile2 >= SEASONS_INDEX and tile2 < SEASONS_INDEX+TILE_NSEASONS then
      TileMatch = 1
      exit function
    end if
  end if
  if tile2 = tile1 then
    TileMatch = 1
  else
    TileMatch = 0
  end if
end function

' Remove two matching tiles that the player has clicked, then redraws the puzzle
' Also checks for a won game and no more moves.
sub RemoveTiles tx1, tx2
  local n
  PushMove tx1, tx2
  slots(SLOT_TILE, tx1) = 0
  slots(SLOT_TILE, tx2) = 0
  inc nmoves
  if IsWon() then
    running = 0
    ShowWin
  end if
  DrawPuzzle
  n = CountMoves()
end sub

' Push the current move onto the undo stack
sub PushMove tx1, tx2
  inc sp
  undo_stack(1, sp) = slots(SLOT_TILE, tx1)
  undo_stack(2, sp) = tx1
  undo_stack(3, sp) = slots(SLOT_TILE, tx2)
  undo_stack(4, sp) = tx2
end sub  

' Undo the previous move if any
sub UndoMove
  local tile1, tindex1, tile2, tindex2
  if sp = 0 then exit sub
    tile1 = undo_stack(1, sp)
    tindex1 = undo_stack(2, sp)
    tile2 = undo_stack(3, sp)
    tindex2 = undo_stack(4, sp)
    inc sp, -1
    slots(SLOT_TILE, tindex1) = tile1
    slots(SLOT_TILE, tindex2) = tile2
    DrawPuzzle
  end if
end sub

' Detect a win
function IsWon()
  local i, won
  won = 1
  for i = 1 to NUM_TOTAL_TILES
    if slots(SLOT_TILE, i) <> 0 then
      won = 0
      exit for
    end if
  next i
  IsWon = won
end function

' Show Win
sub ShowWin
  local x, y, w, h
  w = 400
  h = 50
  x = mm.hres\2 - w\2
  y = 80
  box x, y, w, h, 2, rgb(black), rgb(yellow)
  text x+w\2, y+h\2, "You Win!", "CM", 4,, rgb(blue), -1
end sub  

' Count and report number of available moves
function CountMoves()
  local n, i, t1, t2
  if nfree = 0 then
    CountMoves = 0
    exit function
  end if
  local mark(nfree)
  if IsWon() then exit function
  n = 0
  for i = 1 to nfree
    mark(i) = 0
    t1 = free_list(i)
    for j = 1 to nfree
      if j <> i then
        t2 = free_list(j)
        if t2 = t1 then 
          if mark(i) = 0 and mark(j) = 0 then
            inc n
            mark(i) = 1 : mark(j) = 1
          end if
        end if
      end if
    next j
  next i
  CountMoves = n
  ShowNumberOfMoves n  
end function

' Report number of moves
sub ShowNumberOfMoves n
  local x, y, w, h, cb, ct, m$
  w = 250
  h = 30
  x = mm.hres\2 - w\2
  y = 40
  if n > 0 then
    cb = rgb(green)
    ct = rgb(black)
    if n > 1 then
      m$ = "There are " + str$(n) + " moves"
    else
      m$ = "There is 1 move"
    end if
  else  
    cb = rgb(red)
    ct = rgb(white)
    m$ = "There are no more moves!"
  end if
  box x, y, w, h,, cb, cb
  text x+w\2, y+h\2, m$, "CM", 4,, ct, -1
end sub

' Show Help Screen
sub ShowHelp
  local z$, x, y, yinc
  cls
  gui cursor hide
  print "In Mahjong Solitaire, your task is to find matching tiles and remove them until all"
  print "tiles have been removed."
  print ""
  print "There are  42 unique tile types. Most of these have 4 repetitions, giving 144 tiles in all"
  print "There are 3 'suits' of 9 ranks: Dots, Bamboos, and Chars. Each of these has 4 copies."
  print "There are 4 'wind' tiles and 3 'dragon' tiles, each of which has 4 copies."
  print "There are 4 'flower' tiles and 4 'season' tiles. These tiles do NOT have copies;"
  print "Instead, any of the 4 'season' tiles match any of the other 'season' tiles, and each of"
  print "the 4 'flower' tiles match any of the other 'flower' tiles.
  print ""
  x = 20
  y = 120
  yinc = 80
  print @(x,y) "Here are the 'Dot' suit tiles from Ace to Nine:"
  DrawTiles DOTS_INDEX, TILE_NRANKS, x, y+10
  inc y, yinc
  print @(x,y) "Here are the 'Bamboo' suit tiles from Ace to Nine:"
  DrawTiles BAMBOOS_INDEX, TILE_NRANKS, x, y+10
  inc y, yinc
  print @(x,y) "Here are the 'Chars' suit tiles from Ace to Nine:"
  DrawTiles CHARS_INDEX, TILE_NRANKS, x, y+10
  inc y, yinc
  print @(x,y) "Here are the 4 'Wind' tiles and the 3 'Dragon' Tiles:"
  DrawTiles WINDS_INDEX, 7, x, y+10
  inc y, yinc
  print @(x,y) "Here are the 4 'Flower' tiles and the 4 'Season' Tiles:"
  DrawTiles FLOWERS_INDEX, 8, x, y+10
  print @(40,560) "Press any key to continue"
  z$ = INKEY$
  do
    z$ = INKEY$
  loop until z$ <> ""
  ShowHelp2
end sub

sub ShowHelp2
  local z$
  cls
  print "Note that some of the Ideograms may look very similar but with closer inspection"
  print "you will see they are different. For instance, the 'Season' tiles and the 'Flower'"
  print "tiles look similar, but you should notice that the 'Season' tiles are numbered on the"
  print "right with a RED number, but the flowers are numbered on the left with a BLUE number."
  print "The 4 'Wind' tiles look alike at first glance but a closer look will show that they"
  print "all have a distinct ideogram.
  print ""
  print "There are 5 layers of tiles:
  DrawPuzzleLayout 40, 100
  print @(0,220) ""
  print "Each higher layer goes on top of the center tiles in the previous layer. Tiles that are"
  print "hidden under a layer cannot be seen or removed. If a tile is visible, it still cannot be"
  print "removed if it has neighbors in the same layer to its immediate left and right. Tiles that
  print "cannot be removed are shown grayed out.
  print ""
  print "To remove a pair of matching tiles, click each of them with the mouse. If they match, they"
  print "will be removed, uncovering and maybe unblocking some of their neighbors.
  print "The game is won when you remove all tiles."
  print ""
  print "Keyboard Commands:
  print "    Backspace    Undo the Last move"
  print "    Home         Restart the current puzzle"
  print "    F12          Start a new puzzle"
  print "    ?            Repeat these help screens"
  print "    Escape       Quit the program:"
  print ""
  print ""
  print "Press any key to continue"
  z$ = INKEY$
  do
    z$ = INKEY$
  loop until z$ <> ""
  DrawPuzzle
end sub

' Draw the puzzle layout in schematic form
sub DrawPuzzleLayout xs, ys
  local x, y, row, col, layer, tw, th, ncols, fcol
  tw = 10 : th = 15
  for row = 1 to PZ_NROWS
    y = ys + (row-1)*th
    ncols = pz_cols(row, 1)
    fcol = (PZ_MAXCOLS - ncols)\2 + 1
    for col = 1 to ncols
      x = xs + (fcol+col)*tw
      box x, y, tw, th
    next col
  next row
  box xs, ys+4*th-th\2, tw, th
  box xs+tw, ys+4*th-th\2, tw, th
  box xs+14*tw, ys+4*th-th\2, tw, th
  inc xs, 14*tw
  for row = 1 to PZ_NROWS
    y = ys + (row-1)*th
    ncols = pz_cols(row, 2)
    if ncols = 0 then continue for
    fcol = (PZ_MAXCOLS - ncols)\2 + 1
    for col = 1 to ncols
      x = xs + (fcol+col)*tw
      box x, y, tw, th
    next col
  next row
  inc xs, 7*tw
  for row = 1 to PZ_NROWS
    y = ys + (row-1)*th
    ncols = pz_cols(row, 3)
    if ncols = 0 then continue for
    fcol = (PZ_MAXCOLS - ncols)\2 + 1
    for col = 1 to ncols
      x = xs + (fcol+col)*tw
      box x, y, tw, th
    next col
  next row
  inc xs, 5*tw
  for row = 1 to PZ_NROWS
    y = ys + (row-1)*th
    ncols = pz_cols(row, 4)
    if ncols = 0 then continue for
    fcol = (PZ_MAXCOLS - ncols)\2 + 1
    for col = 1 to ncols
      x = xs + (fcol+col)*tw
      box x, y, tw, th
    next col
  next row
  inc xs, 11*tw
  box xs, ys+4*th-th\2, tw, th
end sub


' Draw some specified range of tiles at the specified location
' (used in the Help Pages)
sub DrawTiles start, num, x, y
  local i, tx, tile
  for i = 1 to num
    tile = start+i-1
    tx = x + (i-1)*TXSTEP
    DrawTile tile, 0, tx, y, 0
  next i
end sub

' Return a uniformly-distributed random integer in the specified closed interval
function RandomIntegerInRange(a, b)
  local v, c
  do
    c = b-a+1
    v = a + (b-a+2)*rnd()
    if v >= a and v <= b then exit do
  loop
  RandomIntegerInRange = v
end function

' Only data statements past this point

' Puzzle rows per layer
data 8, 6, 4, 2, 1

' puzzle columns per row and layer
data 12, 8, 10, 12, 12, 10, 8, 12
data  0, 6,  6,  6,  6,  6, 6,  0
data  0, 0,  4,  4,  4,  4, 0,  0
data  0, 0,  0,  2,  2,  0, 0,  0

' Root filenames for individual tile images, both regular and dark versions
data "Dots1", "Dots2", "Dots3", "Dots4", "Dots5", "Dots6", "Dots7", "Dots8", "Dots9"
data "Bamb1", "Bamb2", "Bamb3", "Bamb4", "Bamb5", "Bamb6", "Bamb7", "Bamb8", "Bamb9"
data "Chrs1", "Chrs2", "Chrs3", "Chrs4", "Chrs5", "Chrs6", "Chrs7", "Chrs8", "Chrs9"
data "WindE", "WindS", "WindW", "WindN", "DragR", "DragG", "DragW"
data "SeasG", "SeasS", "SeasA", "SeasW", "FlowB", "FlowO", "FlowC", "FlowP"

data "Dots1D", "Dots2D", "Dots3D", "Dots4D", "Dots5D", "Dots6D", "Dots7D", "Dots8D", "Dots9D"
data "Bamb1D", "Bamb2D", "Bamb3D", "Bamb4D", "Bamb5D", "Bamb6D", "Bamb7D", "Bamb8D", "Bamb9D"
data "Chrs1D", "Chrs2D", "Chrs3D", "Chrs4D", "Chrs5D", "Chrs6D", "Chrs7D", "Chrs8D", "Chrs9D"
data "WindED", "WindSD", "WindWD", "WindND", "DragRD", "DragGD", "DragWD"
data "SeasGD", "SeasSD", "SeasAD", "SeasWD", "FlowBD", "FlowOD", "FlowCD", "FlowPD"


