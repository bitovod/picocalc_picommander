' PiCOMMANDER, v0.16m, 2025-12-13
'-------------------------------------
' https://forum.clockworkpi.com/t/building-an-mmbasic-utility-library/18496'
' https://github.com/onimahoni/Picocalc-Commander/tree/main
' edit Bitovod, size 17332
' FONT 1 width 8, height 12
' FONT 2 width 12, height 20
' FONT 3 width 16, height 24
' FONT 4 width 10, height 16
' FONT 5 width 24, height 32
' FONT 6 width 32, height 50
' FONT 7 width 6, height  8
' FONT 8 width 4, height  6
' ver 0.16m - matrix saver, time saver, cursor, ftop

Clear
Option BASE 1
Option EXPLICIT

CONST ver$ = "0.16m"
CONST pcn$ = "PiCOMMANDER"

CONST bg1 = RGB(0, 0, 0)        ' default background
CONST bg2 = RGB(64, 64, 64)     ' darkgray background
CONST fg1 = RGB(0, 180, 0)      ' standard txt color green
CONST fg2 = RGB(0, 220, 0)      ' bright txt color green
'CONST fg1 = RGB(255, 191, 0)      ' standard txt color amber
'CONST fg2 = RGB(255, 220, 0)      ' bright txt color amber
CONST fg3 = RGB(255, 255, 255)  ' white txt color
Dim red   = RGB(255, 0, 0)
Dim yellow = RGB(255, 255, 0)

CONST SS_TIME = 30000 ' screensaver timeout

Dim copyFile$, copySource$
Dim curPath$ = "B:/"
Dim fname$(128)
Dim curExt$ = "BAS"
Dim copyMode$
Dim prefix$
Dim fw, fh
dim tim1, saver
DIM cursor = 1
DIM ftop = 1

CONST htxt1$ = "1:    2:    3:    4:    5:    6:    7:    8:    9:  "
CONST htxt2$ = "  hlp   drv   viw   edt   cpy   ren   mkd   del   xx"

Drive "B:"

'----- Start Time Saver
SUB saver_time()
  LOCAL x=32 ' 0 .. 66
  LOCAL y=128 ' 0 .. 228
  LOCAL rax
  LOCAL ray

  RANDOMIZE TIMER
  COLOR fg1
  CLS
  
  x=INT(rnd()*66)
  y=INT(rnd()*228)

  if int(rnd()*4)>2 then 
    rax=1
  else 
    rax=-1
  endif
    
  if int(rnd()*4)>2 then 
    ray=1
  else 
    ray=-1
  endif

  Do
    Text x, y, Time$, , 3, 2
    Text x+128, y+60, Date$, c, 3, 1
    Pause 10
   
    x=x+rax
    y=y+ray
    if x<0 then rax=-rax
    if x>66 then rax=-rax
    if y<0 then ray=-ray
    if y>228 then ray=-ray
    
  Loop UNTIL Inkey$<>""  
end sub

'----- Start Matrix Saver
const mx_CHR_W=mm.info(fontwidth)
const mx_CHR_H=mm.info(fontheight)

const mx_HEAD_C=rgb(0, 255, 0)
const mx_TAIL_C=rgb(0, 160, 0)

dim mx_p(40), mx_t(40), mx_col_x(40)
dim mx_i, mx_x, mx_y, mx_clr_y

sub mx_new_trail(index)
  mx_p(index)=int(rnd()*12)
  mx_t(index)=int(rnd()*12)+4
end sub

function mx_rnd_chr$()
  mx_rnd_chr$=chr$(int(rnd()*90+33))
end function

sub saver_matrix
  randomize timer
  cls
  for mx_i=1 to 40 ' init cols and calculate x pos
    mx_col_x(mx_i)=(mx_i-1)*mx_CHR_W
    mx_new_trail(mx_i)
  next mx_i
  
  do while inkey$ = "" ' iterate over cols
    for mx_i=1 to 40 ' for all cols
      mx_x=mx_col_x(mx_i)
      mx_y=mx_p(mx_i)*mx_CHR_H

      if mx_p(mx_i)<25 then ' new random char
        color mx_TAIL_C
        ? @(mx_x,mx_y-mx_CHR_H) mx_rnd_chr$()
        if mx_p(mx_i)+1<25 then
          color mx_HEAD_C
          ? @(mx_x,mx_y) mx_rnd_chr$()
        endif
      endif

      mx_clr_y=(mx_p(mx_i)-mx_t(mx_i))*mx_CHR_H ' delete char at tail
      if mx_clr_y>=0 and mx_clr_y<25*mx_CHR_H then
        ? @(mx_x,mx_clr_y) " "
      endif

      mx_p(mx_i)=mx_p(mx_i)+1 ' increase y pos
      if (mx_p(mx_i)-mx_t(mx_i))>=25 then ' reset trail if done
        mx_new_trail(mx_i)
      endif
    next mx_i
  loop
  cls ' clean up
end SUB

'----- Start Commander
StartCommander

'----- FCOL (font, f_color, b_color)
SUB fcol(fnt%,fg%,bg%)
  FONT fnt%
  COLOR fg%,bg%
  fw = MM.FONTWIDTH
  fh = MM.FONTHEIGHT  
end SUB

'----- subroutine: Draw Frame
SUB drawFrm()
  Box 0,0,320,32,,fg1
  Box 0,33,320,264,1,fg1    
  Box 0,298,320,22,1,fg1    
END SUB

'----- subroutine: Draw Header w clock & batt
SUB drawHdr
  local fnt = MM.INFO(FONT)
  local fg = MM.INFO(FCOLOUR)
  local bg = MM.INFO(BCOLOUR)
  local bat = mm.info(battery)
  
  fcol(7,fg3,bg)
  if mm.info(charging)>0 then
    color fg3
  else
    if bat <=15 then
      color yellow
    else
      color fg1
    end if
  end if

  ? @(158, 8) "BATT";
  ? @(157, 18) FORMAT$(bat, "%3.0f")
  ? @(158+3*6, 17) "%"; 
  fcol(3,fg3,bg)
  ? @(188, 6) Time$;
  fcol(fnt,fg,bg) 
end sub

'----- subroutine: Commander
Sub StartCommander 
  Local a$, f$
  Local restart, x=0, y=36

  Do
    restart = 0
    CLS bg1    
    drawFrm()    
    line 4, 277, 316, 277, 1, fg1
    fcol(1,fg1,bg1) : ?@(4,4) pcn$; : ?@(5,4,1) pcn$;            
    fcol(7,fg3,bg1) : ?@(4,306,1) htxt1$;
    fcol(7,fg1,bg1) : ?@(4,306,1) htxt2$;

    fcol(1,fg3,bg1)
    If curExt$ <> "*" And curExt$ <> "BAS" Then curExt$ = "BAS"
    a$ = dirwin$(x,y,14,curPath$,curExt$)

    Select Case a$
      Case "HLP"
        ShowHLP
        restart = 1
        If curExt$ <> "*" And curExt$ <> "BAS" Then curExt$ = "BAS"
      Case "RST"
        restart = 1     
      Case "CNT"
        restart = 1            
      Case ""
        restart = 1
      Case Else
        If Instr(a$, "$") > 0 Then
          f$ = Mid$(a$, Instr(a$, "$") + 1)
          If UCase$(Right$(f$,4)) = ".BAS" Then
            RUN f$
          End If
        End If
    End Select
  Loop While restart = 1
End Sub

'----- subroutine: Help Display
Sub ShowHLP
  Local j, k$
  Local txt$(16)
	
  CONST ttxt$ = "KEY OVERVIEW"
  CONST htxt$ = "Press any key to quit"
' txt$(1)  = "123456789012345678901234567890123456789" 
  txt$(1)  = ""
  txt$(2)  = "  up/down   Move cursor"
  txt$(3)  = "  ENTER     Open file/folder"
  txt$(4)  = "  D         Switch drive"
  txt$(5)  = "  F1        Show this help"
  txt$(6)  = "  N         Create folder"
  txt$(7)  = "  DEL       Delete file/folder"
  txt$(8)  = "  C         Copy file"
  txt$(9)  = "  X         Cut file"
  txt$(10) = "  V         Paste into folder"
  txt$(11) = "  R         Rename Folder"
  txt$(12) = "  F         Filter all/.bas"  
  txt$(13) = "  ScrSaver " + FORMAT$(SS_TIME/1000, "% 3.0f") + " s"
  txt$(14) = ""
  txt$(15) = ""
  txt$(16) = "                          Version "+ver$
 
  CLS bg1  
  drawFrm()
  fcol(1,fg1,bg1) : ? @(4, 4) ttxt$ : ? @(5, 4, 1) ttxt$
  fcol(7,fg1,bg1) : ? @( ((320 - Len(htxt$) * 6) / 2), 305) htxt$;
  
  fcol(1,fg3,bg1)
  For j = 1 To 16
    ? @(4, j*16 + 24) txt$(j)
  Next j
  
  tim1 = TIMER
  saver = timer
  Do  
    k$ = INKEY$
    if timer-saver > SS_TIME then
      saver_matrix()
      ''''saver_time()
      tim1 = TIMER
      saver = timer
      exit 
    end if
    if TIMER-tim1 > 200 then
      drawHdr
      tim1 = TIMER
    end if
  Loop Until k$ <> ""
        
  fcol(3,fg2,bg1)
end sub

'------------ edit
SUB EditFile(fname$)
	EDIT FILE fname$
	RUN "A:/pc.bas"
end sub

'----- subroutine: DeleteFolder
Sub DeleteFolderRecursive(folder$)
  Local f$, full$, d$(100), i%, count%

  If UCase$(folder$) = UCase$(Cwd$) Then
    ? "Cannot delete current directory!"
    Exit Sub
  End If

  If Right$(folder$, 1) = "/" Then folder$ = Left$(folder$, Len(folder$) - 1)

  f$ = Dir$(folder$ + "/*", FILE)
  Do While f$ <> ""
    full$ = folder$ + "/" + f$
    On Error Ignore
    Kill full$
    On Error Abort
    f$ = Dir$()
  Loop

  count% = 0
  f$ = Dir$(folder$ + "/*", DIR)
  Do While f$ <> ""
    If f$ <> "." And f$ <> ".." Then
      count% = count% + 1
      d$(count%) = folder$ + "/" + f$
    End If
    f$ = Dir$()
  Loop

  For i% = 1 To count%
    DeleteFolderRecursive d$(i%)
  Next i%

  On Error Ignore
  Rmdir folder$
  On Error Abort
End Sub

'----- subroutine: ShowBMP
Sub ShowBMP(filename$)
  CLS
  If Dir$(filename$, FILE) <> "" Then
    Load Image filename$, 0, 0
    Do : Loop Until Inkey$ <> ""
  Else
    Color red, bg1
    ? "Not a valid BMP file!"
    Pause 1000
  End If
  CLS
End Sub

'----- subroutine: ShowTXT
Sub ShowTXT(filename$)
  Local line$, i, txt$(20)
  Local f%

  CLS bg1
  Color fg3,bg1

  Open filename$ For Input As #1
  i = 1
  Do While Not EOF(1) And i <= 20
    Line Input #1, line$
    txt$(i) = Left$(line$, 38) ' max. Zeichen pro Zeile
    i = i + 1
  Loop
  Close #1

  For f% = 1 To i - 1
    ? @(4, f% * fh + 10) txt$(f%)
  Next f%

  Do : Loop Until Inkey$ <> ""
  CLS bg1
End Sub

'----- subroutine: dirwin$
Function dirwin$(x, y, lines, path$, ext$)
  Local i, file$, p$, fcount, top, bottom
  Local cmode, endstate
  Local a$, o$, delname$, confirm$, newdir$, fline$
  Local k$, code%, src$, dst$, line$, renameSrc$, renameDst$, renameBuf$, editSrc$
  LOCAL fsize, fmod$

  dirwin$  = "RST"
  endstate = 1

  If ext$ = "" Or ext$ = "*" Then
    ext$ = "*"
  Else
    ext$ = "*." + UCase$(ext$)
  End If

  If path$ <> "" Then
    p$ = path$
  Else
    p$ = Cwd$
  End If

  p$ = Cwd$
  On Error Skip
  Chdir p$
  If MM.Errno Then
    p$ = "A:/"
    Drive "A:"
  End If

  Do
    If endstate = 1 Then
      fcount = 0 ' : ftop = 1 ' : cursor = 1
      If Len(p$) > 3 Then
        fcount = 1
        fname$(1) = "1.."
      End If
      On Error Skip 9  
          
      file$ = Dir$("*", DIR)
      Do While file$ <> "" And fcount < 128
        If Left$(file$,1) <> "." Then
          fcount = fcount + 1
          fname$(fcount) = "1" + file$
        End If
        file$ = Dir$()
      Loop
      
      file$ = Dir$(ext$, FILE)
      Do While file$ <> "" And fcount < 128
        If Left$(file$,1) <> "." Then
          fcount = fcount + 1
          fname$(fcount) = "2" + file$
        End If
        file$ = Dir$()
      Loop      
    End If

    If Left$(p$,2) = "A:" Then
      prefix$ = Chr$(168) + " "
    ElseIf Left$(p$,2) = "B:" Then
      prefix$ = Chr$(153) + " "
    Else
      prefix$ = ""
    End If

    fcol(1,fg3,bg2)
    box x+4,y+1,313,14,2,bg2,bg2
    ? @(x+6, y+2) Left$(prefix$ + p$ + Space$(38), 38)

    Color fg3,bg1
    top    = ftop
    bottom = ftop + lines - 1
    
    For i = top To bottom
      If i <= fcount Then
        a$ = Mid$(fname$(i),2) 
        fsize = MM.INFO(FILESIZE a$)        
        fmod$ = MM.INFO$(MODIFIED a$) 

        IF LEN(a$) > 14 THEN
          a$ = LEFT$( a$, 13 ) + ">"
        END IF  

        file$ = Left$(a$ + Space$(15), 15)   
               
        If Left$(fname$(i),1) = "1" Then
          file$ = UCASE$(file$)
          file$ = Left$(file$ + Space$(34), 34) + "[DIR]"
        Else     
          IF fsize >= 1000000 THEN
            file$ = file$ + fmod$ + FORMAT$(fsize/1048576, "% 4.1fM")
          elseIF fsize >= 10000 THEN            
            file$ = file$ + fmod$ + FORMAT$(fsize/1024, "% 4.0fk")
          ELSE
            file$ = file$ + fmod$ + FORMAT$(fsize, "% 5.0f")            
          END IF
        End If
        
        If i = ftop-1 + cursor Then
          cmode = 2      
          If Left$(fname$(i),1) = "2" Then
            Open Mid$(fname$(i),2) For Input As #1
            Line Input #1, fline$
            Close #1    
          else
            fline$ = ""
          end if  
          
          box x+4,y+16+(i-ftop)*16,313,15,2,fg3  
          fcol(7,fg1,bg1)            
          ? @(x+4, 284) "Title: "; 
					color(fg3) : ? LEFT$(Space$(45-LEN(fline$)) + fline$, 45); 
					
          fcol(1,fg3,bg1)
        Else
          cmode = 0   
          box x+4,y+16+(i-ftop)*16,313,15,2,bg1
        End If
      Else
        file$ = Space$(39)
        cmode = 0
      End If    
        
      ? @(x+4, y+18+(i-ftop)*16, cmode) file$
    Next i

    tim1 = TIMER
    saver = timer 
    Do
      k$ = Inkey$   
      if timer-saver >= SS_TIME then
        ''''saver_matrix()
        saver_time()
        saver = timer
        curPath$ = p$
        dirwin$ = "CNT"
        Exit Function
      end if      
      if TIMER-tim1 >= 200 then
         drawHdr
         tim1 = TIMER
      end if
    Loop Until k$ <> ""
    
    fcol(1,fg1,bg1)
    code%    = Asc(k$)
    endstate = 0
    
    Select Case code%
      Case 128, 130      ' DOWN, RIGHT key
        If cursor > 1 Then
          INC cursor, -1          
          endstate = 2
        ElseIf ftop > 1 Then
          INC ftop, -1
          endstate = 3
        End If
      
      Case 129, 131   ' UP, LEFT key
        If (cursor + ftop - 1) < fcount Then
          If cursor < lines Then
            inc cursor
            endstate = 4
          Else
            inc ftop
            endstate = 2
          End If
        End If
        
      Case 27, 48  ' ESC, 0 key
        CLS
        dirwin$ = "QUIT"
        Exit Function
  
      Case 127, 56  ' DEL key, 8
        delname$ = Mid$(fname$(cursor+ftop-1),2)
        Color fg3, red
        ? @(x+4, y+2) "Delete file "+delname$+"? Y/N "
        Do : confirm$ = Inkey$ : Loop Until confirm$ <> ""
        If UCase$(confirm$) = "Y" Then
          If Left$(fname$(cursor+ftop-1),1) = "1" Then
            DeleteFolderRecursive delname$
          Else
            Kill delname$
          End If
        End If
        curPath$ = p$
        dirwin$ = "RST"
        Exit Function

      Case 145, 49  ' F1, 1
        dirwin$ = "HLP"
        Exit Function                             

      CASE 55, 78, 110  ' 7, N,  key
        newdir$ = ""
        Color fg3, bg2
        ? @(x+4, y+2) "New folder: ";
        Input newdir$
        If newdir$ <> "" Then Mkdir newdir$
        curPath$ = p$
        dirwin$ = "RST"
        Exit Function
      
      CASE 50, 68, 100  ' "2", "D", "d"
        If Left$(p$,1) = "A" Then Drive "B:" Else Drive "A:"
        p$ = Cwd$
        dirwin$ = "RST"
        Exit Function        

      CASE 51, 69, 101  ' "3", "D", "d"
				editSrc$ = Mid$(fname$(cursor + ftop - 1), 2)
				EditFile(editSrc$)
        p$ = Cwd$
        dirwin$ = "RST"
        Exit Function  
				
      Case 67, 99   ' "C", "c"
        If Left$(fname$(cursor+ftop-1),1) = "1" Then
          Color red, bg2
          ? @(x+4, y+2) "Cannot copy a folder!"
          Do : k$ = Inkey$ : Loop Until k$ <> ""
          endstate = 1
        Else
          copyFile$   = Mid$(fname$(cursor+ftop-1),2)
          copySource$ = p$
          copyMode$   = "COPY"
          Color yellow, bg2
          ? @(x+4, y+2) "copy file: " + copyFile$
          Do : k$ = Inkey$ : Loop Until k$ <> ""
        End If

      Case 88, 120      ' "X", "x"
        If Left$(fname$(cursor+ftop-1),1) = "1" Then
          Color red, bg2
          ? @(x+4, y+2) "Cannot cut a folder!"
          Do : k$ = Inkey$ : Loop Until k$ <> ""
          endstate = 1
        Else
          copyFile$   = Mid$(fname$(cursor+ftop-1),2)
          copySource$ = p$
          copyMode$   = "CUT"
          Color yellow, bg2
          ? @(x+4, y+2) "cut file: " + copyFile$
          Do : k$ = Inkey$ : Loop Until k$ <> ""
        End If

      Case 86, 118      ' "V", "v"      
        If copyFile$ <> "" Then
          If Right$(copySource$,1) <> "/" Then copySource$ = copySource$ + "/"
          If Right$(p$,1) <> "/" Then p$ = p$ + "/"
          src$ = copySource$ + copyFile$
          dst$ = p$ + copyFile$
          If UCase$(src$) <> UCase$(dst$) Then
            Open src$ For Input As #1
            Open dst$ For Output As #2
            Do While Not EOF(1)
              Line Input #1, line$
              ? #2, line$
            Loop
            Close #1 : Close #2
            If copyMode$ = "CUT" Then Kill src$
          End If
        End If
        copyFile$ = "" : copySource$ = "" : copyMode$ = "" : endstate = 1

      Case 82, 114      ' "R", "r"
        renameSrc$ = Mid$(fname$(cursor + ftop - 1), 2)
        If Right$(p$,1) <> "/" Then p$ = p$ + "/"
        Color fg3, bg2
        ? @(x+4, y+2) "New name:";
        Input renameDst$, x+84, y+2
        renameDst$ = p$ + renameDst$
        If renameDst$ <> "" Then
          If Dir$(renameSrc$) <> "" Then
            Open renameSrc$ For Input As #1
            Open renameDst$ For Output As #2
            Do While Not EOF(1)
              Line Input #1, renameBuf$
              ? #2, renameBuf$
            Loop
            Close #1 : Close #2 : Kill renameSrc$
          EndIf
        EndIf
        dirwin$ = "RST"
        curPath$ = p$
        Exit Function

      Case 70, 102    ' "F", "f"
        If curExt$ = "*" Then
          curExt$ = "BAS"
        Else
          curExt$ = "*"
        End If
        curPath$ = p$
        dirwin$ = "RST"
        Exit Function        

      Case 13, 10   ' ENTER key
        a$ = fname$(cursor + ftop - 1)
        o$ = Mid$(a$, 2)
        If Left$(a$,1) = "1" Then
          endstate = 1
          If o$ = ".." Then
            On Error Skip
            Chdir ".."
            p$ = Cwd$
            curPath$ = p$
          Else
            If Right$(p$,1) <> "/" Then p$ = p$ + "/"
            p$ = p$ + o$
            On Error Skip
            Chdir p$
            curPath$ = p$
            If MM.Errno Then Exit Function
          End If
        Else
          If UCase$(Right$(o$, 4)) = ".BMP" Then
            ShowBMP o$
            Exit Function
          ElseIf UCase$(Right$(o$, 4)) = ".WAV" Then
            Play WAV o$
            Do : Loop Until Inkey$ <> ""
            Exit Function
          ElseIf UCase$(Right$(o$, 4)) = ".MP3" Then
            Play MP3 o$
            Do : Loop Until Inkey$ <> ""
            Exit Function            
          ElseIf UCase$(Right$(o$, 4)) = ".TXT" Then
            ShowTXT o$
            Exit Function
          ElseIf UCase$(Right$(o$, 4)) = ".BAS" Then
            dirwin$ = p$ + "$" + o$
            curPath$ = p$
            Exit Function
          End If
        End If        
    End Select
  Loop
  dirwin$ = "RST"
End Function
