;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@                                                                            @
;@                          M i n e - S w e e p e r                           @
;@                                                                            @
;@             (c) 2004-2007 by Prodatron / SymbiosiS (Jörn Mika)             @
;@                                                                            @
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

relocate_start

;Todo
;- highscore abspeichern
;- user defined -> min/max werte statt nur min werte bei falschem wert

;==============================================================================
;### CODE-TEIL ################################################################
;==============================================================================

;### PROGRAMM-KOPF ############################################################

prgdatcod       equ 0           ;Länge Code-Teil (Pos+Len beliebig; inklusive Kopf!)
prgdatdat       equ 2           ;Länge Daten-Teil (innerhalb 16K Block)
prgdattra       equ 4           ;Länge Transfer-Teil (ab #C000)
prgdatorg       equ 6           ;Original-Origin
prgdatrel       equ 8           ;Anzahl Einträge Relocator-Tabelle
prgdatstk       equ 10          ;Länge Stack (Transfer-Teil beginnt immer mit Stack)
prgdatrs1       equ 12          ;*reserved* (3 bytes)
prgdatnam       equ 15          ;program name (24+1[0] chars)
prgdatflg       equ 40          ;flags (+1=16colour icon available)
prgdat16i       equ 41          ;file offset of 16colour icon
prgdatrs2       equ 43          ;*reserved* (5 bytes)
prgdatidn       equ 48          ;"SymExe10"
prgdatcex       equ 56          ;zusätzlicher Speicher für Code-Bereich
prgdatdex       equ 58          ;zusätzlicher Speicher für Data-Bereich
prgdattex       equ 60          ;zusätzlicher Speicher für Transfer-Bereich
prgdatres       equ 62          ;*reserviert* (28 bytes)
prgdatism       equ 90          ;Icon (klein)
prgdatibg       equ 109         ;Icon (gross)
prgdatlen       equ 256         ;Datensatzlänge

prgpstdat       equ 6           ;Adresse Daten-Teil
prgpsttra       equ 8           ;Adresse Transfer-Teil
prgpstspz       equ 10          ;zusätzliche Prozessnummern (4*1)
prgpstbnk       equ 14          ;Bank (1-8)
prgpstmem       equ 48          ;zusätzliche Memory-Bereiche (8*5)
prgpstnum       equ 88          ;Programm-Nummer
prgpstprz       equ 89          ;Prozess-Nummer

prgcodbeg   dw prgdatbeg-prgcodbeg  ;Länge Code-Teil
            dw prgtrnbeg-prgdatbeg  ;Länge Daten-Teil
            dw prgtrnend-prgtrnbeg  ;Länge Transfer-Teil
prgdatadr   dw #1000                ;Original-Origin                    POST Adresse Daten-Teil
prgtrnadr   dw relocate_count       ;Anzahl Einträge Relocator-Tabelle  POST Adresse Transfer-Teil
prgprztab   dw prgstk-prgtrnbeg     ;Länge Stack                        POST Tabelle Prozesse
            dw 0                    ;*reserved*
prgbnknum   db 0                    ;*reserved*                         POST bank number
            db "MineSweeper":ds 13:db 0 ;Name
            db 1                    ;flags (+1=16c icon)
            dw prgicn16c-prgcodbeg  ;16 colour icon offset
            ds 5                    ;*reserved*
prgmemtab   db "SymExe10"           ;SymbOS-EXE-Kennung                 POST Tabelle Speicherbereiche
            dw 0                    ;zusätzlicher Code-Speicher
            dw 0                    ;zusätzlicher Data-Speicher
            dw 16*15*15             ;zusätzlicher Transfer-Speicher
            ds 28                   ;*reserviert*
prgicnsml   db 2,8,8,#11,#00,#54,#A2,#30,#C0,#61,#F1,#F8,#E0,#30,#C0,#54,#A2,#00,#88
prgicnbig   db 6,24,24
            db #00,#00,#20,#C0,#00,#00,#00,#00,#F0,#F0,#00,#00,#30,#30,#00,#07,#C0,#C0,#60,#C0,#02,#04,#3C,#60,#70,#81,#2E,#2F,#3C,#E0,#30,#02,#47,#0F,#9E,#C0,#20,#01,#0F,#0F,#DE,#C0,#20,#0F,#6F,#6F,#D6,#C0
            db #40,#4F,#FF,#FF,#7D,#E0,#41,#13,#3C,#F7,#FF,#E0,#C3,#57,#C8,#FB,#FD,#F0,#40,#BF,#C8,#F9,#F6,#F1,#C2,#17,#FB,#F0,#FA,#F0,#C2,#1F,#FC,#F0,#FD,#F0,#61,#3F,#FE,#F0,#F4,#E0,#71,#3E,#FA,#F0,#FA,#E0
            db #30,#FB,#F5,#F9,#F0,#C0,#30,#F4,#FA,#FC,#F8,#C0,#10,#F1,#F2,#F0,#F0,#80,#10,#F0,#F0,#F0,#F0,#80,#31,#F0,#F0,#F0,#F0,#C8,#30,#F0,#F0,#F0,#F0,#C0,#10,#80,#F0,#F0,#10,#80,#00,#00,#30,#C8,#00,#00


;### PRGPRZ -> Programm-Prozess
dskprzn     db 2
sysprzn     db 3
windatprz   equ 3   ;Prozeßnummer
windatsup   equ 51  ;Nummer des Superfensters+1 oder 0
prgwin      db 0    ;Nummer des Haupt-Fensters
diawin      db 0    ;Nummer des Dialog-Fensters

cfgfldsiz   db 9,9  ;Größe des Feldes (X,Y)
cfgminanz   db 10   ;Anzahl versteckter Mienen

fldmin  db 0        ;Anzahl markierter Felder
fldopn  db 0        ;Anzahl aufgedeckter Felder
fldanz  db 0        ;Anzahl Felder gesamt [Sieg -> fldanz=fldopn+cfgminanz]

gamtmr  dw 0        ;Timer
gamtol  db 0        ;letzte Sekunde
gammod  db 0        ;0=nicht gestartet, 1=Spiel läuft, 2=beendet

minfld  ds 15*15    ;Mienenfeld (0=unberührt, 1-9=aufgedeckt [x-1 Nachbarn], 10=aufgedeckt undefiniert, 16=Mine vermutet, +128=Miene)

rndtab              ;enthält 128 Random-Werte
db 180,136,148,74,77,198,3,194,208,181,11,105,220,202,95,246,223,14,243,93,134,196,13,151,119,76,159,165,67,71,212,211,150,252,233
db 58,177,250,62,136,27,255,173,4,147,25,26,204,72,11,75,97,77,242,250,102,71,41,41,165,104,105,182,83,162,53,47,149,20,117,231,66
db 201,96,74,235,161,160,109,25,143,177,233,213,5,139,234,110,173,128,131,118,90,103,69,14,62,250,15,99,93,125,39,121,65,160,138,40
db 240,167,129,99,27,200,117,192,152,213,4,53,18,26,84,32,0,137,168,139

prgprz  ld a,(prgprzn)
        ld (prgwindat+windatprz),a
        ld (configwin+windatprz),a
        ld (higscowin+windatprz),a
        ld (higentwin+windatprz),a
        call fldini             ;Feld initialisieren
        call fldwin             ;Fenster initialisieren
        ld (prgwindat+08),de
        ld (prgwindat+10),hl
        ld (prgwindat+16),de
        ld (prgwindat+18),hl
        call gamanz0
        call SySystem_HLPINI

        ld c,MSC_DSK_WINOPN
        ld a,(prgbnknum)
        ld b,a
        ld de,prgwindat
        call msgsnd             ;Fenster aufbauen
prgprz1 call msgdsk             ;Message holen -> IXL=Status, IXH=Absender-Prozeß
        cp MSR_DSK_WOPNER
        jp z,prgend             ;kein Speicher für Fenster -> Prozeß beenden
        cp MSR_DSK_WOPNOK
        jr nz,prgprz1           ;andere Message als "Fenster geöffnet" -> ignorieren
        ld a,(prgmsgb+4)
        ld (prgwin),a           ;Fenster wurde geöffnet -> Nummer merken

prgprz0 call msgget
        jr nc,prgprz0
        cp MSR_DSK_WCLICK       ;*** Fenster-Aktion wurde geklickt
        jr nz,prgprz0
        ld e,(iy+1)
        ld a,(prgwin)
        cp e
        jr z,prgprz4
        ld a,(diawin)
        cp e
        jr nz,prgprz0
        ld a,(iy+2)             ;*** DIALOG-FENSTER
        cp DSK_ACT_CLOSE        ;*** Close wurde geklickt
        jp z,diacnc
        jr prgprz5
prgprz4 ld a,(iy+2)             ;*** HAUPT-FENSTER
        cp DSK_ACT_CLOSE        ;*** Close wurde geklickt
        jp z,prgend
prgprz5 cp DSK_ACT_MENU         ;*** Menü wurde geklickt
        jr z,prgprz2
        cp DSK_ACT_CONTENT      ;*** Inhalt wurde geklickt
        jr nz,prgprz0
        ld a,(iy+9)
        or a
        jr nz,prgprz2
        ld a,(iy+8)
        or a
        jr z,prgprz0
        jp fldclk
prgprz2 ld l,(iy+8)
        ld h,(iy+9)
        ld a,l
        or h
        jr z,prgprz0
        ld a,(iy+3)             ;A=Klick-Typ (0/1/2=Maus links/rechts/doppelt, 7=Tastatur)
        jp (hl)

;### PRGEND -> Programm beenden
prgend  ld a,(prgprzn)
        db #dd:ld l,a
        ld a,(sysprzn)
        db #dd:ld h,a
        ld iy,prgmsgb
        ld (iy+0),MSC_SYS_PRGEND
        ld a,(prgcodbeg+prgpstnum)
        ld (iy+1),a
        rst #10
prgend0 rst #30
        jr prgend0

;### PRGINF -> Info-Fenster anzeigen
prginf  ld hl,prgmsginf         ;*** Info-Fenster
        ld b,1+128
        call prginf0
        jp prgprz0
prginf0 ld (prgmsgb+1),hl
        ld a,(prgbnknum)
        ld c,a
        ld (prgmsgb+3),bc
        ld a,MSC_SYS_SYSWRN
        ld (prgmsgb),a
        ld a,(prgprzn)
        db #dd:ld l,a
        ld a,(sysprzn)
        db #dd:ld h,a
        ld iy,prgmsgb
        rst #10
        ret


;==============================================================================
;### GAME-ROUTINEN ############################################################
;==============================================================================

;### GAMNEW -> Spiel resetten und neu starten
gamnew  call gamnew0
        ld e,-1
        call fldupd3        ;gesamtes Fenster neu aufbauen
        jp prgprz0
gamnew0 xor a
        ld l,a
        ld h,a
        ld (fldmin),a       ;markierte Mienen = 0
        ld (fldopn),a       ;freie gefundene Felder = 0
        ld (gamtmr),hl      ;Zeit = 0
        ld (gammod),a       ;Modus = 0
        rst #20:dw jmp_timget
        inc a
        ld (gamtol),a
        call fldini
        call gamtim0
        call gamanz0
        ld hl,sprsmihap
        ld (prgwinobj0),hl
        ret

;### GAMUPD -> Updated Game-Timer
gamupdc db 0
gamupd  ld a,(gamupdc)
        inc a
        cp 50
        jr c,gamupd0
        xor a
gamupd0 ld (gamupdc),a
        ret nz
        rst #20:dw jmp_timget
        ld hl,gamtol
        cp (hl)
        ret z
        ld (hl),a
        ld de,(gamtmr)
        ld hl,998
        or a
        sbc hl,de
        ret c
        inc de
        ld (gamtmr),de
        jr gamtim

;### GAMTIM -> Zeigt die abgelaufene Zeit an
gamtim  call gamtim0
        ld e,7
        jp fldupd3
gamtim0 ld hl,(gamtmr)
        ld de,timtxt
gamtim1 push de
        call clcnd3
        pop de
        ldi:ldi:ldi
        ret

;### GAMANZ -> Zeigt die Anzahl noch übriger Mienen an
gamanz  call gamanz0
        ld e,4
        jp fldupd3
gamanz0 ld a,(cfgminanz)
        ld hl,fldmin
        sub (hl)
        jr nc,gamanz1
        neg
        call gamanz1
        ld a,"-"
        ld (anztxt),a
        ret
gamanz1 ld l,a
        ld h,0
        ld de,anztxt
        jr gamtim1

;### GAMTYP -> Stellt Feldgröße ein
gamtypn db 0                ;0=Beginner, 1=Advanced, 2=Selfdefined
gamtyp1 ld bc,9*256+9
        ld hl,8*2+2+prgwinmen1
        ld a,10
        ld e,0
        jr gamtyp0
gamtyp2 ld bc,15*256+15
        ld hl,8*3+2+prgwinmen1
        ld a,35
        ld e,1
;BC=Größe, A=Anzahl, HL=Menüpunkt
gamtyp0 ld ix,8*2+2+prgwinmen1
        ld (ix+00),1+0
        ld (ix+08),1+0
        ld (ix+16),1+0
        ld (hl),1+2
        ld (cfgminanz),a
        ld a,e
        ld (gamtypn),a
        ld (cfgfldsiz),bc
        call gamnew0
        call fldwin
        ld a,(prgwin)
        ld b,a
        ld c,MSC_DSK_WINSIZ         ;Fenstergröße anpassen
        call msgsnd
        jp prgprz0


;==============================================================================
;### HIGHSCORE-ROUTINEN #######################################################
;==============================================================================

;### HIGNEW -> Neuen Eintrag übernehmen
hignew  call diaclo
        jr higshw

;### HIGSHW -> Zeigt Highscore-Liste
higshw  call higshw0
        ld de,higscowin
        jp diaopn
higshw0 ld ix,(higscotim1)
        ld iy,higscotxt3
        call higshw1
        ld ix,(higscotim2)
        ld iy,higscotxt4
higshw1 ld de,0
        call clcn32
        db #fd:ld e,l
        db #fd:ld d,h
        inc de
        ld hl,higscotxt6
        ld bc,6
        ldir
        ret

;### HIGCLR -> Löscht Highscore-Liste
higclrt dw 9,0,9,13,0
        db "Prodatron":ds 5:dw 999
higclr  ld hl,higclrt
        ld de,higscobuf1-10
        ld bc,13+1+2+10
        ldir
        ld hl,higclrt
        ld de,higscobuf2-10
        ld bc,13+1+2+10
        ldir
        call higshw0
        ld c,MSC_DSK_WINDIN
        ld a,(diawin)
        ld b,a
        ld e,-1
        call msgsnd
        jp prgprz0

;### HIGENT -> Eintrag in der Highscore-Liste ändern
;### Eingabe    A=Typ (0 oder 1), HL=Zeit
higent  ld bc,higscotxt8a
        ld de,higscoinp1
        ld ix,higscotim1
        or a
        jr z,higent1
        ld bc,higscotxt8b
        ld de,higscoinp2
        ld ix,higscotim2
higent1 ld (higscodsc8),bc
        ld (higentdat2),de
        ld e,(ix+0)
        ld d,(ix+1)
        ex de,hl
        sbc hl,de
        jp c,prgprz0
        jp z,prgprz0
        ld (ix+0),e
        ld (ix+1),d
        ld de,higentwin
        jp diaopn


;==============================================================================
;### FELD-ROUTINEN ############################################################
;==============================================================================

;### FLDCLK -> Klick auf Feld fand stand
;### Eingabe    A=Feld-Nummer (ab 1), (IY+3)=Clicktyp
fldclkn db 0            ;Feldnummer
fldclk  dec a
        ld (fldclkn),a
        ld c,a
        ld a,(gammod)
        cp 2
        jp z,prgprz0
        ld a,1
        ld (gammod),a   ;Spiel starten, falls nicht bereits geschehen
        ld a,c
        push af
        ld b,0
        ld a,(cfgfldsiz)
        ld e,a
        ld d,0
        call clcd16
        ld d,l              ;DE=Feldkoordinaten (E=X, D=Y)
        pop af
        ld l,a
        ld h,0
        ld bc,minfld
        add hl,bc           ;(HL)=Feldinhalt
        ld a,(iy+3)
        cp 1
        ld a,(hl)
        jr nz,fldclk3
        ld c,a              ;*** User ändert Mienenmarkierung
        and 127
        ld b,16
        jr z,fldclk1
        cp b
        jp nz,prgprz0       ;bereits freigelegtes Feld kann nicht geändert werden
        ld b,0
fldclk1 ld a,(fldmin)
        dec a
        bit 4,b
        jr z,fldclk9
        add 2
fldclk9 ld (fldmin),a
        ld a,c
        and 128
        or b
        ld (hl),a
        bit 4,a
        ld bc,sprfldunk
        jr z,fldclk2
        ld bc,sprfldmrk
fldclk2 call fldupd
        call gamanz
        jp fldclk7
fldclk3 ld a,(hl)           ;*** User vermutet keine Miene
        and 127
        jp nz,prgprz0           ;** Feld bereits aufgedeckt oder markiert -> fertig
        push hl
        push de
        ld hl,sprsmiafr
        call smiupd
        pop de
        pop hl
        bit 7,(hl)
        jr z,fldclk4
        set 4,(hl)              ;** auf Miene getrampelt
        ld bc,sprfldbox
        call fldupd
        ld c,0
        call fldshw
        ld a,2                      ;Spiel beenden
        ld (gammod),a
        ld hl,sprsmisad
fldclk8 call smiupd
        jp prgprz0
fldclk4 push hl                 ;** freies Feld gefunden
        push de
        call fldtst
        pop de
        pop hl
        jp nz,fldclk7
fldclk5 ld a,1                  ;** keine Nachbarmienen -> Umgebung analysieren
        ld (fldtstx),a
        ld a,(fldanz)
        ld (fldclkn),a          ;(fldclkn)=Nummer letztes Feld+1
        ld hl,minfld
        ld e,a
        ld d,0
        add hl,de               ;HL=letztes Feld+1
        ld de,(cfgfldsiz)       ;DE=Koordinaten letztes Feld+1
fldclk6 dec hl
        push hl
        push de
        ld ix,fldclkn
        dec (ix+0)
        dec e
        dec d
        ld a,(hl)
        cp 10
        call z,fldtst
        pop de
        pop hl
        dec e
        jr nz,fldclk6
        ld a,(cfgfldsiz)
        ld e,a
        dec d
        jr nz,fldclk6
        ld a,(fldtstx)
        or a
        jr z,fldclk5
fldclk7 ld a,(cfgminanz)
        ld hl,fldopn
        add (hl)
        ld hl,fldanz
        cp (hl)
        jr nz,fldclkb
        ld a,2
        ld (gammod),a
        ld c,1
        call fldshw
        ld hl,sprsmiwin
        call smiupd
        ld hl,(gamtmr)
        ld a,(gamtypn)
        cp 2
        jp c,higent
        jp prgprz0
fldclkb ld hl,sprsmihap
fldclkc jr fldclk8

;### FLDSHW -> nicht gefundene und falsche Bomben aufdecken
;### Eingabe    C=Typ (0=Mißerfolg, 1=Erfolg)
fldshw  ld a,(fldanz)
        ld b,a
        ld hl,minfld
        xor a
        ld (fldclkn),a
fldswh1 push bc
        push hl
        ld a,(hl)
        and 128+16
        dec c
        jr z,fldswh4
        cp 128
        ld bc,sprfldbo1
        jr z,fldswh2
        cp 16
        jr nz,fldshw3
        ld bc,sprfldbo0
fldswh2 call fldupd
        jr fldshw3
fldswh4 cp 128
        jr nz,fldshw3
        ld bc,sprfldmrk
        call fldupd
fldshw3 ld hl,fldclkn
        inc (hl)
        pop hl
        inc hl
        pop bc
        djnz fldswh1
        ret

;### FLDTST -> Nachbar-Felder auf Mienen überprüfen, Feld-Status plotten, nicht vermiente Nachbarfelder für weitere Untersuchung markieren
;### Eingabe    (HL)=Feld, E=X, D=Y, (fldclkn)=Nummer
;### Ausgabe    ZF=1 -> Feld hat keine Mienen als Nachbar, (fldtstx)=0 -> es wurden Nachbarfelder modifiziert
;### Verändert  BC,DE,HL,IX,IY
fldtstx db 0        ;Flag, ob Felder als "aufgedeckt-undefiniert" gekennzeichnet wurden

fldtstt db  1,-1,0   ;links
        db  2, 1,0   ;rechts
        db  4, 0,-1  ;oben
        db  8, 0, 1  ;unten
        db  5,-1,-1  ;links oben
        db  9,-1, 1  ;links unten
        db  6, 1,-1  ;rechts oben
        db 10, 1, 1  ;rechts unten

fldtst  push hl
        ld l,0
        ld bc,(cfgfldsiz)
        inc e
        dec e
        jr z,fldtst2
        set 0,l
fldtst2 inc d
        dec d
        jr z,fldtst3
        set 2,l
fldtst3 ld a,c
        dec a
        cp e
        jr z,fldtst4
        set 1,l
fldtst4 ld a,b
        dec a
        cp d
        jr z,fldtst5
        set 3,l
fldtst5 ld b,l          ;B=Flags (0=links, 1=rechts, 2=oben, 3=unten), C=Breite
        pop hl          ;HL=Zeiger auf aktuelles Feld
        ld ix,fldtstt
        ld iy,8         ;IYL=Zähler, IYH=Anzahl Nachbarmienen
        push bc
        push hl
fldtst6 push hl
        call fldtst1
        jr nz,fldtstc
        ld a,(hl)
        bit 7,a
        jr z,fldtstc
        db #fd:inc h    ;Nachbarmiene gefunden -> Zähler erhöhen
fldtstc pop hl
        inc ix
        inc ix
        inc ix
        db #fd:dec l
        jr nz,fldtst6
        db #fd:ld a,h   ;Anzahl Nachbar-Felder
        inc a
        ld (hl),a
        dec a
        ld l,a
        add a:add a:add a:add a
        add l:add l:add l
        ld l,a
        ld h,0
        ld bc,sprfldemp
        add hl,bc
        ld c,l
        ld b,h
        push iy
        call fldupd     ;Anzahl Nachbar-Mienen anzeigen
        ld hl,fldopn
        inc (hl)
        pop af
        pop hl
        pop bc
        or a
        ret nz          ;falls Nachbar-Mienen vorhanden -> fertig
        ld ix,fldtstt   ;ansonsten alle Nachbarfelder als aufgedeckt-undefiniert kennzeichnen
        ld iy,8         ;IYL=Zähler, IYH=Anzahl Nachbarmienen
fldtst7 push hl
        call fldtst1
        jr nz,fldtstd
        ld a,(hl)
        or a
        jr nz,fldtstd
        ld (hl),10
fldtstd pop hl
        inc ix
        inc ix
        inc ix
        db #fd:dec l
        jr nz,fldtst7
        xor a
        ld (fldtstx),a
        ret
;(HL)=Feld, (IX+0-2)=Infos, B=Flags, C=Breite -> ZF=0 -> in dieser Richtung kein Nachbarfeld vorhanden, ZF=1 -> (HL)=Nachbarfeld
fldtst1 ld e,(ix+0)
        ld a,b
        and e
        cp e
        ret nz
        ld e,(ix+1)
        ld d,0
        bit 7,e
        jr z,fldtst8
        dec d
fldtst8 add hl,de
        ld a,(ix+2)
        cp 1
        jr c,fldtsta
        ld e,c
        ld d,0
        jr z,fldtst9
        dec d
        ld a,c
        neg
        ld e,a
fldtst9 add hl,de       ;HL=Nachbarfeld (0=unberührt, 1-9=aufgedeckt [x-1 Nachbarn], 10=aufgedeckt undefiniert, 16=Mine vermutet, +128=Miene)
fldtsta xor a
        ret

;### FLDUPD -> Feld updaten
;### Eingabe    BC=Sprite, (fldclkn)=Feld
fldupdc db 0
fldupd  ld a,(fldclkn)
        ld l,a
        ld h,0
        add hl,hl
        add hl,hl
        add hl,hl
        add hl,hl
        ld de,prgwinobj1+4
        add hl,de
        ld (hl),c
        inc hl
        ld (hl),b
        add fldofs
        ld e,a
fldupd1 ld a,(fldupdc)
        inc a
        cp 16
        jr c,fldupd2
        rst #30                 ;nach 16 Update-Sends sleepen, damit Desktop-Prozess nachkommt
        xor a
fldupd2 ld (fldupdc),a
fldupd3 ld c,MSC_DSK_WINDIN
        ld a,(prgwin)
        ld b,a
        jp msgsnd

;### FLDWIN -> Paßt das Fenster an die Größe des Minen-Feldes an
;### Ausgabe    DE=neue Xlen, HL=neue Ylen
fldwin  ld hl,(cfgfldsiz)           ;HL=Xlen,Ylen
        ld a,l
        add a:add a:add a
        ld c,a                      ;C=Xlen*8
        add 2
        ld (16*1+prgwinobj+10),a
        ld a,h
        add a:add a:add a
        ld b,a                      ;B=Ylen*8
        add 2
        ld (16*1+prgwinobj+12),a
        ld a,l
        sub 9
        add a:add a
        add 32:ld (16*2+prgwinobj+6),a  ;Objekt-Positionen setzen
        ld a,l
        sub 9
        add a:add a:add a
        add 52:ld (16*6+prgwinobj+6),a
        inc  a:ld (16*8+prgwinobj+6),a
        add  4:ld (16*7+prgwinobj+6),a
        ld a,c
        add 8
        ld e,a
        ld d,0
        ld a,b
        add 28
        ld l,a
        ld h,d
        ret

;### FLDINI -> Löscht und initialisiert Minen-Feld
fldini  ld bc,(cfgfldsiz)   ;*** Objekte setzen
        ld hl,24*256+4
        ld ix,prgwinobj1
        ld iy,1
fldini1 ld de,sprfldunk
        db #fd:ld a,l
        ld (ix+00),a
        ld (ix+01),0
        ld (ix+02),8
        ld (ix+03),255
        ld (ix+04),e
        ld (ix+05),d
        ld (ix+06),l
        ld (ix+07),0
        ld (ix+08),h
        ld (ix+09),0
        ld (ix+10),8
        ld (ix+11),0
        ld (ix+12),8
        ld (ix+13),0
        ld (ix+14),0
        ld (ix+15),0
        ld de,16
        add ix,de
        db #fd:inc l
        ld a,l
        add 8
        ld l,a
        dec c
        jr nz,fldini2
        ld a,h
        add 8
        ld h,a
        ld l,4
        ld a,(cfgfldsiz)
        ld c,a
        dec b
fldini2 jr nz,fldini1
        db #fd:ld a,l       ;*** Feld löschen
        dec a                   ;A=Anzahl Felder
        ld (fldanz),a
        ld l,a
        ld h,0                  ;HL=Anzahl Felder
        add fldofs
        ld (prgwingrp),a
        ld c,l
        dec c
        ld b,h
        ld hl,minfld
        ld de,minfld+1
        ld (hl),0
        ldir
        call fldrnd         ;*** Mienen setzen
        ld a,(cfgminanz)
        ld b,a                  ;B=Anzahl Mienen
fldini3 push bc
        push hl
        ld h,0
        ld bc,rndtab
        add hl,bc
        ld a,(hl)
        add c
        ld c,a
        ld e,a
        ld d,0                  ;DE=0-255
        ld a,(fldanz)
        call clcm16             ;HL=[0-255]*Felderanzahl
        ld a,h                  ;A=[0-255]*Felderanzahl/256=Feld
        ld l,a
        ld h,0
        ld de,minfld
        add hl,de               ;(HL)=Feld
        bit 7,(hl)
        jr z,fldini4
        pop hl
        pop bc
        call fldrnd
        jr fldini3
fldini4 set 7,(hl)
        pop hl
        ld a,l
        add h
        and 127
        ld l,a
        pop bc
        djnz fldini3
        ret

;### FLDRND -> Ermittelt Zufallswerte
;### Ausgabe    L=0-127, H=1-4, C=0-255
;### Verändert  AF,DE
fldrnd  rst #30
        ld a,r
        and 127
        ld l,a
        ld h,0
        ld de,rndtab
        add hl,de
        ld c,(hl)               ;C=erster Wert (0-255)
        ld l,a                  ;L=Start in Random-Tabelle (0-127)
        rst #30
        ld a,r
        and 3
        inc a
        ld h,a                  ;H=Offset zu nächstem Eintrag (1-4)
        ret


;==============================================================================
;### CONFIG-ROUTINEN ##########################################################
;==============================================================================

cfgopn  ld ix,(cfgfldsiz+1)
        db #dd:ld h,0
        ld iy,configbuf1
        call cfgopn0
        ld ix,(cfgfldsiz+0)
        db #dd:ld h,0
        ld iy,configbuf2
        call cfgopn0
        ld ix,(cfgminanz)
        db #dd:ld h,0
        ld iy,configbuf3
        call cfgopn0
        ld de,configwin
        jp diaopn
cfgopn0 ld de,0
        push iy
        call clcn32
        ex (sp),iy
        pop hl
        db #fd:ld e,l
        db #fd:ld d,h
        or a
        sbc hl,de
        inc l
        ld (iy-6),l
        ld (iy-10),l
        ret

cfgset  call diaclo
        ld ix,configbuf2
        xor a
        ld bc,9
        ld de,15
        call clcr16
        ld a,9
        jr c,cfgset1
        ld a,l
cfgset1 ld (cfgfldsiz+0),a
        ld ix,configbuf1
        xor a
        ld bc,6
        ld de,15
        call clcr16
        ld a,9
        jr c,cfgset2
        ld a,l
cfgset2 ld (cfgfldsiz+1),a
        ld ix,configbuf3
        xor a
        ld bc,5
        ld de,50
        call clcr16
        ld a,10
        jr c,cfgset3
        ld a,l
cfgset3 ld (cfgminanz),a
        ld bc,(cfgfldsiz)
        ld a,(cfgminanz)
        ld hl,8*4+2+prgwinmen1
        ld e,2
        jp gamtyp0


;==============================================================================
;### DIALOG-ROUTINEN ##########################################################
;==============================================================================

diaopn  ld c,MSC_DSK_WINOPN     ;Fenster aufbauen
        ld a,(prgbnknum)
        ld b,a
        call msgsnd
diaopn1 call msgdsk             ;Message holen -> IXL=Status, IXH=Absender-Prozeß
        cp MSR_DSK_WOPNER
        ret z                   ;kein Speicher für Fenster -> dann halt nicht
        cp MSR_DSK_WOPNOK
        jr nz,diaopn1           ;andere Message als "Fenster geöffnet" -> ignorieren
        ld a,(prgmsgb+4)
        ld (diawin),a           ;Fenster wurde geöffnet -> Nummer merken
        inc a
        ld (prgwindat+windatsup),a
        jp prgprz0

diacnc  call diaclo             ;*** CANCEL
        jp prgprz0

diaclo  ld c,MSC_DSK_WINCLS     ;Dialog-Fenster schliessen
        ld a,(diawin)
        ld b,a
        jp msgsnd


;==============================================================================
;### SUB-ROUTINEN #############################################################
;==============================================================================

;### Eingabe    HL=Zahl
;### Ausgabe    HL=3Byte String
clcnd3b ds 2+3+1
clcnd3  push hl
        pop ix
        ld hl,48*256+48
        ld (clcnd3b),hl
        ld de,0
        ld iy,clcnd3b+2
        call clcn32
        push iy
        pop hl
        dec hl
        dec hl
        ret

;### SMIUPD -> Updated Smiley
;### Eingabe    HL=Sprite
smiupd  ld (prgwinobj0),hl
        ld e,2
        call fldupd1
        rst #30
        ret

;### MSGGET -> Message für Programm abholen
;### Ausgabe    CF=0 -> keine Message vorhanden, CF=1 -> IXH=Absender, (recmsgb)=Message, A=(recmsgb+0), IY=recmsgb
;### Veraendert 
msgget  ld a,(prgprzn)
        db #dd:ld l,a           ;IXL=Rechner-Prozeß-Nummer
        db #dd:ld h,-1
        ld iy,prgmsgb           ;IY=Messagebuffer
        ld a,(gammod)
        cp 1
        jr nz,msgget1
        push ix
        push iy
        call gamupd
        pop iy
        pop ix
        rst #30
        rst #18
        jr msgget2
msgget1 rst #08                 ;Message holen -> IXL=Status, IXH=Absender-Prozeß
msgget2 or a
        db #dd:dec l
        ret nz
        ld iy,prgmsgb
        ld a,(iy+0)
        or a
        jp z,prgend
        scf
        ret

;### MSGDSK -> Message für Programm von Desktop-Prozess abholen
;### Ausgabe    CF=0 -> keine Message vorhanden, CF=1 -> IXH=Absender, (recmsgb)=Message, A=(recmsgb+0), IY=recmsgb
;### Veraendert 
msgdsk  call msgget
        jr nc,msgdsk            ;keine Message
        ld a,(dskprzn)
        db #dd:cp h
        jr nz,msgdsk            ;Message von anderem als Desktop-Prozeß -> ignorieren
        ld a,(prgmsgb)
        ret

;### MSGSND -> Message an Desktop-Prozess senden
;### Eingabe    C=Kommando, B/E/D/L/H=Parameter1/2/3/4/5
msgsnd  ld a,(dskprzn)
msgsnd1 db #dd:ld h,a
        ld a,(prgprzn)
        db #dd:ld l,a
        ld iy,prgmsgb
        ld (iy+0),c
        ld (iy+1),b
        ld (iy+2),e
        ld (iy+3),d
        ld (iy+4),l
        ld (iy+5),h
        rst #10
        ret

;### SYSCLL -> Betriebssystem-Funktion aufrufen
;### Eingabe    (SP)=Modul/Funktion, AF,BC,DE,HL,IX,IY=Register
;### Ausgabe    AF,BC,DE,HL,IX,IY=Register
sysclln db 0
syscll  ld (prgmsgb+04),bc      ;Register in Message-Buffer kopieren
        ld (prgmsgb+06),de
        ld (prgmsgb+08),hl
        ld (prgmsgb+10),ix
        ld (prgmsgb+12),iy
        push af
        pop hl
        ld (prgmsgb+02),hl
        pop hl
        ld e,(hl)
        inc hl
        ld d,(hl)
        inc hl
        push hl
        ld (prgmsgb+00),de      ;Modul und Funktion in Message-Buffer kopieren
        ld a,e
        ld (sysclln),a
        ld iy,prgmsgb
        ld a,(prgprzn)          ;Desktop und System-Prozessnummer holen
        db #dd:ld l,a
        ld a,(sysprzn)
        db #dd:ld h,a
        rst #10                 ;Message senden
syscll1 rst #30
        ld iy,prgmsgb
        ld a,(prgprzn)
        db #dd:ld l,a
        ld a,(sysprzn)
        db #dd:ld h,a
        rst #18                 ;auf Antwort warten
        db #dd:dec l
        jr nz,syscll1
        ld a,(prgmsgb)
        sub 128
        ld e,a
        ld a,(sysclln)
        cp e
        jr nz,syscll1
        ld hl,(prgmsgb+02)      ;Register aus Message-Buffer holen
        push hl
        pop af
        ld bc,(prgmsgb+04)
        ld de,(prgmsgb+06)
        ld hl,(prgmsgb+08)
        ld ix,(prgmsgb+10)
        ld iy,(prgmsgb+12)
        ret

;### CLCM16 -> Multipliziert zwei Werte (16bit)
;### Eingabe    A=Wert1, DE=Wert2
;### Ausgabe    HL=Wert1*Wert2 (16bit)
;### Veraendert AF,DE
clcm16  ld hl,0
        or a
clcm161 rra
        jr nc,clcm162
        add hl,de
clcm162 sla e
        rl d
        or a
        jr nz,clcm161
        ret

;### CLCD16 -> Dividiert zwei Werte (16bit)
;### Eingabe    BC=Wert1, DE=Wert2
;### Ausgabe    HL=Wert1/Wert2, DE=Wert1 MOD Wert2
;### Veraendert AF,BC,DE
clcd16  ld a,e
        or d
        ld hl,0
        ret z
        ld a,b
        ld b,16
clcd161 rl c
        rla
        rl l
        rl h
        sbc hl,de
        jr nc,clcd162
        add hl,de
clcd162 ccf
        djnz clcd161
        ex de,hl
        rl c
        rla
        ld h,a
        ld l,c
        ret

;### CLCDEZ -> Rechnet Byte in zwei Dezimalziffern um
;### Eingabe    A=Wert
;### Ausgabe    L=10er-Ascii-Ziffer, H=1er-Ascii-Ziffer
;### Veraendert AF
clcdez  ld l,0
clcdez1 sub 10
        jr c,clcdez2
        inc l
        jr clcdez1
clcdez2 add "0"+10
        ld h,a
        ld a,"0"
        add l
        ld l,a
        ret

;### CLCR16 -> Wandelt String in 16Bit Zahl um
;### Eingabe    IX=String, A=Terminator, BC=Untergrenze (>=0), DE=Obergrenze (<=65534)
;### Ausgabe    IX=String hinter Terminator, HL=Zahl, CF=1 -> Ungültiges Format (zu groß/klein, falsches Zeichen/Terminator)
;### Veraendert AF,DE,IYL
clcr16  ld hl,0
        db #fd:ld l,a
clcr161 ld a,(ix+0)
        inc ix
        db #fd:cp l
        jr z,clcr163
        sub "0"
        jr c,clcr162
        cp 10
        jr nc,clcr162
        push af
        push de
        ld a,10
        ex de,hl
        call clcm16
        pop de
        pop af
        add l
        ld l,a
        ld a,0
        adc h
        ld h,a
        jr clcr161
clcr162 scf
        ret
clcr163 sbc hl,bc
        ret c
        add hl,bc
        inc de
        sbc hl,de
        jr nc,clcr162
        add hl,de
        or a
        ret

;### CLCN32 -> Wandelt 32Bit-Zahl in ASCII-String um (mit 0 abgeschlossen)
;### Eingabe    DE,IX=Wert, IY=Adresse
;### Ausgabe    IY=Adresse letztes Zeichen
;### Veraendert AF,BC,DE,HL,IX,IY
clcn32t dw 1,0,     10,0,     100,0,     1000,0,     10000,0
        dw #86a0,1, #4240,#f, #9680,#98, #e100,#5f5, #ca00,#3b9a
clcn32z ds 4

clcn32  ld (clcn32z),ix
        ld (clcn32z+2),de
        ld ix,clcn32t+36
        ld b,9
        ld c,0
clcn321 ld a,"0"
        or a
clcn322 ld e,(ix+0):ld d,(ix+1):ld hl,(clcn32z):  sbc hl,de:ld (clcn32z),hl
        ld e,(ix+2):ld d,(ix+3):ld hl,(clcn32z+2):sbc hl,de:ld (clcn32z+2),hl
        jr c,clcn325
        inc c
        inc a
        jr clcn322
clcn325 ld e,(ix+0):ld d,(ix+1):ld hl,(clcn32z):  add hl,de:ld (clcn32z),hl
        ld e,(ix+2):ld d,(ix+3):ld hl,(clcn32z+2):adc hl,de:ld (clcn32z+2),hl
        ld de,-4
        add ix,de
        inc c
        dec c
        jr z,clcn323
        ld (iy+0),a
        inc iy
clcn323 djnz clcn321
        ld a,(clcn32z)
        add "0"
        ld (iy+0),a
        ld (iy+1),0
        ret

SySystem_HLPFLG db 0    ;flag, if HLP-path is valid
SySystem_HLPPTH db "%help.exe "
SySystem_HLPPTH1 ds 128
SySHInX db ".HLP",0

SySystem_HLPINI
        ld hl,(prgcodbeg)
        ld de,prgcodbeg
        dec h
        add hl,de                   ;HL = CodeEnd = Command line
        ld de,SySystem_HLPPTH1
        ld bc,0
        db #dd:ld l,128
SySHIn1 ld a,(hl)
        or a
        jr z,SySHIn3
        cp " "
        jr z,SySHIn3
        cp "."
        jr nz,SySHIn2
        ld c,e
        ld b,d
SySHIn2 ld (de),a
        inc hl
        inc de
        db #dd:dec l
        ret z
        jr SySHIn1
SySHIn3 ld a,c
        or b
        ret z
        ld e,c
        ld d,b
        ld hl,SySHInX
        ld bc,5
        ldir
        ld a,1
        ld (SySystem_HLPFLG),a
        ret

hlpopn  ld a,(SySystem_HLPFLG)
        or a
        jp z,prgprz0
        ld a,(prgbnknum)
        ld d,a
        ld a,PRC_ID_SYSTEM
        ld c,MSC_SYS_PRGRUN
        ld hl,SySystem_HLPPTH
        ld b,l
        ld e,h
        call msgsnd1
        jp prgprz0


;==============================================================================
;### DATEN-TEIL ###############################################################
;==============================================================================

prgdatbeg

prgicn16c db 12,24,24:dw $+7:dw $+4,12*24:db 5
db #88,#88,#88,#88,#88,#18,#81,#88,#88,#88,#88,#88,#88,#88,#88,#88,#11,#11,#11,#11,#88,#88,#88,#88,#88,#11,#88,#11,#00,#00,#00,#CC,#11,#88,#11,#88,#81,#1F,#11,#00,#00,#C0,#0C,#00,#CC,#11,#F1,#18
db #81,#11,#10,#0C,#CC,#D0,#CC,#DC,#CC,#11,#11,#18,#88,#11,#00,#C0,#0D,#CC,#CC,#CC,#DC,#C1,#11,#88,#88,#10,#00,#0C,#CC,#CC,#CC,#CC,#DD,#C1,#11,#88,#88,#10,#CC,#CC,#CD,#DC,#CD,#DC,#1D,#C1,#11,#88
db #81,#00,#CD,#CC,#DD,#DD,#DD,#DD,#CD,#1D,#11,#18,#81,#0C,#00,#CD,#CC,#11,#1D,#DD,#DD,#DD,#11,#18,#11,#CC,#0D,#CD,#D1,#00,#D1,#DD,#DD,#1D,#11,#11,#81,#00,#DC,#DD,#D1,#00,#D1,#1D,#1D,#D1,#11,#1D
db #11,#C0,#0C,#CD,#D1,#DD,#11,#11,#D1,#D1,#11,#11,#11,#C0,#CC,#CD,#DD,#11,#11,#11,#DD,#1D,#11,#11,#81,#1C,#CC,#DD,#DD,#D1,#11,#11,#1D,#11,#11,#18,#81,#1D,#CC,#D1,#D1,#D1,#11,#11,#D1,#D1,#11,#18
db #88,#11,#D1,#DD,#1D,#1D,#D1,#1D,#11,#11,#11,#88,#88,#11,#1D,#11,#D1,#D1,#DD,#11,#D1,#11,#11,#88,#88,#81,#11,#1D,#11,#D1,#11,#11,#11,#11,#18,#88,#88,#81,#11,#11,#11,#11,#11,#11,#11,#11,#18,#88
db #88,#1F,#11,#11,#11,#11,#11,#11,#11,#11,#F1,#88,#88,#11,#11,#11,#11,#11,#11,#11,#11,#11,#11,#88,#88,#81,#18,#88,#11,#11,#11,#11,#88,#81,#18,#88,#88,#88,#88,#88,#88,#11,#D1,#88,#88,#88,#88,#88

;### Verschiedenes
prgmsginf1 db "MineSweeper",0
prgmsginf2 db " Version 1.2 (Build 060409pdt)",0
prgmsginf3 db " Copyright <c> 2006 SymbiosiS",0

prgwintit   db "MineSweeper",0
configtit   db "User defined",0

configtxt1  db "Height:",0
configtxt2  db "Width:",0
configtxt3  db "Mines:",0

higscotit   db "The fastest MineSweepers",0
higscotxt1  db "Beginner:",0
higscotxt2  db "Advanced:",0
higscotxt3  db "### sec.",0
higscotxt4  db "### sec.",0
higscotxt5  db "Delete",0
higscotxt6  db " sec.",0

higscotxt7  db "You have the fastest time",0
higscotxt8a db "for the beginner level.",0
higscotxt8b db "for the advanced level.",0
higscotxt9  db "Please enter your name.",0

prgtxtok    db "Ok",0
prgtxtcnc   db "Cancel",0
prgtxtyes   db "Yes",0
prgtxtno    db "No",0

;### Menues
prgwinmentx1 db "Game",0
prgwinmen1tx1 db "New",0
prgwinmen1tx2 db "Beginner",0
prgwinmen1tx3 db "Advanced",0
prgwinmen1tx4 db "User defined...",0
prgwinmen1tx5 db "Highscore...",0
prgwinmen1tx6 db "Quit",0

prgwinmentx2 db "?",0
prgwinmen2tx1 db "Index",0
prgwinmen2tx2 db "About MineSweeper...",0

;### Anzeige
anztxt  db "000",0
timtxt  db "000",0

;### Symbole
sprfldunk   db 2,8,8,#00,#11,#00,#33,#03,#3F,#03,#3F,#03,#3F,#03,#3F,#33,#FF,#77,#FF    ;-
sprfldque   db 2,8,8,#00,#11,#30,#B3,#61,#F3,#03,#F3,#12,#B7,#03,#3F,#32,#F7,#77,#FF    ;?
sprfldmrk   db 2,8,8,#00,#11,#10,#B3,#30,#B7,#70,#B7,#30,#B7,#12,#B7,#70,#F1,#77,#FF    ;X
sprfldbo1   db 2,8,8,#1E,#1F,#3C,#97,#58,#D3,#F0,#F1,#78,#D3,#3C,#97,#1E,#1F,#FF,#FF    ;Bombe
sprfldbo0   db 2,8,8,#16,#1D,#38,#93,#58,#53,#E0,#F1,#58,#53,#38,#93,#16,#1D,#FF,#FF    ;falsche Bombenmarkierung
sprfldbox   db 2,8,8,#FE,#FF,#FC,#F7,#D8,#F3,#F0,#F1,#F8,#F3,#FC,#F7,#FE,#FF,#FF,#FF    ;draufgetretene Bombe

sprfldemp   db 2,8,8,#0F,#1F,#0F,#1F,#0F,#1F,#0F,#1F,#0F,#1F,#0F,#1F,#0F,#1F,#FF,#FF    ;0
sprfldnm1   db 2,8,8,#0F,#1F,#1E,#1F,#3C,#1F,#1E,#1F,#1E,#1F,#3C,#97,#0F,#1F,#FF,#FF    ;1
sprfldnm2   db 2,8,8,#0F,#1F,#1E,#97,#2D,#5B,#0F,#97,#1E,#1F,#3C,#D3,#0F,#1F,#FF,#FF    ;2
sprfldnm3   db 2,8,8,#0F,#1F,#1E,#97,#2D,#5B,#0F,#97,#2D,#5B,#1E,#97,#0F,#1F,#FF,#FF    ;3
sprfldnm4   db 2,8,8,#0F,#1F,#2D,#1F,#2D,#5B,#3C,#D3,#0F,#5B,#0F,#5B,#0F,#1F,#FF,#FF    ;4
sprfldnm5   db 2,8,8,#0F,#1F,#3C,#D3,#2D,#1F,#1E,#97,#0F,#5B,#3C,#97,#0F,#1F,#FF,#FF    ;5
sprfldnm6   db 2,8,8,#0F,#1F,#1E,#D3,#2D,#1F,#3C,#97,#2D,#5B,#1E,#97,#0F,#1F,#FF,#FF    ;6
sprfldnm7   db 2,8,8,#0F,#1F,#3C,#D3,#0F,#5B,#0F,#97,#1E,#1F,#1E,#1F,#0F,#1F,#FF,#FF    ;7
sprfldnm8   db 2,8,8,#0F,#1F,#1E,#97,#2D,#5B,#1E,#97,#2D,#5B,#1E,#97,#0F,#1F,#FF,#FF    ;8

;Frohes    Smiley
sprsmihap   db 4,16,16,#F0,#F0,#F0,#F0,#80,#00,#00,#32,#83,#0F,#0F,#3E,#83,#78,#E1,#3E,#83,#80,#10,#3E,#92,#00,#00,#B6,#92,#20,#40,#B6,#92,#20,#40,#B6
            db         #92,#00,#00,#B6,#92,#40,#20,#B6,#92,#30,#C0,#B6,#83,#80,#10,#3E,#83,#78,#E1,#3E,#83,#0F,#0F,#3E,#B3,#FF,#FF,#FE,#F0,#F0,#F0,#F0
;Trauriges Smiley
sprsmisad   db 4,16,16,#F0,#F0,#F0,#F0,#80,#00,#00,#32,#83,#0F,#0F,#3E,#83,#78,#E1,#3E,#83,#80,#10,#3E,#92,#20,#40,#B6,#92,#40,#20,#B6,#92,#00,#00,#B6
            db         #92,#30,#C0,#B6,#92,#40,#20,#B6,#92,#00,#00,#B6,#83,#80,#10,#3E,#83,#78,#E1,#3E,#83,#0F,#0F,#3E,#B3,#FF,#FF,#FE,#F0,#F0,#F0,#F0
;Sieger    Smiley
sprsmiwin   db 4,16,16,#F0,#F0,#F0,#F0,#80,#00,#00,#32,#83,#0F,#0F,#3E,#83,#78,#E1,#3E,#83,#80,#10,#3E,#92,#70,#E0,#B6,#92,#60,#60,#B6,#92,#80,#10,#B6
            db         #92,#00,#00,#B6,#92,#40,#20,#B6,#92,#30,#C0,#B6,#83,#80,#10,#3E,#83,#78,#E1,#3E,#83,#0F,#0F,#3E,#B3,#FF,#FF,#FE,#F0,#F0,#F0,#F0
;Banges    Smiley
sprsmiafr   db 4,16,16,#F0,#F0,#F0,#F0,#80,#00,#00,#32,#83,#0F,#0F,#3E,#83,#78,#E1,#3E,#83,#80,#10,#3E,#92,#60,#60,#B6,#92,#60,#60,#B6,#92,#10,#80,#B6
            db         #92,#20,#40,#B6,#92,#20,#40,#B6,#92,#10,#80,#B6,#83,#80,#10,#3E,#83,#78,#E1,#3E,#83,#0F,#0F,#3E,#B3,#FF,#FF,#FE,#F0,#F0,#F0,#F0


;==============================================================================
;### TRANSFER-TEIL ############################################################
;==============================================================================

prgtrnbeg
;### PRGPRZS -> Stack für Programm-Prozess
        ds 128
prgstk  ds 6*2
        dw prgprz
prgprzn db 0
prgmsgb ds 14

;### INFO-FENSTER #############################################################

prgmsginf  dw prgmsginf1,4*1+2,prgmsginf2,4*1+2,prgmsginf3,4*1+2,prgicnbig

;### CONFIG FENSTER ###########################################################

configwin   dw #1401,4+16,079,035,115,054,0,0,115,054,115,054,115,054,0,configtit,0,0,configgrp,0,0:ds 136+14
configgrp   db 9,0:dw configdat,0,0,256*9+8,0,0,03
configdat
dw      00,         0,2,          0,0,1000,1000,0       ;00=Hintergrund
dw      00,255*256+ 1,configdsc1, 08, 09, 30, 8,0       ;01=Beschreibung "Height"
dw      00,255*256+32,configinp1, 38, 07, 22,12,0       ;02=Eingabe "Height"
dw      00,255*256+ 1,configdsc2, 08, 23, 30, 8,0       ;03=Beschreibung "Width"
dw      00,255*256+32,configinp2, 38, 21, 22,12,0       ;04=Eingabe "Width"
dw      00,255*256+ 1,configdsc3, 08, 37, 30, 8,0       ;05=Beschreibung "Mines"
dw      00,255*256+32,configinp3, 38, 35, 22,12,0       ;06=Eingabe "Filecopy buffer"
dw cfgset ,255*256+16,prgtxtok,   67, 07, 40,12,0       ;07="Ok"    -Button
dw diacnc ,255*256+16,prgtxtcnc,  67, 35, 40,12,0       ;08="Cancel"-Button

configdsc1  dw configtxt1,2+4
configdsc2  dw configtxt2,2+4
configdsc3  dw configtxt3,2+4

configinp1  dw configbuf1,0,0,0,0,2,0
configbuf1  ds 3
configinp2  dw configbuf2,0,0,0,0,2,0
configbuf2  ds 3
configinp3  dw configbuf3,0,0,0,0,2,0
configbuf3  ds 3

;### HIGHSCORE FENSTER ########################################################

higscowin   dw #1401,4+16,079,035,160,060,0,0,160,060,160,060,160,060,0,higscotit,0,0,higscogrp,0,0:ds 136+14
higscogrp   db 9,0:dw higscodat,0,0,256*9+9,0,0,09
higscodat
dw      00,         0,2,          0,0,1000,1000,0       ;00=Hintergrund
dw      00,255*256+ 1,higscodsc1, 08, 08, 46, 8,0       ;01=Beschreibung "Beginner"
dw      00,255*256+ 1,higscodsc2, 08, 22, 46, 8,0       ;02=Beschreibung "Advanced"
dw      00,255*256+ 1,higscodsc3, 54, 08, 38, 8,0       ;03=Beschreibung "Zeit Beg"
dw      00,255*256+ 1,higscodsc4, 54, 22, 38, 8,0       ;04=Beschreibung "Zeit Adv"
dw      00,255*256+ 1,higscodsc5, 96, 08, 56, 8,0       ;05=Beschreibung "Name Beg"
dw      00,255*256+ 1,higscodsc6, 96, 22, 56, 8,0       ;06=Beschreibung "Name Adv"
dw higclr ,255*256+16,higscotxt5, 20, 40, 56,12,0       ;07="Löschen"-Button
dw diacnc ,255*256+16,prgtxtok,   84, 40, 56,12,0       ;08="Ok"     -Button

higscodsc1  dw higscotxt1,2+4
higscodsc2  dw higscotxt2,2+4
higscodsc3  dw higscotxt3,2+4
higscodsc4  dw higscotxt4,2+4
higscodsc5  dw higscobuf1,2+4
higscodsc6  dw higscobuf2,2+4

higscobeg
higscoinp1  dw higscobuf1,0,9,0,9,13,0
higscobuf1  db "Prodatron":ds 5
higscotim1  dw 999
higscoinp2  dw higscobuf2,0,9,0,9,13,0
higscobuf2  db "Prodatron":ds 5
higscotim2  dw 999
higscoend

;### HIGHSCORE-EINGABE FENSTER ################################################

higentwin   dw #0001,4+16,079,035,116,078,0,0,116,078,116,078,116,078,0,0,0,0,higentgrp,0,0:ds 136+14
higentgrp   db 6,0:dw higentdat,0,0,256*6+6,0,0,06
higentdat
dw      00,         0,2,          0,0,1000,1000,0       ;00=Hintergrund
dw      00,255*256+ 1,higscodsc7, 08, 08,100, 8,0       ;01=Beschreibung1
dw      00,255*256+ 1,higscodsc8, 08, 18,100, 8,0       ;02=Beschreibung2
dw      00,255*256+ 1,higscodsc9, 08, 28,100, 8,0       ;03=Beschreibung3
dw      00,255*256+32
higentdat2         dw higscoinp1, 23, 40, 70,12,0       ;04=Eingabe "Name"
dw hignew ,255*256+16,prgtxtok,   33, 58, 50,12,0       ;05="Ok"-Button

higscodsc7  dw higscotxt7, 2+4+512
higscodsc8  dw higscotxt8a,2+4+512
higscodsc9  dw higscotxt9, 2+4+512

;### HAUPT-FENSTER ############################################################

prgwindat dw #3501,0,67,16,80,100,0,0,80,100,1,1,1000,1000,prgicnsml,prgwintit,0,prgwinmen,prgwingrp,0,0:ds 136+14

prgwinmen  dw 2, 1+4,prgwinmentx1,prgwinmen1,0, 1+4,prgwinmentx2,prgwinmen2,0
prgwinmen1 dw 9, 1,prgwinmen1tx1,gamnew,0,  1+8,#0000,0,0, 1+2,prgwinmen1tx2,gamtyp1,0,  1,prgwinmen1tx3,gamtyp2,0,  1,prgwinmen1tx4,cfgopn,0,  1+8,#0000,0,0, 1,prgwinmen1tx5,higshw,0,  1+8,#0000,0,0, 1,prgwinmen1tx6,prgend,0
prgwinmen2 dw 3, 1,prgwinmen2tx1,hlpopn,0, 1+8,0,0,0, 1,prgwinmen2tx2,prginf,0

anzdsc  dw anztxt,1+0+128+512
timdsc  dw timtxt,1+0+128+512

fldofs  equ 9

prgwingrp db fldofs,0:dw prgwinobj,0,0,0,0,0,0
prgwinobj
dw     00,255*256+00,2         ,0,0,1000,1000,0     ;00=Hintergrund
dw     00,255*256+02,1+4       ,3,23,74,74,0        ;01=Rahmen
dw gamnew,255*256+08
prgwinobj0        dw sprsmihap ,32,4,16,16,0        ;02=Smiley
dw     00,255*256+00,1         , 4,4,24,16,0        ;03=Anzahl Hintergrund
dw     00,255*256+01,anzdsc    , 9,8,15, 8,0        ;04=Anzahl Anzeige
dw     00,255*256+02,3+8       , 5,5,22,14,0        ;05=Anzahl Rahmen
dw     00,255*256+00,1         ,52,4,24,16,0        ;06=Zeit Hintergrund
dw     00,255*256+01,timdsc    ,57,8,15, 8,0        ;07=Zeit Anzeige
dw     00,255*256+02,3+8       ,53,5,22,14,0        ;08=Zeit Rahmen
prgwinobj1  ;*** muss letzter Label sein, siehe prgdattex! (16*15*15 Bytes)

prgtrnend

relocate_table
relocate_end
