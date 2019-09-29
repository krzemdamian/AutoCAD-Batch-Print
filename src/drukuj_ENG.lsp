(vl-load-com)


(defun CZEKAJ ()
  (progn
    ;----------
    (getstring "\nNacisnij ENTER")
    (princ)
    ;----------
  );progn
);CZEKAJ


		(defun c:test (/ olddim a1)
		  (setq olddim (getvar "dimzin"))
		  (setvar "dimzin" 0)
		  (setq a1 (rtos 123.321234 2))
		  (alert a1)
		  (setvar "dimzin" olddim)
		)	


;================================================================================
;                             Lista stylów wydruku                              ;
;================================================================================
(defun ActLay ()
  (vla-get-ActiveLayout
    (vla-get-activedocument
      (vlax-get-acad-object)
    )
  )
)		
		
(defun PlotStyleTableNamesList ()
  (vla-RefreshPlotDeviceInfo (ActLay))
  (vlax-safearray->list
    (vlax-variant-value
      (vla-GetPlotStyleTableNames
        (ActLay)
      )
    )
  )
)

; =========================================================================================== ;
;; Set Attribute Value  -  Lee Mac
;; Sets the value of the first attribute with the given tag found within the block, if present.
;; blk - [ent] Block (Insert) Entity Name
;; tag - [str] Attribute TagString
;; val - [str] Attribute Value
;; Returns: [str] Attribute value if successful, else nil.

(defun LM:setattributevalue ( blk tag val / enx )
    (if (= "ATTRIB" (cdr (assoc 0 (setq enx (entget (setq blk (entnext blk)))))))
        (if (= (strcase tag) (strcase (cdr (assoc 2 enx))))
            (if (entmod (subst (cons 1 val) (assoc 1 enx) enx))
                (progn
                    (entupd blk)
                    val
                )
            )
            (LM:setattributevalue blk tag val)
        )
    )
)


; =========================================================================================== ;
;; Get Attribute Value  -  Lee Mac
;; Returns the value held by the specified tag within the supplied block, if present.
;; blk - [ent] Block (Insert) Entity Name
;; tag - [str] Attribute TagString
;; Returns: [str] Attribute value, else nil if tag is not found.
; =========================================================================================== ;
(defun LM:getattributevalue ( blk tag / val enx )
    (while
        (and
            (null val)
            (= "ATTRIB" (cdr (assoc 0 (setq enx (entget (setq blk (entnext blk)))))))
        )
        (if (= (strcase tag) (strcase (cdr (assoc 2 enx))))
            (setq val (cdr (assoc 1 enx)))
        )
    )
)


; =========================================================================================== ;
;                             Pobranie listy drukarek                                         ;
; =========================================================================================== ;
(defun GetPlotDevices (ad)
(vla-RefreshPlotDeviceInfo
(vla-get-activelayout
ad))
(vlax-safearray->list
(vlax-variant-value
(vla-getplotdevicenames
(vla-item (vla-get-layouts ad) "Model"))))
)


; =========================================================================================== ;
; Laczy liste lancuchow w lancuch z separatorem /                                             ;
; Combines a list of strings in the string with the separator                                 ;
;  Lst [LIST] - lista lancuchow / list of strings                                             ;
;  Sep [STR]  - separator / separator                                                         ;
; ------------------------------------------------------------------------------------------- ;
; (cd:STR_ReParse '("OLE2FRAME" "IMAGE" "HATCH") ",")                                         ;
; =========================================================================================== ;
(defun cd:STR_ReParse (Lst Sep / res)
  (setq res (car Lst))
  (foreach % (cdr Lst)
    (setq res (strcat res Sep %))
  )
  res
)

; =========================================================================================== ;
; Pobranie slowa kluczowego od uzytkownika / Get a keyword from the user                      ;
;  Msg  [STR]  - tekst zapytania / query text                                                 ;
;  Keys [LIST] - lista mozliwych slow kluczowych / list of possible keywords                  ;
;  Def  [STR]  - domyslne slowo kluczowe / default keyword                                    ;
; ------------------------------------------------------------------------------------------- ;
; (cd:USR_GetKeyWord "\nUtworz blok" '("Anonimowy" "Nazwa") "Nazwa")                          ;
; =========================================================================================== ;
(defun cd:USR_GetKeyWord (Msg Keys Def / res key)
  (setq key
    (mapcar
      (function
        (lambda (%)
          (cd:STR_ReParse Keys %)
        )
      )
      (list " " "/")
    )
  )
  (initget (car key))
  (setq res
    (vl-catch-all-apply
      (quote getkword)
      (list
        (strcat
          Msg " [" (cadr key) "] <"
          (setq Def
            (if (not (member Def Keys))
              (car Keys)
              Def
            )
          )
          ">: "
        )
      )
    )
  )
  (if res
    (if (= (type res) (quote STR)) res)
    Def
  )
)

; =========================================================================================== ;
; Okno dialogowe z lista "list_box" / Dialog control with list "list_box"                     ;
;  Data      [LIST]     - lista do wyswietlenia / list to display                             ;
;  Pos       [INT]      - pozycja poczatkowa na liscie / select list position                 ;
;  Title     [STR/nil]  - tytul okna / window title                                           ;
;  ListTitle [STR/nil]  - tytul list_box / list_box title                                     ;
;  Width     [INT]      - szerokosc / width                                                   ;
;  Height    [INT]      - wysokosc / height                                                   ;
;  Btns      [0/1/2]    - [cancel/ok/ok_cancel] przyciski / buttons                           ;
;  BtnsWidth [REAL/INT] - szerokosc przyciskow / buttons width                                ;
;  BtnsLabel [LIST]     - etykiety przyciskow / buttons label                                 ;
;  MSelect   [T/nil]    - dopuszczenie multiple_select / allow multiple select                ;
;  DPos      [T/nil]    - zapamietanie pozycji okna / save window position                    ;
;  DblClick  [T/nil]    - podwojny klik (wykluczone Cancel) / double click (not for Cancel)   ;
;  Func      [SUBR]     - funkcja do obslugi wybranej pozycji na liscie /                     ;
;                         function to operate selected position on the list                   ;
; ------------------------------------------------------------------------------------------- ;
; Zwraca / Return:                                                                            ;
;  nil  = nic nie wybrano (anulowano) / nothing was selected (canceled)                       ;
;  INT  = wybrano jedna pozycje / one position selected  | MSelect = nil                      ;
;  LIST = wybrano kilka pozycji / few positions selected | MSelect = T                        ;
; ------------------------------------------------------------------------------------------- ;
; (cd:DCL_StdListDialog                                                                       ;
;   (setq lst (mapcar 'car (cd:DWG_LayoutsList))) (vl-position (getvar "ctab") lst)           ;
;   "List of Layouts" "Select layout:" 40 15 2 13 (list "&Ok" "&Cancel")                      ;
;   nil T T '(setvar "ctab" (nth (atoi res) lst)))                                            ;
; =========================================================================================== ;
(defun cd:DCL_StdListDialog (Data Pos Title ListTitle Width Height
                             Btns BtnsWidth BtnsLabel MSelect DPos DblClick Func
                             / _Sub _Value2List _SetControls fd ok ca tmp dc res)
  (defun _Sub (Val)
    (if (and Func Data) (eval Func))
    (_SetControls (setq res (_Value2List Val)))
  )
  (defun _Value2List (Val) (read (strcat "(" Val ")")))
  (defun _SetControls (Idx)
    (if (and Idx Data)
      (mode_tile (car BtnsLabel) 0)
      (mode_tile (car BtnsLabel) 1)
    )
  )
  (if (not DPos) (setq *cd-TempDlgPosition* (list -1 -1)))
  (cond
    ( (not
        (and
          (setq fd
            (open
              (setq tmp (vl-FileName-MkTemp nil nil ".dcl")) "w"
            )
          )
          (setq ok (strcat
                     ": but { label = \"" (car BtnsLabel) "\";"
                     "  key = \"" (car BtnsLabel) "\"; is_default = true;}"
                   )
                ca (strcat
                     ": but { label=\"" (cadr BtnsLabel) "\";"
                     "  key = \"" (cadr BtnsLabel) "\";is_cancel = true;}"
                   )
          )
          (foreach % 
            (list
              (strcat
                "but : button { width = " (if BtnsWidth (itoa BtnsWidth) 13)
                "; fixed_width = true; }"
                "StdListDialog: dialog {"
                (if Title (strcat "label = \"" Title "\";") "")
                ": list_box { key = \"list\";"
                (if ListTitle (strcat "label = \"" ListTitle "\";") "")
                "fixed_width = true; fixed_height = true;"
                "width = " (if Width (itoa Width) "20" ) ";"
                "height = " (if Height (itoa Height) "20" ) ";"
                "multiple_select = " (if MSelect "true;" "false;")
                "} : row { alignment = centered; fixed_width = true;"
              )
              (cond
                ( (zerop Btns) ca )
                ( (= 1 Btns) ok )
                ( T (strcat ok ca) )
              )
              "}}"
            )
            (write-line % fd)
          )
          (not (close fd))
          (< 0 (setq dc (load_dialog tmp)))
          (new_dialog "StdListDialog" dc ""
            (cond
              ( *cd-TempDlgPosition* )
              ( (quote (-1 -1)) )
            )
          )
        )
      )
    )
    ( T     
      (start_list "list")
      (mapcar (quote add_list) Data)
      (end_list)
      (if
        (or
          (not Pos)
          (not (< -1 Pos (length Data)))
        )
        (setq Pos 0)
      )
      (setq res (set_tile "list" (itoa Pos)))
      (_Sub res)
      (action_tile "list"
        (vl-prin1-to-string
          (quote
            (progn
              (setq res $value)
              (_Sub res)
              (if
                (and
                  DblClick
                  (not (zerop Btns))
                )
                (if (= $reason 4)
                  (setq *cd-TempDlgPosition* (done_dialog 1))
                )
              )
            )
          )
        )
      )
      (action_tile (car BtnsLabel) "(setq *cd-TempDlgPosition* (done_dialog 1))")
      (action_tile (cadr BtnsLabel) "(setq res nil) (done_dialog 0)")
      (start_dialog)
    )
  )
  (if (< 0 dc) (unload_dialog dc))
  (if (setq tmp (findfile tmp)) (vl-file-delete tmp))
  (if (not DPos) (setq *cd-TempDlgPosition* (list -1 -1)))
  (if res (if (= 1 (length res)) (car res) res))
)

; =========================================================================================== ;
; Czyta plik tekstowy / Read a text file                                                      ;
;  Line [INT/nil] - INT = numer linii pliku / file line number                                ;
;                   nil = caly plik / all lines of file                                       ;
;  File [STR]     - nazwa pliku (krotka lub ze sciezka) / short or full path file name        ;
; ------------------------------------------------------------------------------------------- ;
; Zwraca / Return:                                                                            ;
;   nil = gdy Line = INT wieksze niz ilosc linii w pliku lub plik jest pusty /                ;
;         when Line = INT is greater then number of lines in file or file is empty            ;
;     0 = brak dostepu do pliku / no access to file                                           ;
;    -1 = nie znaleziono pliku / file not found                                               ;
;   STR = gdy Line = INT / when Line = INT                                                    ;
;  LIST = gdy Line = nil / when Line = nil                                                    ;
; ------------------------------------------------------------------------------------------- ;
; (cd:SYS_ReadFile nil "data.ini"), (cd:SYS_ReadFile 10 "acad.lin")                           ;
; =========================================================================================== ;
(defun cd:SYS_ReadFile (Line File / fn fd l res)
  (if (setq fn (findfile File))
    (if (setq fd (open fn "r"))
      (progn
        (if Line
          (progn
            (repeat Line (read-line fd))
            (setq res (read-line fd))
          )
          (progn
            (setq l T)
            (while l
              (setq res
                (cons
                  (setq l (read-line fd))
                  res
                )
              )
            )
            (setq res (reverse (cdr res)))
          )
        )
        (close fd)
      )
      (setq res 0)
    )
    (setq res -1)
  )
  res
)


; ============================================================================================ ;
; ******************************************************************************************** ;
;                                                                                              ;
;                                                                                              ;
;                                   POSZCZEGÓLNE FUNKCJE PROGRAMU                              ;
;                                                                                              ;
;                                                                                              ;
; ******************************************************************************************** ;
; ============================================================================================ ;


;==============================================================================================
;=                              FUNKCJA TWORZ¥CA BLOKI WYDRUKU Z PLIKU                        =
;==============================================================================================


(defun c:de ( / nazwa_bloku )
(progn

  ;--------------------------------------modó³ szczytywania lini tekstu
  (setq nazwa_pliku (getfiled "Wybierz plik do otworzenia" ""  "dat" 2))
  (setq nr_lini_start 11)
  (progn
    ;----------
    (setq file_id (open nazwa_pliku "r"))
    (if (/= file_id nil)
      (progn
        ;----------
		(setq licznik 1)
		(setq nazwy_papieru (list))
        (while (/= (setq linia (read-line file_id)) nil)
          (progn
            ;----------
			(if (>= licznik nr_lini_start)
			(progn
			    ;============== 11, 14, ...
				(if (= (rem (+ licznik 1) 3) 0)
				(progn
				;(princ "A ")
				;(write-line linia)  
				(setq nazwa_bloku linia)
				
				;-----tworzenie listy papierów
				(setq nazwy_papieru
					(append
						nazwy_papieru
						(list nazwa_bloku)
					)
					
				);setq			
				);progn
				);if			
			
				;============== 12, 15, ...
				(if (= (rem licznik 3) 0)
				(progn
				;(princ "B ")
				;(write-line linia)  
				(setq szerokosc (atof linia))
				
				);progn
				);if
				

				;============== 13, 16, ...
				(if (= (rem (- licznik 1) 3) 0)
				(progn
				;(princ "C ")
				;(write-line linia) 
				(setq wysokosc (atof linia))
				
				

					;=======================  po pobraniu wszystkich danych przystepuje do definicji bloku
					   ;--------------------------------------modó³ tworzenia bloku
						(if (not (tblsearch "block" nazwa_bloku))
						(progn
							(command 
								"-bedit"  nazwa_bloku
							)
							(command
							"_rectang" (list 0 0) (list (- 0 szerokosc ) wysokosc)
							)
							(command
							"-attdef" "_I" "" "nazwa_rysunku" "Nazwa rysunku" "" (list 0 0) "" ""
							)	
							(command
							"-attdef" "_I" "" "papier" "Nazwa papieru" "" (list 0 0) "" ""
							)	
							(command
							"-attdef" "_I" "" "szer" "Szerokosc" szerokosc (list 0 0) "" ""
							)			
							(command
							"-attdef" "_I" "" "wys" "Wysokosc" wysokosc (list 0 0) "" ""
							)								
							(command
							"-attdef" "_I" "" "skala_od" "Skala OD:# (podaj OD)" "" (list 0 0) "" ""
							)								
							(command
							"-attdef" "_I" "" "skala_do" "Skala #:DO (podaj DO)" "" (list 0 0) "" ""
							)								
							
				
							(command
							"_bsave"
							)							
							(command
							"_bclose"
							)
							;---ustawienie nierozbijalnosci
							(setq BLOCKS
							  (vla-get-Blocks
								(vla-get-activedocument
								  (vlax-get-acad-object)
								)
							  )
							  BLK (vla-Item BLOCKS nazwa_bloku)
							)
							(vla-put-explodable (vla-Item BLOCKS nazwa_bloku) :vlax-false)							
							
						);progn
						);if
						;---------------KONIEC-----------------------modó³ tworzenia bloku

					
					
				);progn
				);if

				
				;(write-line linia)  
			);progn
			);if
            ;----------
			
			(setq licznik (+ licznik 1))
          );progn
        );while
		
        ;----------
        (close file_id)
        ;----------
		

		;-------------- funkcja wypisuj¹ca nazwy papierów do pliku DRUK/papiery.dat
		(progn

		(setq file 
			(strcat 
				(getvar 'dwgprefix)
				"DRUK\\"
				;|
				;----tutaj jest pomys³ na wybór drukarki
				(nth 
					(cd:DCL_StdListDialog                                                                       
						  (GetPlotDevices (vla-get-activedocument (vlax-get-acad-object)))
						  (vl-position (getvar "ctab") lst)           
						  "Lista papierów" "Wybierz papier:" 40 15 1 13 (list "&Ok" "&Cancel")                      
						  nil T T "");cd:DCL_StdListDialog  
						  
					(GetPlotDevices (vla-get-activedocument (vlax-get-acad-object)))
				);nth				
				|;
				"papiery.dat"
			)
		)
			(setq open_p (open file "w"))
			(foreach nazwa_papieru nazwy_papieru
				(write-line nazwa_papieru open_p)
				;(princ nazwy_papieru)
			);foreach
			(close open_p)
			(princ)
		);progn

		

		
      );progn
    );if
    ;----------
    (princ)
    ;----------
  );progn
  ;-------------KONIEC--------------------modó³ szczytywania lini tekstu

(princ)
);progn
);defun

;==============================================================================================
;=                              FUNKCJA WSTAWIAJ¥CA OBSZAR WYDRUKU                            =
;==============================================================================================




(defun C:dw ( / naz_pap wybrana_nazwa_papieru nazwa_rysunku
				skala_od skala_do olddim stara_warstwa)
(progn			
;---------------- modó³ sczytywania nazw papieru z pliku tymczasowego papiery.dat
(progn
	(setq naz_pap (cd:SYS_ReadFile nil (strcat (getvar 'dwgprefix)	"DRUK\\" "papiery.dat")))
	(setq wybrana_nazwa_papieru 
		(nth 
			(cd:DCL_StdListDialog                                                                       
				  naz_pap (vl-position (getvar "ctab") lst)           
				  "Lista papierów" "Wybierz papier:" 40 15 1 13 (list "&Ok" "&Cancel")                      
				  nil T T "");cd:DCL_StdListDialog  
				  
			naz_pap
		);nth
	);setq
  (princ)
);progn ----- wybór papieru

;----- pobranie nazwy rysunku i skali od u¿ytkownika
(setq nazwa_rysunku (getstring "Podaj nazwê rysunku" T))
(setq skala_od (float (getint "Podaj skalê \"OD\" OD\:\#")))
(setq skala_do (float (getint "Podaj skalê \"DO\" \#\:DO")))

;----- zamiana zmiennych systemowych
(setvar "attreq" 1)   ;zmienna systemowa: odpowiada za zapytanie czy pozostawienie wartoœci domyœlnych atrybutów
(setvar "attdia" 0)   ;zmienna systemowa: odpowiada za wyœwietlanie okna dialogowego z atrybutami
(setq olddim (getvar "dimzin"))  ;pobiera wartoœæ zmiennej systemowej
(setvar "dimzin" 0)  ;zmienna systemowa: Steruje pomijaniem zer w zapisie wartoœci jednostek podstawowych

;(princ (/ skala_do skala_od))
;------------------- modó³ zmieniaj¹cy warstwê
(setq stara_warstwa (getvar "CLAYER"))
	(if (not (tblsearch "layer" "DRUK"))
	(progn	
		(command
			"_layer"
			"_M"  ; tworzy i ustawia jako aktualn¹ warstê
			"DRUK"
			""
		)
	);progn
	(progn
		(command
			"_layer"
			"_set"
			"DRUK"
			""
		)
	);progn else
	);if
;------------------- modó³ wstawiania bloku
	(command 
		"_insert"  
		wybrana_nazwa_papieru   
		(getpoint "podaj prawy dolny róg obszaru")  ;punkt wstawienia
		"x" (/ skala_do skala_od)  ;skala x wstawienia
		""   ;skala y taka jak x
		""   ;skala z taka jak x
		""	 ;k¹t obrotu
		nazwa_rysunku   ;atrybut: nazwa rysunku
		wybrana_nazwa_papieru   ;atrybut: nazwa papieru
		""   ;atrybut: szerokoœæ - wartoœæ domyœlna
		""   ;atrybut: wysokoœæ - wartoœæ domyœlna
		skala_od ;atrybut: skala od
		skala_do ;atrybut skala do
	)
(setvar "attdia" 1)  ;zamiana zmiennej systemowej na wyœwietlanie okna atrybutów	

;----- modó³ do zamiany atrybutów szer i wys bloku
	(setq szer (float (atof (LM:getattributevalue (entlast) "szer"))))
	(princ "\n")
	(princ szer)
	(setq wys (float (atof (LM:getattributevalue (entlast) "wys"))))
	(princ "\n")
	;(princ wys)

	(setq szer (rtos (/ (* szer skala_do) skala_od) 2 10))
	(setq wys (rtos (/ (* wys skala_do) skala_od) 2 10))	
	(LM:setattributevalue (entlast) "szer" szer)
	(LM:setattributevalue (entlast) "wys" wys)
	
	
(setvar "dimzin" olddim)  ;zamiana zmiennej systemowej na wartoœæ domyœln¹ [8]	 

;--- ustawia spowrotem na star¹ warstwê
(command
	"_layer"
	"_set"  ; tworzy i ustawia jako aktualn¹ warstê
	stara_warstwa
	""
)
	(princ)
	);progn
);defun







;==============================================================================================
;=                                       FUNKCJA DRUKUJ¥CA                                    =
;==============================================================================================

(defun C:DR ( / zbior_wyboru dlugosc licznik
                ename lista_dxf blk
				operacja
            )
 (progn

(command "_UCS" "_World")

;xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx  TUTAJ POLEG£EM
;|
(princ "\n")
(princ (setq uklad2 (getvar "ctab")))
(princ "\n")
(princ (setq uklad (cons 410 uklad2)))

(princ (setq uklad (cons 410 (strcat "\"" uklad2 "\""))))
|;



;(setq uklad (getvar "ctab"))
(princ "\n")
(if (= (getvar "ctab") "Model") 
	(progn
	(setq operacja (cd:USR_GetKeyWord "Wszysyko, czy Zaznaczenie?" (list "Wszystko" "Zaznaczenie") "Wszystko"))
	(princ operacja) 
	);progn
	(setq operacja "Zaznaczenie") ;else
);if

(if (= operacja "Zaznaczenie")
	(progn 
		;=== wskazanie punktów obszaru wydruku
		(setq A (getpoint "Wska¿ PIERWSZY punkt prostok¹ta obszaru do druku"))
		(setq B (getpoint "Wska¿ DRUGI \(po przek¹tnej\) punkt prostok¹ta obszaru do druku"))
		
		;===wybór elementów
		(setq zbior_wyboru 
			(ssget 
			"_W" A B
			'(
			  (-4 . "<AND")
				(8 . "DRUK")  ; 8 -to wybór warstywy
				(0 . "INSERT")
			  (-4 . "AND>")
			);filtr wyboru
			);ssget
		);setq
		(princ)
	);progn
	(progn
		(setq zbior_wyboru 
			(ssget 
			"X"
			'(
			  (-4 . "<AND")
				(8 . "DRUK")  ; 8 -to wybór warstywy
				(0 . "INSERT")
				(410 . "Model")
			  (-4 . "AND>")
			);filtr wyboru
			);ssget
		);setq	
	);progn else
);if

 
 
 
    ;----------
    (if(= zbior_wyboru nil)
      (progn
        (princ
          "\nZbior wyboru nie zawiera elementow.\n"
        );princ
      );progn
    );if
    (if (= (type zbior_wyboru) 'PICKSET)
      (progn
        (if (> (sslength zbior_wyboru) 0)
          (progn
            ;----------
            ;(textscr)
            (princ "\n\n\n\n")
            (princ "Liczba elementow w zbiorze wyboru: ")
            (princ (sslength zbior_wyboru))
            (princ "\n")
            (setq
              dlugosc (sslength zbior_wyboru)
              licznik 0
            );setq
			
			
			
			(setq nazwa_drukarki "DRUKUJ.pc3")
			;|
			;==========================     wybór drukarki    ============ w fazie rozwoju
			(setq nazwa_drukarki 
				(nth 
					(cd:DCL_StdListDialog                                                                       
						  (GetPlotDevices (vla-get-activedocument (vlax-get-acad-object)))
						  0
						  "Lista drukarek" "Wybierz drukarkê:" 40 15 1 13 (list "&Ok" "&Cancel")                      
						  nil T T "");cd:DCL_StdListDialog  
						  
					(GetPlotDevices (vla-get-activedocument (vlax-get-acad-object)))
				);nth
			);setq
			|;
			
			
			;==========================     wybór stylu wydruku    ============			
			(setq nazwa_stylu 
				(nth 
					(cd:DCL_StdListDialog                                                                       
						  (PlotStyleTableNamesList)
						  0
						  "Lista stylów" "Wybierz styl:" 40 15 1 13 (list "&Ok" "&Cancel")                      
						  nil T T "");cd:DCL_StdListDialog  
						  
					(PlotStyleTableNamesList)
				);nth
			);setq
			
			
			
			
			
            (repeat dlugosc
              (progn
                ;----------
                (setq
                  ename (ssname zbior_wyboru licznik)
                  lista_dxf (entget ename)
                );setq
                (princ "\nLista DXF dla elementu numer ")
                (princ licznik)
                (princ ".\n")
                (princ lista_dxf)
                ;(CZEKAJ)
                (setq licznik (1+ licznik))



;========================   MODÓ£  WYZNACZANIA WIESZCHO£KÓW POLILINI   ===================
				
  (if (and (setq e ename)
       (= (cdr (assoc 0 (entget e))) "INSERT")
       (setq pts (mapcar 'cdr
                 (vl-remove-if-not
                   (function (lambda (pt) (= (car pt) 10)))
                   (entget e)
                 )
             )
       )
       (setq file (strcat (getvar 'dwgprefix)
                  (getvar 'dwgname)
				  "DRUK\\"
                  "_punkty_wstawienia_bloku.dat"
              )
       )
       (setq openf (open file "w"))
      )
    (progn
      (foreach pt pts
    (write-line (vl-prin1-to-string pt) openf)
      )
      (close openf)
    )
  )
	(princ "\n")			
	(princ pts)	   ;sprawdzenie czy drukuje		
	
	

;=======================  Modó³ szczytywania atrybutów bloku ============
(princ "\n")	
(princ (setq nazwa_rysunku (LM:getattributevalue ename "NAZWA_RYSUNKU"))) 
(setq papier (LM:getattributevalue ename "PAPIER"))
(setq szer (atof (LM:getattributevalue ename "SZER")))
(setq wys (atof (LM:getattributevalue ename "WYS")))
(setq skala_od (LM:getattributevalue ename "SKALA_OD"))
(setq skala_do (LM:getattributevalue ename "SKALA_DO"))
(setq filename 
	(strcat 
		(getvar 'dwgprefix)
		"DRUK\\"
		nazwa_rysunku
	)
)


;==================   FUNKCJA DRUKUJ¥CA ==============		

(princ "\n")
(princ (setq pt1 (list (nth 0 (nth 0 pts)) (nth 1 (nth 0 pts)))))
(princ "\n")
(princ (setq pt2 (list (- (nth 0 pt1) szer) (+ (nth 1 pt1) wys))))
(princ 
	(setq skala
		(strcat
			skala_od
			"\="
			skala_do
		)
	);setq
)
(princ "\n")
(princ (getvar "ctab"))
(princ "\n")
(if (= (getvar "ctab") "Model")	
(command "_plot" 
  "_Y"                       ;Detailed plot configuration? [Yes/No] <No>: Y
  (getvar "ctab")                    ;Enter a layout name or [?] <0_01a>:
  nazwa_drukarki          ;Enter an output device name or [?] <DWG To PDF.pc3>:
  papier       ;nazwa_papieru 		 ;Enter paper size or [?] <ISO full bleed A3>:
  "_M"              ;Enter paper units [Inches/Millimeters] <Millimeters>:
  "_L"			             ;Enter drawing orientation [Por.../Lan...] <Lan...>:
  "_N"                       ;Plot upside down? [Yes/No] <No>:
  "_W"                   ;Enter plot area [D.../E.../L.../V.../W...] <Window>:
  pt1                       ;Enter lower left corner of window <0,0>:
  pt2					 ;Enter upper right corner of window <420,297>:
  skala                      ;Enter plot scale or [Fit] <1=1>:
  "_C"                   ;Enter plot offset (x,y) or [Center] <Center>:
  "_Y"                      ;Plot with plot styles? [Yes/No] <Yes>:
  nazwa_stylu               ;Enter plot style table name or [?] <>:
  "_Y"                      ;Plot with lineweights? [Yes/No] <Yes>:
  "_A"                       ;Scale lineweights with plot scale? [Yes/No] <No>:
  filename					;œcie¿ka pliku
  "_N"						;Proceed with plot [Yes/No] <Y>:
  "_Y"
   ) ;comand if
   
(command "_plot" 
  "_Y"                       ;Detailed plot configuration? [Yes/No] <No>: Y
  (getvar "ctab")                    ;Enter a layout name or [?] <0_01a>:
  nazwa_drukarki          ;Enter an output device name or [?] <DWG To PDF.pc3>:
  papier       ;nazwa_papieru 		 ;Enter paper size or [?] <ISO full bleed A3>:
  "_M"              ;Enter paper units [Inches/Millimeters] <Millimeters>:
  "_L"			             ;Enter drawing orientation [Por.../Lan...] <Lan...>:
  "_N"                       ;Plot upside down? [Yes/No] <No>:
  "_W"                   ;Enter plot area [D.../E.../L.../V.../W...] <Window>:
  pt1                       ;Enter lower left corner of window <0,0>:
  pt2					 ;Enter upper right corner of window <420,297>:
  skala                      ;Enter plot scale or [Fit] <1=1>:
  "_C"                   ;Enter plot offset (x,y) or [Center] <Center>:
  "_Y"                      ;Plot with plot styles? [Yes/No] <Yes>:
  nazwa_stylu               ;Enter plot style table name or [?] <>:
  "_Y"                      ;Plot with lineweights? [Yes/No] <Yes>:
  "_Y"					;Scale lineweights with plot scale? [Yes/No] <No>
  "_N"							;Plot paper space first? [Yes/No] <No>
  "_N"							;Hide paperspace objects? [Yes/No] <No>
  filename					;œcie¿ka pliku
  "_N"
  "_Y" 
  "_Y"  ;Proceed with plot [Yes/No] <Y>:
   ) ;command else  
);if


				;----------
              );progn
            );repeat dlugosc
            (graphscr)
            ;----------
          );progn
		  
        );if
      );progn
    );if
    ;----------

	(command "_UCS" "_previous") 
	
    (princ)
    ;----------
  );progn
);WYPISZ_LISTY_DXF




;===================================================================================
; === START START START START START START START START START START START START  =====
;===================================================================================
	
	
	;|   komentarz_obszarowy
	;=== funkcja do wyboru oknem albo wszystko
	(progn
    ;----------
      (setq operator (getstring "\nWybierasz OknO czy drukujesz wszystko \(pozosta³e znaki\)" "O"))
	  (if (or (= operator "O") (= operator "o"))
		(progn
		  (setq wybor "_W")
		  (setq A (getpoint "Wska¿ PIERWSZY punkt obszaru do druku"))
		  (setq B (getpoint "Wska¿ DRUGI punkt obszaru do druku"))
		);progn
		(progn 
		  (setq wybor "X")
		  (setq A nil)
		  (setq B nil)
		);progn ELSE
	  );if
    (princ)
    ;----------
  );progn
  koniec_komentarza |;
	
	

   
   
   ;(setq zbior (ssget "X"))
   

	;(setvar 'Filedia 0)

(progn	
(setq stara_warstwa (getvar "CLAYER"))
	(if (not (tblsearch "layer" "DRUK"))
	(progn	
		(command
			"_layer"
			"_M"  ; tworzy i ustawia jako aktualn¹ warstê
			"DRUK"
			"_P"
			"_N"
			"DRUK"
			"_set"
			stara_warstwa
			""
		)		
	);progn
	(progn
		(command
			"_layer"
			"_M"  ; tworzy i ustawia jako aktualn¹ warstê
			"DRUK"
			"_P"
			"_N"
			"DRUK"
			"_set"
			stara_warstwa
			""
		)
	);progn else
	);if
	
(princ)
);progn
	
