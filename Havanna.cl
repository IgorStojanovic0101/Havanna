;#########################################################################################################
;												GLOBALNE KONSTANTE
;#########################################################################################################
;Konstanta koja predstavlja pocetno stanje igre
(defconstant INICIJALNO_STANJE '( ((A 66) (A 0) (B 0) (C 0) (D 0) (E 0) (F 0) (G 1) (H 2) (I 3) (J 4) (K 5))
							   ((A 99) (A 4) (B 4) (C 2) (D 5) (E 7) (F 6) (G 8) (H 9) (I 10))  ))

;Konstante figura i polja
(defconstant CRNA_FIGURA 'x)
(defconstant BELA_FIGURA 'o)
(defconstant PRAZNO_POLJE_SIMBOL '-)

;Konstante za predstavljanje igraca
(defconstant IGRAC_COVEK 'c)

;Konstante vezane za predstavljanje table pozicioniranje i ogranicenje 
(defconstant SIRINA_TABLE 6)
(defconstant SLOVNE_POZICIJE '(A B C D E F G H I J K))
(defconstant BROJEVNE_POZICIJE '(0 1 2 3 4 5 6 7 8 9 10))

;#########################################################################################################
;												FUNKCIJE IGRE
;#########################################################################################################

;Funkcija za pokretanje igre
(defun Havanna_game ()
	(let* (
			(pocetno_stanje  (Stampaj_tablu INICIJALNO_STANJE))
		)
		(cond 
		
		((null pocetno_stanje) '())
		(:else (Igraj_potez IGRAC_COVEK pocetno_stanje))
		
		)
		
	)
)

;Funkcija za odigravanje poteza 
(defun Igraj_potez (ko_igra stanje)
	  (cond ((equal ko_igra IGRAC_COVEK) (Igraj_potez_covek CRNA_FIGURA stanje))
	  (:else '())
	  )
	  
		   
)



;#########################################################################################################
;										FUNKCIJE ZA IGRANJE POTEZA
;#########################################################################################################

;Funkcija za odigravanje poteza coveka
(defun Igraj_potez_covek (figura stanje)
			(let* ((unos (progn (Stampa_ko_igra figura) (Korisnicki_unos)))
		
			)
			(cond ((Korisnicki_unosP unos) (Stampaj_tablu  (insert unos '(A 66) stanje)))
			(:else (format t "~%Problem sa unosom."))
		)
)
)	



;#########################################################################################################
;									FUNKCIJA ZA STAMPANJE
;#########################################################################################################

;Funkcija koja stampa tablu 'velicine 6'
(defun Stampaj_tablu (stanje)
	(let ((tabla (Kreiraj_tablu SIRINA_TABLE stanje)))
		(progn 
			
			(format t "~%|---------------------------|")
			(format t "~%|         0 1 2 3 4 5       |")
			(format t "~%| A      ~{~a ~}6      |" (nth 10 tabla))
			(format t "~%| B     ~{~a ~}7     |" (nth 9 tabla))
			(format t "~%| C    ~{~a ~}8    |" (nth 8 tabla))
			(format t "~%| D   ~{~a ~}9   |" (nth 7 tabla))
			(format t "~%| E  ~{~a ~}10 |" (nth 6 tabla))
			(format t "~%| F ~{~a ~}  |" (nth 5 tabla))
			(format t "~%| G  ~{~a ~}   |" (nth 4 tabla))
			(format t "~%| H   ~{~a ~}    |" (nth 3 tabla))
			(format t "~%| I    ~{~a ~}     |" (nth 2 tabla))
			(format t "~%| J     ~{~a ~}      |" (nth 1 tabla))
			(format t "~%| K      ~{~a ~}       |" (nth 0 tabla))
			
			(format t "~%|---------------------------|")
			(cond ((null stanje) '()) (:else stanje))
		)
	)
)

;Funkcija koja stampa ko igra
(defun Stampa_ko_igra (figura)
	(cond ((equal figura CRNA_FIGURA) (format t "~%Na potezu je CRNI."))
			((equal figura BELA_FIGURA) (format t "~%Na potezu je BELI."))
			(:else (format t "~%Ne zna se ko igra."))
	)
)


;#########################################################################################################
;						FUNKCIJE ZA FORMIRANJE TABLE OD STANJA I NJIHOVE INTERAKCIJE
;#########################################################################################################

;Funckcija koja definise pocetno stanje table(prazno)
(defun Kreiraj_tablu (duzina_stranice stanje)
	(cond ((or (null duzina_stranice) (> SIRINA_TABLE duzina_stranice)) '()) 
			(:else (Ispuni_tablu(Ispuni_tablu (append (Kreiraj_praznu_tablu (Kreiraj_pola_prazne_table '() duzina_stranice PRAZNO_POLJE_SIMBOL)) (last stanje) stanje) (car stanje) CRNA_FIGURA) (cadr stanje) BELA_FIGURA))
	)
)
;
;Funkcija za ispunjavanje table na osnovu stanja
(defun Ispuni_tablu (tabla stanje figura)
	(cond ((null stanje) tabla)
			(:else (let* ((tabla1 (Zameni_vrednost_u_tabli (Transfer_pozicija_na_tabeli (car stanje)) tabla figura)))
						(cond ((null tabla1) '())
								(:else (Ispuni_tablu tabla1 (cdr stanje) figura)))))
	)
)

;
;Kreira tablu koristeci ono sto je vratila funkcija za popunjavanje table
(defun Kreiraj_praznu_tablu (tabla)
	(cond ((null (cdr tabla)) (list (car tabla)))
		(:else (append (list (car tabla)) (Kreiraj_praznu_tablu (cdr tabla)) (list (car tabla))))
	)
)


;Kreira polovinu table sa praznim poljima
(defun Kreiraj_pola_prazne_table(tabla duzina_stranice prazno_polje)
	(cond ((zerop duzina_stranice) (list tabla))
			((null tabla) (append (Kreiraj_pola_prazne_table (Kreiraj_listu '() duzina_stranice prazno_polje) (- duzina_stranice 1) prazno_polje) tabla))
			(:else (cons tabla (Kreiraj_pola_prazne_table (append tabla (list prazno_polje)) (- duzina_stranice 1)  prazno_polje)))
	)
)



;Funkcija koja vrsi zamenu vrednosti unutar table 
(defun Zameni_vrednost_u_tabli (kordinata tabla nova_vrednost)
	(Zameni_elemenat tabla (car kordinata) (Zameni_elemenat (nth (car kordinata) tabla) (cadr kordinata) nova_vrednost))
)

;Funkcija koja vraca poziciju u stanju u odnosu na unetu poziciju
(defun Transfer_pozicija_na_tabeli (pozicija)
	(case (car pozicija)
					((K) (list 0 (- (cadr pozicija) 5)))
					((J) (list 1 (- (cadr pozicija) 4)))
					((I) (list 2 (- (cadr pozicija) 3)))
					((H) (list 3 (- (cadr pozicija) 2)))
					((G) (list 4 (- (cadr pozicija) 1)))
					((F) (list 5 (- (cadr pozicija) 0)))
					((E) (list 6 (- (cadr pozicija) 0)))
					((D) (list 7 (- (cadr pozicija) 0)))
					((C) (list 8 (- (cadr pozicija) 0)))
					((B) (list 9 (- (cadr pozicija) 0)))
					((A) (list 10(- (cadr pozicija) 0)))
					(:else '())
	)
)


;#########################################################################################################
;									FUNKCIJE ZA UNOS POTEZA
;#########################################################################################################

;Funkcija koja uzima korisnicki ulaz i ispituje dali je sintaksno ispravan ako jeste vraca vraca unos ako ne trazi ponavljanje unosa
(defun Korisnicki_unos ()
	(let* ((ulaz (progn (format t "~% Unesite naredni potez: ") (read)))
		)
			(cond ((Korisnicki_unosP ulaz) ulaz)
					(:else (progn (format t "~%Pogresan unos,ponovo: ") (Korisnicki_unos)))
			)
	)
)
;
;Funkcija pomocna za formatirani ulaz vrsi proveru dali je ulaz sintaksno ispravan
(defun Korisnicki_unosP (ulaz)
	(cond 
			
			((not (equal (length  ulaz) 2)) '())
			((and (Atom_u_listiP (car ulaz) SLOVNE_POZICIJE) (Atom_u_listiP (cadr ulaz) BROJEVNE_POZICIJE)) 't)
			(:else '())
	)
)

;#########################################################################################################
;									UNIVERZALNE POMOCNE FUNKCIJE
;#########################################################################################################

;Funkcija koja ispituje dali je atomicni elemenat u listi
(defun Atom_u_listiP (elemenat lista)
	(cond ((null (member elemenat lista)) '())
			(:else 't)
	)
)
;Funkcija kreira listu sa zadatim elementom kao listovima
(defun Kreiraj_listu (lista broj_lista listovi)
	(cond ((zerop broj_lista) '())
		(:else (append (Kreiraj_listu lista (- broj_lista 1) listovi) (list listovi)))
	)
)

;Funkcija koja menja elemenat liste na zadatom mestu sa,indeksiranje je od 0
(defun Zameni_elemenat (lista index element)
	(cond ((null lista) '())
		((zerop index) (cons element (cdr lista)))
		(:else (cons (car lista) (Zameni_elemenat (cdr lista) (1- index) element)))
	)
)
;Funkcija za dodavanje elementa
(defun insert (new elem list)
  (cond ((null list) '())
        ((equal elem (car list)) (cons new (cons (car list) (insert new elem (cdr list)))))
        ((atom (car list)) (cons (car list) (insert new elem (cdr list))))
        (:else (cons (insert new elem (car list)) (insert new elem (cdr list))))))