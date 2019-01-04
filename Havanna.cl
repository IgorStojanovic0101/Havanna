;#########################################################################################################
;												GLOBALNE KONSTANTE
;#########################################################################################################
;Konstanta koja predstavlja pocetno stanje igre
(defconstant INICIJALNO_STANJE '( ((A 0) (B 1) (B 2) (B 4) (B 5) (D 2) (D 3)(A 5))
							   ((A 3) (F 7)) (0 0)))
;Konstante pocotnih grafova
(defconstant CRNI_GRAF '(
((A 0) ((A 1) (B 0) (B 1))) 
((B 1) ((A 0) (A 1) (B 0) (B 2) (C 1) (C 2)))
((B 2) ((A 1) (A 2) (B 1) (B 3) (C 2) (C 3)))
((B 4) ((A 3) (A 4) (B 3) (B 5) (C 4) (C 5)))
((B 5) ((A 4) (A 5) (B 4) (B 6) (C 5) (C 6)))
((D 2) ((C 1) (C 2) (D 1) (D 3) (E 2) (E 3)))
((D 3) ((C 2) (C 3) (D 2) (D 4) (E 3) (E 4)))


))

(defconstant BELI_GRAF '(
((F 7)((E 6) (E 7) (F 6) (F 8) (G 7) (G 8)))
))

(defconstant RANDOM_GRAF '())

(defconstant TEMENA '((A 0) (F 0) (K 0) (A 5) (F 10) (K 10)))
(defconstant STRANICE '((A 1) (A 2) (A 3) (A 4) (B 0) (C 0)(D 0)(E 0)(B 6) (C 7)(D 8)(E 9)(G 1)(I 3)(J 4)(G 10)(H 2)(H 10)(I 10)(J 10)
(K 6)(K 7)(K 8)(K 9)))



;Konstante figura i polja
(defconstant CRNA_FIGURA 'x)
(defconstant BELA_FIGURA 'o)
(defconstant PRAZNO_POLJE_SIMBOL '-)

;Konstante kordinata skora u stanju
(defconstant SCORE_BELI '(2 1))
(defconstant SCORE_CRNI '(2 0))

;Konstante za predstavljanje igraca
(defconstant IGRAC_COVEK 'c)
(defconstant IGRAC_RACUNAR 'r)
(defconstant IGRAC_COVEK_DRUGI 's)

;Konstante za predstavljanje moda igranja
(defconstant IGRAJU_COVEK_VS_COVEK 'cc)
(defconstant IGRAJU_COVEK_VS_RACUNAR 'cr)
(defconstant IGRAJU_RACUNAR_VS_COVEK 'rc)


;Konstanjte za inicijalno postavljanje figura sa kojima igraci igraju
(defconstant FIGURE_COVEK_VS_COVEK '((c x) (s o)))
(defconstant FIGURE_COVEK_VS_RACUNAR '((c x) (r o)))
(defconstant FIGURE_RACUNAR_VS_COVEK '((r x) (c o)))

;Konstante vezane za predstavljanje table pozicioniranje i ogranicenje 
(defconstant SIRINA_TABLE 6)
(defconstant SLOVNE_POZICIJE '(A B C D E F G H I J K))
(defconstant BROJEVNE_POZICIJE '(0 1 2 3 4 5 6 7 8 9 10))

;Konstante koje predstavljaju moguce pravce
(defconstant GORE_LEVO 'GL)
(defconstant GORE_DESNO 'GD)
(defconstant LEVO 'L)
(defconstant DESNO 'D)
(defconstant DOLE_LEVO 'DL)
(defconstant DOLE_DESNO 'DD)
(defconstant NEMA_PRAVCA 'NA)




(defconstant MAX_DUBINA 10)
(defconstant MAX_VREME 45)


;#########################################################################################################
;												FUNKCIJE IGRE
;#########################################################################################################

;Funkcija za pokretanje igre
(defun Havanna_game ()
	(let* (	
			(pocetno_stanje  (Stampaj_tablu INICIJALNO_STANJE))
			(prvi_igrac (progn (format t "~%Unesite ko igra prvi(cc ili rc i cr): ") (read)))
		)
		(cond 
		((equal prvi_igrac IGRAJU_COVEK_VS_COVEK) (Igraj_potez IGRAC_COVEK  FIGURE_COVEK_VS_COVEK pocetno_stanje))
		((equal prvi_igrac IGRAJU_RACUNAR_VS_COVEK) (Igraj_potez IGRAC_RACUNAR  FIGURE_RACUNAR_VS_COVEK pocetno_stanje))
		((equal prvi_igrac IGRAJU_COVEK_VS_RACUNAR) (Igraj_potez IGRAC_COVEK  FIGURE_COVEK_VS_RACUNAR pocetno_stanje))
		(:else (progn (format t "~%Pogresan unos probajte ponovo.~%")(Havanna_game)))
		
		)
		
	)
)

;Funkcija za odigravanje poteza 
(defun Igraj_potez (ko_igra cije_su_figure stanje)
	(let* ((stanje_posle_novog_poteza (cond ((equal ko_igra IGRAC_COVEK) (Igraj_potez_covek (cadr (assoc IGRAC_COVEK cije_su_figure)) stanje))
											((equal ko_igra IGRAC_COVEK_DRUGI) (Igraj_potez_covek (cadr (assoc IGRAC_COVEK_DRUGI cije_su_figure)) stanje))
											((equal ko_igra IGRAC_RACUNAR) (Igraj_potez_racunar (cadr (assoc IGRAC_RACUNAR cije_su_figure)) stanje))
										))
			(igra_sledeci (progn (Stampaj_tablu stanje_posle_novog_poteza) (Ko_igra_sledeci ko_igra cije_su_figure)))
		)
		(cond ((Ciljno_stanjeP stanje_posle_novog_poteza) (Stampa_kraj_igre stanje_posle_novog_poteza))
				(:else (Igraj_potez igra_sledeci cije_su_figure stanje_posle_novog_poteza))
		)
	)
)
;Funkcija koja vraca ko igra sledeci
(defun Ko_igra_sledeci (ko_igra cije_su_figure)
	(cond ((equal ko_igra (caar cije_su_figure)) (caadr cije_su_figure))
			(:else (caar cije_su_figure))
	)
)
;Funkcija koja vraca sledecu figuru
(defun Sledeca_figura (figura)
	(cond ((equal figura CRNA_FIGURA) BELA_FIGURA)
			((equal figura BELA_FIGURA) CRNA_FIGURA)
			(:else figura)
	)
)
(defun Vrati_figuru_na_onosvu_grafa (graf)
	(cond ((equal graf CRNI_GRAF) CRNA_FIGURA)
			((equal graf BELI_GRAF) BELA_FIGURA)
		
	)
)
(defun Vrati_graf_na_onosvu_figure (figura)
	(cond ((equal figura CRNA_FIGURA) CRNI_GRAF)
			((equal figura BELA_FIGURA) BELI_GRAF)
		
	)
)


;
;Funckija koja proverava dali je dostignuto krajnje stanje 
(defun Ciljno_stanjeP (stanje)
	(cond ((null stanje) '())
			((or (> (caar (last stanje)) 0) (> (cadar (last stanje)) 0)) 't)
			
	)
)
(defun MostP (graf unos stanje figura)
	(cond ((> (Nadjenih_broj graf unos TEMENA stanje figura) 1)  
	(progn (format t "~%Most") 't))
		(:else '())
	)
)
(defun VilaP (graf unos stanje figura)
	(cond ((> (Nadjenih_broj graf unos STRANICE stanje figura) 2)
		(progn (format t "~%Vila") 't))
		(:else '())
	)
)
(defun PrstenP (graf unos stanje figura)
	(let* (
		  (putevi (Nadjeni_putevi graf unos (list unos) stanje figura))
		 
	)
	(cond ( (Broj_elemenata_u_listiP putevi 5)
		(progn (format t "~%Prsten") 't))
		(:else '())
	)
	)
)
(defun End_gameP(graf unos stanje figura)
	(let* (
		  (mostP (MostP graf unos stanje figura))
		  (vilaP (VilaP graf unos stanje figura))
		  (prstenP (PrstenP graf unos stanje figura))
	)
	
	(cond
	 ((or mostP vilaP prstenP) 't)
		(:else '())
	)
	)
	
)


;#########################################################################################################
;										FUNKCIJE ZA IGRANJE POTEZA
;#########################################################################################################

;Funkcija za odigravanje poteza coveka
(defun Igraj_potez_covek (figura stanje)
	(let* ((unos (progn (Stampa_ko_igra figura) (Korisnicki_unos)))	
		(figure_na_pozicijama (Na_pozicijama_figuraP (list unos) stanje figura))
	)
		(cond 
				((null(Potez_ispravanP unos)) (progn (format t "~%Prekoracenje,ponovo unesite potez. ") (Igraj_potez_covek figura stanje)))
				((equal figure_na_pozicijama '()) (progn (format t "~%Postoji figura na poziciji ,ponovo unesite potez. ") (Igraj_potez_covek figura stanje)))
				(:else 
				(cond
			    ((equal CRNA_FIGURA figura)  (Unos_u_graf unos figura CRNI_GRAF stanje))
				((equal BELA_FIGURA figura)  (Unos_u_graf unos figura BELI_GRAF stanje))
				)
				)
		)
	)	
)	

(defun Igraj_potez_racunar (figura stanje)
	(Stampa_ko_igra figura)
	(Min_max_alfa_beta stanje 1 (get-universal-time) figura stanje)
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

;Funkcija koja stampa da je kraj igre i ko je pobedio 
(defun Stampa_kraj_igre (stanje)
	(cond ((> (caar (last stanje)) 0) (progn (format t "~%~%~%KRAJ IGRE pobedio je CRNI igrac.~%~%~%") '(C E S T I T A M O ! ! !)))
			((> (cadar (last stanje)) 0) (progn (format t "~%~%~%~%KRAJ IGRE pobedio je BELI igrac.~%~%~%") '(C E S T I T A M O ! ! !)))
	)
)
;
;Funkcija koja stampa ko igra
(defun Stampa_ko_igra (figura)
	(cond ((equal figura CRNA_FIGURA) (format t "~%Na potezu je CRNI."))
			((equal figura BELA_FIGURA) (format t "~%Na potezu je BELI."))
			(:else (format t "~%Nezna se ko je na potezu."))
	)
)

;#########################################################################################################
;									FUNKCIJE ZA FORMIRANJE TABLE
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

;#########################################################################################################
;							FUNKCIJE ZA INTERAKCIJU SA STANJEM I TABELOM
;#########################################################################################################
(defun Ostale_pozicije (tabla vrsta kolona)
	(cond ((null tabla) '())
	(t (append (Update_elemenat (car tabla) vrsta kolona)(Ostale_pozicije (cdr tabla) (+ vrsta 1) kolona) ))
	)
)
(defun Update_elemenat (tabla vrsta kolona)
	(cond ((null tabla) '())
		
		((equal (car tabla) PRAZNO_POLJE_SIMBOL) 
		(case (Transfer_broj_u_slovo vrsta)
					((K) (cons (list (Transfer_broj_u_slovo vrsta) (+ kolona 5))(Update_elemenat (cdr tabla) vrsta (+ kolona 1))))
					((J) (cons (list (Transfer_broj_u_slovo vrsta) (+ kolona 4))(Update_elemenat (cdr tabla) vrsta (+ kolona 1))))
					((I) (cons (list (Transfer_broj_u_slovo vrsta) (+ kolona 3))(Update_elemenat (cdr tabla) vrsta (+ kolona 1))))
					((H) (cons (list (Transfer_broj_u_slovo vrsta) (+ kolona 2))(Update_elemenat (cdr tabla) vrsta (+ kolona 1))))
					((G) (cons (list (Transfer_broj_u_slovo vrsta) (+ kolona 1))(Update_elemenat (cdr tabla) vrsta (+ kolona 1))))
					((F) (cons (list (Transfer_broj_u_slovo vrsta) (+ kolona 0))(Update_elemenat (cdr tabla) vrsta (+ kolona 1))))
					((E) (cons (list (Transfer_broj_u_slovo vrsta) (+ kolona 0))(Update_elemenat (cdr tabla) vrsta (+ kolona 1))))
					((D) (cons (list (Transfer_broj_u_slovo vrsta) (+ kolona 0))(Update_elemenat (cdr tabla) vrsta (+ kolona 1))))
					((C) (cons (list (Transfer_broj_u_slovo vrsta) (+ kolona 0))(Update_elemenat (cdr tabla) vrsta (+ kolona 1))))
					((B) (cons (list (Transfer_broj_u_slovo vrsta) (+ kolona 0))(Update_elemenat (cdr tabla) vrsta (+ kolona 1))))
					((A) (cons (list (Transfer_broj_u_slovo vrsta) (+ kolona 0))(Update_elemenat (cdr tabla) vrsta (+ kolona 1))))
					(:else '())
			)
			)


		
		(:else (Update_elemenat (cdr tabla) vrsta (+ kolona 1)))
	)

)
;Funkcija koja vrsi zamenu vrednosti unutar table 
(defun Zameni_vrednost_u_tabli (kordinata tabla nova_vrednost)
	(Zameni_elemenat tabla (car kordinata) 
	(Zameni_elemenat (nth (car kordinata) tabla) (cadr kordinata) nova_vrednost))
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
;Funkcija koja proverava dali se pozicija nalazi na tabli 
(defun Na_tabliP (pozicija)
		(cond ((null pozicija) '())
			((atom pozicija) '())
			((or (null (car pozicija)) (null (cadr pozicija))) '())
			((case (car pozicija)
					((K) (if (and (< (cadr pozicija) 11) (> (cadr pozicija) 4)) '() 't))
					((J) (if (and (< (cadr pozicija) 11) (> (cadr pozicija) 3)) '() 't))
					((I) (if (and (< (cadr pozicija) 11) (> (cadr pozicija) 2)) '() 't))
					((H) (if (and (< (cadr pozicija) 11) (> (cadr pozicija) 1)) '() 't))
					((G) (if (and (< (cadr pozicija) 11) (> (cadr pozicija) 0)) '() 't))
					((F) (if (and (< (cadr pozicija) 11)(> (cadr pozicija) -1)) '() 't))
					((E) (if (and (< (cadr pozicija) 10)(> (cadr pozicija) -1))'() 't))
					((D) (if (and (< (cadr pozicija) 9) (> (cadr pozicija) -1)) '() 't))
					((C) (if (and (< (cadr pozicija) 8) (> (cadr pozicija) -1))'() 't))
					((B) (if (and (< (cadr pozicija) 7) (> (cadr pozicija) -1)) '() 't))
					((A) (if (and (< (cadr pozicija) 6) (> (cadr pozicija) -1)) '() 't))
					(:else '())
				)
					'()
			)
			(:else pozicija)
	)
				
)
;Funkcija koja menja odgovarajuci broj u slovo
(defun Transfer_broj_u_slovo (broj)
	(case broj
		((10) 'A)
		((9) 'B)
		((8) 'C)
		((7) 'D)
		((6) 'E)
		((5) 'F)
		((4) 'G)
		((3) 'H)
		((2) 'I)
		((1) 'J)
		((0) 'K)
		((:else) '())
	)
)

;Funkcija koja menja odgovarajuce slovo u broj
(defun Transfer_slovo_u_broj (slovo)
   (case slovo
		 ((A) '10)
		 ((B) '9)
		 ((C) '8)
		 ((D) '7)
		 ((E) '6)
		 ((F) '5)
		 ((G) '4)
		 ((H) '3)
		 ((I) '2)
		 ((J) '1)
		 ((K) '0)
		 (:else '())
	 )
)
;Funkcija koja daje listu pozicija figura u stanju
(defun Pozicije_figura (figura stanje)
	(cond	((equal figura CRNA_FIGURA) (car stanje))
			((equal figura BELA_FIGURA) (cadr stanje))
			(:else '())
	)
)

;Funkcija koja proverava dali su figure na zadatim pozicijama
(defun Na_pozicijama_figuraP (pozicija stanje figura)
	(cond ((null pozicija) 't)
			((or (Podlista_u_listiP (car pozicija) (Pozicije_figura figura stanje)) (Podlista_u_listiP (car pozicija) (Pozicije_figura (Sledeca_figura figura) stanje))) '())
			
			(:else 't) 
	)
)

(defun Na_poziciji_igraceva_figura (pozicija stanje figura)
	(cond ((null pozicija) '())
			((Podlista_u_listiP pozicija (Pozicije_figura figura stanje)) 't)
			
			(:else '()) 
	)
)
;#########################################################################################################
;									Funkcija za imple. Min-max algoritma
;#########################################################################################################

;Funkcija koja nalazi najbolji potez
(defun Min_max_alfa_beta (stanje pd_dubina pocetno_vreme figura ciljno_stanje) 
	(progn (format t "~%Trazi na ~a" pd_dubina)
	(cond ((or (> (- (get-universal-time) pocetno_vreme ) MAX_VREME) (equal pd_dubina (1+ MAX_DUBINA))) (progn (format t "~%Dubina trazenja ~a" (1- pd_dubina)) ciljno_stanje))
			(:else 
			 (let* (
						(novo_ciljno_stanje (Maxi_mini stanje figura))
						
			)
					
				(cond 
						((null novo_ciljno_stanje) (Min_max_alfa_beta stanje (1+ pd_dubina) pocetno_vreme figura ciljno_stanje)) 
						(:else  (Uredi stanje figura novo_ciljno_stanje)  )
						)
					)
			)
	))
	
)
(defun Maxi_mini (stanje figura)																						
	(let*(  (tabla (Kreiraj_tablu SIRINA_TABLE stanje))
			(moguce_pozicije (Ostale_pozicije tabla 0 0))
			(graf_sa_svim_pozicijama (Update_graf RANDOM_GRAF moguce_pozicije figura))
			(sortirane_pozicije (Sortiraj_sledbenike_opadajuce moguce_pozicije))
			)		
		
		
		 (Max_vrednost graf_sa_svim_pozicijama sortirane_pozicije stanje figura)
	)
)

(defun Max_vrednost (graf_sa_svim_pozicijama sortirani_sledbenici stanje_roditelja figura)
	(let* ((pozicija (list (caar sortirani_sledbenici) (cadar sortirani_sledbenici))) 
		)
		(cond ((End_gameP graf_sa_svim_pozicijama pozicija stanje_roditelja figura) 
				(cond 
		   			((equal CRNA_FIGURA figura)(Zameni_vrednost_u_tabli SCORE_CRNI stanje_roditelja (1+ (caar(last stanje_roditelja)))))
		   			((equal BELA_FIGURA figura) (Zameni_vrednost_u_tabli SCORE_BELI stanje_roditelja (1+ (cadar(last stanje_roditelja)))))
				)
			)
		  (:else pozicija)
		)
	)
	
)
;Funkcija koja sortira sledbenika u opadajucem redosledu po vrednosti 
(defun Sortiraj_sledbenike_opadajuce (sledbenici)
	(let* ((sledbenici_vrednost (Dodaj_random_sledbenicima sledbenici))
		)
		(Uredi_sledbenike sledbenici_vrednost)
	)
)
;
;Funkcija koja ide kroz sledbenike
(defun Uredi_sledbenike (sledbenici)
	(cond ((null sledbenici) '())
			(:else (Umetni_sledbenike (car sledbenici) (Uredi_sledbenike (cdr sledbenici))))
	)
)
;
;Funkcija koja nalazi najveci elemenat niza i konstruise sortiran niz
(defun Umetni_sledbenike (elemenat sledbenici)
	(cond ((null sledbenici) (list elemenat))
			((> (car (last elemenat)) (car (last (car sledbenici)))) (cons elemenat sledbenici))
			(:else (cons (car sledbenici) (Umetni_sledbenike elemenat (cdr sledbenici))))
	)
)

;Funkcija koja sledbenicima dodaje random vrednost
(defun Dodaj_random_sledbenicima (sledbenici) 
	(cond ((null sledbenici) '())
			(:else (let* (
							(random_vrednost (random 100))
							)
						(cons (append (car sledbenici) (list random_vrednost)) (Dodaj_random_sledbenicima (cdr sledbenici) ))
					)
			)
	)
)


;#########################################################################################################
;										FUNKCIJE ZA TRAZENJE
;#########################################################################################################

(defun Unos_u_graf (unos figura graf stanje)
	(let* (
	(in (Uredi stanje figura unos))
	(graf_new (napravi_graf unos figura graf))
	(end_game (End_gameP graf_new unos in figura))
	)
	(cond 
	((equal end_game 't) 
		(cond 
		   ((equal CRNA_FIGURA figura)(Zameni_vrednost_u_tabli SCORE_CRNI in (1+ (caar(last stanje)))))
		   ((equal BELA_FIGURA figura) (Zameni_vrednost_u_tabli SCORE_BELI in (1+ (cadar(last stanje)))))
		)
		
	)
	(:else in)
		)
	)

)
(defun Nadjenih_broj (graf unos lista stanje figura)		
		(cond 
		((null lista) '0)
		 ((and (Nadji-putP graf unos  (car lista)) (Na_poziciji_igraceva_figura (car lista) stanje figura) )
		 (1+ (Nadjenih_broj graf unos (cdr lista) stanje figura)))
		  (:else (Nadjenih_broj graf unos (cdr lista) stanje figura))
	)
)
(defun Broj_elemenata_u_listiP (lista broj)	
	
		(cond 
			((null lista) '())
		 	((> (length(car lista)) broj) 't)
		 
		  (:else (Broj_elemenata_u_listiP (cdr lista) broj))
		)	
		
)

(defun Nadji-putP(graf unos cilj)
	(cond ((null graf) '())
		
		  ((null(nadji-put graf (list unos) cilj '())) '()) 
		  (:else t)
	)
)
(defun Nadjeni_putevi (graf unos lista stanje figura)
	
	(cond ((null lista) '())
		  ((and (Nadji-putP graf unos  (car lista)) (Na_poziciji_igraceva_figura (car lista) stanje figura))
		  (cons (nadji-put graf (list unos)  (car lista) '())
		  (Nadjeni_putevi graf unos (cdr lista) stanje figura)))
		  (:else (Nadjeni_putevi graf unos (cdr lista) stanje figura))
	)
		
)
;Funkcija koja pravi graf
(defun Update_graf (graf pozicije figura)
	(cond  ((null pozicije)  '())
			(:else (cons (car (napravi_graf (car pozicije) figura graf))(Update_graf graf (cdr pozicije) figura) ))) 
	
	

)
(defun napravi_graf (potez figura graf)
	(cond  ((null potez)  '()) 
		((equal figura CRNA_FIGURA) 
			(cond ((null graf)(cons (cons potez (list (Nadji_susede potez))) graf ))
				  (:else (cons (car graf)(cons(cons potez (list (Nadji_susede potez)))(cdr graf) )))
				  ))
		((equal figura BELA_FIGURA) 
			(cond ((null graf)(cons (cons potez (list (Nadji_susede potez))) graf ))
				 (:else (cons (car graf)(cons(cons potez (list (Nadji_susede potez)))(cdr graf) )))
				 ))
		(:else '())
	)

)

(defun nadji-put (graf l cilj cvorovi) 
	(cond  ((null l)  '()) 
	
		((equal (car l) cilj)  (list cilj))
		(t  (let* 
		((cvorovi1 (append cvorovi (list (car l)))) 
		(potomci1 (dodaj-potomke graf (car l) (append (cdr l)  cvorovi1))) 
		(l1 (append (cdr l) potomci1)) 
		(nadjeni-put (nadji-put graf l1 cilj cvorovi1)))

		(cond ((null nadjeni-put)  '()) 
			((Podlista_u_listiP(car nadjeni-put) potomci1)  (cons (car l) nadjeni-put)) 
	(t  nadjeni-put)))))
)

(defun dodaj-potomke (graf cvor cvorovi)
       (cond ((null graf) '())
		((equal (caar graf) cvor)   (novi-cvorovi (cadar graf) cvorovi))
	(t (dodaj-potomke (cdr graf) cvor cvorovi)))
) 


(defun novi-cvorovi (potomci cvorovi)
       (cond ((null potomci) '())
	((member (car potomci) cvorovi)(novi-cvorovi (cdr potomci) cvorovi))
	(t (cons (car potomci)
	(novi-cvorovi (cdr potomci) cvorovi))))
)


;#########################################################################################################
;									FUNKCIJA ZA ORIJENTACIJU
;#########################################################################################################




;Funkcija koja proverava dali se pozicija nalazi u okolini
(defun U_okoliniP (pozicija pozicija_provere)
	(cond ((Podlista_u_listiP pozicija_provere (Nadji_susede pozicija)) 't)
			(:else '())
	)
)

;Funkcija daje poziciju u pravcu na rastojanju 1
(defun Nadji_u_pravcu (pozicija pravac)
	(Na_tabliP (cond ((equal pravac GORE_DESNO) (list (Transfer_broj_u_slovo(1+ (Transfer_slovo_u_broj (car pozicija)))) (cadr pozicija)))
					((equal pravac GORE_LEVO) (list (Transfer_broj_u_slovo(1+ (Transfer_slovo_u_broj (car pozicija)))) (1- (cadr pozicija))))
					((equal pravac LEVO) (list (Transfer_broj_u_slovo(Transfer_slovo_u_broj (car pozicija))) (1- (cadr pozicija))))
					((equal pravac DESNO) (list (Transfer_broj_u_slovo(Transfer_slovo_u_broj (car pozicija))) (1+ (cadr pozicija))))
					((equal pravac DOLE_LEVO) (list (Transfer_broj_u_slovo(1- (Transfer_slovo_u_broj (car pozicija)))) (cadr pozicija)))
					((equal pravac DOLE_DESNO) (list (Transfer_broj_u_slovo(1- (Transfer_slovo_u_broj (car pozicija)))) (1+ (cadr pozicija))))
			(:else '())
		)
	)
)

;Funkcija koja vraca sve okolne pozicije koje su na tabli
(defun Nadji_susede (pozicija)
	(cond ((null (Na_tabliP pozicija)) '())
			(:else (let	((glevo (Nadji_u_pravcu pozicija GORE_LEVO))
						(gdeno (Nadji_u_pravcu pozicija GORE_DESNO))
						(levo_l (Nadji_u_pravcu pozicija LEVO))
						(desno_l (Nadji_u_pravcu pozicija DESNO))
						(dlevo (Nadji_u_pravcu pozicija DOLE_LEVO))
						(ddesno (Nadji_u_pravcu pozicija DOLE_DESNO))
						)
						(append '()
								(if (null glevo) '() (list glevo))
								(if (null gdeno) '() (list gdeno))
								(if (null levo_l) '() (list levo_l))
								(if (null desno_l) '() (list desno_l))
								(if (null dlevo) '() (list dlevo))
								(if (null ddesno) '() (list ddesno))
						)
					)
			)
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
	(cond 	((null ulaz)'())
			((or (null (car ulaz))  (null (cadr ulaz) ))'())	
			((or (listp (car ulaz))  (listp(cadr ulaz) ))'())
			((not (equal (length  ulaz) 2)) '())
			((and (Atom_u_listiP (car ulaz) BROJEVNE_POZICIJE) (Atom_u_listiP (cadr ulaz) BROJEVNE_POZICIJE)) '())
			((and (Atom_u_listiP (car ulaz) SLOVNE_POZICIJE) (Atom_u_listiP (cadr ulaz) SLOVNE_POZICIJE)) '())
			((and (Atom_u_listiP (car ulaz) SLOVNE_POZICIJE) (Atom_u_listiP (cadr ulaz) BROJEVNE_POZICIJE)) 't)
			(:else '())
	)
)

;Funkcija koja proverava sintaksno ispravan potez 
(defun Potez_ispravanP (ulaz)
		(cond ((null ulaz) '())
				( (not(Na_tabliP ulaz)) '())
				
				(:else 't)
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
(defun Uredi (stanje figura potez)
	(cond ((null stanje) '())
			((equal figura CRNA_FIGURA) (cons (Umetni potez (car stanje)) (cdr stanje)))
			((equal figura BELA_FIGURA)  (cons (car stanje)(cons (Umetni potez (cadr stanje)) (cddr stanje) )))
			(:else stanje)
	)
)

(defun Umetni (elemenat sledbenici)
	(cond ((null sledbenici) (list elemenat))
	(:else 
		 (cons (car sledbenici) (Umetni elemenat (cdr sledbenici)))
	)
	)
)

;Funkcija koja kaze dali se podlista nalazi u listi
(defun Podlista_u_listiP (podlista lista)
	(cond ((null lista) '())
			((equal podlista (car lista)) 't)
			(:else (Podlista_u_listiP podlista (cdr lista)))
	)
)		