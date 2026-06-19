let coloration_first_fit graphe =
  let n = Array.length graphe in
  let coloration = Array.make n (-1) in
  let dispo = Array.make n false in 

  let rec maj_voisins liste_voisins = match liste_voisins with (* marque les couleurs des voisins *)
    | [] -> ()
    | voisin::q -> 
        let couleur_voisin = coloration.(voisin) in
        if couleur_voisin <> -1 then dispo.(couleur_voisin) <- true;
        maj_voisins q
  in

  let couleur_min () = (* renvoie la couleur minimale disponible pour le sommet *)
    let couleur = ref 0 in
    while dispo.(!couleur) do
      couleur := !couleur + 1
    done;
    !couleur
  in

  let rec nettoie_voisins liste_voisins = match liste_voisins with (* remet dispo à false pour le prochain sommet *)
    | [] -> ()
    | voisin::q ->
        let couleur_voisin = coloration.(voisin) in
        if couleur_voisin <> -1 then dispo.(couleur_voisin) <- false;
        nettoie_voisins q
  in

  for sommet = 0 to (n-1) do (* parcourt les différents sommets et les colorie *)
    maj_voisins graphe.(sommet);
    let couleur = couleur_min () in
    coloration.(sommet) <- couleur;
    nettoie_voisins graphe.(sommet) 
  done;
  coloration
;; (* O(n + m) *)

let file_vide n = Array.make (n+1) (0,0) ;;
let est_file_vide fp = let taille,_ = fp.(0) in taille = 0 ;;
let donnee_maxi fp = if est_file_vide fp then failwith "file vide" else fp.(1) ;;
let echange fp i j = let vi = fp.(i) in (fp.(i) <- fp.(j) ; fp.(j) <- vi) ;;

let remonter fp i =
  let k = ref i in
  while !k <> 1 && fp.(!k) > fp.(!k /2) do
    echange fp !k (!k/2) ;
    k := !k /2 ;
  done ;;
  

let insere fp (prio,som) =
  let taille,_ = fp.(0) in
  if taille+1 >= Array.length fp then failwith "file pleine"
  else (fp.(0) <- (taille+1,0); fp.(taille+1) <- (prio,som) ; remonter fp (taille+1) ) ;;

let rec descente fp i =
  let n = ref i in
  let taille,_ = fp.(0) in
  if 2*i <= taille && fp.(2*i) > fp.(i) then n := 2*i ;
  if 2*i+1 <= taille && fp.(2*i+1) > fp.(!n) then n := 2*i+1 ;
  if !n <> i then (echange fp i !n; descente fp !n) ;;

let extrait_max fp =
  if not (est_file_vide fp) then
    (let taille,_ = fp.(0) in
     fp.(0) <- (taille-1,0) ;
     let max = fp.(1) in
     echange fp 1 taille ;
     descente fp 1 ;
     max)
  else failwith "file vide" ;;

let degre sommet g = List.length g.(sommet) ;;













let coloration_welsh_powell graphe =
  let n = Array.length graphe in
  let coloration = Array.make n (-1) in
  let dispo = Array.make n false in
  let file_prio = file_vide n in

  for sommet = 0 to (n-1) do                        (* on ajoute les sommets dans la file de priorité *)
    insere file_prio (degre sommet graphe, -sommet)
  done ;

  let rec maj_voisins liste_voisins = match liste_voisins with       (* marque les couleurs des voisins *)
    | [] -> ()
    | voisin::q -> 
        let couleur_voisin = coloration.(voisin) in
        if couleur_voisin <> -1 then dispo.(couleur_voisin) <- true;
        maj_voisins q
  in

  let couleur_min () =        (* renvoie la couleur minimale disponible pour le sommet *)
    let couleur = ref 0 in
    while dispo.(!couleur) do
      couleur := !couleur + 1
    done ; !couleur
  in

  let rec nettoie_voisins liste_voisins = match liste_voisins with   (* remet dispo à false pour le prochain sommet *)
    | [] -> ()
    | voisin::q ->
        let couleur_voisin = coloration.(voisin) in
        if couleur_voisin <> -1 then dispo.(couleur_voisin) <- false;
        nettoie_voisins q
  in

  while not (est_file_vide file_prio) do
    let _, sommet_neg = extrait_max file_prio in
    let sommet = -sommet_neg in              (* parcourt tous les sommets en dépilant file_prio *)
    maj_voisins graphe.(sommet);
    let couleur = couleur_min () in
    coloration.(sommet) <- couleur ;
    nettoie_voisins graphe.(sommet)          (* on nettoie le tableau dispo à moindre coût *)
  done ;
  coloration
;;(* O(nlog(n) + m) *)


let file_vide_dsatur n = Array.make (n+1) (0,0,0) ;;
let est_file_vide_dsatur fp = let taille,_,_ = fp.(0) in taille = 0 ;;
let donnee_maxi_dsatur fp = if est_file_vide_dsatur fp then failwith "file vide" else fp.(1) ;;

let echange_dsatur fp hash i j =
  let dsat_i,deg_i,som_neg_i = fp.(i) in
  let _,_,som_neg_j = fp.(j) in
  fp.(i) <- fp.(j) ;
  fp.(j) <- dsat_i,deg_i,som_neg_i ;
  hash.(-som_neg_i) <- j ;
  hash.(-som_neg_j) <- i ;;

let remonter_dsatur fp hash i =
  let k = ref i in
  while !k <> 1 && fp.(!k) > fp.(!k /2) do
    echange_dsatur fp hash !k (!k/2) ;
    k := !k /2 ;
  done ;;

let insere_dsatur fp hash (dsat,deg,som) =
  let taille,_,_ = fp.(0) in
  if taille+1 >= Array.length fp then failwith "file pleine"
  else (fp.(0) <- (taille+1,0,0); fp.(taille+1) <- (dsat,deg,som) ; hash.(-som) <- taille+1 ; remonter_dsatur fp hash (taille+1) ) ;;

let rec descente_dsatur fp hash i =
  let n = ref i in
  let taille,_,_ = fp.(0) in 
  if 2*i <= taille && fp.(2*i) > fp.(i) then n := 2*i ;
  if 2*i+1 <= taille && fp.(2*i+1) > fp.(!n) then n := 2*i+1 ;
  if !n <> i then (echange_dsatur fp hash i !n; descente_dsatur fp hash !n) ;;

let extrait_max_dsatur fp hash =
  if not (est_file_vide_dsatur fp) then
    (let taille,_,_ = fp.(0) in
     fp.(0) <- (taille-1,0,0) ;
     let max = fp.(1) in
     echange_dsatur fp hash 1 taille ;
     descente_dsatur fp hash 1 ;
     max)
  else failwith "file toujours vide" ;;

let degre sommet g = List.length g.(sommet) ;;


let coloration_dsatur_file graphe =
  let n = Array.length graphe in
  let colo = Array.make n (-1) in
  let interdit = Array.make n [||] in
  let hash = Array.make n (-1) in
  let file_prio = file_vide_dsatur n in

  for s = 0 to n-1 do
    let d = degre s graphe in
    interdit.(s) <- Array.make (d + 1) false;
    insere_dsatur file_prio hash (0,d, -s)
  done ;

  let couleur_min sommet =
    let couleur = ref 0 in
    while interdit.(sommet).(!couleur) do
      couleur := !couleur + 1
    done ;
    !couleur
  in

  let rec maj_voisins liste_voisins couleur = match liste_voisins with
    | [] -> ()
    | voisin :: q ->
        if (couleur < (List.length graphe.(voisin))) && colo.(voisin) = -1 && not interdit.(voisin).(couleur) then
            (interdit.(voisin).(couleur) <- true ;
            let (dsat_v, deg_v, som_neg_v) = file_prio.(hash.(voisin)) in
            file_prio.(hash.(voisin)) <- (dsat_v + 1, deg_v, som_neg_v) ;
            remonter_dsatur file_prio hash hash.(voisin);
            maj_voisins q couleur)
      else maj_voisins q couleur
  in

  while not (est_file_vide_dsatur file_prio) do
    let _, _, som_neg = extrait_max_dsatur file_prio hash in
    let sommet = -som_neg in
    let couleur = couleur_min sommet in
    colo.(sommet) <- couleur ;
    maj_voisins graphe.(sommet) couleur
  done ;
  colo;;


let coloration_dsatur g =
  let n = Array.length g in
  let coloration = Array.make n (-1) in
  let dsat_degre_tab = Array.make n (0,0) in
  let interdit = Array.make n [||] in
  for s = 0 to (n-1) do                                          (* seul truc changé *)
    let d = degre s g in
    dsat_degre_tab.(s) <- (0, d) ;
    interdit.(s) <- Array.make (d + 1) false
  done ;

  let max_dsat () =
    let indice_max = ref (-1) in
    for sommet = 0 to (n-1) do
      let dsat_sommet, degre_sommet = dsat_degre_tab.(sommet) in
      if coloration.(sommet) = -1 then
        match !indice_max with
        | -1 -> indice_max := sommet
        | j ->
            let dsat_indice_max, degre_indice_max = dsat_degre_tab.(j) in
            if dsat_sommet > dsat_indice_max then indice_max := sommet
            else if dsat_sommet = dsat_indice_max && degre_sommet > degre_indice_max then indice_max := sommet
            else if dsat_sommet = dsat_indice_max && degre_sommet = degre_indice_max && sommet < j then indice_max := sommet
    done ;
    !indice_max
  in
  for _ = 0 to (n-1) do
    let sommet = max_dsat () in
    let couleur = ref 0 in
    while interdit.(sommet).(!couleur) do
      couleur := !couleur + 1
    done ;
    coloration.(sommet) <- !couleur ;
    let rec maj_voisins liste_voisins = match liste_voisins with
      | [] -> ()
      | voisin :: q ->
          if (!couleur < (List.length g.(voisin))) && coloration.(voisin) = -1 && not interdit.(voisin).(!couleur) then
            (interdit.(voisin).(!couleur) <- true ;
             let dsat_v, degre_v = dsat_degre_tab.(voisin) in
             dsat_degre_tab.(voisin) <- (dsat_v + 1, degre_v)) ;
          maj_voisins q
    in maj_voisins g.(sommet)
  done ;
  coloration ;;











let coloration_Branch_and_bound graphe =
  let n = Array.length graphe in
  if n = 0 then [||],0
  else
    (let coloration_initiale = Array.make n 0 in
     for i = 1 to (n-1) do
       coloration_initiale.(i) <- i
     done ;
     let xmax = ref n in         (* coul_max qui diminue, càd nombre chromatique - 1*)
     let coloration_opti = ref coloration_initiale          (*coloration optimale qui s'améliore *)
     in



     let continue coloration somm =         (*vérifie si oui ou non on continue la coloration
                                      après avoir mis une couleur au sommet som *)
       let rec est_couleur_valide liste = match liste with
         | [] -> true
         | vois :: q -> if coloration.(vois) = coloration.(somm) && vois < somm
             then false
             else est_couleur_valide q
       in
       coloration.(somm) < !xmax && est_couleur_valide graphe.(somm)
     in



     let rec coloration_suivante coloration indice =          (*maj coloration pour mettre la prochaine coloration                                                             valide et renvoie l'indice du dernier élément rempli*)
       let i = ref indice in
       while !i >= 0 && coloration.(!i) = !xmax do
         coloration.(!i) <- -1 ;
         i:= !i -1 ;
       done ;
       if !i >= 0 then
         (coloration.(!i) <- coloration.(!i) +1 ;
          if continue coloration !i then !i
          else coloration_suivante coloration !i )
       else !i
     in



     let coloration= Array.make n (-1) in          (* la coloration qu'on va modifier *)
     coloration.(0) <- 0 ;         (*on initialise la coloration *)
     let som = ref 0          (*le dernier indice coloré valide *)
     in



     let maj_coloration_opti coloration =          (*si la coloration est finie, alors elle est améliore la coloration_opti *)
       let coul_max = ref 0 in
       let imax = ref 0 in
       for i = 0 to (n-1) do
         if coloration.(i) > !coul_max then
           (coul_max := coloration.(i) ;
            imax := i ;)
       done ;
       coloration_opti := Array.copy coloration ;
       xmax := !coul_max ;
       som := !imax ;
       for j = !som to (n-1) do
         coloration.(j)<- -1;
       done; 
       som := coloration_suivante coloration !som
     in


     let rec backtracking coloration =
       if coloration.(0) = -1 then !coloration_opti,(!xmax +1)         (*si on a traité tous les sommets*)
       else
         (if !som = n-1 then             (* si la coloration est finie alors elle est améliorée*)
            maj_coloration_opti coloration            (*som est déjà mis à jour pour appeler fonction*)
          else
            (coloration.(!som +1) <- 0 ;             (*on ajoute un sommet potentiel à la couleur 0 *)
             if not (continue coloration (!som+1)) then          (* si il est pas bon, on coloration suivante*)
               som := coloration_suivante coloration (!som+1)

             else som := !som +1 (*si il est bon, on indente som*)
            ) ;
          backtracking coloration
         )

     in
     backtracking coloration
    ) ;;









