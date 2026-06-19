
(* 1. NETTOYAGE ET CONVERSION DES VOISINS *)
let chaine_to_liste s =
  let s_clean = String.trim s in
  if s_clean = "" then []
  else
    let morceaux = String.split_on_char ';' s_clean in
    let rec convertit l = match l with
      | [] -> []
      | t::q -> 
          let t_clean = String.trim t in
          if t_clean = "" then convertit q 
          else
            try (int_of_string t_clean) :: convertit q
            with Failure _ -> convertit q
    in convertit morceaux
;;

(* 2. PARSEUR DE GRAPHE RÉSISTANT AUX CROCHETS PARASITES *)
let extrait_un_exemplaire_groupe s_ex taille_graphe =
  let g = Array.make taille_graphe [] in
  (* On découpe par chaque crochet ouvrant pour isoler les listes de voisins *)
  let morceaux = String.split_on_char '[' s_ex in
  let i = ref 0 in
  
  List.iter (fun m ->
    let len = String.length m in
    let b = Buffer.create len in
    (* On extrait uniquement les chiffres et les points-virgules *)
    for j = 0 to len - 1 do
      let c = m.[j] in
      if (c >= '0' && c <= '9') || c = ';' then Buffer.add_char b c
    done;
    let propre = String.trim (Buffer.contents b) in
    (* Si le morceau contient de vrais voisins et qu'on n'a pas dépassé la taille du graphe *)
    if propre <> "" && !i < taille_graphe then begin
      g.(!i) <- chaine_to_liste propre;
      incr i
    end
  ) morceaux;
  g
;;

(* 3. LECTEUR DE FICHIER GÉNÉRAL ADAPTÉ *)
let g_recup_direct (t, d) nom_fichier_ml =
  let ic = open_in nom_fichier_ml in
  let prefixe_cible = "let g_" ^ (string_of_int t) ^ "_" ^ (string_of_int d) ^ " =" in
  let len_prefixe = String.length prefixe_cible in
  let tableau_graphes = ref [||] in
  let trouve = ref false in
  
  try
    while not !trouve do
      let ligne = String.trim (input_line ic) in
      if String.length ligne >= len_prefixe && String.sub ligne 0 len_prefixe = prefixe_cible then
        begin
          trouve := true;
          (* On découpe la ligne par la barre verticale pour séparer les exemplaires *)
          let exemplaires_bruts = String.split_on_char '|' ligne in
          let liste_finales = ref [] in
          
          List.iter (fun ex_brut ->
            (* On compte si le morceau contient des crochets pour s'assurer que c'est un graphe *)
            let nb_crochets = ref 0 in
            String.iter (fun c -> if c = '[' then incr nb_crochets) ex_brut;
            
            (* Si le morceau est valide, on l'analyse *)
            if !nb_crochets >= t then begin
              let g = extrait_un_exemplaire_groupe ex_brut t in
              liste_finales := g :: !liste_finales
            end
          ) exemplaires_bruts;
          
          tableau_graphes := Array.of_list (List.rev !liste_finales)
        end
    done;
    close_in ic;
    !tableau_graphes
  with End_of_file ->
    close_in ic;
    [||] (* Évite de faire crash le benchmark si une catégorie manque *)
;;



(* --- LOGIQUE DE MESURE ET GÉNÉRATION CSV --- *)
let mes_algos = [
 coloration_first_fit;
 coloration_welsh_powell;
 coloration_dsatur; 
coloration_dsatur_file
] ;;

let algo_bb = [fun c -> fst (coloration_Branch_and_bound c)] ;;


let max t =
  let n = Array.length t in
  let m = ref 0 in
  for i = 0 to (n-1) do
    if t.(i) > !m then m := t.(i)
  done;
  !m + 1
;;

let mesure_tps_et_couleurs f g =
  let debut = Unix.gettimeofday () in
  let coloration = f g in
  let fin = Unix.gettimeofday () in
  let tps = fin -. debut in
  if tps < 0.0001 then
    (let debut_boucle = Unix.gettimeofday () in
     for k = 1 to 100 do let _ = f g in () done;
     let fin_boucle = Unix.gettimeofday () in
     ((fin_boucle -. debut_boucle) /. 100.0, max coloration))
  else (tps, max coloration)
;;

let genere_csv file nom_fichier_graphes liste_couples list_fonction =
  let oc = open_out file in
  
  Printf.fprintf oc "Taille,Aretes,Exemplaire";
  let rec ecrit_entete lf i = match lf with
    | [] -> Printf.fprintf oc "\n"
    | _::q -> Printf.fprintf oc ",Tps_Algo%i,NbCol_Algo%i" i i; ecrit_entete q (i+1)
  in ecrit_entete list_fonction 1;

  let rec boucle_couples l = match l with
    | [] -> print_endline "=== FIN DE TOUS LES TESTS AVEC SUCCÈS ==="
    | (t, d)::q_couples ->        
        print_endline ("Tentative d'extraction de : g_" ^ string_of_int t ^ "_" ^ string_of_int d);
        
        try
          let tableau_exemplaires = g_recup_direct (t, d) nom_fichier_graphes in
          let n_exemplaires = Array.length tableau_exemplaires in
          print_endline ("-> Trouvé ! " ^ string_of_int n_exemplaires ^ " exemplaires à tester.");
          
          for i = 0 to (n_exemplaires - 1) do
            let g = tableau_exemplaires.(i) in
            Printf.fprintf oc "%i,%i,%i" t d (i + 1);
            
            let rec boucle_algos lf = match lf with
              | [] -> Printf.fprintf oc "\n"
              | f::qf ->
                  let tps, nb_col = mesure_tps_et_couleurs f g in
                  Printf.fprintf oc ",%.15f,%i" tps nb_col;
                  let rec_qf = boucle_algos qf in
                  rec_qf
            in boucle_algos list_fonction;
          done;
          flush oc;
          boucle_couples q_couples
        with e -> 
          print_endline ("❌ ERREUR sur le couple g_" ^ string_of_int t ^ "_" ^ string_of_int d ^ " : " ^ Printexc.to_string e);
          boucle_couples q_couples
  in
  boucle_couples liste_couples;
  close_out oc
;;


let l_couples_1 = [ 3,1 ; 4,1 ; 4,2 ; 5,1 ; 5,2 ; 5,3 ; 6,1 ; 6,2 ; 6,3 ; 6,4 ; 7,1 ; 7,2 ; 7,3 ; 7,4 ; 7,5 ; 8,1 ; 8,2 ; 8,3 ; 8,4 ; 8,5 ; 8,6 ; 9,1 ; 9,2 ; 9,3 ; 9,4 ; 9,5 ; 9,6 ; 9,7 ; 10,1 ; 10,2 ; 10,3 ; 10,4 ; 10,5 ; 10,6 ; 10,7 ; 10,8 ; 11,1 ; 11,2 ; 11,3 ; 11,4 ; 11,5 ; 11,6 ; 11,7 ; 11,8 ; 11,9 ; 12,1 ; 12,2 ; 12,3 ; 12,4 ; 12,5 ; 12,6 ; 12,7 ; 12,8 ; 12,9 ; 12,10 ; 13,1 ; 13,2 ; 13,3 ; 13,4 ; 13,5 ; 13,6 ; 13,7 ; 13,8 ; 13,9 ; 13,10 ; 13,11 ; 14,1 ; 14,2 ; 14,3 ; 14,4 ; 14,5 ; 14,6 ; 14,7 ; 14,8 ; 14,9 ; 14,10 ; 14,11 ; 14,12 ; 15,1 ; 15,2 ; 15,3 ; 15,4 ; 15,5 ; 15,6 ; 15,7 ; 15,8 ; 15,9 ; 15,10 ; 15,11 ; 15,12 ; 15,13 ; 16,1 ; 16,2 ; 16,3 ; 16,4 ; 16,5 ; 16,6 ; 16,7 ; 16,8 ; 16,9 ; 16,10 ; 16,11 ; 16,12 ; 16,13 ; 16,14 ; 17,1 ; 17,2 ; 17,3 ; 17,4 ; 17,5 ; 17,6 ; 17,7 ; 17,8 ; 17,9 ; 17,10 ; 17,11 ; 17,12 ; 17,13 ; 17,14 ; 17,15 ; 18,1 ; 18,2 ; 18,3 ; 18,4 ; 18,5 ; 18,6 ; 18,7 ; 18,8 ; 18,9 ; 18,10 ; 18,11 ; 18,12 ; 18,13 ; 18,14 ; 18,15 ; 18,16 ; 19,1 ; 19,2 ; 19,3 ; 19,4 ; 19,5 ; 19,6 ; 19,7 ; 19,8 ; 19,9 ; 19,10 ; 19,11 ; 19,12 ; 19,13 ; 19,14 ; 19,15 ; 19,16 ; 19,17 ; 20,1 ; 20,2 ; 20,3 ; 20,4 ; 20,5 ; 20,6 ; 20,7 ; 20,8 ; 20,9 ; 20,10 ; 20,11 ; 20,12 ; 20,13 ; 20,14 ; 20,15 ; 20,16 ; 20,17 ; 20,18 ] ;;



