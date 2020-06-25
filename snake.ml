open Graphics;;
open Unix;;

(*le jeu se fait sur un tableau 40 * 40 *)
open_graph " 800x800";;

type direction = Haut | Gauche | Bas | Droite ;;
type contenu = Rien | Snake | Food;;
type case = {mutable cont : contenu; mutable dir : direction};;
type game_state = {
    matrice : case array array ;
    score : int ; 
    tete : (int*int) ;
    queue : (int*int);
    mort : bool
    };;


let draw g = 
    let m = g.matrice in 
    let n = Array.length m in 
    clear_graph ();
    moveto 0 0;
    for i = 0 to n-1 do 
        for j = 0 to n-1 do 
            match (m.(i).(j)).cont with
            |Rien -> ()
            |Snake -> set_color red; fill_rect (20*j) (20*i) 20 20
            |Food -> set_color green; fill_rect (20*j) (20*i) 20 2
        done;
    done;;

(*renvoie simplement la coordonnée où placer la nouriture*)
(*implémentation très naive*)
let rec add_food m = 
    let n = Array.length m in
    let r = Random.int (n*n) in 
    if (m.(r/n).(r mod n)).cont = Rien then ( r/n ,r mod n) else add_food m;;

let update g dir = 
    let m = g.matrice in 
    let n = Array.length m in
    let (xt,yt)= g.tete and (xq,yq) = g.queue in 
    let ntete = ref (xt,yt) and nqueue = ref (xq,yq) and s = ref g.score in
    let test =  (xt>=n)||(xt<0)||(yt>=n)||(yt<0) in 
    (match dir with
    |Haut -> ((m.(xt).(yt + 1)).cont <- Snake);ntete:=(xt,yt+1)
    |Gauche -> ((m.(xt - 1).(yt)).cont <- Snake);ntete:=(xt-1,yt)
    |Bas -> ((m.(xt).(yt - 1)).cont <- Snake);ntete:=(xt,yt-1)
    |Droite -> ((m.(xt +1).(yt)).cont <- Snake);ntete:=(xt,yt+1)
    );
    let (nxt,nyt)= !ntete in 
    if test||((m.(nxt).(nyt)).cont=Food) then
    {matrice = g.matrice ;score =  g.score;tete = g.tete;queue = g.queue; mort = true}
    else 
    let b= (((m.(xt).(yt)).cont)=Food) in
    (m.(xt).(yt)).dir <- dir;
    
    (* si une pastille a été mangée*)
    if b then incr(s)
    else (
    (m.(xq).(yq)).cont <- Rien;
    match (m.(xq).(yq)).dir with
    |Haut -> nqueue := (xq,yq +1)
    |Gauche -> nqueue := (xq-1,yq)
    |Bas -> nqueue := (xq,yq-1)
    |Droite -> nqueue := (xq+1,yq)
    );
    let (xf,yf)=add_food m in 
    (m.(xf).(yf)).cont <- Food;
    {matrice = m;score = !s;tete  = !ntete;queue = !nqueue;mort = false} 
    ;;
    

let main = 
    let m = Array.make_matrix 40 40 {cont = Rien;dir = Haut} in 
    m.(0).(0)<- {cont = Snake ;dir = Droite}; m.(0).(1)<-{cont = Snake ;dir = Droite };
    let game = ref {matrice = m; score = 2;tete = (0,1);queue = (0,0);mort = false } in 
    let t0 = ref (int_of_float (time ())) in 
    let dir = ref Droite in 
    draw !game;
    while not (!game).mort do 
        while int_of_float (time ()) - !t0 < 1 do 
            let s = wait_next_event [Key_pressed; Poll] in 
            match s.key with 
            |'z'-> dir := Haut 
            |'q'-> dir := Gauche
            |'s'-> dir := Bas
            |'d'-> dir := Droite
            |_->()
            ;
        done;
        t0:= int_of_float ( time ());
        game := update !game !dir;
    done;
    moveto 400 400;
    set_color black;
    draw_string (string_of_int !game.score);
    read_key ();
    ;;

main ;;

