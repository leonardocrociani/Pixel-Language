type ide = string ;; 
type 't env = ide -> 't ;;
type evT = 
  | Unbound
  | Pixel of int*int*int ;;
type exp =
  | Ide of ide
  | Pix of int*int*int
  | Lighten of exp
  | Darken of exp
  | Let of ide*exp*exp ;;

let espressione = 
  Let(
    "redder",
    Pix(255,31,42),
    Darken(Lighten(Ide("redder")))
  ) ;;
let emptyenv = fun x -> Unbound ;;
let bind (s:evT env) (id:ide) (v:evT) =
  fun ask -> if ask = id then v else s id;; 

let cch ch =  (* check channel *)
  ch<=255&ch>=0;;

let dark ch = 
  if ch=0 then 0
  else (ch-1) ;;

let light ch = 
  if ch=255 then 255
  else (ch+1) ;;
    

let rec eval e s = 
  match e with
  | Ide (x) -> s x
  | Pix (r,g,b) -> (
      match (cch(r), cch(g), cch(b)) with 
      | true,true,true -> Pixel (r,g,b)
      | _,_,_ -> failwith "Invalid channel" 
    )
  | Lighten (p) -> let evp = eval p s in (
      match evp with
      | Pixel(r,g,b) -> Pixel(light(r),light(g),light(b))
      | _ -> failwith "Can't apply lighten to a non-pixel value"
    )
  | Darken (p) -> let evp = eval p s in (
      match evp with
      | Pixel(r,g,b) -> Pixel(dark(r),dark(g),dark(b))
      | _ -> failwith "Can't apply darken to a non-pixel value"
    )
  | Let (ide,e1,e2) -> eval e2 (bind s ide (eval e1 s)) ;;


eval espressione emptyenv;;
